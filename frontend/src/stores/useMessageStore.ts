import { create } from "zustand";
import { ChatMessage, SENDING } from "../Domain/ChatMessage";
import { ChatMessageDto } from "../Dtos/ChatMessageDto";
import { getWsController } from "../Websocket/WebsocketController";

function toChatMessage(dto: ChatMessageDto): ChatMessage {
  return {
    id: dto.id,
    tempId: dto.temp_id,
    userId: dto.user_id,
    topicId: dto.topic_id,
    message: dto.content,
    created_at: dto.created_at ? Number(dto.created_at) : Date.now(),
    status: dto.status,
  };
}

interface MessageState {
  messages: Record<number, ChatMessage[]>;

  setMessages: (topicId: number, msgs: ChatMessage[]) => void;

  addIncomingMessage: (dto: ChatMessageDto) => void;

  confirmMessage: (dto: ChatMessageDto) => void;

  addOptimisticMessage: (msg: ChatMessage) => void;

  publishMessage: (params: {
    userId: number;
    topicId: number;
    message: string;
    tempId: string;
  }) => void;

  acknowledgeMessage: (tempId: string, userId: number) => void;
}

export const useMessageStore = create<MessageState>((set, get) => ({
  messages: {},

  setMessages: (topicId, msgs) => {
    set((state) => ({
      messages: { ...state.messages, [topicId]: msgs },
    }));
  },

  addIncomingMessage: (dto) => {
    const msg = toChatMessage(dto);
    set((state) => {
      const existing = state.messages[msg.topicId] || [];
      if (existing.some((m) => m.id === msg.id)) return state;
      return {
        messages: {
          ...state.messages,
          [msg.topicId]: [...existing, msg],
        },
      };
    });
  },

  confirmMessage: (dto) => {
    const msg = toChatMessage(dto);
    set((state) => {
      const existing = state.messages[msg.topicId] || [];
      const idx = existing.findIndex((m) => m.tempId === msg.tempId);
      if (idx === -1) return state;
      const updated = [...existing];
      updated[idx] = { ...msg, tempId: null };
      return {
        messages: { ...state.messages, [msg.topicId]: updated },
      };
    });

    // Send ACK
    get().acknowledgeMessage(dto.temp_id, dto.user_id);
  },

  addOptimisticMessage: (msg) => {
    set((state) => {
      const existing = state.messages[msg.topicId] || [];
      return {
        messages: { ...state.messages, [msg.topicId]: [...existing, msg] },
      };
    });
  },

  publishMessage: ({ userId, topicId, message, tempId }) => {
    const optimistic: ChatMessage = {
      id: null,
      tempId,
      userId,
      topicId,
      message,
      status: SENDING,
      created_at: Date.now(),
    };
    get().addOptimisticMessage(optimistic);

    const controller = getWsController();
    controller?.sendCommand({
      command: "publish",
      temp_id: tempId,
      topic_id: topicId,
      content: message,
      user_id: userId,
    });
  },

  acknowledgeMessage: (tempId, userId) => {
    const controller = getWsController();
    controller?.sendCommand({
      command: "acknowledge",
      user_id: userId,
      message_temp_id: tempId,
    });
  },
}));
