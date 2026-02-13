import { useQuery } from "@tanstack/react-query";
import { ChatMessage } from "../Domain/ChatMessage";
import { ChatMessageDto } from "../Dtos/ChatMessageDto";
import { useMessageStore } from "../stores/useMessageStore";

interface MessagesResponse {
  topic: number;
  messages: ChatMessageDto[];
}

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

async function fetchMessages(topicId: number): Promise<ChatMessage[]> {
  const res = await fetch(`/get_messages/topic/${topicId}`);
  if (!res.ok) throw new Error("Failed to fetch messages");
  const data: MessagesResponse = await res.json();
  return (data.messages || []).map(toChatMessage);
}

export function useMessagesQuery(topicId: number | undefined) {
  const setMessages = useMessageStore((s) => s.setMessages);

  return useQuery({
    queryKey: ["messages", topicId],
    queryFn: async () => {
      const msgs = await fetchMessages(topicId!);
      setMessages(topicId!, msgs);
      return msgs;
    },
    enabled: !!topicId,
  });
}
