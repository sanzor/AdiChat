import { create } from "zustand";
import { Channel } from "../Domain/Channel";

interface ChannelState {
  channels: Channel[];
  currentChannel: Channel | null;
  setChannels: (channels: Channel[]) => void;
  setCurrentChannel: (channel: Channel | null) => void;
  addChannel: (channel: Channel) => void;
  removeChannel: (channelId: number) => void;
}

export const useChannelStore = create<ChannelState>((set, get) => ({
  channels: [],
  currentChannel: null,

  setChannels: (channels) => set({ channels }),

  setCurrentChannel: (channel) => set({ currentChannel: channel }),

  addChannel: (channel) => {
    const { channels } = get();
    if (!channels.find((c) => c.id === channel.id)) {
      set({ channels: [...channels, channel] });
    }
  },

  removeChannel: (channelId) => {
    const { channels, currentChannel } = get();
    const updated = channels.filter((c) => c.id !== channelId);
    set({
      channels: updated,
      currentChannel:
        currentChannel?.id === channelId
          ? updated[0] ?? null
          : currentChannel,
    });
  },
}));
