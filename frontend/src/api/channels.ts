import { useQuery, useMutation, useQueryClient } from "@tanstack/react-query";
import { Channel } from "../Domain/Channel";
import { useChannelStore } from "../stores/useChannelStore";

interface SubscriptionsResponse {
  user_id: number;
  subscriptions: Channel[];
}

interface SubscribeResponse {
  topicId: number;
  userId: number;
}

async function fetchChannels(userId: number): Promise<Channel[]> {
  const res = await fetch(`/get_subscriptions/${userId}`);
  if (!res.ok) throw new Error("Failed to fetch channels");
  const data: SubscriptionsResponse = await res.json();
  return data.subscriptions;
}

async function subscribeToChannel(
  userId: number,
  topic: string
): Promise<SubscribeResponse> {
  const res = await fetch(
    `/subscribe?userId=${userId}&topic=${encodeURIComponent(topic)}`,
    { method: "POST" }
  );
  if (!res.ok) throw new Error("Failed to subscribe");
  return res.json();
}

async function unsubscribeFromChannel(
  userId: number,
  topicId: number
): Promise<void> {
  const res = await fetch(
    `/unsubscribe?user=${userId}&topic=${topicId}`,
    { method: "POST" }
  );
  if (!res.ok) throw new Error("Failed to unsubscribe");
}

export function useChannelsQuery(userId: number | undefined) {
  const setChannels = useChannelStore((s) => s.setChannels);

  return useQuery({
    queryKey: ["channels", userId],
    queryFn: async () => {
      const channels = await fetchChannels(userId!);
      setChannels(channels);
      return channels;
    },
    enabled: !!userId,
  });
}

export function useSubscribeMutation() {
  const qc = useQueryClient();

  return useMutation({
    mutationFn: ({ userId, topic }: { userId: number; topic: string }) =>
      subscribeToChannel(userId, topic),
    onSuccess: (_data, variables) => {
      qc.invalidateQueries({ queryKey: ["channels", variables.userId] });
    },
  });
}

export function useUnsubscribeMutation() {
  const qc = useQueryClient();

  return useMutation({
    mutationFn: ({
      userId,
      topicId,
    }: {
      userId: number;
      topicId: number;
    }) => unsubscribeFromChannel(userId, topicId),
    onSuccess: (_data, variables) => {
      qc.invalidateQueries({ queryKey: ["channels", variables.userId] });
    },
  });
}
