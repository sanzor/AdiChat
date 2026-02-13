import { useState } from "react";
import { Hash, Plus, X, LogOut } from "lucide-react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Separator } from "@/components/ui/separator";
import { Badge } from "@/components/ui/badge";
import { useChannelStore } from "@/stores/useChannelStore";
import { useMessageStore } from "@/stores/useMessageStore";
import { useUserStore } from "@/stores/useUserStore";
import {
  useChannelsQuery,
  useSubscribeMutation,
  useUnsubscribeMutation,
} from "@/api/channels";
import { Channel } from "@/Domain/Channel";

export default function Sidebar() {
  const [topicName, setTopicName] = useState("");
  const user = useUserStore((s) => s.user);
  const logout = useUserStore((s) => s.logout);
  const { channels, currentChannel, setCurrentChannel } = useChannelStore();
  const messages = useMessageStore((s) => s.messages);

  useChannelsQuery(user?.id);
  const subscribeMut = useSubscribeMutation();
  const unsubscribeMut = useUnsubscribeMutation();

  const handleSubscribe = () => {
    if (!topicName.trim() || !user) return;
    subscribeMut.mutate(
      { userId: user.id, topic: topicName.trim() },
      { onSuccess: () => setTopicName("") }
    );
  };

  const handleUnsubscribe = (e: React.MouseEvent, channel: Channel) => {
    e.stopPropagation();
    if (!user) return;
    unsubscribeMut.mutate({ userId: user.id, topicId: channel.id });
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") handleSubscribe();
  };

  return (
    <div className="flex h-full w-64 flex-col border-r border-border bg-card">
      {/* Header */}
      <div className="flex items-center justify-between px-4 py-3">
        <h2 className="text-lg font-semibold">Channels</h2>
        <Button variant="ghost" size="icon" onClick={logout} title="Logout">
          <LogOut className="h-4 w-4" />
        </Button>
      </div>
      <Separator />

      {/* Subscribe input */}
      <div className="flex gap-1 p-3">
        <Input
          placeholder="Channel name..."
          value={topicName}
          onChange={(e) => setTopicName(e.target.value)}
          onKeyDown={handleKeyDown}
          className="h-8 text-sm"
        />
        <Button
          size="icon"
          className="h-8 w-8 shrink-0"
          onClick={handleSubscribe}
          disabled={subscribeMut.isPending}
        >
          <Plus className="h-4 w-4" />
        </Button>
      </div>
      <Separator />

      {/* Channel list */}
      <ScrollArea className="flex-1">
        <div className="space-y-0.5 p-2">
          {channels.map((channel) => {
            const isActive = currentChannel?.id === channel.id;
            const msgCount = messages[channel.id]?.length || 0;

            return (
              <button
                key={channel.id}
                onClick={() => setCurrentChannel(channel)}
                className={`group flex w-full items-center gap-2 rounded-md px-2 py-1.5 text-sm transition-colors ${
                  isActive
                    ? "bg-accent text-accent-foreground"
                    : "text-muted-foreground hover:bg-accent/50 hover:text-accent-foreground"
                }`}
              >
                <Hash className="h-4 w-4 shrink-0 opacity-70" />
                <span className="truncate flex-1 text-left">{channel.name}</span>
                {msgCount > 0 && (
                  <Badge variant="secondary" className="h-5 min-w-[20px] px-1 text-xs">
                    {msgCount}
                  </Badge>
                )}
                <button
                  onClick={(e) => handleUnsubscribe(e, channel)}
                  className="hidden shrink-0 rounded p-0.5 text-muted-foreground hover:text-destructive group-hover:block"
                  title="Unsubscribe"
                >
                  <X className="h-3 w-3" />
                </button>
              </button>
            );
          })}
        </div>
      </ScrollArea>

      {/* User info footer */}
      {user && (
        <>
          <Separator />
          <div className="flex items-center gap-2 px-4 py-3">
            <div className="flex h-8 w-8 items-center justify-center rounded-full bg-primary text-xs font-bold text-primary-foreground">
              {user.name.charAt(0).toUpperCase()}
            </div>
            <span className="truncate text-sm font-medium">{user.name}</span>
          </div>
        </>
      )}
    </div>
  );
}
