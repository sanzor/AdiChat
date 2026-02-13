import { useEffect, useRef } from "react";
import { Hash } from "lucide-react";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Separator } from "@/components/ui/separator";
import { useChannelStore } from "@/stores/useChannelStore";
import { useMessageStore } from "@/stores/useMessageStore";
import { useMessagesQuery } from "@/api/messages";
import ChatMessage from "./ChatMessage";
import ChatInput from "./ChatInput";

export default function ChatPanel() {
  const currentChannel = useChannelStore((s) => s.currentChannel);
  const messages = useMessageStore((s) => s.messages);
  const channelMessages = currentChannel
    ? messages[currentChannel.id] || []
    : [];
  const bottomRef = useRef<HTMLDivElement>(null);

  // Fetch messages when channel changes
  useMessagesQuery(currentChannel?.id);

  // Auto-scroll to bottom when new messages arrive
  useEffect(() => {
    bottomRef.current?.scrollIntoView({ behavior: "smooth" });
  }, [channelMessages.length]);

  if (!currentChannel) {
    return (
      <div className="flex flex-1 flex-col items-center justify-center text-muted-foreground">
        <Hash className="mb-2 h-12 w-12 opacity-30" />
        <p className="text-lg font-medium">No channel selected</p>
        <p className="text-sm">Pick a channel from the sidebar to start chatting</p>
      </div>
    );
  }

  return (
    <div className="flex flex-1 flex-col">
      {/* Channel header */}
      <div className="flex items-center gap-2 border-b border-border px-4 py-3">
        <Hash className="h-5 w-5 text-muted-foreground" />
        <h2 className="text-lg font-semibold">{currentChannel.name}</h2>
      </div>

      {/* Messages */}
      <ScrollArea className="flex-1">
        <div className="flex flex-col py-4">
          {channelMessages.length === 0 ? (
            <div className="flex flex-col items-center justify-center py-20 text-muted-foreground">
              <Hash className="mb-2 h-8 w-8 opacity-30" />
              <p className="text-sm">
                No messages yet in #{currentChannel.name}
              </p>
              <p className="text-xs">Be the first to send a message!</p>
            </div>
          ) : (
            channelMessages.map((msg) => (
              <ChatMessage key={msg.id ?? msg.tempId} message={msg} />
            ))
          )}
          <div ref={bottomRef} />
        </div>
      </ScrollArea>

      <ChatInput />
    </div>
  );
}
