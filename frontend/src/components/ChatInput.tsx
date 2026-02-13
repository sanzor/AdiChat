import { useState } from "react";
import { Send } from "lucide-react";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { useMessageStore } from "@/stores/useMessageStore";
import { useUserStore } from "@/stores/useUserStore";
import { useChannelStore } from "@/stores/useChannelStore";

export default function ChatInput() {
  const [text, setText] = useState("");
  const user = useUserStore((s) => s.user);
  const currentChannel = useChannelStore((s) => s.currentChannel);
  const publishMessage = useMessageStore((s) => s.publishMessage);

  const handleSend = () => {
    if (!text.trim() || !user || !currentChannel) return;
    const tempId = `temp-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
    publishMessage({
      userId: user.id,
      topicId: currentChannel.id,
      message: text.trim(),
      tempId,
    });
    setText("");
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      handleSend();
    }
  };

  return (
    <div className="border-t border-border p-4">
      <div className="flex gap-2">
        <Input
          placeholder={
            currentChannel
              ? `Message #${currentChannel.name}`
              : "Select a channel..."
          }
          value={text}
          onChange={(e) => setText(e.target.value)}
          onKeyDown={handleKeyDown}
          disabled={!currentChannel}
          className="flex-1"
        />
        <Button
          size="icon"
          onClick={handleSend}
          disabled={!text.trim() || !currentChannel}
        >
          <Send className="h-4 w-4" />
        </Button>
      </div>
    </div>
  );
}
