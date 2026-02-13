import { ChatMessage as ChatMessageType, SENDING } from "@/Domain/ChatMessage";

interface ChatMessageProps {
  message: ChatMessageType;
}

function formatTime(timestamp: number): string {
  if (!timestamp) return "";
  const d = new Date(timestamp);
  return d.toLocaleTimeString([], { hour: "2-digit", minute: "2-digit" });
}

export default function ChatMessage({ message }: ChatMessageProps) {
  const isSending = message.status === SENDING;

  return (
    <div className={`group flex gap-3 px-4 py-2 hover:bg-accent/30 ${isSending ? "opacity-50" : ""}`}>
      <div className="flex h-8 w-8 shrink-0 items-center justify-center rounded-full bg-muted text-xs font-bold text-muted-foreground">
        {String(message.userId).charAt(0)}
      </div>
      <div className="min-w-0 flex-1">
        <div className="flex items-baseline gap-2">
          <span className="text-sm font-semibold">User {message.userId}</span>
          <span className="text-xs text-muted-foreground">
            {formatTime(message.created_at)}
          </span>
          {isSending && (
            <span className="text-xs italic text-muted-foreground">sending...</span>
          )}
        </div>
        <p className="text-sm leading-relaxed break-words">{message.message}</p>
      </div>
    </div>
  );
}
