import { useMessageStore } from "../stores/useMessageStore";

export class MessageService {
  handleMessage(data: any) {
    switch (data.kind) {
      case "chat":
        this.handleChat(data);
        break;
      case "command_result":
        this.handleCommandResult(data);
        break;
      case "user_event":
        // User events (subscribe/unsubscribe notifications) now handled via HTTP
        break;
      default:
        console.warn("Unknown message kind:", data.kind);
    }
  }

  private handleChat(data: any) {
    const store = useMessageStore.getState();
    switch (data.type) {
      case "new_message":
        store.addIncomingMessage(data);
        break;
      case "message_published":
        store.confirmMessage(data);
        break;
      default:
        console.warn("Unknown chat message type:", data.type);
    }
  }

  private handleCommandResult(data: any) {
    // Most command results are now handled via HTTP/React Query.
    // Handle any remaining WebSocket command results here if needed.
    switch (data.command) {
      case "get_newest_messages": {
        const store = useMessageStore.getState();
        const msgs = data.result?.messages || [];
        msgs.forEach((m: any) => store.addIncomingMessage(m));
        break;
      }
      default:
        break;
    }
  }
}
