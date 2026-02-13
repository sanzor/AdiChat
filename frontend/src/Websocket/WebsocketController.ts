import { MessageService } from "./MessageService";
import { connect, close, send, onMessage, onClose } from "./Websocket";

let instance: WebSocketController | null = null;

export function getWsController(): WebSocketController | null {
  return instance;
}

export class WebSocketController {
  private socket: WebSocket | null = null;
  private isRetrying = false;
  private retryCount = 0;
  private messageQueue: string[] = [];
  private messageService: MessageService;
  private currentUrl: string | null = null;
  private onConnected?: () => void;
  private onDisconnected?: () => void;
  private onFailed?: () => void;

  constructor(opts?: {
    onConnected?: () => void;
    onDisconnected?: () => void;
    onFailed?: () => void;
  }) {
    this.messageService = new MessageService();
    this.onConnected = opts?.onConnected;
    this.onDisconnected = opts?.onDisconnected;
    this.onFailed = opts?.onFailed;
    instance = this;
  }

  connect(url: string) {
    if (this.socket) {
      if (this.socket.readyState === WebSocket.OPEN) return;
      if (this.socket.readyState === WebSocket.CONNECTING) return;
    }

    this.currentUrl = url;

    try {
      this.socket = connect(url);
      onMessage(this.socket, this.handleMessage);
      onClose(this.socket, this.handleClose);

      this.socket.onopen = () => {
        this.retryCount = 0;
        this.isRetrying = false;
        while (this.messageQueue.length > 0) {
          const msg = this.messageQueue.shift();
          if (msg) this.sendRaw(msg);
        }
        this.onConnected?.();
      };

      this.socket.onerror = () => {
        if (!this.isRetrying) {
          this.onFailed?.();
          this.retryConnection();
        }
      };
    } catch {
      this.onFailed?.();
    }
  }

  disconnect() {
    instance = null;
    if (!this.socket) return;
    if (this.socket.readyState === WebSocket.CLOSED) {
      this.socket = null;
      return;
    }
    close();
    this.socket = null;
    this.onDisconnected?.();
  }

  sendCommand(payload: object) {
    const json = JSON.stringify(payload);
    if (!this.socket || this.socket.readyState !== WebSocket.OPEN) {
      this.messageQueue.push(json);
      return;
    }
    this.sendRaw(json);
  }

  private sendRaw(data: string) {
    send(data);
  }

  private retryConnection() {
    if (!this.currentUrl) return;
    this.isRetrying = true;
    const delay = Math.min(1000 * Math.pow(2, this.retryCount), 30000);
    setTimeout(() => {
      this.retryCount++;
      this.connect(this.currentUrl!);
    }, delay);
  }

  private handleMessage = (event: MessageEvent) => {
    try {
      const data = JSON.parse(event.data);
      this.messageService.handleMessage(data);
    } catch (err) {
      console.error("WebSocket: Failed to parse message:", err);
    }
  };

  private handleClose = () => {
    this.socket = null;
    this.onDisconnected?.();
  };
}
