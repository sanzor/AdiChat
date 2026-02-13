// Websocket.ts
let socket: WebSocket | null = null;

/**
 * Connects to the WebSocket server and returns the WebSocket instance.
 */
export function connect(url: string): WebSocket {
  console.log(`Connecting using url:${url}`);
  if (!socket || socket.readyState === WebSocket.CLOSED) {
    socket = new WebSocket(url);

    // Optional: Add default handlers for debugging
    socket.onopen = () => {console.log("WebSocket connection opened.");}
    socket.onerror = (error) => console.error(`WebSocket error:${JSON.stringify(error)}`);
  }
  return socket;
}

/**
 * Sends data through the WebSocket.
 */
export function send(data: string) {
  if (!socket || socket.readyState !== WebSocket.OPEN) {
    console.error("WebSocketController: Cannot send command, WebSocket is not connected.");
    return;
  }
  try {
    console.log("WebSocketController: Sending data:", data);
    socket.send(data);
  } catch (error) {
    console.error("WebSocketController: Failed to send data:", error);
  }
}

/**
 * Attaches a message handler to the WebSocket.
 */
export function onMessage(socket: WebSocket, callback: (message: MessageEvent) => void) {
  socket.onmessage = callback;
}

/**
 * Attaches a close handler to the WebSocket.
 */
export function onClose(socket: WebSocket, callback: () => void) {
  socket.onclose = callback;
}

/**
 * Closes the WebSocket connection.
 */
export function close() {
  if (socket) {
    socket.close();
    socket = null;
  }
}
