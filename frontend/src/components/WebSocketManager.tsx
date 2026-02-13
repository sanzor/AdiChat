import { useEffect, useRef } from "react";
import { WebSocketController } from "@/Websocket/WebsocketController";
import { useUserStore } from "@/stores/useUserStore";
import config from "@/Config";

export default function WebSocketManager() {
  const user = useUserStore((s) => s.user);
  const controllerRef = useRef<WebSocketController | null>(null);

  useEffect(() => {
    if (!user) {
      // Disconnect if user logged out
      controllerRef.current?.disconnect();
      controllerRef.current = null;
      return;
    }

    const controller = new WebSocketController();
    controllerRef.current = controller;
    controller.connect(`${config.baseWsUrl}/ws/id/${user.id}`);

    return () => {
      controller.disconnect();
      controllerRef.current = null;
    };
  }, [user?.id]);

  return null;
}
