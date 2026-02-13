import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import path from "node:path";

const proxyTarget = process.env.VITE_PROXY_TARGET || "http://localhost:8080";

export default defineConfig({
  plugins: [react()],
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
  server: {
    port: 5173,
    proxy: {
      "/auth": { target: proxyTarget, changeOrigin: true },
      "/get-user": { target: proxyTarget, changeOrigin: true },
      "/create-user": { target: proxyTarget, changeOrigin: true },
      "/delete-user": { target: proxyTarget, changeOrigin: true },
      "/create-topic": { target: proxyTarget, changeOrigin: true },
      "/delete-topic": { target: proxyTarget, changeOrigin: true },
      "/subscribe": { target: proxyTarget, changeOrigin: true },
      "/unsubscribe": { target: proxyTarget, changeOrigin: true },
      "/get_subscriptions": { target: proxyTarget, changeOrigin: true },
      "/publish": { target: proxyTarget, changeOrigin: true },
      "/get_messages": { target: proxyTarget, changeOrigin: true },
      "/ws": { target: proxyTarget, ws: true, changeOrigin: true }
    }
  }
});

