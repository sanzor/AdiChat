const defaultHttpBase = typeof window !== "undefined" ? window.location.origin : "http://localhost:5173";
const defaultWsBase =
  typeof window !== "undefined"
    ? `${window.location.protocol === "https:" ? "wss" : "ws"}://${window.location.host}`
    : "ws://localhost:5173";

const config = {
  baseHttpUrl: import.meta.env.VITE_API_URL || defaultHttpBase,
  baseWsUrl: import.meta.env.VITE_WS_URL || defaultWsBase,
};

export default config;
