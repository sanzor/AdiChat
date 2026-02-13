import { useState } from "react";
import { useUserStore } from "./stores/useUserStore";
import LoginForm from "./components/LoginForm";
import RegisterForm from "./components/RegisterForm";
import Sidebar from "./components/Sidebar";
import ChatPanel from "./components/ChatPanel";
import WebSocketManager from "./components/WebSocketManager";

type View = "login" | "register";

export default function App() {
  const user = useUserStore((s) => s.user);
  const [authView, setAuthView] = useState<View>("login");

  // Not logged in - show auth forms
  if (!user) {
    return authView === "login" ? (
      <LoginForm onRegister={() => setAuthView("register")} />
    ) : (
      <RegisterForm onBackToLogin={() => setAuthView("login")} />
    );
  }

  // Logged in - show main chat layout
  return (
    <div className="flex h-screen bg-background text-foreground">
      <WebSocketManager />
      <Sidebar />
      <ChatPanel />
    </div>
  );
}
