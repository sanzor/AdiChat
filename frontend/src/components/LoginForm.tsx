import { useState } from "react";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { useLoginMutation, useResendVerificationMutation, AuthError } from "@/api/users";
import { useUserStore } from "@/stores/useUserStore";

interface LoginFormProps {
  onRegister: () => void;
}

export default function LoginForm({ onRegister }: LoginFormProps) {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const setUser = useUserStore((s) => s.setUser);
  const login = useLoginMutation();
  const resend = useResendVerificationMutation();

  const handleLogin = () => {
    login.mutate(
      { email, password },
      {
        onSuccess: (user) => setUser(user),
      }
    );
  };

  const handleResend = () => {
    resend.mutate(email);
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Enter") handleLogin();
  };

  const errorCode = login.error instanceof AuthError ? login.error.code : null;

  return (
    <div className="flex h-screen items-center justify-center bg-background">
      <Card className="w-full max-w-sm">
        <CardHeader>
          <CardTitle className="text-center text-2xl">Login</CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="space-y-2">
            <label className="text-sm font-medium">Email</label>
            <Input
              type="email"
              placeholder="you@example.com"
              value={email}
              onChange={(e) => setEmail(e.target.value)}
              onKeyDown={handleKeyDown}
            />
          </div>
          <div className="space-y-2">
            <label className="text-sm font-medium">Password</label>
            <Input
              type="password"
              placeholder="Password"
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              onKeyDown={handleKeyDown}
            />
          </div>
          {login.isError && (
            <div className="space-y-2">
              <p className="text-sm text-destructive">{login.error.message}</p>
              {errorCode === "not_verified" && (
                <Button
                  variant="link"
                  className="h-auto p-0 text-sm"
                  onClick={handleResend}
                  disabled={resend.isPending}
                >
                  {resend.isPending ? "Sending..." : "Resend verification email"}
                </Button>
              )}
              {resend.isSuccess && (
                <p className="text-sm text-green-500">Verification email sent!</p>
              )}
            </div>
          )}
          <div className="flex gap-2">
            <Button
              className="flex-1"
              onClick={handleLogin}
              disabled={login.isPending}
            >
              {login.isPending ? "Logging in..." : "Login"}
            </Button>
            <Button variant="outline" className="flex-1" onClick={onRegister}>
              Register
            </Button>
          </div>
        </CardContent>
      </Card>
    </div>
  );
}
