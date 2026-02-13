import { useMutation } from "@tanstack/react-query";
import { User } from "../Domain/User";

interface LoginParams {
  email: string;
  password: string;
}

interface RegisterParams {
  name: string;
  email: string;
  password: string;
}

export class AuthError extends Error {
  code: string;
  constructor(message: string, code: string) {
    super(message);
    this.code = code;
  }
}

async function loginUser(params: LoginParams): Promise<User> {
  const res = await fetch("/auth/login", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(params),
  });
  if (!res.ok) {
    const body = await res.json().catch(() => ({}));
    if (res.status === 404) throw new AuthError("User not found. Please register first.", "not_found");
    if (res.status === 403 && body.reason === "not_verified")
      throw new AuthError("Please verify your email before logging in. Check your inbox.", "not_verified");
    if (res.status === 403 && body.reason === "not_active")
      throw new AuthError("Your account has been deactivated.", "not_active");
    if (res.status === 401) throw new AuthError("Invalid password.", "invalid_password");
    throw new AuthError("Login failed", "unknown");
  }
  return res.json();
}

async function registerUser(params: RegisterParams): Promise<{ message: string }> {
  const res = await fetch("/auth/register", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(params),
  });
  if (res.status === 409) throw new AuthError("User already exists", "conflict");
  if (!res.ok) throw new AuthError("Registration failed", "unknown");
  return res.json();
}

async function resendVerification(email: string): Promise<{ message: string }> {
  const res = await fetch("/auth/resend-verification", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ email }),
  });
  if (!res.ok) throw new AuthError("Failed to resend verification email", "unknown");
  return res.json();
}

export function useLoginMutation() {
  return useMutation({
    mutationFn: loginUser,
  });
}

export function useRegisterMutation() {
  return useMutation({
    mutationFn: registerUser,
  });
}

export function useResendVerificationMutation() {
  return useMutation({
    mutationFn: resendVerification,
  });
}
