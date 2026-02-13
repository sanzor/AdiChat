import React, { Component, ReactNode } from "react";

class ErrorBoundary extends Component<{ children: ReactNode }, { hasError: boolean }> {
  constructor(props: { children: ReactNode }) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError(error: any) {
    console.error("⚠️ Caught an error in ErrorBoundary:", error);
    return { hasError: true };
  }

  componentDidCatch(error: any, info: any) {
    console.error("Error Details:", error, info);
  }

  render() {
    if (this.state.hasError) {
      return <h2>Something went wrong in ChatProvider.</h2>;
    }
    return this.props.children;
  }
}

export default ErrorBoundary;
