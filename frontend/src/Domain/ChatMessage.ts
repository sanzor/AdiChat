 // Example: "550e8400-e29b-41d4-a716-446655440000"

export interface ChatMessage{
    id:number|null;
    tempId:string|null;
    userId:number;
    topicId:number;
    message:string;
    created_at:number;
    status:string
};
export const SENDING="sending";
export const SENT="sent";