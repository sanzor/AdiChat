import { Message } from "./message";

export interface ChatMessage extends Message{
    kind:""
    userId:number;
    topicId:number;
    message:string;
}