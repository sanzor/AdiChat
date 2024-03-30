import { Message } from "./Message";

export interface ChatMessage extends Message{
    kind:""
    userId:number;
    topicId:number;
    message:string;
}