import { Message } from "../Message";

export interface UnsubscribeCommand extends Message {
    kind: "unsubscribe";
    topicId: number;
}