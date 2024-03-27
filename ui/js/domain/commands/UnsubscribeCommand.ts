import { Message } from "../message";

export interface UnsubscribeCommand extends Message {
    kind: "unsubscribe";
    topicId: number;
}