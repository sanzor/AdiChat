import { Command } from "./Command";

export interface PublishMessage extends Command {
    kind: "publish";
    userId: number;
    topicId: number;
    message: string;
}