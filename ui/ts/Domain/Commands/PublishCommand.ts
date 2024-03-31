import { Command } from "./Command";

export interface PublishCommand extends Command {
    kind: "publish";
    userId: number;
    topicId: number;
    message: string;
}