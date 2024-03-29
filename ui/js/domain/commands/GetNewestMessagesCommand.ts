import { Command } from "./Command";

export interface GetNewestMessagesCommand extends Command {
    kind: "get_newest_messages";
    topicId: number;
    count: number;
    startIndex:number;
}