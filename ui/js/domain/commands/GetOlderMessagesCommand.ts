import { Command } from "./Command";

export interface GetOlderMessagesCommand extends Command {
    kind: "get_older_messages";
    topicId: number;
    startIndex: number;
    count: number;
}