import { CommandResult } from "./CommandResult";

export interface UnsubscribeResult extends CommandResult{
    topicId:string;
}