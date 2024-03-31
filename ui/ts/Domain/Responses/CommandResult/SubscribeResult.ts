import { Channel } from "../../Channel";
import { CommandResult } from "./CommandResult";

export interface SubscribeResult extends CommandResult{
    topic?:Channel;
}