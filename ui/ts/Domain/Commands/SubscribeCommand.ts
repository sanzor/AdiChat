import { Command } from "./Command";
export interface SubscribeCommand extends Command{
    kind:"subscribe";
    topic:string;   
}
