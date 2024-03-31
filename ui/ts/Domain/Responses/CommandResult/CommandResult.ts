import { ReceivedMessage } from "../ReceivedMessage";

export interface CommandResult extends ReceivedMessage{
    kind:"command_result";
    command:string;
    result:any;
}