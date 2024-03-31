import { Channel } from "../Channel";
import { User } from "../User";

export interface DisplayMessage{
    user:User;
    topic:Channel;
    message:string;
}