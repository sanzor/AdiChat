import { Message } from "../Message";

export interface RefreshChannelsCommand extends Message {
    kind: "refresh_channels";
}