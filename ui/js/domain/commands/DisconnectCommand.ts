import { Command } from "./Command";

export interface DisconnectCommand extends Command {
    kind: "disconnect";
}
