
import { ChatMessage } from "./Domain/ChatMessage";
import { CommandResult } from "./Domain/Responses/CommandResult/CommandResult";
import { SubscribeResult } from "./Domain/Responses/CommandResult/SubscribeResult";
import { UnsubscribeResult } from "./Domain/Responses/CommandResult/UnsubscribeResult";
import { ReceivedMessage } from "./Domain/Responses/ReceivedMessage";
import { publishEvent ,subscribeToEvent} from "./bus";
import { 
        REFRESH_CHANNELS_COMMAND, 
        REFRESH_CHANNELS_COMMAND_RESULT,
        RESET_CHAT, SOCKET_RECEIVE, SUBSCRIBE_COMMAND,
        SUBSCRIBE_COMMAND_RESULT, 
        SUBSCRIBE_COMMAND_RESULT_U, 
        UNSUBSCRIBE_COMMAND,
        UNSUBSCRIBE_COMMAND_RESULT, 
        UNSUBSCRIBE_COMMAND_RESULT_U,
        NEW_INCOMING_MESSAGE,
        GET_NEWEST_MESSAGES,
        GET_OLDER_MESSAGES} from "./events";

subscribeToEvent(SOCKET_RECEIVE,onSocketReceive);
function onSocketReceive(ev:any){
   
    var data=ev.detail as ReceivedMessage;
    if(data.kind=="chat"){
        handle_chat_message(data as ChatMessage);
        return;
    }
    if(data.kind=="command_result"){
        handle_command_result(data as CommandResult);
        return;
    }
    if(data.kind="user_event"){
        
        console.log("Received user event");
        console.log(data as any);
        
    }

    
}

function onNewChatMessage(data:any){
   
}
function handle_chat_message(data:any){
    console.log("New Message !!!");
    console.log(data.detail);
    publishEvent(NEW_INCOMING_MESSAGE,data.detail);
    
    
}
function handle_command_result(data:CommandResult){
    if(data.command==SUBSCRIBE_COMMAND){
        callback_subscribe(data);
    }
    if(data.command==UNSUBSCRIBE_COMMAND){
        callback_unsubscribe(data as UnsubscribeResult);
    }
    if(data.command==REFRESH_CHANNELS_COMMAND){
        
        console.log(data);
        publishEvent(REFRESH_CHANNELS_COMMAND_RESULT,data.result);
    }
    if(data.command==GET_NEWEST_MESSAGES){
        callback_get_newest_messages(data);
    }
    if(data.command==GET_OLDER_MESSAGES){
        callback_get_older_messages(data);
    }
}
function handle_user_event(data:any){
    if(data.user_event_kind==SUBSCRIBE_COMMAND){
        handle_user_event_subscribe(data);
    }
    if(data.user_event_kind==UNSUBSCRIBE_COMMAND){
        handle_user_event_unsubscribe(data);
    }
}

function handle_user_event_subscribe(data:any){
    console.log("Publishing update channels");
    publishEvent(SUBSCRIBE_COMMAND_RESULT_U,data);
   
}
function handle_user_event_unsubscribe(data:any){
    console.log("Publishing update channels");
    publishEvent(UNSUBSCRIBE_COMMAND_RESULT_U,data);
}
function callback_subscribe(data:SubscribeResult){
    console.log(data);
    publishEvent(SUBSCRIBE_COMMAND_RESULT,data);
   
}
function callback_unsubscribe(data:UnsubscribeResult){
    console.log(data);
    if(data.result=="ok"){
        publishEvent(UNSUBSCRIBE_COMMAND_RESULT,data);
        publishEvent(RESET_CHAT,{});
    }
}
function callback_get_newest_messages(data:any){
    publishEvent("getNewestMessages",data.result)
    console.log(data.messages);
}
function callback_get_older_messages(data:any){
    publishEvent("getOlderMessages",data.result);
}


