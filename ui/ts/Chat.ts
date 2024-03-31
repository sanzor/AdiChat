import { publishCommand, publishEvent, subscribeToEvent } from "./bus";
import { Channel } from "./Domain/Channel";
import {CURRENT_CHANNEL, USER} from "./constants";
import { 
    chatSendMessageBox,
    loadOlderMessagesBtn} from "./elements";
import { PUBLISH_MESSAGE, RESET_CHAT, RESET_CHAT_DOM, SELF_PUBLISH_MESSAGE, SET_CHAT ,SET_CHAT_DOM} from "./events";
import { getItemFromStorage } from "./utils";
import { User } from "./Domain/User";
import { PublishCommand } from "./Domain/Commands/PublishCommand";


loadOlderMessagesBtn?.addEventListener("click",onLoadOlderMessages);
subscribeToEvent(SET_CHAT,onSetChat);
subscribeToEvent(RESET_CHAT,onResetChat);
subscribeToEvent("get_messages_result",onGetMessagesResult);

subscribeToEvent(SELF_PUBLISH_MESSAGE,onSelfPublish);



function onResetChat(_:CustomEvent){
    publishEvent(RESET_CHAT_DOM,{});
}
function onSelfPublish(ev:CustomEvent){
    console.log("Inside send message");
    var currentChannel=getItemFromStorage<Channel>(CURRENT_CHANNEL)!;
    var user=getItemFromStorage<User>(USER);
    console.log("Channel publish:"+currentChannel);
    var message={ 
         kind:PUBLISH_MESSAGE,
         userId:user?.id,
         topicId: Number.parseInt(currentChannel.id),
         message:chatSendMessageBox.value} as PublishCommand;
    publishCommand(message);
}




function onSetChat(ev:CustomEvent){
    console.log(ev.detail);
    console.log(ev.detail.name);
    setChatWithChannel(ev.detail);
    publishEvent(SET_CHAT_DOM,ev.detail);
}

function setChatWithChannel(channel:Channel){
}


function onLoadOlderMessages(ev:Event){
    // var count=Array.length(chatMessageContainer.children);
    // var eventPayload=get_older_messages(currentChannel.id,count,CHANNEL_MESSAGES_COUNT);
    // publishEvent(SOCKET_COMMAND,eventPayload);
}
function onGetMessagesResult(ev:CustomEvent){
   
}

function get_older_messages(id:number,startIndex:number,count:number){
    var message={
        kind:"get-oldest-channel-messages",
        topicId:id,
        count:count,
        startIndex:startIndex
    };
    
    return message;
}

function get_newest_messages(id:number,count:number){
    var message={
        kind:"get-newest-channel-messages",
        id:id,
        count:count,
    };
    
    return message;
}




