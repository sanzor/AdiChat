import { publishCommand, publishEvent, subscribeToEvent } from "./bus.js";
import { Channel } from "./Domain/Channel.js";
import { KIND,CHANNEL_ID, MESSAGE_CONTENT, CURRENT_CHANNEL, USER} from "./constants.js";
import { 
    chatSendMessageBox,
    loadOlderMessagesBtn} from "./elements.js";
import { PUBLISH_MESSAGE, RESET_CHAT, RESET_CHAT_DOM, SELF_PUBLISH_MESSAGE, SET_CHAT ,SET_CHAT_DOM} from "./events.js";
import { getItemFromStorage } from "./utils.js";
import { User } from "./Domain/User.js";
import { PublishCommand } from "./Domain/Commands/PublishCommand";


loadOlderMessagesBtn?.addEventListener("click",onLoadOlderMessages);
subscribeToEvent(SET_CHAT,onSetChat);
subscribeToEvent(RESET_CHAT,onResetChat);
subscribeToEvent("get_messages_result",onGetMessagesResult);

subscribeToEvent(SELF_PUBLISH_MESSAGE,onSelfPublish);



function onResetChat(_){
    publishEvent(RESET_CHAT_DOM,{});
}
function onSelfPublish(ev:CustomEvent){
    
    console.log("Inside send message");
    var currentChannel=getItemFromStorage<Channel>(CURRENT_CHANNEL)!;
    var user=getItemFromStorage<User>(USER);
    console.log("Channel publish:"+currentChannel);
    var toSend={
        [KIND]:PUBLISH_MESSAGE,
        "userId":user?.id,
        [CHANNEL_ID]:currentChannel.id,
        [MESSAGE_CONTENT]:chatSendMessageBox.value
    } as PublishCommand;
    
    console.log(toSend);
    publishCommand(toSend);
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

function get_older_messages(id,startIndex,count){
    var message={
        kind:"get-oldest-channel-messages",
        topicId:id,
        count:count,
        startIndex:startIndex
    };
    
    return message;
}

function get_newest_messages(id,count){
    var message={
        kind:"get-newest-channel-messages",
        id:id,
        count:count,
    };
    
    return message;
}




