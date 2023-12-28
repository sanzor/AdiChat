import { publishEvent, subscribeToEvent } from "./bus.js";
import { KIND, SOCKET_COMMAND,CHANNEL_ID, MESSAGE_CONTENT } from "./constants.js";
import { channelsContainer, chatContainer,
    chatSendMessageBox,
    chatSendMessageBtn,
    currentChannel,
    loadOlderMessagesBtn } from "./elements.js";
import { PUBLISH_MESSAGE, RESET_CHAT, SET_CHAT } from "./events.js";
import { showElement } from "./utils.js";

const CHANNEL_MESSAGES_COUNT=10;
subscribeToEvent("new_chat_message",onNewMessage);
subscribeToEvent(SET_CHAT,onSetChatWindow);
subscribeToEvent("get_messages_result",onGetMessagesResult);
subscribeToEvent(RESET_CHAT,onResetChat);

loadOlderMessagesBtn.addEventListener("click",onLoadOlderMessages);
chatSendMessageBtn.addEventListener("click",onSendMessage);

function onSendMessage(){
    var channelId=parseInt(localStorage.getItem("channelId"));
    console.log("Channel publish:"+channelId);
    var message=chatSendMessageBox.value;
    publishEvent(SOCKET_COMMAND,{
        [KIND]:PUBLISH_MESSAGE,
        [CHANNEL_ID]:channelId,
        [MESSAGE_CONTENT]:message
    });
}
function onNewMessage(ev){
    console.log("Inside on new message");
    appendMessageToChat(ev.detail);
}

function appendMessageToChat(message){
    var messageElement=createChatMessageContainer(message);
    chatContainer.appendChild(messageElement);
}
function onSetChatWindow(ev){
    console.log(ev.detail);
    console.log(ev.detail.name);
    setChatWithChannel(ev.detail);
    
   
}

function setChatWithChannel(channel){
    console.log(channel.name);
    currentChannel.innerText=channel.name;
    resetChat();
    var event_payload=get_newest_messages(currentChannel.id,CHANNEL_MESSAGES_COUNT);
    publishEvent(SOCKET_COMMAND,event_payload);
    
}
function setChatWithDefaultChannel(){
    //to make this with event and async , not leak channel logic
    if(channelsContainer.children.length==0){
        console.log("No channels present");
        //implement default view of chat
        return;
    }
    var defaultChannel=channelsContainer.children[0];
    var channel=JSON.parse(defaultChannel.getAttribute("channel"));
    setChatWithChannel(channel);

}
function onResetChat(_){
    localStorage.removeItem("currentChannelId");
    chatContainer.innerHTML=null;
    currentChannel.innerText=null;
}

function onLoadOlderMessages(){
    var count=Array.length(chatMessageContainer.children);
    var eventPayload=get_older_messages(currentChannel.id,count,CHANNEL_MESSAGES_COUNT);
    publishEvent("socket_command",eventPayload);
}
function onGetMessagesResult(ev){
   
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



function createChatMessageContainer(data){
    var user=data.user;
    var topic=data.topic;
    var message=data.message;

    
    var icon=document.createElement("img");
    var content=document.createElement("div","chatMessageContent");
    var meta=document.createElement("div");

    meta.setAttribute("class","chatMessageMeta");
    meta.innerText=user;

    icon.setAttribute("class","chatMessageIcon");

    content.setAttribute("class","chatMessageContent");
    content.innerText=message;


    var chatMessageContainer=document.createElement("div");
    chatMessageContainer.setAttribute("class","chatMessageContainer");

    chatMessageContainer.appendChild(icon);
    chatMessageContainer.appendChild(meta);
    chatMessageContainer.appendChild(content);
    return chatMessageContainer;


}
function resetChat(){
    chatContainer.innerHTML=null;
}


