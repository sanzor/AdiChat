import { publishEvent, subscribeToEvent } from "./bus.js";
import { KIND, SOCKET_COMMAND,CHANNEL_ID, MESSAGE_CONTENT, CURRENT_CHANNEL, ID} from "./constants.js";
import { channelsContainer, chatContainer,
    chatSendMessageBox,
    chatSendMessageBtn,
    currentChannel,
    loadOlderMessagesBtn } from "./elements.js";
import { PUBLISH_MESSAGE, RESET_CHAT, SET_CHAT } from "./events.js";
import { getItemFromStorage,setItemInStorage } from "./utils.js";

const CHANNEL_MESSAGES_COUNT=10;
const APPEND_MESSAGE="append_message";

subscribeToEvent(APPEND_MESSAGE,onNewMessage);
subscribeToEvent(SET_CHAT,onSetChatWindow);
subscribeToEvent("get_messages_result",onGetMessagesResult);
subscribeToEvent(RESET_CHAT,onResetChat);
subscribeToEvent(MESSAGE_PUBLISHED,onMessagePublished);

function onMessagePublished(ev){

}

loadOlderMessagesBtn.addEventListener("click",onLoadOlderMessages);
chatSendMessageBtn.addEventListener("click",onSendMessage);

function onSendMessage(){
    var currentChannel=getItemFromStorage(CURRENT_CHANNEL);
    console.log("Channel publish:"+currentChannel);
    var toSend={
        [KIND]:PUBLISH_MESSAGE,
        [CHANNEL_ID]:currentChannel[ID],
        [MESSAGE_CONTENT]:chatSendMessageBox.value
    };
    console.log(toSend);
    publishEvent(APPEND_MESSAGE,toSend);
    publishEvent(SOCKET_COMMAND,{[KIND]:PUBLISH_MESSAGE,});
}
function onNewMessage(ev){
    var message=ev.detail;
    console.log("Inside on new message");
    var messageElement=createChatMessageContainer(message);
    chatContainer.appendChild(messageElement);
}

function changeMessageStatus(messageId,status){
    
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
    clearChatMessageBox();
    
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
    publishEvent(SOCKET_COMMAND,eventPayload);
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

function clearChatMessageBox(){
    chatSendMessageBox.value="";
}

function createChatMessageContainer(data){
    var user=data.user;
    var topic=data.topic;
    var message=data.message;

    var chatMessageContainer=document.createElement("div");
    chatMessageContainer.setAttribute("class","chatMessageContainer");

    var icon=document.createElement("img");
    var content=document.createElement("div","chatMessageContent");
    var meta=document.createElement("div");
    var status=document.createElement("div");

    meta.setAttribute("class","chatMessageMeta");
    meta.innerText=user;

    icon.setAttribute("class","chatMessageIcon");

    content.setAttribute("class","chatMessageContent");
    content.innerText=message;

    status.setAttribute("class","chatMessageStatusPending");
    status.innerText="tick";

  

    chatMessageContainer.appendChild(icon);
    chatMessageContainer.appendChild(meta);
    chatMessageContainer.appendChild(content);
    chatMessageContainer.appendChild(status);
    return chatMessageContainer;


}
function resetChat(){
    chatContainer.innerHTML=null;
}


