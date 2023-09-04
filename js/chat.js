import { publishEvent, subscribeToEvent } from "./bus.js";
import { chatContainer,
    chatSendMessageBox,
    chatSendMessageBtn,
    currentChannel,
    loadOlderMessagesBtn } from "./elements.js";
import { showElement } from "./utils.js";

const CHANNEL_MESSAGES_COUNT=10;
subscribeToEvent("displayChannelChat",onDisplayChannelChat);
subscribeToEvent("get_messages_result",onGetMessagesResult);
subscribeToEvent("resetChat",onResetChat);
loadOlderMessagesBtn.addEventListener("click",onLoadOlderMessages);
chatSendMessageBtn.addEventListener("click",onSendMessage);

function onSendMessage(){
    var channelId=parseInt(localStorage.getItem("channelId"));
    console.log("Channel publish:"+channelId);
    var message=document.getElementById("chatSendMessageBox").value;
    publishEvent("socket_command",{
        "kind":"publish",
        "topicId":channelId,
        "content":message
    })
}

function onDisplayChannelChat(ev){
    console.log(ev.detail);
    var currentChannelId=parseInt(localStorage.getItem("currentChannelId"));
    if(currentChannelId!=ev.detail.id){
        localStorage.setItem("currentChannelId",ev.detail.id);
        currentChannel.innerText=ev.detail.name;
        resetChat();
        var event_payload=get_newest_messages(currentChannel.id,CHANNEL_MESSAGES_COUNT);
        publishEvent("socket_command",event_payload);
    }
   
}
function onResetChat(ev){
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


