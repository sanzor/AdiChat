
import{PUBLISH_MESSAGE, RESET_CHAT_DOM} from '../js/events';
import { publishEvent, subscribeToEvent } from './bus';
import { chatSendMessageBox } from './elements';
const APPEND_MESSAGE="append_message";

subscribeToEvent(APPEND_MESSAGE,onNewMessage);
subscribeToEvent(RESET_CHAT_DOM,onResetChat);
chatSendMessageBtn.addEventListener("click",onSendMessage);

function onSendMessage(){
    publishEvent(APPEND_MESSAGE,toSend);
    publishEvent(PUBLISH_MESSAGE,chatSendMessageBox.value);
}

function onNewMessage(ev){
    var message=ev.detail;
    console.log("Inside on new message");
    var messageElement=createChatMessageContainer(message);
    chatContainer.appendChild(messageElement);
}

function onResetChat(_){
    localStorage.removeItem("currentChannelId");
    chatContainer.innerHTML=null;
    currentChannel.innerText=null;
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