
import{PUBLISH_MESSAGE, RESET_CHAT_DOM, SELF_PUBLISH_MESSAGE} from '../js/events';
import { publishEvent, subscribeToEvent } from './bus';
import { CURRENT_CHANNEL } from './constants';
import { chatSendMessageBox, currentChannel } from './elements';
import { getItemFromStorage } from './utils';
const APPEND_MESSAGE_DOM="append_message";

subscribeToEvent(APPEND_MESSAGE_DOM,onAppendedMessage);
subscribeToEvent(RESET_CHAT_DOM,onResetChat);
subscribeToEvent(SET_CHAT_DOM,onSetChat);
chatSendMessageBtn.addEventListener("click",onSendMessage);

function onSetChat(ev){
    console.log(channel.name);
    currentChannel.innerText=ev.detail.name;
}
function onSendMessage(_){
    var message=chatSendMessageBox.value;
    var user=getItemFromStorage("user");
    var channel=getItemFromStorage(CURRENT_CHANNEL);
    var toSend={
        ["user"]:user["name"],
        ["topic"]:channel,
        ["message"]:message
    }
    console.log(toSend);
    publishEvent(APPEND_MESSAGE_DOM,toSend);
    publishEvent(SELF_PUBLISH_MESSAGE,message);
}

function onAppendedMessage(ev){
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