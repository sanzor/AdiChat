
import{PUBLISH_MESSAGE,SET_CHAT_DOM, RESET_CHAT_DOM, SELF_PUBLISH_MESSAGE} from './events.js';
import { publishEvent, subscribeToEvent } from './bus.js';
import { Channel } from './domain/Channel.js';
import { CURRENT_CHANNEL } from './constants.js';
import { chatSendMessageBox, currentChannel,chatContainer, chatSendMessageBtn } from './elements.js';
import { getItemFromStorage } from './utils.js';
import { ChatMessage } from './domain/chatMessage.js';
const APPEND_MESSAGE_DOM="append_message";

subscribeToEvent(APPEND_MESSAGE_DOM,onAppendedMessage);
subscribeToEvent(RESET_CHAT_DOM,onResetChat);
subscribeToEvent(SET_CHAT_DOM,onSetChat);
chatSendMessageBtn.addEventListener("click",onSendMessage);

function onSetChat(ev:CustomEvent){
    console.log(ev.detail);
    var channel=ev.detail;
    console.log("On set chat dom");
    console.log(channel.name);
    currentChannel.innerText=ev.detail.name;
}
function onSendMessage(_:Event){
    var message=chatSendMessageBox.value;
    var user=getItemFromStorage<User>("user")!;
    var channel=getItemFromStorage<Channel>(CURRENT_CHANNEL);
    var appendMessage={
        ["user"]:user.name,
        ["topic"]:channel,
        ["message"]:message
    }

    console.log(appendMessage);
    publishEvent(APPEND_MESSAGE_DOM,appendMessage);
    publishEvent(SELF_PUBLISH_MESSAGE,message);
}

function onAppendedMessage(ev:CustomEvent){
    var message=ev.detail;
    console.log("Inside on new message");
    var messageElement=createChatMessageContainer(message);
    chatContainer.appendChild(messageElement);
}

function onResetChat(_:CustomEvent){
    localStorage.removeItem("currentChannelId");
    chatContainer.innerHTML='';
    currentChannel.innerText='';
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
    var content=document.createElement("div");
    content.classList.add("chatMessageContent");
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