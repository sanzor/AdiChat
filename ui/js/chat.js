import { publishEvent, subscribeToEvent } from "./bus.js";
import { KIND, SOCKET_COMMAND,CHANNEL_ID, MESSAGE_CONTENT, CURRENT_CHANNEL, ID} from "./constants.js";
import { channelsContainer, chatContainer,
    chatSendMessageBox,
    chatSendMessageBtn,
    currentChannel,
    loadOlderMessagesBtn, 
    loginFailMessage} from "./elements.js";
import { PUBLISH_MESSAGE, RESET_CHAT, RESET_CHAT_DOM, SELF_PUBLISH_MESSAGE, SET_CHAT ,SET_CHAT_DOM} from "./events.js";
import { getItemFromStorage,setItemInStorage } from "./utils.js";

const CHANNEL_MESSAGES_COUNT=10;

subscribeToEvent(SET_CHAT,onSetChat);
subscribeToEvent(RESET_CHAT,onResetChat);
subscribeToEvent("get_messages_result",onGetMessagesResult);

subscribeToEvent(SELF_PUBLISH_MESSAGE,onSelfPublish);



function onResetChat(_){
    publishEvent(RESET_CHAT_DOM,{});
}
function onSelfPublish(ev){
    console.log("Inside send message");
    var currentChannel=getItemFromStorage(CURRENT_CHANNEL);
    console.log("Channel publish:"+currentChannel);
    var toSend={
        [KIND]:PUBLISH_MESSAGE,
        [CHANNEL_ID]:currentChannel[ID],
        [MESSAGE_CONTENT]:chatSendMessageBox.value
    };
    console.log(toSend);
    publishEvent(SOCKET_COMMAND,toSend);
}

loadOlderMessagesBtn.addEventListener("click",onLoadOlderMessages);


function onSetChat(ev){
    var newestMessagesPromise=new Promise((resolve,reject)=>{
        subscribeToEvent()
    });
    console.log(ev.detail);
    console.log(ev.detail.name);
    setChatWithChannel(ev.detail);
    publishEvent(SET_CHAT_DOM,ev);
}

function setChatWithChannel(channel){
    // console.log(channel.name);
    // currentChannel.innerText=channel.name;
    // resetChat();
    // var event_payload=get_newest_messages(currentChannel.id,CHANNEL_MESSAGES_COUNT);
    // publishEvent(SOCKET_COMMAND,event_payload);
    // clearChatMessageBox();
    
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


function resetChat(){
    chatContainer.innerHTML=null;
}


