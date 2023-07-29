import { chatContainer,
    chatSendMessageBox,
    chatSendMessageButton,
    currentChannel } from "./elements";

function onPublish(){
    var channel=currentChannel.innerText;
    var date=new Date().toDateString();
    console.log("Channel publish:"+channel);
    var message=document.getElementById("chatSendMessageBox").value;
    command_publish(channel,message);
}

function openChannelChat(channelName){
    console.log("inside channel chat");
    console.log("Channel:"+currentChannel.value);
    if(currentChannel.innerText!=channelName){
        console.log("end1");
        currentChannel.innerText=channelName;
        resetChat();
        //sent event to fetch chat messages from another channel
        //acel event face pull la mesaje si apoi apel la functiile de chat si anume reset inner text si set cu cele noi
    }
    console.log("end");
}

function updateChatOnMessage(data){
    var messageContainer=createChatMessageContainer(data);
    chatContainer.appendChild(messageContainer);
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


