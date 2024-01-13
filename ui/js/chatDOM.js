


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