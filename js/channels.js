import { publishEvent, subscribeToEvent } from "./bus.js";

import{channelsContainer} from "./elements.js";
subscribeToEvent("updateChannels",onUpdateChannels);
subscribeToEvent("new_message",onNewMessage);

function onNewMessage(ev){
    var channels=
}
function onUpdateChannels(ev){
    console.log("Received update channels");
    createChannelsContainer(ev.detail);
    if(ev.detail.length==0){
        return;
    }
    publishEvent("displayChannelChat",ev.detail[0])
}

function resetSubscriptionTable(){
    var table=document.getElementById("channelsContainer");
    table.innerHTML='';
}
function createChannelsContainer(subscriptions){
    console.log(subscriptions);
    var table=document.getElementById("channelsContainer");
    table.innerHTML='';
    var headerRow=document.createElement("tr");
    var h1=document.createElement("th");
    var h2=document.createElement("th");
    h1.innerText="Channel";
    h2.innerText="-";
    headerRow.appendChild(h1);
    headerRow.appendChild(h2);
    if(subscriptions==='no_subscriptions'){
        return;
    }
    subscriptions.forEach(element => {
        createChannelContainer(element);
    });
   
}

function createChannelContainer(channel){
    console.log("inside channel container");
    var container=document.getElementById("channelsContainer");
    var channelContainer=document.createElement("span");
    channelContainer.setAttribute("class","channelRow");
    channelContainer.setAttribute("channelId",channel.id);
    var unsubscribeBtn=createUnsubscribeChannelButton(channel);
    var openChatButton=createDisplayChannelChatButton(channel);
    var newMessagesBox=createNewMessagesBox(channel);
    channelContainer.appendChild(newMessagesBox);
    channelContainer.appendChild(unsubscribeBtn);
    channelContainer.appendChild(openChatButton);
    container.appendChild(channelContainer);
}


function createIdElement(Id){
    var p=document.createElement("p");
    p.innerHTML=Id;
    p.setAttribute("display","none");
    return p;
}
function createUnsubscribeChannelButton(channel){
    var unsubscribeBtn=document.createElement("button");
    unsubscribeBtn.id=channel.id+'_unsubscribe_btn';
    unsubscribeBtn.innerText="X";
    unsubscribeBtn.setAttribute("class","channelRowUnsubscribeBtn");
    
    unsubscribeBtn.onclick=function(){publishEvent("socket_command",{"kind":"unsubscribe","topicId":channel.id});};
    return unsubscribeBtn;
}
function createDisplayChannelChatButton(channel){
    console.log(channel);
    var channelButton=document.createElement("button");
    channelButton.id=channel.id;
    channelButton.setAttribute('content',channel.name);
    channelButton.setAttribute("class",'button');
    channelButton.setAttribute("style","channelButton");
    channelButton.textContent=channel.name;
    channelButton.onclick=function(args){ publishEvent("displayChannelChat",channel)};
    return channelButton;
}
function createNewMessagesBox(channel){
    var newMessagesBox=document.createElement("p");
    newMessagesBox.setAttribute("class","newMessagesBox");
    newMessagesBox.innerHTML="0";
    return newMessagesBox;
}
function removeSubscriptionRow(){
    var table=document.getElementById('channelsContainer');
    var row=document.getElementById(channel+'channel_row');
    table.removeChild(row);
}

function updateChannelsOnMessage(data){
    var targetChannel=channelsContainer.children.filter(child=>child.innerText==data.topic);
}
