import { publishEvent, subscribeToEvent } from "./bus.js";

import{channelsContainer} from "./elements.js";
subscribeToEvent("updateChannels",onUpdateChannels);
subscribeToEvent("new_channel_message",onNewMessage);

function onNewMessage(ev){
   // channelsContainer.children.forEach(elem=>)
}
function onUpdateChannels(ev){
    console.log("Inside update channels:");
    console.log(ev.detail);
    updateChannelsContainer(ev.detail);
    
    if(ev.detail.length==0){
        return;
    }
    publishEvent("displayChannelChat",ev.detail[0])
}

function resetChannelsContainer(){
    var channelsContainer=document.getElementById("channelsContainer");
    channelsContainer.innerHTML='';
}
function updateChannelsContainer(subscriptions){
    console.log(subscriptions);
    resetChannelsContainer();
    var headerRow=document.createElement("tr");
    var h1=document.createElement("th");
    var h2=document.createElement("th");
    headerRow.appendChild(h1);
    headerRow.appendChild(h2);
    if(subscriptions==='no_subscriptions'){
        return parentContainer;
    }
    var channels=createChannels(subscriptions);
    channels.forEach(element=>{
        channelsContainer.appendChild(element);
    })
    return channelsContainer;
   
}
function createChannels(subscriptions){
    console.log(subscriptions);
    var channels=subscriptions.map(createChannelContainer);
    return channels;
}
function createChannelContainer(channel){
    var channelContainer=document.createElement("span");
    channelContainer.setAttribute("channelId",channel.id);
    channelContainer.setAttribute("class","channelRow");
    channelContainer.setAttribute("channelData",channel);
    var unsubscribeBtn=createUnsubscribeChannelButton(channel);
    var openChatButton=createDisplayChannelChatButton(channel);
    var newMessagesBox=createNewMessagesBox(channel);
    channelContainer.appendChild(newMessagesBox);
    channelContainer.appendChild(unsubscribeBtn);
    channelContainer.appendChild(openChatButton);
    return channelContainer;
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
