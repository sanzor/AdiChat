import { publishEvent, subscribeToEvent } from "./bus.js";
const channelsContainer=document.getElementById("channelsContainer");


subscribeToEvent("updateChannels",onUpdateChannels);


function onUpdateChannels(ev){
    console.log("Received update channels");
    createChannelsContainer(ev.detail);
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

function createChannelContainer(channelName){
    console.log("inside channel container");
    var container=document.getElementById("channelsContainer");
    var channelContainer=document.createElement("span");
    channelContainer.setAttribute("class","channelRow");
    var unsubscribeBtn=createUnsubscribeChannelButton(channelName);
    var openChatButton=createChannelButton(channelName);
    channelContainer.appendChild(unsubscribeBtn);
    channelContainer.appendChild(openChatButton);
    container.appendChild(channelContainer);
}



function createUnsubscribeChannelButton(channelName){
    var unsubscribeBtn=document.createElement("button");
    unsubscribeBtn.id=channelName+'_unsubscribe_btn';
    unsubscribeBtn.innerText="X";
    unsubscribeBtn.setAttribute("class","channelRowUnsubscribeBtn");
    console.log(channelName);
    unsubscribeBtn.onclick=function(){publishEvent("socket_command",{"kind":"unsubscribe","topic":channelName});};
    return unsubscribeBtn;
}
function createChannelButton(channelName){
    var channelButton=document.createElement("button");
    channelButton.id=channelName;
    channelButton.setAttribute('content',channelName);
    channelButton.setAttribute("class",'button');
    channelButton.setAttribute("style","channelButton");
    channelButton.textContent=channelName;
    channelButton.onclick=function(args){openChannelChat(channelName)};
    return channelButton;
}
function removeSubscriptionRow(){
    var table=document.getElementById('channelsContainer');
    var row=document.getElementById(channel+'channel_row');
    table.removeChild(row);
}

function updateChannelsOnMessage(data){
    var targetChannel=channelsContainer.children.filter(child=>child.innerText==data.topic);
}
