
import { publishEvent } from "./bus.js";
import config from "./config.js";
import {CHANNEL_ID, CHANNEL} from "./constants.js";
import { subscribeToEvent } from "./bus.js";
import {UNSUBSCRIBE_BUTTON_CLICK , SET_CHANNELS,ADD_CHANNEL,REMOVE_CHANNEL ,CHANNEL_CLICK } from "./events.js";
subscribeToEvent(REMOVE_CHANNEL,onRemoveChannelFromDOM);
subscribeToEvent(ADD_CHANNEL,onAddChannelToDOM);
subscribeToEvent(SET_CHANNELS,onSetDOMChannels);




function getChannelDomElementById(topicId){
    
    var elem=[...channelsContainer.children].find(x=>{
         var channelId=x.getAttribute(CHANNEL_ID);
         return channelId==topicId;
    });
    return elem;
    
 }

function onSetDOMChannels(ev){
    var channels=ev.detail;
    setChannelsContainer(channels);
}

function onAddChannelToDOM(ev){
    var channel=ev.detail;
    var newChannel=createChannelContainer(channel);
    channelsContainer.appendChild(newChannel);
}
function onRemoveChannelFromDOM(ev){
    var channelElement=getChannelDomElementById(ev.detail.topicId);
    channelsContainer.removeChild(channelElement);
}

function resetChannels(){
    var channelsContainer=document.getElementById("channelsContainer");
    channelsContainer.innerHTML='';
}
function setChannelsContainer(channels){
    console.log(channels);
    resetChannels();
    var headerRow=document.createElement("tr");
    var h1=document.createElement("th");
    var h2=document.createElement("th");
    headerRow.appendChild(h1);
    headerRow.appendChild(h2);
    if(channels==='no_channels'){
        return parentContainer;
    }
    var channels=createChannels(channels);
    channels.forEach(element=>{
        channelsContainer.appendChild(element);
    });
    return channelsContainer;
   
}
function createChannels(channels){
    console.log(channels);
    var channels=channels.map(createChannelContainer);
    return channels;
}
function createChannelContainer(channel){
    var channelContainer=document.createElement("span");
    channelContainer.setAttribute(CHANNEL_ID,channel.id);
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

function createUnsubscribeChannelButton(channel){
    var unsubscribeBtn=document.createElement("button");
    unsubscribeBtn.id=channel.id+'_unsubscribe_btn';
    unsubscribeBtn.innerText="X";
    unsubscribeBtn.setAttribute("class","channelRowUnsubscribeBtn");
    unsubscribeBtn.setAttribute(CHANNEL,JSON.stringify(channel));
    unsubscribeBtn.onclick=function(){
        publishEvent(UNSUBSCRIBE_BUTTON_CLICK,channel);
    };
    return unsubscribeBtn;
}
function createDisplayChannelChatButton(channel){
    var channelButton=document.createElement("button");
    channelButton.id=channel.id;
    channelButton.setAttribute('content',channel.name);
    channelButton.setAttribute("class",'button');
    channelButton.setAttribute("style","channelButton");
    channelButton.textContent=channel.name;
    channelButton.onclick=function(_){ publishEvent(CHANNEL_CLICK,channel)};
    return channelButton;
}
function createNewMessagesBox(channel){
    var newMessagesBox=document.createElement("p");
    newMessagesBox.setAttribute("class","newMessagesBox");
    newMessagesBox.innerHTML="0";
    return newMessagesBox;
}
