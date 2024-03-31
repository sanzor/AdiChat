
import { publishEvent,subscribeToEvent } from "./bus";
import {CHANNEL_ID, CHANNEL} from "./constants";
import {UNSUBSCRIBE_BUTTON_CLICK , SET_CHANNELS,ADD_CHANNEL,REMOVE_CHANNEL ,CHANNEL_CLICK, NEW_INCOMING_MESSAGE } from "./events";
import { channelsContainer } from "./elements";
import { Channel } from "./Domain/Channel";
subscribeToEvent(REMOVE_CHANNEL,onRemoveChannelFromDOM);
subscribeToEvent(ADD_CHANNEL,onAddChannelToDOM);
subscribeToEvent(SET_CHANNELS,onSetDOMChannels);
subscribeToEvent(NEW_INCOMING_MESSAGE,onNewMessageOnChannel)



function getChannelDomElementById(topicId:number):HTMLElement|undefined{
    const childrenArray = Array.from(channelsContainer.children) as HTMLElement[];
    const elem = childrenArray.find((x: HTMLElement) => {
        const channelId = parseInt(x.getAttribute('CHANNEL_ID')!);
        return channelId === topicId;
    });
    return elem;
    
 }

function onSetDOMChannels(ev:CustomEvent){
    var channels=ev.detail;
    setChannelsContainer(channels);
}
function onNewMessageOnChannel(ev:CustomEvent){
    var message=ev.detail;
    var channelElement=getChannelDomElementById(ev.detail.topicId);
}
function onAddChannelToDOM(ev:CustomEvent){
    var channel=ev.detail;
    var newChannel=createChannelContainer(channel);
    channelsContainer.appendChild(newChannel);
}
function onRemoveChannelFromDOM(ev:CustomEvent){
    var channelElement=getChannelDomElementById(ev.detail.topicId);
    if(channelElement!=null){
        channelsContainer.removeChild(channelElement);
    }
    
}

function resetChannels(){
    channelsContainer.innerHTML='';
}
function setChannelsContainer(channels:Array<Channel> | string):HTMLDivElement{
    console.log(channels);
    resetChannels();
    var headerRow=document.createElement("tr");
    var h1=document.createElement("th");
    var h2=document.createElement("th");
    headerRow.appendChild(h1);
    headerRow.appendChild(h2);
    if(channels==='no_channels'){
        return channelsContainer;
    }
    var channelContainers=createChannelContainers(channels as Array<Channel>);
    channelContainers.forEach(element=>{
        channelsContainer.appendChild(element);
    });
    return channelsContainer;
   
}
function createChannelContainers(channels:Array<Channel>):Array<HTMLSpanElement>{
    console.log(channels);
    var items=channels.map(createChannelContainer);
    return items;
}
function createChannelContainer(channel:Channel):HTMLSpanElement{
    var channelContainer=document.createElement("span");
    channelContainer.setAttribute(CHANNEL_ID,channel.id);
    channelContainer.setAttribute("class","channelRow");
    channelContainer.setAttribute("channelData",JSON.stringify(channel));
    var unsubscribeBtn=createUnsubscribeChannelButton(channel);
    var openChatButton=createDisplayChannelChatButton(channel);
    var newMessagesBox=createNewMessagesBox(channel);
    channelContainer.appendChild(newMessagesBox);
    channelContainer.appendChild(unsubscribeBtn);
    channelContainer.appendChild(openChatButton);
    return channelContainer;
}

function createUnsubscribeChannelButton(channel:Channel):HTMLButtonElement{
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
function createDisplayChannelChatButton(channel:Channel):HTMLButtonElement{
    var channelButton=document.createElement("button");
    channelButton.id=channel.id;
    channelButton.setAttribute('content',channel.name);
    channelButton.setAttribute("class",'button');
    channelButton.setAttribute("style","channelButton");
    channelButton.textContent=channel.name;
    channelButton.onclick=function(_){ publishEvent(CHANNEL_CLICK,channel)};
    return channelButton;
}
function createNewMessagesBox(channel:Channel):HTMLParagraphElement{
    var newMessagesBox=document.createElement("p");
    newMessagesBox.setAttribute("class","newMessagesBox");
    newMessagesBox.innerHTML="0";
    return newMessagesBox;
}
