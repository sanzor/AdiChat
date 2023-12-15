import { publishEvent, subscribeToEvent ,unsubscribeFromEvent} from "./bus.js";
import config from "./config.js";
import{channelsContainer} from "./elements.js";
import { getDataAsync, postDataAsync } from "./utils.js";
import {subscribeBtn} from "./elements.js";
import { KIND, SOCKET_COMMAND, TOPIC} from "./constants.js";
import { 
    REFRESH_CHANNELS_COMMAND, 
    REFRESH_CHANNELS_COMMAND_RESULT, 

    SUBSCRIBE_COMMAND,
    SUBSCRIBE_COMMAND_RESULT,
    SUBSCRIBE_COMMAND_RESULT_U,

    UNSUBSCRIBE_COMMAND, 
    UNSUBSCRIBE_COMMAND_RESULT,
    UNSUBSCRIBE_COMMAND_RESULT_U ,
    
    SET_CHAT,
    RESET_CHAT,
    NEW_CHANNEL_MESSAGE} from "./events.js";

const CHANNELS="channels";
const CURRENT_CHANNEL="current_channel";
const CHANNEL="channel";
const REMOVE_CHANNEL_FROM_LIST="remove_channel";
const ADD_CHANNEL_TO_LIST="add_channel";
const TOPIC_ID="topicId";
const CHANNEL_ID="channelId";

subscribeToEvent(SUBSCRIBE_COMMAND_RESULT_U,onSubscribeResultU);
subscribeToEvent(UNSUBSCRIBE_COMMAND_RESULT_U,onUnSubscribeResultU);
subscribeToEvent(REFRESH_CHANNELS_COMMAND_RESULT,onRefreshChannels);
subscribeToEvent(NEW_CHANNEL_MESSAGE,onNewMessage);
subscribeToEvent(REMOVE_CHANNEL_FROM_LIST,onRemoveChannelFromDOM);
subscribeToEvent(ADD_CHANNEL_TO_LIST,onAddChannelToDOM);

function getChannelDomElementById(topicId){
    
   var elem=[...channelsContainer.children].find(x=>{
        var channelId=x.getAttribute(CHANNEL_ID);
        return channelId==topicId;
   });
   return elem;
   
}

function getDOMChannels(){

}
function getItemFromStorage(Key){return JSON.parse(localStorage.getItem(Key));}
function setItemInStorage(Key,Value){ localStorage.setItem(Key,JSON.stringify(Value));}


subscribeBtn.addEventListener("click",onSubscribeAsync);

function onRefreshChannels(ev){
    var channels=setChannels(ev.detail);
    if(channels.length==0){
        return;
    }
    var currentChannel=getItemFromStorage(CURRENT_CHANNEL);
    if(currentChannel){
        publishEvent(SET_CHAT,currentChannel);
    }
    else{

        publishEvent(SET_CHAT,channels[0]);
    }

}

async function onSubscribeAsync(){
   
    function onOwnSubscribeResult(ev,resolve,_){
        unsubscribeFromEvent(SUBSCRIBE_COMMAND_RESULT,(_)=>{
            console.log("unsubscribed from subscribe_result");
        });
       resolve(ev.detail);
    }
   
    var subscribeResult =await new Promise((resolve,reject)=>{
        console.log(KIND);
        subscribeToEvent(SUBSCRIBE_COMMAND_RESULT,(ev)=>onOwnSubscribeResult(ev,resolve,reject));
        publishEvent(SOCKET_COMMAND,{[KIND]:SUBSCRIBE_COMMAND,[TOPIC]:subscribeBox.value});
    });
    var _=await handleSubscribeResultAsync(subscribeResult);
    
    
}

async function handleSubscribeResultAsync(subscribeResult){
    console.log(subscribeResult.result);
    if(subscribeResult.result!="ok" && subscribeResult.result!="already_subscribed"){
        var message="Could not subscribe to channel:"+subscribeBox.value;
        console.log(message);
        return new Error(message);
    }
    var targetChannel={id:subscribeResult.topic.id,name:subscribeResult.topic.name};
    var existingChannels=getItemFromStorage(CHANNELS);
    var newChannelList=[...existingChannels,targetChannel];
    if(!newChannelList || newChannelList.length==0){
        publishEvent(SET_CHAT,targetChannel);
    }
    setItemInStorage(CHANNELS,newChannelList);
    publishEvent(ADD_CHANNEL_TO_LIST,targetChannel);
}

async function onUnsubscribeAsync(event){
    var unsubscribeResult=await new Promise((resolve,_)=>{
        subscribeToEvent(UNSUBSCRIBE_COMMAND_RESULT,(ev)=>{
            unsubscribeFromEvent(REFRESH_CHANNELS_COMMAND_RESULT,function(_){
                console.log("unsbuscribed from refresh_channels after unsubscribe from channel");
            });
            resolve(ev.detail);
        });
        var targetUnsubscribeBtn=document.getElementById(event.target.id);
        var channel=JSON.parse(targetUnsubscribeBtn.getAttribute(CHANNEL));
        publishEvent(SOCKET_COMMAND,{[KIND]:UNSUBSCRIBE_COMMAND,"topicId":channel.id});
    });
    var _=await handleUnsubscribeResultAsync(unsubscribeResult);
     
}
async function handleUnsubscribeResultAsync(unsubscribeResult){
    console.log(unsubscribeResult);
    if(unsubscribeResult.result=="not_joined"){
        console.log("Not joined");
        return "not_joined";
    }
    if(unsubscribeResult.result!="ok"){
        var message="Could not unsubscribe from channel";
        return new Error(message);
    }
    var existingChannels=getItemFromStorage(CHANNELS);
    if(!existingChannels){
        setItemInStorage(CHANNELS,[]);
        publishEvent(RESET_CHAT,{});
        return;
    }
    if(existingChannels.length==0){
        publishEvent(RESET_CHAT,{});
        return;
    }
    publishEvent(REMOVE_CHANNEL_FROM_LIST,{[TOPIC_ID]:unsubscribeResult.topicId});
    var currentChannel=getItemFromStorage(CURRENT_CHANNEL);
    if(unsubscribeResult.topicId==currentChannel.id){
        publishEvent(SET_CHAT,existingChannels[0]);
    }
}


function onNewMessage(ev){
   // channelsContainer.children.forEach(elem=>)
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
function setChannels(channels){
    setItemInStorage(CHANNELS,channels);
    updateChannelsContainer(channels);
    return channels;
}




function onSubscribeResultU(ev){
    var channels=setChannels(ev.detail.subscriptions);
    if(channels.length==0){
        publishEvent(RESET_CHAT,channels[0]);
        return;
    } 
}



function onUnSubscribeResultU(ev){
    var channels=setChannels(ev.detail.subscriptions);
    if(channels.length==0){
        publishEvent(RESET_CHAT,{});
        return;
    }
    publishEvent(SET_CHAT,channels.slice(-1));
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
    });
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

function createUnsubscribeChannelButton(channel){
    var unsubscribeBtn=document.createElement("button");
    unsubscribeBtn.id=channel.id+'_unsubscribe_btn';
    unsubscribeBtn.innerText="X";
    unsubscribeBtn.setAttribute("class","channelRowUnsubscribeBtn");
    unsubscribeBtn.setAttribute(CHANNEL,JSON.stringify(channel));
    unsubscribeBtn.onclick=onUnsubscribeAsync;
    return unsubscribeBtn;
}
function createDisplayChannelChatButton(channel){
   
    var channelButton=document.createElement("button");
    channelButton.id=channel.id;
    channelButton.setAttribute('content',channel.name);
    channelButton.setAttribute("class",'button');
    channelButton.setAttribute("style","channelButton");
    channelButton.textContent=channel.name;
    channelButton.onclick=function(args){ publishEvent(SET_CHAT,channel)};
    return channelButton;
}
function createNewMessagesBox(channel){
    var newMessagesBox=document.createElement("p");
    newMessagesBox.setAttribute("class","newMessagesBox");
    newMessagesBox.innerHTML="0";
    return newMessagesBox;
}

function updateChannelsOnMessage(data){
    var targetChannel=channelsContainer.children.filter(child=>child.innerText==data.topic);
}
