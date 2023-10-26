import { publishEvent, subscribeToEvent ,unsubscribeFromEvent} from "./bus.js";
import config from "./config.js";
import{channelsContainer} from "./elements.js";
import { getDataAsync, postDataAsync } from "./utils.js";
import {subscribeBtn} from "./elements.js";

subscribeToEvent("subscribe_result_u",onSubscribeResultU);
subscribeToEvent("unsubscribe_result_u",onUnSubscribeResultU);
subscribeToEvent("refresh_channels",onRefreshChannels);
subscribeToEvent("new_channel_message",onNewMessage);


subscribeBtn.addEventListener("click",onSubscribe);

async function onSubscribe(){
    function refreshAfterSubscribe(ev,resolve,reject){
        unsubscribeFromEvent("refresh_channels",(_)=>{
            console.log("unsbuscribed from refresh_channels");
        });
        try{
            resolve(ev.detail.subscriptions);
        }catch(err){
            reject(e)
        }
    }
    function onOwnSubscribeResult(ev,resolve,reject){
        unsubscribeFromEvent("subscribe_result",(ev)=>{
            console.log("unsubscribed from subscribe_result");
        });
        console.log("Own subscribe result");
        console.log(ev.detail);
        if(ev.detail.result!="ok"){
            if(ev.topic.result=="already_subscribed"){
                console.log("already_subscribed");
                resolve("already_subscribed");
                return;
            }
            reject(ev.detail.result);
        }
        resolve(ev.detail);
    }
    console.log("inside onsubscribe");
    var subscribeResult =await new Promise((resolve,reject)=>{
        subscribeToEvent("subscribe_result",(ev)=>onOwnSubscribeResult(ev,resolve,reject));
        publishEvent("socket_command",{"kind":"subscribe","topic":subscribeBox.value});
      
    });
    console.log(subscribeResult);
    if(subscribeResult.result!="ok"){
        console.log("Could not subscribe to channel:"+subscribeBox.value);
        return;
    }
    var getSubscriptionsResult=await new Promise((resolve,reject)=>{
        subscribeToEvent("refresh_channels",(ev)=>refreshAfterSubscribe(ev,resolve,reject));
        publishEvent("refresh_channels",{});
       
    });
   
    
}

function subscribeAndClose(eventName,subscribeAction,unsubscribeAction){
    subscribeToEvent(eventName,(ev)=>{
        unsubscribeFromEvent(eventName,unsubscribeAction());
        subscribeAction();
    })
}

function onNewMessage(ev){
   // channelsContainer.children.forEach(elem=>)
}

function setChannels(channels){
    localStorage.setItem("channels",JSON.stringify(channels));
    updateChannelsContainer(channels);
    return channels;
}

function onRefreshChannels(ev){
    var channels=setChannels(ev.detail);
    if(channels.length==0){
        return;
    }
    publishEvent("setChat",channels[0]);
}


function onSubscribeResultU(ev){
    var channels=setChannels(ev.detail.subscriptions);
    if(channels.length==0){
        publishEvent("setChat",channels[0]);
        return;
    } 
}

function onUnSubscribeResult(ev){
    var channels=setChannels(ev.detail.subscriptions);
    if(channels.length==0){
        publishEvent("resetChat",{});
        return;
    }
    var currentChannel=JSON.parse(localStorage.getItem("currentChannel"));
    if(ev.detail.topicId==currentChannel.id){
        publishEvent("setChat",channels[0]);
        return;
    }
    
}

function onUnSubscribeResultU(ev){
    var channels=setChannels(ev.detail.subscriptions);
    if(channels.length==0){
        publishEvent("resetChat",{});
        return;
    }
    publishEvent("setChat",channels.slice(-1));
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
   
    var channelButton=document.createElement("button");
    channelButton.id=channel.id;
    channelButton.setAttribute('content',channel.name);
    channelButton.setAttribute("class",'button');
    channelButton.setAttribute("style","channelButton");
    channelButton.textContent=channel.name;
    channelButton.onclick=function(args){ publishEvent("setChat",channel)};
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
