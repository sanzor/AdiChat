import { publishEvent, subscribeToEvent ,unsubscribeFromEvent} from "./bus.js";
import config from "./config.js";
import{channelsContainer} from "./elements.js";
import { getDataAsync, postDataAsync } from "./utils.js";
import {subscribeBtn} from "./elements.js";
import { 
    REFRESH_CHANNELS_COMMAND, 
    REFRESH_CHANNELS_COMMAND_RESULT, 
    SUBSCRIBE_COMMAND,
    SUBSCRIBE_COMMAND_RESULT,
    UNSUBSCRIBE_COMMAND, 
    UNSUBSCRIBE_COMMAND_RESULT } from "./commands.js";

subscribeToEvent("subscribe_result_u",onSubscribeResultU);
subscribeToEvent("unsubscribe_result_u",onUnSubscribeResultU);
subscribeToEvent(REFRESH_CHANNELS_COMMAND_RESULT,onRefreshChannels);
subscribeToEvent("new_channel_message",onNewMessage);


subscribeBtn.addEventListener("click",onSubscribe);

async function onSubscribe(){
   
    function onOwnSubscribeResult(ev,resolve,_){
        unsubscribeFromEvent(SUBSCRIBE_COMMAND_RESULT,(_)=>{
            console.log("unsubscribed from subscribe_result");
        });
       
       resolve(ev.detail);
    }
   
    var subscribeResult =await new Promise((resolve,reject)=>{
        subscribeToEvent(SUBSCRIBE_COMMAND_RESULT,(ev)=>onOwnSubscribeResult(ev,resolve,reject));
        publishEvent("socket_command",{"kind":SUBSCRIBE_COMMAND,"topic":subscribeBox.value});
      
    });
    
    var subs=await handleSubscribeResultAsync(subscribeResult);
    
    
}

async function handleSubscribeResultAsync(subscribeResult){
    function refreshAfterSubscribe(ev,resolve,reject){
        unsubscribeFromEvent(REFRESH_CHANNELS_COMMAND_RESULT,(_)=>{
            console.log("unsbuscribed from refresh_channels after subscribed to channel");
        });
        try{
           
            resolve(ev.detail);
        }catch(err){
            reject(e)
        }
    }
    if(subscribeResult.result!="ok" && subscribeResult.result!="already_subscribed"){
        var message="Could not subscribe to channel:"+subscribeBox.value;
        console.log(message);
        return new Error(message);
    }
    var getSubscriptionsResult=await new Promise((resolve,reject)=>{
        subscribeToEvent(REFRESH_CHANNELS_COMMAND_RESULT,(ev)=>refreshAfterSubscribe(ev,resolve,reject));
        publishEvent("socket_command",{"kind":REFRESH_CHANNELS_COMMAND});
    });
    console.log("ending subscribe");
    console.log(getSubscriptionsResult);
    return getSubscriptionsResult.result;
    

}

async function onUnsubscribeClick(){
    function onOwnUnsubscribeResult(ev,resolve,_){
        unsubscribeFromEvent(REFRESH_CHANNELS_COMMAND_RESULT,function(_){
            console.log("unsbuscribed from refresh_channels after unsubscribe from channel");
        });
        resolve(ev.detail);
    }
    
   
    var unsubscribeResult=new Promise((resolve,reject)=>{
        subscribeToEvent(UNSUBSCRIBE_COMMAND_RESULT,(ev)=>onOwnUnsubscribeResult(ev,resolve,reject));
        publishEvent("socket_command",{"kind":UNSUBSCRIBE_COMMAND,"topicId":channel.id});
    });
    var rez=await handleUnsubscribeResultAsync(unsubscribeResult);
     
}
async function handleUnsubscribeResultAsync(unsubscribeResult){
    function refreshAfterUnsubscribe(ev,resolve,reject){
        unsubscribeFromEvent(REFRESH_CHANNELS_COMMAND,function(_){
            console.log("unsbuscribed from refresh_channels after unsubscribe from channel");
        });
        try{

            resolve(ev.detail);
        }catch(err){
            reject(err);
        }
    }
    if(unsubscribeResult.result=="not_joined"){
        console.log("Not joined");
        return "not_joined";
    }
    if(unsubscribeResult.result!="ok"){
        var message="Could not unsubscribe from channel "
        return new Error(message);
    }
    var getSubscriptionsResult=await new Promise((resolve,reject)=>{
        subscribeToEvent(REFRESH_CHANNELS_COMMAND_RESULT,(ev)=>refreshAfterUnsubscribe(ev,resolve,reject));
        publishEvent("socket_command",{"kind":REFRESH_CHANNELS_COMMAND});
    });
}

function onUnSubscribeResult(ev){
    publishEvent("socket_command",{"kind":REFRESH_CHANNELS_COMMAND});
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


function onNewMessage(ev){
   // channelsContainer.children.forEach(elem=>)
}

function setChannels(channels){
    localStorage.setItem("channels",JSON.stringify(channels));
    updateChannelsContainer(channels);
    return channels;
}

function onRefreshChannels(ev){
    console.log("Inside onRefresh Channels");
    console.log(ev.detail);
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
    unsubscribeBtn.onclick=onUnsubscribeClick;
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
