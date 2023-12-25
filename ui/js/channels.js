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
    UNSUBSCRIBE_BUTTON_CLICK,
    UNSUBSCRIBE_COMMAND_RESULT,
    UNSUBSCRIBE_COMMAND_RESULT_U ,
    
    SET_CHAT,
    RESET_CHAT,
    NEW_CHANNEL_MESSAGE,
    SET_CHANNELS,
    ADD_CHANNEL,
    REMOVE_CHANNEL} from "./events.js";

const CHANNELS="channels";
const CURRENT_CHANNEL="current_channel";
const TOPIC_ID="topicId";

const SET_DOM_CHANNELS="set_DOM_channels";

subscribeToEvent(SUBSCRIBE_COMMAND_RESULT_U,onSubscribeResultU);
subscribeToEvent(UNSUBSCRIBE_COMMAND_RESULT_U,onUnSubscribeResultU);
subscribeToEvent(REFRESH_CHANNELS_COMMAND_RESULT,onRefreshChannelsCommandResult);
subscribeToEvent(NEW_CHANNEL_MESSAGE,onNewMessage);
subscribeToEvent(UNSUBSCRIBE_BUTTON_CLICK,onUnsubscribeAsync);


function getItemFromStorage(Key){return JSON.parse(localStorage.getItem(Key));}
function setItemInStorage(Key,Value){ localStorage.setItem(Key,JSON.stringify(Value));}


subscribeBtn.addEventListener("click",onSubscribeAsync);


function onRefreshChannelsCommandResult(ev){
    var channels=ev.detail;
    setItemInStorage(CHANNELS,channels);
    if(channels.length==0 || !channels){
        if(!getItemFromStorage(CURRENT_CHANNEL)){
            setItemInStorage(CURRENT_CHANNEL,null);
        }
        publishEvent(SET_CHANNELS,[]);
        return;
    }
    publishEvent(SET_CHANNELS,channels);
    var currentChannel=getItemFromStorage(CURRENT_CHANNEL);
    if(!channels.find(x=>x.id==currentChannel.id)|| !currentChannel){
        publishEvent(SET_CHAT,channels[0]);
        return;
    }
    publishEvent(SET_CHAT,currentChannel);

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
    if(subscribeResult.result=='already_subscribed'){
        console.log("already subscribed");
        return;
    }
    var targetChannel={id:subscribeResult.topic.id,name:subscribeResult.topic.name};
    var existingChannels=getItemFromStorage(CHANNELS);
    if(!existingChannels){
        existingChannels=[];
    }
    var newChannelList=[...existingChannels,targetChannel];
    var currentChannel=getItemFromStorage(CURRENT_CHANNEL);
    if(!currentChannel){
        setItemInStorage(CURRENT_CHANNEL,targetChannel);
        publishEvent(SET_CHAT,targetChannel);
    }
    
    setItemInStorage(CHANNELS,newChannelList);
    publishEvent(ADD_CHANNEL,targetChannel);
}

async function onUnsubscribeAsync(event){
    var channel=event.detail;
    var unsubscribeResult=await new Promise((resolve,_)=>{
        subscribeToEvent(UNSUBSCRIBE_COMMAND_RESULT,(ev)=>{
            unsubscribeFromEvent(REFRESH_CHANNELS_COMMAND_RESULT,function(_){
                console.log("unsbuscribed from refresh_channels after unsubscribe from channel");
            });
            resolve(ev.detail);
        });
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
        setItemInStorage(CURRENT_CHANNEL,null);
        setItemInStorage(CHANNELS,[]);
        publishEvent(RESET_CHAT,{});
        return;
    }
    if(existingChannels.length==0){
        setItemInStorage(CURRENT_CHANNEL,null);
        publishEvent(RESET_CHAT,{});
        return;
    }
    var currentChannel=getItemFromStorage(CURRENT_CHANNEL);
    var newExistingChannels=existingChannels.filter(x=>x.id==unsubscribeResult.topicId);
    setItemInStorage(CHANNELS,newExistingChannels);
    publishEvent(REMOVE_CHANNEL,{[TOPIC_ID]:unsubscribeResult.topicId});
    if(unsubscribeResult.topicId==currentChannel.id){
        console.log("inside last if");
        setItemInStorage(CURRENT_CHANNEL,existingChannels[0]);
        publishEvent(SET_CHAT,existingChannels[0]);
    }
}


function onNewMessage(ev){
   // channelsContainer.children.forEach(elem=>)
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


function updateChannelsOnMessage(data){
    var targetChannel=channelsContainer.children.filter(child=>child.innerText==data.topic);
}
