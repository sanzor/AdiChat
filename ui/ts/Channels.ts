import { publishCommand, publishEvent, subscribeToEvent ,unsubscribeFromEvent} from "./bus";
import { ChatMessage } from "./Domain/ChatMessage";
import { Channel } from "./Domain/Channel";
import{channelsContainer} from "./elements";
import { setItemInStorage,getItemFromStorage } from "./utils";
import {subscribeBtn,subscribeBox} from "./elements";
import { KIND, SOCKET_COMMAND, CURRENT_CHANNEL} from "./constants";
import {
    REFRESH_CHANNELS_COMMAND_RESULT, 
    SUBSCRIBE_COMMAND_RESULT,
    SUBSCRIBE_COMMAND_RESULT_U,

    UNSUBSCRIBE_COMMAND, 
    UNSUBSCRIBE_BUTTON_CLICK,
    UNSUBSCRIBE_COMMAND_RESULT,
    UNSUBSCRIBE_COMMAND_RESULT_U ,
    
    SET_CHAT,
    RESET_CHAT,
    NEW_INCOMING_MESSAGE,
    SET_CHANNELS,
    ADD_CHANNEL,
    REMOVE_CHANNEL,
    CHANNEL_CLICK} from "./events";
import { SubscribeCommand } from "./Domain/Commands/SubscribeCommand";
import { SubscribeResult } from "./Domain/Responses/CommandResult/SubscribeResult";
import { UnsubscribeResult } from "./Domain/Responses/CommandResult/UnsubscribeResult";

const CHANNELS="channels";

const TOPIC_ID="topicId";


subscribeToEvent(SUBSCRIBE_COMMAND_RESULT_U,onSubscribeResultU);
subscribeToEvent(UNSUBSCRIBE_COMMAND_RESULT_U,onUnSubscribeResultU);
subscribeToEvent(REFRESH_CHANNELS_COMMAND_RESULT,onRefreshChannelsCommandResult);
subscribeToEvent(NEW_INCOMING_MESSAGE,onNewIncomingMessage);

subscribeToEvent(UNSUBSCRIBE_BUTTON_CLICK,onUnsubscribeAsync);
subscribeToEvent(CHANNEL_CLICK,onChannelClick);

function onNewIncomingMessage(ev:CustomEvent){
     var message=ev.detail as ChatMessage;
     var currentChannel=getItemFromStorage<Channel>(CURRENT_CHANNEL);
     if(currentChannel ==null){
        return;
     }
     updateChannelsOnMessage(message);
     
 }

 function updateChannelsOnMessage(data:ChatMessage){
    var targetChannel=Array.from(channelsContainer.children)
        .filter(child=>(child as HTMLDivElement).innerText==data.topicId.toString());
}




subscribeBtn.addEventListener("click",onSubscribeAsync);


function onChannelClick(event:CustomEvent){
    var channel=event.detail;
    setItemInStorage(CURRENT_CHANNEL,channel);
    publishEvent(SET_CHAT,channel);
}
function onRefreshChannelsCommandResult(ev:CustomEvent){
    var channels=ev.detail as Array<Channel>;
   
    if(channels.length==0 || !channels){
        publishEvent(SET_CHANNELS,[]);
        return;

        
    }
    setItemInStorage(CHANNELS,channels);
    publishEvent(SET_CHANNELS,channels);
    var currentChannel=getItemFromStorage<Channel>(CURRENT_CHANNEL)!;
    
    if(currentChannel==null || !channels.find(x=>x.id==currentChannel.id)){
        setItemInStorage(CURRENT_CHANNEL,channels[0]);
        publishEvent(SET_CHAT,channels[0]);
        return;
    }
    publishEvent(SET_CHAT,currentChannel);

}

async function onSubscribeAsync(){
   
    function onOwnSubscribeResult(ev:CustomEvent,resolve:(value: SubscribeResult | PromiseLike<SubscribeResult>) => void,_:(reason?: any) => void){
        unsubscribeFromEvent(SUBSCRIBE_COMMAND_RESULT,(_:any)=>{
            console.log("unsubscribed from subscribe_result");
        });
       resolve(ev.detail as SubscribeResult);
    }
   
    var subscribeResult =await new Promise<SubscribeResult>((resolve,reject)=>{
        console.log(KIND);
        subscribeToEvent(SUBSCRIBE_COMMAND_RESULT,(ev:CustomEvent)=>onOwnSubscribeResult(ev,resolve,reject));
        publishCommand({kind:"subscribe",topic: subscribeBox.value} as SubscribeCommand);
    });
    var _=await handleSubscribeResultAsync(subscribeResult);
    
    
}
const state={
    first_chat_set:false
};
async function handleSubscribeResultAsync(subscribeResult:SubscribeResult){

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
    var targetChannel:Channel=subscribeResult.topic!;
    var existingChannels=getItemFromStorage<Array<Channel>>(CHANNELS);
    if(!existingChannels){
        setItemInStorage(CURRENT_CHANNEL,targetChannel);
        
        publishEvent(SET_CHAT,targetChannel);
        setItemInStorage(CHANNELS,[targetChannel]);
        publishEvent(ADD_CHANNEL,targetChannel);
        return;
    }
    var newChannelList=[...existingChannels,targetChannel];
    var currentChannel=getItemFromStorage<Channel>(CURRENT_CHANNEL);
    console.log(currentChannel);
    if(currentChannel==null ||  !existingChannels.find(x=>x.id!=currentChannel?.id)){
        setItemInStorage(CURRENT_CHANNEL,targetChannel);
        setItemInStorage(CHANNELS,newChannelList);
        publishEvent(SET_CHAT,targetChannel);
        publishEvent(ADD_CHANNEL,targetChannel);
        return;
    }
    
    setItemInStorage(CHANNELS,newChannelList);
    publishEvent(ADD_CHANNEL,targetChannel);
    if(state.first_chat_set==false){
        state.first_chat_set=true;
        publishEvent(SET_CHAT,targetChannel);
    }
}

async function onUnsubscribeAsync(event:CustomEvent):Promise<void>{
    var channel=event.detail;
    var unsubscribeResult=await new Promise<UnsubscribeResult>((resolve,_)=>{
        subscribeToEvent(UNSUBSCRIBE_COMMAND_RESULT,(ev:CustomEvent)=>{
            unsubscribeFromEvent(REFRESH_CHANNELS_COMMAND_RESULT,function(_:any){
                console.log("unsbuscribed from refresh_channels after unsubscribe from channel");
            });
            resolve(ev.detail as UnsubscribeResult);
        });
        publishEvent(SOCKET_COMMAND,{[KIND]:UNSUBSCRIBE_COMMAND,"topicId":channel.id});
    });
    var _=await handleUnsubscribeResultAsync(unsubscribeResult);
     
}
async function handleUnsubscribeResultAsync(unsubscribeResult:UnsubscribeResult):Promise<any>{
    console.log(unsubscribeResult);
    if(unsubscribeResult.result=="not_joined"){
        console.log("Not joined");
        return "not_joined";
    }
    if(unsubscribeResult.result!="ok"){
        var message="Could not unsubscribe from channel";
        return new Error(message);
    }
    var existingChannels=getItemFromStorage<Array<Channel>>(CHANNELS);
    
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
    var currentChannel=getItemFromStorage<Channel>(CURRENT_CHANNEL);
    var newExistingChannels=existingChannels.filter(x=>x.id!=unsubscribeResult.topicId);
    setItemInStorage(CHANNELS,newExistingChannels);
    publishEvent(REMOVE_CHANNEL,{[TOPIC_ID]:unsubscribeResult.topicId});
    console.log(unsubscribeResult);
    if(unsubscribeResult.topicId==currentChannel?.id && newExistingChannels && newExistingChannels.length>0){
        console.log(newExistingChannels);
        
        console.log("inside last if");
        setItemInStorage(CURRENT_CHANNEL,newExistingChannels[0]);
        publishEvent(SET_CHAT,newExistingChannels[0]);
        
    }
}





function setChannels(channels:Array<Channel>):Array<Channel>{
    setItemInStorage(CHANNELS,channels);
    //updateChannelsContainer(channels);
    return channels;
}




function onSubscribeResultU(ev:CustomEvent):void{
    var channels=setChannels(ev.detail.subscriptions);
    if(channels.length==0){
        publishEvent(RESET_CHAT,channels[0]);
        return;
    } 
}



function onUnSubscribeResultU(ev:CustomEvent){
    var channels=setChannels(ev.detail.subscriptions);
    if(channels.length==0){
        publishEvent(RESET_CHAT,{});
        return;
    }
    publishEvent(SET_CHAT,channels.slice(-1));
}


