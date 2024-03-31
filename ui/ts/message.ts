import { Channel } from "./Domain/Channel";
import { subscribeToEvent,publishEvent } from "./bus";
import { CURRENT_CHANNEL } from "./constants";
import { getItemFromStorage } from "./utils";
subscribeToEvent("new_message",onNewChatMessage);

function onNewChatMessage(ev:CustomEvent){
   var currentChannel=getItemFromStorage<Channel>(CURRENT_CHANNEL);
  
   var newMessageChannelId=ev.detail.topic_id;
   if(newMessageChannelId==currentChannel){
     publishEvent("new_chat_message",ev.detail);
   }
   else{
     publishEvent("new_channel_message",ev.detail);
   }
}