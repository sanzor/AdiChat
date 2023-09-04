import { subscribeToEvent,publishEvent } from "./bus";
subscribeToEvent("new_message",onNewChatMessage);

function onNewChatMessage(ev){
   var selectedChannel=parseInt(localStorage.getItem("currentChannelId"));
   var newMessageChannelId=ev.detail.topic_id;
   if(newMessageChannelId==selectedChannel){
     publishEvent("new_chat_message",ev.detail);
   }
   else{
     publishEvent("new_channel_message",ev.detail);
   }
}