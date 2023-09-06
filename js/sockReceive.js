
import { publishEvent ,subscribeToEvent} from "./bus.js  ";

subscribeToEvent("socketReceive",onSocketReceive);
function onSocketReceive(ev){
    var data=ev.detail;
    if(data.kind=="chat"){
        handle_chat_message(data);
    }
    if(data.kind=="command_result"){
        handle_command_result(data);
    }
    if(data.kind="user_event"){
        handle_user_event(data);
    }

    
}

function onNewChatMessage(data){
   
}
function handle_chat_message(data){
    publishEvent("new_message",data.detail);
    
    
}
function handle_command_result(data){
    if(data.command=="subscribe"){
        callback_subscribe(data);
    }
    if(data.command=="unsubscribe"){
        callback_unsubscribe(data);
    }
    if(data.command=="get_subscriptions"){
        console.log("publishing subs");
        
        publishEvent("updateChannels",data.result);
    }
    if(data.command=="get_newest_messages"){
        callback_get_newest_messages(data);
    }
    if(data.command=="get_older_messages"){
        callback_get_older_messages(data);
    }
}
function handle_user_event(data){
    if(data.user_event_kind=="subscribe"){
        handle_user_event_subscribe(data);
    }
    if(data.user_event_kind=="unsubscribe"){
        handle_user_event_unsubscribe(data);
    }
}

function handle_user_event_subscribe(data){
    console.log("Publishing update channels");
    publishEvent("subscribe_result_u",data.subscriptions);
   
}
function handle_user_event_unsubscribe(data){
    console.log("Publishing update channels");
    publishEvent("unsubscribe_result_u",data.subscriptions);
}
function callback_subscribe(data){
    if(data.result=="ok"){
        publishEvent("subscribe_result",data.subscriptions);
    }
    if(data.result="already_subscribed"){
        console.log("\nAlready subscribed to topic:",data.topic,"\n");
    }
}
function callback_unsubscribe(data){
    if(data.result=="ok"){
        publishEvent("unsubscribe_result",data.subscriptions);
        publishEvent("resetChat",{});
    }
}
function callback_get_newest_messages(data){
    publishEvent("getNewestMessages",data.result)
    console.log(data.messages);
}
function callback_get_older_messages(data){
    publishEvent("getOlderMessages",data.result);
}


