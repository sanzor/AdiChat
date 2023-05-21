function handle_callback_message(data){
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
function handle_chat_message(data){
    console.log("\nIncoming chat message\n");
}
function handle_command_result(data){
    if(data.command=="subscribe"){
        callback_subscribe(data);
    }
    if(data.command=="unsubscribe"){
        callback_unsubscribe(data);
    }
    if(data.command=="get_subscriptions"){
        callback_get_subscriptions(data);
    }
    if(data.command=="get_messages"){
        callback_get_messages(data);
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
    createSubscriptionTable(data.subscriptions);
}
function handle_user_event_unsubscribe(data){
    createSubscriptionTable(data.subscriptions);
}
function callback_subscribe(data){
    if(data.result=="ok"){
        createSubscriptionTable(data.subscriptions);
    }
    if(data.result="already_subscribed"){
        console.log("\nAlready subscribed to topic:",data.topic,"\n");
    }
}
function callback_unsubscribe(data){
    if(data.result=="ok"){
        // channel=data.topic;
        // removeSubscriptionRow(channel);
        createSubscriptionTable(data.subscriptions);
    }
}
function callback_get_messages(data){
    console.log(data.messages);
}
function callback_get_subscriptions(data){
    var subscriptions=data.result;
    createSubscriptionTable(subscriptions);
}