function handle_callback_message(data){
    console.log("Command:"+ data.command +', result:'+ data.result);
    if(data.kind=="chat"){
        handle_chat_message(data);
    }
    if(data.kind=="command_result"){
        handle_command_result(data);
    }

    
}
function handle_chat_message(data){
    console.log("Incoming chat message\n:"+data.message+"\n");
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

function callback_subscribe(data){
    if(data.result=="ok"){
        createSubscriptionTable(data.subscriptions);
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