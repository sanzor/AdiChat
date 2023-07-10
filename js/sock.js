
var socket=null;

 function connect (){
    
    var url=urlBox.value;
    console.log(url);
    socket=new WebSocket(url);
    socket.onopen=function (e){
        console.log("\nConnection established\n");
        command_get_subscriptions();
    }
    socket.onmessage=function(ev){
      
        if(ev.data=="ping"){
            console.log("\nReceived ping");
            socket.send("pong");
        }
        var message=JSON.parse(ev.data);
        console.log("\nReceived: ");
        console.log(message);
        handle_callback_message(message);

    }
    socket.onclose=function(e){
        console.log(`Socket closed with code: ${e.code} , reason: ${e.reason}`);
        reset();
       
    }
}

function command_subscribe(topic){
    var message={
        "command":"subscribe",
        "topic":topic
    }
    console.log("\nSending:" + JSON.stringify(message));
    socket.send(JSON.stringify(message))
}
 function command_unsubscribe(topic){
    console.log(topic);
    var message={
        "command":"unsubscribe",
        "topic":topic
    }
     console.log("\nSending:" + JSON.stringify(message));
     socket.send(JSON.stringify(message));
}

function command_get_subscriptions(){
   
    var message={
        "command":"get_subscriptions"
    }
    console.log("\nSending:" + JSON.stringify(message));
    socket.send(JSON.stringify(message));
    
}
function command_disconnect(url){
    socket.close();
    resetSubscriptionTable();
}

function command_publish(topic,message){
    var toSend={
        "command":"publish",
        "topic":topic,
        "message":message
    }
    console.log("topic:"+topic);
    console.log("\nSending:" + JSON.stringify(toSend));
    socket.send(JSON.stringify(toSend));
}


