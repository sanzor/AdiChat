
var socket=null;

 function connect (url){
   socket=new WebSocket("ws://localhost:8080/ws/user/adi/cookie/cook");
    socket.onopen=function (e){
        console.log("Connection established");
        command_get_subscriptions();
    }
    socket.onmessage=function(ev){
      
        if(ev.data=="ping"){
            console.log("Received ping");
            socket.send("pong");
        }
        var message=JSON.parse(ev.data);
        console.log("got message from ws:");
        console.log(message);
        handle_callback_message(message);

    }
    socket.onclose=function(e){
        console.log(`Socket closed with code: ${e.code} , reason: ${e.reason}`);
        console.log(`Connection died`);
    }
}

function command_subscribe(channelToSubscribe){
    var message={
        "command":"subscribe",
        "topic":channelToSubscribe
    }
    console.log("sending command:" + JSON.stringify(message));
    socket.send(JSON.stringify(message))
}
 function command_unsubscribe(topic){
    var message={
        "command":"unsubscribe",
        "topic":topic
    }
    console.log("sending command:" + JSON.stringify(message));
     socket.send(JSON.stringify(message));
}

function command_get_subscriptions(){
   
    var message={
        "command":"get_subscriptions"
    }
    console.log("sending command:" + JSON.stringify(message));
    socket.send(JSON.stringify(message));
    
}
function command_disconnect(url){
    socket.close();
}

function command_publish(message,topic){
    var message={
        "command":"publish",
        "topic":topic,
        "message":message
    }
    console.log("sending command:" + JSON.stringify(message));
    socket.send(JSON.stringify(message));
}
