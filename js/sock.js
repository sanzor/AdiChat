
var socket=null;

 function connect (url){
   socket=new WebSocket("ws://localhost:8080/ws/user/adi/cookie/cook");
    socket.onopen=function (e){
        alert("Connection established");
    }
    socket.onmessage=function(ev){
        alert(`Message received: ${ev.data}`);
        if(ev.data=="ping"){
            console.log("Received ping");
            socket.send("pong");
        }
        handle_message(ev);

    }
    socket.onclose=function(e){
        console.log(`Socket closed with code: ${e.code} , reason: ${e.reason}`);
        console.log(`Connection died`);
    }
    
    
}

function disconnect(url){
    socket.close();
}

function socket_publish(message,topic){
    var message={
        "command":"publish",
        "topic":topic,
        "message":message
    }
    socket.send(message)
}
function handle_message(ev){
    console.log("Received:"+ev.data);
}

function socket_subscribe(topic){
    var message={
        "command":"subscribe",
        "topic":topic
    }
    socket.send(message);
}
 function socket_unsubscribe(topic){
    var message={
        "command":"unsubscribe",
        "topic":topic
    }
     socket.send(message)
}


