
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

    }
    socket.onclose=function(e){
        console.log(`Socket closed with code: ${e.code} , reason: ${e.reason}`);
        console.log(`Connection died`);
    }
    
    
}

function disconnect(url){
    socket.close();
}


