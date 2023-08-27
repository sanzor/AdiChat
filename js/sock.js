import { publishEvent, subscribeToEvent } from "./bus.js";
import config from "./config.js";
export{connect};

subscribeToEvent("close_socket",onCloseSocketCommand);
subscribeToEvent("socket_command",onSendCommand);
window.addEventListener("beforeunload",onUnload);
var socket=null;

function onCloseSocketCommand(){
    if(socket){
        console.log("closing websocket");
        socket.close();
    }
}
function onUnload(){
    publishEvent("close_socket",{});
}
function get_url(){
    var user=JSON.parse(localStorage.user);
    var url= `${config.baseWsUrl}/ws/id/${user.id}`;
    console.log("Url:"+url);
    return url;
}
 function connect (){
    var url=get_url();
    console.log("\nAttempting connect to:"+url+"\n");
    
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
        publishEvent("socketReceive",message);

    }
    socket.onclose=function(e){
        console.log(`Socket closed with code: ${e.code} , reason: ${e.reason}`);
        publishEvent("socketClosed",{});
       
    }
}

function onDomContentLoaded(){

}
function onSendCommand(ev){
    var data=ev.detail;
    console.log(`\nSending [${data.kind}] command : ${data} to socket\n`);
    switch(data.kind){
        case "subscribe": command_subscribe(data.topic);break;
        case "unsubscribe" : command_unsubscribe(data.topicId);break;
        case "get_subscriptions": command_get_subscriptions();break;
        case "publish" :command_publish(data.topicId,data.content);break;
        case "disconnect":command_disconnect();break;
        case  "get_newest_messages": command_get_newest_messages(data.topicId,data.count);break;
        case  "get_older_messages": command_get_older_messages(
            data.topicId,
            data.startIndex,
            data.count);break;
    }
}
function command_subscribe(topic){
    console.log(topic);
    var message={
        "command":"subscribe",
        "topic":topic
    };
    console.log("\nSending:" + JSON.stringify(message));
    console.log(socket);
    socket.send(JSON.stringify(message))
}
 function command_unsubscribe(topicId){
    console.log(topicId);
    var message={
        "command":"unsubscribe",
        "topicId":topicId
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
function command_disconnect(){
    publishEvent("close_socket",{});
    resetSubscriptionTable();
}

function command_publish(topic,message){
    var toSend={
        "command":"publish",
        "topicId":topic,
        "content":message
    };
    console.log("topic:"+topic);
    console.log("\nSending:" + JSON.stringify(toSend));
    socket.send(JSON.stringify(toSend));
}

function command_get_newest_messages(topicId,count){
    
    var message={
        "command":"get_newest_messages",
        "topicId":topicId,
        "count":count,
        
    }
    console.log(message);
    socket.send(JSON.stringify(message));
}
function command_get_older_messages(topicId,startIndex,count){
    var message={
        "command":"get_older_messages",
        "topicId":topicId,
        "startIndex":startIndex,
        "count":count,
        
    }
    console.log(message);
    socket.send(JSON.stringify(message));
}

