import { publishEvent, subscribeToEvent } from "./bus.js";
import config from "./config.js";
import { CHANNEL_ID, MESSAGE_CONTENT, SOCKET_COMMAND } from "./constants.js";
import{
    SUBSCRIBE_COMMAND,
    UNSUBSCRIBE_COMMAND,
    REFRESH_CHANNELS_COMMAND,
    PUBLISH_MESSAGE,
    GET_NEWEST_MESSAGES,
    GET_OLDER_MESSAGES,
    SOCKET_RECEIVE,
    SOCKET_CLOSED,
    } from "./events.js";
export{connect,send};

const SOCKET_COMMAND_KIND="kind";
const SOCKET_COMMAND_CONTENT="content";

subscribeToEvent("close_socket",onCloseSocketCommand);
subscribeToEvent(SOCKET_COMMAND,onAsyncCommand);
window.addEventListener("beforeunload",onUnload);
var socket=null;

// function send(commandKind,content){
//     publishEvent(SOCKET_COMMAND,{SOCKET_COMMAND_KIND:commandKind,SOCKET_COMMAND_CONTENT:content});
// }
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
function send(){

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
        console.log("\nReceived on socket: ");
        console.log(message);
        publishEvent(SOCKET_RECEIVE,message);

    }
    socket.onclose=function(e){
        console.log(`Socket closed with code: ${e.code} , reason: ${e.reason}`);
        publishEvent(SOCKET_CLOSED,{});
       
    }
}

function onDomContentLoaded(){

}


function onAsyncCommand(ev){
    var data=ev.detail;
    onCommand(data);
}
function onCommand(data){
    console.log(data);
    console.log(`\nSending [${data.kind}] command : ${data} to socket\n`);
    switch(data.kind){
        case SUBSCRIBE_COMMAND:   command_subscribe(data.topic);break;
        case UNSUBSCRIBE_COMMAND : command_unsubscribe(data.topicId);break;
        case REFRESH_CHANNELS_COMMAND: command_get_subscriptions();break;
        case PUBLISH_MESSAGE :command_publish(data);break;
        case "disconnect":command_disconnect();break;
        case  GET_NEWEST_MESSAGES: command_get_newest_messages(data.topicId,data.count);break;
        case  GET_OLDER_MESSAGES: command_get_older_messages(
            data.topicId,
            data.startIndex,
            data.count);break;
    }
}
function command_subscribe(topic){
    console.log("inside command sub");
    console.log(topic);
    var message={
        "command":SUBSCRIBE_COMMAND,
        "topic":topic
    };
    console.log("\nSending:" + JSON.stringify(message));
    console.log(socket);
    socket.send(JSON.stringify(message))
}
 function command_unsubscribe(topicId){
    console.log(topicId);
    var message={
        "command":UNSUBSCRIBE_COMMAND,
        "topicId":topicId
    }
     console.log("\nSending:" + JSON.stringify(message));
     socket.send(JSON.stringify(message));
}

function command_get_subscriptions(){
   
    var message={
        "command":REFRESH_CHANNELS_COMMAND
    }
    console.log("\nSending:" + JSON.stringify(message));
    socket.send(JSON.stringify(message));
    
}
function command_disconnect(){
    publishEvent("close_socket",{});
    resetSubscriptionTable();
}

function command_publish(data){
    var toSend={
        "command":PUBLISH_MESSAGE,
        "topicId":data[CHANNEL_ID],
        "content":data[MESSAGE_CONTENT]
    };
    console.log("\nSending:" + JSON.stringify(toSend));
    socket.send(JSON.stringify(toSend));
}

function command_get_newest_messages(topicId,count){
    
    var message={
        "command":GET_NEWEST_MESSAGES,
        "topicId":topicId,
        "count":count,
        
    }
    console.log(message);
    socket.send(JSON.stringify(message));
}
function command_get_older_messages(topicId,startIndex,count){
    var message={
        "command":GET_OLDER_MESSAGES,
        "topicId":topicId,
        "startIndex":startIndex,
        "count":count,
        
    }
    console.log(message);
    socket.send(JSON.stringify(message));
}

