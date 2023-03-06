
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

function command_subscribe(topic){
    var message={
        "command":"subscribe",
        "topic":topic
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

function handle_callback_message(data){
    console.log("Command:"+ data.command +', result:'+ data.result);
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
        command_get_subscriptions();
    }
}
function callback_unsubscribe(data){
    if(data.result=="ok"){
        // channel=data.topic;
        // removeSubscriptionRow(channel);
        command_get_subscriptions();
    }
}
function callback_get_messages(data){
    console.log(data.messages);
}
function callback_get_subscriptions(data){
    var subscriptions=data.result;
    createSubscriptionTable(subscriptions);
}

function createSubscriptionTable(subscriptions){
    var table=document.getElementById("channelTable");
    table.innerHTML='';
   
    if(subscriptions==='no_subscriptions'){
        return;
    }
    
    subscriptions.forEach(element => {
       createSubscriptionRow(element);
    });
}

function createSubscriptionRow(channelName){
    var table=document.getElementById("channelTable");

    var channelRow=document.createElement("tr");
    channelRow.id=channelName+'_channel_row';

    var cell1=document.createElement("td");
    var label=document.createElement("label");
    label.innerText=channelName;
    cell1.appendChild(label);
    
    var cell2=document.createElement("td");
    var unsubscribeBtn=document.createElement("button");
    unsubscribeBtn.id=channelName+'_unsubscribe_btn';
    unsubscribeBtn.innerText="X";
    unsubscribeBtn.onclick=function(){command_unsubscribe(channelName);};
    cell2.appendChild(unsubscribeBtn);

    channelRow.appendChild(cell1);
    channelRow.appendChild(cell2);
    table.appendChild(channelRow);
}

function removeSubscriptionRow(){
    var table=document.getElementById('channelTable');
    var row=document.getElementById(channel+'channel_row');
    table.removeChild(row);
}

function get_existing_rows(){
    var table=document.getElementById("channelTable");
    
}


