

function onConnect(){
    connect();
    var disconnectBtn=document.getElementById("disconnectBtn");
    var connectBtn=document.getElementById("connectBtn");
    disconnectBtn.disabled=false;
    connectBtn.disabled=true;
}
function onDisconnect(){
    command_disconnect();
    var connectBtn=document.getElementById("connectBtn");
    var disconnectBtn=document.getElementById("disconnectBtn");
    connectBtn.disabled=false;
    disconnectBtn.disabled=true;
    
}
function subscribe(){
    var topic=document.getElementById("topicBox").innerText;
    command_subscribe(topic);
}

function clickHandler(element){
    element.addEventListener("click",function(){
        
    })
}