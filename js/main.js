

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
    var channelToSubscribe=document.getElementById("subscribeToChannelBox").textContent;
    command_subscribe(channelToSubscribe);
}

function clickHandler(element){
    element.addEventListener("click",function(){
        
    })
}