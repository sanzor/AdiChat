

function onConnect(){
    connect();
    var disconnectBtn=document.getElementById("disconnectBtn");
    var connectBtn=document.getElementById("connectBtn");
    var subscribeBtn=document.getElementById("subscribeBtn");
    var subscribeBox=document.getElementById("subscribeBox");
    var urlBox=document.getElementById("urlBox");
    urlBox.setAttribute("disabled",true);
    disconnectBtn.disabled=false;
    connectBtn.disabled=true;
    subscribeBtn.disabled=false;
    subscribeBox.disabled=false;
}
function onDisconnect(){
    command_disconnect();
    reset();
    
}
function reset(){
    var connectBtn=document.getElementById("connectBtn");
    var disconnectBtn=document.getElementById("disconnectBtn");
    var subscribeBox=document.getElementById("subscribeBox");
    var urlBox=document.getElementById("urlBox");
    urlBox.disabled=false;
    connectBtn.disabled=false;
    disconnectBtn.disabled=true;
    subscribeBtn.disabled=true;
    subscribeBox.disabled=true;
}
function subscribe(){
    var channelToSubscribe=document.getElementById("subscribeBox").value;
    command_subscribe(channelToSubscribe);
}

function onPublish(){
    var channel=document.getElementById("currentChannelNameLabel").value;
    var message=document.getElementById("chatSendMessageBox").value;
    command_publish(channel,message);
}
