

function onConnect(){
    connect();
    var disconnectBtn=document.getElementById("disconnectBtn");
    var connectBtn=document.getElementById("connectBtn");
    var subscribeBtn=document.getElementById("subscribeBtn");
    var subscribeBox=document.getElementById("subscribeBox");
    disconnectBtn.disabled=false;
    connectBtn.disabled=true;
    subscribeBtn.disabled=false;
    subscribeBox.disabled=false;
}
function onDisconnect(){
    command_disconnect();
    var connectBtn=document.getElementById("connectBtn");
    var disconnectBtn=document.getElementById("disconnectBtn");
    var subscribeBox=document.getElementById("subscribeBox");
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
    var textValue=document.getElementById("publishBox").value;
    command_publish("hello",textValue);
}
