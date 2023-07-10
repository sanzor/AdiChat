
const connectBtn=document.getElementById("connectBtn");
const disconnectBtn=document.getElementById("disconnectBtn");
const subscribeBtn=document.getElementById("subscribeBtn");
const subscribeBox=document.getElementById("subscribeBox");
const urlBox=document.getElementById("urlBox");
const currentChannel=document.getElementById("currentChannelNameLabel");
const channels=document.getElementById("channelsContainer");

function onConnect(){
    connect();
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
    urlBox.disabled=false;
    connectBtn.disabled=false;
    disconnectBtn.disabled=true;
    subscribeBtn.disabled=true;
    subscribeBox.disabled=true;
}
function subscribe(){
    command_subscribe(subscribeBox.value);
}

function onPublish(){
    var channel=currentChannel.innerText;
    console.log("Channel publish:"+channel);
    var message=document.getElementById("chatSendMessageBox").value;
    command_publish(channel,message);
}
