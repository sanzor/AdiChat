

function onConnect(){
    var disconnectBtn=document.getElementById("disconnectBtn");
    var connectBtn=document.getElementById("connectBtn");
    disconnectBtn.disabled=false;
    connectBtn.disabled=true;
    
}
function onDisconnect(){
    var connectBtn=document.getElementById("connectBtn");
    var disconnectBtn=document.getElementById("disconnectBtn");
    connectBtn.disabled=false;
    disconnectBtn.disabled=true;
    
}
function getChannels(){
    ws.send()
}
function starts(){
    connect();
    console.log("started script");
}