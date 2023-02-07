

function onConnect(){
    connect();
    var disconnectBtn=document.getElementById("disconnectBtn");
    var connectBtn=document.getElementById("connectBtn");
    disconnectBtn.disabled=false;
    connectBtn.disabled=true;
    
}
function onDisconnect(){
    disconnect();
    var connectBtn=document.getElementById("connectBtn");
    var disconnectBtn=document.getElementById("disconnectBtn");
    connectBtn.disabled=false;
    disconnectBtn.disabled=true;
    
}
