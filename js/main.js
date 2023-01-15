
function addListeners(){
    document.addEventListener("onConnect",function(e){
        console.log("Connect event fired");
        onConnect()
       
    });
    document.addEventListener("onDisconnect",function(e){
        console.log("Disconnect event fired");
        onDisconnect();
    })
}

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
function starts(){
    connect();
    console.log("started script");
}