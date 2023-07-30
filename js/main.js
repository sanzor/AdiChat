
import { publishEvent ,subscribeToEvent} from "./eventBus";

const connectBtn=document.getElementById("connectBtn");
const disconnectBtn=document.getElementById("disconnectBtn");
const subscribeBtn=document.getElementById("subscribeBtn");
const logoutBtn=document.getElementById("logoutBtn");

const subscribeBox=document.getElementById("subscribeBox");
const urlBox=document.getElementById("urlBox");




connectBtn.addEventListener("click",onConnect);
disconnectBtn.addEventListener("click",onDisconnect);
subscribeBtn.addEventListener("click",onSubscribe);
logoutBtn.addEventListener("click",onLogout);

 function  onConnect(e){
    
    connect();
    urlBox.setAttribute("disabled",true);
    disconnectBtn.disabled=false;
    connectBtn.disabled=true;
    subscribeBtn.disabled=false;
    subscribeBox.disabled=false;
}

async function OnConnectEvent(){

}
function onDisconnect(){
    command_disconnect();
    reset();
}

function onSubscribe(){
    command_subscribe(subscribeBox.value);
}



function onLogout(){
    localStorage.removeItem("user"); 
    publishEvent("showLoginModal",{});
}


function reset(){
    urlBox.disabled=false;
    connectBtn.disabled=false;
    disconnectBtn.disabled=true;
    subscribeBtn.disabled=true;
    subscribeBox.disabled=true;
}


