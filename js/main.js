
import { publishEvent ,subscribeToEvent} from "./eventBus";

const connectBtn=document.getElementById("connectBtn");
const disconnectBtn=document.getElementById("disconnectBtn");
const subscribeBtn=document.getElementById("subscribeBtn");
const logoutBtn=document.getElementById("logoutBtn");

const subscribeBox=document.getElementById("subscribeBox");
const urlBox=document.getElementById("urlBox");


subscribeToEvent("loadMainModal",onLoadMainModal);


connectBtn.addEventListener("click",onConnect);
disconnectBtn.addEventListener("click",onDisconnect);
subscribeBtn.addEventListener("click",onSubscribe);
logoutBtn.addEventListener("click",onLogout);


 function onLoadMainModal(e){
    connect();
    console.log(e);
    showMainModal();
}
 function  onConnect(e){
    
    connect();
    showMainModal();
}



function showMainModal(){
    registerModal.style.display="none";
    loginModal.style.display="none";
    parentPanel.style.display="flex";
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


