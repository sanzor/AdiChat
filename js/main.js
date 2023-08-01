
import {publishEvent ,subscribeToEvent} from "./bus.js";
import { hideElement, showElement } from "./utils.js";


const connectBtn=document.getElementById("connectBtn");
const disconnectBtn=document.getElementById("disconnectBtn");
const subscribeBtn=document.getElementById("subscribeBtn");
const logoutBtn=document.getElementById("logoutBtn");

const subscribeBox=document.getElementById("subscribeBox");
const urlBox=document.getElementById("urlBox");


subscribeToEvent("showMain",onShowMain);
subscribeToEvent("hideMain",onHideMain);
subscribeToEvent("connect",{});


connectBtn.addEventListener("click",onConnect);
disconnectBtn.addEventListener("click",onDisconnect);
subscribeBtn.addEventListener("click",onSubscribe);
logoutBtn.addEventListener("click",onLogout);


function onShowMain(e){
    showElement("mainModal");
    publishEvent("connect");
}
function onHideMain(e){
    hideElement("mainModal");
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


