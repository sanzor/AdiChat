
import {publishEvent ,subscribeToEvent} from "./bus.js";
import {hideElement, showElement } from "./utils.js";
import {connect} from "./sock.js";


const connectBtn=document.getElementById("connectBtn");
const disconnectBtn=document.getElementById("disconnectBtn");
const subscribeBtn=document.getElementById("subscribeBtn");
const logoutBtn=document.getElementById("logoutBtn");

const subscribeBox=document.getElementById("subscribeBox");


subscribeToEvent("showMain",onShowMain);
subscribeToEvent("hideMain",onHideMain);
subscribeToEvent("connect",onConnect);


connectBtn.addEventListener("click",btnConnect);
disconnectBtn.addEventListener("click",onDisconnect);
subscribeBtn.addEventListener("click",onSubscribe);
logoutBtn.addEventListener("click",onLogout);


function onShowMain(e){
    showElement("mainModal");
    publishEvent("connect",{});
}
function onHideMain(e){
    hideElement("mainModal");
}
 function  onConnect(e){
    
    connect();
    showMainModal();
}



function onDisconnect(){
    command_disconnect();
}

function onSubscribe(){
    command_subscribe(subscribeBox.value);
}



function onLogout(){
    localStorage.removeItem("user"); 
    publishEvent("hideMain",{});
    publishEvent("showLogin",{});
}
function btnConnect(){}



