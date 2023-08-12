
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
subscribeToEvent("socketEvent",onSocketEvent);

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
}

function onSocketEvent(){
    
}

function onDisconnect(){
    command_disconnect();
}

function onSubscribe(){
    publishEvent("socket_command",{"kind":"subscribe","topic":subscribeBox.value});
}



function onLogout(){
    console.log("On logout");
    publishEvent("close_socket",{});
    localStorage.removeItem("user"); 
    publishEvent("hideMain",{});
    publishEvent("showLogin",{});
}
function btnConnect(){
    
}



