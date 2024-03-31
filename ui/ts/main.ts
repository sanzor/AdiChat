
import {publishEvent ,subscribeToEvent} from "./bus";
import {hideElement, showElement } from "./utils";
import {connect} from "./sock";
import { HIDE_MAIN, SHOW_LOGIN, SHOW_MAIN } from "./events";


const connectBtn=document.getElementById("connectBtn")!;
const disconnectBtn=document.getElementById("disconnectBtn")!;
const logoutBtn=document.getElementById("logoutBtn")!;


subscribeToEvent(SHOW_MAIN,onShowMain);
subscribeToEvent(HIDE_MAIN,onHideMain);
subscribeToEvent("connect",onConnect);
subscribeToEvent("socketEvent",onSocketEvent);

connectBtn.addEventListener("click",btnConnect);
disconnectBtn.addEventListener("click",onDisconnect);

logoutBtn.addEventListener("click",onLogout);


function onShowMain(e:CustomEvent){
    showElement("mainModal");
    publishEvent("connect",{});
}
function onHideMain(e:CustomEvent){
    hideElement("mainModal");
}

function  onConnect(e:CustomEvent){
    connect();
}

function onSocketEvent(){
    
}

function onDisconnect(){
}





function onLogout(){
    console.log("On logout");
    publishEvent("close_socket",{});
    localStorage.removeItem("user"); 
    publishEvent(HIDE_MAIN,{});
    publishEvent(SHOW_LOGIN,{});
}
function btnConnect(){
    
}



