
import config from "./config";
const connectBtn=document.getElementById("connectBtn");
const disconnectBtn=document.getElementById("disconnectBtn");
const subscribeBtn=document.getElementById("subscribeBtn");
const subscribeBox=document.getElementById("subscribeBox");
const urlBox=document.getElementById("urlBox");
const currentChannel=document.getElementById("currentChannelNameLabel");
const channelsContainer=document.getElementById("channelsContainer");
const chatContainer=document.getElementById("messagesContainer");
const baseUrl=document.getElementById("baseUrlBox");
 function  onConnect(){
    
    connect();
    urlBox.setAttribute("disabled",true);
    disconnectBtn.disabled=false;
    connectBtn.disabled=true;
    subscribeBtn.disabled=false;
    subscribeBox.disabled=false;
  
   
   
}

async function ResolveUser(){
    if(localStorage.user==null || localStorage.user.id==null){
        let user=await createUserAsync();
        localStorage.user=user;
        return;
    }
   
}
async function createUserAsync(){
   
    var url=`{config.baseHttpUrl}/create-user`;
    console.log(url);
    var username=usernameBox.value;
    var result=await postData(url, { name: username });
    console.log(result);
    return result;
}
async function getUserAsync(){
    var id=
    var url=`{config.baseHttpUrl}/get-user?id=`;

}
async function postData(url = "", data = {}) {
    try {
        const response = await fetch(url, {
            method: "POST", // *GET, POST, PUT, DELETE, etc.
          //   mode: "no-cors", // no-cors, *cors, same-origin
            cache: "no-cache", // *default, no-cache, reload, force-cache, only-if-cached
            credentials: "same-origin", // include, *same-origin, omit
            headers: {
              "Content-Type": "application/json",
              // 'Content-Type': 'application/x-www-form-urlencoded',
            },
            redirect: "follow", // manual, *follow, error
            referrerPolicy: "no-referrer", // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
            body: JSON.stringify(data), // body data type must match "Content-Type" header
          });
          console.log(response);
          return response.json(); 
    }catch(error){
        console.error(error);
    }
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
    var date=new Date().toDateString();
    console.log("Channel publish:"+channel);
    var message=document.getElementById("chatSendMessageBox").value;
    command_publish(channel,message);
}

