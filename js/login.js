import config from "./config.js";
import{loginButton,registerButton,registerModal,loginModal,emailLoginBox,passwordLoginBox,loginFailMessage} 
from "./elements.js";
import { subscribeToEvent,publishEvent} from "./bus.js";
import { hideElement,showElement } from "./utils.js";
loginButton.addEventListener("click",onLogin);
registerButton.addEventListener("click",onRegister);


subscribeToEvent("DOMContentLoaded",onDomContentLoaded);
subscribeToEvent("showLogin",onShowLogin);
subscribeToEvent("hideLogin",onHideLogin);

function onDomContentLoaded(){
    if(localStorage.user.id==undefined || localStorage.user.id==null){
        publishEvent("showLogin",{});
        return;
    }
    console.log(localStorage.user.id);
    publishEvent("showMain",{});

}

function onShowLogin(ev){
    showElement("loginModal");
}
function onHideLogin(ev){
    hideElement("loginModal");
}
async function onLogin(){
    var loginResult=await loginAsync();
    if(loginResult!=true){
        showLoginErrorMessage(loginResult.message);
        return;
    }
    console.log(`\nLogin succesfull for ${localStorage.user}\n`);
    publishEvent("hideLogin",{});
    publishEvent("showMain",{});

}
async function loginAsync(){
    const email=emailLoginBox.value;
    const password=passwordLoginBox.value;
    const url=`${config.baseHttpUrl}/get-user?email=${email}&password=${password}`;
    try{
        var user=await getUserByEmailAsync(url);
        localStorage.setItem("user",JSON.stringify(user));
        console.log(`\nLogin succesfull for ${localStorage.user}\n`);
        return true;

    }catch(error){
        return error;
    }
}
function onRegister(){
    showRegisterModal();
 }





async function getUserByEmailAsync(){
    var email=emailLoginBox.value;
    var password=passwordLoginBox.value;
    var url=`${config.baseHttpUrl}/get-user-by-email?email=${email}&password=${password}`;
    var result=await getDataAsync(url);
    console.log(result);
    return result;

}

async function getDataAsync(url=""){
        console.log(url);
        const response = await fetch(url, {
            method: "GET", // *GET, POST, PUT, DELETE, etc.
          //   mode: "no-cors", // no-cors, *cors, same-origin
            cache: "no-cache", // *default, no-cache, reload, force-cache, only-if-cached
            credentials: "same-origin", // include, *same-origin, omit
            redirect: "follow", // manual, *follow, error
            referrerPolicy: "no-referrer", // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
          });
          console.log(response);
          return response.json(); 
}



function clearLoginErrorMessage(){
    loginFailMessage.innerHTML=undefined;
    loginFailMessage.style.display="none";
}
function showLoginErrorMessage(message){
    console.log("Inside show login error message");
    loginFailMessage.innerHTML=`Could not login. Reason:${message}`;
    loginFailMessage.style.display="block";
}

function showRegisterModal(){
    cleanSubmitFailMessage();
    parentPanel.style.display="none";
    loginModal.style.display="none";
    registerModal.style.display="block";
}

