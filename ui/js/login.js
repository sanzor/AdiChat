import config from "./config.js";
import { getDataAsync } from "./utils.js";
import{loginButton,emailLoginBox,passwordLoginBox,loginFailMessage, registerBtn} 
from "./elements.js";
import { subscribeToEvent,publishEvent} from "./bus.js";
import { hideElement,showElement } from "./utils.js";
loginButton.addEventListener("click",onLogin);
registerBtn.addEventListener("click",onRegister);


subscribeToEvent("DOMContentLoaded",onDomContentLoaded);
window.onload=async function(){
    console.log("onload");
    await startLoginFlow();
}
subscribeToEvent("showLogin",onShowLogin);
subscribeToEvent("hideLogin",onHideLogin);

async function onDomContentLoaded(){
    console.log("ondomcontentloaded");
    await startLoginFlow();
   

}
async function startLoginFlow(){
    console.log("start login flow");
    var userRaw=localStorage.getItem("user");
    
    if(!userRaw){
        publishEvent("showLogin",{});
        return;
    }
    var userResult=JSON.parse(userRaw);
   
    await tryLoginAsync(userResult);
}
function onShowLogin(ev){
    showElement("loginModal");
}
function onHideLogin(ev){
    hideElement("loginModal");
}

async function tryLoginAsync(User){
    console.log("try login");
    try {
        console.log(User.id);
        var userResult=await getUserByIdAsync(User.id);
        if(userResult){
            publishEvent("showMain",{});
            return;
        }
        
        console.log("Could not log in with the user ");
        publishEvent("showLogin",{});
    } catch (error) {
        console.log("Error at try login");
        publishEvent("showLogin",{});
    }
   
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
        console.log(user);
        localStorage.setItem("user",JSON.stringify(user));
        return true;

    }catch(error){
        return error;
    }
}
function onRegister(){
    clearLoginErrorMessage();
    publishEvent("hideLogin",{});
    publishEvent("showRegister",{});
    
 }



async function getUserByIdAsync(Id){
    var url=`${config.baseHttpUrl}/get-user?id=${Id}`;
    var result=await getDataAsync(url);
    return result;
}

async function getUserByEmailAsync(){
    var email=emailLoginBox.value;
    var password=passwordLoginBox.value;
    var url=`${config.baseHttpUrl}/get-user-by-email?email=${email}&password=${password}`;
    var result=await getDataAsync(url);
    console.log(result);
    return result;

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



