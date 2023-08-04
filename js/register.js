
import config from "./config.js";
import{emailBox,passwordBox,usernameBox,retypePasswordBox} from "./elements.js";
import {publishEvent,subscribeToEvent } from "./bus.js";

import{
    backToLoginBtn,
    submitBtn,
    registerFailMessage,
} from "./elements.js";
import { hideElement, showElement } from "./utils.js";

subscribeToEvent("showRegister",onShowRegister);
subscribeToEvent("hideRegister",onHideRegister);
submitBtn.addEventListener("click",onSubmit);
backToLoginBtn.addEventListener("click",onBackToLogin);





function onShowRegister(_){
    console.log("called register");
    showElement("registerModal");
}
function onHideRegister(ev){
    cleanSubmitFailMessage();
    hideElement("registerModal");
}
function cleanSubmitFailMessage(){
    registerFailMessage.innerHTML=undefined;
}

function showSubmitFailMessage(message){
    registerFailMessage.innerHTML=message;
    showElement("registerFailMessage");
}

function onBackToLogin(){
    publishEvent("hideRegister",{});
    publishEvent("showLogin",{});
}
async function onSubmit(){
    console.log("onSubmit");
    var userData=getCreateUserData();
    console.log(`User data for creating:${userData}`);  
    var validateResult=validateCreateUserData(userData);
    if(!validateResult){
         
         showSubmitFailMessage(`Invalid data having reason:${validateResult.message}`);
         return;
    }
    try{
    
     var userResult=await createUserAsync(userData);
     if(userResult.result=="error"){
       handleUserCreateError(userResult);
       return;
     }
     console.log(`User created:${userResult}`);
     localStorage.setItem("user",JSON.stringify(userResult));
     publishEvent("hideRegister",{});
     publishEvent("showMain",{});
    }catch(error){
        showSubmitFailMessage(error);
    }
}

function getCreateUserData(){
    let userData={
        email:emailBox.value,
        password:passwordBox.value,
        retypePassword:retypePasswordBox.value,
        name:usernameBox.value

   }
   return userData;
}
function validateCreateUserData(data){
    console.log(data);
    if(data.name==undefined || data.name==null){
        return new Error("Invalid username");
    }
    if(data.password==undefined || data.password==null){
        return new Error("Invalid password");
    }
    if(data.password!=data.retypePassword){
        
        return new Error("Passwords do not match");
    }
    return data;

}

async function createUserAsync(userData){
    var url=`${config.baseHttpUrl}/create-user`;
    console.log(url);
    
    var result=await postData(url,userData);
    console.log(result);
    return result;
}
function handleUserCreateError(userResult){
    console.log(`Inside handling user error ${userResult}`);
    registerFailMessage.innerHTML=userResult.reason;
    return;
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
          if(response.status==409){
            return {result:"error",reason:"user_already_exists"};
          }
          return response.json(); 
    }catch(error){
        console.error(error);
    }
  }



