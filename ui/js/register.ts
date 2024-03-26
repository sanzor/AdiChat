
import config from "./config";
import{emailBox,passwordBox,usernameBox,retypePasswordBox} from "./elements.js";
import {publishEvent,subscribeToEvent } from "./bus.js";
import { postDataAsync } from "./utils.js";

import{
    backToLoginBtn,
    submitBtn,
    registerFailMessage,
} from "./elements.js";
import { hideElement, showElement } from "./utils.js";
import { HIDE_REGISTER, SHOW_LOGIN, SHOW_MAIN, SHOW_REGISTER } from "./events.js";

subscribeToEvent(SHOW_REGISTER,onShowRegister);
subscribeToEvent(HIDE_REGISTER,onHideRegister);
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
    publishEvent(HIDE_REGISTER,{});
    publishEvent(SHOW_LOGIN,{});
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
     publishEvent(HIDE_REGISTER,{});
     publishEvent(SHOW_MAIN,{});
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
    
    var result=await postDataAsync(url,userData);
    console.log("Result create user\n:");
    console.log(result);
    return result;
}
function handleUserCreateError(userResult){
    console.log(`Inside handling user error ${userResult}`);
    registerFailMessage.innerHTML=userResult.reason;
    return;
}





