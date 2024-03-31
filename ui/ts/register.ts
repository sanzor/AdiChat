
import config from "./config";
import{emailBox,passwordBox,usernameBox,retypePasswordBox} from "./elements";
import {publishEvent,subscribeToEvent } from "./bus";
import { postDataAsync, setItemInStorage } from "./utils";

import{
    backToLoginBtn,
    submitBtn,
    registerFailMessage,
} from "./elements";
import { hideElement, showElement } from "./utils";
import { HIDE_REGISTER, SHOW_LOGIN, SHOW_MAIN, SHOW_REGISTER } from "./events";
import { USER } from "./constants";
import { CreateUserParams } from "./Domain/DTO/CreateUserParams";
import { CreateUserResult } from "./Domain/DTO/CreateUserResult";
import { User } from "./Domain/User";

subscribeToEvent(SHOW_REGISTER,onShowRegister);
subscribeToEvent(HIDE_REGISTER,onHideRegister);
submitBtn.addEventListener("click",onSubmit);
backToLoginBtn.addEventListener("click",onBackToLogin);





function onShowRegister(_:CustomEvent):void{
    console.log("called register");
    showElement("registerModal");
}
function onHideRegister(ev:CustomEvent):void{
    cleanSubmitFailMessage();
    hideElement("registerModal");
}
function cleanSubmitFailMessage():void{
    registerFailMessage.innerHTML='';
}

function showSubmitFailMessage(message:string):void{
    registerFailMessage.innerHTML=message;
    showElement("registerFailMessage");
}

function onBackToLogin():void{
    publishEvent(HIDE_REGISTER,{});
    publishEvent(SHOW_LOGIN,{});
}
async function onSubmit():Promise<void>{
    console.log("onSubmit");
    var userData=getCreateUserData();
    console.log(`User data for creating:${userData}`);  
    var validateResult=validateCreateUserData(userData);
    if(validateResult instanceof Error){
         const err:Error=validateResult
         showSubmitFailMessage(`Invalid data having reason:${err.message}`);
         return;
    }
    const succesfulData:CreateUserParams=validateResult;
    try{
    
     var userResult=await createUserAsync(succesfulData);
     if(userResult.result=="error"){
       handleUserCreateError( new Error(userResult.result));
       return;
     }
     console.log(`User created:${userResult}`);
     setItemInStorage(USER,(userResult.result as User));
     publishEvent(HIDE_REGISTER,{});
     publishEvent(SHOW_MAIN,{});
    }catch(error:any){
        showSubmitFailMessage(error.message);
    }
}

function getCreateUserData():CreateUserParams{
    let userData={
        email:emailBox.value,
        password:passwordBox.value,
        retypePassword:retypePasswordBox.value,
        name:usernameBox.value

   }
   return userData;
}
function validateCreateUserData(data:CreateUserParams):CreateUserParams|Error{
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

async function createUserAsync(userData:CreateUserParams):Promise<CreateUserResult>{
    var url:string=`${config.baseHttpUrl}/create-user`;
    console.log(url);
    var result=await postDataAsync(url,userData);
    console.log("Result create user\n:");
    console.log(result);
    return result as CreateUserResult;
}
function handleUserCreateError(userResult:Error):void{
    console.log(`Inside handling user error ${userResult}`);
    registerFailMessage.innerHTML=userResult.message;
    return;
}





