import config from "./config.js";


import{
    loginButton,
    registerButton,
    backToLoginBtn,
    submitBtn,
    registerModal,
    registerFailMessage,
    loginModal,
    emailLoginBox,
    passwordLoginBox,
    loginFailMessage
} from "./elements.js";


import{emailBox,passwordBox,usernameBox,retypePasswordBox} from "./elements.js";

import { subscribeToEvent,publishEvent} from "./eventBus.js";



loginButton.addEventListener("click",onLogin);
registerButton.addEventListener("click",onRegister);
submitBtn.addEventListener("click",onSubmit);
backToLoginBtn.addEventListener("click",onBackToLogin);
subscribeToEvent("DOMContentLoaded",checkIfLoggedin);

//register


function checkIfLoggedin(){
    if(localStorage.user.id==undefined || localStorage.user.id==null){
        showLoginModal();
        return;
    }
    console.log(localStorage.user.id);
    publishEvent("connect",{});

}
async function onLogin(){
    var loginResult=await loginAsync();
    if(loginResult!=true){
        showLoginErrorMessage(loginResult.message);
        return;
    }
    console.log(`\nLogin succesfull for ${localStorage.user}\n`);
    showMainPanel();

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

async function onSubmit(){
    console.log("onSubmit");
    var userData=getCreateUserData();
    console.log(userData);  
    var validateResult=validateCreateUserData(userData);
    if(validateResult!=true){
         
         showSubmitFailMessage(`Invalid data having reason:${validateResult.message}`);
         return;
    }
    try{
     console.log("Inside submit");
     var user=await createUserAsync();
     localStorage.setItem("user",JSON.stringify(user));
    }catch(error){
        showSubmitFailMessage();
    }
}

 function onBackToLogin(){
    showLoginModal();
}

function tryGetUser(){
    if(localStorage.user==null){
        return undefined;
    }
    return localStorage.user;
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
    if(data.username==undefined || data.username==null){
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
    var url=`{config.baseHttpUrl}/create-user`;
    console.log(url);
    
    var result=await postData(url,userData);
    console.log(result);
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


function clearLoginErrorMessage(){
    loginFailMessage.innerHTML=undefined;
    loginFailMessage.style.display="none";
}
function showLoginErrorMessage(message){
    console.log("Inside show login error message");
    loginFailMessage.innerHTML=`Could not login. Reason:${message}`;
    loginFailMessage.style.display="block";
}
function cleanSubmitFailMessage(){
    registerFailMessage.innerHTML=undefined;
    registerFailMessage.style.display="none";
}
function showSubmitFailMessage(message){
    registerFailMessage.value=message;
    registerFailMessage.style.display="block";
}
function showLoginModal(){
    clearLoginErrorMessage();
    parentPanel.style.display="none";
    registerModal.style.display="none";
    loginModal.style.display="block";
    
}
function showRegisterModal(){
    cleanSubmitFailMessage();
    parentPanel.style.display="none";
    loginModal.style.display="none";
    registerModal.style.display="block";
}
function showMainPanel(){
    registerModal.style.display="none";
    loginModal.style.display="none";
    parentPanel.style.display="flex";
}
