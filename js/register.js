
import config from "./config.js";
import{emailBox,passwordBox,usernameBox,retypePasswordBox} from "./elements.js";

import{
    backToLoginBtn,
    submitBtn,
    registerFailMessage,
} from "./elements.js";

submitBtn.addEventListener("click",onSubmit);
backToLoginBtn.addEventListener("click",onBackToLogin);

function cleanSubmitFailMessage(){
    registerFailMessage.innerHTML=undefined;
    registerFailMessage.style.display="none";
}

function showSubmitFailMessage(message){
    registerFailMessage.value=message;
    registerFailMessage.style.display="block";
}

function onBackToLogin(){
    showLoginModal();
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
     var user=await createUserAsync(userData);
     localStorage.setItem("user",JSON.stringify(user));
    }catch(error){
        showSubmitFailMessage();
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
    var url=`${config.baseHttpUrl}/create-user`;
    console.log(url);
    
    var result=await postData(url,userData);
    console.log(result);
    return result;
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



