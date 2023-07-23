import config from "./config";
const loginButton=document.getElementById("loginBtn");
const registerButton=document.getElementById("registerBtn");
const backToLoginBtn=document.getElementById("backToLoginBtn");
const submitBtn=document.getElementById("submitBtn");

const emailBox=document.getElementById("emailBox");
const passwordBox=document.getElementById("passwordBox");


const registerForm=document.getElementById("registerForm");

loginButton.addEventListener("click",login);
registerButton.addEventListener("click",register);
submitBtn.addEventListener("click",submit);
backToLoginBtn.addEventListener("click",backToLogin);



async function login(){
    const email=emailBox.value;
    const password=passwordBox.value;
    const url=`${config.baseHttpUrl}/get-user?email=${email}&password=${password}`;
    console.log(url);
    try{
        var user=await getUserByEmailAsync(url);
        localStorage.user=user;

    }catch(error){
        console.log(error);
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
async function getUserByEmailAsync(){
    var email=emailBox.value;
    var password=passwordBox.value;
    var url=`{config.baseHttpUrl}/get-user-by-email?email=${email}&password=${password}`;
    var result=await getData(url);
    console.log(result);
    return result;

}

async function getData(url=""){
    try {
        const response = await fetch(url, {
            method: "GET", // *GET, POST, PUT, DELETE, etc.
          //   mode: "no-cors", // no-cors, *cors, same-origin
            cache: "no-cache", // *default, no-cache, reload, force-cache, only-if-cached
            credentials: "same-origin", // include, *same-origin, omit
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
async function register(){

}

async function submit(){

}

async function backToLogin(){

}

function showLoginPanel(){

}