import config from "./config";


const loginButton=document.getElementById("loginBtn");
const registerButton=document.getElementById("registerBtn");
const backToLoginBtn=document.getElementById("backToLoginBtn");
const submitBtn=document.getElementById("submitBtn");

const loginPanel=document.getElementById("loginPanel");
const registerPanel=document.getElementById("registerPanel");

const loginModal=document.getElementById("loginModal");
const parentPanel=document.getElementById("parentPanel");

const emailLoginBox=document.getElementById("emailLoginBox");
const passwordLoginBox=document.getElementById("passwordLoginBox");

loginButton.addEventListener("click",login);
registerButton.addEventListener("click",register);
submitBtn.addEventListener("click",submitAsync);
backToLoginBtn.addEventListener("click",backToLogin);

//register


const emailBox=document.getElementById("emailBox");
const passwordBox=document.getElementById("passwordBox");
const usernameBox=document.getElementById("usernameBox");
const retypePasswordBox=document.getElementById("retypePasswordBox");

async function login(){
    const email=emailLoginBox.value;
    const password=passwordLoginBox.value;
    const url=`${config.baseHttpUrl}/get-user?email=${email}&password=${password}`;
    console.log(url);
    try{
        var user=await getUserByEmailAsync(url);
        localStorage.user=user;
        console.log(`\nLogin succesfull for ${localStorage.user}\n`);
        showMainPanel();

    }catch(error){
        console.log(error);
    }

}

function register(){
   showRegisterPanel();
}
function getCreateUserData(){
    let userData={
        email:emailBox.value,
        password:passwordBox.value,
        retypePassword:retypePasswordBox.value,
        username:usernameBox.value

   }
   return userData;
}
function validateCreateUserData(data){
    if(data.password!=data.retypePassword){
        console.log(error("Passwords do not match"));
        return false;
    }
    return true;

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
    var email=emailLoginBox.value;
    var password=passwordLoginBox.value;
    var url=`{config.baseHttpUrl}/get-user-by-email?email=${email}&password=${password}`;
    var result=await getDataAsync(url);
    console.log(result);
    return result;

}

async function getDataAsync(url=""){
   
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


async function submitAsync(){
    var userData=getCreateUserData();
    if(!validateCreateUserData(userData)){
         console.log("Could not submit form. Bad arguments");
         return;
    }
    try{
     var user=await createUserAsync();
     localStorage.user=user;
    }catch(error){
 
    }
}

 function backToLogin(){
    showLoginPanel();
}

function showLoginPanel(){
    registerPanel.style.display=none;
    loginPanel.style.display=block;
    
}
function showRegisterPanel(){
    loginPanel.style.display=none;
    registerPanel.style.display=block;
}
function showMainPanel(){
    loginModal.style.display=none;
    parentPanel.style.display=flex;
}
function showLoginModal(){
    parentPanel.style.display=none;
    loginModal.style.display=block;
}