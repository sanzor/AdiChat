
const connectBtn:HTMLButtonElement=document.getElementById("connectBtn")! as HTMLButtonElement;
const logoutBtn:HTMLButtonElement=document.getElementById("logoutBtn")! as HTMLButtonElement;
const disconnectBtn:HTMLButtonElement=document.getElementById("disconnectBtn")! as HTMLButtonElement;

export{
    connectBtn,
    disconnectBtn,
    logoutBtn
};
export{
    chatContainer,
    chatSendMessageBtn,
    chatSendMessageBox,
    currentChannel
};
const chatContainer:HTMLDivElement =document.getElementById("messagesContainer")! as HTMLDivElement;
const chatSendMessageBtn:HTMLButtonElement =document.getElementById("chatSendMessageBtn")! as HTMLButtonElement;
const chatSendMessageBox:HTMLInputElement =document.getElementById("chatSendMessageBox")! as HTMLInputElement;
const currentChannel:HTMLLabelElement=document.getElementById("currentChannelNameLabel")! as HTMLLabelElement;


export{
    loginButton,
    registerBtn,
    backToLoginBtn,
    submitBtn,
    registerModal,
    registerFailMessage,
    loginModal,
    emailLoginBox,
    passwordLoginBox,
    loginFailMessage};

const loginButton:HTMLButtonElement=document.getElementById("loginBtn")! as HTMLButtonElement;
const registerBtn:HTMLButtonElement=document.getElementById("registerBtn")!as HTMLButtonElement;
const backToLoginBtn:HTMLButtonElement=document.getElementById("backToLoginBtn")!as HTMLButtonElement;
const submitBtn:HTMLButtonElement=document.getElementById("submitBtn")!as HTMLButtonElement;

const registerModal:HTMLDivElement=document.getElementById("registerModal")! as HTMLDivElement;
const registerFailMessage:HTMLParagraphElement=document.getElementById("registerFailMessage")! as HTMLParagraphElement;

const loginModal:HTMLElement=document.getElementById("loginModal")!;
const emailLoginBox:HTMLInputElement=document.getElementById("emailLoginBox")! as HTMLInputElement;
const passwordLoginBox:HTMLInputElement=document.getElementById("passwordLoginBox")! as HTMLInputElement;
const loginFailMessage:HTMLParagraphElement=document.getElementById("loginFailMessage")! as HTMLParagraphElement;

export{emailBox,passwordBox,usernameBox,retypePasswordBox};

const emailBox:HTMLInputElement=document.getElementById("emailBox")! as HTMLInputElement;
const passwordBox:HTMLInputElement=document.getElementById("passwordBox")!as HTMLInputElement;
const usernameBox:HTMLInputElement=document.getElementById("usernameBox")!as HTMLInputElement;
const retypePasswordBox:HTMLInputElement=document.getElementById("retypePasswordBox")!as HTMLInputElement;

export{channelsContainer,subscribeBtn,subscribeBox};

const channelsContainer:HTMLDivElement=document.getElementById("channelsContainer")! as HTMLDivElement;
const subscribeBtn:HTMLButtonElement=document.getElementById("subscribeBtn")!as HTMLButtonElement;
const subscribeBox:HTMLInputElement=document.getElementById("subscribeBox")! as HTMLInputElement;

export{loadOlderMessagesBtn};

const loadOlderMessagesBtn:HTMLButtonElement=document.getElementById("loadOlderMessagesBtn")!as HTMLButtonElement;


