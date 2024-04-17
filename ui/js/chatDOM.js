"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var events_1 = require("./events");
var bus_1 = require("./bus");
var constants_1 = require("./constants");
var elements_1 = require("./elements");
var utils_1 = require("./utils");
var APPEND_MESSAGE_DOM = "append_message";
(0, bus_1.subscribeToEvent)(APPEND_MESSAGE_DOM, onAppendedMessage);
(0, bus_1.subscribeToEvent)(events_1.RESET_CHAT_DOM, onResetChat);
(0, bus_1.subscribeToEvent)(events_1.SET_CHAT_DOM, onSetChat);
elements_1.chatSendMessageBtn.addEventListener("click", onSendMessage);
function onSetChat(ev) {
    console.log(ev.detail);
    var channel = ev.detail;
    console.log("On set chat dom");
    console.log(channel.name);
    elements_1.currentChannel.innerText = ev.detail.name;
}
function onSendMessage(_) {
    var message = elements_1.chatSendMessageBox.value;
    var user = (0, utils_1.getItemFromStorage)("user");
    var channel = (0, utils_1.getItemFromStorage)(constants_1.CURRENT_CHANNEL);
    var appendMessage = {
        user: user,
        topic: channel,
        message: message
    };
    console.log(appendMessage);
    (0, bus_1.publishEvent)(APPEND_MESSAGE_DOM, appendMessage);
    (0, bus_1.publishEvent)(events_1.SELF_PUBLISH_MESSAGE, message);
}
function onAppendedMessage(ev) {
    var message = ev.detail;
    console.log("Inside on new message");
    var messageElement = createChatMessageContainer(message);
    elements_1.chatContainer.appendChild(messageElement);
}
function onResetChat(_) {
    localStorage.removeItem("currentChannelId");
    elements_1.chatContainer.innerHTML = '';
    elements_1.currentChannel.innerText = '';
}
function clearChatMessageBox() {
    elements_1.chatSendMessageBox.value = "";
}
function createChatMessageContainer(data) {
    var user = data.user;
    var topic = data.topic;
    var message = data.message;
    var chatMessageContainer = document.createElement("div");
    chatMessageContainer.setAttribute("class", "chatMessageContainer");
    var icon = document.createElement("img");
    var content = document.createElement("div");
    content.classList.add("chatMessageContent");
    var meta = document.createElement("div");
    var status = document.createElement("div");
    meta.setAttribute("class", "chatMessageMeta");
    meta.innerText = JSON.stringify(user);
    icon.setAttribute("class", "chatMessageIcon");
    content.setAttribute("class", "chatMessageContent");
    content.innerText = message;
    status.setAttribute("class", "chatMessageStatusPending");
    status.innerText = "tick";
    chatMessageContainer.appendChild(icon);
    chatMessageContainer.appendChild(meta);
    chatMessageContainer.appendChild(content);
    chatMessageContainer.appendChild(status);
    return chatMessageContainer;
}
