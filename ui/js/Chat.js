"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var bus_1 = require("./bus");
var constants_1 = require("./constants");
var elements_1 = require("./elements");
var events_1 = require("./events");
var utils_1 = require("./utils");
elements_1.loadOlderMessagesBtn === null || elements_1.loadOlderMessagesBtn === void 0 ? void 0 : elements_1.loadOlderMessagesBtn.addEventListener("click", onLoadOlderMessages);
(0, bus_1.subscribeToEvent)(events_1.SET_CHAT, onSetChat);
(0, bus_1.subscribeToEvent)(events_1.RESET_CHAT, onResetChat);
(0, bus_1.subscribeToEvent)("get_messages_result", onGetMessagesResult);
(0, bus_1.subscribeToEvent)(events_1.SELF_PUBLISH_MESSAGE, onSelfPublish);
function onResetChat(_) {
    (0, bus_1.publishEvent)(events_1.RESET_CHAT_DOM, {});
}
function onSelfPublish(ev) {
    console.log("Inside send message");
    var currentChannel = (0, utils_1.getItemFromStorage)(constants_1.CURRENT_CHANNEL);
    var user = (0, utils_1.getItemFromStorage)(constants_1.USER);
    console.log("Channel publish:" + currentChannel);
    var message = {
        kind: events_1.PUBLISH_MESSAGE,
        userId: user === null || user === void 0 ? void 0 : user.id,
        topicId: Number.parseInt(currentChannel.id),
        message: elements_1.chatSendMessageBox.value
    };
    (0, bus_1.publishCommand)(message);
}
function onSetChat(ev) {
    console.log(ev.detail);
    console.log(ev.detail.name);
    setChatWithChannel(ev.detail);
    (0, bus_1.publishEvent)(events_1.SET_CHAT_DOM, ev.detail);
}
function setChatWithChannel(channel) {
}
function onLoadOlderMessages(ev) {
    // var count=Array.length(chatMessageContainer.children);
    // var eventPayload=get_older_messages(currentChannel.id,count,CHANNEL_MESSAGES_COUNT);
    // publishEvent(SOCKET_COMMAND,eventPayload);
}
function onGetMessagesResult(ev) {
}
function get_older_messages(id, startIndex, count) {
    var message = {
        kind: "get-oldest-channel-messages",
        topicId: id,
        count: count,
        startIndex: startIndex
    };
    return message;
}
function get_newest_messages(id, count) {
    var message = {
        kind: "get-newest-channel-messages",
        id: id,
        count: count,
    };
    return message;
}
