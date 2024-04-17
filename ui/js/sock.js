"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.send = exports.connect = void 0;
var bus_1 = require("./bus");
var config_1 = require("./config");
var constants_1 = require("./constants");
var events_1 = require("./events");
(0, bus_1.subscribeToEvent)("close_socket", onCloseSocketCommand);
(0, bus_1.subscribeToEvent)(constants_1.SOCKET_COMMAND, onAsyncCommand);
window.addEventListener("beforeunload", onUnload);
var socket = null;
function onCloseSocketCommand() {
    if (socket) {
        console.log("closing websocket");
        socket.close();
    }
}
function onUnload() {
    (0, bus_1.publishEvent)("close_socket", {});
}
function get_url() {
    var user = JSON.parse(localStorage.user);
    var url = "".concat(config_1.default.baseWsUrl, "/ws/id/").concat(user.id);
    console.log("Url:" + url);
    return url;
}
function send() {
}
exports.send = send;
function connect() {
    var url = get_url();
    console.log("\nAttempting connect to:" + url + "\n");
    socket = new WebSocket(url);
    socket.onopen = function (e) {
        console.log("\nConnection established\n");
        command_get_subscriptions();
    };
    socket.onmessage = function (ev) {
        if (ev.data == "ping") {
            console.log("\nReceived ping");
            socket.send("pong");
        }
        var message = JSON.parse(ev.data);
        console.log("\nReceived on socket: ");
        console.log(message);
        (0, bus_1.publishEvent)(events_1.SOCKET_RECEIVE, message);
    };
    socket.onclose = function (e) {
        console.log("Socket closed with code: ".concat(e.code, " , reason: ").concat(e.reason));
        (0, bus_1.publishEvent)(events_1.SOCKET_CLOSED, {});
    };
}
exports.connect = connect;
function onDomContentLoaded() {
}
function onAsyncCommand(ev) {
    var data = ev.detail;
    onCommand(data);
}
function onCommand(data) {
    console.log(data);
    console.log("\nSending [".concat(data.kind, "] command : ").concat(data, " to socket\n"));
    switch (data.kind) {
        case events_1.SUBSCRIBE_COMMAND:
            if (isSubscribeCommand(data))
                command_subscribe(data.topic);
            break;
        case events_1.UNSUBSCRIBE_COMMAND:
            if (isUnsubscribeCommand(data))
                command_unsubscribe(data.topicId);
            break;
        case events_1.REFRESH_CHANNELS_COMMAND:
            if (isRefreshChannelsCommand(data))
                command_get_subscriptions();
            break;
        case events_1.PUBLISH_MESSAGE:
            if (isPublishMessage(data))
                command_publish(data);
            break;
        case "disconnect":
            if (isDisconnectCommand(data))
                command_disconnect();
            break;
        case events_1.GET_NEWEST_MESSAGES:
            if (isGetNewestMessagesCommand(data)) {
                var cmd = data;
                command_get_newest_messages(cmd);
            }
            break;
        case events_1.GET_OLDER_MESSAGES:
            if (isGetOlderMessagesCommand(data)) {
                var command = data;
                command_get_older_messages(command);
            }
            break;
    }
}
function command_subscribe(topic) {
    console.log("inside command sub");
    console.log(topic);
    var message = {
        "command": events_1.SUBSCRIBE_COMMAND,
        "topic": topic
    };
    console.log("\nSending:" + JSON.stringify(message));
    console.log(socket);
    socket.send(JSON.stringify(message));
}
function command_unsubscribe(topicId) {
    console.log(topicId);
    var message = {
        "command": events_1.UNSUBSCRIBE_COMMAND,
        "topicId": topicId
    };
    console.log("\nSending:" + JSON.stringify(message));
    socket.send(JSON.stringify(message));
}
function command_get_subscriptions() {
    var message = {
        "command": events_1.REFRESH_CHANNELS_COMMAND
    };
    console.log("\nSending:" + JSON.stringify(message));
    socket.send(JSON.stringify(message));
}
function command_disconnect() {
    (0, bus_1.publishEvent)("close_socket", {});
}
function command_publish(command) {
    console.log(command.topicId);
    var toSend = {
        "command": events_1.PUBLISH_MESSAGE,
        "topicId": command.topicId,
        "content": command.message
    };
    console.log("\nSending:" + JSON.stringify(toSend));
    socket.send(JSON.stringify(toSend));
}
function command_get_newest_messages(command) {
    var message = {
        "command": events_1.GET_NEWEST_MESSAGES,
        "topicId": command.topicId,
        "count": command.count,
    };
    console.log(message);
    socket.send(JSON.stringify(message));
}
function command_get_older_messages(command) {
    var message = {
        "command": events_1.GET_OLDER_MESSAGES,
        "topicId": command.topicId,
        "startIndex": command.startIndex,
        "count": command.count,
    };
    console.log(message);
    socket.send(JSON.stringify(message));
}
function isSubscribeCommand(command) {
    return command.kind === "subscribe";
}
function isUnsubscribeCommand(command) {
    return command.kind === "unsubscribe";
}
function isRefreshChannelsCommand(command) {
    return command.kind === "refresh_channels";
}
function isPublishMessage(command) {
    return command.kind === "publish";
}
function isDisconnectCommand(command) {
    return command.kind === "disconnect";
}
function isGetNewestMessagesCommand(command) {
    return command.kind === "get_newest_messages";
}
function isGetOlderMessagesCommand(command) {
    return command.kind === "get_older_messages";
}
