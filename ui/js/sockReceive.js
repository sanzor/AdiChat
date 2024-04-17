"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var bus_1 = require("./bus");
var events_1 = require("./events");
(0, bus_1.subscribeToEvent)(events_1.SOCKET_RECEIVE, onSocketReceive);
function onSocketReceive(ev) {
    var data = ev.detail;
    if (data.kind == "chat") {
        handle_chat_message(data);
        return;
    }
    if (data.kind == "command_result") {
        handle_command_result(data);
        return;
    }
    if (data.kind = "user_event") {
        console.log("Received user event");
        console.log(data);
    }
}
function onNewChatMessage(data) {
}
function handle_chat_message(data) {
    console.log("New Message !!!");
    console.log(data.detail);
    (0, bus_1.publishEvent)(events_1.NEW_INCOMING_MESSAGE, data.detail);
}
function handle_command_result(data) {
    if (data.command == events_1.SUBSCRIBE_COMMAND) {
        callback_subscribe(data);
    }
    if (data.command == events_1.UNSUBSCRIBE_COMMAND) {
        callback_unsubscribe(data);
    }
    if (data.command == events_1.REFRESH_CHANNELS_COMMAND) {
        console.log(data);
        (0, bus_1.publishEvent)(events_1.REFRESH_CHANNELS_COMMAND_RESULT, data.result);
    }
    if (data.command == events_1.GET_NEWEST_MESSAGES) {
        callback_get_newest_messages(data);
    }
    if (data.command == events_1.GET_OLDER_MESSAGES) {
        callback_get_older_messages(data);
    }
}
function handle_user_event(data) {
    if (data.user_event_kind == events_1.SUBSCRIBE_COMMAND) {
        handle_user_event_subscribe(data);
    }
    if (data.user_event_kind == events_1.UNSUBSCRIBE_COMMAND) {
        handle_user_event_unsubscribe(data);
    }
}
function handle_user_event_subscribe(data) {
    console.log("Publishing update channels");
    (0, bus_1.publishEvent)(events_1.SUBSCRIBE_COMMAND_RESULT_U, data);
}
function handle_user_event_unsubscribe(data) {
    console.log("Publishing update channels");
    (0, bus_1.publishEvent)(events_1.UNSUBSCRIBE_COMMAND_RESULT_U, data);
}
function callback_subscribe(data) {
    console.log(data);
    (0, bus_1.publishEvent)(events_1.SUBSCRIBE_COMMAND_RESULT, data);
}
function callback_unsubscribe(data) {
    console.log(data);
    if (data.result == "ok") {
        (0, bus_1.publishEvent)(events_1.UNSUBSCRIBE_COMMAND_RESULT, data);
        (0, bus_1.publishEvent)(events_1.RESET_CHAT, {});
    }
}
function callback_get_newest_messages(data) {
    (0, bus_1.publishEvent)("getNewestMessages", data.result);
    console.log(data.messages);
}
function callback_get_older_messages(data) {
    (0, bus_1.publishEvent)("getOlderMessages", data.result);
}
