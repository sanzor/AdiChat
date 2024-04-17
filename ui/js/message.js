"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var bus_1 = require("./bus");
var constants_1 = require("./constants");
var utils_1 = require("./utils");
(0, bus_1.subscribeToEvent)("new_message", onNewChatMessage);
function onNewChatMessage(ev) {
    var currentChannel = (0, utils_1.getItemFromStorage)(constants_1.CURRENT_CHANNEL);
    var newMessageChannelId = ev.detail.topic_id;
    if (newMessageChannelId == currentChannel) {
        (0, bus_1.publishEvent)("new_chat_message", ev.detail);
    }
    else {
        (0, bus_1.publishEvent)("new_channel_message", ev.detail);
    }
}
