"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.HIDE_REGISTER = exports.SHOW_REGISTER = exports.HIDE_LOGIN = exports.SHOW_LOGIN = exports.HIDE_MAIN = exports.SHOW_MAIN = exports.GET_NEWEST_MESSAGES = exports.GET_OLDER_MESSAGES = exports.SELF_PUBLISH_MESSAGE = exports.PUBLISH_MESSAGE = exports.NEW_INCOMING_MESSAGE = exports.RESET_CHAT_DOM = exports.RESET_CHAT = exports.SET_CHAT_DOM = exports.SET_CHAT = exports.SOCKET_CLOSED = exports.SOCKET_RECEIVE = exports.SUBSCRIBE_COMMAND_RESULT_U = exports.SUBSCRIBE_COMMAND_RESULT = exports.SUBSCRIBE_COMMAND = exports.UNSUBSCRIBE_COMMAND_RESULT_U = exports.UNSUBSCRIBE_COMMAND_RESULT = exports.UNSUBSCRIBE_COMMAND = exports.UNSUBSCRIBE_BUTTON_CLICK = exports.CHANNEL_CLICK = exports.REMOVE_CHANNEL = exports.ADD_CHANNEL = exports.SET_CHANNELS = exports.REFRESH_CHANNELS_COMMAND_RESULT = exports.REFRESH_CHANNELS_COMMAND = void 0;
//socket commands
var REFRESH_CHANNELS_COMMAND = "get_subscriptions";
exports.REFRESH_CHANNELS_COMMAND = REFRESH_CHANNELS_COMMAND;
var SUBSCRIBE_COMMAND = "subscribe";
exports.SUBSCRIBE_COMMAND = SUBSCRIBE_COMMAND;
var UNSUBSCRIBE_COMMAND = "unsubscribe";
exports.UNSUBSCRIBE_COMMAND = UNSUBSCRIBE_COMMAND;
var PUBLISH_MESSAGE = "publish";
exports.PUBLISH_MESSAGE = PUBLISH_MESSAGE;
var SELF_PUBLISH_MESSAGE = "self_publish";
exports.SELF_PUBLISH_MESSAGE = SELF_PUBLISH_MESSAGE;
var GET_OLDER_MESSAGES = "get_older_messages";
exports.GET_OLDER_MESSAGES = GET_OLDER_MESSAGES;
var GET_NEWEST_MESSAGES = "get_newest_messages";
exports.GET_NEWEST_MESSAGES = GET_NEWEST_MESSAGES;
//socket command results
var REFRESH_CHANNELS_COMMAND_RESULT = "get_subscriptions_result";
exports.REFRESH_CHANNELS_COMMAND_RESULT = REFRESH_CHANNELS_COMMAND_RESULT;
var UNSUBSCRIBE_BUTTON_CLICK = "unsubscribe_button_click";
exports.UNSUBSCRIBE_BUTTON_CLICK = UNSUBSCRIBE_BUTTON_CLICK;
var CHANNEL_CLICK = "channel_click";
exports.CHANNEL_CLICK = CHANNEL_CLICK;
var UNSUBSCRIBE_COMMAND_RESULT = "unsubscribe_result";
exports.UNSUBSCRIBE_COMMAND_RESULT = UNSUBSCRIBE_COMMAND_RESULT;
var UNSUBSCRIBE_COMMAND_RESULT_U = "unsubscribe_result_u";
exports.UNSUBSCRIBE_COMMAND_RESULT_U = UNSUBSCRIBE_COMMAND_RESULT_U;
var SUBSCRIBE_COMMAND_RESULT = "subscribe_result";
exports.SUBSCRIBE_COMMAND_RESULT = SUBSCRIBE_COMMAND_RESULT;
var SUBSCRIBE_COMMAND_RESULT_U = "subscribe_result_u";
exports.SUBSCRIBE_COMMAND_RESULT_U = SUBSCRIBE_COMMAND_RESULT_U;
var SOCKET_RECEIVE = "socketReceive";
exports.SOCKET_RECEIVE = SOCKET_RECEIVE;
var SOCKET_CLOSED = "socketClosed";
exports.SOCKET_CLOSED = SOCKET_CLOSED;
var NEW_INCOMING_MESSAGE = "new_channel_message";
exports.NEW_INCOMING_MESSAGE = NEW_INCOMING_MESSAGE;
var SET_CHAT = "set_chat";
exports.SET_CHAT = SET_CHAT;
var SET_CHAT_DOM = "set_chat_dom";
exports.SET_CHAT_DOM = SET_CHAT_DOM;
var RESET_CHAT = "reset_chat";
exports.RESET_CHAT = RESET_CHAT;
var RESET_CHAT_DOM = "reset_chat_dom";
exports.RESET_CHAT_DOM = RESET_CHAT_DOM;
var SET_CHANNELS = "set_channels";
exports.SET_CHANNELS = SET_CHANNELS;
var REMOVE_CHANNEL = "remove_channel";
exports.REMOVE_CHANNEL = REMOVE_CHANNEL;
var ADD_CHANNEL = "add_channel";
exports.ADD_CHANNEL = ADD_CHANNEL;
var SHOW_MAIN = "showMain";
exports.SHOW_MAIN = SHOW_MAIN;
var HIDE_MAIN = "hideMain";
exports.HIDE_MAIN = HIDE_MAIN;
var SHOW_LOGIN = "showLogin";
exports.SHOW_LOGIN = SHOW_LOGIN;
var HIDE_LOGIN = "hideLogin";
exports.HIDE_LOGIN = HIDE_LOGIN;
var SHOW_REGISTER = "showRegister";
exports.SHOW_REGISTER = SHOW_REGISTER;
var HIDE_REGISTER = "hideRegister";
exports.HIDE_REGISTER = HIDE_REGISTER;
