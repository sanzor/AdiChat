"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (this && this.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
Object.defineProperty(exports, "__esModule", { value: true });
var bus_1 = require("./bus");
var elements_1 = require("./elements");
var utils_1 = require("./utils");
var elements_2 = require("./elements");
var constants_1 = require("./constants");
var events_1 = require("./events");
var CHANNELS = "channels";
var TOPIC_ID = "topicId";
(0, bus_1.subscribeToEvent)(events_1.SUBSCRIBE_COMMAND_RESULT_U, onSubscribeResultU);
(0, bus_1.subscribeToEvent)(events_1.UNSUBSCRIBE_COMMAND_RESULT_U, onUnSubscribeResultU);
(0, bus_1.subscribeToEvent)(events_1.REFRESH_CHANNELS_COMMAND_RESULT, onRefreshChannelsCommandResult);
(0, bus_1.subscribeToEvent)(events_1.NEW_INCOMING_MESSAGE, onNewIncomingMessage);
(0, bus_1.subscribeToEvent)(events_1.UNSUBSCRIBE_BUTTON_CLICK, onUnsubscribeAsync);
(0, bus_1.subscribeToEvent)(events_1.CHANNEL_CLICK, onChannelClick);
function onNewIncomingMessage(ev) {
    var message = ev.detail;
    var currentChannel = (0, utils_1.getItemFromStorage)(constants_1.CURRENT_CHANNEL);
    if (currentChannel == null) {
        return;
    }
    updateChannelsOnMessage(message);
}
function updateChannelsOnMessage(data) {
    var targetChannel = Array.from(elements_1.channelsContainer.children)
        .filter(function (child) { return child.innerText == data.topicId.toString(); });
}
elements_2.subscribeBtn.addEventListener("click", onSubscribeAsync);
function onChannelClick(event) {
    var channel = event.detail;
    (0, utils_1.setItemInStorage)(constants_1.CURRENT_CHANNEL, channel);
    (0, bus_1.publishEvent)(events_1.SET_CHAT, channel);
}
function onRefreshChannelsCommandResult(ev) {
    var channels = ev.detail;
    if (channels.length == 0 || !channels) {
        (0, bus_1.publishEvent)(events_1.SET_CHANNELS, []);
        return;
    }
    (0, utils_1.setItemInStorage)(CHANNELS, channels);
    (0, bus_1.publishEvent)(events_1.SET_CHANNELS, channels);
    var currentChannel = (0, utils_1.getItemFromStorage)(constants_1.CURRENT_CHANNEL);
    if (currentChannel == null || !channels.find(function (x) { return x.id == currentChannel.id; })) {
        (0, utils_1.setItemInStorage)(constants_1.CURRENT_CHANNEL, channels[0]);
        (0, bus_1.publishEvent)(events_1.SET_CHAT, channels[0]);
        return;
    }
    (0, bus_1.publishEvent)(events_1.SET_CHAT, currentChannel);
}
function onSubscribeAsync() {
    return __awaiter(this, void 0, void 0, function () {
        function onOwnSubscribeResult(ev, resolve, _) {
            (0, bus_1.unsubscribeFromEvent)(events_1.SUBSCRIBE_COMMAND_RESULT, function (_) {
                console.log("unsubscribed from subscribe_result");
            });
            resolve(ev.detail);
        }
        var subscribeResult, _;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0: return [4 /*yield*/, new Promise(function (resolve, reject) {
                        console.log(constants_1.KIND);
                        (0, bus_1.subscribeToEvent)(events_1.SUBSCRIBE_COMMAND_RESULT, function (ev) { return onOwnSubscribeResult(ev, resolve, reject); });
                        (0, bus_1.publishCommand)({ kind: "subscribe", topic: elements_2.subscribeBox.value });
                    })];
                case 1:
                    subscribeResult = _a.sent();
                    return [4 /*yield*/, handleSubscribeResultAsync(subscribeResult)];
                case 2:
                    _ = _a.sent();
                    return [2 /*return*/];
            }
        });
    });
}
var state = {
    first_chat_set: false
};
function handleSubscribeResultAsync(subscribeResult) {
    return __awaiter(this, void 0, void 0, function () {
        var message, targetChannel, existingChannels, newChannelList, currentChannel;
        return __generator(this, function (_a) {
            console.log(subscribeResult.result);
            if (subscribeResult.result != "ok" && subscribeResult.result != "already_subscribed") {
                message = "Could not subscribe to channel:" + elements_2.subscribeBox.value;
                console.log(message);
                return [2 /*return*/, new Error(message)];
            }
            if (subscribeResult.result == 'already_subscribed') {
                console.log("already subscribed");
                return [2 /*return*/];
            }
            targetChannel = subscribeResult.topic;
            existingChannels = (0, utils_1.getItemFromStorage)(CHANNELS);
            if (!existingChannels) {
                (0, utils_1.setItemInStorage)(constants_1.CURRENT_CHANNEL, targetChannel);
                (0, bus_1.publishEvent)(events_1.SET_CHAT, targetChannel);
                (0, utils_1.setItemInStorage)(CHANNELS, [targetChannel]);
                (0, bus_1.publishEvent)(events_1.ADD_CHANNEL, targetChannel);
                return [2 /*return*/];
            }
            newChannelList = __spreadArray(__spreadArray([], existingChannels, true), [targetChannel], false);
            currentChannel = (0, utils_1.getItemFromStorage)(constants_1.CURRENT_CHANNEL);
            console.log(currentChannel);
            if (currentChannel == null || !existingChannels.find(function (x) { return x.id != (currentChannel === null || currentChannel === void 0 ? void 0 : currentChannel.id); })) {
                (0, utils_1.setItemInStorage)(constants_1.CURRENT_CHANNEL, targetChannel);
                (0, utils_1.setItemInStorage)(CHANNELS, newChannelList);
                (0, bus_1.publishEvent)(events_1.SET_CHAT, targetChannel);
                (0, bus_1.publishEvent)(events_1.ADD_CHANNEL, targetChannel);
                return [2 /*return*/];
            }
            (0, utils_1.setItemInStorage)(CHANNELS, newChannelList);
            (0, bus_1.publishEvent)(events_1.ADD_CHANNEL, targetChannel);
            if (state.first_chat_set == false) {
                state.first_chat_set = true;
                (0, bus_1.publishEvent)(events_1.SET_CHAT, targetChannel);
            }
            return [2 /*return*/];
        });
    });
}
function onUnsubscribeAsync(event) {
    return __awaiter(this, void 0, void 0, function () {
        var channel, unsubscribeResult, _;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    channel = event.detail;
                    return [4 /*yield*/, new Promise(function (resolve, _) {
                            var _a;
                            (0, bus_1.subscribeToEvent)(events_1.UNSUBSCRIBE_COMMAND_RESULT, function (ev) {
                                (0, bus_1.unsubscribeFromEvent)(events_1.REFRESH_CHANNELS_COMMAND_RESULT, function (_) {
                                    console.log("unsbuscribed from refresh_channels after unsubscribe from channel");
                                });
                                resolve(ev.detail);
                            });
                            (0, bus_1.publishEvent)(constants_1.SOCKET_COMMAND, (_a = {}, _a[constants_1.KIND] = events_1.UNSUBSCRIBE_COMMAND, _a["topicId"] = channel.id, _a));
                        })];
                case 1:
                    unsubscribeResult = _a.sent();
                    return [4 /*yield*/, handleUnsubscribeResultAsync(unsubscribeResult)];
                case 2:
                    _ = _a.sent();
                    return [2 /*return*/];
            }
        });
    });
}
function handleUnsubscribeResultAsync(unsubscribeResult) {
    return __awaiter(this, void 0, void 0, function () {
        var message, existingChannels, currentChannel, newExistingChannels;
        var _a;
        return __generator(this, function (_b) {
            console.log(unsubscribeResult);
            if (unsubscribeResult.result == "not_joined") {
                console.log("Not joined");
                return [2 /*return*/, "not_joined"];
            }
            if (unsubscribeResult.result != "ok") {
                message = "Could not unsubscribe from channel";
                return [2 /*return*/, new Error(message)];
            }
            existingChannels = (0, utils_1.getItemFromStorage)(CHANNELS);
            if (!existingChannels) {
                (0, utils_1.setItemInStorage)(constants_1.CURRENT_CHANNEL, null);
                (0, utils_1.setItemInStorage)(CHANNELS, []);
                (0, bus_1.publishEvent)(events_1.RESET_CHAT, {});
                return [2 /*return*/];
            }
            if (existingChannels.length == 0) {
                (0, utils_1.setItemInStorage)(constants_1.CURRENT_CHANNEL, null);
                (0, bus_1.publishEvent)(events_1.RESET_CHAT, {});
                return [2 /*return*/];
            }
            currentChannel = (0, utils_1.getItemFromStorage)(constants_1.CURRENT_CHANNEL);
            newExistingChannels = existingChannels.filter(function (x) { return x.id != unsubscribeResult.topicId; });
            (0, utils_1.setItemInStorage)(CHANNELS, newExistingChannels);
            (0, bus_1.publishEvent)(events_1.REMOVE_CHANNEL, (_a = {}, _a[TOPIC_ID] = unsubscribeResult.topicId, _a));
            console.log(unsubscribeResult);
            if (unsubscribeResult.topicId == (currentChannel === null || currentChannel === void 0 ? void 0 : currentChannel.id) && newExistingChannels && newExistingChannels.length > 0) {
                console.log(newExistingChannels);
                console.log("inside last if");
                (0, utils_1.setItemInStorage)(constants_1.CURRENT_CHANNEL, newExistingChannels[0]);
                (0, bus_1.publishEvent)(events_1.SET_CHAT, newExistingChannels[0]);
            }
            return [2 /*return*/];
        });
    });
}
function setChannels(channels) {
    (0, utils_1.setItemInStorage)(CHANNELS, channels);
    //updateChannelsContainer(channels);
    return channels;
}
function onSubscribeResultU(ev) {
    var channels = setChannels(ev.detail.subscriptions);
    if (channels.length == 0) {
        (0, bus_1.publishEvent)(events_1.RESET_CHAT, channels[0]);
        return;
    }
}
function onUnSubscribeResultU(ev) {
    var channels = setChannels(ev.detail.subscriptions);
    if (channels.length == 0) {
        (0, bus_1.publishEvent)(events_1.RESET_CHAT, {});
        return;
    }
    (0, bus_1.publishEvent)(events_1.SET_CHAT, channels.slice(-1));
}
