"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var bus_1 = require("./bus");
var utils_1 = require("./utils");
var sock_1 = require("./sock");
var events_1 = require("./events");
var elements_1 = require("./elements");
(0, bus_1.subscribeToEvent)(events_1.SHOW_MAIN, onShowMain);
(0, bus_1.subscribeToEvent)(events_1.HIDE_MAIN, onHideMain);
(0, bus_1.subscribeToEvent)("connect", onConnect);
(0, bus_1.subscribeToEvent)("socketEvent", onSocketEvent);
elements_1.connectBtn.addEventListener("click", btnConnect);
elements_1.disconnectBtn.addEventListener("click", onDisconnect);
elements_1.logoutBtn.addEventListener("click", onLogout);
function onShowMain(e) {
    (0, utils_1.showElement)("mainModal");
    (0, bus_1.publishEvent)("connect", {});
}
function onHideMain(e) {
    (0, utils_1.hideElement)("mainModal");
}
function onConnect(e) {
    (0, sock_1.connect)();
}
function onSocketEvent() {
}
function onDisconnect() {
}
function onLogout() {
    console.log("On logout");
    (0, bus_1.publishEvent)("close_socket", {});
    localStorage.removeItem("user");
    (0, bus_1.publishEvent)(events_1.HIDE_MAIN, {});
    (0, bus_1.publishEvent)(events_1.SHOW_LOGIN, {});
}
function btnConnect() {
}
