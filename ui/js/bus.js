"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.publishCommand = exports.unsubscribeFromEvent = exports.subscribeToEvent = exports.publishEvent = void 0;
var eventBus = new EventTarget();
function publishCommand(command) {
    var customEvent = new CustomEvent(command.kind, { "detail": command });
    return eventBus.dispatchEvent(customEvent);
}
exports.publishCommand = publishCommand;
function publishEvent(eventName, eventData) {
    var customEvent = new CustomEvent(eventName, { "detail": eventData });
    return eventBus.dispatchEvent(customEvent);
}
exports.publishEvent = publishEvent;
function subscribeToEvent(eventName, callback) {
    eventBus.addEventListener(eventName, callback);
}
exports.subscribeToEvent = subscribeToEvent;
function unsubscribeFromEvent(eventName, callback) {
    eventBus.removeEventListener(eventName, callback);
}
exports.unsubscribeFromEvent = unsubscribeFromEvent;
