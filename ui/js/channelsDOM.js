"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var bus_1 = require("./bus");
var constants_1 = require("./constants");
var events_1 = require("./events");
var elements_1 = require("./elements");
(0, bus_1.subscribeToEvent)(events_1.REMOVE_CHANNEL, onRemoveChannelFromDOM);
(0, bus_1.subscribeToEvent)(events_1.ADD_CHANNEL, onAddChannelToDOM);
(0, bus_1.subscribeToEvent)(events_1.SET_CHANNELS, onSetDOMChannels);
(0, bus_1.subscribeToEvent)(events_1.NEW_INCOMING_MESSAGE, onNewMessageOnChannel);
function getChannelDomElementById(topicId) {
    var childrenArray = Array.from(elements_1.channelsContainer.children);
    var elem = childrenArray.find(function (x) {
        var channelId = parseInt(x.getAttribute('CHANNEL_ID'));
        return channelId === topicId;
    });
    return elem;
}
function onSetDOMChannels(ev) {
    var channels = ev.detail;
    setChannelsContainer(channels);
}
function onNewMessageOnChannel(ev) {
    var message = ev.detail;
    var channelElement = getChannelDomElementById(ev.detail.topicId);
}
function onAddChannelToDOM(ev) {
    var channel = ev.detail;
    var newChannel = createChannelContainer(channel);
    elements_1.channelsContainer.appendChild(newChannel);
}
function onRemoveChannelFromDOM(ev) {
    var channelElement = getChannelDomElementById(ev.detail.topicId);
    if (channelElement != null) {
        elements_1.channelsContainer.removeChild(channelElement);
    }
}
function resetChannels() {
    elements_1.channelsContainer.innerHTML = '';
}
function setChannelsContainer(channels) {
    console.log(channels);
    resetChannels();
    var headerRow = document.createElement("tr");
    var h1 = document.createElement("th");
    var h2 = document.createElement("th");
    headerRow.appendChild(h1);
    headerRow.appendChild(h2);
    if (channels === 'no_channels') {
        return elements_1.channelsContainer;
    }
    var channelContainers = createChannelContainers(channels);
    channelContainers.forEach(function (element) {
        elements_1.channelsContainer.appendChild(element);
    });
    return elements_1.channelsContainer;
}
function createChannelContainers(channels) {
    console.log(channels);
    var items = channels.map(createChannelContainer);
    return items;
}
function createChannelContainer(channel) {
    var channelContainer = document.createElement("span");
    channelContainer.setAttribute(constants_1.CHANNEL_ID, channel.id);
    channelContainer.setAttribute("class", "channelRow");
    channelContainer.setAttribute("channelData", JSON.stringify(channel));
    var unsubscribeBtn = createUnsubscribeChannelButton(channel);
    var openChatButton = createDisplayChannelChatButton(channel);
    var newMessagesBox = createNewMessagesBox(channel);
    channelContainer.appendChild(newMessagesBox);
    channelContainer.appendChild(unsubscribeBtn);
    channelContainer.appendChild(openChatButton);
    return channelContainer;
}
function createUnsubscribeChannelButton(channel) {
    var unsubscribeBtn = document.createElement("button");
    unsubscribeBtn.id = channel.id + '_unsubscribe_btn';
    unsubscribeBtn.innerText = "X";
    unsubscribeBtn.setAttribute("class", "channelRowUnsubscribeBtn");
    unsubscribeBtn.setAttribute(constants_1.CHANNEL, JSON.stringify(channel));
    unsubscribeBtn.onclick = function () {
        (0, bus_1.publishEvent)(events_1.UNSUBSCRIBE_BUTTON_CLICK, channel);
    };
    return unsubscribeBtn;
}
function createDisplayChannelChatButton(channel) {
    var channelButton = document.createElement("button");
    channelButton.id = channel.id;
    channelButton.setAttribute('content', channel.name);
    channelButton.setAttribute("class", 'button');
    channelButton.setAttribute("style", "channelButton");
    channelButton.textContent = channel.name;
    channelButton.onclick = function (_) { (0, bus_1.publishEvent)(events_1.CHANNEL_CLICK, channel); };
    return channelButton;
}
function createNewMessagesBox(channel) {
    var newMessagesBox = document.createElement("p");
    newMessagesBox.setAttribute("class", "newMessagesBox");
    newMessagesBox.innerHTML = "0";
    return newMessagesBox;
}
