function openChannelChat(channelName){
    console.log("inside channel chat");
    console.log("Channel:"+channelLabel);
    if(currentChannel.innerText!=channelName){
        console.log("end1");
        currentChannel.innerText=channelName;
    }
    console.log("end");
}

function updateChatOnMessage(data){
    var messageContainer=createChatContainer(data);


}

function createChatContainer(data){
    var user=data.user;
    var topic=data.topic;
    var message=data.message;
    document.createElement("")
}


