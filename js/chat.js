function openChannelChat(channelName){
    console.log("inside channel chat");
    console.log("Channel:"+channelLabel);
    if(currentChannel.innerText!=channelName){
        console.log("end1");
        currentChannel.innerText=channelName;
    }
    console.log("end");
    
}