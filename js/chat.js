function openChannelChat(channelName){
    console.log("inside channel chat");
    var channelLabel=document.getElementById("currentChannelNameLabel");
    console.log(channelLabel);
    if(channelLabel.innerText!=channelName){
        console.log("end1");
        channelLabel.innerText=channelName;
    }
    console.log("end");
    
}