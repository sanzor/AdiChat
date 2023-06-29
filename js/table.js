function resetSubscriptionTable(){
    var table=document.getElementById("channelTable");
    table.innerHTML='';
}
function createSubscriptionTable(subscriptions){
    var table=document.getElementById("channelTable");
    table.innerHTML='';
    var headerRow=document.createElement("tr");
    var h1=document.createElement("th");
    var h2=document.createElement("th");
    h1.innerText="Channel";
    h2.innerText="-";
    headerRow.appendChild(h1);
    headerRow.appendChild(h2);
    if(subscriptions==='no_subscriptions'){
        return;
    }
    subscriptions.forEach(element => {
       createSubscriptionRow(element);
    });
}

function createSubscriptionRow(channelName){
    var table=document.getElementById("channelTable");
    var channelRow=document.createElement("tr");
    channelRow.setAttribute("class","channelTableRow");
    channelRow.id=channelName+'_channel_row';

    var channelButtonCell=document.createElement("td");
    var channelButton=createChannelButton(channelName);
    channelButtonCell.appendChild(channelButton);
    
    
    var unsubscribeBtnCell=document.createElement("td");
    var unsubscribeBtn=createUnsubscribeChannelButton(channelName);
    unsubscribeBtnCell.appendChild(unsubscribeBtn);
    channelRow.appendChild(unsubscribeBtnCell);
    channelRow.appendChild(channelButtonCell);
    table.appendChild(channelRow);
}



function createUnsubscribeChannelButton(channelName){
    var unsubscribeBtn=document.createElement("button");
    unsubscribeBtn.id=channelName+'_unsubscribe_btn';
    unsubscribeBtn.innerText="X";
    unsubscribeBtn.setAttribute("class","channelTableRowUnsubscribeBtn")
    console.log(channelName);
    unsubscribeBtn.onclick=function(){command_unsubscribe(channelName);};
    return unsubscribeBtn;
}
function createChannelButton(channelName){
    var channelButton=document.createElement("button");
    channelButton.id=channelName;
    channelButton.setAttribute('content',channelName);
    channelButton.setAttribute("class",'button');
    channelButton.setAttribute("style","channelButton");
    channelButton.textContent=channelName;
    channelButton.onclick=function(args){openChannelChat(channelName)};
    return channelButton;
}
function removeSubscriptionRow(){
    var table=document.getElementById('channelTable');
    var row=document.getElementById(channel+'channel_row');
    table.removeChild(row);
}