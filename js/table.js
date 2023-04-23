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
    channelRow.id=channelName+'_channel_row';

    var cell1=document.createElement("td");
    var buttonCell=createChannelCell(channelName);
    cell1.appendChild(buttonCell);
    
    var cell2=document.createElement("td");
    cell2.setAttribute("bgcolor","red");
    var unsubscribeBtn=document.createElement("button");
    unsubscribeBtn.id=channelName+'_unsubscribe_btn';
    unsubscribeBtn.innerText="X";
    console.log(channelName);
    unsubscribeBtn.onclick=function(){command_unsubscribe(channelName);};
    cell2.appendChild(unsubscribeBtn);

    channelRow.appendChild(cell2);
    channelRow.appendChild(cell1);
    table.appendChild(channelRow);
}

function createChannelCell(channelName){
    var cell1=document.createElement("td");
    var messageStreamBtn=document.createElement("button");
    messageStreamBtn.setAttribute('content',channelName);
    messageStreamBtn.setAttribute("class",'button');
    messageStreamBtn.setAttribute("style","channelButton");
    messageStreamBtn.textContent=channelName;
    messageStreamBtn.click=function(){showMessageStream(channelName)};
    cell1.appendChild(messageStreamBtn);
    return cell1;
}
function removeSubscriptionRow(){
    var table=document.getElementById('channelTable');
    var row=document.getElementById(channel+'channel_row');
    table.removeChild(row);
}