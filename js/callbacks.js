
function handle_callback_message(data){
    console.log("Command:"+ data.command +', result:'+ data.result);
    if(data.command=="subscribe"){
        console.log("xx");
        callback_subscribe(data);
    }
    if(data.command=="unsubscribe"){
        callback_unsubscribe(data);
    }
    if(data.command=="get_subscriptions"){
        callback_get_subscriptions(data);
    }
    if(data.command=="get_messages"){
        callback_get_messages(data);
    }
}

function callback_subscribe(data){
    if(data.result=="ok"){
       
        createSubsribeRow(data.topic);
    }
}
function callback_unsubscribe(data){
    channel=data.topic;
    var table=document.getElementById('channelTable');
    var row=document.getElementById(channel+'channel_row');
    table.removeChild(row);
    
}
function callback_get_messages(data){
    console.log(data.messages);
}
function callback_get_subscriptions(data){

    var table=document.getElementById("channelTable");
    table.innerHTML='';
    var subscriptions=data.result;
    console.log(subscriptions);
    if(subscriptions==='no_subscriptions'){
        return;
    }
    
    subscriptions.forEach(element => {
       createSubsribeRow(element);
    });
    
}
function createSubsribeRow(channelName){
    var table=document.getElementById("channelTable");

    var channelRow=document.createElement("tr");
    channelRow.id=channelName+'_channel_row';

    var cell1=document.createElement("td");
    var label=document.createElement("label");
    label.innerText=channelName;
    cell1.appendChild(label);
    
    var cell2=document.createElement("td");
    var unsubscribeBtn=document.createElement("button");
    unsubscribeBtn.id=channelName+'_unsubscribe_btn';
    unsubscribeBtn.innerText="X";
    unsubscribeBtn.onclick=function(){command_unsubscribe(channelName);};
    cell2.appendChild(unsubscribeBtn);

    channelRow.appendChild(cell1);
    channelRow.appendChild(cell2);
    table.appendChild(channelRow);
}


