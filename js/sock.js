
var ws=null;
function init(){
    const terminationEvent = 'onpagehide' in self ? 'pagehide' : 'unload';
    window.addEventListener(terminationEvent, (event) => {
    if (event.persisted === false) {
        // client is gone
        ws.onclose = function () { };
        ws.close();
    }});
}
 function connect (url){
    var socket=new WebSocket("ws://localhost:8080/ws/user/adi/cookie/cook");
    socket.onopen=function (e){
        alert("Connection established");
        let message=JSON.stringify({
            topic:"adita",
            message:"Hello from adita"
        });
        socket.send(message)
    }
    socket.onmessage=function(message){
        alert(`Message received: ${message.data}`);
    }
    socket.onclose=function(e){
        alert(`Socket closed with code: ${e.code} , reason: ${e.reason}`);
        alert(`Connection died`);
    }
    
}
function disconnect(url){
    ws.close();
}
