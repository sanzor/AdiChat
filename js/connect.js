

 function connect (url){
    document.socket=new WebSocket("ws://localhost:8080/ws/user/adi/cookie/cook");
    socket.onopen=function (e){
        alert("Connection established");
        let message=JSON.stringify({
            topic:"adita",
            message:"Hello from adita"
        });
        socket.send(message)
    }
    window.
    socket.onmessage=function(message){
        alert(`Message received: ${message.data}`);
    }
    socket.onclose=function(e){
        alert(`Socket closed with code: ${e.code} , reason: ${e.reason}`);
        alert(`Connection died`);
    }
    this.g
}
function disconnect(url){

}
