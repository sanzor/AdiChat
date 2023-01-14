export {start};

function start (){
    let socket=new WebSocket("ws://localhost:8080/ws/user/adi");
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
