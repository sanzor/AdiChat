
export{run};

async function  run(){
    var bus=new EventTarget();

    bus.addEventListener("send",ev=>{
        var receiveEvent=new CustomEvent("receive",{});
        bus.dispatchEvent(receiveEvent); //simulate return event as response from somewhere
    });

    var result=await new Promise((resolve,reject)=>{
        bus.addEventListener("receive",(ev)=>{
            bus.removeEventListener("receive",ev=>{
                console.log("before await , at unsubscribe");
            });
            if(Date.now().toFixed() %2==0){
                resolve(0)
            }
            else{
                reject(-1);
            }
        });
        var sendEvent=new CustomEvent("send",{});
        bus.dispatchEvent(sendEvent);
        return result;
    });

    console.log("after await");
   
}



