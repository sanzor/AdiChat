const eventBus=new EventTarget();

function publishEvent(eventName,eventData){
    const customEvent=new customEvent(eventName,{ data:eventData});
    eventBus.dispatchEvent(customEvent);
}

function subscribeToEvent(eventName,callback){
    eventBus.addEventListener(eventName,callback);
}


export{publishEvent,subscribeToEvent};