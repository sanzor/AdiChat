


const eventBus=new EventTarget();

function publishEvent(eventName,eventData){
    const customEvent=new CustomEvent(eventName,{ data:eventData});
    return eventBus.dispatchEvent(customEvent);
}

function subscribeToEvent(eventName,callback){
    eventBus.addEventListener(eventName,callback);
}

export {publishEvent,subscribeToEvent};
