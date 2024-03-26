


const eventBus=new EventTarget();

function publishEvent(eventName:string,eventData:any){
    const customEvent=new CustomEvent(eventName,{ "detail":eventData});
    return eventBus.dispatchEvent(customEvent);
}

function subscribeToEvent(eventName:string,callback){
    eventBus.addEventListener(eventName,callback);
}
function unsubscribeFromEvent(eventName,callback){
    eventBus.removeEventListener(eventName,callback);
}
export {publishEvent,subscribeToEvent,unsubscribeFromEvent};
