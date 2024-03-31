import { Command } from "./Domain/Commands/Command";




const eventBus=new EventTarget();

function publishCommand(command:Command){
    const customEvent=new CustomEvent(command.kind,{"detail":command});
    return eventBus.dispatchEvent(customEvent);
}
function publishEvent(eventName:string,eventData:any){
    const customEvent=new CustomEvent(eventName,{ "detail":eventData});
    return eventBus.dispatchEvent(customEvent);
}

function subscribeToEvent(eventName:string,callback:any){
    eventBus.addEventListener(eventName,callback);
}
function unsubscribeFromEvent(eventName:string,callback:any){
    eventBus.removeEventListener(eventName,callback);
}
export {publishEvent,subscribeToEvent,unsubscribeFromEvent,publishCommand};
