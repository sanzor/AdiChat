import{PUBLISH} from "../constants";
interface PublishMessage{
        [KIND]:typeof PUBLISH_MESSAGE,
        [CHANNEL_ID]:string,
        [MESSAGE_CONTENT]:string
}