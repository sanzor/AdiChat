
//socket commands
const REFRESH_CHANNELS_COMMAND:string="get_subscriptions";
const SUBSCRIBE_COMMAND:string="subscribe";
const UNSUBSCRIBE_COMMAND:string="unsubscribe";
const PUBLISH_MESSAGE:string="publish";
const SELF_PUBLISH_MESSAGE:string="self_publish";
const GET_OLDER_MESSAGES:string="get_older_messages";
const GET_NEWEST_MESSAGES:string="get_newest_messages";



//socket command results
const REFRESH_CHANNELS_COMMAND_RESULT:string="get_subscriptions_result";
const UNSUBSCRIBE_BUTTON_CLICK:string="unsubscribe_button_click";
const CHANNEL_CLICK:string="channel_click";
const UNSUBSCRIBE_COMMAND_RESULT:string="unsubscribe_result";
const UNSUBSCRIBE_COMMAND_RESULT_U:string="unsubscribe_result_u";
const SUBSCRIBE_COMMAND_RESULT:string="subscribe_result";
const SUBSCRIBE_COMMAND_RESULT_U:string="subscribe_result_u";

const SOCKET_RECEIVE:string="socketReceive";
const SOCKET_CLOSED:string="socketClosed";


const NEW_INCOMING_MESSAGE:string="new_channel_message";
const SET_CHAT:string="set_chat";
const SET_CHAT_DOM:string="set_chat_dom";
const RESET_CHAT:string="reset_chat";
const RESET_CHAT_DOM:string="reset_chat_dom";

const SET_CHANNELS:string="set_channels";
const REMOVE_CHANNEL:string="remove_channel";
const ADD_CHANNEL:string="add_channel";

const SHOW_MAIN:string="showMain";
const HIDE_MAIN:string="hideMain";

const SHOW_LOGIN:string="showLogin";
const HIDE_LOGIN:string="hideLogin";

const SHOW_REGISTER:string="showRegister";
const HIDE_REGISTER:string="hideRegister";


export {
    REFRESH_CHANNELS_COMMAND,
    REFRESH_CHANNELS_COMMAND_RESULT,

    SET_CHANNELS,
    ADD_CHANNEL,
    REMOVE_CHANNEL,
    
    CHANNEL_CLICK,
    UNSUBSCRIBE_BUTTON_CLICK,
    UNSUBSCRIBE_COMMAND,
    UNSUBSCRIBE_COMMAND_RESULT,
    UNSUBSCRIBE_COMMAND_RESULT_U,


    SUBSCRIBE_COMMAND,
    SUBSCRIBE_COMMAND_RESULT,
    SUBSCRIBE_COMMAND_RESULT_U,
    SOCKET_RECEIVE,
    SOCKET_CLOSED,
    SET_CHAT,
    SET_CHAT_DOM,
    RESET_CHAT,
    RESET_CHAT_DOM,
    NEW_INCOMING_MESSAGE,

    PUBLISH_MESSAGE,
    SELF_PUBLISH_MESSAGE,
    GET_OLDER_MESSAGES,
    GET_NEWEST_MESSAGES,

    SHOW_MAIN,
    HIDE_MAIN,
    
    SHOW_LOGIN,
    HIDE_LOGIN,

    SHOW_REGISTER,
    HIDE_REGISTER};