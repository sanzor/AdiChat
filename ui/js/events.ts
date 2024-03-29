
//socket commands
const REFRESH_CHANNELS_COMMAND="get_subscriptions";
const SUBSCRIBE_COMMAND="subscribe";
const UNSUBSCRIBE_COMMAND="unsubscribe";
const PUBLISH_MESSAGE="publish";
const SELF_PUBLISH_MESSAGE="self_publish";
const GET_OLDER_MESSAGES="get_older_messages";
const GET_NEWEST_MESSAGES="get_newest_messages";



//socket command results
const REFRESH_CHANNELS_COMMAND_RESULT="get_subscriptions_result";
const UNSUBSCRIBE_BUTTON_CLICK="unsubscribe_button_click";
const CHANNEL_CLICK="channel_click";
const UNSUBSCRIBE_COMMAND_RESULT="unsubscribe_result";
const UNSUBSCRIBE_COMMAND_RESULT_U="unsubscribe_result_u";
const SUBSCRIBE_COMMAND_RESULT="subscribe_result";
const SUBSCRIBE_COMMAND_RESULT_U="subscribe_result_u";

const SOCKET_RECEIVE="socketReceive";
const SOCKET_CLOSED="socketClosed";


const NEW_INCOMING_MESSAGE="new_channel_message";
const SET_CHAT="set_chat";
const SET_CHAT_DOM="set_chat_dom";
const RESET_CHAT="reset_chat";
const RESET_CHAT_DOM="reset_chat_dom";

const SET_CHANNELS="set_channels";
const REMOVE_CHANNEL="remove_channel";
const ADD_CHANNEL="add_channel";

const SHOW_MAIN="showMain";
const HIDE_MAIN="hideMain";

const SHOW_LOGIN="showLogin";
const HIDE_LOGIN="hideLogin";

const SHOW_REGISTER="showRegister";
const HIDE_REGISTER="hideRegister";


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