-module(auth_controller).
-include("../../include/domain.hrl").
-export([
         login/1,
         register/1,
         verify_user/1,
         resend_verification/1
        ]).

login(_Req=#{json := #{<<"email">> := Email, <<"password">> := Password}}) ->
    case wsapp_server:login(Email, Password) of
        {ok, User} ->
            Payload = utils:from_user(User),
            {json, 200, #{<<"Content-Type">> => <<"application/json">>}, Payload};
        user_does_not_exist ->
            {json, 404, #{}, #{<<"error">> => <<"user_not_found">>}};
        invalid_password ->
            {json, 401, #{}, #{<<"error">> => <<"invalid_password">>}};
        not_verified ->
            {json, 403, #{}, #{<<"error">> => <<"not_verified">>,
                               <<"message">> => <<"Please verify your email before logging in">>}};
        not_active ->
            {json, 403, #{}, #{<<"error">> => <<"not_active">>,
                               <<"message">> => <<"Your account has been deactivated">>}}
    end.

register(_Req=#{json := #{<<"name">> := Name, <<"email">> := Email, <<"password">> := Password}}) ->
    Params = #create_user_params{name = Name, email = Email, password = Password},
    case wsapp_server:create_user(Params) of
        {ok, User} ->
            %% Create verification token and send email
            case wsapp_server:create_verification_token(User#user.id) of
                {ok, Token} ->
                    email_service:send_verification_email(Email, Token);
                _ -> ok
            end,
            Payload = utils:from_user(User),
            {json, 201, #{<<"Content-Type">> => <<"application/json">>}, Payload};
        {error, user_already_exists} ->
            {json, 409, #{}, #{<<"error">> => <<"user_already_exists">>}};
        {error, {bad_request, ValidationErrors}} ->
            {json, 400, #{<<"Content-Type">> => <<"application/json">>}, ValidationErrors};
        {error, Error} ->
            {json, 500, #{<<"Content-Type">> => <<"application/json">>}, Error}
    end.

verify_user(_Req=#{parsed_qs := #{<<"token">> := Token}}) ->
    case wsapp_server:verify_user_by_token(Token) of
        {ok, User} ->
            Payload = utils:from_user(User),
            {json, 200, #{<<"Content-Type">> => <<"application/json">>}, Payload};
        token_not_found ->
            {json, 404, #{}, #{<<"error">> => <<"token_not_found">>,
                               <<"message">> => <<"Invalid verification link">>}};
        token_expired ->
            {json, 410, #{}, #{<<"error">> => <<"token_expired">>,
                               <<"message">> => <<"Verification link has expired. Please request a new one.">>}};
        {error, Error} ->
            {json, 500, #{}, #{<<"error">> => Error}}
    end.

resend_verification(_Req=#{json := #{<<"email">> := Email}}) ->
    case wsapp_server:resend_verification(Email) of
        ok ->
            {json, 200, #{}, #{<<"message">> => <<"Verification email sent">>}};
        user_does_not_exist ->
            {json, 404, #{}, #{<<"error">> => <<"user_not_found">>}};
        already_verified ->
            {json, 400, #{}, #{<<"error">> => <<"already_verified">>,
                               <<"message">> => <<"This account is already verified">>}};
        {error, Error} ->
            {json, 500, #{}, #{<<"error">> => Error}}
    end.
