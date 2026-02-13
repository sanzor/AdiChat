-module(email_service).
-export([send_verification_email/2]).

-spec send_verification_email(Email::binary(), Token::binary()) -> ok | {error, term()}.
send_verification_email(Email, Token) ->
    SmtpConfig = application:get_env(wsapp, smtp, []),
    Relay = proplists:get_value(relay, SmtpConfig, "smtp.gmail.com"),
    Port = proplists:get_value(port, SmtpConfig, 587),
    Username = proplists:get_value(username, SmtpConfig, "noreply@example.com"),
    Password = proplists:get_value(password, SmtpConfig, ""),
    FromEmail = proplists:get_value(from_email, SmtpConfig, Username),
    BaseUrl = proplists:get_value(base_url, SmtpConfig, "http://localhost:5173"),

    VerifyUrl = lists:flatten(io_lib:format("~s/verify?token=~s", [BaseUrl, binary_to_list(Token)])),

    Subject = "Verify your AdiChat account",
    Body = lists:flatten(io_lib:format(
        "Welcome to AdiChat!\r\n\r\n"
        "Please verify your email by clicking the link below:\r\n\r\n"
        "~s\r\n\r\n"
        "This link expires in 24 hours.\r\n\r\n"
        "If you did not create an account, you can ignore this email.",
        [VerifyUrl])),

    From = binary_to_list(ensure_binary(FromEmail)),
    To = binary_to_list(ensure_binary(Email)),

    MimeBody = mimemail:encode({
        <<"text">>, <<"plain">>,
        [
            {<<"From">>, list_to_binary(From)},
            {<<"To">>, list_to_binary(To)},
            {<<"Subject">>, list_to_binary(Subject)}
        ],
        [],
        list_to_binary(Body)
    }),

    Options = [
        {relay, Relay},
        {port, Port},
        {username, Username},
        {password, Password},
        {tls, starttls}
    ],

    case gen_smtp_client:send({From, [To], MimeBody}, Options) of
        {ok, _Receipt} ->
            io:format("\nVerification email sent to ~s\n", [To]),
            ok;
        {error, Reason} ->
            io:format("\nFailed to send verification email to ~s: ~p\n", [To, Reason]),
            {error, Reason}
    end.

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_list(V) -> list_to_binary(V).
