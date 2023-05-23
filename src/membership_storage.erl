-module(membership_storage).
-export([subscribe/2,unsubscribe/2]).


-spec subscribe(Topic::binary(),User::binary()) -> ok | already_subscribed | {error,Error::term()}.
subscribe(Topic,User)->
    ok.

-spec unsubscribe(Topic::binary(),User::binary())-> ok | not_joined | {error,Error::term()}.
unsubscribe(Topic,User)->
    ok.