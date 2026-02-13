FROM erlang:26-alpine

WORKDIR /app

RUN apk add --no-cache git build-base rebar3

COPY rebar.config rebar.lock ./
COPY config ./config
COPY include ./include
COPY src ./src
COPY resources ./resources

RUN rebar3 compile

EXPOSE 8080

CMD ["sh", "-lc", "rebar3 nova serve"]

