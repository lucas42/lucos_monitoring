FROM erlang:22-alpine

WORKDIR /web/lucos/monitoring

RUN apk add erlang-ssl erlang-crypto erlang-public-key

COPY *.erl ./

RUN erlc *.erl

ENV PORT 8015
EXPOSE $PORT
COPY service-list ./

CMD [ "erl", "-noshell", "-run", "server", "start" ]