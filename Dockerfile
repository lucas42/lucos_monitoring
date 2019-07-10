FROM erlang:22-alpine

WORKDIR /web/lucos/monitoring

COPY *.erl ./

RUN erlc server.erl

ENV PORT 8015
EXPOSE $PORT

CMD [ "erl", "-noshell", "-run", "server", "start" ]