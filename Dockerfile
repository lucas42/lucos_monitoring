FROM erlang:22-alpine

WORKDIR /web/lucos/monitoring

COPY *.erl ./

RUN erlc *.erl

ENV PORT 8015
EXPOSE $PORT
COPY service-list ./

CMD [ "erl", "-noshell", "-run", "server", "start" ]