FROM lucas42/lucos_navbar:latest as navbar
FROM erlang:27

WORKDIR /web/lucos/monitoring

RUN apt-get update
RUN apt-get install -y erlang-ssl erlang-crypto erlang-public-key

ENV ERL_LIBS _build/default/lib/
COPY rebar.config ./
RUN rebar3 compile

COPY public ./
COPY --from=navbar lucos_navbar.js .
COPY *.erl ./

RUN erlc *.erl

ENV PORT 8015
EXPOSE $PORT
COPY service-list ./

CMD [ "erl", "-noshell", "-run", "server", "start" ]