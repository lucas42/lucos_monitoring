FROM erlang:22

WORKDIR /web/lucos/monitoring

RUN apt-get update
RUN apt-get install -y erlang-ssl erlang-crypto erlang-public-key erlang-jiffy
ENV ERL_LIBS /usr/lib/erlang/lib/jiffy-0.14.8

COPY public ./
COPY *.erl ./

RUN erlc *.erl

ENV PORT 8015
EXPOSE $PORT
COPY service-list ./

CMD [ "erl", "-noshell", "-run", "server", "start" ]