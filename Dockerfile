FROM erlang:23

WORKDIR /web/lucos/monitoring

RUN apt-get update
RUN apt-get install -y erlang-ssl erlang-crypto erlang-public-key erlang-jiffy
RUN git clone https://github.com/gen-smtp/gen_smtp.git
ENV ERL_LIBS /usr/lib/erlang/lib/jiffy-0.14.11

COPY public ./
COPY *.erl ./
RUN cp gen_smtp/src/*.erl ./
RUN rm smtp_server_example.erl

RUN erlc *.erl

ENV PORT 8015
EXPOSE $PORT
COPY service-list ./

CMD [ "erl", "-noshell", "-run", "server", "start" ]