FROM lucas42/lucos_navbar:latest AS navbar
FROM erlang:27 AS build

WORKDIR /lucos_monitoring
RUN apt-get update
RUN apt-get install -y erlang-ssl erlang-crypto erlang-public-key
ENV ERL_LIBS _build/default/lib/
COPY rebar.* ./
RUN rebar3 compile

COPY public ./
RUN mkdir src
COPY src/* src/
RUN rebar3 as prod release


FROM debian:bookworm

WORKDIR /web
RUN apt-get update && apt-get install -y ca-certificates

COPY --from=build /lucos_monitoring/_build/prod/rel/prod/ ./
COPY --from=navbar lucos_navbar.js .
COPY public ./
COPY service-list .

ENV PORT 8015
EXPOSE $PORT
CMD ["bin/prod", "foreground"]