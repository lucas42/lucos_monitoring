FROM lucas42/lucos_navbar:2.1.49 AS navbar
FROM erlang:28.4.2.0 AS build

WORKDIR /lucos_monitoring
RUN apt-get update
RUN apt-get install -y erlang-ssl erlang-crypto erlang-public-key curl
ENV ERL_LIBS _build/default/lib/
COPY rebar.* ./
RUN rebar3 compile

COPY resources ./
RUN mkdir src
COPY src/* src/
RUN rebar3 as prod release

RUN mkdir -p config
RUN curl -s "https://configy.l42.eu/systems/http" -H "Accept: application/json" > config/info-systems-list.json
RUN curl -s "https://configy.l42.eu/hosts/http" -H "Accept: application/json" > config/info-hosts-list.json
RUN curl -s "https://configy.l42.eu/systems" -H "Accept: application/json" > config/ci-systems-list.json
RUN curl -s "https://configy.l42.eu/components" -H "Accept: application/json" > config/ci-components-list.json

FROM debian:trixie

WORKDIR /web
RUN apt-get update && apt-get install -y ca-certificates wget

COPY --from=build /lucos_monitoring/_build/prod/rel/prod/ ./
COPY --from=navbar lucos_navbar.js .
COPY resources ./
COPY --from=build /lucos_monitoring/config/ ./config

CMD ["bin/prod", "foreground"]