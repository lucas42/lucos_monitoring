FROM lucas42/lucos_navbar:2.1.20 AS navbar
FROM erlang:28.4.1.0 AS build

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

RUN curl "https://configy.l42.eu/systems/http?fields=domain" -H "Accept: text/csv;header=absent" > service-list
RUN curl "https://configy.l42.eu/hosts/http?fields=domain" -H "Accept: text/csv;header=absent" >> service-list
RUN curl -s "https://configy.l42.eu/systems" -H "Accept: application/json" > ci-systems-list
RUN curl -s "https://configy.l42.eu/components" -H "Accept: application/json" > ci-components-list

FROM debian:trixie

WORKDIR /web
RUN apt-get update && apt-get install -y ca-certificates wget

COPY --from=build /lucos_monitoring/_build/prod/rel/prod/ ./
COPY --from=navbar lucos_navbar.js .
COPY resources ./
COPY --from=build /lucos_monitoring/service-list ./
COPY --from=build /lucos_monitoring/ci-systems-list ./
COPY --from=build /lucos_monitoring/ci-components-list ./

CMD ["bin/prod", "foreground"]