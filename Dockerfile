FROM erlang:22.3.4-alpine AS base

RUN apk add --no-cache --update ncurses dbus bluez erlang \
    libsodium libstdc++

ARG REBAR_DIAGNOSTIC=0
ENV DIAGNOSTIC=${REBAR_DIAGNOSTIC}

ARG REBAR_BUILD_TARGET
ARG TAR_PATH=_build/$REBAR_BUILD_TARGET/rel/*/*.tar.gz

FROM base AS build

RUN apk add --no-cache --update \
    autoconf automake bison build-base bzip2 cmake curl \
    dbus-dev flex git gmp-dev libsodium-dev libtool linux-headers lz4 \
    openssl-dev pkgconfig protoc sed tar wget cargo

WORKDIR /tmp/gateway_config
COPY . .
RUN make && make release

FROM base AS final

COPY --from=build /tmp/gateway_config/_build/prod/rel/gateway_config /opt/gateway_config
WORKDIR /opt/gateway_config

ENTRYPOINT ["/opt/gateway_config/bin/gateway_config"]
CMD ["foreground"]
