[![Travis][travis badge]][travis]
[![License][license badge]][license]
[![Erlang Versions][erlang version badge]][erlang]
[![Build Tool][build tool]][rebar]


# gateway-config

The Helium configuration application. Manages the configuration of the
Helium Hotspot over Bluetooth, and ties together various services over
dbus.


  * [Features](#features)
  * [Install](#install)

## Features

  * Exposes a GATT BLE tree for configuring the hotspot over bluetooth
  * Signals gateway configuration over dbus (Enabled QRCode based
    registration)
  * Listens for GPS location on SPI and signals the current Position
    of the hotspot over dbus.
  * SOON: Exposes an LED challenge service to authorize hotspot
    configuration over bluetooth

## Install

Make sure you have dbus installed and running on your machine if you
intend to run this in development.

Clone the repo and make a release of the package:

``` shell
make && make release
```

Copy the generated release into it's final destination.

``` shell
cp -R _build/prod/rel/gateway_config /opt/gateway_config
```

Copy the dbus config file into the system.d folder to set permissions
for the config application. The exact location of dbus configuration
may differ on your Linux installation.

``` shell
cp -R _build/prod/rel/gateway_config/config/com.helium.Config.conf /etc/dbus-1/system.d/
```

Restart dbus to pick up the new configuration. This varies per Linux
distribution.

On the Helium Hotspot the gateway-config service is started as part of
system boot.

On a void-linux development system:

``` shell
sudo sv restart dbus
```

Start the gateway-config service. On the Helium Hotspot the service is
started on system init.

On a development system you can start the service by starting a rebar shell:

``` shell
./rebar3 shell
```

<!-- Badges -->
[travis]: https://travis-ci.com/helium/gateway-config
[travis badge]: https://img.shields.io/travis/com/helium/gateway-config/master.svg?style=flat-square
[hex]: https://hex.pm/packages/ebus
[license]: https://github.com/helium/gateway-config/blob/master/LICENSE
[license badge]: https://img.shields.io/github/license/helium/gateway-config.svg
[erlang version badge]: https://img.shields.io/badge/erlang-21.1-blue.svg?style=flat-square
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-orange.svg?style=flat-square
