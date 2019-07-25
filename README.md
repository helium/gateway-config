[![Travis][travis badge]][travis]
[![License][license badge]][license]
[![Erlang Versions][erlang version badge]][erlang]
[![Build Tool][build tool]][rebar]


# gateway-config

The Helium configuration application. Manages the configuration of the
Helium Hotspot over Bluetooth, and ties together various services over dbus.


  * [Features](#features)
  * [Install](#install)

## Features

  * Exposes a GATT BLE tree for configuring the hotspot over Bluetooth.
  * Signals gateway configuration (public key and Wi-Fi credentials) over dbus.
  * Listens for GPS location on SPI and signals the current Position
    of the hotspot over dbus.

## Install

To build this project, make sure you have libdbus-1-dev (or its equivalent)
and cmake installed on your development machine.

To run this project, make sure you have dbus, bluez and connman installed
and running on your development machine.

Clone the repo and make a release of the package:

``` shell
git clone https://github.com/helium/gateway-config.git
cd gateway-config
make && make release
```

Copy the generated release to its final destination.

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

On a Void Linux development system:

``` shell
sudo sv restart dbus
```

Start the gateway-config service.

On the Helium Hotspot the service is started on system boot.

On a development system you can start the service by starting a rebar shell:

``` shell
sudo ./rebar3 shell
```

Or `cd` to the release directory and run the service in the background:

``` shell
cd _build/prod/rel/gateway_config
sudo bin/gateway_config start
```

<!-- Badges -->
[travis]: https://travis-ci.com/helium/gateway-config
[travis badge]: https://img.shields.io/travis/com/helium/gateway-config/master.svg?style=flat-square
[hex]: https://hex.pm/packages/ebus
[license]: https://github.com/helium/gateway-config/blob/master/LICENSE
[license badge]: https://img.shields.io/github/license/helium/gateway-config.svg
[erlang version badge]: https://img.shields.io/badge/erlang-21.1-blue.svg?style=flat-square
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-orange.svg?style=flat-square

[rebar]: http://rebar3.org
[erlang]: http://erlang.org
