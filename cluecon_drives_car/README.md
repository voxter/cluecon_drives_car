# ClueCon Drives Car - Erlang

Handles the webhooks from SignalWire, tablulating votes, giving the dashboard
fun info, and telling the car what to do.

# Dependencies

- [Erlang/OTP 24+](https://www.erlang.org/downloads)
  - Make sure `erl` is on your `PATH`
- [GNU Make](https://www.gnu.org/software/make/)

# Build

`make` should be enough.

# Run

`make shell` will start an interactive erlang shell and start the server. By default, an HTTP server will be started on port 7000. If the port is already in use by another service on your system (e.g. if you are have Sharing > AirPlay Receiver enabled in macOS's System Preferences), you can override the port using the `WEB_SERVER_PORT` environment variable: `WEB_SERVER_PORT=7001 make shell`.

At this point, pair the car with your computer. See [the robot README](../robot/README.md).

The server was developed running behind a nat / firewall, so we used
[ngrok](https://ngrok.com/) to accept the webhooks from SignalWire and forward them to us.

An exmaple ngrok config is provided with the default port cluecon uses. However,
if you've never used ngrok before, it is easiest to do the simplest invocation.

    ngrok http $PORT

where `$PORT` is the port being used for the Erlang server.

For example, `ngrok http 7000` will start an ngrok proxy that forwards to a plaintext HTTP server running
on port 7000. The server does not run using https, so if you plan to expose this
to the internet, it is recommended to run it behind a reverse proxy, such as
nginx.

# Configuration

There are 3 parts to the configuration.

1. SignalWire API communication
2. Webhook configuration
3. Car communication

## SignalWire API

You'll need to have a SignalWire account setup. Within the account, have a
phone number provisioned with phone and sms enabled. You need your `project_id`,
`project_token`, `space_url`, `phone_number`, `phone_number_id`.

Once you've gathered that, use the following to configure the application:

```erlang
sw_api:project_id(<<"poject_id">>).
sw_api:token(<<"project_token">>).
% note the next one is a string, not a binary.
sw_api:host("space_url").
sw_api:phone_number(<<"phone_number">>).
sw_api:phone_number_id(<<"phone_number_id">>).
```

The defaults are blank, and therefore will break horribly.

## Webhook configuration

When you started ngrok it output a URL. Pass that URL as a string to `sw_api:update_webhooks/1`. This updates the SignalWire phone number you have
configured to point to the ngrok url. If you are not using ngrok, you can skip
this.

If you are setting the webhooks manually, the server expects them as POST at the
following paths:

* when a call comes in: `/calls`
* when a call changes status: `/calls/change`
* handle messages: `/messages`.

## Car

The `cdc_car_driver` doesn't know how to communicate to car, and needs to be
told about the serial port (ie, the /dev/filename thing). For OSX, this will
be something like `/dev/cu.HC-06-Port`.

`cdc_car_driver:car_port/2` is the joy for that. Pass it the device file name and
baud, and it'll set up the serial port for you. For example:

```erlang
cdc_car_driver:car_port("/dev/cu.HC-06-Port", 115200).
```

`cdc_car_driver:car_port/0` has a default for OS X on the developers machine.
The default buad of `115200` is valid.

If commands are misbehaving or you are not receiving `command received` payloads back from the robot car, you might have a baud rate mismatch. See [Re-programming the HC-06](../robot/README.md#re-programming-the-hc-06) for some potential help.
