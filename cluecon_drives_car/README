# ClueCon Drives Car - Erlang

Handles the webhooks from SignalWire, tablulating votes, giving the dashboard
fun info, and telling the car what to do.

# Build

`make` should be enough.

# Run

`make shell` will start an interactive erlang shell and start the server.

Pair your computer with the car.

The server was developed running behind a nat / firewall, so we used
(ngrok)[ngrok.com] to accept the webhooks from SignalWire and send it to use.

An exmaple ngrok config is provided with the default port cluecon uses. However,
if you've never used ngrok before, it is easiest to do the simplest invocation.

    ngrok http 7000

This will start a ngrok proxy that forwards to a plaintext http server running
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

When you started ngrok it output a url. Use that url to call
`sw_api:update_webhooks/1`. This updates the SignalWire phone number you have
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
baud, and it'll set up the serial port for you.

`cdc_car_driver:car_port/0` has a default for OS X on the developers machine.
The default buad of `115200` is valid.
