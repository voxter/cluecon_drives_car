# ClueCon Drives Car

An entry into a compentition to use SignalWire's api to do fun or insteresting
stuff. Inspired by "Twitch Plays Pokemon", this uses the SignalWire api to
accpet text messsages, dtmf, and voice commands sent to configured number to
control a simple anduino powered robot acting as the car.

# Tech Stack

The components are the car itself, a web dashboard, SignalWire's api, and an
erlang server that ties it all together.

We're using a mono-repo structure as each part is very tied to the
implemetations of the others. The top level `Makefile` will build them all, but
to get it all working you'll need to cd into the directory of the part you
wish to edit and invoke `make` there. Except the SignalWire part; that's done
through the admin portal on their website.

## Car

I don't know the specifics at the moment, such as the model of kit, or other
componeents. This will be updated with the source code that drives the car
eventually.

## Dashboard

More of a humor report. When the car is given a command, the same command is
forwarded to the dashboard over a websocket. The dashboard then shows
"information" in the form of memes.

There is no build system for the dashboard. It's a simple web page meant to be
served up locally, and then projected on a wall (or something similar). Of
course there's nothing to stop the page from being hosted on a server somewhere.

## SignalWire

Handling the phone number allocation as well as sip traffic, sms routing, and
voice caputer is [SignalWire's api](https://signalwire.com/). The full scope of
the signalwire api is beyond what this project needs. At minimum, you will need
a phone number that has voice and sms enabled, with LaML Webhooks for both calls
and messaging.

## Erlang Server

Built using Erlang/OTP-24.

The erlang server recieves webhook requests from SignalWire, tracks votes, and
forwards the results to both the dashboard and the car. The server needs to be
accessible to signal wire over the internet. The solution this project when with
is the same as what SignalWire suggests: use [ngrok](ngrok.com) as a proxy to
your local erlang server.
