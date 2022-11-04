# ClueCon Drives Car

An entry into a compentition to use SignalWire's api to do fun or insteresting
stuff. Inspired by "Twitch Plays Pokemon", this uses the SignalWire api to
accpet text messsages, dtmf, and voice commands sent to configured number to
control a simple anduino powered robot acting as the car.

# Tech Stack

The components are the robot car itself, the Arduino IDE, a web dashboard, a 3-in-1 FPV camera/transmitter/antenna and an FPV receiver, SignalWire's API, and an
erlang server that ties it all together.

We're using a mono-repo structure as each part is very tied to the
implemetations of the others. The top level `Makefile` will build the Erlang server but
to get it all working you'll need to cd into the directory of the part you
wish to edit follow the build instructions there. That is aside from the SignalWire part; that's done
through the admin portal on their website.

## Car

[Dedicated README](./robot/README.md)

The robot car used for this project is the [UCTRONICS K0070](https://www.uctronics.com/wiki/(SKU:_K0069_/K0070)Smart-Robot-Car-Arduino). The Arduino libraries and example Scratch files that our code is based on can be found at https://github.com/UCTRONICS/Smart-Robot-Car-Arduino.

The Bluetooth module is the [HC-06](https://components101.com/wireless/hc-06-bluetooth-module-pinout-datasheet).

## Arduino IDE

Used for programming the Arduino Uno aboard the robot car.

For whatever reason, the 2.x version of the Arduino IDE doesn't seem to load the Scratch files for the car. Use [the latest 1.x version](https://www.arduino.cc/en/software#legacy-ide-18x) (1.8.19 at time of writing).

## Dashboard

[Dedicated README](./dashboard/README.md)

More of a humor report. When the car is given a command, the same command is
forwarded to the dashboard over a websocket. The dashboard then shows
"information" in the form of memes.

There is no build system for the dashboard. It's a simple web page meant to be
served up locally, and then projected on a wall (or something similar). Of
course there's nothing to stop the page from being hosted on a server somewhere.

### FPV Camera Kit

If this wasn't already exciting enough, we display a live camera feed from the robot car. The camera mounted on the car is the [GOQOTOMO GT02](https://www.amazon.com/GOQOTOMO-GT02-5-8GHz-Transmitter-600TVL/dp/B01MY3QSIE) 3-in-1 camera/transmitter/antenna. The receiver is the [FPVKing FPV OTG Receiver 150CH](https://www.amazon.ca/5-8GHz-Receiver-Downlink-Android-Smartphone/dp/B074T6X1WM) which acts as a webcam device when connected to a computer. The dashboard displays this webcam for some real, live FPV action!

## SignalWire

Handling the phone number allocation as well as sip traffic, sms routing, and
voice caputer is [SignalWire's api](https://signalwire.com/). The full scope of
the signalwire api is beyond what this project needs. At minimum, you will need
a phone number that has voice and sms enabled, with LaML Webhooks for both calls
and messaging.

## Erlang Server

[Dedicated README](./cluecon_drives_car/README.md)

Built using Erlang/OTP-24.

The erlang server recieves webhook requests from SignalWire, tracks votes, and
forwards the results to both the dashboard and the car. The server needs to be
accessible to signal wire over the internet. The solution this project when with
is the same as what SignalWire suggests: use [ngrok](ngrok.com) as a proxy to
your local erlang server.
