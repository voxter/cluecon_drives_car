# Robot

Model: [UCTRONICS K0070](https://www.uctronics.com/wiki/(SKU:_K0069_/K0070)Smart-Robot-Car-Arduino)

Bluetooth module: [HC-06](https://components101.com/wireless/hc-06-bluetooth-module-pinout-datasheet)

## Dependencies

- [Arduino IDE 1.x](https://www.arduino.cc/en/software#legacy-ide-18x)
- [UCTRONICS Arduino Smart Robot Car libraries](https://github.com/UCTRONICS/Smart-Robot-Car-Arduino)

## Build

1. Assemble the robot according to the instructions in [the manuals directory](./K0070%20Manuals).
2. Follow the instructions in Chapter 7.2 of [K0070.pdf](./K0070%20Manuals/K0070.pdf) to set up the Smart Robot Car libraries and connect to the Arduino Uno's serial port.

> ⚠️ Take note - you cannot program the Arduino while the Bluetooth module is connected.

3. Open [the robot car Scratch](./Smart_Robot_Car_K0070_scratch/Smart_Robot_Car_K0070_scratch.ino) in the Arduino IDE.

4. Upload the sketch onto the Arduino.

## Run

1. Disconnect the robot car from your machine.
2. Attach the Bluetooth module.
3. Turn on the robot car.
4. Pair your computer with the robot car's Bluetooth module. Out of the factory, the module appears with the name `HC-06`.

> ⚠️ If you have just as bad luck as us, fully connecting to the Bluetooth module on macOS after pairing will cause this whole thing to break. Completely remove the module from your Bluetooth settings and re-pair. Leave the module paired but **not** connected. Don't ask us why this is required.

5. Start or finish setting up [the Erlang server](../cluecon_drives_car/README.md).

## Troubleshooting

### Re-programming the HC-06

Glad to see you've got the same headache we did. There's a good chance your HC-06 is operating at a different baud rate than you specified to the Erlang app.

You can use [the set-baud-rate sketch](./set-baud-rate/set-baud-rate.ino) to try and reset the baud rate.

1. Read the sketch and make changes as fit.
2. Program the Arduino using the sketch.
3. Disconnect the robot.
4. Reconnect the Bluetooth module.
5. See if the new name appears in your machine's Bluetooth settings, indicating that the changes took effect.

## Misc

- `bt-test.c` is based on `rfcomm-serial-setname.c` and was used for testing communication with the BT module/Arduino while the Erlang app was under development. At this point, it might just be easier to use the Erlang code.
