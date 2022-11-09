void setup()
{
  pinMode(LED_BUILTIN, OUTPUT);

  // 1. Set baud rate to what you think it currently is
  Serial.begin(115200);

  // 2. Set baud rate to what you want it to be
  Serial.print("AT+BAUD8");
  delay(2000);

  // 3. Set the device's name
  // About the only way you can get some feedback as to whether your commands worked
  Serial.print("AT+NAMEHC-06");
  delay(2000);
}

void loop()
{
  // 4. Pair BT device and check if you're now getting data in the Serial Monitor
  Serial.println("data");
  delay(500);
}
