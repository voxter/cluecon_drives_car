// UCTRONICS Smart Robot Car demo (C)2018 uctronics
// Web: http://www.uctronics.com

// ClueCon Drives Car
// Based on UCTRONICS code: https://github.com/UCTRONICS/Smart-Robot-Car-Arduino

#include <UCMotor.h>
#include "UCNEC.h"

union {
  byte byteVal[4];
  float floatVal;
  long longVal;
} val;

union {
  byte byteVal[8];
  double doubleVal;
} valDouble;

union {
  byte byteVal[2];
  short shortVal;
} valShort;

String mVersion = "0d.01.105";
boolean isAvailable = false;
boolean isBluetooth = false;
boolean isDetecte = false;
int len = 52;
char buffer[52];
char bufferBt[52];
byte index = 0;
byte dataLen;
boolean isStart = false;
char serialRead;

// define the device ID
#define ROBOTCAR 54

#define RUN 2
#define RESET 4
#define START 5
unsigned char prevc = 0;
double lastTime = 0.0;
double currentTime = 0.0;

bool isSmartMode  = true;
bool isIrMode  = true;
unsigned int S;
unsigned int Sleft;
unsigned int Sright;

uint32_t irValue = 0;

UC_DCMotor leftMotor1(3, MOTOR34_64KHZ);
UC_DCMotor rightMotor1(4, MOTOR34_64KHZ);
UC_DCMotor leftMotor2(1, MOTOR34_64KHZ);
UC_DCMotor rightMotor2(2, MOTOR34_64KHZ);

UCNEC myIR(2);

void setup() {
  myIR.begin();
  Serial.begin(115200);
//  Serial.print("Version: ");
//  Serial.println(mVersion);
}

void loop() {
  currentTime = millis() / 1000.0 - lastTime;
  readSerial();
  if (isAvailable) {
    unsigned char c = serialRead & 0xff;
    if (c == 0x55 && isStart == false) {
      if (prevc == 0xff) {
        index = 1;
        isStart = true; isSmartMode = false;
      }
    } else {
      prevc = c;
      if (isStart) {
        if (index == 2) {
          dataLen = c;
        } else if (index > 2) {
          dataLen--;
        }
        writeBuffer(index, c);
      }
    }
    index++;
    if (index > 51) {
      index = 0;
      isStart = false;
    }
    if (isStart && dataLen == 0 && index > 3) {
      isStart = false;
      parseData();
      Serial.println("command received");
      index = 0;
    } else if (!isStart) {
      if (serialRead >= 1 && serialRead <= 5) { //0x01->forward  0x02->backward  0x03->left  0x04-> right  0x05->stop
        if(serialRead == 1)  {isDetecte = true;}
        else  {isDetecte = false;}
        leftMotor1.run(serialRead); rightMotor1.run(serialRead);
        leftMotor2.run(serialRead); rightMotor2.run(serialRead);
        leftMotor1.setSpeed(200); rightMotor1.setSpeed(200);
        leftMotor2.setSpeed(200); rightMotor2.setSpeed(200);
        isSmartMode = false;
        myIR.begin();
      }
       if (serialRead == 0x11 || serialRead == 'v') {
        Serial.write(0xAA);
        Serial.print("{\"version\":\"K0070\"}");
        Serial.write(0xAB);
      }
      if (serialRead == 0x10) { //reset
        isSmartMode = false;
        myIR.begin();
        stop();
      }
    }
  }
}

void stop() {
  leftMotor1.run(STOP); rightMotor1.run(STOP);
  leftMotor2.run(STOP); rightMotor2.run(STOP);
  leftMotor1.setSpeed(0); rightMotor1.setSpeed(0);
  leftMotor2.setSpeed(0); rightMotor2.setSpeed(0);
}

unsigned char readBuffer(int index) {
  return isBluetooth ? bufferBt[index] : buffer[index];
}
void writeBuffer(int index, unsigned char c) {
  if (isBluetooth) {
    bufferBt[index] = c;
  } else {
    buffer[index] = c;
  }
}
void writeHead() {
  writeSerial(0xff);
  writeSerial(0x55);
}
void writeEnd() {
  Serial.println();
#if defined(__AVR_ATmega32U4__)
  Serial1.println();
#endif
}
void writeSerial(unsigned char c) {
  Serial.write(c);
#if defined(__AVR_ATmega32U4__)
  Serial1.write(c);
#endif
}
void readSerial() {
  isAvailable = false;
  if (Serial.available() > 0) {
    isAvailable = true;
    isBluetooth = false;
    serialRead = Serial.read();
  }
  while (myIR.available())
  {
    irValue =  myIR.read();
  }
  if (irValue == 0xFF46B9 || irValue == 0xFF18E7)  //forward
  {

    irValue = 0; isSmartMode = false;

    irValue = 0;isDetecte = true;

    leftMotor1.run(1); rightMotor1.run(1);//1-> forward
    leftMotor2.run(1); rightMotor2.run(1);
    leftMotor1.setSpeed(200); rightMotor1.setSpeed(200);
    leftMotor2.setSpeed(200); rightMotor2.setSpeed(200);
  } else if (irValue == 0xFF15EA || irValue == 0xFF52AD) { //backward

    irValue = 0; isSmartMode = false;

    irValue = 0;isDetecte = false;

    leftMotor1.run(2); rightMotor1.run(2);//2-> backward
    leftMotor2.run(2); rightMotor2.run(2);
    leftMotor1.setSpeed(200); rightMotor1.setSpeed(200);
    leftMotor2.setSpeed(200); rightMotor2.setSpeed(200);
  } else if (irValue == 0xFF44BB || irValue == 0xFF08F7) { // left

    irValue = 0; isSmartMode = false;

    irValue = 0;isDetecte = false;

    leftMotor1.run(3); rightMotor1.run(3);//3-> left
    leftMotor2.run(3); rightMotor2.run(3);
    leftMotor1.setSpeed(200); rightMotor1.setSpeed(200);
    leftMotor2.setSpeed(200); rightMotor2.setSpeed(200);
  } else if (irValue == 0xFF43BC || irValue == 0xFF5AA5) { //right

    irValue = 0; isSmartMode = false;

    irValue = 0;isDetecte = false;

    leftMotor1.run(4); rightMotor1.run(4);//4-> right
    leftMotor2.run(4); rightMotor2.run(4);
    leftMotor1.setSpeed(200); rightMotor1.setSpeed(200);
    leftMotor2.setSpeed(200); rightMotor2.setSpeed(200);
  } else if (irValue == 0xFF40BF|| irValue == 0xFF1CE3) {  //stop

    irValue = 0; isSmartMode = false;

    irValue = 0;isDetecte = false;

    leftMotor1.run(5); rightMotor1.run(5);//5-> stop
    leftMotor2.run(5); rightMotor2.run(5);
    leftMotor1.setSpeed(200); rightMotor1.setSpeed(200);
    leftMotor2.setSpeed(200); rightMotor2.setSpeed(200);
  }
}
/*
  ff 55 len action device dataA dataB dataC dataD
  0  1  2   3      4      5     6     7     8
*/
void parseData() {
  isStart = false;
  int action = readBuffer(3);
  int device = readBuffer(4);
  switch (action) {
    case RUN: {
        runModule(device);
        callOK();
      }
      break;
    case RESET: {
        //reset
        stop();
        isSmartMode = false;
        myIR.begin();
        callOK();
      }
      break;
    case START: {
        //start
        callOK();
      }
      break;
  }
}
void callOK() {
  writeSerial(0xff);
  writeSerial(0x55);
  writeEnd();
}
void sendByte(char c) {
  writeSerial(1);
  writeSerial(c);
}
void sendString(String s) {
  int l = s.length();
  writeSerial(4);
  writeSerial(l);
  for (int i = 0; i < l; i++) {
    writeSerial(s.charAt(i));
  }
}
void sendFloat(float value) {
  writeSerial(0x2);
  val.floatVal = value;
  writeSerial(val.byteVal[0]);
  writeSerial(val.byteVal[1]);
  writeSerial(val.byteVal[2]);
  writeSerial(val.byteVal[3]);
}
void sendShort(double value) {
  writeSerial(3);
  valShort.shortVal = value;
  writeSerial(valShort.byteVal[0]);
  writeSerial(valShort.byteVal[1]);
}
void sendDouble(double value) {
  writeSerial(2);
  valDouble.doubleVal = value;
  writeSerial(valDouble.byteVal[0]);
  writeSerial(valDouble.byteVal[1]);
  writeSerial(valDouble.byteVal[2]);
  writeSerial(valDouble.byteVal[3]);
}
short readShort(int idx) {
  valShort.byteVal[0] = readBuffer(idx);
  valShort.byteVal[1] = readBuffer(idx + 1);
  return valShort.shortVal;
}
float readFloat(int idx) {
  val.byteVal[0] = readBuffer(idx);
  val.byteVal[1] = readBuffer(idx + 1);
  val.byteVal[2] = readBuffer(idx + 2);
  val.byteVal[3] = readBuffer(idx + 3);
  return val.floatVal;
}
long readLong(int idx) {
  val.byteVal[0] = readBuffer(idx);
  val.byteVal[1] = readBuffer(idx + 1);
  val.byteVal[2] = readBuffer(idx + 2);
  val.byteVal[3] = readBuffer(idx + 3);
  return val.longVal;
}
void runModule(int device) {
  switch (device) {
    case ROBOTCAR: {
        int leftMode = readBuffer(5);
        int leftSpeed = readBuffer(6);
        int rightMode = readBuffer(7);
        int rightSpeed = readBuffer(8);

        leftMotor1.run(leftMode);
        leftMotor2.run(leftMode);
        leftMotor1.setSpeed(leftSpeed);
        leftMotor2.setSpeed(leftSpeed);

        rightMotor1.run(rightMode);
        rightMotor2.run(rightMode);
        rightMotor1.setSpeed(rightSpeed);
        rightMotor2.setSpeed(rightSpeed);
      }
      break;
  }
}
