/* Notes by Henry Walker
   2022-10-18:  compile with the line
      gcc -o bt-test bt-test.c
*/

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>

#define DEVICE "/dev/cu.HC-06"

#define BUF_SIZE 52

#define ACTION_RUN 2

#define DEVICE_ROBOTCAR 54

#define DIRECTION_FORWARD 1
#define DIRECTION_BACKWARD 2
#define DIRECTION_LEFT 3
#define DIRECTION_RIGHT 4
#define DIRECTION_STOP 5

void setHead(unsigned char message[52])
{
  message[0] = 0xff;
  message[1] = 0x55;
}

int set_interface_attribs(int fd, int speed)
{
    struct termios tty;

    if (tcgetattr(fd, &tty) < 0) {
        printf("Error from tcgetattr: %s\n", strerror(errno));
        return -1;
    }

    cfsetospeed(&tty, (speed_t)speed);
    cfsetispeed(&tty, (speed_t)speed);

    // tty.c_cflag |= (CLOCAL | CREAD);    /* ignore modem controls */
    // tty.c_cflag &= ~CSIZE;
    // tty.c_cflag |= CS8;         /* 8-bit characters */
    // tty.c_cflag &= ~PARENB;     /* no parity bit */
    // tty.c_cflag &= ~CSTOPB;     /* only need 1 stop bit */
    // tty.c_cflag &= ~CRTSCTS;    /* no hardware flowcontrol */

    // /* setup for non-canonical mode */
    // tty.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
    // tty.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
    // tty.c_oflag &= ~OPOST;

    // /* fetch bytes as they become available */
    // tty.c_cc[VMIN] = 1;
    // tty.c_cc[VTIME] = 1;

    if (tcsetattr(fd, TCSANOW, &tty) != 0) {
        printf("Error from tcsetattr: %s\n", strerror(errno));
        return -1;
    }
    return 0;
}

void readVersionInfo(int fd)
{
  int versionStrLen = 0;
  char buf[BUF_SIZE];
  char versionStr[BUF_SIZE];

  versionStr[versionStrLen] = 0;

  while (versionStrLen == 0 || versionStr[versionStrLen - 1] != '\n') {
    int bytesRead = read(fd, buf, BUF_SIZE);

    for (int i = 0; i < bytesRead; i++) {
      versionStr[versionStrLen++] = buf[i];
    }
  }

  printf("%.*s", versionStrLen, versionStr);
}

int main(int argc, char **argv)
{
  unsigned char message[BUF_SIZE];
  int robot;
  int bytesRead;

  setbuf(stdout, NULL);

  printf("open %s\n", DEVICE);
  robot = open(DEVICE, O_RDWR | O_NOCTTY | O_SYNC);
  set_interface_attribs(robot, B115200);

  // printf("read version info\n");
  // readVersionInfo(robot);

  // Single-byte commands

  // Run forward
  // message[0] = 0x01;
  // write(robot, message, 1);

  // Request version info
  // message[0] = 0x11;
  // write(robot, message, 1);

  // Multi-byte commands
  // ff 55 len action device dataA dataB dataC dataD
  // 0  1  2   3      4      5     6     7     8

  // Run forward
  //                         lMod  lSpd  rMod  rSpd
  // ff 55 06  02     36     01    200   01    20
  setHead(message);
  message[2] = 6;
  message[3] = ACTION_RUN;
  message[4] = DEVICE_ROBOTCAR;
  message[5] = DIRECTION_FORWARD;
  message[6] = 200;
  message[7] = DIRECTION_FORWARD;
  message[8] = 200;
  write(robot, message, 9);

  sleep(5);

  // Run left
  //                         lMod  lSpd  rMod  rSpd
  // ff 55 06  02     36     01    200   01    20
  setHead(message);
  message[2] = 6;
  message[3] = ACTION_RUN;
  message[4] = DEVICE_ROBOTCAR;
  message[5] = DIRECTION_FORWARD;
  message[6] = 200;
  message[7] = DIRECTION_STOP;
  message[8] = 0;
  write(robot, message, 9);

  // // Stop
  // setHead(message);
  // message[2] = 6;
  // message[4] = ACTION_RUN;
  // message[5] = DEVICE_ROBOTCAR;
  // message[6] = DIRECTION_STOP;
  // message[8] = 0;
  // write(robot, message, 9);

  // readAndDisplay(robot);

  sleep(2);
  tcflush(robot, TCIOFLUSH);
  close(robot);

  return 0;
}
