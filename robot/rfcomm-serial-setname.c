/* Notes by Henry Walker
   2013-06-19:  compile with the line
      gcc -o rfcomm-serial-open rfcomm-serial-open.c -lbluetooth
*/

#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char **argv)
{
  char message[9];
  int robot = open ("/dev/rfcomm0", O_RDWR);
  int nbytes, i;

  message[0]= 110; // set first 8 bytes of the robot's 16-byte name
  message[1]= 'a';  // not used
  message[2]= 'b';  // not used
  message[3]= 'c';  // not used
  message[4]= 'd';  // not used
  message[5]= 'e';  // not used
  message[6]= 'f';  // not used
  message[7]= 'g';  // not used
  message[8]= 'h';  // not used

  printf ("setting first part of Scribbler's name to abcdefgh\n");
  write (robot, message, 9);
  // read echoed command returned by Scribbler
  i = 9;
  while (i > 0)
    i-=read(robot, message, 1);

  message[0]= 119; // set last 8 bytes of the robot's 16-byte name
  message[1]= 'A';  // not used
  message[2]= 'B';  // not used
  message[3]= 'C';  // not used
  message[4]= 'D';  // not used
  message[5]= 'E';  // not used
  message[6]= 'F';  // not used
  message[7]= 'G';  // not used
  message[8]= 'H';  // not used

  printf ("setting last part of Scribbler's name to ABCDEFGH\n");
  write (robot, message, 9);
  // read echoed command returned by Scribbler
  i = 9;
  while (i > 0)
    i-=read(robot, message, 1);

  close(robot);
  return 0;
}