/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

#include <sys/wait.h>
#include <stdio.h>
#include <unistd.h>

int main (int argc, char** argv) {
  struct rusage rus;
  int p,return_code;
  if (argc < 3) {
    printf("Usage: %s <prefix> <program> [arguments]\nRuns <program> with [arguments] and outputs the results with <prefix>\n",
           argv[0]);
    return 1;
  }

  p = fork();
  if (!p) {
    execvp(argv[2], argv+2);
  } else {
    wait4(p,&return_code,0,&rus);
    printf("%s %ld\n", argv[1], rus.ru_maxrss);
    /* printf("maximum resident set size:\t%ld\n", rus.ru_maxrss); */
    /* printf("integral shared memory size:\t%ld\n", rus.ru_ixrss); */
    /* printf("integral unshared memory size:\t%ld\n", rus.ru_idrss); */
    /* printf("integral unshared stack size:\t%ld\n", rus.ru_isrss); */
    /* printf("page faults:\t\t\t%ld\n", rus.ru_majflt); */
    /* printf("blocking inputs:\t\t%ld\n", rus.ru_inblock); */
    /* printf("blocking outputs:\t\t%ld\n", rus.ru_oublock); */
  }
  return WEXITSTATUS(return_code);
}
