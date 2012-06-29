/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
#include <sys/wait.h>
#include <stdio.h>

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
