#include <stdio.h>

int main() {
  char array[100000] = {0};
  char *ptr = array;
  ++ptr;
  *ptr += 8;
  while (*ptr) {
    --*ptr;
    --ptr;
    *ptr += 9;
    ++ptr;
  }
  --ptr;
  putchar(*ptr);
  ptr += 2;
  ++*ptr;
  ++ptr;
  --*ptr;
  while (*ptr) {
    ++*ptr;
  }
  *ptr += 2;
  ++ptr;
  *ptr += 2;
  ++ptr;
  *ptr += 3;
  while (*ptr) {
    ++ptr;
    while (*ptr) {
      --*ptr;
      ++ptr;
      *ptr += 3;
      ptr -= 2;
      *ptr += 3;
      ++ptr;
    }
    ptr -= 2;
  }
  ++ptr;
  *ptr -= 5;
  putchar(*ptr);
  ++ptr;
  --*ptr;
  ++ptr;
  *ptr += 3;
  putchar(*ptr);
  putchar(*ptr);
  *ptr += 3;
  putchar(*ptr);
  ++ptr;
  --*ptr;
  putchar(*ptr);
  ptr -= 2;
  ++*ptr;
  while (*ptr) {
    ++ptr;
    while (*ptr) {
      ++*ptr;
      ++ptr;
      ++*ptr;
    }
    ptr += 2;
  }
  --ptr;
  *ptr -= 14;
  putchar(*ptr);
  ptr += 2;
  putchar(*ptr);
  *ptr += 3;
  putchar(*ptr);
  *ptr -= 6;
  putchar(*ptr);
  *ptr -= 8;
  putchar(*ptr);
  ++ptr;
  ++*ptr;
  putchar(*ptr);
  ++ptr;
  ++*ptr;
  putchar(*ptr);
}
