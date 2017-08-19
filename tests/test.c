int main() {
  char array[1000] = {0};
  char *ptr = array;
  ++ptr;
  *ptr = getchar();
  while (*ptr) {
    ++ptr;
    *ptr = getchar();
  }
  --ptr;
  while (*ptr) {
    putchar(*ptr);
    --ptr;
  }
}
