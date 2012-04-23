#define _FILE_OFFSET_BITS 64
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>

void* dumb_mmap(size_t length, int fd, uint64_t offset) {
  mmap(NULL, length, PROT_READ | PROT_WRITE, MAP_SHARED, fd, offset);
}
