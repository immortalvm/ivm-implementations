#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <unistd.h>

#if UINTPTR_MAX != 0xffffffffffffffff
#error The address space must be 64-bit.
#endif

#if defined(__BIG_ENDIAN__) || defined(__BYTE_ORDER) && __BYTE_ORDER == __BIG_ENDIAN
#error The byte ordering must be little-endian.
#endif

// Error codes
#define OPTION_PARSE_ERROR 1
#define FILE_NOT_FOUND 2
#define NOT_IMPLEMENTED_YET 3
#define UNDEFINED_INSTRUCTION 4
#define OUT_OF_MEMORY 5
#define STRING_TOO_LONG 6
#define NOT_WRITEABLE 7

// Instructions
#define EXIT 0
#define NOP 1
#define JUMP 2
#define JUMP_ZERO 3
#define SET_SP 4
#define GET_PC 5
#define GET_SP 6
#define PUSH0 7
#define PUSH1 8
#define PUSH2 9
#define PUSH4 10
#define PUSH8 11
#define SIGX1 12
#define SIGX2 13
#define SIGX4 14
#define LOAD1 16
#define LOAD2 17
#define LOAD4 18
#define LOAD8 19
#define STORE1 20
#define STORE2 21
#define STORE4 22
#define STORE8 23
#define ADD 32
#define MULT 33
#define DIV 34
#define REM 35
#define LT 36
#define AND 40
#define OR 41
#define NOT 42
#define XOR 43
#define POW2 44

#define READ_FRAME (uint8_t)-1
#define READ_PIXEL (uint8_t)-2
#define NEW_FRAME (uint8_t)-3
#define SET_PIXEL (uint8_t)-4
#define ADD_SAMPLE (uint8_t)-5
#define PUT_CHAR (uint8_t)-6
#define PUT_BYTE (uint8_t)-7

// Machine state
void* pc;
void* sp;

static inline void push(uint64_t x) { *((uint64_t*) (sp -= 8)) = x; }
static inline uint64_t pop() { uint64_t result = *((uint64_t*) sp); sp += 8; return result; }
static inline uint64_t top() { return *((uint64_t*) sp); }

static inline uint8_t next1() { return *((uint8_t*)(pc++)); }
static inline uint16_t next2() { uint16_t result = *((uint16_t*)pc); pc += 2; return result; }
static inline uint32_t next4() { uint32_t result = *((uint32_t*)pc); pc += 4; return result; }
static inline uint64_t next8() { uint64_t result = *((uint64_t*)pc); pc += 8; return result; }

static inline uint64_t signExtend1(uint64_t x) { return ((uint64_t)(int64_t)(int8_t)(uint8_t)x); }
static inline uint64_t signExtend2(uint64_t x) { return ((uint64_t)(int64_t)(int16_t)(uint16_t)x); }
static inline uint64_t signExtend4(uint64_t x) { return ((uint64_t)(int64_t)(int32_t)(uint32_t)x); }

// Options
uint64_t memorySize = 1 << 24; // 16 MiB by default
char* argFile = NULL;
char* inpDir = NULL;
char* outDir = NULL;
char* binFile = NULL;

// cf. https://pubs.opengroup.org/onlinepubs/7908799/xsh/getopt.html
void parseOptions(int argc, char** argv) {
  int c;
  int errflg = 0;
  extern char* optarg;
  extern int optind, optopt;

  while ((c = getopt(argc, argv, ":m:a:i:o:")) != -1) {
    switch (c) {
    case 'm':
      memorySize = strtoull(optarg, (char**)NULL, 10);
      break;
    case 'a':
      argFile = optarg;
      break;
    case 'i':
      inpDir = optarg;
      break;
    case 'o':
      outDir = optarg;
      break;
    case ':':
      fprintf(stderr, "Option -%c requires an operand\n", optopt);
      errflg++;
      break;
    case '?':
      fprintf(stderr, "Unrecognised option: -%c\n", optopt);
      errflg++;
    }
  }

  if (optind != argc - 1) {
    fprintf(stderr, "A single binary file must be specified.\n");
    errflg++;
  } else {
    binFile = argv[optind++];
  }
  if (errflg) {
    fprintf(stderr, "Usage: %s [-m <size in bytes>] [-a <arg file>] [-i <input dir>] [-o <output dir>] binary\n", argv[0]);
    exit(OPTION_PARSE_ERROR);
  }
}

/* Based on https://stackoverflow.com/a/22059317. */
long readFile(char* filename, void* start) {
  FILE* fileptr = fopen(filename, "rb");
  if (!fileptr) {
    fprintf(stderr, "File not found or not readable: %s\n", filename);
    exit(FILE_NOT_FOUND);
  }
  fseek(fileptr, 0, SEEK_END);
  long filelen = ftell(fileptr);
  rewind(fileptr);

  fread(start, filelen, 1, fileptr);
  fclose(fileptr);
  return filelen;
}

void writeFile(char* filename, void* start, size_t size) {
  FILE* fileptr = fopen(filename, "wb");
  if (!fileptr || fwrite(start, 1, size, fileptr) < size) {
    fprintf(stderr, "Trouble writing: %s\n", filename);
    exit(NOT_WRITEABLE);
  }
  fclose(fileptr);
}

// http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html
typedef struct
{
    uint8_t  chunkId1[4];
    uint32_t chunkSize1;
    uint8_t  format[4];
    uint8_t  chunkId2[4];
    uint32_t chunkSize2;
    uint16_t audioFormat;
    uint16_t numChannels;
    uint32_t sampleRate;
    uint32_t byteRate;
    uint16_t blockAlign;
    uint16_t bitsPerSample;
    uint8_t  chunkId3[4];
    int32_t  chunkSize3;
} WavHeader;

const WavHeader wavBasicHeader = {
  {'R', 'I', 'F', 'F'}, 36U, {'W', 'A', 'V', 'E'},
  {'f', 'm', 't', ' '}, 16U, 1U, 2U, 0U, 0U, 4U, 16U,
  {'d', 'a', 't', 'a'}, 0U
};

void writeWav(char* filename, void* start, size_t size, uint32_t sampleRate) {
  FILE* fileptr = fopen(filename, "wb");
  if (!fileptr) {
    fprintf(stderr, "Trouble writing: %s\n", filename);
    exit(NOT_WRITEABLE);
  }
  WavHeader h = wavBasicHeader;
  h.chunkSize1 += size;
  h.sampleRate = sampleRate;
  h.byteRate = 4U * sampleRate;
  h.chunkSize3 = size;
  fwrite((const void*) &h, sizeof(WavHeader), 1, fileptr);
  if (fwrite(start, 1, size, fileptr) < size) {
    fprintf(stderr, "Trouble writing: %s\n", filename);
    exit(NOT_WRITEABLE);
  }
  fclose(fileptr);
}


/* Growing byte buffer inspired by https://stackoverflow.com/a/3536261 */

typedef struct {
  uint8_t* array;
  size_t size;
  size_t used;
} Bytes;

void bytesInitialize(Bytes* b, size_t initialSize) {
  b->array = (uint8_t*) malloc(initialSize * sizeof(uint8_t));
  if (!b->array) {
    exit(OUT_OF_MEMORY);
  }
  b->size = initialSize;
  b->used = 0;
}

void bytesMakeSpace(Bytes* b, size_t extra) {
  // Precondition: extra <= initial size
  if (b->used + extra > b->size) {
    b->size *= 2;
    b->array = (uint8_t*) realloc(b->array, b->size * sizeof(uint8_t));
    if (!b->array) {
      exit(OUT_OF_MEMORY);
    }
  }
}

void bytesPutByte(Bytes* b, uint8_t x) {
    bytesMakeSpace(b, 1);
    b->array[b->used++] = x;
}

// UTF-32 to UTF-8
void bytesPutChar(Bytes* b, uint32_t c) {
  if (c < 0x80) {
    bytesMakeSpace(b, 1);
    b->array[b->used++] = (uint8_t) c;
  } else if (c < 0x800) {
    bytesMakeSpace(b, 2);
    b->array[b->used++] = (uint8_t) (0xc0 | c >> 6);
    b->array[b->used++] = (uint8_t) (0x80 | (0x3f & c));
  } else if (c < 0x10000) {
    bytesMakeSpace(b, 3);
    b->array[b->used++] = (uint8_t) (0xe0 | c >> 12);
    b->array[b->used++] = (uint8_t) (0x80 | (0x3f & c >> 6));
    b->array[b->used++] = (uint8_t) (0x80 | (0x3f & c));
  } else {
    bytesMakeSpace(b, 4);
    b->array[b->used++] = (uint8_t) (0xf0 | (0x07 & c >> 18)); // &: in case is > 21 bits
    b->array[b->used++] = (uint8_t) (0x80 | (0x3f & c >> 12));
    b->array[b->used++] = (uint8_t) (0x80 | (0x3f & c >> 6));
    b->array[b->used++] = (uint8_t) (0x80 | (0x3f & c));
  }
}

void bytesPutSample(Bytes* b, uint16_t left, uint16_t right) {
  bytesMakeSpace(b, 4);
  uint16_t* pos = (uint16_t*) (b->array + b->used);
  pos[0] = left;
  pos[1] = right;
  b->used += 4;
}


/* Output state */

// 16 MiB
#define INITIAL_TEXT_SIZE 0x1000000
#define INITIAL_BYTES_SIZE 0x1000000
#define INITIAL_SAMPLES_SIZE 0x1000000
#define MAX_FILENAME 128

int outputCounter = 0;
Bytes currentText;
Bytes currentBytes;
Bytes currentSamples;
uint32_t currentSampleRate;

void ioInit() {
  if (outDir) {
    if (strlen(outDir) + 16 > MAX_FILENAME) {
      exit(STRING_TOO_LONG);
    }
  }
  bytesInitialize(&currentText, INITIAL_TEXT_SIZE);
  bytesInitialize(&currentBytes, INITIAL_BYTES_SIZE);
  bytesInitialize(&currentSamples, INITIAL_SAMPLES_SIZE);
}

void ioFlush() {
  if (outDir) {
    char filename[MAX_FILENAME];
    char* ext = filename + sprintf(filename, "%s/%08d.", outDir, outputCounter);
    if (currentText.used > 0) {
      sprintf(ext, "text");
      writeFile(filename, currentText.array, currentText.used);
    }
    if (currentBytes.used > 0) {
      sprintf(ext, "bytes");
      writeFile(filename, currentBytes.array, currentBytes.used);
    }
    if (currentSamples.used > 0) {
      sprintf(ext, "wav");
      writeWav(filename, currentSamples.array, currentSamples.used, currentSampleRate);
    }
  }
  currentText.used = 0;
  currentBytes.used = 0;
  currentSamples.used = 0;
  outputCounter++;
}

void ioPutChar(uint32_t c) {
  int start = currentText.used;
  bytesPutChar(&currentText, c);
  int len = currentText.used - start;
  printf("%.*s", len, currentText.array + start);
}

void ioPutByte(uint8_t x) {
  bytesPutByte(&currentBytes, x);
}

void ioAddSample(uint16_t left, uint16_t right) {
  bytesPutSample(&currentSamples, left, right);
}

void ioNewFrame(uint16_t width, uint16_t height, uint32_t sampleRate) {
  currentSampleRate = sampleRate;
  // TODO
}

void ioSetPixel(uint16_t x, uint16_t y, uint64_t r, uint64_t g, uint64_t b) {
  // TODO
}


// [-m <bytes>] [-a <file>] [-i <dir>] [-o <dir>] binary
int main(int argc, char** argv) {
  parseOptions(argc, argv);

  void* memStart = calloc(memorySize, 1);
  void* memStop = memStart + memorySize;

  void* argStart = memStart + readFile(binFile, memStart) + 8;
  long argLength = argFile ? readFile(argFile, argStart) : 0;
  *((uint64_t*) (argStart - 8)) = argLength;

  ioInit();

  pc = memStart;
  sp = memStop;
  uint64_t x, y, r, g, b;

  while (true) {
    switch (next1()) {
    case EXIT: goto terminated;
    case NOP: break;
    case JUMP: pc = (void*) pop(); break;

    case JUMP_ZERO:
      x = signExtend1(next1());
      if (pop() == 0) {
        pc += x;
      }
      break;

    case SET_SP: sp = (void*) pop(); break;
    case GET_PC: push((uint64_t) pc); break;
    case GET_SP: push((uint64_t) sp); break;

    case PUSH0: push(0); break;
    case PUSH1: push(next1()); break;
    case PUSH2: push(next2()); break;
    case PUSH4: push(next4()); break;
    case PUSH8: push(next8()); break;

    case SIGX1: push(signExtend1(pop())); break;
    case SIGX2: push(signExtend2(pop())); break;
    case SIGX4: push(signExtend4(pop())); break;

    case LOAD1: push(*((uint8_t*)pop())); break;
    case LOAD2: push(*((uint16_t*)pop())); break;
    case LOAD4: push(*((uint32_t*)pop())); break;
    case LOAD8: push(*((uint64_t*)pop())); break;

    case STORE1: x = pop(); *((uint8_t*)x) = (uint8_t) pop(); break;
    case STORE2: x = pop(); *((uint16_t*)x) = (uint16_t) pop(); break;
    case STORE4: x = pop(); *((uint32_t*)x) = (uint32_t) pop(); break;
    case STORE8: x = pop(); *((uint64_t*)x) = pop(); break;

    case ADD: push(pop() + pop()); break;
    case MULT: push(pop() * pop()); break;
    case DIV:
      x = pop();
      y = pop();
      push(x == 0 ? 0 : y / x);
      break;
    case REM:
      x = pop();
      y = pop();
      push(x == 0 ? 0 : y % x);
      break;
    case LT: x = pop(); y = pop(); push(y < x ? (uint64_t)-1 : 0); break;

    case AND: push(pop() & pop()); break;
    case OR: push(pop() | pop()); break;
    case NOT: push(~pop()); break;
    case XOR: push(pop() ^ pop()); break;

    case POW2: x = pop(); push(x <= 63 ? 1UL << x : 0); break;

    // TODO:
    case READ_FRAME: push(0); push(0); break;
    case READ_PIXEL: pop(); pop(); push(0); break;
    case NEW_FRAME:
      r = pop(); y = pop(); x = pop();
      ioFlush(); ioNewFrame(x, y, r);
      printf("\r\f");
      break;
    case SET_PIXEL:
      b = pop(); g = pop(); r = pop();
      y = pop(); x = pop();
      ioSetPixel(x, y, r, g, b);
      break;
    case ADD_SAMPLE: x = pop(); y = pop(); ioAddSample(y, x); break;

    // TODO: Remove the printf?
    case PUT_CHAR: ioPutChar(pop()); break;
    case PUT_BYTE: ioPutByte(pop()); break;
    default:
      fprintf(stderr, "Undefined instruction: %d\n", *((uint8_t*)(pc-1)));
      return UNDEFINED_INSTRUCTION;
    }
  }

terminated:
  ioFlush();
  printf("\n");
  int res = sp != memStop ? (int)top() : -1;
  while (sp != memStop) {
    // Print stack destructively
    x = pop();
    printf("0x..%05llX %7lld\n", x & 0xfffffUL, x);
  }
  return(res);
}
