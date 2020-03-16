#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <wchar.h>
#include <unistd.h>

#if UINTPTR_MAX != 0xffffffffffffffff
#error The address space must be 64-bit.
#endif

#if defined(__BIG_ENDIAN__) || defined(__BYTE_ORDER) && __BYTE_ORDER == __BIG_ENDIAN
#error The byte ordering must be little-endian.
#endif

#define OPTION_PARSE_ERROR 1
#define FILE_NOT_FOUND 2
#define NOT_IMPLEMENTED_YET 3
#define UNDEFINED_INSTRUCTION 4

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

void* pc;
void* sp;

static inline void push(uint64_t x) { *((uint64_t*) (sp -= 8)) = x; }
static inline uint64_t pop() { uint64_t result = *((uint64_t*) sp); sp += 8; return result; }

static inline uint8_t next1() { return *((uint8_t*)(pc++)); }
static inline uint16_t next2() { uint16_t result = *((uint16_t*)pc); pc += 2; return result; }
static inline uint32_t next4() { uint32_t result = *((uint32_t*)pc); pc += 4; return result; }
static inline uint64_t next8() { uint64_t result = *((uint64_t*)pc); pc += 8; return result; }

static inline uint64_t signExtend1(uint64_t x) { return ((uint64_t)(int64_t)(int8_t)(uint8_t)x); }
static inline uint64_t signExtend2(uint64_t x) { return ((uint64_t)(int64_t)(int16_t)(uint16_t)x); }
static inline uint64_t signExtend4(uint64_t x) { return ((uint64_t)(int64_t)(int32_t)(uint32_t)x); }

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
  if (fileptr == NULL) {
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

/* Example: clang vm.c && ./a.out 50 ex3_quick_sort.bin empty */
// [-m <bytes>] [-a <file>] [-o <dir>] binary
int main(int argc, char** argv) {
  parseOptions(argc, argv);

  void* memStart = calloc(memorySize, 1);
  void* memStop = memStart + memorySize;

  void* argStart = memStart + readFile(binFile, memStart) + 8;
  long argLength = argFile == NULL ? 0 : readFile(argFile, argStart);
  *((uint64_t*) (argStart - 8)) = argLength;
  void* heapStart = argStart + argLength;

  pc = memStart;
  sp = memStop;
  bool terminated = false;
  uint64_t x, y;
  bool undefIo = false;

  while (!terminated) {
    switch (next1()) {
    case EXIT: terminated = true; break;
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
    case LT: push(pop() > pop() ? (uint64_t)-1 : 0); break;

    case AND: push(pop() & pop()); break;
    case OR: push(pop() | pop()); break;
    case NOT: push(~pop()); break;
    case XOR: push(pop() ^ pop()); break;

    case POW2: x = pop(); push(x <= 63 ? 1UL << x : 0); break;

    // TODO:
    case READ_FRAME: undefIo = true; push(0); push(0); break;
    case READ_PIXEL: undefIo = true; pop(); pop(); push(0); break;
    case NEW_FRAME: undefIo = true; printf("\r\f"); pop(); pop(); pop(); break;
    case SET_PIXEL: undefIo = true; pop(); pop(); pop(); pop(); pop(); break;
    case ADD_SAMPLE: undefIo = true; pop(); pop(); break;

    case PUT_CHAR: printf("%lc", (wchar_t)pop()); break;
    default:
      fprintf(stderr, "Undefined instruction: %d\n", *((uint8_t*)(pc-1)));
      return UNDEFINED_INSTRUCTION;
    }
  }

  // Print stack
  printf("\n");
  while (sp != memStop) {
    x = pop();
    printf("0x..%05llX %7lld\n", x & 0xfffffUL, x);
  }
  return(0);
}
