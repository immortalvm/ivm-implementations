/* Fast C implementation of the iVM virtual machine
 * Compile with: cc -Wall -Ofast vm.c -lpng -ovm
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <unistd.h>
#include <png.h>
#include <dirent.h>

#if UINTPTR_MAX != 0xffffffffffffffff
#error The address space must be 64-bit.
#endif

#if defined(__BIG_ENDIAN__) || defined(__BYTE_ORDER) && __BYTE_ORDER == __BIG_ENDIAN
#error The byte ordering must be little-endian.
#endif

#define MAX_FILENAME 260

// Error codes
#define OPTION_PARSE_ERROR 1
#define NOT_READABLE 2
#define NOT_IMPLEMENTED 3
#define UNDEFINED_INSTRUCTION 4
#define OUT_OF_MEMORY 5
#define STRING_TOO_LONG 6
#define NOT_WRITEABLE 7
#define PNG_TROUBLE 8

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

#define GET_SP_8 64
#define GET_SP_16 65
#define GET_SP_24 66
#define GET_SP_32 67
#define GET_SP_40 68
#define GET_SP_48 69
#define GET_SP_56 70
#define GET_SP_64 71

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
    exit(NOT_READABLE);
  }
  fseek(fileptr, 0, SEEK_END);
  long filelen = ftell(fileptr);
  rewind(fileptr);

  size_t actual = fread(start, 1, filelen, fileptr);
  if (actual < filelen) {
    fprintf(stderr, "Partially read: %s\n", filename);
    exit(NOT_READABLE);
  }
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

void writePng(char* filename, void* start, uint16_t width, uint16_t height) {
  png_structp png;
  png_infop info;
  if (!(png = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL))
  || !(info = png_create_info_struct(png))
  || setjmp(png_jmpbuf(png))) {
    exit(PNG_TROUBLE);
  }
  FILE *fileptr = fopen(filename, "wb");
  if (!fileptr) {
    fprintf(stderr, "Trouble writing: %s\n", filename);
    exit(NOT_WRITEABLE);
  }
  png_init_io(png, fileptr);
  png_set_IHDR(
    png,
    info,
    width, height,
    8, // 8-bit color depth
    PNG_COLOR_TYPE_RGB,
    PNG_INTERLACE_NONE,
    PNG_COMPRESSION_TYPE_DEFAULT,
    PNG_FILTER_TYPE_DEFAULT
  );
  png_write_info(png, info);
  for (int y = 0; y < height; y++) {
    png_write_row(png, start + y * width * 3);
  }
  png_write_end(png, NULL);
  fclose(fileptr);
  png_destroy_write_struct(&png, &info);
}


/* Reusable memory */

typedef struct {
  void* array;
  size_t size;
  size_t used;
} Space;

void spaceInit(Space* s) {
  s->array = NULL;
  s->size = s->used = 0;
}

void spaceReset(Space* s, size_t needed) {
  s->used = needed;
  if (s->size >= needed) return;
  free(s->array);
  s->array = malloc(needed);
  if (!s->array) exit(OUT_OF_MEMORY);
  s->size = needed;
}


/* Growing byte buffer */

typedef struct {
  uint8_t* array;
  size_t size;
  size_t used;
} Bytes;

void bytesInit(Bytes* b, size_t initialSize) {
  b->array = malloc(initialSize);
  if (!b->array) exit(OUT_OF_MEMORY);
  b->size = initialSize;
  b->used = 0;
}

void bytesMakeSpace(Bytes* b, size_t extra) {
  if (b->used + extra > b->size) {
    b->size += extra > b->size ? extra : b->size;
    b->array = realloc(b->array, b->size);
    if (!b->array) exit(OUT_OF_MEMORY);
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


/* Input state */

// 16 MiB
#define INITIAL_IN_IMG_SIZE 0x1000000

struct dirent** inpFiles;
int numInpFiles;
Space currentInImage;
size_t currentInRowbytes = 0;
Space currentInRowpointers;

int acceptPng(const struct dirent* entry) {
  char* ext;
  return (entry->d_type == DT_REG)
    && (ext = strrchr(entry->d_name, '.'))
    && strcmp(ext, ".png") == 0;
}

void ioInitIn() {
  numInpFiles = inpDir ? scandir(inpDir, &inpFiles, acceptPng, alphasort) : 0;
  if (numInpFiles < 0) {
    perror("scandir");
    exit(NOT_READABLE);
  }
  spaceInit(&currentInImage);
  spaceInit(&currentInRowpointers);
}

void ioReadFrame(uint64_t i, uint64_t* width, uint64_t* height) {
  if (i >= numInpFiles) {
    *width = 0;
    *height = 0;
    return;
  }
  static char filename[MAX_FILENAME];
  struct dirent* f = inpFiles[i];
  sprintf(filename, "%s/%s", inpDir, f->d_name);

  FILE *fileptr;
  png_structp png;
  png_infop info;
  if (!(fileptr = fopen(filename, "rb"))
  || !(png = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL))
  || !(info = png_create_info_struct(png))
  || setjmp(png_jmpbuf(png))) {
    perror("png");
    exit(NOT_READABLE);
  }
  png_init_io(png, fileptr);
  png_read_info(png, info);
  *width = png_get_image_width(png, info);
  *height = png_get_image_height(png, info);

  if (png_get_bit_depth(png, info) == 16) {
    png_set_strip_16(png);
  }
  png_byte color_type = png_get_color_type(png, info);
  if (color_type == PNG_COLOR_TYPE_RGB ||
      color_type == PNG_COLOR_TYPE_RGB_ALPHA) {
    png_set_rgb_to_gray(png, 1, -1.0, -1.0); // Default weights
  }
  if (color_type & PNG_COLOR_MASK_ALPHA) {
    png_set_strip_alpha(png);
  }
  png_read_update_info(png, info);
  size_t rowbytes = png_get_rowbytes(png, info);
  size_t needed = rowbytes * *height;

  // We have attempted to optimize for the expected case
  // when all the inp frames have the same format.

  if (needed > currentInImage.size) {
    spaceReset(&currentInImage, needed);
    currentInRowbytes = 0;
  }
  if (*height > currentInRowpointers.size) {
    spaceReset(&currentInRowpointers, sizeof(void*) * *height);
    currentInRowbytes = 0;
  }
  if (rowbytes != currentInRowbytes) {
    void** rp = currentInRowpointers.array;
    for (int y = 0; y < *height; y++) {
      rp[y] = currentInImage.array + rowbytes * y;
    }
    currentInRowbytes = rowbytes;
  }

  png_read_image(png, currentInRowpointers.array);
  fclose(fileptr);
  png_destroy_read_struct(&png, &info, NULL);
}

uint8_t ioReadPixel(uint16_t x, uint16_t y) {
  return *((uint8_t*) currentInImage.array + currentInRowbytes * y + x);
}


/* Output state */

// 16 MiB
#define INITIAL_TEXT_SIZE 0x1000000
#define INITIAL_BYTES_SIZE 0x1000000
#define INITIAL_SAMPLES_SIZE 0x1000000
#define INITIAL_OUT_IMG_SIZE 0x1000000

int outputCounter = 0;
Bytes currentText;
Bytes currentBytes;
Bytes currentSamples;
uint32_t currentSampleRate;
Space currentOutImage;
uint16_t currentOutWidth;
uint16_t currentOutHeight;

void ioInitOut() {
  if (outDir) {
    if (strlen(outDir) + 16 > MAX_FILENAME) {
      exit(STRING_TOO_LONG);
    }
  }
  bytesInit(&currentText, INITIAL_TEXT_SIZE);
  bytesInit(&currentBytes, INITIAL_BYTES_SIZE);
  bytesInit(&currentSamples, INITIAL_SAMPLES_SIZE);
  spaceInit(&currentOutImage);
}

void ioFlush() {
  if (outDir) {
    static char filename[MAX_FILENAME];
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
    if (currentOutImage.used > 0) {
      sprintf(ext, "png");
      writePng(filename, currentOutImage.array, currentOutWidth, currentOutHeight);
    }
  }
  currentText.used = 0;
  currentBytes.used = 0;
  currentSamples.used = 0;
  currentOutImage.used = 0;
  outputCounter++;
}

// TODO: Remove the printf?
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
  currentOutWidth = width;
  currentOutHeight = height;
  spaceReset(&currentOutImage, 3 * width * height);
}

void ioSetPixel(uint16_t x, uint16_t y, uint64_t r, uint64_t g, uint64_t b) {
  uint8_t* p = currentOutImage.array + (y * currentOutWidth + x) * 3;
  p[0] = (uint8_t) r;
  p[1] = (uint8_t) g;
  p[2] = (uint8_t) b;
}


// [-m <bytes>] [-a <file>] [-i <dir>] [-o <dir>] binary
int main(int argc, char** argv) {
  parseOptions(argc, argv);

  void* memStart = calloc(memorySize, 1);
  void* memStop = memStart + memorySize;

  void* argStart = memStart + readFile(binFile, memStart) + 8;
  long argLength = argFile ? readFile(argFile, argStart) : 0;
  *((uint64_t*) (argStart - 8)) = argLength;

  ioInitIn();
  ioInitOut();

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

    case GET_SP_8: push((uint64_t) sp + 8); break;
    case GET_SP_16: push((uint64_t) sp + 16); break;
    case GET_SP_24: push((uint64_t) sp + 24); break;
    case GET_SP_32: push((uint64_t) sp + 32); break;
    case GET_SP_40: push((uint64_t) sp + 40); break;
    case GET_SP_48: push((uint64_t) sp + 48); break;
    case GET_SP_56: push((uint64_t) sp + 56); break;
    case GET_SP_64: push((uint64_t) sp + 64); break;

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

    case READ_FRAME:
      ioReadFrame(pop(), &x, &y);
      push(x);
      push(y);
      break;
    case READ_PIXEL:
      y = pop(); x = pop();
      push(ioReadPixel(x, y));
      break;
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
    printf("0x..%05" PRIx64 " %7" PRIu64 "\n", x & 0xfffffUL, x);
  }
  return(res);
}
