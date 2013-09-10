#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <arpa/inet.h> /* htonl, ntohl */

#include <gelf.h>
#include <libelf.h>


/* 0x12345678\n\0 */
#define MAX_LINE 16

/* fail on error if != 0 */
static int fail = 1;


static struct {
  uint32_t entry, offset, vaddr, filesz;
} elfinfo;



inline
static extract(uint32_t word, unsigned offset, unsigned width)
{
  return (word >> offset) & ((1 << width) - 1);
}

inline
static deposit(uint32_t word, unsigned offset, unsigned width, uint32_t value)
{
  return word | ((value & ((1 << width) - 1)) << offset);
}

/**
 * Reporting function.
 * Write messages to stderr, and optionally exit program with failure.
 * @param fail  if not 0, exit the program with failure
 * @param fmt   format string
 * @param ...   format arguments
 */
void report(int fail, const char *fmt, ...)
{
  va_list arg;
  va_start(arg, fmt);
  (void) vfprintf(stderr, fmt, arg);
  va_end(arg);
  if (fail) exit(EXIT_FAILURE);
}

/**
 * Get an address as long from string
 * @return -1 on failure, else the address
 */
static long get_address(char *buf)
{
  long val;
  char *endptr, *nptr;

  /* remove newline */
  if( (nptr = strchr(buf, '\n')) != NULL) {
    *nptr = '\0';
  }

  errno = 0;
  val = strtol(buf, &endptr, 0);

  /* Check for various possible errors */

  if ((errno == ERANGE && (val == LONG_MAX || val == LONG_MIN))
      || (errno != 0 && val == 0)) {
    report(fail, "Error: strtol - %s\n", strerror(errno));
    return -1;
  }

  if (endptr == buf) {
    report(fail, "Error: no digits were found\n");
    return -1;
  }

  /* no appended characters allowed */
  if (endptr != nptr) {
    report(fail, "Error: trailing characters\n");
    return -1;
  }

  /* check for negative address */
  if (val < 0) {
    report(fail, "Error: negative address\n");
    return -1;
  }

  return val;
}

/**
 * Read information about ELF binary to elfinfo
 * @param fd  file descriptor of the open binary
 */
static void get_elfinfo(int fd)
{
  /* check libelf version */
  elf_version(EV_CURRENT);

  Elf *elf = elf_begin(fd, ELF_C_READ, NULL);
  assert(elf);

  /* check file kind */
  Elf_Kind ek = elf_kind(elf);
  assert(ek == ELF_K_ELF);

  /* check class */
  int ec = gelf_getclass(elf);
  assert(ec == ELFCLASS32);

  /* get elf header */
  GElf_Ehdr hdr;
  GElf_Ehdr *tmphdr = gelf_getehdr(elf, &hdr);
  /* get entry point */
  assert(tmphdr);

  /* remember entry */
  elfinfo.entry = hdr.e_entry;

  /* get program headers */
  size_t n, i;
  int ntmp = elf_getphdrnum (elf, &n);
  assert(ntmp == 0);

  for(i = 0; i < n; i++)
  {
    GElf_Phdr phdr;
    GElf_Phdr *phdrtmp = gelf_getphdr(elf, i, &phdr);
    assert(phdrtmp);

    if (phdr.p_type == PT_LOAD && phdr.p_flags == PF_R + PF_X)
    {
      assert(phdr.p_vaddr == phdr.p_paddr);
      assert(phdr.p_filesz <= phdr.p_memsz);


      elfinfo.offset = phdr.p_offset;
      elfinfo.vaddr  = phdr.p_vaddr;
      elfinfo.filesz = phdr.p_filesz;

      /* assume single text section */
      break;
    }
  }

  elf_end(elf);
}




/**
 * Check if a given program address is valid: in range and aligned
 * @param addr  program address
 * @return  0 on success, -1 else
 */
static int check_address(long addr)
{
  /* check address */
  if (addr < elfinfo.entry || addr > (elfinfo.vaddr + elfinfo.filesz - 4)) {
    report(fail, "Error: address %#lx out of range\n", addr);
    return -1;
  }

  if (addr & 0x3 != 0) {
    report(fail, "Error: address %#lx unaligned (required: 4)\n", addr);
    return -1;
  }
  return 0;
}


/**
 * Rewrite the load instruction given in word
 * @param word  pointer to 32bit word containing the instruction encoding
 * @return  0 on success, -1 else
 */
int rewrite_load(uint32_t *word)
{
  uint32_t opc, ldtype;

  /* load opcode at [26:22] must be 01010 */
  opc = extract(*word, 22, 5);
  if (opc != 0xa) {
    report(fail, "Error: instruction is not a load instruction\n");
    return -1;
  }

  /* type is at bits [11:7] */
  ldtype = extract(*word, 7, 5);

  report(0, "Info: opc=%#x ldtype=%#x\n", opc, ldtype);

  if (ldtype < 0x14) {
    /* normal loads: type < 10100 (hex 0x14)
     *   mem type is bits [8:7]; ([11:9] is w/h/b/hu/bu)
     *   lXc = 10, lXm = 11
     */
    switch (ldtype & 0x3) {
      case 0x2: /* it is a load from cache */
        /* rewrite to load from memory (bypass cache), type = xxx11 */
        *word = deposit(*word, 7, 2, 0x3);
        break;
      case 0x3: /* already bypass, continue */
        report(0, "Warning: instruction is already a bypass load\n");
        return -1;
      default:
        report(fail, "Error: instruction is not a load from cache\n");
        return -1;
    }


  } else if ((ldtype >= 0x14) && (ldtype < 0x1e)) {
    /* decoupled loads: 10100 <= type < 11110
     *   mem type is bit [7:7] ([11:8] is w/h/b/hu/bu)
     */
    switch (ldtype & 0x1) {
      case 0x0: /* it is a load from cache */
        /* rewrite to load from memory (bypass cache), type = xxxx1 */
        *word = deposit(*word, 7, 1, 0x1);
        break;
      case 0x1: /* already bypass, continue */
        return -1;
      default:
        assert(0 && "unreachable");
    }

  } else {
    report(fail, "Error: unused load type");
    return -1;
  }
  return 0;
}



/**
 * Patch the instruction at a program address
 * @param fd    file descriptor of the binary
 * @param addr  program address
 */
void patch_instruction(int fd, long addr)
{

  if (check_address(addr) == -1) {
    return;
  }

  /* the position in the elf file where the instruction at addr is located */
  unsigned elfpos = elfinfo.offset + addr - elfinfo.vaddr;

  uint32_t word, word_h;

  /* read instruction from address */

  /* read word from the file */
  lseek(fd, elfpos, SEEK_SET);
  read(fd, &word, sizeof(word));

  /* host byte order */
  word_h = ntohl(word);

  report(0, "--\nInfo: Read %0#10x at address %#lx\n", word_h, addr);


  /* rewrite instruction */
  if (rewrite_load(&word_h) == -1) {
    return;
  }


  /* write rewritten instruction back to address */

  /* back to network byte order (= big endian, as on patmos) */
  word = htonl(word_h);

  /* reposition and write to output file */
  lseek(fd, elfpos, SEEK_SET);
  write(fd, &word, sizeof(word));

  report(0, "Info: Rewritten instruction to %0#10x\n", word_h);
}




/**
 * Main program entry point.
 */
int main(int argc, char* argv[])
{

  if (argc != 2) {
    report(1, "Usage: %s file\n", argv[0]);
  }

  int fd = open(argv[1], O_RDWR, 0);
  if (fd == -1) {
    report(1, "Error: cannot open file: %s\n", strerror(errno));
  }


  get_elfinfo(fd);

  report(0, "Info: ELF entry=%x vaddr=%x offset=%x filesz=%x\n",
      elfinfo.entry, elfinfo.vaddr, elfinfo.offset, elfinfo.filesz);

  /*
   * Get addresses from stdin and (try to) patch them
   */
  char input_line[MAX_LINE];
  while(fgets(input_line, MAX_LINE, stdin) != NULL) {
    long addr = get_address(input_line);
    if (addr >= 0) {
      patch_instruction(fd, addr);
    }
  }


  if (close(fd) == -1) {
    report(1, "Error: cannot close file: %s\n", strerror(errno));
  }

  return 0;
}
