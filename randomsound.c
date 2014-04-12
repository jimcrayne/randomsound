#include <sys/select.h>
#include <sys/times.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include <linux/types.h>
#include <linux/random.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "bitbuffer.h"
#include "debias.h"
#include "micfill.h"

static char *version_number = "0.3";
static const char *pidfile = NULL;
static const char *testdumpfilename = NULL;

int testdumpsize = 0;
int daemonise = 0;
int verbose = 0;
int minwatermark = 256;
int maxwatermark = 4096-256;
int depositsize = 64;
int buffersize = 64 * 16;
volatile int time_to_quit = 0;

BitBuffer incoming_bits;
BitBuffer buffered_bits;

volatile int randomfd;

struct injector {
  int ent_count;
  int size;
  union {
    int ints[128];
    BitField bitfield[512];
  } value;
} random_injector;

int
bits_in_pool(void)
{
  int ret;
  if (NULL != testdumpfilename)
    ret = testdumpsize;
  else
    ioctl(randomfd, RNDGETENTCNT, &ret);
  return ret;
}

void
do_mixin_bits(void)
{
  int i;
  int bits_before = bits_in_pool();
  if (verbose > 3)
    printf("Injecting %d bits of entropy into the kernel\n", depositsize * 8);
  random_injector.ent_count = depositsize * 8;
  random_injector.size = depositsize;
  for (i = 0; i < depositsize; ++i) {
    bitbuffer_extract_bits(buffered_bits, random_injector.value.bitfield + i, 8);
  }
  if (NULL != testdumpfilename) {
    int fd = open(testdumpfilename,O_CREAT | O_WRONLY | O_APPEND, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH);
    if (fd > 0 ) {
      struct pollfd fds[1] = { 0 } ;
      fds[0].fd = fd;
      fds[0].events = POLLOUT;
      int write_ret = -2;
      if (poll(fds,1, 1000))
        write_ret = write(fds[0].fd, (void *)(random_injector.value.bitfield), random_injector.size );
      if (write_ret < 0){
        fprintf(stderr, "ERROR: (errno=%d) %s\n", errno, strerror(errno));
      }
      int fsync_ret = fsync(fd);
      int close_ret = close(fd);
      if ((write_ret <0) || (close_ret <0) || (fsync_ret <0)) {
        fprintf(stderr, "ERROR: (write_ret=%d)(fsync_ret=%d)(close_ret = %d)(errno=%d) %s\n", 
            write_ret, fsync_ret, close_ret,
            errno, strerror(errno));
        fprintf(stderr, "ERROR: Could not write to test dump file (%s)..\n", testdumpfilename);
      } else {
        testdumpsize += write_ret * 8;
        if (verbose > 0)
          fprintf(stdout, "Wrote %d bytes to %s.\n", write_ret, testdumpfilename);
      }
    }

  } else if (ioctl(randomfd, RNDADDENTROPY, &random_injector) == -1) {
    perror("ioctl");
  }
  if (verbose > 3)
    printf("Kernel now at %d bits of entropy\n", bits_in_pool());
  if (bits_before == bits_in_pool()) {
    printf("Did it fail?!?!\n");
    if (NULL != testdumpfilename){
      time_to_quit = 1;
      testdumpsize = maxwatermark + 9;
    }
  }
}

void
main_loop()
{
  int ret;
  int adding = 0;
  struct pollfd fds[1];
  fds[0].fd = randomfd;
  fds[0].events = POLLOUT;
  fds[0].revents = 0;
  
  while (!time_to_quit) {
    if (verbose > 3)
      printf("Sleeping... (entropy pool at %d bits)\n", bits_in_pool());
    ret = (NULL==testdumpfilename) ? poll(fds, 1, -1) : 1;
    if (ret == -1) {
      printf("woke poll interrupted!\n");
      if (!time_to_quit)
        perror("poll");
      return;
    }
    else {
      if (verbose > 3)
        printf("woke!\n");
    }



    while (bits_in_pool() < maxwatermark) {

      int e = bitbuffer_fill(incoming_bits);
      if (e<0) {
        printf("Error: %s\n", mic_strerror(e));
        return;
      }
      ret = transfer_bits_and_debias(incoming_bits, buffered_bits);
      if (verbose > 3 && ret > 0)
        printf("Added %d bits to cache. Now at %d/%d bits in it\n", ret, bitbuffer_available_bits(buffered_bits), buffersize * 8);

      while((bits_in_pool() < maxwatermark) && (bitbuffer_available_bits(buffered_bits) >= (depositsize * 8))) {
        do_mixin_bits();
        if (verbose > 3)
          printf("Kernel entropy pool now sits at %d bits\n", bits_in_pool());
      }

    }
    if (NULL != testdumpfilename)
      time_to_quit = 1;
  }
}

void
usage(const char* prog, FILE *output)
{
  fprintf(output, "%s: Usage:\n"
          "\n"
          "Argument: h - display this help message\n"
          "          V - display version information.\n"
          "          D - Daemonize\n"
          "          p - pidfile\n"
          "          v - Increase verbosity. Can be used more than once.\n"
          "          m - specify minimum number of bits of entropy in the pool.\n"
          "          M - specify max number of bits in the pool.\n"
          "          b - specify number of bytes of randomness to buffer for use.\n"
          "          d - specify number of bytes to deposit into the pool each time.\n"
          "          T - deposit into specified file for external testing.\n",
          prog);
}

void
version(const char* prog, FILE *output)
{
  fprintf(output, "%s: %s\n"\
                  "Copyright 2007 Daniel Silverstone\n",
          prog, version_number);
}

void
cleanup(void)
{
  if (pidfile)
    if (unlink(pidfile) < 0)
      perror(pidfile);
  exit(0);
}

static void
caught_signal(int sig)
{
  time_to_quit++;
  close(randomfd);
}

void SetWriteWakeupThreshold() {
  if (NULL == testdumpfilename) {
    FILE *procWakeThresh = fopen("/proc/sys/kernel/random/write_wakeup_threshold", "w" );
    if (procWakeThresh != NULL) {
      fprintf(procWakeThresh, "%d", minwatermark);
      fclose(procWakeThresh);
    } else {
      fprintf(stderr,"%s", "Unable to write to kernel interface. Be sure to run as root or super user.\n");
      exit(2);
    }
  }
}

int
main(int argc, char **argv)
{
  int opt, oldpid;
  FILE *file;
  struct sigaction sigact;

  while ((opt = getopt(argc, argv, ":hDp:vVm:M:b:d:T:")) != -1) {
    switch (opt) {
    case 'h':
      usage(argv[0], stdout);
      return 0;
    case 'V':
      version(argv[0], stdout);
      return 0;
    case 'D':
      daemonise = 1;
      break;
    case 'p':
      pidfile = optarg;
      break;
    case 'v':
      verbose += 1;
      break;
    case 'm':
      minwatermark = atoi(optarg);
      break;
    case 'M':
      maxwatermark = atoi(optarg);
      break;
    case 'b':
      buffersize = atoi(optarg);
      break;
    case 'd':
      depositsize = atoi(optarg);
      break;
    case 'T':
      testdumpfilename = optarg;
      break;
    default:
      usage(argv[0], stderr);
      return 1;
    }
  }
  
  /* Validate options now */
  if (minwatermark < 64) {
    fprintf(stderr, "Minimum watermark is below 64 bits. This is silly.\n");
    return 2;
  }
  if (minwatermark > 4096) {
    fprintf(stderr, "Ninimum watermark is above 4096. This is not possible.\n");
    return 2;
  }
  if (maxwatermark > 4096) {
    fprintf(stderr, "Maxmimum watermark is above 4096. This is not possible.\n");
    return 2;
  }
  if (buffersize < depositsize || buffersize > (1024*1024)) {
    fprintf(stderr, "Buffer size smaller than deposit size or greater than one megabyte.\n");
    return 2;
  }
  if (depositsize < 4 || depositsize > 512 || (depositsize & 3) != 0) {
    fprintf(stderr, "Deposit size must be a multiple of four, between 4 and 512 inclusive.\n");
    return 2;
  }

  // minwatermark is valid, go ahead and write to 
  // /proc/sys/kernel/random/write_wakeup_threshold
  SetWriteWakeupThreshold();
  
  if (daemonise && !pidfile)
    pidfile = "/var/run/randomsound.pid";

  if (verbose > 0) {
    printf("Random sound daemon. Copyright 2007 Daniel Silverstone.\n\n");
    printf("Will keep random pool between %d and %d bits of entropy.\n", minwatermark, maxwatermark);
    printf("Will retain a buffer of %d bytes, making entropy deposits of %d bytes at a time.\n", buffersize, depositsize);
    if (daemonise == 1) {
      printf("Will daemonise.\n");
    }
  }
  
  if (verbose > 1) {
    printf("Allocating a %d bit buffer for the incoming bits.\n",
           buffersize * 8 * 4);
  }
  incoming_bits = bitbuffer_new(buffersize * 8 * 4);
  buffered_bits = bitbuffer_new(buffersize * 8);
  if (incoming_bits == NULL || buffered_bits == NULL) {
    fprintf(stderr, "Unable to allocate buffers.\n");
    return 3;
  }

  if (testdumpfilename) {
    printf("Running in test dump mode (no daemon).\n Dumping entropy bits to file: %s\n", testdumpfilename);
    maxwatermark = 20480*8;
    printf("Increasing maxwatermark(M) to %d (2480*8).\n", maxwatermark);
    remove(testdumpfilename);
    main_loop();
  }
  
  randomfd = open("/dev/random", O_RDWR);
  
  if (randomfd == -1) {
    perror("Opening /dev/random\n");
    return 3;
  }
  
  if (daemonise == 1) {
    printf("Daemonising\n");
    fflush(stdout);
    int fd = fork();
    if (fd == -1 ) {
      perror("forking daemon.\n");
      return 4;
    }
    if (fd != 0) return 0;

    memset(&sigact, 0, sizeof(struct sigaction));
    sigact.sa_handler = caught_signal;
    sigaction(SIGTERM, &sigact, NULL);
    sigaction(SIGHUP, &sigact, NULL);
    sigaction(SIGINT, &sigact, NULL);

    if((file = fopen(pidfile, "r")) != NULL) {
      if ((fscanf(file, "%d", &oldpid)) > 0) {
        if ((kill (oldpid, 0)) == 0) {
          fprintf(stderr, "another randomsound is already running with pid %d\n", oldpid);
          exit(0);
        }
      }
      fclose(file);
    }

    if((file = fopen(pidfile, "w")) == NULL) {
      perror("Can't open pidfile\n");
      exit(1);
    }

    fprintf(file, "%d\n", (int)getpid());
    fclose(file);
    setpgrp();
    setsid();
    atexit(cleanup);
  }
  
  main_loop();
  
  return 0;
}
