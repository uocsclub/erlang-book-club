#include <assert.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#define BUF_LEN 1024
#define MAX(x, y) (((x) > (y)) ? (x) : (y))

// To enable debug messages, turn this to a 1.
#define DEBUG 1

/* 
 * General notes about types:
 * - size_t is just unsigned long, and ssize_t is signed long.
 * - unsigned char -> byte.
 */

/* 
 * This function reads a command from stdin. It is mostly
 * self-explanatory, and the parts which aren't, I added comments for.
 */
ssize_t read_cmd(unsigned char *buf, size_t max_len, size_t header_len) {
        ssize_t rd = read(STDIN_FILENO, buf, header_len);
        if (rd != (ssize_t) header_len) {
                fprintf(stderr, "Could not read header of cmd, rd = %ld\n", rd);
                return -1;
        }

        // if (DEBUG) fprintf(stderr, "read header of size %ld\n", rd);

        /* The following for loop turns the header_len-long header
         * into a value for our reading (i.e. it is doing the decoding
         * of a big-endian-encoded integer */
        size_t len = 0; // length of message
        for (int i = header_len - 1, exp = 0; i >= 0; --i, ++exp) {
                len |= (buf[i] << exp * 8);
        }

        // if (DEBUG) fprintf(stderr, "header says size of buf is: %lu\n", len);

        /* The following loop reads from stdin into buf until we've
         * either filled the buffer or finished reading */
        int curr_i = 0;
        while (len > 0 && (max_len - curr_i > 0)) {
                rd = read(STDIN_FILENO, buf + curr_i, MAX(max_len - curr_i, len));
                // if (DEBUG) fprintf(stderr, "read %ld bytes from stdin\n", rd);
                if (rd <= 0) { /* error or EOF */
                        fprintf(stderr,
                                "return value of %ld while reading message\n",
                                rd);
                        return -1;
                }
                curr_i += rd;
                len -= rd;
        }
        /* return how much we've read */
        return curr_i;
}

/* 
 * This function outputs a hexdump to stderr.
 */
void hexdump(unsigned char *buf, size_t len) {
        int line_len = 8;
        for (size_t i = 0; i < len; ++i) {
                if (DEBUG) fprintf(stderr, "%x%x ", (buf[i] >> 4) & 0xf, buf[i] & 0xf);
                if ((i % line_len == 0) && (i != 0)) {
                        if (DEBUG) fprintf(stderr, "\n");
                }
        }
        if (DEBUG) fprintf(stderr, "\n");
}

/* 
 * Decodes an int starting at the pointer provided. 
 * Silently fails on len >= sizeof(unsigned long) 
 */
unsigned long decode_le(unsigned char *buf, size_t len) {
        unsigned int ret = 0;
        unsigned char *ptr = buf + len - 1;
        while (ptr >= buf) { // look, pointer arithemtic
                ret <<= 8;
                ret |= *ptr--;
        }
        return ret;
}

/* 
 * This function should really be trashed (because it doesn't support
 * fragmentation of result. There should be a loop very similar to the
 * read loop in read_cmd here), but it does the job technically...
 */
void write_result(unsigned long x) {
        unsigned char i = 1;
        unsigned char buf[sizeof(unsigned long) + 2];
        while (x) {
                buf[i++] = x & 0xff;
                x >>= 8;
        }
        buf[0] = i - 1;
        unsigned char head[2] = {0, i};
        if (DEBUG) {
                fprintf(stderr,
                        "C program sending the following bytes to erlang "
                        "(with header):\n");
        }
        int wr = 0;
        wr = write(STDOUT_FILENO, head, 2);
        if (wr != 2) { fprintf(stderr, "failed to write result, wr = %d\n", wr); }
        for (int j = 0; j < 2; ++j) {
                if (DEBUG) fprintf(stderr, "%x%x ", (head[j] >> 4) & 0xf, head[j] & 0xf);
        }
        wr = write(STDOUT_FILENO, buf, i);
        if (wr != i) { fprintf(stderr, "failed to write result, i = %d, wr = %d\n", i, wr); }
        for (int j = 0; j < i; ++j) {
                if (DEBUG) fprintf(stderr, "%x%x ", (buf[j] >> 4) & 0xf, buf[j] & 0xf);
        }
        if (DEBUG) fprintf(stderr, "\n");
}

int main() {
        if (DEBUG) fprintf(stderr, "starting external program\n");
        unsigned char buf[BUF_LEN];

        /* This is the event loop. Read a command, figure out which
         * command is being invoked, then send the result back. */
        ssize_t rd;
        while ((rd = read_cmd(buf, BUF_LEN, 2)) >= 0) {
                if (DEBUG) {
                        fprintf(stderr, 
                                "Hexdump of bytes received by C program, minus header:\n");
                        hexdump(buf, rd);
                }

                switch (buf[0]) {
                case 1: {
                        int len_x = buf[1];
                        assert((size_t) len_x < sizeof(unsigned long));
                        int x = decode_le(buf + 2, len_x);
                        int len_y = buf[2 + len_x];
                        assert((size_t) len_y < sizeof(unsigned long));
                        int y = decode_le(buf + 3 + len_x, len_y);
                        write_result(x + y);
                } break;

                case 2: {
                        int len_x = buf[1];
                        assert((size_t) len_x < sizeof(unsigned long));
                        int x = decode_le(buf + 2, len_x);
                        write_result(x << 1);
                } break;

                default:
                        fprintf(stderr,
                                "Unrecognized function received through pipe: %d\n",
                                buf[0]);
                }
        }
}
