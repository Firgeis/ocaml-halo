#define _POSIX_C_SOURCE 200809L
#include <arpa/inet.h>
#include <openssl/ssl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <fcntl.h>
#include <strings.h>

static int read_byte(int fd, SSL *ssl, char *c) { return ssl ? SSL_read(ssl, c, 1) : (int)read(fd, c, 1); }
static int line(int fd, SSL *ssl, char *buf, size_t cap) {
  size_t n = 0; char c;
  while (n + 1 < cap && read_byte(fd, ssl, &c) == 1) { buf[n++] = c; if (c == '\n') { buf[n] = 0; return 1; } }
  return 0;
}
static int bytes(int fd, SSL *ssl, unsigned char *buf, size_t n) {
  for (size_t i = 0; i < n; i++) if (read_byte(fd, ssl, (char *)&buf[i]) != 1) return 0;
  return 1;
}
static void serve(int fd, SSL_CTX *ctx, int stall, const char *log_path) {
  alarm(3);
  if (stall) { sleep(2); return; }
  SSL *ssl = ctx ? SSL_new(ctx) : NULL;
  if (ssl) { SSL_set_fd(ssl, fd); if (SSL_accept(ssl) != 1) { SSL_free(ssl); return; } }
  char buf[8192], header[1024]; unsigned char body[65536];
  while (line(fd, ssl, buf, sizeof buf)) {
    size_t length = 0, used = 0; int chunked = 0, close_conn = 0; header[0] = 0;
    while (line(fd, ssl, buf, sizeof buf) && strcmp(buf, "\r\n")) {
      if (!strncasecmp(buf, "Content-Length:", 15)) length = strtoul(buf + 15, NULL, 10);
      if (!strncasecmp(buf, "Transfer-Encoding:", 18) && strstr(buf, "chunked")) chunked = 1;
      if (!strncasecmp(buf, "Connection:", 11) && strstr(buf, "close")) close_conn = 1;
      if (!strncasecmp(buf, "X-Test:", 7)) { size_t n = strcspn(buf + 7, "\r\n"); if (n >= sizeof header) n = sizeof header - 1; memcpy(header, buf + 7, n); header[n] = 0; }
    }
    if (chunked) {
      for (;;) {
        if (!line(fd, ssl, buf, sizeof buf)) goto done;
        length = strtoul(buf, NULL, 16);
        if (!length) { if (!line(fd, ssl, buf, sizeof buf)) goto done; break; }
        if (length > sizeof body - used || !bytes(fd, ssl, body + used, length)) goto done;
        used += length;
        if (!line(fd, ssl, buf, sizeof buf)) goto done;
      }
    } else { if (length > sizeof body || !bytes(fd, ssl, body, length)) goto done; used = length; }
    if (log_path) {
      int log = open(log_path, O_WRONLY | O_CREAT | O_APPEND, 0600);
      char record[140000]; int n = snprintf(record, sizeof record, "%ld|%s|", (long)getpid(), header);
      for (size_t i = 0; i < used && n + 3 < (int)sizeof record; i++) n += snprintf(record + n, sizeof record - (size_t)n, "%02x", body[i]);
      record[n++] = '\n'; if (log >= 0) { (void)write(log, record, (size_t)n); close(log); }
    }
    const char response[] = "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nok";
    if (ssl) { if (SSL_write(ssl, response, sizeof response - 1) <= 0) break; }
    else if (write(fd, response, sizeof response - 1) <= 0) break;
    if (close_conn) break;
  }
done:
  if (ssl) { SSL_shutdown(ssl); SSL_free(ssl); }
}
int main(int argc, char **argv) {
  if (argc != 5) return 2;
  signal(SIGPIPE, SIG_IGN); signal(SIGCHLD, SIG_IGN); setpgid(0, 0);
  SSL_CTX *ctx = NULL;
  if (strcmp(argv[1], "plain") && strcmp(argv[1], "stall")) {
    ctx = SSL_CTX_new(TLS_server_method()); if (!ctx) return 3;
    int version = !strcmp(argv[1], "TLS12") ? TLS1_2_VERSION : TLS1_3_VERSION;
    if (!SSL_CTX_set_min_proto_version(ctx, version) || !SSL_CTX_set_max_proto_version(ctx, version) ||
        SSL_CTX_use_certificate_file(ctx, argv[2], SSL_FILETYPE_PEM) != 1 ||
        SSL_CTX_use_PrivateKey_file(ctx, argv[3], SSL_FILETYPE_PEM) != 1) return 4;
    SSL_CTX_set_session_cache_mode(ctx, SSL_SESS_CACHE_SERVER);
    const unsigned char sid[] = "halo-bench-fixture";
    SSL_CTX_set_session_id_context(ctx, sid, sizeof sid - 1);
  }
  int server = socket(AF_INET, SOCK_STREAM, 0); if (server < 0) return 5;
  struct sockaddr_in addr = {0}; addr.sin_family = AF_INET; addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  if (bind(server, (struct sockaddr *)&addr, sizeof addr) || listen(server, 64)) return 6;
  socklen_t size = sizeof addr; if (getsockname(server, (struct sockaddr *)&addr, &size)) return 7;
  printf("%u\n", ntohs(addr.sin_port)); fflush(stdout);
  for (;;) {
    int fd = accept(server, NULL, NULL); if (fd < 0) continue;
    pid_t child = fork();
    if (!child) { close(server); serve(fd, ctx, !strcmp(argv[1], "stall"), strcmp(argv[4], "-") ? argv[4] : NULL); close(fd); _exit(0); }
    close(fd);
  }
}
