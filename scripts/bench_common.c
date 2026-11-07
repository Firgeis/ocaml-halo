#define _POSIX_C_SOURCE 200809L
#include "bench_common.h"
#include <arpa/inet.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <getopt.h>
#include <limits.h>
#include <math.h>
#include <netdb.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

typedef struct {
  char *data;
  size_t len;
  size_t cap;
} buffer;

typedef struct {
  const char *host;
  const char *method;
  const char *path;
  const char *scenario;
  const char *server_name;
  const char *ca_file;
  const char *reuse_mode;
  const char *tls_version;
  const char *body_arg;
  const char *body_file;
  int port;
  int concurrency;
  int requests;
  int warmup;
  int chunked;
  int chunk_size;
  int keep_alive;
  int tls;
  double timeout;
  char **headers;
  int nheaders;
  int *expected;
  int nexpected;
  buffer request;
  size_t payload_len;
} config;

typedef struct {
  config *cfg;
  int iterations;
  int count;
  int reused;
  double *latencies;
  int *statuses;
  char error[512];
} worker;

typedef struct {
  int fd;
  SSL *ssl;
} connection;

static SSL_CTX *make_context(config *c, char *err, size_t n);
static void close_connection(connection *c);
static int open_connection(config *cfg, connection *c, SSL_CTX *ctx,
                           SSL_SESSION *session, char *err, size_t n);

static void die(const char *s) {
  fprintf(stderr, "%s\n", s);
  exit(2);
}

static void *xrealloc(void *p, size_t n) {
  void *q = realloc(p, n ? n : 1);
  if (!q)
    die("out of memory");
  return q;
}

static void append(buffer *b, const void *data, size_t len) {
  if (len > SIZE_MAX - b->len - 1)
    die("request too large");
  size_t need = b->len + len + 1;
  if (need > b->cap) {
    size_t cap = b->cap ? b->cap : 256;
    while (cap < need) {
      if (cap > SIZE_MAX / 2) {
        cap = need;
        break;
      }
      cap *= 2;
    }
    b->data = xrealloc(b->data, cap);
    b->cap = cap;
  }
  memcpy(b->data + b->len, data, len);
  b->len += len;
  b->data[b->len] = 0;
}

static void format(buffer *b, const char *fmt, ...) {
  va_list ap, copy;
  va_start(ap, fmt);
  va_copy(copy, ap);
  int n = vsnprintf(NULL, 0, fmt, copy);
  va_end(copy);
  if (n < 0)
    die("formatting failed");
  char *s = xrealloc(NULL, (size_t)n + 1);
  vsnprintf(s, (size_t)n + 1, fmt, ap);
  va_end(ap);
  append(b, s, (size_t)n);
  free(s);
}

static double now(void) {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return t.tv_sec + t.tv_nsec / 1e9;
}

static void json_string(const char *s) {
  putchar('"');
  for (const unsigned char *p = (const unsigned char *)s; *p; p++) {
    if (*p == '"' || *p == '\\') {
      putchar('\\');
      putchar(*p);
    } else if (*p < 32)
      printf("\\u%04x", *p);
    else
      putchar(*p);
  }
  putchar('"');
}

static char *trim(char *s) {
  while (isspace((unsigned char)*s))
    s++;
  char *e = s + strlen(s);
  while (e > s && isspace((unsigned char)e[-1]))
    *--e = 0;
  return s;
}

static long integer(const char *s, const char *name) {
  char *end;
  errno = 0;
  long n = strtol(s, &end, 10);
  if (errno || *s == 0 || *end || n < INT_MIN || n > INT_MAX) {
    fprintf(stderr, "invalid %s: %s\n", name, s);
    exit(2);
  }
  return n;
}

static double decimal(const char *s) {
  char *end;
  errno = 0;
  double d = strtod(s, &end);
  if (errno || *s == 0 || *end || !isfinite(d) || d <= 0 ||
      d > INT_MAX / 1000.0)
    die("--timeout-seconds must be positive and finite");
  return d;
}

static void build_request(config *c) {
  buffer body = {0};
  if (c->body_file) {
    FILE *f = fopen(c->body_file, "rb");
    if (!f) {
      perror(c->body_file);
      exit(2);
    }
    char buf[16384];
    size_t n;
    while ((n = fread(buf, 1, sizeof buf, f)))
      append(&body, buf, n);
    if (ferror(f))
      die("error reading body file");
    fclose(f);
  } else
    append(&body, c->body_arg, strlen(c->body_arg));
  c->payload_len = body.len;
  buffer *r = &c->request;
  format(r, "%s %s HTTP/1.1\r\nHost: %s\r\n", c->method, c->path,
         c->tls ? c->server_name : c->host);
  int has_connection = 0, has_content_length = 0, has_transfer_encoding = 0;
  for (int i = 0; i < c->nheaders; i++) {
    char *copy = strdup(c->headers[i]);
    if (!copy)
      die("out of memory");
    char *colon = strchr(copy, ':');
    if (!colon)
      die("header must be in NAME:VALUE form");
    *colon++ = 0;
    char *name = trim(copy), *value = trim(colon);
    has_connection |= !strcasecmp(name, "Connection");
    has_content_length |= !strcasecmp(name, "Content-Length");
    has_transfer_encoding |= !strcasecmp(name, "Transfer-Encoding");
    format(r, "%s: %s\r\n", name, value);
    free(copy);
  }
  if (!has_connection)
    format(r, "Connection: %s\r\n", c->keep_alive ? "keep-alive" : "close");
  if (c->chunked && has_content_length)
    die("chunked requests cannot include Content-Length");
  if (c->chunked && !has_transfer_encoding)
    format(r, "Transfer-Encoding: chunked\r\n");
  if (body.len && !c->chunked && !has_content_length)
    format(r, "Content-Length: %zu\r\n", body.len);
  append(r, "\r\n", 2);
  if (c->chunked) {
    for (size_t off = 0; off < body.len;) {
      size_t n = body.len - off;
      if (n > (size_t)c->chunk_size)
        n = (size_t)c->chunk_size;
      format(r, "%zX\r\n", n);
      append(r, body.data + off, n);
      append(r, "\r\n", 2);
      off += n;
    }
    append(r, "0\r\n\r\n", 5);
  } else if (body.len)
    append(r, body.data, body.len);
  free(body.data);
}

static int connect_socket(config *c, char *err, size_t errlen) {
  char port[16];
  snprintf(port, sizeof port, "%d", c->port);
  struct addrinfo hints = {0}, *list = NULL;
  hints.ai_socktype = SOCK_STREAM;
  hints.ai_family = AF_UNSPEC;
  int e = getaddrinfo(c->host, port, &hints, &list);
  if (e) {
    snprintf(err, errlen, "%s", gai_strerror(e));
    return -1;
  }
  int fd = -1, saved = ECONNREFUSED;
  for (struct addrinfo *a = list; a; a = a->ai_next) {
    fd = socket(a->ai_family, a->ai_socktype, a->ai_protocol);
    if (fd < 0) {
      saved = errno;
      continue;
    }
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0 || fcntl(fd, F_SETFL, flags | O_NONBLOCK) < 0) {
      saved = errno;
      close(fd);
      fd = -1;
      continue;
    }
    int rc = connect(fd, a->ai_addr, a->ai_addrlen);
    if (rc < 0 && errno == EINPROGRESS) {
      struct pollfd p = {fd, POLLOUT, 0};
      do {
        rc = poll(&p, 1, (int)(c->timeout * 1000));
      } while (rc < 0 && errno == EINTR);
      if (rc > 0) {
        socklen_t l = sizeof saved;
        if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &saved, &l) < 0)
          saved = errno;
        rc = saved ? -1 : 0;
      } else {
        saved = rc == 0 ? ETIMEDOUT : errno;
        rc = -1;
      }
    } else if (rc < 0)
      saved = errno;
    if (rc == 0 && fcntl(fd, F_SETFL, flags) == 0) {
      struct timeval tv = {
          (time_t)c->timeout,
          (suseconds_t)((c->timeout - floor(c->timeout)) * 1e6)};
      if (!tv.tv_sec && !tv.tv_usec)
        tv.tv_usec = 1;
      setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof tv);
      setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof tv);
      break;
    }
    close(fd);
    fd = -1;
  }
  freeaddrinfo(list);
  if (fd < 0)
    snprintf(err, errlen, "%s", strerror(saved));
  return fd;
}

/* A child also bounds getaddrinfo, which has no portable timeout interface. */
static int wait_for_server(config *c) {
  double deadline = now() + 5.0;
  pid_t child = fork();
  if (child < 0) {
    perror("fork");
    return 1;
  }
  if (child == 0) {
    c->timeout = 0.2;
    SSL_CTX *ctx = NULL;
    char err[256];
    if (c->tls) {
      ctx = make_context(c, err, sizeof err);
      if (!ctx) {
        fprintf(stderr, "%s\n", err);
        _exit(1);
      }
    }
    while (now() < deadline) {
      connection conn = {-1, NULL};
      if (!open_connection(c, &conn, ctx, NULL, err, sizeof err)) {
        close_connection(&conn);
        SSL_CTX_free(ctx);
        _exit(0);
      }
      struct timespec delay = {0, 50000000};
      while (nanosleep(&delay, &delay) < 0 && errno == EINTR)
        ;
    }
    _exit(1);
  }

  int status;
  for (;;) {
    pid_t result = waitpid(child, &status, WNOHANG);
    if (result == child) {
      if (WIFEXITED(status) && WEXITSTATUS(status) == 0 && now() <= deadline)
        return 0;
      break;
    }
    if (result < 0 && errno != EINTR) {
      perror("waitpid");
      return 1;
    }
    double remaining = deadline - now();
    if (remaining <= 0) {
      kill(child, SIGKILL);
      while (waitpid(child, &status, 0) < 0 && errno == EINTR)
        ;
      break;
    }
    if (remaining > 0.01)
      remaining = 0.01;
    struct timespec delay = {0, (long)(remaining * 1e9)};
    nanosleep(&delay, NULL);
  }
  fprintf(stderr, "server did not start listening in time\n");
  return 1;
}

static SSL_CTX *make_context(config *c, char *err, size_t n) {
  SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
  int version =
      !strcasecmp(c->tls_version, "TLS12") ? TLS1_2_VERSION : TLS1_3_VERSION;
  if (!ctx || !SSL_CTX_set_min_proto_version(ctx, version) ||
      !SSL_CTX_set_max_proto_version(ctx, version) ||
      !SSL_CTX_load_verify_locations(ctx, c->ca_file, NULL)) {
    unsigned long e = ERR_get_error();
    snprintf(err, n, "TLS context: %s",
             e ? ERR_error_string(e, NULL) : "initialization failed");
    SSL_CTX_free(ctx);
    return NULL;
  }
  SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
  SSL_CTX_set_session_cache_mode(ctx, SSL_SESS_CACHE_CLIENT);
  return ctx;
}

static void close_connection(connection *c) {
  if (c->ssl)
    SSL_free(c->ssl);
  if (c->fd >= 0)
    close(c->fd);
  c->fd = -1;
  c->ssl = NULL;
}

static int open_connection(config *cfg, connection *c, SSL_CTX *ctx,
                           SSL_SESSION *session, char *err, size_t n) {
  c->fd = connect_socket(cfg, err, n);
  if (c->fd < 0)
    return -1;
  if (cfg->tls) {
    c->ssl = SSL_new(ctx);
    if (!c->ssl || !SSL_set_fd(c->ssl, c->fd) ||
        !SSL_set_tlsext_host_name(c->ssl, cfg->server_name) ||
        (session && !SSL_set_session(c->ssl, session)) ||
        SSL_connect(c->ssl) != 1) {
      unsigned long e = ERR_get_error();
      snprintf(err, n, "TLS handshake: %s",
               e ? ERR_error_string(e, NULL) : strerror(errno));
      close_connection(c);
      return -1;
    }
  }
  return 0;
}

static int send_all(connection *c, const char *data, size_t len, char *err,
                    size_t n) {
  while (len) {
    int part = len > INT_MAX ? INT_MAX : (int)len;
    int rc = c->ssl ? SSL_write(c->ssl, data, part)
                    : (int)send(c->fd, data, (size_t)part, 0);
    if (rc <= 0) {
      unsigned long e = c->ssl ? ERR_get_error() : 0;
      snprintf(err, n, "send: %s",
               e ? ERR_error_string(e, NULL) : strerror(errno));
      return -1;
    }
    data += rc;
    len -= (size_t)rc;
  }
  return 0;
}

static int read_response(connection *c, char *err, size_t n) {
  buffer b = {0};
  char chunk[4096];
  bool headers_complete = false;
  size_t body_start = 0, length = 0;
  int status = 0, has_length = 0;
  for (;;) {
    int rc = c->ssl ? SSL_read(c->ssl, chunk, sizeof chunk)
                    : (int)recv(c->fd, chunk, sizeof chunk, 0);
    if (rc <= 0) {
      snprintf(err, n, "%s",
               headers_complete ? "connection closed before body completed"
                                : "connection closed before headers completed");
      free(b.data);
      return -1;
    }
    append(&b, chunk, (size_t)rc);
    char *end = headers_complete ? NULL : strstr(b.data, "\r\n\r\n");
    if (end) {
      body_start = (size_t)(end - b.data) + 4;
      *end = 0;
      if (sscanf(b.data, "%*s %d", &status) != 1 || status < 0 ||
          status > 999) {
        snprintf(err, n, "invalid status line");
        free(b.data);
        return -1;
      }
      char *line = strstr(b.data, "\r\n");
      while (line && line < end) {
        line += 2;
        if (!strncasecmp(line, "Content-Length:", 15)) {
          char *p = line + 15, *stop;
          while (*p == ' ' || *p == '\t')
            p++;
          errno = 0;
          if (*p == '-') {
            snprintf(err, n, "invalid Content-Length");
            free(b.data);
            return -1;
          }
          unsigned long long v = strtoull(p, &stop, 10);
          if (errno || stop == p || v > SIZE_MAX - body_start ||
              (*stop != '\r' && *stop != '\0' &&
               !isspace((unsigned char)*stop))) {
            snprintf(err, n, "invalid Content-Length");
            free(b.data);
            return -1;
          }
          length = (size_t)v;
          has_length = 1;
          break;
        }
        line = strstr(line, "\r\n");
      }
      if (!has_length && !(status >= 100 && status < 200) && status != 204 &&
          status != 304) {
        snprintf(err, n, "response omitted Content-Length");
        free(b.data);
        return -1;
      }
      /* Restore the separator before the buffer can be reallocated. */
      *end = '\r';
      headers_complete = true;
    }
    if (headers_complete && b.len - body_start >= length)
      break;
  }
  free(b.data);
  return status;
}

static void *run_worker(void *arg) {
  worker *w = arg;
  config *c = w->cfg;
  SSL_CTX *shared = NULL;
  SSL_SESSION *session = NULL;
  connection conn = {-1, NULL};
  if (c->tls && !strcmp(c->reuse_mode, "context-reuse")) {
    shared = make_context(c, w->error, sizeof w->error);
    if (!shared)
      goto done;
  }
  if (c->keep_alive &&
      open_connection(c, &conn, NULL, NULL, w->error, sizeof w->error))
    goto done;
  for (int i = 0; i < w->iterations; i++) {
    SSL_CTX *ctx = shared;
    if (c->tls && !ctx) {
      ctx = make_context(c, w->error, sizeof w->error);
      if (!ctx)
        break;
    }
    double start = now();
    if (!c->keep_alive &&
        open_connection(c, &conn, ctx, session, w->error, sizeof w->error)) {
      if (ctx != shared)
        SSL_CTX_free(ctx);
      break;
    }
    if (send_all(&conn, c->request.data, c->request.len, w->error,
                 sizeof w->error)) {
      close_connection(&conn);
      if (ctx != shared)
        SSL_CTX_free(ctx);
      break;
    }
    int status = read_response(&conn, w->error, sizeof w->error);
    if (status < 0) {
      close_connection(&conn);
      if (ctx != shared)
        SSL_CTX_free(ctx);
      break;
    }
    w->latencies[w->count] = (now() - start) * 1000;
    w->statuses[w->count++] = status;
    if (c->tls) {
      w->reused += SSL_session_reused(conn.ssl);
      SSL_shutdown(conn.ssl);
      if (shared) {
        SSL_SESSION *next = SSL_get1_session(conn.ssl);
        SSL_SESSION_free(session);
        session = next;
      }
    }
    if (!c->keep_alive)
      close_connection(&conn);
    if (ctx != shared)
      SSL_CTX_free(ctx);
  }
done:
  close_connection(&conn);
  SSL_SESSION_free(session);
  SSL_CTX_free(shared);
  return NULL;
}

static int compare_double(const void *a, const void *b) {
  double x = *(const double *)a, y = *(const double *)b;
  return (x > y) - (x < y);
}

static double percentile(double *v, int n, double p) {
  if (!n)
    return 0;
  double rank = p * (n - 1);
  int lo = (int)rank, hi = lo + (lo + 1 < n);
  return v[lo] * (1 - (rank - lo)) + v[hi] * (rank - lo);
}

static int compare_int(const void *a, const void *b) {
  int x = *(const int *)a, y = *(const int *)b;
  return (x > y) - (x < y);
}

static int allowed(config *c, int status) {
  for (int i = 0; i < c->nexpected; i++)
    if (c->expected[i] == status)
      return 1;
  return 0;
}

static int benchmark(config *c, int requests, int concurrency, int warmup) {
  int count = requests < concurrency ? requests : concurrency;
  worker *workers = calloc((size_t)count, sizeof *workers);
  pthread_t *threads = calloc((size_t)count, sizeof *threads);
  if (!workers || !threads)
    die("out of memory");
  double start = now();
  int started = 0;
  for (int i = 0; i < count; i++) {
    worker *w = &workers[i];
    w->cfg = c;
    w->iterations = requests / concurrency + (i < requests % concurrency);
    w->latencies = calloc((size_t)w->iterations, sizeof(double));
    w->statuses = calloc((size_t)w->iterations, sizeof(int));
    if (!w->latencies || !w->statuses)
      die("out of memory");
    int rc = pthread_create(&threads[i], NULL, run_worker, w);
    if (rc) {
      snprintf(w->error, sizeof w->error, "pthread_create: %s", strerror(rc));
      break;
    }
    started++;
  }
  for (int i = 0; i < started; i++)
    pthread_join(threads[i], NULL);
  double duration = now() - start;
  int completed = 0, reused = 0, errors = 0, counts[1000] = {0};
  double *lat = calloc((size_t)requests, sizeof *lat);
  if (!lat)
    die("out of memory");
  double sum = 0;
  for (int i = 0; i < count; i++) {
    worker *w = &workers[i];
    if (w->error[0])
      errors++;
    reused += w->reused;
    for (int j = 0; j < w->count; j++) {
      lat[completed++] = w->latencies[j];
      sum += w->latencies[j];
      counts[w->statuses[j]]++;
    }
  }
  if (warmup && !errors)
    goto cleanup;
  qsort(lat, (size_t)completed, sizeof *lat, compare_double);
  if (warmup) {
    printf("{\"scenario\":");
    json_string(c->scenario);
    printf(",\"warmup\":");
  }
  printf("{\"requests\":%d,\"completed_requests\":%d,\"concurrency\":%d,"
         "\"duration_seconds\":%.17g,\"throughput_rps\":%.17g,",
         requests, completed, concurrency, duration,
         duration > 0 ? completed / duration : 0.0);
  printf("\"latency_ms\":{\"mean\":%.17g,\"median\":%.17g,\"p95\":%.17g,"
         "\"p99\":%.17g,\"max\":%.17g},\"status_counts\":{",
         completed ? sum / completed : 0.0, percentile(lat, completed, .5),
         percentile(lat, completed, .95), percentile(lat, completed, .99),
         completed ? lat[completed - 1] : 0.0);
  int sep = 0;
  for (int i = 0; i < 1000; i++)
    if (counts[i]) {
      printf("%s\"%d\":%d", sep++ ? "," : "", i, counts[i]);
    }
  printf("},\"errors\":[");
  sep = 0;
  for (int i = 0; i < count; i++)
    if (workers[i].error[0]) {
      if (sep++)
        putchar(',');
      json_string(workers[i].error);
    }
  putchar(']');
  if (c->tls) {
    printf(",\"session_reused_count\":%d,\"reuse_mode\":", reused);
    json_string(c->reuse_mode);
  }
  if (!warmup) {
    printf(",\"scenario\":");
    json_string(c->scenario);
    if (!c->tls) {
      printf(",\"request_bytes\":%zu,\"payload_bytes\":%zu,\"method\":",
             c->request.len, c->payload_len);
      json_string(c->method);
      printf(",\"path\":");
      json_string(c->path);
      printf(",\"chunked\":%s,\"keep_alive\":%s", c->chunked ? "true" : "false",
             c->keep_alive ? "true" : "false");
      if (c->chunked)
        printf(",\"chunk_size\":%d", c->chunk_size);
      if (c->nexpected) {
        printf(",\"expected_statuses\":[");
        sep = 0;
        qsort(c->expected, (size_t)c->nexpected, sizeof *c->expected,
              compare_int);
        for (int i = 0; i < c->nexpected; i++)
          if (i == 0 || c->expected[i] != c->expected[i - 1])
            printf("%s%d", sep++ ? "," : "", c->expected[i]);
        printf("],\"unexpected_status_counts\":{");
        sep = 0;
        for (int i = 0; i < 1000; i++)
          if (counts[i] && !allowed(c, i)) {
            printf("%s\"%d\":%d", sep++ ? "," : "", i, counts[i]);
            errors++;
          }
        putchar('}');
      }
    }
  }
  putchar('}');
  if (warmup)
    putchar('}');
  putchar('\n');
cleanup:
  for (int i = 0; i < count; i++) {
    free(workers[i].latencies);
    free(workers[i].statuses);
  }
  free(workers);
  free(threads);
  free(lat);
  return errors ? 1 : 0;
}

int bench_main(int argc, char **argv, int tls) {
  config c = {.host = "127.0.0.1",
              .port = 8080,
              .method = "GET",
              .path = "/",
              .server_name = "localhost",
              .body_arg = "",
              .concurrency = tls ? 16 : 32,
              .requests = tls ? 1000 : 5000,
              .warmup = tls ? 100 : 200,
              .timeout = 5,
              .scenario = "unnamed",
              .reuse_mode = "context-reuse",
              .tls_version = "TLS13",
              .chunk_size = 8192,
              .tls = tls};
  enum {
    HOST = 256,
    PORT,
    METHOD,
    PATH,
    HEADER,
    BODY,
    BODY_FILE,
    CONCURRENCY,
    REQUESTS,
    WARMUP,
    TIMEOUT,
    SCENARIO,
    EXPECT,
    CHUNKED,
    CHUNK_SIZE,
    KEEP,
    SERVER_NAME,
    CA_FILE,
    REUSE,
    TLS_VERSION,
    WAIT_READY
  };
  static const struct option opts[] = {
      {"host", required_argument, 0, HOST},
      {"port", required_argument, 0, PORT},
      {"method", required_argument, 0, METHOD},
      {"path", required_argument, 0, PATH},
      {"header", required_argument, 0, HEADER},
      {"body", required_argument, 0, BODY},
      {"body-file", required_argument, 0, BODY_FILE},
      {"concurrency", required_argument, 0, CONCURRENCY},
      {"requests", required_argument, 0, REQUESTS},
      {"warmup-requests", required_argument, 0, WARMUP},
      {"timeout-seconds", required_argument, 0, TIMEOUT},
      {"scenario", required_argument, 0, SCENARIO},
      {"expect-status", required_argument, 0, EXPECT},
      {"chunked", no_argument, 0, CHUNKED},
      {"chunk-size", required_argument, 0, CHUNK_SIZE},
      {"keep-alive", no_argument, 0, KEEP},
      {"server-name", required_argument, 0, SERVER_NAME},
      {"ca-file", required_argument, 0, CA_FILE},
      {"reuse-mode", required_argument, 0, REUSE},
      {"tls-version", required_argument, 0, TLS_VERSION},
      {"wait-ready", no_argument, 0, WAIT_READY},
      {"help", no_argument, 0, 'h'},
      {0, 0, 0, 0}};
  int o, wait_ready = 0;
  while ((o = getopt_long(argc, argv, "h", opts, NULL)) != -1) {
    switch (o) {
    case HOST:
      c.host = optarg;
      break;
    case PORT:
      c.port = (int)integer(optarg, "port");
      break;
    case METHOD:
      c.method = optarg;
      break;
    case PATH:
      c.path = optarg;
      break;
    case HEADER:
      c.headers =
          xrealloc(c.headers, (size_t)(c.nheaders + 1) * sizeof *c.headers);
      c.headers[c.nheaders++] = optarg;
      break;
    case BODY:
      c.body_arg = optarg;
      break;
    case BODY_FILE:
      c.body_file = optarg;
      break;
    case CONCURRENCY:
      c.concurrency = (int)integer(optarg, "concurrency");
      break;
    case REQUESTS:
      c.requests = (int)integer(optarg, "requests");
      break;
    case WARMUP:
      c.warmup = (int)integer(optarg, "warmup-requests");
      break;
    case TIMEOUT:
      c.timeout = decimal(optarg);
      break;
    case SCENARIO:
      c.scenario = optarg;
      break;
    case EXPECT:
      if (tls)
        die("unknown option --expect-status");
      c.expected =
          xrealloc(c.expected, (size_t)(c.nexpected + 1) * sizeof *c.expected);
      c.expected[c.nexpected++] = (int)integer(optarg, "expect-status");
      break;
    case CHUNKED:
      if (tls)
        die("unknown option --chunked");
      c.chunked = 1;
      break;
    case CHUNK_SIZE:
      if (tls)
        die("unknown option --chunk-size");
      c.chunk_size = (int)integer(optarg, "chunk-size");
      break;
    case KEEP:
      if (tls)
        die("unknown option --keep-alive");
      c.keep_alive = 1;
      break;
    case SERVER_NAME:
      if (!tls)
        die("unknown option --server-name");
      c.server_name = optarg;
      break;
    case CA_FILE:
      if (!tls)
        die("unknown option --ca-file");
      c.ca_file = optarg;
      break;
    case REUSE:
      if (!tls)
        die("unknown option --reuse-mode");
      c.reuse_mode = optarg;
      break;
    case TLS_VERSION:
      if (!tls)
        die("unknown option --tls-version");
      c.tls_version = optarg;
      break;
    case WAIT_READY:
      wait_ready = 1;
      break;
    case 'h':
      printf("Usage: %s [--host HOST] [--port PORT] [--method METHOD] [--path "
             "PATH] [--header NAME:VALUE] [--body BODY | --body-file FILE] "
             "[--concurrency N] [--requests N] [--warmup-requests N] "
             "[--timeout-seconds N] [--scenario NAME] [--wait-ready] %s\n",
             argv[0],
             tls ? "--ca-file FILE [--server-name NAME] [--reuse-mode "
                   "full|context-reuse] [--tls-version TLS12|TLS13]"
                 : "[--expect-status N] [--chunked] [--chunk-size N] "
                   "[--keep-alive]");
      return 0;
    default:
      return 2;
    }
  }
  if (optind != argc)
    die("unexpected positional argument");
  if (c.concurrency <= 0)
    die("--concurrency must be positive");
  if (c.requests <= 0)
    die("--requests must be positive");
  if (c.warmup < 0)
    die("--warmup-requests must be non-negative");
  if (c.chunk_size <= 0)
    die("--chunk-size must be positive");
  if (c.port < 0 || c.port > 65535)
    die("invalid port");
  if (tls && !c.ca_file)
    die("--ca-file is required");
  if (tls && strcasecmp(c.tls_version, "TLS12") &&
      strcasecmp(c.tls_version, "TLS13"))
    die("--tls-version must be TLS12 or TLS13");
  signal(SIGPIPE, SIG_IGN);
  if (wait_ready) {
    int rc = wait_for_server(&c);
    free(c.headers);
    free(c.expected);
    return rc;
  }
  if (tls && strcmp(c.reuse_mode, "full") &&
      strcmp(c.reuse_mode, "context-reuse"))
    die("--reuse-mode must be full or context-reuse");
  build_request(&c);
  int rc = 0;
  if (c.warmup)
    rc = benchmark(&c, c.warmup,
                   c.concurrency < c.warmup ? c.concurrency : c.warmup, 1);
  if (!rc)
    rc = benchmark(&c, c.requests, c.concurrency, 0);
  free(c.request.data);
  free(c.headers);
  free(c.expected);
  return rc;
}
