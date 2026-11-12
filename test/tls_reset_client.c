#include <arpa/inet.h>
#include <openssl/ssl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

/* Signal after sending the request, then wait for the parent to request RST. */
int main(int argc, char** argv) {
  const char request[] = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n";
  struct sockaddr_in address = {0};
  struct linger linger = {1, 0};
  SSL_CTX* context;
  SSL* connection;
  char signal;
  int socket_fd;

  if (argc != 2)
    return 1;
  if (strcmp(argv[1], "--wait-for-eof") == 0) {
    if (write(STDOUT_FILENO, "R", 1) != 1)
      return 1;
    return read(STDIN_FILENO, &signal, 1) == 0 ? 0 : 1;
  }
  if (strcmp(argv[1], "--stubborn") == 0) {
    if (write(STDOUT_FILENO, "R", 1) != 1)
      return 1;
    for (;;)
      pause();
  }
  socket_fd = socket(AF_INET, SOCK_STREAM, 0);
  if (socket_fd < 0 || setsockopt(socket_fd, SOL_SOCKET, SO_LINGER,
                                  &linger, sizeof(linger)) != 0)
    return 1;
  address.sin_family = AF_INET;
  address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
  address.sin_port = htons((unsigned short) atoi(argv[1]));
  if (connect(socket_fd, (struct sockaddr*) &address, sizeof(address)) != 0)
    return 1;
  context = SSL_CTX_new(TLS_client_method());
  if (context == NULL)
    return 1;
  connection = SSL_new(context);
  if (connection == NULL || SSL_set_fd(connection, socket_fd) != 1
      || SSL_connect(connection) != 1
      || SSL_write(connection, request, sizeof(request) - 1) != (int) sizeof(request) - 1
      || write(STDOUT_FILENO, "R", 1) != 1
      || read(STDIN_FILENO, &signal, 1) != 1)
    return 1;
  close(socket_fd);
  SSL_free(connection);
  SSL_CTX_free(context);
  return 0;
}
