#!/usr/bin/env python3

import argparse
import json
import socket
import ssl
import statistics
import threading
import time
from typing import Dict, List, Optional, Tuple


CRLF = b"\r\n"
HEADER_BODY_SEPARATOR = b"\r\n\r\n"


def build_request(
    method: str,
    path: str,
    host: str,
    headers: List[Tuple[str, str]],
    body: bytes,
) -> bytes:
    lines = [f"{method} {path} HTTP/1.1", f"Host: {host}"]
    has_connection = False
    has_content_length = False

    for name, value in headers:
        lower = name.lower()
        if lower == "connection":
            has_connection = True
        if lower == "content-length":
            has_content_length = True
        lines.append(f"{name}: {value}")

    if not has_connection:
        lines.append("Connection: close")
    if body and not has_content_length:
        lines.append(f"Content-Length: {len(body)}")

    return CRLF.join(line.encode("ascii") for line in lines) + CRLF + CRLF + body


def parse_header(header: str) -> Tuple[str, str]:
    if ":" not in header:
        raise argparse.ArgumentTypeError(
            f"header must be in NAME:VALUE form: {header!r}"
        )
    name, value = header.split(":", 1)
    return name.strip(), value.strip()


def parse_content_length(header_block: bytes) -> Optional[int]:
    for line in header_block.split(CRLF):
        lower = line.lower()
        if lower.startswith(b"content-length:"):
            value = line.split(b":", 1)[1].strip()
            return int(value)
    return None


def read_response(sock: ssl.SSLSocket) -> bytes:
    chunks = bytearray()
    while HEADER_BODY_SEPARATOR not in chunks:
        data = sock.recv(4096)
        if not data:
            raise RuntimeError("connection closed before headers completed")
        chunks.extend(data)

    header_end = chunks.index(HEADER_BODY_SEPARATOR)
    body_start = header_end + len(HEADER_BODY_SEPARATOR)
    header_block = bytes(chunks[:header_end])
    content_length = parse_content_length(header_block)

    if content_length is None:
        status_line = header_block.split(CRLF, 1)[0].decode("ascii", errors="replace")
        parts = status_line.split(" ")
        if len(parts) < 2:
            raise RuntimeError("invalid status line")
        status = int(parts[1])
        if 100 <= status < 200 or status in (204, 304):
            content_length = 0
        else:
            raise RuntimeError("response omitted Content-Length")

    while len(chunks) - body_start < content_length:
        data = sock.recv(4096)
        if not data:
            raise RuntimeError("connection closed before body completed")
        chunks.extend(data)

    return bytes(chunks[: body_start + content_length])


def build_context(
    ca_file: str,
    tls_version: ssl.TLSVersion,
    server_name: str,
) -> ssl.SSLContext:
    context = ssl.create_default_context(cafile=ca_file)
    context.check_hostname = False
    context.minimum_version = tls_version
    context.maximum_version = tls_version
    context.session_stats()
    _ = server_name
    return context


def percentile(sorted_values: List[float], p: float) -> float:
    if not sorted_values:
        return 0.0
    if len(sorted_values) == 1:
        return sorted_values[0]
    rank = p * (len(sorted_values) - 1)
    lower = int(rank)
    upper = min(lower + 1, len(sorted_values) - 1)
    weight = rank - lower
    return sorted_values[lower] * (1.0 - weight) + sorted_values[upper] * weight


def send_request(
    host: str,
    port: int,
    server_name: str,
    request: bytes,
    timeout: float,
    context: ssl.SSLContext,
    session: Optional[ssl.SSLSession],
) -> Tuple[int, bool, ssl.SSLSession]:
    raw = socket.create_connection((host, port), timeout=timeout)
    try:
        sock = context.wrap_socket(
            raw,
            server_hostname=server_name,
            session=session,
        )
        try:
            sock.settimeout(timeout)
            sock.sendall(request)
            response = read_response(sock)
            status_line = response.split(CRLF, 1)[0].decode("ascii", errors="replace")
            parts = status_line.split(" ")
            if len(parts) < 2:
                raise RuntimeError("invalid status line")
            return int(parts[1]), sock.session_reused, sock.session
        finally:
            sock.close()
    except Exception:
        raw.close()
        raise


def worker(
    host: str,
    port: int,
    server_name: str,
    request: bytes,
    timeout: float,
    iterations: int,
    latencies_ms: List[float],
    status_counts: Dict[int, int],
    errors: List[str],
    reused_counts: List[int],
    lock: threading.Lock,
    ca_file: str,
    tls_version: ssl.TLSVersion,
    reuse_mode: str,
) -> None:
    local_latencies: List[float] = []
    local_status_counts: Dict[int, int] = {}
    local_reused = 0

    shared_context: Optional[ssl.SSLContext] = None
    shared_session: Optional[ssl.SSLSession] = None
    if reuse_mode == "context-reuse":
        shared_context = build_context(ca_file, tls_version, server_name)

    try:
        for _ in range(iterations):
            context = shared_context or build_context(ca_file, tls_version, server_name)
            started = time.perf_counter()
            status, reused, next_session = send_request(
                host,
                port,
                server_name,
                request,
                timeout,
                context,
                shared_session,
            )
            ended = time.perf_counter()
            local_latencies.append((ended - started) * 1000.0)
            local_status_counts[status] = local_status_counts.get(status, 0) + 1
            if reused:
                local_reused += 1
            if reuse_mode == "context-reuse":
                shared_session = next_session
    except Exception as exc:  # pragma: no cover - script path
        with lock:
            errors.append(str(exc))
        return

    with lock:
        latencies_ms.extend(local_latencies)
        reused_counts.append(local_reused)
        for status, count in local_status_counts.items():
            status_counts[status] = status_counts.get(status, 0) + count


def run_benchmark(
    host: str,
    port: int,
    server_name: str,
    request: bytes,
    concurrency: int,
    requests: int,
    timeout: float,
    ca_file: str,
    tls_version: ssl.TLSVersion,
    reuse_mode: str,
) -> Dict[str, object]:
    threads = []
    latencies_ms: List[float] = []
    status_counts: Dict[int, int] = {}
    errors: List[str] = []
    reused_counts: List[int] = []
    lock = threading.Lock()

    base_iterations = requests // concurrency
    remainder = requests % concurrency

    started = time.perf_counter()
    for index in range(concurrency):
        iterations = base_iterations + (1 if index < remainder else 0)
        if iterations == 0:
            continue
        thread = threading.Thread(
            target=worker,
            args=(
                host,
                port,
                server_name,
                request,
                timeout,
                iterations,
                latencies_ms,
                status_counts,
                errors,
                reused_counts,
                lock,
                ca_file,
                tls_version,
                reuse_mode,
            ),
        )
        thread.start()
        threads.append(thread)

    for thread in threads:
        thread.join()
    ended = time.perf_counter()

    latencies_ms.sort()
    duration_s = ended - started
    completed = len(latencies_ms)

    return {
        "requests": requests,
        "completed_requests": completed,
        "concurrency": concurrency,
        "duration_seconds": duration_s,
        "throughput_rps": (completed / duration_s) if duration_s > 0 else 0.0,
        "latency_ms": {
            "mean": statistics.fmean(latencies_ms) if latencies_ms else 0.0,
            "median": statistics.median(latencies_ms) if latencies_ms else 0.0,
            "p95": percentile(latencies_ms, 0.95),
            "p99": percentile(latencies_ms, 0.99),
            "max": latencies_ms[-1] if latencies_ms else 0.0,
        },
        "status_counts": status_counts,
        "errors": errors,
        "session_reused_count": sum(reused_counts),
        "reuse_mode": reuse_mode,
    }


def tls_version_from_name(name: str) -> ssl.TLSVersion:
    normalized = name.upper()
    if normalized == "TLS13":
        return ssl.TLSVersion.TLSv1_3
    if normalized == "TLS12":
        return ssl.TLSVersion.TLSv1_2
    raise SystemExit("--tls-version must be TLS12 or TLS13")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8080)
    parser.add_argument("--server-name", default="localhost")
    parser.add_argument("--ca-file", required=True)
    parser.add_argument("--method", default="GET")
    parser.add_argument("--path", default="/")
    parser.add_argument("--header", action="append", default=[], type=parse_header)
    parser.add_argument("--body", default="")
    parser.add_argument("--body-file")
    parser.add_argument("--concurrency", type=int, default=16)
    parser.add_argument("--requests", type=int, default=1000)
    parser.add_argument("--warmup-requests", type=int, default=100)
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    parser.add_argument("--scenario", default="unnamed")
    parser.add_argument(
        "--reuse-mode",
        choices=["full", "context-reuse"],
        default="context-reuse",
    )
    parser.add_argument("--tls-version", default="TLS13")
    args = parser.parse_args()

    if args.concurrency <= 0:
        raise SystemExit("--concurrency must be positive")
    if args.requests <= 0:
        raise SystemExit("--requests must be positive")
    if args.warmup_requests < 0:
        raise SystemExit("--warmup-requests must be non-negative")

    if args.body_file is not None:
        with open(args.body_file, "rb") as body_file:
            body = body_file.read()
    else:
        body = args.body.encode("utf-8")

    request = build_request(args.method, args.path, args.server_name, args.header, body)
    tls_version = tls_version_from_name(args.tls_version)
    reuse_mode = "full" if args.reuse_mode == "full" else "context-reuse"

    if args.warmup_requests > 0:
        warmup_result = run_benchmark(
            args.host,
            args.port,
            args.server_name,
            request,
            min(args.concurrency, max(args.warmup_requests, 1)),
            args.warmup_requests,
            args.timeout_seconds,
            args.ca_file,
            tls_version,
            reuse_mode,
        )
        if warmup_result["errors"]:
            print(json.dumps({"scenario": args.scenario, "warmup": warmup_result}, indent=2))
            return 1

    result = run_benchmark(
        args.host,
        args.port,
        args.server_name,
        request,
        args.concurrency,
        args.requests,
        args.timeout_seconds,
        args.ca_file,
        tls_version,
        reuse_mode,
    )
    result["scenario"] = args.scenario
    print(json.dumps(result, indent=2, sort_keys=True))
    return 1 if result["errors"] else 0


if __name__ == "__main__":
    raise SystemExit(main())
