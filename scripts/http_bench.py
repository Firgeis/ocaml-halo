#!/usr/bin/env python3

import argparse
import json
import socket
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
    chunked: bool,
    chunk_size: int,
) -> bytes:
    lines = [f"{method} {path} HTTP/1.1", f"Host: {host}"]
    has_connection = False
    has_content_length = False
    has_transfer_encoding = False
    for name, value in headers:
        lower = name.lower()
        if lower == "connection":
            has_connection = True
        if lower == "content-length":
            has_content_length = True
        if lower == "transfer-encoding":
            has_transfer_encoding = True
        lines.append(f"{name}: {value}")

    if not has_connection:
        lines.append("Connection: close")
    if chunked:
        if has_content_length:
            raise ValueError("chunked requests cannot include Content-Length")
        if not has_transfer_encoding:
            lines.append("Transfer-Encoding: chunked")
        encoded_body = encode_chunked_body(body, chunk_size)
    else:
        encoded_body = body
    if body and not chunked and not has_content_length:
        lines.append(f"Content-Length: {len(body)}")

    return CRLF.join(line.encode("ascii") for line in lines) + CRLF + CRLF + encoded_body


def encode_chunked_body(body: bytes, chunk_size: int) -> bytes:
    if chunk_size <= 0:
        raise ValueError("chunk_size must be positive")
    chunks = bytearray()
    for offset in range(0, len(body), chunk_size):
        chunk = body[offset : offset + chunk_size]
        chunks.extend(f"{len(chunk):X}".encode("ascii"))
        chunks.extend(CRLF)
        chunks.extend(chunk)
        chunks.extend(CRLF)
    chunks.extend(b"0\r\n\r\n")
    return bytes(chunks)


def parse_content_length(header_block: bytes) -> Optional[int]:
    for line in header_block.split(CRLF):
        lower = line.lower()
        if lower.startswith(b"content-length:"):
            value = line.split(b":", 1)[1].strip()
            return int(value)
    return None


def read_response(sock: socket.socket) -> bytes:
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


def send_request(host: str, port: int, request: bytes, timeout: float) -> int:
    with socket.create_connection((host, port), timeout=timeout) as sock:
        sock.settimeout(timeout)
        sock.sendall(request)
        response = read_response(sock)
    status_line = response.split(CRLF, 1)[0].decode("ascii", errors="replace")
    parts = status_line.split(" ")
    if len(parts) < 2:
        raise RuntimeError("invalid status line")
    return int(parts[1])


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


def worker(
    host: str,
    port: int,
    request: bytes,
    timeout: float,
    iterations: int,
    latencies_ms: List[float],
    status_counts: Dict[int, int],
    errors: List[str],
    lock: threading.Lock,
) -> None:
    local_latencies: List[float] = []
    local_status_counts: Dict[int, int] = {}

    try:
        for _ in range(iterations):
            started = time.perf_counter()
            status = send_request(host, port, request, timeout)
            ended = time.perf_counter()
            local_latencies.append((ended - started) * 1000.0)
            local_status_counts[status] = local_status_counts.get(status, 0) + 1
    except Exception as exc:  # pragma: no cover - script path
        with lock:
            errors.append(str(exc))
        return

    with lock:
        latencies_ms.extend(local_latencies)
        for status, count in local_status_counts.items():
            status_counts[status] = status_counts.get(status, 0) + count


def run_benchmark(
    host: str,
    port: int,
    request: bytes,
    concurrency: int,
    requests: int,
    timeout: float,
) -> Dict[str, object]:
    threads = []
    latencies_ms: List[float] = []
    status_counts: Dict[int, int] = {}
    errors: List[str] = []
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
                request,
                timeout,
                iterations,
                latencies_ms,
                status_counts,
                errors,
                lock,
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
    }


def parse_header(header: str) -> Tuple[str, str]:
    if ":" not in header:
        raise argparse.ArgumentTypeError(
            f"header must be in NAME:VALUE form: {header!r}"
        )
    name, value = header.split(":", 1)
    return name.strip(), value.strip()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--host", default="127.0.0.1")
    parser.add_argument("--port", type=int, default=8080)
    parser.add_argument("--method", default="GET")
    parser.add_argument("--path", default="/")
    parser.add_argument("--header", action="append", default=[], type=parse_header)
    parser.add_argument("--body", default="")
    parser.add_argument("--body-file")
    parser.add_argument("--concurrency", type=int, default=32)
    parser.add_argument("--requests", type=int, default=5000)
    parser.add_argument("--warmup-requests", type=int, default=200)
    parser.add_argument("--timeout-seconds", type=float, default=5.0)
    parser.add_argument("--scenario", default="unnamed")
    parser.add_argument("--expect-status", action="append", default=[], type=int)
    parser.add_argument("--chunked", action="store_true")
    parser.add_argument("--chunk-size", type=int, default=8192)
    args = parser.parse_args()

    if args.concurrency <= 0:
        raise SystemExit("--concurrency must be positive")
    if args.requests <= 0:
        raise SystemExit("--requests must be positive")
    if args.warmup_requests < 0:
        raise SystemExit("--warmup-requests must be non-negative")
    if args.chunk_size <= 0:
        raise SystemExit("--chunk-size must be positive")

    if args.body_file is not None:
        with open(args.body_file, "rb") as body_file:
            body = body_file.read()
    else:
        body = args.body.encode("utf-8")

    request = build_request(
        args.method,
        args.path,
        args.host,
        args.header,
        body,
        args.chunked,
        args.chunk_size,
    )

    if args.warmup_requests > 0:
        warmup_result = run_benchmark(
            args.host,
            args.port,
            request,
            min(args.concurrency, max(args.warmup_requests, 1)),
            args.warmup_requests,
            args.timeout_seconds,
        )
        if warmup_result["errors"]:
            print(json.dumps({"scenario": args.scenario, "warmup": warmup_result}, indent=2))
            return 1

    result = run_benchmark(
        args.host,
        args.port,
        request,
        args.concurrency,
        args.requests,
        args.timeout_seconds,
    )
    result["scenario"] = args.scenario
    result["request_bytes"] = len(request)
    result["payload_bytes"] = len(body)
    result["method"] = args.method
    result["path"] = args.path
    result["chunked"] = args.chunked
    if args.chunked:
        result["chunk_size"] = args.chunk_size
    if args.expect_status:
        allowed_statuses = set(args.expect_status)
        unexpected_status_counts = {
            status: count
            for status, count in result["status_counts"].items()
            if status not in allowed_statuses
        }
        result["expected_statuses"] = sorted(allowed_statuses)
        result["unexpected_status_counts"] = unexpected_status_counts
    print(json.dumps(result, indent=2, sort_keys=True))
    if result["errors"]:
        return 1
    if args.expect_status and result["unexpected_status_counts"]:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
