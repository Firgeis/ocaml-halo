#!/usr/bin/env python3

import argparse
import json
import os
import time
from typing import Dict, List, Optional, Tuple


def read_total_jiffies() -> int:
    with open("/proc/stat", "r", encoding="utf-8") as stat_file:
        first_line = stat_file.readline().strip()
    parts = first_line.split()
    if not parts or parts[0] != "cpu":
        raise RuntimeError("unexpected /proc/stat format")
    return sum(int(value) for value in parts[1:])


def read_process_times(pid: int) -> Tuple[int, int]:
    with open(f"/proc/{pid}/stat", "r", encoding="utf-8") as stat_file:
        content = stat_file.read()
    end_comm = content.rfind(")")
    fields = content[end_comm + 2 :].split()
    utime = int(fields[11])
    stime = int(fields[12])
    return utime, stime


def read_process_rss_kb(pid: int) -> Optional[int]:
    try:
        with open(f"/proc/{pid}/status", "r", encoding="utf-8") as status_file:
            for line in status_file:
                if line.startswith("VmRSS:"):
                    parts = line.split()
                    return int(parts[1])
    except FileNotFoundError:
        return None
    return None


def process_exists(pid: int) -> bool:
    return os.path.exists(f"/proc/{pid}")


def wait_for_process(pid: int, timeout_seconds: float) -> bool:
    deadline = time.time() + timeout_seconds
    while time.time() < deadline:
        if process_exists(pid):
            return True
        time.sleep(0.01)
    return process_exists(pid)


def sample_process(pid: int, interval_seconds: float) -> Dict[str, object]:
    cpu_count = os.cpu_count() or 1
    samples: List[Dict[str, float]] = []

    if not wait_for_process(pid, 5.0):
        return {
            "sample_interval_seconds": interval_seconds,
            "sample_count": 0,
            "cpu_percent_mean": 0.0,
            "cpu_percent_max": 0.0,
            "rss_kb_mean": 0.0,
            "rss_kb_max": 0.0,
            "samples": [],
        }

    prev_total = read_total_jiffies()
    prev_proc_user, prev_proc_system = read_process_times(pid)

    while process_exists(pid):
        time.sleep(interval_seconds)
        if not process_exists(pid):
            break

        try:
            total = read_total_jiffies()
            proc_user, proc_system = read_process_times(pid)
            rss_kb = read_process_rss_kb(pid)
        except FileNotFoundError:
            break

        total_delta = total - prev_total
        proc_delta = (proc_user + proc_system) - (prev_proc_user + prev_proc_system)
        cpu_percent = 0.0
        if total_delta > 0:
            cpu_percent = 100.0 * cpu_count * proc_delta / total_delta

        samples.append(
            {
                "cpu_percent": cpu_percent,
                "rss_kb": float(rss_kb or 0),
            }
        )

        prev_total = total
        prev_proc_user = proc_user
        prev_proc_system = proc_system

    cpu_values = [sample["cpu_percent"] for sample in samples]
    rss_values = [sample["rss_kb"] for sample in samples]

    return {
        "sample_interval_seconds": interval_seconds,
        "sample_count": len(samples),
        "cpu_percent_mean": (sum(cpu_values) / len(cpu_values)) if cpu_values else 0.0,
        "cpu_percent_max": max(cpu_values) if cpu_values else 0.0,
        "rss_kb_mean": (sum(rss_values) / len(rss_values)) if rss_values else 0.0,
        "rss_kb_max": max(rss_values) if rss_values else 0.0,
        "samples": samples,
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pid", type=int, required=True)
    parser.add_argument("--interval-seconds", type=float, default=0.1)
    parser.add_argument("--output", required=True)
    args = parser.parse_args()

    payload = sample_process(args.pid, args.interval_seconds)
    with open(args.output, "w", encoding="utf-8") as output_file:
        json.dump(payload, output_file, indent=2, sort_keys=True)
        output_file.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
