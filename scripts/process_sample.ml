(* Proc files report length zero, so read them line by line instead. *)
let read_lines path =
  let ch = open_in path in
  Fun.protect ~finally:(fun () -> close_in_noerr ch) (fun () ->
      let rec loop acc =
        match input_line ch with
        | line -> loop (line :: acc)
        | exception End_of_file -> List.rev acc
      in
      loop [])

let words s =
  String.split_on_char ' ' (String.map (fun c -> if c = '\t' then ' ' else c) s)
  |> List.filter (fun s -> s <> "")

let total_jiffies () =
  match read_lines "/proc/stat" with
  | line :: _ -> (
      match words line with
      | "cpu" :: values -> List.fold_left (fun n v -> Int64.add n (Int64.of_string v)) 0L values
      | _ -> failwith "unexpected /proc/stat format")
  | [] -> failwith "empty /proc/stat"

let process_times pid =
  let line = List.hd (read_lines (Printf.sprintf "/proc/%d/stat" pid)) in
  let end_comm = String.rindex line ')' in
  let fields = words (String.sub line (end_comm + 2) (String.length line - end_comm - 2)) in
  Int64.add (Int64.of_string (List.nth fields 11)) (Int64.of_string (List.nth fields 12))

let rss pid =
  match read_lines (Printf.sprintf "/proc/%d/status" pid)
        |> List.find_opt (fun s -> String.starts_with ~prefix:"VmRSS:" s) with
  | Some line -> (match words line with _ :: value :: _ -> float_of_string value | _ -> 0.)
  | None -> 0.

let cpu_count () =
  (* Linux's possible CPU mask corresponds to sysconf(_SC_NPROCESSORS_CONF),
     which is what Python's os.cpu_count uses. *)
  try
    let mask = List.hd (read_lines "/sys/devices/system/cpu/possible") in
    let count = String.split_on_char ',' mask |> List.fold_left (fun n range ->
        match String.split_on_char '-' (String.trim range) with
        | [a] -> ignore (int_of_string a); n + 1
        | [a; b] -> n + int_of_string b - int_of_string a + 1
        | _ -> failwith "invalid CPU mask") 0 in
    max 1 count
  with _ -> 1

let exists pid = Sys.file_exists (Printf.sprintf "/proc/%d" pid)
let sleep seconds = ignore (Unix.select [] [] [] seconds)
let wait_for_process pid =
  let deadline = Unix.gettimeofday () +. 5. in
  while not (exists pid) && Unix.gettimeofday () < deadline do sleep 0.01 done;
  exists pid

let sample pid interval =
  if not (wait_for_process pid) then []
  else
    try
      let prev_total = ref (total_jiffies ()) and prev_proc = ref (process_times pid) in
      let cpus = float_of_int (cpu_count ()) in
      let samples = ref [] in
      (try
         while exists pid do
           sleep interval;
           if not (exists pid) then raise Exit;
           let total = total_jiffies () and proc = process_times pid and rss = rss pid in
           let total_delta = Int64.sub total !prev_total in
           let cpu = if total_delta > 0L then
               100. *. cpus *. Int64.to_float (Int64.sub proc !prev_proc) /. Int64.to_float total_delta
             else 0. in
           samples := (cpu, rss) :: !samples;
           prev_total := total; prev_proc := proc
         done
       with Sys_error _ | Unix.Unix_error (Unix.ENOENT, _, _) | Exit -> ());
      List.rev !samples
    with Sys_error _ | Unix.Unix_error (Unix.ENOENT, _, _) -> []

let () =
  let pid = ref None and interval = ref 0.1 and output = ref None in
  let specs = [
    "--pid", Arg.Int (fun n -> pid := Some n), "PID process to sample";
    "--interval-seconds", Arg.Set_float interval, "SECONDS sampling interval";
    "--output", Arg.String (fun s -> output := Some s), "FILE JSON output";
  ] in
  Arg.parse specs (fun s -> raise (Arg.Bad ("unexpected argument " ^ s))) "process_sample --pid PID --output FILE";
  let pid = match !pid with Some n when n > 0 -> n | _ -> prerr_endline "--pid must be positive and is required"; exit 2 in
  let output = match !output with Some s -> s | None -> prerr_endline "--output is required"; exit 2 in
  if not (Float.is_finite !interval) || !interval < 0. then (prerr_endline "--interval-seconds must be non-negative and finite"; exit 2);
  let samples = sample pid !interval in
  let count = List.length samples in
  let cpu_sum, cpu_max, rss_sum, rss_max = List.fold_left (fun (cs, cm, rs, rm) (c, r) ->
      (cs +. c, max cm c, rs +. r, max rm r)) (0., 0., 0., 0.) samples in
  let mean n = if count = 0 then 0. else n /. float_of_int count in
  let ch = open_out output in
  Fun.protect ~finally:(fun () -> close_out_noerr ch) (fun () ->
      Printf.fprintf ch "{\n  \"sample_interval_seconds\": %.17g,\n  \"sample_count\": %d,\n  \"cpu_percent_mean\": %.17g,\n  \"cpu_percent_max\": %.17g,\n  \"rss_kb_mean\": %.17g,\n  \"rss_kb_max\": %.17g,\n  \"samples\": [" !interval count (mean cpu_sum) cpu_max (mean rss_sum) rss_max;
      List.iteri (fun i (cpu, rss) -> Printf.fprintf ch "%s\n    {\"cpu_percent\": %.17g, \"rss_kb\": %.17g}" (if i = 0 then "" else ",") cpu rss) samples;
      Printf.fprintf ch "\n  ]\n}\n")
