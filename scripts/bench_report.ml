(* Report formatting follows json.dumps(indent=2, sort_keys=True). *)
let rec format_json level json =
  let indent n = String.make (2 * n) ' ' in
  let collection open_char close_char entries render =
    match entries with
    | [] -> String.make 1 open_char ^ String.make 1 close_char
    | _ ->
        String.make 1 open_char ^ "\n"
        ^ String.concat ",\n" (List.map (fun entry -> indent (level + 1) ^ render entry) entries)
        ^ "\n" ^ indent level ^ String.make 1 close_char
  in
  match json with
  | `Assoc fields ->
      let fields = List.sort (fun (a, _) (b, _) -> String.compare a b) fields in
      collection '{' '}' fields (fun (key, value) ->
          Yojson.Safe.to_string (`String key) ^ ": " ^ format_json (level + 1) value)
  | `List values -> collection '[' ']' values (format_json (level + 1))
  | value -> Yojson.Safe.to_string value

let escape_non_ascii text =
  let output = Buffer.create (String.length text) in
  let rec loop i =
    if i < String.length text then (
      let byte j = Char.code text.[j] in
      let first = byte i in
      if first < 128 then (Buffer.add_char output text.[i]; loop (i + 1))
      else (
        let width, prefix =
          if first land 0xe0 = 0xc0 then 2, first land 0x1f
          else if first land 0xf0 = 0xe0 then 3, first land 0x0f
          else if first land 0xf8 = 0xf0 then 4, first land 0x07
          else failwith "invalid UTF-8 in JSON" in
        if i + width > String.length text then failwith "invalid UTF-8 in JSON";
        let code = ref prefix in
        for j = i + 1 to i + width - 1 do
          let next = byte j in
          if next land 0xc0 <> 0x80 then failwith "invalid UTF-8 in JSON";
          code := (!code lsl 6) lor (next land 0x3f)
        done;
        if !code <= 0xffff then Printf.bprintf output "\\u%04x" !code
        else (
          let code = !code - 0x10000 in
          Printf.bprintf output "\\u%04x\\u%04x" (0xd800 + (code lsr 10)) (0xdc00 + (code land 0x3ff)));
        loop (i + width))) in
  loop 0;
  Buffer.contents output

let write_report path payload =
  let formatted = escape_non_ascii (format_json 0 payload) ^ "\n" in
  let ch = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr ch) (fun () -> output_string ch formatted);
  print_string formatted

let read_lines path =
  let ch = open_in path in
  Fun.protect ~finally:(fun () -> close_in_noerr ch) (fun () ->
      let rec loop acc = match input_line ch with
        | line -> loop (line :: acc)
        | exception End_of_file -> List.rev acc
      in loop [])

let words line =
  String.split_on_char ' ' (String.map (function '\t' | '\r' -> ' ' | c -> c) line)
  |> List.filter (fun s -> s <> "")

let baseline args =
  let result, time_log, pidstat_log, process_path = args.(0), args.(1), args.(2), args.(3) in
  let metrics = ref [] in
  let set key value = metrics := (key, `String value) :: List.remove_assoc key !metrics in
  List.iter (fun line -> match String.index_opt line '=' with
      | None -> ()
      | Some i -> set (String.sub line 0 i) (String.sub line (i + 1) (String.length line - i - 1))) (read_lines time_log);
  if !metrics = [] then set "timing_tool" "unavailable";
  if Sys.file_exists pidstat_log then (
    let samples = List.filter_map (fun raw ->
        let line = String.trim raw in
        if line = "" || String.starts_with ~prefix:"Linux " line || String.starts_with ~prefix:"Average:" line then None
        else let parts = words line in
          if List.length parts < 8 || (List.mem "UID" parts && List.mem "%usr" parts) then None
          else match List.rev parts with
            | cpu :: _ :: rss :: _ -> (try Some (float_of_string cpu, float_of_string rss) with Failure _ -> None)
            | _ -> None) (read_lines pidstat_log) in
    if samples <> [] then (
      let count = List.length samples in
      let cs, cm, rs, rm = List.fold_left (fun (cs, cm, rs, rm) (c, r) ->
          (cs +. c, max cm c, rs +. r, max rm r)) (0., neg_infinity, 0., neg_infinity) samples in
      set "pidstat_cpu_percent_mean" (Printf.sprintf "%.2f" (cs /. float_of_int count));
      set "pidstat_cpu_percent_max" (Printf.sprintf "%.2f" cm);
      set "pidstat_sample_count" (string_of_int count);
      set "pidstat_rss_kb_mean" (Printf.sprintf "%.2f" (rs /. float_of_int count));
      set "pidstat_rss_kb_max" (Printf.sprintf "%.0f" rm)));
  if Sys.file_exists process_path then (
    let sample = Yojson.Safe.from_file process_path in
    let field key default = match Yojson.Safe.Util.member key sample with `Null -> default | x -> x in
    let number key = match field key (`Float 0.) with
      | `Int n -> float_of_int n | `Float x -> x | _ -> failwith ("invalid process sample: " ^ key) in
    let simple = function
      | `Int n -> string_of_int n
      | `Float f -> let s = string_of_float f in if String.ends_with ~suffix:"." s then s ^ "0" else s
      | _ -> failwith "invalid process sample" in
    set "process_sample_interval_seconds" (simple (field "sample_interval_seconds" (`Float 0.)));
    set "process_sample_count" (simple (field "sample_count" (`Int 0)));
    List.iter (fun (key, fmt) -> set ("process_" ^ key) (fmt (number key)))
      ["cpu_percent_mean", Printf.sprintf "%.2f"; "cpu_percent_max", Printf.sprintf "%.2f";
       "rss_kb_mean", Printf.sprintf "%.2f"; "rss_kb_max", Printf.sprintf "%.0f"]);
  let scenarios = List.init 6 (fun i -> Yojson.Safe.from_string args.(i + 4)) in
  write_report result (`Assoc ["server_metrics", `Assoc !metrics; "scenarios", `List scenarios])

let body_report args =
  let names = [|"accepted_large_body"; "accepted_large_body_chunked"; "oversized_large_body"; "oversized_large_body_chunked"|] in
  let scenarios = List.init 4 (fun i ->
      let code = int_of_string args.(6 + i) in
      let fields = match (try Some (Yojson.Safe.from_string args.(10 + i)) with Yojson.Json_error _ -> None) with
        | Some (`Assoc fields) -> fields
        | Some _ -> failwith "scenario result must be a JSON object"
        | None -> ["scenario", `String names.(i); "errors", `List [`String args.(10 + i)]] in
      let fields = ("exit_code", `Int code) :: List.remove_assoc "exit_code" fields in
      let fields = if code <> 0 then ("failed", `Bool true) :: List.remove_assoc "failed" fields else fields in
      `Assoc fields) in
  let config = `Assoc ["max_body_size", `Int (int_of_string args.(2));
    "accepted_body_size", `Int (int_of_string args.(3)); "oversized_body_size", `Int (int_of_string args.(4));
    "chunk_size", `Int (int_of_string args.(5)); "keep_alive", `Bool (args.(14) = "1")] in
  write_report args.(0) (`Assoc ["config", config; "process_sample", Yojson.Safe.from_file args.(1); "scenarios", `List scenarios]);
  let failed = List.exists (fun scenario -> match Yojson.Safe.Util.member "failed" scenario with
      | `Null | `Bool false -> false | `Int 0 -> false | `String "" -> false | `List [] | `Assoc [] -> false | _ -> true) scenarios in
  if failed then exit 1

let write_body path size byte =
  let size = Int64.of_string size in
  if size < 0L then invalid_arg "body size must be non-negative";
  let block = Bytes.make 65536 byte in
  let ch = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr ch) (fun () ->
      let rec loop remaining =
        if remaining > 0L then (
          let count = Int64.to_int (Int64.min remaining (Int64.of_int (Bytes.length block))) in
          output ch block 0 count;
          loop (Int64.sub remaining (Int64.of_int count))) in
      loop size)

let () =
  let usage () = prerr_endline "usage: bench_report (baseline|body-report|body-files) arguments..."; exit 2 in
  if Array.length Sys.argv < 2 then usage ();
  let args = Array.sub Sys.argv 2 (Array.length Sys.argv - 2) in
  match Sys.argv.(1) with
  | "baseline" when Array.length args = 10 -> baseline args
  | "body-report" when Array.length args = 15 -> body_report args
  | "body-files" when Array.length args = 4 -> write_body args.(0) args.(1) 'a'; write_body args.(2) args.(3) 'b'
  | _ -> usage ()
