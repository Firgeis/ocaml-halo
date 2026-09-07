open Halo.Router

(* Measure Router.dispatch in isolation: route-table and request-value
   construction happen outside timed samples. The router is composed
   left-associatively to match ordinary [route_a <|> route_b <|> route_c]
   syntax. *)

type benchmark_case = {
  name : string;
  request : request;
  expected_status : int;
}

type sample = {
  nanoseconds_per_operation : float;
  bytes_per_operation : float;
}

let samples = ref 5
let route_counts = ref [100; 1_000; 10_000]
let iterations_override = ref None

let ok_response = text Status.ok "ok\n"

let request request_method raw_target =
  {
    request_method;
    raw_target;
    target = parse_request_target request_method raw_target;
    headers = [];
    body = "";
  }

let make_route index =
  let segment = string_of_int index in
  route
    Matcher.(
      let+ () =
        request_method_is GET
        *> path_segment "routes"
        *> path_segment segment
        *> path_end
      in
      fun _request -> ok_response)

let make_router count =
  if count <= 0 then invalid_arg "route count must be positive";
  let router = ref (make_route 0) in
  for index = 1 to count - 1 do
    router := !router <|> make_route index
  done;
  !router

let benchmark_cases count =
  let target index = Printf.sprintf "/routes/%d" index in
  [
    { name = "hit-first"; request = request GET (target 0); expected_status = 200 };
    {
      name = "hit-middle";
      request = request GET (target (count / 2));
      expected_status = 200;
    };
    {
      name = "hit-last";
      request = request GET (target (count - 1));
      expected_status = 200;
    };
    {
      name = "method-miss";
      request = request POST (target 0);
      expected_status = 405;
    };
    {
      name = "not-found";
      request = request GET "/missing";
      expected_status = 404;
    };
  ]

let status response = Response.status response |> Status.to_int

let dispatch_and_check router benchmark_case =
  let actual =
    dispatch (Sys.opaque_identity router)
      (Sys.opaque_identity benchmark_case.request)
    |> status
  in
  if actual <> benchmark_case.expected_status then
    failwith
      (Printf.sprintf "%s returned %d, expected %d" benchmark_case.name actual
         benchmark_case.expected_status);
  actual

let iterations_for count =
  match !iterations_override with
  | Some iterations -> iterations
  | None -> max 20 (500_000 / count)

let run_sample router benchmark_case iterations =
  Gc.full_major ();
  let allocated_before = Gc.allocated_bytes () in
  let started = Unix.gettimeofday () in
  let status_sum = ref 0 in
  for _ = 1 to iterations do
    status_sum := !status_sum + dispatch_and_check router benchmark_case
  done;
  let elapsed = Unix.gettimeofday () -. started in
  let allocated = Gc.allocated_bytes () -. allocated_before in
  Sys.opaque_identity !status_sum |> ignore;
  {
    nanoseconds_per_operation = elapsed *. 1e9 /. float iterations;
    bytes_per_operation = allocated /. float iterations;
  }

let median values =
  let sorted = Array.of_list values in
  Array.sort Float.compare sorted;
  let length = Array.length sorted in
  if length mod 2 = 1 then
    sorted.(length / 2)
  else
    (sorted.((length / 2) - 1) +. sorted.(length / 2)) /. 2.0

let measure router benchmark_case iterations =
  let warmup_iterations = max 5 (iterations / 10) in
  for _ = 1 to warmup_iterations do
    ignore (dispatch_and_check router benchmark_case)
  done;
  let results =
    List.init !samples (fun _ -> run_sample router benchmark_case iterations)
  in
  let times = List.map (fun sample -> sample.nanoseconds_per_operation) results in
  let allocations = List.map (fun sample -> sample.bytes_per_operation) results in
  median times, List.fold_left min Float.infinity times,
  List.fold_left max Float.neg_infinity times, median allocations

let parse_positive name value =
  match int_of_string_opt value with
  | Some parsed when parsed > 0 -> parsed
  | _ -> raise (Arg.Bad (name ^ " must be a positive integer"))

let set_samples value = samples := parse_positive "--samples" value

let set_iterations value =
  iterations_override := Some (parse_positive "--iterations" value)

let set_route_counts value =
  let counts =
    value
    |> String.split_on_char ','
    |> List.map (parse_positive "--route-counts")
  in
  match counts with
  | [] -> raise (Arg.Bad "--route-counts must not be empty")
  | _ -> route_counts := counts

let options =
  [
    ( "--samples",
      Arg.String set_samples,
      "N Number of measured samples per case (default: 5)" );
    ( "--iterations",
      Arg.String set_iterations,
      "N Operations per sample (default: adaptive to route count)" );
    ( "--route-counts",
      Arg.String set_route_counts,
      "N,... Route table sizes (default: 100,1000,10000)" );
  ]

let () =
  Arg.parse options
    (fun argument -> raise (Arg.Bad ("unexpected argument: " ^ argument)))
    "router_bench [options]";
  Printf.printf
    "routes  case          iterations   median ns/op      min      max       B/op\n";
  List.iter
    (fun count ->
      let router = make_router count in
      let iterations = iterations_for count in
      List.iter
        (fun benchmark_case ->
          let median_ns, min_ns, max_ns, median_bytes =
            measure router benchmark_case iterations
          in
          Printf.printf "%6d  %-12s  %10d  %13.0f  %7.0f  %7.0f  %9.0f\n%!"
            count benchmark_case.name iterations median_ns min_ns max_ns
            median_bytes)
        (benchmark_cases count))
    !route_counts
