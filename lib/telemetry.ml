type server_request = {
  request_id : int64;
  method_ : string;
  target : string;
  headers : (string * string) list;
  http_version : int * int;
  connection_info : Router.connection_info;
  is_early_data : bool;
  start_time_unix_ns : int64 option;
  start_monotonic_ns : int64;
}

type server_request_result = {
  route : string option;
  status_code : int option;
  outcome : Router.response_outcome option;
  end_monotonic_ns : int64 option;
}

module type Observer = sig
  type span

  val start_server_request : server_request -> span
  val with_span : span -> (unit -> 'a) -> 'a
  val finish_server_request : span -> server_request_result -> unit
end

type t =
  | Observer : (module Observer with type span = 'span) -> t
  | Noop

let of_observer observer =
  let module Observer = (val observer : Observer) in
  Observer (module Observer)

let noop = Noop

module Lifecycle = struct
  type span =
    | Span : (module Observer with type span = 'span) * 'span -> span
    | No_span

  let enabled = function Observer _ -> true | Noop -> false

  let start_server_request telemetry request =
    match telemetry with
    | Noop -> No_span
    | Observer observer ->
        let module Observer = (val observer) in
        (try Span (observer, Observer.start_server_request request) with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | _ -> No_span)

  let run_with_observer_scope with_scope run =
    let result = ref None in
    let callback_raised = ref false in
    try
      ignore
        (with_scope (fun () ->
             try
               let value = run () in
               result := Some value;
               value
             with exn ->
               callback_raised := true;
               raise exn));
      match !result with Some value -> value | None -> run ()
    with exn ->
      let cancellation =
        match exn with Eio.Cancel.Cancelled _ -> true | _ -> false
      in
      if !callback_raised || cancellation then
        let backtrace = Printexc.get_raw_backtrace () in
        Printexc.raise_with_backtrace exn backtrace
      else match !result with Some value -> value | None -> run ()

  let with_span span run =
    match span with
    | No_span -> run ()
    | Span (observer, observer_span) ->
        let module Observer = (val observer) in
        run_with_observer_scope
          (fun callback -> Observer.with_span observer_span callback)
          run

  let finish_server_request span result =
    match span with
    | No_span -> ()
    | Span (observer, observer_span) ->
        let module Observer = (val observer) in
        (try Observer.finish_server_request observer_span result
         with Eio.Cancel.Cancelled _ as exn -> raise exn
         | _ -> ())
end
