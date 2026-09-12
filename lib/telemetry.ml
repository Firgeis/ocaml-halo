module Trace_id = struct
  type t = int64 * int64

  let of_hex value =
    if
      String.length value = 32
      && Ascii_string.is_nonzero_lower_hex value ~offset:0 ~length:32
    then
      match
        ( Int64_encoding.of_lower_hex value ~offset:0,
          Int64_encoding.of_lower_hex value ~offset:16 )
      with
      | Some high, Some low -> Some (high, low)
      | _ -> None
    else None

  let of_bytes value =
    if String.length value <> 16 then None
    else
      match
        ( Int64_encoding.of_big_endian value ~offset:0,
          Int64_encoding.of_big_endian value ~offset:8 )
      with
      | Some high, Some low ->
          if Int64.equal high 0L && Int64.equal low 0L then None
          else Some (high, low)
      | _ -> None

  let to_hex (high, low) = Printf.sprintf "%016Lx%016Lx" high low

  let to_bytes (high, low) =
    Int64_encoding.to_big_endian high ^ Int64_encoding.to_big_endian low

  let equal (left_high, left_low) (right_high, right_low) =
    Int64.equal left_high right_high && Int64.equal left_low right_low

  let compare (left_high, left_low) (right_high, right_low) =
    let high = Int64.unsigned_compare left_high right_high in
    if high <> 0 then high else Int64.unsigned_compare left_low right_low
end

module Span_id = struct
  type t = int64

  let of_hex value =
    if
      String.length value = 16
      && Ascii_string.is_nonzero_lower_hex value ~offset:0 ~length:16
    then Int64_encoding.of_lower_hex value ~offset:0
    else None

  let of_bytes value =
    if String.length value <> 8 then None
    else
      match Int64_encoding.of_big_endian value ~offset:0 with
      | Some id when not (Int64.equal id 0L) -> Some id
      | _ -> None

  let to_hex value = Printf.sprintf "%016Lx" value
  let to_bytes = Int64_encoding.to_big_endian
  let equal = Int64.equal
  let compare = Int64.unsigned_compare
end

external secure_random_bytes : int -> string
  = "ocaml_halo_telemetry_random_bytes"

module Id_generator = struct
  type t = {
    trace_id : unit -> Trace_id.t;
    span_id : unit -> Span_id.t;
    trace_and_span_id : unit -> Trace_id.t * Span_id.t;
    random : bool;
  }

  let create ?(random = false) ~trace_id ~span_id () =
    {
      trace_id;
      span_id;
      trace_and_span_id = (fun () -> (trace_id (), span_id ()));
      random;
    }

  let rec secure_trace_id () =
    let bytes = secure_random_bytes 16 in
    match Trace_id.of_bytes bytes with
    | Some id -> id
    | None -> secure_trace_id ()

  let rec secure_span_id () =
    let bytes = secure_random_bytes 8 in
    match Span_id.of_bytes bytes with
    | Some id -> id
    | None -> secure_span_id ()

  let rec secure_trace_and_span_id () =
    let bytes = secure_random_bytes 24 in
    let trace_id = String.sub bytes 0 16 |> Trace_id.of_bytes in
    let span_id = String.sub bytes 16 8 |> Span_id.of_bytes in
    match (trace_id, span_id) with
    | Some trace_id, Some span_id -> (trace_id, span_id)
    | _ -> secure_trace_and_span_id ()

  let secure =
    {
      trace_id = secure_trace_id;
      span_id = secure_span_id;
      trace_and_span_id = secure_trace_and_span_id;
      random = true;
    }

  let trace_id generator = generator.trace_id ()
  let span_id generator = generator.span_id ()
  let trace_and_span_id generator = generator.trace_and_span_id ()
end

module Trace_state = struct
  type t = (string * string) list

  let is_tracestate_key_character character =
    Ascii_string.is_lowercase_letter character
    || Ascii_string.is_digit character
    || match character with '_' | '-' | '*' | '/' -> true | _ -> false

  let is_valid_simple_key key =
    let length = String.length key in
    length >= 1 && length <= 256
    && Ascii_string.is_lowercase_letter key.[0]
    && String.for_all is_tracestate_key_character key

  let is_valid_tenant_id tenant_id =
    let length = String.length tenant_id in
    length >= 1 && length <= 241
    && (Ascii_string.is_lowercase_letter tenant_id.[0]
       || Ascii_string.is_digit tenant_id.[0])
    && String.for_all is_tracestate_key_character tenant_id

  let is_valid_system_id system_id =
    let length = String.length system_id in
    length >= 1 && length <= 14
    && Ascii_string.is_lowercase_letter system_id.[0]
    && String.for_all is_tracestate_key_character system_id

  let is_valid_tracestate_key key =
    match String.index_opt key '@' with
    | None -> is_valid_simple_key key
    | Some separator ->
        let length = String.length key in
        length <= 256
        && (not (String.contains_from key (separator + 1) '@'))
        && is_valid_tenant_id (String.sub key 0 separator)
        && is_valid_system_id
             (String.sub key (separator + 1) (length - separator - 1))

  let is_valid_tracestate_value value =
    let length = String.length value in
    let is_allowed character =
      Ascii_string.is_printable character
      && character <> ',' && character <> '='
    in
    length >= 1 && length <= 256
    && String.for_all is_allowed value
    && value.[length - 1] <> ' '

  let parse_tracestate_member member =
    let member = Ascii_string.trim_optional_whitespace member in
    match String.index_opt member '=' with
    | None -> None
    | Some separator ->
        let key =
          String.sub member 0 separator |> Ascii_string.trim_optional_whitespace
        in
        let value =
          String.sub member (separator + 1)
            (String.length member - separator - 1)
          |> Ascii_string.trim_optional_whitespace
        in
        if is_valid_tracestate_key key && is_valid_tracestate_value value then
          Some (key, value)
        else None

  let serialize tracestate =
    tracestate
    |> List.map (fun (key, value) -> key ^ "=" ^ value)
    |> String.concat ","

  let validate tracestate =
    let rec loop seen count = function
      | [] -> count <= 32 && String.length (serialize tracestate) <= 512
      | (key, value) :: rest ->
          if
            count = 32 || List.mem key seen
            || (not (is_valid_tracestate_key key))
            || not (is_valid_tracestate_value value)
          then false
          else loop (key :: seen) (count + 1) rest
    in
    loop [] 0 tracestate

  let parse_header_values values =
    let combined = String.concat "," values in
    if String.length combined > 512 then []
    else
      let rec parse seen count parsed = function
        | [] -> List.rev parsed
        | member :: rest -> (
            match parse_tracestate_member member with
            | Some (key, value) when count < 32 && not (List.mem key seen) ->
                parse (key :: seen) (count + 1) ((key, value) :: parsed) rest
            | _ -> [])
      in
      parse [] 0 [] (String.split_on_char ',' combined)

  let empty = []
  let of_list entries = if validate entries then Some entries else None
  let to_list state = state
  let find key state = List.assoc_opt key state

  let remove key state =
    List.filter (fun (candidate, _) -> candidate <> key) state

  let add key value state =
    if not (is_valid_tracestate_key key && is_valid_tracestate_value value) then
      None
    else
      let rec take count = function
        | [] -> []
        | _ when count = 0 -> []
        | entry :: rest -> entry :: take (count - 1) rest
      in
      let updated = (key, value) :: remove key state |> take 32 in
      if String.length (serialize updated) <= 512 then Some updated else None
end

module Trace_context = struct
  type t = {
    trace_id : Trace_id.t;
    span_id : Span_id.t;
    trace_flags : int;
    trace_state : Trace_state.t;
    remote : bool;
  }

  let parse_traceparent value =
    let length = String.length value in
    if
      length < 55
      || value.[2] <> '-'
      || value.[35] <> '-'
      || value.[52] <> '-'
      || (not (Ascii_string.is_nonzero_lower_hex value ~offset:3 ~length:32))
      || not (Ascii_string.is_nonzero_lower_hex value ~offset:36 ~length:16)
    then None
    else
      match
        ( Ascii_string.lower_hex_byte value ~offset:0,
          Ascii_string.lower_hex_byte value ~offset:53 )
      with
      | Some version, Some trace_flags
        when version <> 0xff
             &&
             if version = 0 then length = 55
             else length = 55 || (length > 56 && value.[55] = '-') -> (
          let trace_id = String.sub value 3 32 |> Trace_id.of_hex in
          let span_id = String.sub value 36 16 |> Span_id.of_hex in
          match (trace_id, span_id) with
          | Some trace_id, Some span_id -> Some (trace_id, span_id, trace_flags)
          | _ -> None)
      | _ -> None

  let create_from_validated_parts ~trace_id ~span_id ~trace_flags ~trace_state
      ~remote =
    { trace_id; span_id; trace_flags; trace_state; remote }

  let make ~trace_id ~span_id ~trace_flags ~tracestate =
    match
      ( Trace_id.of_hex trace_id,
        Span_id.of_hex span_id,
        Trace_state.of_list tracestate )
    with
    | Some trace_id, Some span_id, Some trace_state
      when trace_flags >= 0 && trace_flags <= 0xff ->
        Some
          (create_from_validated_parts ~trace_id ~span_id ~trace_flags
             ~trace_state ~remote:false)
    | _ -> None

  let of_headers headers =
    let traceparents, tracestates =
      List.fold_left
        (fun (traceparents, tracestates) (name, value) ->
          match String.lowercase_ascii name with
          | "traceparent" ->
              ( Ascii_string.trim_optional_whitespace value :: traceparents,
                tracestates )
          | "tracestate" -> (traceparents, value :: tracestates)
          | _ -> (traceparents, tracestates))
        ([], []) headers
    in
    match traceparents with
    | [ traceparent ] -> (
        match parse_traceparent traceparent with
        | None -> None
        | Some (trace_id, span_id, trace_flags) ->
            let trace_state =
              Trace_state.parse_header_values (List.rev tracestates)
            in
            Some { trace_id; span_id; trace_flags; trace_state; remote = true })
    | _ -> None

  let trace_id context = Trace_id.to_hex context.trace_id
  let span_id context = Span_id.to_hex context.span_id
  let trace_id_value context = context.trace_id
  let span_id_value context = context.span_id
  let trace_flags context = context.trace_flags
  let sampled context = context.trace_flags land 1 <> 0
  let is_remote context = context.remote
  let tracestate context = Trace_state.to_list context.trace_state
  let trace_state context = context.trace_state

  let traceparent context =
    Printf.sprintf "00-%s-%s-%02x"
      (Trace_id.to_hex context.trace_id)
      (Span_id.to_hex context.span_id)
      context.trace_flags

  let tracestate_header context =
    match Trace_state.to_list context.trace_state with
    | [] -> None
    | _ -> Some (Trace_state.serialize context.trace_state)

  let to_headers context =
    match tracestate_header context with
    | None -> [ ("traceparent", traceparent context) ]
    | Some tracestate ->
        [ ("traceparent", traceparent context); ("tracestate", tracestate) ]
end

type server_request = {
  request_id : int64;
  method_ : string;
  target : string;
  headers : (string * string) list;
  remote_parent : Trace_context.t option;
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

type t = Observer : (module Observer with type span = 'span) -> t | Noop

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
    | Observer observer -> (
        let module Observer = (val observer) in
        try Span (observer, Observer.start_server_request request) with
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
    with exn -> (
      let cancellation =
        match exn with Eio.Cancel.Cancelled _ -> true | _ -> false
      in
      if !callback_raised || cancellation then
        let backtrace = Printexc.get_raw_backtrace () in
        Printexc.raise_with_backtrace exn backtrace
      else match !result with Some value -> value | None -> run ())

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
    | Span (observer, observer_span) -> (
        let module Observer = (val observer) in
        try Observer.finish_server_request observer_span result with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | _ -> ())
end

module Attribute = struct
  type value =
    | String of string
    | Bool of bool
    | Int of int64
    | Float of float
    | Bytes of string
    | Array of value list

  type t = string * value
end

module Span_kind = struct
  type t = Internal | Server | Client | Producer | Consumer
end

module Sampler = struct
  type decision = Drop | Record_only | Record_and_sample

  type input = {
    parent : Trace_context.t option;
    trace_id : Trace_id.t;
    name : string;
    kind : Span_kind.t;
    attributes : Attribute.t list;
  }

  type t = input -> decision

  let create sample = sample
  let always_on _ = Record_and_sample
  let always_off _ = Drop

  let parent_based ?(root = always_on) () input =
    match input.parent with
    | None -> root input
    | Some parent ->
        if Trace_context.sampled parent then Record_and_sample else Drop
end

let unix_time_ns () = Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.0)

external monotonic_time_ns : unit -> int64
  = "ocaml_halo_telemetry_monotonic_time_ns"

module Span = struct
  type status = Unset | Ok | Error of string option

  type event = {
    name : string;
    timestamp_unix_ns : int64;
    attributes : Attribute.t list;
  }

  type data = {
    name : string;
    context : Trace_context.t;
    parent_span_id : Span_id.t option;
    kind : Span_kind.t;
    start_time_unix_ns : int64;
    end_time_unix_ns : int64;
    attributes : Attribute.t list;
    events : event list;
    status : status;
    instrumentation_scope : string;
    instrumentation_scope_version : string option;
  }

  type state = {
    name : string;
    attributes : Attribute.t list;
    events : event list;
    status : status;
    finished : bool;
  }

  type t = {
    context : Trace_context.t;
    parent_span_id : Span_id.t option;
    kind : Span_kind.t;
    start_time_unix_ns : int64;
    start_monotonic_ns : int64;
    instrumentation_scope : string;
    instrumentation_scope_version : string option;
    recording : bool;
    on_finish : data -> unit;
    state : state Atomic.t;
  }

  let context span = span.context
  let is_recording span = span.recording
  let name span = (Atomic.get span.state).name

  let rec update span change =
    let current = Atomic.get span.state in
    if current.finished || not span.recording then ()
    else
      let updated = change current in
      if Atomic.compare_and_set span.state current updated then ()
      else update span change

  let set_name span name = update span (fun state -> { state with name })

  let set_attribute span key value =
    update span (fun state ->
        let attributes =
          (key, value)
          :: List.filter
               (fun (candidate, _) -> candidate <> key)
               state.attributes
        in
        { state with attributes })

  let add_attributes span attributes =
    List.iter (fun (key, value) -> set_attribute span key value) attributes

  let add_event ?timestamp_unix_ns ?(attributes = []) span name =
    let timestamp_unix_ns =
      Option.value timestamp_unix_ns ~default:(unix_time_ns ())
    in
    update span (fun state ->
        {
          state with
          events = { name; timestamp_unix_ns; attributes } :: state.events;
        })

  let set_status span status = update span (fun state -> { state with status })

  let finish_at ?end_time_unix_ns ?end_monotonic_ns span =
    let rec commit () =
      let current = Atomic.get span.state in
      if current.finished then None
      else
        let updated = { current with finished = true } in
        if Atomic.compare_and_set span.state current updated then Some current
        else commit ()
    in
    match commit () with
    | None -> ()
    | Some final_state ->
        if span.recording then
          let end_time_unix_ns =
            match (end_time_unix_ns, end_monotonic_ns) with
            | Some timestamp, _ -> timestamp
            | None, Some end_monotonic_ns ->
                Int64.add span.start_time_unix_ns
                  (Int64.max 0L
                     (Int64.sub end_monotonic_ns span.start_monotonic_ns))
            | None, None ->
                Int64.add span.start_time_unix_ns
                  (Int64.max 0L
                     (Int64.sub (monotonic_time_ns ()) span.start_monotonic_ns))
          in
          let finished =
            {
              name = final_state.name;
              context = span.context;
              parent_span_id = span.parent_span_id;
              kind = span.kind;
              start_time_unix_ns = span.start_time_unix_ns;
              end_time_unix_ns;
              attributes = List.rev final_state.attributes;
              events = List.rev final_state.events;
              status = final_state.status;
              instrumentation_scope = span.instrumentation_scope;
              instrumentation_scope_version = span.instrumentation_scope_version;
            }
          in
          span.on_finish finished

  let finish ?end_time_unix_ns span = finish_at ?end_time_unix_ns span

  let create ~context ~parent_span_id ~kind ~start_time_unix_ns
      ~start_monotonic_ns ~instrumentation_scope ~instrumentation_scope_version
      ~recording ~on_finish ~name ~attributes =
    {
      context;
      parent_span_id;
      kind;
      start_time_unix_ns;
      start_monotonic_ns;
      instrumentation_scope;
      instrumentation_scope_version;
      recording;
      on_finish;
      state =
        Atomic.make
          {
            name;
            attributes = List.rev attributes;
            events = [];
            status = Unset;
            finished = false;
          };
    }
end

module Context = struct
  let current_span_key = Eio.Fiber.create_key ()
  let current_span () = Eio.Fiber.get current_span_key
  let current () = Option.map Span.context (current_span ())
  let with_span span run = Eio.Fiber.with_binding current_span_key span run
end

module Exporter = struct
  type t = {
    export : Span.data list -> (unit, string) result;
    force_flush : unit -> (unit, string) result;
    shutdown : unit -> (unit, string) result;
  }

  let create ?(force_flush = fun () -> Ok ()) ?(shutdown = fun () -> Ok ())
      ~export () =
    { export; force_flush; shutdown }

  let protect operation =
    try operation () with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn -> Error (Printexc.to_string exn)

  let export exporter spans = protect (fun () -> exporter.export spans)
  let force_flush exporter = protect exporter.force_flush
  let shutdown exporter = protect exporter.shutdown
end

module In_memory_exporter = struct
  type state = Open of Span.data list | Closed of Span.data list
  type t = { state : state Atomic.t }

  let create () = { state = Atomic.make (Open []) }

  let rec add t spans =
    let current = Atomic.get t.state in
    match current with
    | Closed _ -> Error "in-memory exporter is shut down"
    | Open stored ->
        let updated = Open (List.rev_append spans stored) in
        if Atomic.compare_and_set t.state current updated then Ok ()
        else add t spans

  let rec shutdown t =
    let current = Atomic.get t.state in
    match current with
    | Closed _ -> Ok ()
    | Open spans ->
        if Atomic.compare_and_set t.state current (Closed spans) then Ok ()
        else shutdown t

  let exporter t =
    Exporter.create ~export:(add t) ~shutdown:(fun () -> shutdown t) ()

  let finished_spans t =
    match Atomic.get t.state with Open spans | Closed spans -> List.rev spans

  let rec clear t =
    let current = Atomic.get t.state in
    match current with
    | Closed _ -> ()
    | Open _ ->
        if not (Atomic.compare_and_set t.state current (Open [])) then clear t
end

type span_processor = {
  on_end : Span.data -> unit;
  force_flush : unit -> (unit, string) result;
  shutdown : unit -> (unit, string) result;
  dropped_spans : unit -> int;
  last_error : unit -> string option;
}

let record_error target message = Atomic.set target (Some message)

module Processor = struct
  type t = span_processor

  let simple exporter =
    let stopped = Atomic.make false in
    let last_error = Atomic.make None in
    let run operation =
      match operation () with
      | Ok () as result -> result
      | Error message as result ->
          record_error last_error message;
          result
    in
    {
      on_end =
        (fun span ->
          if not (Atomic.get stopped) then
            ignore (run (fun () -> Exporter.export exporter [ span ])));
      force_flush =
        (fun () ->
          if Atomic.get stopped then Error "span processor is shut down"
          else run (fun () -> Exporter.force_flush exporter));
      shutdown =
        (fun () ->
          if Atomic.compare_and_set stopped false true then
            run (fun () -> Exporter.shutdown exporter)
          else Ok ());
      dropped_spans = (fun () -> 0);
      last_error = (fun () -> Atomic.get last_error);
    }

  let force_flush processor = processor.force_flush ()
  let shutdown processor = processor.shutdown ()
  let dropped_spans processor = processor.dropped_spans ()
  let last_error processor = processor.last_error ()
end

module Batch_processor = struct
  type queue = { items : Span.data list; length : int; accepting : bool }

  type command =
    | Flush of (unit, string) result Eio.Promise.u
    | Shutdown of (unit, string) result Eio.Promise.u

  let rec increment counter =
    let current = Atomic.get counter in
    if not (Atomic.compare_and_set counter current (current + 1)) then
      increment counter

  let rec enqueue queue max_queue_size span =
    let current = Atomic.get queue in
    if (not current.accepting) || current.length >= max_queue_size then false
    else
      let updated =
        {
          current with
          items = span :: current.items;
          length = current.length + 1;
        }
      in
      if Atomic.compare_and_set queue current updated then true
      else enqueue queue max_queue_size span

  let rec close_queue queue =
    let current = Atomic.get queue in
    if not current.accepting then ()
    else if
      not
        (Atomic.compare_and_set queue current
           { current with accepting = false })
    then close_queue queue

  let rec take_batch queue maximum =
    let current = Atomic.get queue in
    if current.length = 0 then []
    else
      let chronological = List.rev current.items in
      let rec split count taken remaining =
        match remaining with
        | rest when count = 0 -> (List.rev taken, rest)
        | [] -> (List.rev taken, [])
        | item :: rest -> split (count - 1) (item :: taken) rest
      in
      let batch, remaining = split maximum [] chronological in
      let updated =
        {
          current with
          items = List.rev remaining;
          length = current.length - List.length batch;
        }
      in
      if Atomic.compare_and_set queue current updated then batch
      else take_batch queue maximum

  let rec append_command commands command =
    let current = Atomic.get commands in
    if not (Atomic.compare_and_set commands current (current @ [ command ]))
    then append_command commands command

  let rec take_command commands =
    match Atomic.get commands with
    | [] -> None
    | command :: rest as current ->
        if Atomic.compare_and_set commands current rest then Some command
        else take_command commands

  let create ~sw ?(max_queue_size = 2048) ?(scheduled_delay = 5.0)
      ?(export_timeout = 30.0) ?(max_export_batch_size = 512) exporter =
    if max_queue_size <= 0 then invalid_arg "max_queue_size must be positive";
    if max_export_batch_size <= 0 then
      invalid_arg "max_export_batch_size must be positive";
    if max_export_batch_size > max_queue_size then
      invalid_arg "max_export_batch_size must not exceed max_queue_size";
    if scheduled_delay <= 0.0 then
      invalid_arg "scheduled_delay must be positive";
    if export_timeout <= 0.0 then invalid_arg "export_timeout must be positive";
    let queue = Atomic.make { items = []; length = 0; accepting = true } in
    let commands = Atomic.make [] in
    let signal = Eio.Condition.create () in
    let dropped = Atomic.make 0 in
    let last_error = Atomic.make None in
    let lifecycle = Atomic.make `Running in
    let remember = function
      | Ok () as result -> result
      | Error message as result ->
          record_error last_error message;
          result
    in
    let with_timeout message operation =
      Eio.Fiber.first operation (fun () ->
          Eio_unix.sleep export_timeout;
          Error message)
      |> remember
    in
    let export_batch batch =
      if batch = [] then Ok ()
      else
        with_timeout "span export timed out" (fun () ->
            Exporter.export exporter batch)
    in
    let rec drain result =
      match take_batch queue max_export_batch_size with
      | [] -> result
      | batch ->
          let next = export_batch batch in
          let result = match result with Ok () -> next | Error _ -> result in
          drain result
    in
    let resolve resolver result =
      try Eio.Promise.resolve resolver result with Invalid_argument _ -> ()
    in
    let rec reject_commands message =
      match take_command commands with
      | None -> ()
      | Some (Flush resolver | Shutdown resolver) ->
          resolve resolver (Error message);
          reject_commands message
    in
    let wait () =
      Eio.Fiber.first
        (fun () ->
          Eio.Condition.loop_no_mutex signal (fun () ->
              let pending_command = Atomic.get commands <> [] in
              let queued = (Atomic.get queue).length in
              if pending_command || queued >= max_export_batch_size then Some ()
              else None))
        (fun () -> Eio_unix.sleep scheduled_delay)
    in
    let rec worker () =
      match take_command commands with
      | Some (Flush resolver) ->
          let result = drain (Ok ()) in
          let flush_result =
            with_timeout "span exporter flush timed out" (fun () ->
                Exporter.force_flush exporter)
          in
          let result =
            match result with Ok () -> flush_result | Error _ -> result
          in
          resolve resolver result;
          worker ()
      | Some (Shutdown resolver) ->
          let result = drain (Ok ()) in
          let shutdown_result =
            with_timeout "span exporter shutdown timed out" (fun () ->
                Exporter.shutdown exporter)
          in
          let result =
            match result with Ok () -> shutdown_result | Error _ -> result
          in
          Atomic.set lifecycle `Stopped;
          resolve resolver result
      | None ->
          let queued = (Atomic.get queue).length in
          if queued >= max_export_batch_size then (
            ignore (export_batch (take_batch queue max_export_batch_size));
            worker ())
          else (
            wait ();
            ignore (drain (Ok ()));
            worker ())
    in
    Eio.Fiber.fork ~sw (fun () ->
        Fun.protect worker ~finally:(fun () ->
            close_queue queue;
            Atomic.set lifecycle `Stopped;
            reject_commands "batch processor worker stopped"));
    let request command =
      let promise, resolver = Eio.Promise.create () in
      append_command commands (command resolver);
      Eio.Condition.broadcast signal;
      Eio.Promise.await promise
    in
    {
      on_end =
        (fun span ->
          if enqueue queue max_queue_size span then
            Eio.Condition.broadcast signal
          else increment dropped);
      force_flush =
        (fun () ->
          match Atomic.get lifecycle with
          | `Running -> request (fun resolver -> Flush resolver)
          | `Shutting_down | `Stopped -> Error "span processor is shut down");
      shutdown =
        (fun () ->
          if Atomic.compare_and_set lifecycle `Running `Shutting_down then (
            close_queue queue;
            request (fun resolver -> Shutdown resolver))
          else Ok ());
      dropped_spans = (fun () -> Atomic.get dropped);
      last_error = (fun () -> Atomic.get last_error);
    }
end

module Tracer = struct
  type t = {
    id_generator : Id_generator.t;
    sampler : Sampler.t;
    instrumentation_scope : string;
    instrumentation_scope_version : string option;
    processor : span_processor;
  }

  let create ?(id_generator = Id_generator.secure)
      ?(sampler = Sampler.parent_based ())
      ?(instrumentation_scope = "ocaml-halo") ?instrumentation_scope_version
      ~processor () =
    {
      id_generator;
      sampler;
      instrumentation_scope;
      instrumentation_scope_version;
      processor;
    }

  let start_span_with_parent ~parent ?(kind = Span_kind.Internal)
      ?start_time_unix_ns ?start_monotonic_ns ?(attributes = []) tracer name =
    let ( trace_id,
          generated_span_id,
          trace_state,
          inherited_flags,
          parent_span_id ) =
      match parent with
      | Some parent ->
          ( Trace_context.trace_id_value parent,
            None,
            Trace_context.trace_state parent,
            Trace_context.trace_flags parent,
            Some (Trace_context.span_id_value parent) )
      | None ->
          let trace_id, span_id =
            Id_generator.trace_and_span_id tracer.id_generator
          in
          ( trace_id,
            Some span_id,
            Trace_state.empty,
            (if tracer.id_generator.random then 0x02 else 0),
            None )
    in
    let decision =
      tracer.sampler Sampler.{ parent; trace_id; name; kind; attributes }
    in
    let sampled = decision = Sampler.Record_and_sample in
    let trace_flags =
      if sampled then inherited_flags lor 1 else inherited_flags land lnot 1
    in
    let span_id =
      match generated_span_id with
      | Some span_id -> span_id
      | None -> Id_generator.span_id tracer.id_generator
    in
    let context =
      Trace_context.create_from_validated_parts ~trace_id ~span_id ~trace_flags
        ~trace_state ~remote:false
    in
    let current_monotonic_ns = monotonic_time_ns () in
    let start_monotonic_ns =
      Option.value start_monotonic_ns ~default:current_monotonic_ns
    in
    let start_time_unix_ns =
      match start_time_unix_ns with
      | Some start_time_unix_ns -> start_time_unix_ns
      | None ->
          Int64.sub (unix_time_ns ())
            (Int64.max 0L (Int64.sub current_monotonic_ns start_monotonic_ns))
    in
    Span.create ~context ~parent_span_id ~kind ~start_time_unix_ns
      ~start_monotonic_ns ~instrumentation_scope:tracer.instrumentation_scope
      ~instrumentation_scope_version:tracer.instrumentation_scope_version
      ~recording:(decision <> Sampler.Drop) ~on_finish:tracer.processor.on_end
      ~name ~attributes

  let start_span ?kind ?start_time_unix_ns ?start_monotonic_ns ?attributes
      tracer name =
    start_span_with_parent ~parent:(Context.current ()) ?kind
      ?start_time_unix_ns ?start_monotonic_ns ?attributes tracer name
end

let socket_attributes prefix = function
  | Unix.ADDR_INET (address, port) ->
      [
        ( prefix ^ ".address",
          Attribute.String (Unix.string_of_inet_addr address) );
        (prefix ^ ".port", Attribute.Int (Int64.of_int port));
      ]
  | Unix.ADDR_UNIX path -> [ (prefix ^ ".address", Attribute.String path) ]

let path_of_target target =
  match target with
  | "" | "*" -> None
  | target when target.[0] = '/' ->
      let end_ =
        match String.index_opt target '?' with
        | None -> String.length target
        | Some index -> index
      in
      Some (String.sub target 0 end_)
  | target
    when String.starts_with ~prefix:"http://" target
         || String.starts_with ~prefix:"https://" target ->
      let uri = Uri.of_string target in
      let path = Uri.path uri in
      if path = "" then Some "/" else Some path
  | _ -> None

let of_tracer tracer =
  let module Server_span_observer = struct
    type span = Span.t

    let start_server_request request =
      let major, minor = request.http_version in
      let connection = request.connection_info in
      let attributes =
        [
          ("http.request.method", Attribute.String request.method_);
          ( "url.scheme",
            Attribute.String
              (match connection.tls with None -> "http" | Some _ -> "https") );
          ( "network.protocol.version",
            Attribute.String (Printf.sprintf "%d.%d" major minor) );
        ]
        @ socket_attributes "client" connection.remote_address
        @ socket_attributes "server" connection.local_address
      in
      let attributes =
        match path_of_target request.target with
        | None -> attributes
        | Some path -> ("url.path", Attribute.String path) :: attributes
      in
      Tracer.start_span_with_parent ~parent:request.remote_parent
        ~kind:Span_kind.Server ?start_time_unix_ns:request.start_time_unix_ns
        ~start_monotonic_ns:request.start_monotonic_ns ~attributes tracer
        request.method_

    let with_span = Context.with_span

    let finish_server_request span result =
      Option.iter
        (fun route ->
          Span.set_attribute span "http.route" (Attribute.String route);
          Span.set_name span (Printf.sprintf "%s %s" (Span.name span) route))
        result.route;
      Option.iter
        (fun status_code ->
          Span.set_attribute span "http.response.status_code"
            (Attribute.Int (Int64.of_int status_code));
          if status_code >= 500 then Span.set_status span (Span.Error None))
        result.status_code;
      (match result.outcome with
      | Some Router.Completed -> ()
      | Some (Router.Aborted _) ->
          Span.set_attribute span "error.type" (Attribute.String "aborted");
          Span.set_status span (Span.Error (Some "response aborted"))
      | Some Router.Client_disconnected ->
          Span.set_attribute span "error.type"
            (Attribute.String "client_disconnected");
          Span.set_status span (Span.Error (Some "client disconnected"))
      | None ->
          Span.set_attribute span "error.type" (Attribute.String "incomplete");
          Span.set_status span (Span.Error (Some "request incomplete")));
      Span.finish_at ?end_monotonic_ns:result.end_monotonic_ns span
  end in
  of_observer (module Server_span_observer)

module Otlp_http = struct
  type transport =
    endpoint:Uri.t ->
    headers:(string * string) list ->
    body:string ->
    (unit, string) result

  module Protobuf = struct
    let add_varint buffer value =
      let rec loop value =
        if Int64.unsigned_compare value 0x80L < 0 then
          Buffer.add_char buffer (Char.chr (Int64.to_int value))
        else (
          Buffer.add_char buffer
            (Char.chr
               (Int64.to_int (Int64.logor (Int64.logand value 0x7fL) 0x80L)));
          loop (Int64.shift_right_logical value 7))
      in
      loop value

    let add_key buffer field_number wire_type =
      add_varint buffer (Int64.of_int ((field_number lsl 3) lor wire_type))

    let add_bytes buffer field_number value =
      add_key buffer field_number 2;
      add_varint buffer (Int64.of_int (String.length value));
      Buffer.add_string buffer value

    let add_message buffer field_number encode =
      let nested = Buffer.create 128 in
      encode nested;
      add_bytes buffer field_number (Buffer.contents nested)

    let add_uint buffer field_number value =
      add_key buffer field_number 0;
      add_varint buffer (Int64.of_int value)

    let add_int64 buffer field_number value =
      add_key buffer field_number 0;
      add_varint buffer value

    let add_fixed64 buffer field_number value =
      add_key buffer field_number 1;
      for index = 0 to 7 do
        Buffer.add_char buffer
          (Char.chr
             (Int64.to_int
                (Int64.logand
                   (Int64.shift_right_logical value (index * 8))
                   0xffL)))
      done

    let add_fixed32 buffer field_number value =
      add_key buffer field_number 5;
      for index = 0 to 3 do
        Buffer.add_char buffer (Char.chr ((value lsr (index * 8)) land 0xff))
      done
  end

  let rec encode_any_value buffer = function
    | Attribute.String value -> Protobuf.add_bytes buffer 1 value
    | Attribute.Bool value ->
        Protobuf.add_uint buffer 2 (if value then 1 else 0)
    | Attribute.Int value -> Protobuf.add_int64 buffer 3 value
    | Attribute.Float value ->
        Protobuf.add_fixed64 buffer 4 (Int64.bits_of_float value)
    | Attribute.Bytes value -> Protobuf.add_bytes buffer 7 value
    | Attribute.Array values ->
        Protobuf.add_message buffer 5 (fun array ->
            List.iter
              (fun value ->
                Protobuf.add_message array 1 (fun encoded ->
                    encode_any_value encoded value))
              values)

  let encode_attribute buffer (key, value) =
    Protobuf.add_bytes buffer 1 key;
    Protobuf.add_message buffer 2 (fun encoded ->
        encode_any_value encoded value)

  let encode_attributes buffer field_number attributes =
    List.iter
      (fun attribute ->
        Protobuf.add_message buffer field_number (fun encoded ->
            encode_attribute encoded attribute))
      attributes

  let encode_event buffer (event : Span.event) =
    Protobuf.add_fixed64 buffer 1 event.timestamp_unix_ns;
    Protobuf.add_bytes buffer 2 event.name;
    encode_attributes buffer 3 event.attributes

  let span_kind_number = function
    | Span_kind.Internal -> 1
    | Span_kind.Server -> 2
    | Span_kind.Client -> 3
    | Span_kind.Producer -> 4
    | Span_kind.Consumer -> 5

  let encode_status buffer = function
    | Span.Unset -> ()
    | Span.Ok -> Protobuf.add_uint buffer 3 1
    | Span.Error description ->
        Option.iter (Protobuf.add_bytes buffer 2) description;
        Protobuf.add_uint buffer 3 2

  let encode_span buffer (span : Span.data) =
    let context = span.context in
    Protobuf.add_bytes buffer 1
      (Trace_context.trace_id_value context |> Trace_id.to_bytes);
    Protobuf.add_bytes buffer 2
      (Trace_context.span_id_value context |> Span_id.to_bytes);
    (match Trace_context.tracestate context with
    | [] -> ()
    | _ ->
        Protobuf.add_bytes buffer 3
          (Trace_context.trace_state context |> Trace_state.serialize));
    Option.iter
      (fun parent -> Protobuf.add_bytes buffer 4 (Span_id.to_bytes parent))
      span.parent_span_id;
    Protobuf.add_bytes buffer 5 span.name;
    Protobuf.add_uint buffer 6 (span_kind_number span.kind);
    Protobuf.add_fixed64 buffer 7 span.start_time_unix_ns;
    Protobuf.add_fixed64 buffer 8 span.end_time_unix_ns;
    encode_attributes buffer 9 span.attributes;
    List.iter
      (fun event ->
        Protobuf.add_message buffer 11 (fun encoded ->
            encode_event encoded event))
      span.events;
    (match span.status with
    | Span.Unset -> ()
    | status ->
        Protobuf.add_message buffer 15 (fun encoded ->
            encode_status encoded status));
    Protobuf.add_fixed32 buffer 16 (Trace_context.trace_flags context)

  let same_scope (left : Span.data) (right : Span.data) =
    left.instrumentation_scope = right.instrumentation_scope
    && left.instrumentation_scope_version = right.instrumentation_scope_version

  let group_by_scope spans =
    let rec insert span = function
      | [] -> [ (span, [ span ]) ]
      | (representative, grouped) :: rest when same_scope representative span ->
          (representative, span :: grouped) :: rest
      | group :: rest -> group :: insert span rest
    in
    List.fold_left (fun groups span -> insert span groups) [] spans

  let encode_scope_spans buffer ((representative : Span.data), spans) =
    Protobuf.add_message buffer 1 (fun scope ->
        Protobuf.add_bytes scope 1 representative.instrumentation_scope;
        Option.iter
          (Protobuf.add_bytes scope 2)
          representative.instrumentation_scope_version);
    List.rev spans
    |> List.iter (fun span ->
        Protobuf.add_message buffer 2 (fun encoded -> encode_span encoded span))

  let encode_request ~service_name ~resource_attributes spans =
    let resource_attributes =
      ("service.name", Attribute.String service_name)
      :: List.filter (fun (key, _) -> key <> "service.name") resource_attributes
    in
    let request = Buffer.create 4096 in
    Protobuf.add_message request 1 (fun resource_spans ->
        Protobuf.add_message resource_spans 1 (fun resource ->
            encode_attributes resource 1 resource_attributes);
        group_by_scope spans
        |> List.iter (fun group ->
            Protobuf.add_message resource_spans 2 (fun scope_spans ->
                encode_scope_spans scope_spans group)));
    Buffer.contents request

  let default_endpoint = Uri.of_string "http://localhost:4318/v1/traces"

  let header_is_reserved name =
    match String.lowercase_ascii name with
    | "host" | "content-type" | "content-length" | "connection" -> true
    | _ -> false

  let validate_headers headers =
    List.iter
      (fun (name, value) ->
        if
          (not (Router.Headers.is_valid_name name))
          || (not (Router.Headers.is_valid_value value))
          || header_is_reserved name
        then invalid_arg ("invalid OTLP HTTP header: " ^ name))
      headers

  let exporter_with_transport ~transport ~service_name
      ?(endpoint = default_endpoint) ?(headers = []) ?(resource_attributes = [])
      () =
    if service_name = "" then invalid_arg "service_name must not be empty";
    validate_headers headers;
    let stopped = Atomic.make false in
    let send spans =
      if spans = [] then Ok ()
      else if Atomic.get stopped then Error "OTLP exporter is shut down"
      else
        let body = encode_request ~service_name ~resource_attributes spans in
        transport ~endpoint ~headers ~body
    in
    Exporter.create ~export:send
      ~shutdown:(fun () ->
        Atomic.set stopped true;
        Ok ())
      ()

  let exporter ~net ~service_name ?endpoint ?headers ?resource_attributes () =
    let transport ~endpoint ~headers ~body =
      Otlp_http_transport.post ~net ~endpoint ~headers
        ~content_type:"application/x-protobuf" ~body
    in
    exporter_with_transport ~transport ~service_name ?endpoint ?headers
      ?resource_attributes ()
end
