type attribute_value = String of string | Int of int64 | Bool of bool
type attribute = string * attribute_value
type number = Int of int64 | Float of float

type histogram = {
  count : int64;
  sum : float;
  explicit_bounds : float list;
  bucket_counts : int64 list;
  min : float option;
  max : float option;
}

type aggregation =
  | Gauge of number
  | Sum of { value : number; monotonic : bool }
  | Histogram of histogram

type point = {
  name : string;
  description : string;
  unit_ : string;
  attributes : attribute list;
  aggregation : aggregation;
}

type snapshot = {
  start_time_unix_ns : int64;
  time_unix_ns : int64;
  points : point list;
}

type histogram_state = {
  bounds : float array;
  counts : int64 array;
  mutable count : int64;
  mutable sum : float;
  mutable min : float option;
  mutable max : float option;
}

type request_group = {
  metric_attributes : attribute list;
  count_attributes : attribute list;
  mutable requests : int64;
  duration : histogram_state;
  request_size : histogram_state;
  response_size : histogram_state;
}

type runtime = {
  utilization : float;
  active_connections : int64;
  active_requests : int64;
  accepted_connections : int64;
  rejected_connections : int64;
  completed_requests : int64;
  tls_full_handshakes : int64;
  tls_resumed_handshakes : int64;
  tls_handshake_failures : int64;
  event_queue_depth : int64;
  command_queue_depth : int64;
  controller_state : string;
  backpressure_enabled : bool;
}

type t = {
  enabled : bool;
  start_time_unix_ns : int64;
  duration_bounds : float array;
  size_bounds : float array;
  active : (string * string, int64) Hashtbl.t;
  requests : (attribute list, request_group) Hashtbl.t;
  mutable runtime : runtime option;
}

type request = {
  collector : t;
  method_ : string;
  scheme : string;
  start_monotonic_ns : int64;
}

let unix_time_ns () = Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.0)

let default_duration_buckets =
  [
    0.005;
    0.01;
    0.025;
    0.05;
    0.075;
    0.1;
    0.25;
    0.5;
    0.75;
    1.0;
    2.5;
    5.0;
    7.5;
    10.0;
  ]

let default_size_buckets =
  [ 0.0; 100.0; 1_000.0; 10_000.0; 100_000.0; 1_000_000.0; 10_000_000.0 ]

let validate_buckets name buckets =
  let rec loop previous = function
    | [] -> ()
    | value :: rest ->
        if
          (not (Float.is_finite value))
          || value < 0.0
          || Option.fold ~none:false ~some:(fun last -> value <= last) previous
        then
          invalid_arg
            (name ^ " must contain finite, non-negative, increasing values");
        loop (Some value) rest
  in
  loop None buckets;
  Array.of_list buckets

let create ?(duration_buckets = default_duration_buckets)
    ?(size_buckets = default_size_buckets) () =
  {
    enabled = true;
    start_time_unix_ns = unix_time_ns ();
    duration_bounds = validate_buckets "duration_buckets" duration_buckets;
    size_bounds = validate_buckets "size_buckets" size_buckets;
    active = Hashtbl.create 8;
    requests = Hashtbl.create 32;
    runtime = None;
  }

let noop =
  {
    enabled = false;
    start_time_unix_ns = 0L;
    duration_bounds = [||];
    size_bounds = [||];
    active = Hashtbl.create 0;
    requests = Hashtbl.create 0;
    runtime = None;
  }

let histogram_state bounds =
  {
    bounds;
    counts = Array.make (Array.length bounds + 1) 0L;
    count = 0L;
    sum = 0.0;
    min = None;
    max = None;
  }

let record_histogram histogram value =
  if Float.is_finite value && value >= 0.0 then (
    let rec bucket index =
      if index = Array.length histogram.bounds then index
      else if value <= histogram.bounds.(index) then index
      else bucket (index + 1)
    in
    let index = bucket 0 in
    histogram.counts.(index) <- Int64.succ histogram.counts.(index);
    histogram.count <- Int64.succ histogram.count;
    histogram.sum <- histogram.sum +. value;
    histogram.min <-
      Some (Option.fold ~none:value ~some:(Float.min value) histogram.min);
    histogram.max <-
      Some (Option.fold ~none:value ~some:(Float.max value) histogram.max))

let snapshot_histogram histogram =
  let cumulative = ref 0L in
  let bucket_counts =
    Array.to_list histogram.counts
    |> List.map (fun count ->
        cumulative := Int64.add !cumulative count;
        !cumulative)
  in
  {
    count = histogram.count;
    sum = histogram.sum;
    explicit_bounds = Array.to_list histogram.bounds;
    bucket_counts;
    min = histogram.min;
    max = histogram.max;
  }

let string_of_attribute_value = function
  | String value -> "s:" ^ value
  | Int value -> "i:" ^ Int64.to_string value
  | Bool value -> if value then "b:1" else "b:0"

let status_class = function
  | Some status when status >= 100 && status <= 599 ->
      Printf.sprintf "%dxx" (status / 100)
  | _ -> "unknown"

let error_type status outcome =
  match outcome with
  | "aborted" -> Some "aborted"
  | "client_disconnected" -> Some "client_disconnected"
  | "incomplete" -> Some "incomplete"
  | _ -> (
      match status with
      | Some status when status >= 500 -> Some (string_of_int status)
      | _ -> None)

let metric_attributes request ~route ~status_code ~outcome =
  let attributes =
    [
      ("http.request.method", String request.method_);
      ("url.scheme", String request.scheme);
    ]
  in
  let attributes =
    Option.fold ~none:attributes
      ~some:(fun route -> ("http.route", String route) :: attributes)
      route
  in
  let attributes =
    Option.fold ~none:attributes
      ~some:(fun status ->
        ("http.response.status_code", Int (Int64.of_int status)) :: attributes)
      status_code
  in
  Option.fold ~none:attributes
    ~some:(fun value -> ("error.type", String value) :: attributes)
    (error_type status_code outcome)
  |> List.sort compare

let count_attributes metric_attributes status_code outcome =
  ("http.response.status_class", String (status_class status_code))
  :: ("halo.response.outcome", String outcome)
  :: metric_attributes
  |> List.sort compare

let start_request collector ~method_ ~scheme ~start_monotonic_ns =
  (if collector.enabled then
     let key = (method_, scheme) in
     let active =
       Option.value (Hashtbl.find_opt collector.active key) ~default:0L
     in
     Hashtbl.replace collector.active key (Int64.succ active));
  { collector; method_; scheme; start_monotonic_ns }

let finish_request request ~route ~status_code ~outcome ~end_monotonic_ns
    ~request_body_size ~response_body_size =
  let collector = request.collector in
  if collector.enabled then (
    let active_key = (request.method_, request.scheme) in
    let active =
      Option.value (Hashtbl.find_opt collector.active active_key) ~default:1L
    in
    Hashtbl.replace collector.active active_key
      (Int64.max 0L (Int64.pred active));
    let metric_attributes =
      metric_attributes request ~route ~status_code ~outcome
    in
    let count_attributes =
      count_attributes metric_attributes status_code outcome
    in
    let key = count_attributes in
    let group =
      match Hashtbl.find_opt collector.requests key with
      | Some group -> group
      | None ->
          let group =
            {
              metric_attributes;
              count_attributes;
              requests = 0L;
              duration = histogram_state collector.duration_bounds;
              request_size = histogram_state collector.size_bounds;
              response_size = histogram_state collector.size_bounds;
            }
          in
          Hashtbl.add collector.requests key group;
          group
    in
    group.requests <- Int64.succ group.requests;
    Option.iter
      (fun end_ns ->
        let elapsed =
          Int64.max 0L (Int64.sub end_ns request.start_monotonic_ns)
        in
        record_histogram group.duration (Int64.to_float elapsed /. 1e9))
      end_monotonic_ns;
    record_histogram group.request_size (Int64.to_float request_body_size);
    record_histogram group.response_size (Int64.to_float response_body_size))

let record_runtime collector ~utilization ~active_connections ~active_requests
    ~accepted_connections ~rejected_connections ~completed_requests
    ~tls_full_handshakes ~tls_resumed_handshakes ~tls_handshake_failures
    ~event_queue_depth ~command_queue_depth ~controller_state
    ~backpressure_enabled =
  if collector.enabled then
    collector.runtime <-
      Some
        {
          utilization;
          active_connections;
          active_requests;
          accepted_connections;
          rejected_connections;
          completed_requests;
          tls_full_handshakes;
          tls_resumed_handshakes;
          tls_handshake_failures;
          event_queue_depth;
          command_queue_depth;
          controller_state;
          backpressure_enabled;
        }

let point ?(attributes = []) ~name ~description ~unit_ aggregation =
  { name; description; unit_; attributes; aggregation }

let request_points collector =
  let active =
    Hashtbl.to_seq collector.active
    |> Seq.map (fun ((method_, scheme), value) ->
        point ~name:"http.server.active_requests"
          ~description:"Number of active HTTP server requests."
          ~unit_:"{request}"
          ~attributes:
            [
              ("http.request.method", String method_);
              ("url.scheme", String scheme);
            ]
          (Gauge (Int value)))
    |> List.of_seq
  in
  Hashtbl.to_seq_values collector.requests
  |> Seq.fold_left
       (fun points group ->
         point ~name:"halo.http.server.requests"
           ~description:
             "Number of HTTP server requests by terminal status class."
           ~unit_:"{request}" ~attributes:group.count_attributes
           (Sum { value = Int group.requests; monotonic = true })
         :: point ~name:"http.server.request.duration"
              ~description:"Duration of HTTP server requests." ~unit_:"s"
              ~attributes:group.metric_attributes
              (Histogram (snapshot_histogram group.duration))
         :: point ~name:"http.server.request.body.size"
              ~description:"Size of HTTP server request bodies." ~unit_:"By"
              ~attributes:group.metric_attributes
              (Histogram (snapshot_histogram group.request_size))
         :: point ~name:"http.server.response.body.size"
              ~description:"Size of HTTP server response bodies." ~unit_:"By"
              ~attributes:group.metric_attributes
              (Histogram (snapshot_histogram group.response_size))
         :: points)
       active

let runtime_points = function
  | None -> []
  | Some runtime ->
      let gauge name description unit_ value =
        point ~name ~description ~unit_ (Gauge value)
      in
      let sum name description unit_ value =
        point ~name ~description ~unit_
          (Sum { value = Int value; monotonic = true })
      in
      [
        gauge "halo.runtime.utilization" "Reactor event-loop utilization." "1"
          (Float runtime.utilization);
        gauge "halo.runtime.connections.active" "Active client connections."
          "{connection}" (Int runtime.active_connections);
        gauge "halo.runtime.requests.active" "Requests active in the reactor."
          "{request}" (Int runtime.active_requests);
        sum "halo.runtime.connections.accepted" "Accepted client connections."
          "{connection}" runtime.accepted_connections;
        sum "halo.runtime.connections.rejected" "Rejected client connections."
          "{connection}" runtime.rejected_connections;
        sum "halo.runtime.requests.completed" "Completed HTTP requests."
          "{request}" runtime.completed_requests;
        point ~name:"halo.runtime.tls.handshakes"
          ~description:"TLS handshakes by result and session mode."
          ~unit_:"{handshake}"
          ~attributes:
            [ ("result", String "success"); ("session.mode", String "full") ]
          (Sum { value = Int runtime.tls_full_handshakes; monotonic = true });
        point ~name:"halo.runtime.tls.handshakes"
          ~description:"TLS handshakes by result and session mode."
          ~unit_:"{handshake}"
          ~attributes:
            [ ("result", String "success"); ("session.mode", String "resumed") ]
          (Sum { value = Int runtime.tls_resumed_handshakes; monotonic = true });
        point ~name:"halo.runtime.tls.handshakes"
          ~description:"TLS handshakes by result and session mode."
          ~unit_:"{handshake}"
          ~attributes:[ ("result", String "failure") ]
          (Sum { value = Int runtime.tls_handshake_failures; monotonic = true });
        ( gauge "halo.runtime.queue.depth" "Current bridge queue depth."
            "{item}" (Int runtime.event_queue_depth)
        |> fun point ->
          { point with attributes = [ ("queue", String "event") ] } );
        ( gauge "halo.runtime.queue.depth" "Current bridge queue depth."
            "{item}" (Int runtime.command_queue_depth)
        |> fun point ->
          { point with attributes = [ ("queue", String "command") ] } );
        point ~name:"halo.runtime.controller.state"
          ~description:"Current adaptive controller state." ~unit_:"1"
          ~attributes:[ ("state", String runtime.controller_state) ]
          (Gauge (Int 1L));
        gauge "halo.runtime.backpressure"
          "Whether admission backpressure is enabled." "1"
          (Int (if runtime.backpressure_enabled then 1L else 0L));
      ]

let compare_attribute (left_name, left_value) (right_name, right_value) =
  let by_name = String.compare left_name right_name in
  if by_name <> 0 then by_name
  else
    String.compare
      (string_of_attribute_value left_value)
      (string_of_attribute_value right_value)

let compare_point left right =
  let by_name = String.compare left.name right.name in
  if by_name <> 0 then by_name
  else List.compare compare_attribute left.attributes right.attributes

let snapshot collector =
  let points =
    if collector.enabled then
      request_points collector @ runtime_points collector.runtime
    else []
  in
  {
    start_time_unix_ns = collector.start_time_unix_ns;
    time_unix_ns = unix_time_ns ();
    points = List.sort compare_point points;
  }

module Prometheus = struct
  let content_type = "text/plain; version=0.0.4; charset=utf-8"

  let sanitize value =
    String.map
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | ':') as char -> char
        | _ -> '_')
      value

  let ends_with value suffix = String.ends_with ~suffix value

  let with_unit name unit_ =
    match unit_ with
    | "s" when not (ends_with name "_seconds") -> name ^ "_seconds"
    | "By" when not (ends_with name "_bytes") -> name ^ "_bytes"
    | _ -> name

  let metric_name point =
    let name = with_unit (sanitize point.name) point.unit_ in
    match point.aggregation with
    | Sum { monotonic = true; _ } when not (ends_with name "_total") ->
        name ^ "_total"
    | _ -> name

  let escape_label value =
    let buffer = Buffer.create (String.length value) in
    String.iter
      (function
        | '\\' -> Buffer.add_string buffer "\\\\"
        | '"' -> Buffer.add_string buffer "\\\""
        | '\n' -> Buffer.add_string buffer "\\n"
        | char -> Buffer.add_char buffer char)
      value;
    Buffer.contents buffer

  let label_value = function
    | String value -> value
    | Int value -> Int64.to_string value
    | Bool value -> string_of_bool value

  let labels ?extra attributes =
    let attributes =
      match extra with None -> attributes | Some label -> label :: attributes
    in
    match attributes with
    | [] -> ""
    | attributes ->
        attributes
        |> List.sort compare_attribute
        |> List.map (fun (name, value) ->
            Printf.sprintf "%s=\"%s\"" (sanitize name)
              (escape_label (label_value value)))
        |> String.concat "," |> Printf.sprintf "{%s}"

  let float value =
    if Float.is_infinite value then if value > 0.0 then "+Inf" else "-Inf"
    else if Float.is_nan value then "NaN"
    else Printf.sprintf "%.17g" value

  let number = function
    | Int value -> Int64.to_string value
    | Float value -> float value

  let help_text value =
    let buffer = Buffer.create (String.length value) in
    String.iter
      (function
        | '\\' -> Buffer.add_string buffer "\\\\"
        | '\n' -> Buffer.add_string buffer "\\n"
        | char -> Buffer.add_char buffer char)
      value;
    Buffer.contents buffer

  let add_sample buffer name labels value =
    Buffer.add_string buffer name;
    Buffer.add_string buffer labels;
    Buffer.add_char buffer ' ';
    Buffer.add_string buffer value;
    Buffer.add_char buffer '\n'

  let add_histogram buffer name attributes histogram =
    let rec buckets bounds counts =
      match (bounds, counts) with
      | bound :: rest_bounds, count :: rest_counts ->
          add_sample buffer (name ^ "_bucket")
            (labels ~extra:("le", String (float bound)) attributes)
            (Int64.to_string count);
          buckets rest_bounds rest_counts
      | [], [ count ] ->
          add_sample buffer (name ^ "_bucket")
            (labels ~extra:("le", String "+Inf") attributes)
            (Int64.to_string count)
      | _ -> invalid_arg "malformed histogram snapshot"
    in
    buckets histogram.explicit_bounds histogram.bucket_counts;
    add_sample buffer (name ^ "_sum") (labels attributes) (float histogram.sum);
    add_sample buffer (name ^ "_count") (labels attributes)
      (Int64.to_string histogram.count)

  let encode snapshot =
    let buffer = Buffer.create 8192 in
    let rec loop previous_name = function
      | [] -> ()
      | point :: rest ->
          let name = metric_name point in
          if previous_name <> Some name then (
            Buffer.add_string buffer "# HELP ";
            Buffer.add_string buffer name;
            Buffer.add_char buffer ' ';
            Buffer.add_string buffer (help_text point.description);
            Buffer.add_char buffer '\n';
            Buffer.add_string buffer "# TYPE ";
            Buffer.add_string buffer name;
            Buffer.add_char buffer ' ';
            Buffer.add_string buffer
              (match point.aggregation with
              | Gauge _ -> "gauge"
              | Sum { monotonic = true; _ } -> "counter"
              | Sum _ -> "gauge"
              | Histogram _ -> "histogram");
            Buffer.add_char buffer '\n');
          (match point.aggregation with
          | Gauge value | Sum { value; _ } ->
              add_sample buffer name (labels point.attributes) (number value)
          | Histogram histogram ->
              add_histogram buffer name point.attributes histogram);
          loop (Some name) rest
    in
    loop None (List.sort compare_point snapshot.points);
    Buffer.contents buffer
end

module Otlp_http = struct
  module Retry = struct
    type t = {
      max_attempts : int;
      initial_delay : float;
      max_delay : float;
      multiplier : float;
      jitter : float;
    }

    let create ?(max_attempts = 5) ?(initial_delay = 0.5) ?(max_delay = 5.0)
        ?(multiplier = 2.0) ?(jitter = 0.2) () =
      if max_attempts <= 0 then invalid_arg "max_attempts must be positive";
      if (not (Float.is_finite initial_delay)) || initial_delay <= 0.0 then
        invalid_arg "initial_delay must be positive and finite";
      if (not (Float.is_finite max_delay)) || max_delay < initial_delay then
        invalid_arg "max_delay must be finite and at least initial_delay";
      if (not (Float.is_finite multiplier)) || multiplier < 1.0 then
        invalid_arg "multiplier must be finite and at least one";
      if (not (Float.is_finite jitter)) || jitter < 0.0 || jitter > 1.0 then
        invalid_arg "jitter must be between zero and one";
      { max_attempts; initial_delay; max_delay; multiplier; jitter }

    let default = create ()
    let disabled = create ~max_attempts:1 ()
  end

  type transport =
    endpoint:Uri.t ->
    headers:(string * string) list ->
    body:string ->
    (unit, string) result

  type transport_error = {
    message : string;
    retryable : bool;
    retry_after : float option;
  }

  type exporter = {
    transport :
      endpoint:Uri.t ->
      headers:(string * string) list ->
      body:string ->
      (unit, transport_error) result;
    endpoint : Uri.t;
    headers : (string * string) list;
    service_name : string;
    resource_attributes : attribute list;
    instrumentation_scope : string;
    instrumentation_scope_version : string option;
    retry : Retry.t;
    stopped : bool Atomic.t;
  }

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

    let add_key buffer field wire =
      add_varint buffer (Int64.of_int ((field lsl 3) lor wire))

    let add_bytes buffer field value =
      add_key buffer field 2;
      add_varint buffer (Int64.of_int (String.length value));
      Buffer.add_string buffer value

    let add_message buffer field encode =
      let nested = Buffer.create 128 in
      encode nested;
      add_bytes buffer field (Buffer.contents nested)

    let add_bool buffer field value =
      add_key buffer field 0;
      add_varint buffer (if value then 1L else 0L)

    let add_uint buffer field value =
      add_key buffer field 0;
      add_varint buffer value

    let add_fixed64 buffer field value =
      add_key buffer field 1;
      for index = 0 to 7 do
        Buffer.add_char buffer
          (Char.chr
             (Int64.to_int
                (Int64.logand
                   (Int64.shift_right_logical value (index * 8))
                   0xffL)))
      done

    let raw_fixed64 buffer value =
      for index = 0 to 7 do
        Buffer.add_char buffer
          (Char.chr
             (Int64.to_int
                (Int64.logand
                   (Int64.shift_right_logical value (index * 8))
                   0xffL)))
      done

    let add_packed_fixed64 buffer field values =
      let packed = Buffer.create (List.length values * 8) in
      List.iter (raw_fixed64 packed) values;
      add_bytes buffer field (Buffer.contents packed)
  end

  let encode_attribute_value buffer = function
    | String value -> Protobuf.add_bytes buffer 1 value
    | Bool value -> Protobuf.add_bool buffer 2 value
    | Int value -> Protobuf.add_uint buffer 3 value

  let encode_attribute buffer (key, value) =
    Protobuf.add_bytes buffer 1 key;
    Protobuf.add_message buffer 2 (fun encoded ->
        encode_attribute_value encoded value)

  let encode_attributes buffer field attributes =
    List.iter
      (fun attribute ->
        Protobuf.add_message buffer field (fun encoded ->
            encode_attribute encoded attribute))
      attributes

  let encode_number_point (snapshot : snapshot) buffer attributes value =
    encode_attributes buffer 7 attributes;
    Protobuf.add_fixed64 buffer 2 snapshot.start_time_unix_ns;
    Protobuf.add_fixed64 buffer 3 snapshot.time_unix_ns;
    match value with
    | Float value -> Protobuf.add_fixed64 buffer 4 (Int64.bits_of_float value)
    | Int value -> Protobuf.add_fixed64 buffer 6 value

  let encode_histogram_point (snapshot : snapshot) buffer attributes
      (histogram : histogram) =
    let bucket_counts =
      let rec differences previous = function
        | [] -> []
        | cumulative :: rest ->
            Int64.sub cumulative previous :: differences cumulative rest
      in
      differences 0L histogram.bucket_counts
    in
    encode_attributes buffer 9 attributes;
    Protobuf.add_fixed64 buffer 2 snapshot.start_time_unix_ns;
    Protobuf.add_fixed64 buffer 3 snapshot.time_unix_ns;
    Protobuf.add_fixed64 buffer 4 histogram.count;
    Protobuf.add_fixed64 buffer 5 (Int64.bits_of_float histogram.sum);
    Protobuf.add_packed_fixed64 buffer 6 bucket_counts;
    Protobuf.add_packed_fixed64 buffer 7
      (List.map Int64.bits_of_float histogram.explicit_bounds);
    Option.iter
      (fun value -> Protobuf.add_fixed64 buffer 11 (Int64.bits_of_float value))
      histogram.min;
    Option.iter
      (fun value -> Protobuf.add_fixed64 buffer 12 (Int64.bits_of_float value))
      histogram.max

  let same_aggregation left right =
    match (left, right) with
    | Gauge _, Gauge _ | Histogram _, Histogram _ -> true
    | Sum { monotonic = left; _ }, Sum { monotonic = right; _ } -> left = right
    | _ -> false

  let same_metric (left : point) (right : point) =
    String.equal left.name right.name
    && String.equal left.description right.description
    && String.equal left.unit_ right.unit_
    && same_aggregation left.aggregation right.aggregation

  let group_metrics points =
    let rec loop current groups = function
      | [] ->
          List.rev (if current = [] then groups else List.rev current :: groups)
      | point :: rest -> (
          match current with
          | representative :: _ when same_metric representative point ->
              loop (point :: current) groups rest
          | [] -> loop [ point ] groups rest
          | _ -> loop [ point ] (List.rev current :: groups) rest)
    in
    loop [] [] (List.sort compare_point points)

  let encode_metric (snapshot : snapshot) buffer points =
    let point = List.hd points in
    Protobuf.add_bytes buffer 1 point.name;
    Protobuf.add_bytes buffer 2 point.description;
    Protobuf.add_bytes buffer 3 point.unit_;
    match point.aggregation with
    | Gauge _ ->
        Protobuf.add_message buffer 5 (fun gauge ->
            List.iter
              (fun point ->
                match point.aggregation with
                | Gauge value ->
                    Protobuf.add_message gauge 1 (fun data_point ->
                        encode_number_point snapshot data_point point.attributes
                          value)
                | _ -> assert false)
              points)
    | Sum { monotonic; _ } ->
        Protobuf.add_message buffer 7 (fun sum ->
            List.iter
              (fun point ->
                match point.aggregation with
                | Sum { value; _ } ->
                    Protobuf.add_message sum 1 (fun data_point ->
                        encode_number_point snapshot data_point point.attributes
                          value)
                | _ -> assert false)
              points;
            Protobuf.add_uint sum 2 2L;
            Protobuf.add_bool sum 3 monotonic)
    | Histogram _ ->
        Protobuf.add_message buffer 9 (fun encoded ->
            List.iter
              (fun point ->
                match point.aggregation with
                | Histogram histogram ->
                    Protobuf.add_message encoded 1 (fun data_point ->
                        encode_histogram_point snapshot data_point
                          point.attributes histogram)
                | _ -> assert false)
              points;
            Protobuf.add_uint encoded 2 2L)

  let encode_request exporter (snapshot : snapshot) =
    let resource_attributes =
      ("service.name", String exporter.service_name)
      :: List.filter
           (fun (name, _) -> not (String.equal name "service.name"))
           exporter.resource_attributes
    in
    let request = Buffer.create 8192 in
    Protobuf.add_message request 1 (fun resource_metrics ->
        Protobuf.add_message resource_metrics 1 (fun resource ->
            encode_attributes resource 1 resource_attributes);
        Protobuf.add_message resource_metrics 2 (fun scope_metrics ->
            Protobuf.add_message scope_metrics 1 (fun scope ->
                Protobuf.add_bytes scope 1 exporter.instrumentation_scope;
                Option.iter
                  (Protobuf.add_bytes scope 2)
                  exporter.instrumentation_scope_version);
            group_metrics snapshot.points
            |> List.iter (fun points ->
                Protobuf.add_message scope_metrics 2 (fun metric ->
                    encode_metric snapshot metric points))));
    Buffer.contents request

  let default_endpoint = Uri.of_string "http://localhost:4318/v1/metrics"

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

  let jittered_delay retry random delay =
    let spread = delay *. retry.Retry.jitter in
    Float.min retry.max_delay
      (delay -. spread +. Random.State.float random (spread *. 2.0))

  let make_exporter ~transport ~service_name ?(endpoint = default_endpoint)
      ?(headers = []) ?(resource_attributes = [])
      ?(instrumentation_scope = "ocaml-halo") ?instrumentation_scope_version
      ?(retry = Retry.default) () =
    if service_name = "" then invalid_arg "service_name must not be empty";
    if instrumentation_scope = "" then
      invalid_arg "instrumentation_scope must not be empty";
    validate_headers headers;
    {
      transport;
      endpoint;
      headers;
      service_name;
      resource_attributes;
      instrumentation_scope;
      instrumentation_scope_version;
      retry;
      stopped = Atomic.make false;
    }

  let exporter_with_transport ~transport ~service_name ?endpoint ?headers
      ?resource_attributes ?instrumentation_scope ?instrumentation_scope_version
      ?retry () =
    let transport ~endpoint ~headers ~body =
      match transport ~endpoint ~headers ~body with
      | Ok () -> Ok ()
      | Error message -> Error { message; retryable = true; retry_after = None }
    in
    make_exporter ~transport ~service_name ?endpoint ?headers
      ?resource_attributes ?instrumentation_scope ?instrumentation_scope_version
      ?retry ()

  let exporter ~net ~service_name ?endpoint ?headers ?resource_attributes
      ?instrumentation_scope ?instrumentation_scope_version ?retry () =
    let transport ~endpoint ~headers ~body =
      match
        Otlp_http_transport.post ~net ~endpoint ~headers
          ~content_type:"application/x-protobuf" ~body
      with
      | Ok () -> Ok ()
      | Error error ->
          Error
            {
              message = error.message;
              retryable = error.retryable;
              retry_after = error.retry_after;
            }
    in
    make_exporter ~transport ~service_name ?endpoint ?headers
      ?resource_attributes ?instrumentation_scope ?instrumentation_scope_version
      ?retry ()

  let export exporter (snapshot : snapshot) =
    if Atomic.get exporter.stopped then
      Error "OTLP metrics exporter is shut down"
    else if snapshot.points = [] then Ok ()
    else
      let body = encode_request exporter snapshot in
      let random = lazy (Random.State.make_self_init ()) in
      let rec attempt number backoff =
        if Atomic.get exporter.stopped then
          Error "OTLP metrics exporter is shut down"
        else
          match
            exporter.transport ~endpoint:exporter.endpoint
              ~headers:exporter.headers ~body
          with
          | Ok () -> Ok ()
          | Error error
            when error.retryable && number < exporter.retry.max_attempts ->
              let delay =
                match error.retry_after with
                | Some delay -> Float.min exporter.retry.max_delay delay
                | None ->
                    jittered_delay exporter.retry (Lazy.force random) backoff
              in
              Eio_unix.sleep delay;
              attempt (number + 1)
                (Float.min exporter.retry.max_delay
                   (backoff *. exporter.retry.multiplier))
          | Error error -> Error error.message
      in
      attempt 1 exporter.retry.initial_delay

  let shutdown exporter = Atomic.set exporter.stopped true
end

module Private = struct
  type nonrec request = request

  let enabled collector = collector.enabled
  let start_request = start_request
  let finish_request = finish_request
  let record_runtime = record_runtime
end
