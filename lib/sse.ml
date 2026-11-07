type event = {
  event : string option;
  data : string list;
  id : string option;
  retry_ms : int option;
}

type message = Event of event | Comment of string

let default_headers = Router.Headers.sse_defaults
let event ?event ?id ?retry_ms data = Event { event; data; id; retry_ms }
let comment line = Comment line

let add_prefixed_lines buffer prefix value =
  String.split_on_char '\n' value
  |> List.iter (fun line ->
      Buffer.add_string buffer prefix;
      Buffer.add_string buffer line;
      Buffer.add_string buffer "\n")

let encode_message = function
  | Comment line ->
      let buffer = Buffer.create (String.length line + 8) in
      add_prefixed_lines buffer ": " line;
      Buffer.add_string buffer "\n";
      Buffer.contents buffer
  | Event event ->
      let buffer = Buffer.create 128 in
      Option.iter
        (fun name ->
          Buffer.add_string buffer "event: ";
          Buffer.add_string buffer name;
          Buffer.add_string buffer "\n")
        event.event;
      Option.iter
        (fun id ->
          Buffer.add_string buffer "id: ";
          Buffer.add_string buffer id;
          Buffer.add_string buffer "\n")
        event.id;
      Option.iter
        (fun retry_ms ->
          Buffer.add_string buffer "retry: ";
          Buffer.add_string buffer (string_of_int retry_ms);
          Buffer.add_string buffer "\n")
        event.retry_ms;
      List.iter (add_prefixed_lines buffer "data: ") event.data;
      Buffer.add_string buffer "\n";
      Buffer.contents buffer

let respond ?(headers = Router.Headers.empty) producer =
  Stream.respond ~status:Router.Status.ok
    ~headers:(Router.Headers.append default_headers headers) (fun ~yield ->
      producer ~send:(fun message -> yield (encode_message message)))

let respond_buffered ?(headers = Router.Headers.empty) ?max_buffered_bytes
    producer =
  Stream.respond_buffered ~status:Router.Status.ok ?max_buffered_bytes
    ~headers:(Router.Headers.append default_headers headers)
    (fun ~yield ~flush ->
      producer ~send:(fun message -> yield (encode_message message)) ~flush)
