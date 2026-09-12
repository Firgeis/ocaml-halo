let response_status line =
  match
    String.split_on_char ' ' line |> List.filter (fun part -> part <> "")
  with
  | protocol :: code :: _ when String.starts_with ~prefix:"HTTP/" protocol ->
      int_of_string_opt code
  | _ -> None

let endpoint_parts endpoint =
  let scheme = Uri.scheme endpoint |> Option.map String.lowercase_ascii in
  if scheme <> Some "http" then
    invalid_arg "the built-in OTLP transport requires an http endpoint";
  let host =
    match Uri.host endpoint with
    | Some host when host <> "" -> host
    | _ -> invalid_arg "the OTLP endpoint must contain a host"
  in
  let port = Option.value (Uri.port endpoint) ~default:80 in
  let target =
    match Uri.path_and_query endpoint with "" -> "/" | target -> target
  in
  let host_header =
    let host = if String.contains host ':' then "[" ^ host ^ "]" else host in
    if port = 80 then host else Printf.sprintf "%s:%d" host port
  in
  (host, port, target, host_header)

let add_header request (name, value) =
  Buffer.add_string request name;
  Buffer.add_string request ": ";
  Buffer.add_string request value;
  Buffer.add_string request "\r\n"

let request ~target ~host_header ~headers ~content_type ~body =
  let request = Buffer.create (String.length body + 512) in
  Buffer.add_string request ("POST " ^ target ^ " HTTP/1.1\r\n");
  add_header request ("Host", host_header);
  add_header request ("Content-Type", content_type);
  add_header request ("Content-Length", string_of_int (String.length body));
  add_header request ("Connection", "close");
  List.iter (add_header request) headers;
  Buffer.add_string request "\r\n";
  Buffer.add_string request body;
  Buffer.contents request

let read_response flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:65_536 in
  let status_line = Eio.Buf_read.line reader in
  let rec read_headers count =
    if count = 256 then failwith "too many HTTP response headers"
    else
      match Eio.Buf_read.line reader with
      | "" -> ()
      | _ -> read_headers (count + 1)
  in
  read_headers 0;
  match response_status status_line with
  | Some status when status >= 200 && status < 300 -> Ok ()
  | Some status ->
      Error (Printf.sprintf "HTTP endpoint returned status %d" status)
  | None -> Error "HTTP endpoint returned an invalid response"

let post ~net ~endpoint ~headers ~content_type ~body =
  let host, port, target, host_header = endpoint_parts endpoint in
  let request = request ~target ~host_header ~headers ~content_type ~body in
  Eio.Net.with_tcp_connect ~host ~service:(string_of_int port) net (fun flow ->
      Eio.Flow.copy_string request flow;
      read_response flow)
