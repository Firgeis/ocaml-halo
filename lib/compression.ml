type encoding = Identity | Gzip | Brotli

let encoding_name = function
  | Identity -> "identity"
  | Gzip -> "gzip"
  | Brotli -> "br"

let parse_quality value =
  match String.split_on_char '.' value with
  | [ "0" ] -> Some 0
  | [ "1" ] -> Some 1000
  | [ "0"; fraction ]
    when String.length fraction <= 3
         && String.for_all Ascii_string.is_digit fraction ->
      let padded = fraction ^ String.make (3 - String.length fraction) '0' in
      int_of_string_opt padded
  | [ "1"; fraction ]
    when String.length fraction <= 3 && String.for_all (Char.equal '0') fraction
    ->
      Some 1000
  | _ -> None

let parse_quality_parameter parameter =
  match String.split_on_char '=' parameter with
  | [ name; value ]
    when String.equal
           (String.lowercase_ascii (Ascii_string.trim_optional_whitespace name))
           "q" ->
      Option.value
        (parse_quality (Ascii_string.trim_optional_whitespace value))
        ~default:0
  | _ -> 0

let parse_coding item =
  match String.split_on_char ';' item with
  | [] -> None
  | coding :: parameters ->
      let coding =
        String.lowercase_ascii (Ascii_string.trim_optional_whitespace coding)
      in
      if not (Router.Headers.is_valid_name coding) then None
      else
        let quality =
          match parameters with
          | [] -> 1000
          | [ parameter ] -> parse_quality_parameter parameter
          | _ -> 0
        in
        Some (coding, quality)

let parse_accept_encoding values =
  let add_coding accepted (coding, quality) =
    let quality =
      match List.assoc_opt coding accepted with
      | None -> quality
      | Some previous -> min previous quality
    in
    (coding, quality) :: List.remove_assoc coding accepted
  in
  values
  |> List.concat_map (String.split_on_char ',')
  |> List.filter_map parse_coding
  |> List.fold_left add_coding []

let encoding_quality accepted encoding =
  match List.assoc_opt (encoding_name encoding) accepted with
  | Some quality -> quality
  | None -> (
      match (encoding, List.assoc_opt "*" accepted) with
      | Identity, Some 0 -> 0
      | Identity, _ ->
          (* Identity remains a fallback unless explicitly excluded. *)
          1
      | (Gzip | Brotli), wildcard -> Option.value wildcard ~default:0)

let preference = function Brotli -> 3 | Gzip -> 2 | Identity -> 1

let select_representation ~encoding_of candidates values =
  let identity =
    match values with
    | [] ->
        List.find_opt
          (fun candidate ->
            match encoding_of candidate with
            | Identity -> true
            | Gzip | Brotli -> false)
          candidates
    | _ -> None
  in
  match identity with
  | Some _ -> identity
  | None -> (
      let accepted = parse_accept_encoding values in
      let quality encoding =
        match values with [] -> 1000 | _ -> encoding_quality accepted encoding
      in
      let choose best candidate =
        let encoding = encoding_of candidate in
        let candidate_quality = quality encoding in
        let candidate_preference = preference encoding in
        if candidate_quality = 0 then best
        else
          match best with
          | Some (_, best_quality, best_preference)
            when candidate_quality < best_quality
                 || candidate_quality = best_quality
                    && candidate_preference <= best_preference ->
              best
          | _ -> Some (candidate, candidate_quality, candidate_preference)
      in
      match List.fold_left choose None candidates with
      | None -> None
      | Some (candidate, _, _) -> Some candidate)

let negotiate ~available values =
  select_representation ~encoding_of:Fun.id available values

let media_type value =
  let value =
    match String.index_opt value ';' with
    | None -> value
    | Some index -> String.sub value 0 index
  in
  String.lowercase_ascii (Ascii_string.trim_optional_whitespace value)

let is_compressible_content_type value =
  match media_type value with
  | "text/event-stream" -> false
  | "application/json" | "application/xml" | "application/javascript"
  | "application/x-javascript" | "application/ecmascript" | "image/svg+xml" ->
      true
  | content_type ->
      String.starts_with ~prefix:"text/" content_type
      || String.starts_with ~prefix:"application/" content_type
         && (String.ends_with ~suffix:"+json" content_type
            || String.ends_with ~suffix:"+xml" content_type)

external gzip_native : string -> int -> string = "ocaml_halo_gzip"

let validate_options ~min_body_size ~level =
  if min_body_size < 0 then
    invalid_arg "Compression: min_body_size must be nonnegative";
  if level < 0 || level > 9 then
    invalid_arg "Compression: gzip level must be between 0 and 9"

let gzip ?(level = 6) body =
  validate_options ~min_body_size:0 ~level;
  gzip_native body level

let header_values headers name =
  Router.Headers.to_list headers
  |> List.filter_map (fun (key, value) ->
      if String.equal (String.lowercase_ascii key) name then Some value
      else None)

let has_header headers name =
  Router.Headers.to_list headers
  |> List.exists (fun (key, _) ->
      String.equal (String.lowercase_ascii key) name)

let remove_headers names headers =
  Router.Headers.to_list headers
  |> List.filter (fun (key, _) ->
      not (List.mem (String.lowercase_ascii key) names))
  |> Router.Headers.of_list_exn

let vary headers =
  let tokens =
    header_values headers "vary"
    |> List.concat_map (String.split_on_char ',')
    |> List.map Ascii_string.trim_optional_whitespace
    |> List.filter (fun token -> token <> "")
  in
  if List.mem "*" tokens then headers
  else
    let add_token (seen, reversed) token =
      let normalized = String.lowercase_ascii token in
      if List.mem normalized seen then (seen, reversed)
      else (normalized :: seen, token :: reversed)
    in
    let _, reversed =
      List.fold_left add_token ([], []) (tokens @ [ "Accept-Encoding" ])
    in
    let tokens = List.rev reversed in
    Router.Headers.set_exn "Vary" (String.concat ", " tokens) headers

type list_state = Unquoted | Quoted | Escaped

(* HTTP list fields can contain commas inside quoted strings. *)
let split_http_list value =
  let rec loop start index state result =
    if index = String.length value then
      List.rev (String.sub value start (index - start) :: result)
    else
      match (state, value.[index]) with
      | Unquoted, ',' ->
          loop (index + 1) (index + 1) Unquoted
            (String.sub value start (index - start) :: result)
      | Unquoted, '"' -> loop start (index + 1) Quoted result
      | Quoted, '\\' -> loop start (index + 1) Escaped result
      | Quoted, '"' -> loop start (index + 1) Unquoted result
      | Escaped, _ -> loop start (index + 1) Quoted result
      | (Unquoted | Quoted), _ -> loop start (index + 1) state result
  in
  loop 0 0 Unquoted []

let has_no_transform values =
  values
  |> List.concat_map split_http_list
  |> List.exists (fun directive ->
      let name =
        match String.index_opt directive '=' with
        | None -> directive
        | Some index -> String.sub directive 0 index
      in
      String.equal
        (String.lowercase_ascii (Ascii_string.trim_optional_whitespace name))
        "no-transform")

let no_transform request headers =
  has_no_transform (header_values headers "cache-control")
  || has_no_transform (Router.Request.headers request "cache-control")

let digest_headers =
  [ "content-md5"; "digest"; "content-digest"; "repr-digest" ]

let weaken_etag headers =
  let etags = header_values headers "etag" in
  let headers = remove_headers [ "etag" ] headers in
  match etags with
  | [ etag ] ->
      let etag = Ascii_string.trim_optional_whitespace etag in
      if
        String.starts_with ~prefix:"W/\"" etag
        && String.ends_with ~suffix:"\"" etag
      then Router.Headers.set_exn "ETag" etag headers
      else if
        String.starts_with ~prefix:"\"" etag
        && String.ends_with ~suffix:"\"" etag
      then Router.Headers.set_exn "ETag" ("W/" ^ etag) headers
      else headers
  | _ -> headers

let not_acceptable headers =
  let headers =
    remove_headers
      (digest_headers
      @ [
          "etag";
          "last-modified";
          "content-length";
          "content-encoding";
          "content-range";
          "content-type";
        ])
      headers
    |> vary
  in
  Router.make ~status:Router.Status.not_acceptable ~headers ""

let bodyless request status =
  match Router.Status.to_int status with
  | 204 | 205 | 304 -> true
  | code when code >= 200 && code < 300 ->
      Router.Request.request_method request = Some Router.CONNECT
  | _ -> false

let compression_allowed ~compressible request response =
  let headers = Router.Response.headers response in
  let content_type =
    match header_values headers "content-type" with
    | [ value ] -> Some (media_type value)
    | _ -> None
  in
  match content_type with
  | None | Some ("" | "text/event-stream") -> false
  | Some content_type ->
      compressible content_type
      && Router.Status.to_int (Router.Response.status response) <> 206
      && (not (has_header headers "content-range"))
      && not (no_transform request headers)

let identity_response ~identity_accepted response headers =
  if identity_accepted then Router.with_headers headers response
  else not_acceptable headers

let gzip_response ~level ~identity_accepted response headers body =
  let compressed = gzip_native body level in
  if String.length compressed >= String.length body && identity_accepted then
    Router.with_headers headers response
  else
    let headers =
      remove_headers ("accept-ranges" :: digest_headers) headers
      |> weaken_etag
      |> Router.Headers.set_exn "Content-Encoding" "gzip"
      |> Router.Headers.set_exn "Content-Length"
           (string_of_int (String.length compressed))
    in
    Router.make ~status:(Router.Response.status response) ~headers compressed

let apply_with_options ~min_body_size ~level ~compressible request response =
  let status = Router.Response.status response in
  let headers = Router.Response.headers response in
  if bodyless request status || has_header headers "content-encoding" then
    response
  else
    let values = Router.Request.headers request "accept-encoding" in
    let headers = vary headers in
    let identity_accepted =
      Option.is_some (negotiate ~available:[ Identity ] values)
    in
    let encoding = negotiate ~available:[ Gzip; Identity ] values in
    match (Router.Response.body response, encoding) with
    | Router.Fixed_body body, Some Gzip
      when compression_allowed ~compressible request response
           && (String.length body >= min_body_size || not identity_accepted) ->
        gzip_response ~level ~identity_accepted response headers body
    | _ -> identity_response ~identity_accepted response headers

let apply ?(min_body_size = 1024) ?(level = 6)
    ?(compressible = is_compressible_content_type) request response =
  validate_options ~min_body_size ~level;
  apply_with_options ~min_body_size ~level ~compressible request response

let middleware ?(min_body_size = 1024) ?(level = 6)
    ?(compressible = is_compressible_content_type) () =
  validate_options ~min_body_size ~level;
  fun next request ->
    apply_with_options ~min_body_size ~level ~compressible request
      (next request)

let stat_regular path =
  match Eio.Path.stat ~follow:true path with
  | stat when stat.Eio.File.Stat.kind = `Regular_file -> Some stat
  | _ -> None
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> None

let load_opt path =
  try Some (Eio.Path.load path)
  with Eio.Io (Eio.Fs.E (Not_found _), _) -> None

let weak_etag value =
  let value = Ascii_string.trim_optional_whitespace value in
  if String.starts_with ~prefix:"W/" value then
    String.sub value 2 (String.length value - 2)
  else value

let etag_matches request etag =
  Router.Request.headers request "if-none-match"
  |> List.concat_map split_http_list
  |> List.exists (fun token ->
      String.equal (Ascii_string.trim_optional_whitespace token) "*"
      || String.equal (weak_etag token) etag)

let asset_candidates path ~modified =
  let root, name = path in
  let is_current (_, sibling) =
    match stat_regular sibling with
    | Some stat -> stat.Eio.File.Stat.mtime >= modified
    | None -> false
  in
  let sidecars =
    [ (Brotli, (root, name ^ ".br")); (Gzip, (root, name ^ ".gz")) ]
    |> List.filter is_current
  in
  (Identity, path) :: sidecars

let asset_response request headers encoding body =
  let etag =
    Printf.sprintf "\"%s-%s\"" (encoding_name encoding)
      (Digest.to_hex (Digest.string body))
  in
  let headers =
    Router.Headers.set_exn "ETag" etag headers
    |> Router.Headers.set_exn "Content-Length"
         (string_of_int (String.length body))
  in
  let headers =
    match encoding with
    | Identity -> headers
    | Gzip | Brotli ->
        Router.Headers.set_exn "Content-Encoding" (encoding_name encoding)
          headers
  in
  if etag_matches request etag then
    Router.make ~status:Router.Status.not_modified ~headers ""
  else Router.make ~status:Router.Status.ok ~headers body

let rec serve_asset request headers candidates =
  let selected =
    select_representation ~encoding_of:fst candidates
      (Router.Request.headers request "accept-encoding")
  in
  match selected with
  | None -> not_acceptable headers
  | Some (encoding, path) -> (
      match (encoding, load_opt path) with
      | _, Some body -> asset_response request headers encoding body
      | Identity, None -> Router.text Router.Status.not_found "not found\n"
      | (Gzip | Brotli), None ->
          serve_asset request headers (List.remove_assoc encoding candidates))

let serve_file ~headers ~compressible ~content_type path request =
  match stat_regular path with
  | None -> Router.text Router.Status.not_found "not found\n"
  | Some original ->
      let candidates =
        if
          String.equal content_type "text/event-stream"
          || (not (compressible content_type))
          || no_transform request headers
        then [ (Identity, path) ]
        else asset_candidates path ~modified:original.Eio.File.Stat.mtime
      in
      serve_asset request headers candidates

let file ?(headers = Router.Headers.empty)
    ?(compressible = is_compressible_content_type) ~content_type path =
  let headers =
    remove_headers
      (digest_headers
      @ [
          "content-encoding";
          "content-length";
          "etag";
          "content-range";
          "last-modified";
        ])
      headers
    |> Router.Headers.set_exn "Content-Type" content_type
    |> Router.Headers.set_exn "Accept-Ranges" "none"
    |> vary
  in
  let content_type = media_type content_type in
  fun request ->
    match Router.Request.request_method request with
    | Some (Router.GET | Router.HEAD) ->
        serve_file ~headers ~compressible ~content_type path request
    | _ ->
        Router.text Router.Status.method_not_allowed "method not allowed\n"
        |> Router.with_header "Allow" "GET, HEAD"
