type header_mode = Forwarded_only | X_forwarded_only | Prefer_forwarded
type legacy_metadata_mode = Aligned_lists | Nearest_proxy_singletons
type status = Disabled | Untrusted_peer | Absent | Applied | Partial | Ignored
type source = Direct | Forwarded | X_forwarded

type issue =
  | Malformed_input
  | Incomplete_chain
  | Unsupported_value
  | Metadata_misalignment
  | Resource_limit

type info = {
  status : status;
  client_address_source : source;
  host_source : source;
  scheme_source : source;
  issues : issue list;
}

type t = {
  networks : Ipaddr.Prefix.t list;
  mode : header_mode;
  legacy : legacy_metadata_mode;
}

(* Ipaddr accepts IPv6 hextets with more than four digits when the value fits.
   Reject those spellings; the dotted IPv4 tail is validated by Ipaddr. *)
let valid_ipv6_components address =
  (not (String.contains address ':'))
  || List.for_all
       (fun component ->
         String.contains component '.' || String.length component <= 4)
       (String.split_on_char ':' address)

let create ~trusted_proxies ~header_mode ?(legacy_metadata_mode = Aligned_lists)
    () =
  let rec parse acc = function
    | [] ->
        Ok
          {
            networks = List.rev acc;
            mode = header_mode;
            legacy = legacy_metadata_mode;
          }
    | value :: rest -> (
        let address =
          match String.index_opt value '/' with
          | Some index -> String.sub value 0 index
          | None -> value
        in
        let parsed =
          if not (valid_ipv6_components address) then
            Error (`Msg "invalid IPv6 component")
          else if String.contains value '/' then Ipaddr.Prefix.of_string value
          else Result.map Ipaddr.Prefix.of_addr (Ipaddr.of_string value)
        in
        match parsed with
        | Ok network -> parse (network :: acc) rest
        | Error _ -> Error ("invalid trusted proxy: " ^ value))
  in
  parse [] trusted_proxies

(* Parsing errors are local to the scanner; policy decisions use results. *)
exception Bad
exception Limit

let scan f =
  try Ok (f ()) with
  | Bad -> Error Malformed_input
  | Limit -> Error Resource_limit

let trim = Ascii_string.trim_optional_whitespace

let token s =
  String.length s > 0 && String.for_all Ascii_string.is_http_token_char s

let split separator s =
  match Http_syntax.split_quoted ~strict:true ~separator s with
  | Ok parts -> parts
  | Error () -> raise Bad

let value s =
  let n = String.length s in
  if token s then s
  else if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then
    let b = Buffer.create n in
    let rec loop i =
      if i = n - 1 then Buffer.contents b
      else if i >= n - 1 then raise Bad
      else
        match s.[i] with
        | '"' -> raise Bad
        | '\\' ->
            if i + 1 >= n - 1 then raise Bad
            else (
              Buffer.add_char b s.[i + 1];
              loop (i + 2))
        | c ->
            Buffer.add_char b c;
            loop (i + 1)
    in
    loop 1
  else raise Bad

let list_values split_field values =
  let items = List.concat_map split_field values in
  if List.length items > 64 then raise Limit;
  List.map trim items |> List.filter (fun x -> x <> "")

let http_list values = list_values (split ',') values

(* XFF has no quoted-string grammar. An untrusted prefix cannot quote a comma. *)
let xff_list values = list_values (String.split_on_char ',') values

type entry = {
  node : string option;
  host : string option;
  proto : string option;
}

type family = Standard | Legacy

type selected = {
  family : family;
  primary : string list;
  hosts : string list;
  protos : string list;
  size : int;
}

let select_headers mode headers =
  let has_forwarded =
    List.exists (fun (n, _) -> String.lowercase_ascii n = "forwarded") headers
  in
  let family =
    match mode with
    | Forwarded_only -> Standard
    | X_forwarded_only -> Legacy
    | Prefer_forwarded -> if has_forwarded then Standard else Legacy
  in
  let empty = { family; primary = []; hosts = []; protos = []; size = 0 } in
  let selected =
    List.fold_left
      (fun acc (name, value) ->
        let name = String.lowercase_ascii name in
        let size = acc.size + String.length name + String.length value in
        match (family, name) with
        | Standard, "forwarded" | Legacy, "x-forwarded-for" ->
            { acc with primary = value :: acc.primary; size }
        | Legacy, "x-forwarded-host" ->
            { acc with hosts = value :: acc.hosts; size }
        | Legacy, "x-forwarded-proto" ->
            { acc with protos = value :: acc.protos; size }
        | _ -> acc)
      empty headers
  in
  {
    selected with
    primary = List.rev selected.primary;
    hosts = List.rev selected.hosts;
    protos = List.rev selected.protos;
  }

let enforce_limits selected =
  if selected.size > 16384 then Error Resource_limit
  else
    let positions values =
      List.fold_left
        (fun n v -> n + List.length (String.split_on_char ',' v))
        0 values
    in
    if
      selected.family = Legacy
      && List.exists
           (fun values -> positions values > 64)
           [ selected.primary; selected.hosts; selected.protos ]
    then Error Resource_limit
    else Ok ()

let parse_entry element =
  let params = split ';' element in
  let pairs =
    List.fold_left
      (fun acc part ->
        if part = "" then acc
        else
          match String.index_opt part '=' with
          | None -> raise Bad
          | Some i ->
              let name = String.sub part 0 i |> String.lowercase_ascii in
              if (not (token name)) || List.mem_assoc name acc then raise Bad;
              ( name,
                value (String.sub part (i + 1) (String.length part - i - 1)) )
              :: acc)
      [] params
  in
  {
    node = List.assoc_opt "for" pairs;
    host = List.assoc_opt "host" pairs;
    proto = List.assoc_opt "proto" pairs;
  }

let parse_chain selected =
  scan (fun () ->
      let entries =
        match selected.family with
        | Standard -> http_list selected.primary |> List.map parse_entry
        | Legacy ->
            xff_list selected.primary
            |> List.map (fun node ->
                { node = Some node; host = None; proto = None })
      in
      Array.of_list entries)

let port ~legacy s =
  String.length s > 0
  && ((legacy || String.length s <= 5)
      && String.for_all Ascii_string.is_digit s
     || s.[0] = '_'
        && String.length s > 1
        && String.for_all
             (function
               | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' -> true
               | _ -> false)
             s)

let node ~legacy s =
  let len = String.length s in
  let obfuscated s =
    String.length s > 1
    && s.[0] = '_'
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' | '_' | '-' -> true
           | _ -> false)
         s
  in
  let unknown s = String.lowercase_ascii s = "unknown" || obfuscated s in
  let incomplete =
    if unknown s then true
    else
      match String.index_opt s ':' with
      | Some i ->
          let n = String.sub s 0 i and p = String.sub s (i + 1) (len - i - 1) in
          unknown n && port ~legacy p
      | None -> false
  in
  if len = 0 || incomplete then None
  else
    let address =
      if s.[0] = '[' then
        match String.index_opt s ']' with
        | None -> raise Bad
        | Some i -> (
            let suffix = String.sub s (i + 1) (len - i - 1) in
            if
              suffix <> ""
              && (suffix.[0] <> ':'
                 || (not
                       (port ~legacy
                          (String.sub suffix 1 (String.length suffix - 1))))
                 || (legacy && String.length suffix > 1 && suffix.[1] = '_'))
            then raise Bad;
            let ip = String.sub s 1 (i - 1) in
            if not (valid_ipv6_components ip) then raise Bad;
            match Ipaddr.of_string ip with
            | Ok (Ipaddr.V6 _ as address) -> address
            | _ -> raise Bad)
      else
        match Ipaddr.of_string s with
        | Ok (Ipaddr.V6 _) when (not legacy) || not (valid_ipv6_components s) ->
            raise Bad
        | Ok address -> address
        | Error _ -> (
            match String.index_opt s ':' with
            | Some i -> (
                let ip = String.sub s 0 i
                and p = String.sub s (i + 1) (len - i - 1) in
                if
                  (not (port ~legacy p))
                  || (legacy && String.length p > 0 && p.[0] = '_')
                then raise Bad;
                match Ipaddr.of_string ip with
                | Ok (Ipaddr.V4 _ as address) -> address
                | _ -> raise Bad)
            | None -> raise Bad)
    in
    Some address

let trusted policy ip =
  List.exists
    (fun prefix ->
      match (ip, prefix) with
      | Ipaddr.V4 _, Ipaddr.V4 _ | Ipaddr.V6 _, Ipaddr.V6 _ ->
          Ipaddr.Prefix.mem ip prefix
      | _ -> false)
    policy.networks

let find_boundary policy ~peer family entries =
  let rec walk index current =
    if index < 0 || not (trusted policy current) then Ok (index + 1, current)
    else
      match entries.(index).node with
      | None -> Error Incomplete_chain
      | Some raw -> (
          match scan (fun () -> node ~legacy:(family = Legacy) raw) with
          | Error issue -> Error issue
          | Ok None -> Error Incomplete_chain
          | Ok (Some address) -> walk (index - 1) address)
  in
  if Array.length entries = 0 then Error Incomplete_chain
  else walk (Array.length entries - 1) peer

let legacy_metadata mode ~count ~index values =
  if values = [] then Ok None
  else
    match scan (fun () -> http_list values) with
    | Error issue -> Error issue
    | Ok items ->
        let expected =
          match mode with
          | Aligned_lists -> count
          | Nearest_proxy_singletons -> 1
        in
        if List.length items <> expected then Error Metadata_misalignment
        else
          Ok
            (Some
               (List.nth items
                  (match mode with
                  | Aligned_lists -> index
                  | Nearest_proxy_singletons -> 0)))

let select_metadata policy selected entries index =
  match selected.family with
  | Standard -> (Ok entries.(index).host, Ok entries.(index).proto)
  | Legacy ->
      let select =
        legacy_metadata policy.legacy ~count:(Array.length entries) ~index
      in
      (select selected.hosts, select selected.protos)

let resolve_host = function
  | Error issue -> (None, [ issue ])
  | Ok None -> (None, [])
  | Ok (Some raw) -> (
      match Host.parse_forwarded raw with
      | Ok host -> (Some host, [])
      | Error Host.Malformed -> (None, [ Malformed_input ])
      | Error Host.Unsupported -> (None, [ Unsupported_value ]))

let resolve_scheme = function
  | Error issue -> (None, [ issue ])
  | Ok None -> (None, [])
  | Ok (Some raw) ->
      let scheme = String.lowercase_ascii raw in
      if scheme = "http" || scheme = "https" then (Some scheme, [])
      else
        let valid =
          String.length scheme > 0
          && Ascii_string.is_lowercase_letter scheme.[0]
          && String.for_all
               (fun c ->
                 Ascii_string.is_lowercase_letter c
                 || Ascii_string.is_digit c || c = '+' || c = '-' || c = '.')
               scheme
        in
        (None, [ (if valid then Unsupported_value else Malformed_input) ])

type resolution = {
  info : info;
  address : Ipaddr.t option;
  host : Host.t option;
  scheme : string option;
}

let info status issues =
  {
    status;
    client_address_source = Direct;
    host_source = Direct;
    scheme_source = Direct;
    issues;
  }

let fallback status issues =
  { info = info status issues; address = None; host = None; scheme = None }

let applied selected address (host, host_issues) (scheme, scheme_issues) =
  let issues = host_issues @ scheme_issues in
  let source =
    match selected.family with Standard -> Forwarded | Legacy -> X_forwarded
  in
  {
    address = Some address;
    host;
    scheme;
    info =
      {
        status = (if issues = [] then Applied else Partial);
        client_address_source = source;
        host_source = (if Option.is_some host then source else Direct);
        scheme_source = (if Option.is_some scheme then source else Direct);
        issues;
      };
  }

let resolve policy ~peer headers =
  let selected = select_headers policy.mode headers in
  let result =
    Result.bind (enforce_limits selected) (fun () ->
        if selected.primary = [] then Ok (fallback Absent [])
        else
          Result.bind (parse_chain selected) (fun entries ->
              Result.map
                (fun (index, address) ->
                  let host, scheme =
                    select_metadata policy selected entries index
                  in
                  applied selected address (resolve_host host)
                    (resolve_scheme scheme))
                (find_boundary policy ~peer selected.family entries)))
  in
  match result with
  | Ok resolution -> resolution
  | Error issue -> fallback Ignored [ issue ]

module Private = struct
  type nonrec resolution = resolution

  let disabled = info Disabled []
  let untrusted = fallback Untrusted_peer []
  let trusted = trusted
  let resolve = resolve
  let info resolution = resolution.info
  let address resolution = resolution.address

  let host resolution =
    Option.map
      (fun (host : Host.t) -> (host.hostname, host.port))
      resolution.host

  let scheme resolution = resolution.scheme
end
