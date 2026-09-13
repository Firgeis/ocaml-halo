type media_type = {
  type_ : string;
  subtype : string;
  parameters : (string * string) list;
}

let normalize_name name = String.lowercase_ascii name
let trim = String.trim

let split_once character value =
  match String.index_opt value character with
  | None -> None
  | Some index ->
      let left = String.sub value 0 index in
      let right =
        String.sub value (index + 1) (String.length value - index - 1)
      in
      Some (left, right)

let header (request : Router.request) name =
  let wanted = normalize_name name in
  request.Router.headers
  |> List.find_map (fun (header_name, value) ->
      if String.equal (normalize_name header_name) wanted then Some value
      else None)

let headers (request : Router.request) name =
  let wanted = normalize_name name in
  request.Router.headers
  |> List.filter_map (fun (header_name, value) ->
      if String.equal (normalize_name header_name) wanted then Some value
      else None)

let parse_parameter parameter =
  match split_once '=' parameter with
  | None -> None
  | Some (name, value) -> Some (trim name, trim value)

let parse_media_type value =
  let parts = String.split_on_char ';' value |> List.map trim in
  match parts with
  | [] -> None
  | media_type :: parameters -> (
      match split_once '/' media_type with
      | None -> None
      | Some (type_, subtype) ->
          let type_ = trim type_ in
          let subtype = trim subtype in
          if String.equal type_ "" || String.equal subtype "" then None
          else
            let parameters = List.filter_map parse_parameter parameters in
            Some { type_; subtype; parameters })

let content_type (request : Router.request) =
  match header request "content-type" with
  | None -> None
  | Some value -> parse_media_type value
