type header_lists = (string * string) list * (string * string) list

(* The bridge constructs this record. [packed] owns its bytes in the OCaml
   heap, so decoding remains safe after the C event and connection are gone. *)
type t = { packed : string; mutable decoded : header_lists option }

external decode : string -> header_lists = "ocaml_halo_bridge_decode_headers"

let lists t =
  match t.decoded with
  | Some lists -> lists
  | None ->
      let lists = decode t.packed in
      t.decoded <- Some lists;
      lists

let headers t = fst (lists t)
let raw_headers t = snd (lists t)
