type t = {
  pull : unit -> string option;
  mutex : Eio.Mutex.t;
  mutable ended : bool;
}

let create ~read = { pull = read; mutex = Eio.Mutex.create (); ended = false }

let of_string value =
  let pending = ref (if String.length value = 0 then None else Some value) in
  let body =
    create ~read:(fun () ->
        match !pending with
        | None -> None
        | Some chunk ->
            pending := None;
            Some chunk)
  in
  if String.length value = 0 then body.ended <- true;
  body

let with_lock t fn =
  Eio.Mutex.lock t.mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock t.mutex) fn

let read_unlocked t =
  if t.ended then None
  else
    match t.pull () with
    | Some chunk -> Some chunk
    | None ->
        t.ended <- true;
        None

let read t = with_lock t (fun () -> read_unlocked t)

let read_all_with_prefix ~prefix t =
  with_lock t (fun () ->
      let rec drain chunks_rev =
        match read_unlocked t with
        | Some chunk -> drain (chunk :: chunks_rev)
        | None -> (
            match chunks_rev with
            | [] -> prefix
            | [ chunk ] when String.length prefix = 0 -> chunk
            | [ chunk ] -> prefix ^ chunk
            | chunks_rev ->
                let chunks = List.rev chunks_rev in
                String.concat ""
                  (if String.length prefix = 0 then chunks else prefix :: chunks)
            )
      in
      drain [])

let read_all t = read_all_with_prefix ~prefix:"" t
let is_complete t = t.ended

module Private = struct
  let create = create
  let of_string = of_string
end
