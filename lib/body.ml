type t = {
  pull : unit -> string option;
  mutex : Eio.Mutex.t;
  mutable chunks_rev : string list;
  mutable ended : bool;
  mutable cached : string option;
}

let create ~read =
  {
    pull = read;
    mutex = Eio.Mutex.create ();
    chunks_rev = [];
    ended = false;
    cached = None;
  }

let of_string value =
  let pending = ref (if String.length value = 0 then None else Some value) in
  let body = create ~read:(fun () ->
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
    | Some chunk ->
        t.chunks_rev <- chunk :: t.chunks_rev;
        Some chunk
    | None ->
        t.ended <- true;
        None

let read t = with_lock t (fun () -> read_unlocked t)

let read_all t =
  with_lock t (fun () ->
      match t.cached with
      | Some body -> body
      | None ->
          while Option.is_some (read_unlocked t) do
            ()
          done;
          let body =
            match t.chunks_rev with
            | [] -> ""
            | [ chunk ] -> chunk
            | chunks_rev -> String.concat "" (List.rev chunks_rev)
          in
          t.chunks_rev <- [];
          t.cached <- Some body;
          body)

let is_complete t = t.ended

module Private = struct
  let create = create
  let of_string = of_string
end
