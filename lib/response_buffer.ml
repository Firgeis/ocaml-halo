(* Domain-local leases. An occupied entry stays occupied across suspension;
   the bridge copies its bytes before an enqueue succeeds. *)
type t = { mutable data : bytes; mutable length : int; mutable busy : bool }

let max_retained = 64 * 1024
let slots = 16

let pool =
  Domain.DLS.new_key (fun () ->
      Array.init slots (fun i ->
          {
            data = Bytes.create (if i < 2 then 512 else 0);
            length = 0;
            busy = false;
          }))

let prewarm () = ignore (Domain.DLS.get pool)

let acquire () =
  let entries = Domain.DLS.get pool in
  let rec find i =
    if i = Array.length entries then
      { data = Bytes.create 512; length = 0; busy = true }
    else if entries.(i).busy then find (i + 1)
    else
      let entry = entries.(i) in
      entry.busy <- true;
      entry
  in
  find 0

let release t =
  t.length <- 0;
  if Bytes.length t.data > max_retained then t.data <- Bytes.empty;
  t.busy <- false

let with_buffer f =
  let buffer = acquire () in
  Fun.protect ~finally:(fun () -> release buffer) (fun () -> f buffer)

let ensure t extra =
  if extra > Sys.max_string_length - t.length then
    invalid_arg "response too large";
  let needed = t.length + extra in
  if needed > Bytes.length t.data then (
    let rec size n =
      if n >= needed then n
      else if n > Sys.max_string_length / 2 then needed
      else size (n * 2)
    in
    let data = Bytes.create (size (max 512 (Bytes.length t.data))) in
    Bytes.blit t.data 0 data 0 t.length;
    t.data <- data)

let add_char t c =
  ensure t 1;
  Bytes.set t.data t.length c;
  t.length <- t.length + 1

let add_string t s =
  let len = String.length s in
  ensure t len;
  Bytes.blit_string s 0 t.data t.length len;
  t.length <- t.length + len

let add_substring t s offset len =
  ensure t len;
  Bytes.blit_string s offset t.data t.length len;
  t.length <- t.length + len

let clear t = t.length <- 0
let data t = t.data
let length t = t.length
