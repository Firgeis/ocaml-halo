type producer = yield:(string -> unit) -> unit

let validate_chunk chunk =
  if String.length chunk = 0 then
    invalid_arg "streamed response chunks must not be empty"

let respond ?(headers = Router.Headers.empty) ~status producer =
  Router.stream ~status ~headers (fun ~yield ->
      producer ~yield:(fun chunk ->
          validate_chunk chunk;
          yield chunk))

type buffered_producer = yield:(string -> unit) -> flush:(unit -> unit) -> unit

let respond_buffered ?headers ?(max_buffered_bytes = 64 * 1024) ~status producer
    =
  if max_buffered_bytes <= 0 then
    invalid_arg "max_buffered_bytes must be positive";
  Router.Private.stream_bytes ?headers ~status (fun ~yield:send ->
      Response_buffer.with_buffer (fun buffer ->
          let mutex = Eio.Mutex.create () in
          let active = ref true in
          let failure = ref None in
          let check_active () =
            if not !active then
              invalid_arg "buffered stream producer has returned";
            Option.iter
              (fun (exn, backtrace) ->
                Printexc.raise_with_backtrace exn backtrace)
              !failure
          in
          let with_lock fn =
            Eio.Mutex.lock mutex;
            Fun.protect
              ~finally:(fun () -> Eio.Mutex.unlock mutex)
              (fun () ->
                check_active ();
                fn ())
          in
          let flush_unlocked () =
            if Response_buffer.length buffer > 0 then (
              try
                send
                  (Response_buffer.data buffer)
                  (Response_buffer.length buffer);
                Response_buffer.clear buffer
              with exn ->
                let backtrace = Printexc.get_raw_backtrace () in
                failure := Some (exn, backtrace);
                Printexc.raise_with_backtrace exn backtrace)
          in
          let yield chunk =
            validate_chunk chunk;
            with_lock (fun () ->
                let rec append offset =
                  if offset < String.length chunk then (
                    let length =
                      min
                        (max_buffered_bytes - Response_buffer.length buffer)
                        (String.length chunk - offset)
                    in
                    Response_buffer.add_substring buffer chunk offset length;
                    if Response_buffer.length buffer = max_buffered_bytes then
                      flush_unlocked ();
                    append (offset + length))
                in
                append 0)
          in
          let flush () = with_lock flush_unlocked in
          Fun.protect
            ~finally:(fun () ->
              active := false;
              Response_buffer.clear buffer)
            (fun () ->
              producer ~yield ~flush;
              flush ())))
