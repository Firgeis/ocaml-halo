open Effect
open Effect.Deep

type _ Effect.t += Fail : Error.t -> 'a Effect.t
type _ Effect.t += Finally : (unit -> unit) -> unit Effect.t
type _ Effect.t += Await_request_body : string option Effect.t
type _ Effect.t += Sleep_ms : int -> unit Effect.t
type _ Effect.t += Start_response_stream : Router.response_head -> unit Effect.t
type _ Effect.t += Write_response_chunk : string -> unit Effect.t
type _ Effect.t += Finish_response_stream : 'a Effect.t
type _ Effect.t +=
  Send_informational : Router.informational_response -> unit Effect.t
type _ Effect.t += On_client_disconnect : (unit -> unit) -> unit Effect.t
type _ Effect.t += Check_client_connected : unit Effect.t

exception Request_cancelled
exception Client_disconnected

let fail message = perform (Fail message)
let finalize_runtime cleanup = perform (Finally cleanup)
let await_request_body () = perform Await_request_body
let sleep_ms delay_ms = perform (Sleep_ms delay_ms)
let start_response_stream response = perform (Start_response_stream response)
let write_response_chunk chunk = perform (Write_response_chunk chunk)
let finish_response_stream () = perform Finish_response_stream
let send_informational response = perform (Send_informational response)
let on_client_disconnect cleanup = perform (On_client_disconnect cleanup)
let check_client_connected () = perform Check_client_connected

let or_fail_runtime = function
  | Ok value -> value
  | Error message -> fail (Error.Runtime_error message)

let or_fail_runtime_status = function
  | None -> ()
  | Some message -> fail (Error.Runtime_error message)

let run f =
  let cleanups = ref [] in
  let run_cleanups () =
    List.iter
      (fun cleanup ->
        try cleanup () with
        | exn -> prerr_endline (Printexc.to_string exn))
      (List.rev !cleanups)
  in
  let rec handle thunk =
    match_with thunk ()
      {
        retc =
          (fun result ->
            run_cleanups ();
            result);
        exnc =
          (fun exn ->
            run_cleanups ();
            raise exn);
        effc =
          (fun (type a) (eff : a Effect.t) ->
            match eff with
            | Finally cleanup ->
                Some
                  (fun (k : (a, ('b, Error.t) result) continuation) ->
                    cleanups := cleanup :: !cleanups;
                    handle (fun () -> continue k ()))
            | Fail message ->
                Some
                  (fun (_k : (a, ('b, Error.t) result) continuation) ->
                    run_cleanups ();
                    Error message)
            | _ -> None);
      }
  in
  handle f
