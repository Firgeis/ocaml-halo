# ocaml-halo -- Performance oriented HTTP server built on top of libuv

Halo is an HTTP server focused on performance. The rationale for this development is taking advantage of
modern OCaml to separate performance-oriented code handled in C on top of the following libraries:

- libuv
- llhttp
- s2n 

## Installation

System prerequisites:

- `make`
- a C compiler toolchain
- OpenSSL headers and libraries
- `pkg-config`

Install from a release tarball:

```sh
opam pin add ocaml-halo https://github.com/firge/ocaml-halo/releases/download/vX.Y.Z/ocaml-halo-X.Y.Z.tar.gz
opam install ocaml-halo
```

## Development

Halo currently targets Unix-like systems and expects a C toolchain plus OpenSSL development files.

System prerequisites for working from this repository:

- `make`
- a C compiler toolchain
- OpenSSL headers and libraries
- `pkg-config`

Fresh clones only contain stub directories under `vendor/`, so local development starts by fetching the
pinned upstream sources:

```sh
make vendor-fetch
make build
```

After the vendored trees are present, the build and test flows compile the required native artifacts as part
of the `dune` build. `make local-setup` is optional; it persists a detected `LIBCRYPTO_ROOT` for manual
builds on machines where OpenSSL is not found automatically.

## Basic Usage

```ocaml
let router () =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_end in
          fun _request -> text 200 "hello world\n"))

  let server_config router =
    Halo.Server.Config.create
      ~host:"127.0.0.1"
      ~port:8080
      router

  let run_server runtime =
    let router = router () in
    let config = server_config router in
    let server =
      Halo.Server.listen runtime config
      |> Halo.Server_effects.or_fail_runtime
    in
    Halo.Server_effects.finalize_runtime (fun () ->
        Halo.Server_effects.or_fail_runtime_status
          (Halo.Server.close_server server));
    try
      Halo.Server.run runtime;
      Ok ()
    with
    | Sys.Break ->
        Halo.Server.stop runtime;
        Ok ()

  let () =
    Sys.catch_break true;
    match Halo.Server.with_runtime run_server with
    | Ok () -> ()
    | Error err ->
        prerr_endline (Halo.Error.to_string err);
        exit 1
```

Notice the following:

- We are defining a basic route using Halo.Router.
- Use Halo.Server.Config.create to create a configuration.
- Halo.Server_effects.finalize_runtime registers a function to run when your server ends.
- Use Halo.Server_effects.or_fail_runtime to handle any startup errors.
- Halo.Server.with_runtime is a convenience wrapper that manages the server runtime lifecycle for you.

For lifecycle and error semantics, `Halo.Server.with_runtime` and `Halo.Server_effects.run` make cleanup work the same way on normal return, effect-based failure, and exceptions. Register server cleanup with `finalize_runtime`, use `or_fail_runtime` and `or_fail_runtime_status` to turn runtime strings into `Halo.Error.t`, and expect uncaught handler exceptions to become `500 Internal Server Error` responses. If a request is cancelled while waiting for more body data or while streaming, Halo raises `Halo.Server_effects.Request_cancelled` into that in-flight handler.

Here is an example handling request cancellation explicitly while still closing the listener through `finalize_runtime`:

```ocaml
let run_server runtime =
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is POST
          and+ () = path_segment "upload"
          and+ () = path_end in
          fun _request ->
            try
              let body =
                match Halo.Server_effects.await_request_body () with
                | None -> ""
                | Some value -> value
              in
              text 200 ("received:" ^ body)
            with
            | Halo.Server_effects.Request_cancelled ->
                text 499 "request cancelled\n"))
  in
  let config = Halo.Server.Config.create ~port:8080 router in
  let server =
    Halo.Server.listen runtime config
    |> Halo.Server_effects.or_fail_runtime
  in
  Halo.Server_effects.finalize_runtime (fun () ->
      Halo.Server_effects.or_fail_runtime_status
        (Halo.Server.close_server server));
  Halo.Server.run runtime;
  Ok ()
```

## Server configuration

### Network binding

Module: `Halo.Server.Config`

Configuration:
- `~host`, example: `"127.0.0.1"`
- `~port`, example: `8080`

### Connection queueing and limits

Module: `Halo.Server.Config`

Configuration:
- `~backlog`, example: `256`
- `~max_request_size`, example: `1024 * 1024`

### Timeouts

Module: `Halo.Server.Config`

Configuration:
- `~request_timeout_ms`, example: `30_000`
- `~idle_timeout_ms`, example: `5_000`

### TLS/HTTPS

Module: `Halo.Server.Config`

Configuration:
- `~tls:tls_config`
- `Halo.Server.Config.tls`
  - `cert_chain_path`, example: `"certs/fullchain.pem"`
  - `private_key_path`, example: `"certs/private-key.pem"`
  - `minimum_protocol_version`, example: `TLS13`
  - `alpn_protocols`, example: `["h2"; "http/1.1"]`
  - `ocsp_staple_path`, example: `"certs/ocsp.der"`
  - `client_auth`, example: `Halo.Server.Config.tls_client_auth Client_auth_optional`
  - `session_tickets_enabled`, example: `true`
  - `session_ticket_keys_path`, example: `"certs/ticket-keys.txt"`
  - `max_early_data_size`, example: `16384`
- `Halo.Server.Config.tls_client_auth`
  - `mode`, example: `Client_auth_required`
  - `ca_cert_path`, example: `"certs/client-ca.pem"`
  - `ca_cert_dir`, example: `"certs/client-ca"`

### Runtime/load control

Module: `Halo.Server.Config`

Configuration:
- `~controller:controller_config`
- `Halo.Server.Config.default_controller`
  - `sample_interval_ms`, example: `1000`
  - `busy_threshold`, example: `0.75`
  - `saturated_threshold`, example: `0.9`
  - `recovery_threshold`, example: `0.5`
  - `busy_samples`, example: `3`
  - `saturated_samples`, example: `5`
  - `recovery_samples`, example: `2`


## Router

The router is configured by applicatively applying the different router functions and router blocks together.

Here is a basic GET route matcher with parameters:

```ocaml
Halo.Router.(
  route
    Matcher.(
      let+ () = request_method_is GET
      and+ () = path_segment "users"
      and+ user_id = int_parameter
      and+ () = path_end in
      fun _request -> text 200 (string_of_int user_id)))
```

Remember to use `path_end` to terminate that specific path configuration.

Here is a POST example returning a JSON body:

```ocaml
Halo.Router.(
  route
    Matcher.(
      let+ () = request_method_is POST
      and+ () = path_segment "users"
      and+ () = path_end in
      fun _request -> json 201 "{\"status\":\"created\"}"))
```

As you can see, the json helper merely takes a string so you can use your favorite parser to return the body

```ocaml
Halo.Router.(
  route
    Matcher.(
      let+ () = request_method_is POST
      and+ () = path_segment "users"
      and+ user_id = int_parameter
      and+ () = path_end in
      fun _request ->
        let body =
          `Assoc
            [
              ("status", `String "created");
              ("user_id", `Int user_id);
            ]
          |> Yojson.Safe.to_string
        in
        json 201 body))
```

Use `scope` to group your routes and `require_body` to validate required bodies:

```ocaml
Halo.Router.(
  scope_path_segment "api"
    (route
       Matcher.(
         let+ () = request_method_is POST
         and+ () = path_segment "users"
         and+ user_id = int_parameter
         and+ () = path_segment "posts"
         and+ () = path_end
         and+ () = require_body in
         fun request ->
           text 201
             ("created post for user "
             ^ string_of_int user_id
             ^ " with body: "
             ^ Request.body request))))
```

You can also chain your router matchers:

```ocaml
Halo.Router.(
  route
    Matcher.(
      let+ () = request_method_is GET
      and+ () = path_segment "health"
      and+ () = path_end in
      fun _request -> text 200 "ok")
  <|>
  route
    Matcher.(
      let+ () = request_method_is GET
      and+ () = path_segment "version"
      and+ () = path_end in
      fun _request -> text 200 "v1"))
```


## Handling requests and headers

To optimize request and header parsing, these structures are kept in the C runtime and exposed through a borrowed request view. If you need to move the data outside the handler or modify it, materialize it first to avoid runtime exceptions from the lifetime checker.

Here is a bad example, storing `Halo.Router.Request.t` and using it after the handler returns:

```ocaml
let leaked_request : Halo.Router.Request.t option ref = ref None

let router =
  Halo.Router.(
    route
      Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "debug"
        and+ () = path_end in
        fun request ->
          leaked_request := Some request;
          text 200 "stored\n"))

let use_leaked_request_later () =
  match !leaked_request with
  | None -> ()
  | Some request ->
      ignore (Halo.Router.Request.raw_target request)
```

Good example, using `Halo.Router.Request.materialize` before the data escapes:

```ocaml
let saved_request : Halo.Router.request option ref = ref None

let router =
  Halo.Router.(
    route
      Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "debug"
        and+ () = path_end in
        fun request ->
          saved_request := Some (Halo.Router.Request.materialize request);
          text 200 "stored\n"))

let use_saved_request_later () =
  match !saved_request with
  | None -> ()
  | Some request ->
      ignore request.raw_target;
      ignore request.headers
```

If you only need the headers, use `Halo.Router.Request.copy_headers`:

```ocaml
let saved_headers : (string * string) list option ref = ref None

let router =
  Halo.Router.(
    route
      Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "debug"
        and+ () = path_end in
        fun request ->
          saved_headers := Some (Halo.Router.Request.copy_headers request);
          text 200 "stored\n"))
```

For query parameters, use the matcher helpers to decode optional, required, or repeated values directly in the route:

```ocaml
Halo.Router.(
  route
    Matcher.(
      let+ () = request_method_is GET
      and+ () = path_segment "search"
      and+ () = path_end
      and+ query = queryparam_required "q"
      and+ page = int_queryparam "page"
      and+ tags = queryparams "tag" in
      fun _request ->
        let page_text =
          match page with
          | None -> "1"
          | Some value -> string_of_int value
        in
        let tags_text =
          match tags with
          | [] -> "none"
          | values -> String.concat "," values
        in
        text 200
          ("query="
          ^ query
          ^ ", page="
          ^ page_text
          ^ ", tags="
          ^ tags_text)))
```

## Middleware
Middleware is divided into two different kinds: pre-routing middleware defined with `use_pre` and post-routing middleware defined with `use`.

Technically, pre-routing middleware participates in the routing pipeline itself, while post-routing middleware wraps an already selected handler.

In this API, pre-routing middleware returns a pre_result, which can:

- continue routing
- short-circuit with a response
- rewrite the request before matching

```ocaml
let rewrite_legacy request =
  match Halo.Router.Request.raw_target request with
  | "/legacy" ->
      Halo.Router.Rewrite
        {
          request_method = None;
          raw_target = Some "/";
          headers = None;
          body = None;
        }
  | "/forbidden" -> Halo.Router.Respond (Halo.Router.text 403 "forbidden\n")
  | _ -> Halo.Router.Continue

let add_example_header next request =
  next request |> Halo.Router.with_header "X-Example-Middleware" "post-route"

let router =
  Halo.Router.(
    route
      Matcher.(
        let+ () = request_method_is GET
        and+ () = path_end in
        fun _request -> text 200 "hello world\n")
    |> use_pre rewrite_legacy
    |> use add_example_header)
```

## Streaming and effects

For advanced request and response flows, use `Halo.Server_effects` inside your handlers. These runtime effects let you drain request bodies on demand, send informational responses, and stream response chunks without building the entire body first.

Here is an example draining the request body before responding:

```ocaml
Halo.Router.(
  route
    Matcher.(
      let+ () = request_method_is POST
      and+ () = path_segment "upload"
      and+ () = path_end in
      fun _request ->
        let body =
          match Halo.Server_effects.await_request_body () with
          | None -> ""
          | Some value -> value
        in
        text 200 ("received:" ^ body)))
```

For streaming responses, start the response head first and then write chunks incrementally:

```ocaml
Halo.Router.(
  route
    Matcher.(
      let+ () = request_method_is GET
      and+ () = path_segment "stream"
      and+ () = path_end in
      fun _request ->
        Halo.Server_effects.start_response_stream
          (response_head 200 [("Content-Type", "text/plain; charset=utf-8")]);
        Halo.Server_effects.write_response_chunk "hello ";
        Halo.Server_effects.write_response_chunk "world\n";
        Halo.Server_effects.finish_response_stream ()))
```

You can also send informational responses before the final response:

```ocaml
Halo.Router.(
  route
    Matcher.(
      let+ () = request_method_is GET
      and+ () = path_segment "early"
      and+ () = path_end in
      fun _request ->
        Halo.Server_effects.send_informational
          (informational 103
             [("Link", "</style.css>; rel=preload; as=style")]);
        text 200 "done\n"))
```

## TLS and HTTPS

To serve HTTPS, build a TLS configuration with `Halo.Server.Config.tls` and pass it to `Halo.Server.Config.create`. This keeps the normal router setup unchanged while letting you configure certificates, protocol versions, ALPN, client authentication, and session ticket behavior in one place.

Here is an example HTTPS configuration with TLS 1.3, HTTP/1.1 ALPN, optional client authentication, and session tickets enabled:

```ocaml
let router () =
  Halo.Router.(
    route
      Matcher.(
        let+ () = request_method_is GET
        and+ () = path_end in
        fun _request -> text 200 "secure hello\n"))

let tls_config =
  Halo.Server.Config.tls
    ~cert_chain_path:"certs/fullchain.pem"
    ~private_key_path:"certs/private-key.pem"
    ~minimum_protocol_version:Halo.Server.TLS13
    ~alpn_protocols:["http/1.1"]
    ~client_auth:
      (Halo.Server.Config.tls_client_auth
         ~ca_cert_path:"certs/client-ca.pem"
         Halo.Server.Client_auth_optional)
    ~session_tickets_enabled:true
    ~session_ticket_keys_path:"certs/ticket-keys.txt"
    ()

let server_config router =
  Halo.Server.Config.create
    ~host:"0.0.0.0"
    ~port:8443
    ~tls:tls_config
    router
```

If you do not need mutual TLS, omit the `client_auth` override and Halo will use `Client_auth_none`.
