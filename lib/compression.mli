(** Buffered response compression and precompressed assets. *)

type encoding = Identity | Gzip | Brotli

val negotiate : available:encoding list -> string list -> encoding option
(** Select an available representation from the values of all [Accept-Encoding]
    fields. An absent field ([]) prefers identity; an empty field ([" "])
    permits only identity. Explicit quality values and exclusions override [*].
    An implicit identity is a fallback; an explicit identity participates in
    quality comparison. Ties prefer Brotli, then gzip, then identity.

    Malformed quality values exclude that coding. Conflicting duplicate codings
    use the lowest quality. [None] means no available representation is
    accepted. *)

val is_compressible_content_type : string -> bool
(** Default predicate for text, JSON, XML, JavaScript and SVG. Parameters and
    case are ignored. SSE and already compressed formats are excluded. *)

val gzip : ?level:int -> string -> string
(** Produce a gzip member using zlib. The default level is 6; levels 0 through 9
    are valid. Compression is synchronous and uses memory proportional to the
    input. *)

val apply :
  ?min_body_size:int ->
  ?level:int ->
  ?compressible:(string -> bool) ->
  Router.Request.t ->
  Router.response ->
  Router.response

val middleware :
  ?min_body_size:int ->
  ?level:int ->
  ?compressible:(string -> bool) ->
  unit ->
  Router.middleware
(** Opt-in buffered gzip compression. The default minimum size is 1024 bytes.
    The predicate receives a lowercase media type without parameters.

    Streams, SSE, partial responses and [Cache-Control: no-transform] are not
    compressed. Existing content encodings and bodyless statuses are left to the
    application. HEAD negotiates the same buffered representation as GET.
    Negotiated responses merge [Vary: Accept-Encoding], including identity
    fallbacks. If identity is excluded, gzip may be used below the size
    threshold or even when larger; otherwise larger compressed bodies are
    discarded. When no representation is acceptable, return an empty 406
    response.

    Compression sets the encoded length, weakens strong ETags, and removes
    digests of the original representation. Streaming compression is not
    supported. Invalid minimum sizes or levels raise [Invalid_argument]. *)

val file :
  ?headers:Router.Headers.t ->
  ?compressible:(string -> bool) ->
  content_type:string ->
  _ Eio.Path.t ->
  Router.handler
(** Serve an explicit asset path for GET/HEAD, negotiating its [.br] and [.gz]
    siblings without a Brotli encoder. The original file must exist. Missing,
    non-regular or older sidecars are ignored. Other I/O errors propagate.

    The original content type is preserved, lengths and strong ETags describe
    the selected bytes, and [If-None-Match] is evaluated after negotiation. The
    helper owns content encoding, length, ETag, range and digest headers, and
    removes supplied Last-Modified values; other supplied headers are preserved.
    [no-transform] and the content-type predicate disable sidecar selection.
    Streams and byte ranges are not supported: Range requests receive the full
    representation.

    Assets are buffered in memory. Choose the path in application code using an
    appropriately confined Eio directory; this helper does not map URLs to
    filesystem paths. *)
