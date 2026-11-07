type t
(** Trusted proxy forwarding policy. See the forwarding guide for the proxy
    trust contract, supported values and limits. *)

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

val create :
  trusted_proxies:string list ->
  header_mode:header_mode ->
  ?legacy_metadata_mode:legacy_metadata_mode ->
  unit ->
  (t, string) result

(** Internal integration with the router. *)
module Private : sig
  type resolution

  val disabled : info
  val untrusted : resolution
  val trusted : t -> Ipaddr.t -> bool
  val resolve : t -> peer:Ipaddr.t -> (string * string) list -> resolution
  val info : resolution -> info
  val address : resolution -> Ipaddr.t option
  val host : resolution -> (string * int option) option
  val scheme : resolution -> string option
end
