# Trusted forwarding headers

Forwarding is disabled unless a router is wrapped with a policy. A forwarding
header is a claim, and the proxy at every trusted hop must sanitize or correctly
maintain the selected header family. Use `X_forwarded_only` when proxies only
maintain X-Forwarded-For: an attacker can otherwise inject a `Forwarded` header.
`Prefer_forwarded` requires the contract for **both** families: it chooses
`Forwarded` whenever that field exists, even if empty or invalid, and never
falls back to X-Forwarded after a failure.

```ocaml
let policy =
  match Halo.Forwarded.create
          ~trusted_proxies:["192.0.2.10"; "2001:db8:1234::/48"]
          ~header_mode:Halo.Forwarded.Forwarded_only () with
  | Ok policy -> policy
  | Error message -> failwith message

let router = Halo.Router.with_forwarded_headers policy router
```

The allowlist accepts literal IPv4 and IPv6 addresses or CIDR prefixes; it does
not resolve names. An empty list trusts nobody. Families are matched separately:
a dual-stack listener can report an IPv4 peer as an IPv4-mapped IPv6 address,
so configure the corresponding IPv6 literal or prefix explicitly. Synthetic
requests and Unix-domain peers cannot activate forwarding. The peer is always
the socket peer, never a header or rewrite.

Halo walks the list right to left, starting at that peer, and consumes another
address only while the current address is trusted. The first untrusted address
is the selected boundary; if all are trusted the leftmost address is selected.
Values to the left of that boundary are not used. A missing, `unknown`, or
obfuscated address encountered in the walk makes the chain incomplete; an
invalid traversed address also stops resolution. These cases apply no overrides.
The boundary identifies the first address outside the trusted proxy chain, not
necessarily the original end user.

For example, with `10.0.0.0/8` trusted:

* A direct connection from `198.51.100.4` ignores all forwarding headers and
  uses that socket address, the original Host, and the connection scheme.
* A connection from `10.0.0.1` with `Forwarded: for=198.51.100.4;proto=https`
  selects `198.51.100.4` and `https`.
* A connection from `10.0.0.1` with `Forwarded: for=203.0.113.9,
  for=198.51.100.4;host=public.example;proto=https, for=10.0.0.2` selects
  `198.51.100.4` and the host and scheme from that same entry; the earlier
  address is outside the trust boundary.

`Forwarded` uses RFC 7239 syntax and HTTP list parsing, including repeated
fields, quoted strings and bounded empty elements. Node ports are discarded;
numeric ports of one to five digits and obfuscated ports are accepted. Halo
accepts only IP node addresses as effective client addresses. X-Forwarded-For is a deployment convention,
not RFC 7239: it accepts IPv4, bare IPv6, and bracketed IPv6, plus numeric ports
where unambiguous. Bare IPv6 is always an address, never guessed to have a port.
Only X-Forwarded-For, X-Forwarded-Host, and X-Forwarded-Proto affect resolution.

The default legacy mode `Aligned_lists` requires each supplied host or proto
list to have the same number of entries as X-Forwarded-For and uses the
boundary index. `Nearest_proxy_singletons` instead requires one value in each
supplied metadata header and attributes it to the immediate trusted proxy.
Multi-value metadata is unusable in this mode. Configure this mode only when
that proxy supplies the corresponding singleton fields correctly.

After address resolution, host and scheme are resolved independently. A missing
value uses the direct default; malformed, unsupported, or misaligned metadata
falls back for that field and produces `Partial`. Hosts use Halo's parsed Host
representation (including bracketed IPv6 and optional numeric port); IPvFuture
and other unsupported authority forms cannot become effective hosts. Schemes
are normalized to `http` or `https`; other valid schemes are unsupported.
Selected-family headers are limited to 16 KiB of names and values and 64 list
positions per list, including empty positions. Limit failures apply no overrides.

`Request.effective_client_address`, `Request.effective_host`, and
`Request.effective_scheme` expose the resulting values. The default client is
the socket IP (or `None` for synthetic and Unix-domain requests); the default
host is the original parsed Host, and the default scheme is derived from TLS
(`http` for synthetic requests). `Matcher.effective_host` matches the effective
host; `Matcher.host` continues to match the ordinary request Host.
`Request.forwarded_info` reports `Disabled`, `Untrusted_peer`, `Absent`,
`Applied`, `Partial`, or `Ignored`, with a `Direct`, `Forwarded`, or
`X_forwarded` source per field and a list of issues. `Applied` has no issues;
optional missing metadata does not cause `Partial`. An unusable address chain
is `Ignored`. Inspect these values when a deployment needs to distinguish a
direct default from a forwarded claim.

Policies execute before pre-middleware and matchers. They use the original
header snapshot and peer even after a rewrite; nested policies recompute from
that snapshot and failed branches do not alter siblings. Ordinary header and
connection accessors, TLS metadata, and telemetry retain their original
meaning. TRACE responses omit `Forwarded` and `X-Forwarded-*` fields without
changing stored request headers.
