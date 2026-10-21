# Changelog

## 0.1.0.0

- Initial Hub client: paginated model, dataset, and Space discovery;
  revision metadata, identity, buffered and streaming file downloads.
- Hosted chat completions, feature extraction, text classification,
  text generation, and generic JSON inference requests.
- Parsed identifiers, opaque tokens, explicit errors, bounded redirects,
  and origin-restricted pagination and authentication.
- Offline integration tests and source-distribution CI across GHC 9.6–9.14.
- Opaque domain types for repository metadata, endpoints, and chat identifiers;
  parsers refine input once and JSON decoding preserves the same guarantees.
- Structural repository identities and routed models, with total constructors
  for composing parsed components and reusing response identifiers in requests.
- Nonempty conversations and embedding batches, and smart constructors for
  positive limits/timeouts and bounded sampling settings. Client construction
  consumes parsed configuration and no longer returns an input-error branch.
- Parsec grammars over Text for domain values and endpoint origins, and over
  ByteString for Link headers, with complete input consumption and redacted
  errors. Quoted Link parameters preserve escaped delimiters.
- Focused public modules for authentication, repositories, Hub metadata,
  routing, chat, tasks, numeric parameters, and pagination. Errors have their
  own module; shared parsing/JSON and HTTP utilities live in `HuggingFace.Utils`.
  Existing convenience imports remain supported, with no changes to wire behavior.
- Domain modules own their implementations directly. Only the transport retains
  an `Internal` module, and all library modules are exposed by Cabal. Read-only
  accessors and composable parsers preserve opaque construction boundaries.
- `optparse-applicative` CLI parsing with command-specific help, typed arguments,
  and usage errors handled before authentication or client construction.
- Opaque `HuggingFaceM` over `ExceptT HuggingFaceError IO`, with `mtl` instances,
  an IO runner, and lifting/error utilities. Hub, inference, custom transport
  operations, and download callbacks compose in the concrete monad; pure
  parsers keep returning `Either`. Scoped responses close on typed failures.
- Use `DerivingVia` for the concrete monad and representation-preserving domain
  equality, ordering, and JSON encoding. Keep parsing, redaction, custom wire
  formats, and constructor-aware diagnostics unchanged.
- Declare direct newtype accessors as record fields. Repository identities and
  inference targets use records for their separate components. Existing accessor
  names remain available, with normal record updates; derived `Show` includes
  field names. Computed renderers and redacted diagnostics retain their behavior.
