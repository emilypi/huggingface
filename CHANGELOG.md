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