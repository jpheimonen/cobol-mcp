# GPT-5.2 Spec Review

## Overall Assessment
The specs are detailed and readable, but several core behaviors are under-specified or inconsistent across files. The biggest risks are JSON-RPC correctness, tool result/schema shape, and ambiguous input types, which can lead to a server that passes local checks yet fails real MCP clients.

## Recent Changes Review
`claude_changes.md` does not exist in this directory, so there are no documented recent changes to evaluate.

## Critical Issues
- JSON-RPC error semantics are incorrect/unclear: parse errors (-32700) are used for missing `method`, while JSON-RPC would treat that as invalid request (-32600). The specs also claim "proper JSON-RPC" but never define the exact error object shape or when to return `id: null`.
- `id` handling is ambiguous: step 001 suggests storing `id` as a string, which risks quoting numeric IDs on output and violating JSON-RPC interoperability. No requirement clarifies how to preserve numeric vs string IDs.
- `validate_date` input type is inconsistent: step 003 says the argument is a string, but the testing plan examples use unquoted numbers (e.g., 20250115). This will produce different parsing paths and brittle tests.
- Tool result schema is inconsistent: requirements say "unstructured text," step 003 says `content` items with `type: "text"` and `text`, and tests barely validate result structure. The exact JSON response shape for `tools/call` must be specified.
- `tools/list` response schema is underspecified: you require `inputSchema` but do not define JSON Schema structure (type, required, additionalProperties), which makes tests subjective and implementations inconsistent.
- Lifecycle state is wrong in step 002: it says the initialization-complete flag is set after `initialize` response, but MCP initialization completes after `notifications/initialized`. This is a protocol correctness issue even if the server never sends its own requests.
- File path expectations are inconsistent: testing and step 001 assume `src/mcp-server.cob`, but requirements and overview never lock that down. This risks "correct" implementations failing structural validation.
- Testability gaps: the protocol conformance script does not include a runtime malformed JSON test, does not define dependencies or JSON parsing strategy, and the "no output for notification" check is inherently timing-sensitive without a prescribed timeout or follow-up request.

## Suggestions
- Define numeric ranges, rounding, and negative number behavior for `add` and `format_currency`, plus overflow handling (PIC overflows often yield asterisks).
- Specify JSON escaping requirements for output (quotes, backslashes, and newlines) and whether tool text must be newline-free or escaped.
- Make the test script parse JSON structurally (e.g., with `jq`) instead of brittle string matching, and declare dependencies explicitly.
- Clarify ordering expectations for the `tools` array or make tests order-independent.
- Explicitly document how to handle unexpected notifications or missing `id` fields beyond `notifications/initialized`.
- Move `.gitignore` requirements into `requirements.md` or the top-level overview to keep acceptance criteria aligned.

## Per-File Notes

### requirements.md
- "Full MCP lifecycle" is vague and conflicts with non-goals; define exactly which MCP lifecycle steps are required.
- Error handling claims "proper JSON-RPC" but does not specify invalid request behavior (-32600) or error response shape.
- "At least three tools" conflicts with testing/architecture expecting exactly three.
- Does not specify file paths (`src/mcp-server.cob`) that testing relies on.
- "No structured content / output schemas" conflicts with `tools/list` requiring `inputSchema` and step 003 requiring `content` items.

### testing.md
- Runtime tests do not cover malformed JSON / parse errors despite requirements.
- `validate_date` examples are numeric literals, conflicting with a string input schema.
- The plan does not specify how the test script parses JSON or what dependencies it uses, making verification subjective.
- "No output for notification" needs a deterministic timeout and a follow-up request to prove the server is still alive.
- Structural checks are string-presence-only and can pass even if logic is wrong.

### architecture.md
- Initialization completion is described ambiguously; MCP considers initialization complete after `notifications/initialized`, not after the `initialize` response.
- `capabilities.tools` shape is not defined; most MCP clients expect at least `{"tools":{"listChanged":false}}` or an explicit object.
- JSON parsing strategy is too vague about whitespace, key order, and escaped strings, which will cause fragile parsing.
- `id` type preservation is not addressed.

### overview.md
- "Fully functional MCP server" overstates capability given the intentionally limited JSON parsing; clarify the supported subset.
- Does not mention required file paths or `.gitignore` updates that later steps assume.

### 001.md
- `id` storage as a string risks invalid JSON-RPC responses; clarify how to preserve numeric IDs.
- JSON extraction requirements do not define whitespace handling, key order assumptions, or behavior when key strings appear inside string values.
- Response generation does not define the exact JSON shape for success/error responses (fields required, ordering optional, etc.).

### 002.md
- Initialization flag is set too early; should flip after `notifications/initialized` if you track it at all.
- Parse error criteria are defined as "missing method," which is invalid request per JSON-RPC; error code choice needs correction.
- Pre-initialization behavior is vague: are `tools/list`/`tools/call` allowed before initialization or should they return an error?

### 003.md
- Input type mismatch for `validate_date` vs testing plan.
- Invalid date handling is inconsistent: "isError true OR message" conflicts with acceptance criteria that expect `isError` true.
- Currency formatting is underspecified (scale, rounding, negative numbers, overflow behavior).
- `inputSchema` structure is not defined, which weakens testability.
- The exact `tools/call` result object shape is implied but never explicitly defined.

### 004.md
- `run.sh` build output location is unspecified; the test script could depend on it.
- Test script dependencies and JSON parsing method are not defined; results will be brittle.
- "Skip gracefully if not installed" needs a clear exit code and message expectation.

### 005.md
- Requires `.gitignore` updates without that being a stated requirement earlier; align with `requirements.md`.
- Structural validation is mostly string presence; consider minimal semantic checks or stricter patterns.
- "Final polish pass" is subjective with no acceptance criteria, making completion ambiguous.
