# Testing Plan

## Important Context

There is no COBOL compiler available in this environment. The COBOL source code cannot be compiled or executed. Traditional unit testing of the COBOL logic itself is not possible.

Testing focuses on two areas:
1. **Structural validation** — Verify that all expected files exist and have the right shape
2. **Protocol conformance via the wrapper script** — A test script that validates the server's expected JSON-RPC behavior by piping input to stdin and checking stdout. This test script will only work for someone who has GnuCOBOL installed, but it should be created as part of the deliverable so the project is "test-ready" if compiled.

## Structural Validation Tests

These tests verify the deliverables exist and are well-formed without requiring a COBOL compiler.

- [ ] The main COBOL source file exists at the expected path under `src/`
- [ ] The COBOL source file contains all four required COBOL divisions: IDENTIFICATION, ENVIRONMENT, DATA, and PROCEDURE
- [ ] The COBOL source file contains GnuCOBOL free-format directives (not fixed-format column-based layout)
- [ ] The wrapper script `run.sh` exists and is executable
- [ ] The wrapper script references the COBOL source file and the `cobc` compiler command
- [ ] `README.md` exists and is non-empty
- [ ] The README contains MCP client configuration examples (mentions `claude_desktop_config.json` or equivalent)
- [ ] The README mentions all three tools by name: `add`, `format_currency`, `validate_date`

## COBOL Source Content Validation

These tests grep/scan the COBOL source to verify it contains the expected protocol handling logic, without compiling it.

- [ ] The source contains handling for the `initialize` method string
- [ ] The source contains handling for `notifications/initialized`
- [ ] The source contains handling for the `ping` method
- [ ] The source contains handling for `tools/list`
- [ ] The source contains handling for `tools/call`
- [ ] The source contains the server name and version in a form that would appear in the initialize response
- [ ] The source contains the protocol version string `2025-11-25`
- [ ] The source contains tool definitions for `add`, `format_currency`, and `validate_date`
- [ ] The source contains error code -32601 (Method not found) for unknown methods
- [ ] The source contains error code -32700 (Parse error) for malformed input
- [ ] The source reads from stdin (uses ACCEPT or equivalent)
- [ ] The source writes to stdout (uses DISPLAY or equivalent)

## Protocol Conformance Test Script

A shell-based test script (`test/test-mcp-server.sh`) should be created as a deliverable. This script pipes JSON-RPC messages to the compiled server's stdin and validates the stdout responses. It will only run if GnuCOBOL is installed and the server compiles successfully. The test script itself should be validated for existence and structure.

The script should test the following behaviors:

### Lifecycle

- [ ] Sending an `initialize` request produces a response containing `protocolVersion`, `serverInfo`, and `capabilities` with `tools` present
- [ ] Sending a `notifications/initialized` notification produces no output on stdout
- [ ] Sending a `ping` request produces a response with an empty result object and the matching request ID

### Tool Listing

- [ ] Sending a `tools/list` request returns a response containing a `tools` array with exactly three tools
- [ ] Each tool in the list has `name`, `description`, and `inputSchema` fields
- [ ] The tool names returned are `add`, `format_currency`, and `validate_date`

### Tool Execution — add

- [ ] Calling `add` with two integer arguments returns a result containing their sum as text
- [ ] Calling `add` with decimal/floating-point arguments returns a correct sum
- [ ] Calling `add` with non-numeric arguments returns a result with `isError` set to true

### Tool Execution — format_currency

- [ ] Calling `format_currency` with a plain number returns a dollar-formatted string (with dollar sign, commas, and decimal point)
- [ ] Calling `format_currency` with zero returns a sensible formatted result
- [ ] Calling `format_currency` with a large number includes comma separators in the appropriate positions

### Tool Execution — validate_date

- [ ] Calling `validate_date` with a valid date (e.g. 20250115) returns a result indicating the date is valid
- [ ] Calling `validate_date` with an invalid month (e.g. 20251301) returns a result with `isError` true or an invalidity message
- [ ] Calling `validate_date` with an invalid day for the month (e.g. 20250230) returns a result indicating invalidity
- [ ] Calling `validate_date` with Feb 29 on a leap year (e.g. 20240229) returns valid
- [ ] Calling `validate_date` with Feb 29 on a non-leap year (e.g. 20250229) returns invalid

### Error Handling

- [ ] Sending a request with an unknown method returns a JSON-RPC error response with code -32601
- [ ] Calling `tools/call` with an unknown tool name returns a result with `isError` true (not a protocol error)
- [ ] All responses include `"jsonrpc": "2.0"`
- [ ] All responses to requests include the same `id` as the request

## Manual Testing

**None** — structural validation is automated via the test script. Runtime protocol testing is automated via the conformance test script (requires GnuCOBOL to be installed by whoever runs it).
