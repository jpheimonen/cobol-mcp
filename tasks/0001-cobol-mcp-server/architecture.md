# Architecture

## Components Affected

This is a greenfield project with no existing code. All components are new.

### Main COBOL Program (`src/mcp-server.cob`)

The single monolithic COBOL source file containing the entire MCP server. Written in GnuCOBOL free-format. This file contains all logic: the main event loop, JSON parsing, JSON generation, request routing, and tool implementations.

The program runs as a long-lived process, continuously reading lines from stdin, processing them as JSON-RPC messages, and writing responses to stdout.

### Copybooks (`src/copybooks/`)

Optional COBOL copybooks (the COBOL equivalent of header/include files) to organize data definitions. These get included into the main program at compile time. Useful for separating the massive WORKING-STORAGE SECTION into logical chunks if the single file becomes unwieldy.

Potential copybooks:
- **JSON field definitions**: Working storage variables used during JSON parsing and generation (field buffers, position trackers, temporary strings)
- **MCP protocol constants**: Server name, version, protocol version string, tool names and descriptions
- **Tool data definitions**: Working storage for tool-specific input/output variables (numeric fields for addition, date fields for validation, currency PICTURE fields for formatting)

### Wrapper Script (`run.sh`)

A shell script that compiles the COBOL source with GnuCOBOL (`cobc`) and then executes the resulting binary. This is what an MCP client would point at as its server command. It should check for `cobc` availability and give a helpful error if not found.

### README (`README.md`)

Project documentation including what it is, why, how to build/run, MCP client configuration snippets, and the list of exposed tools.

## New Entities/Endpoints

### MCP Methods Handled

The server must recognize and dispatch on the following JSON-RPC method strings:

- **`initialize`** — Responds with server info (name: "cobol-mcp-server", version), the protocol version, and a capabilities object declaring tools support. No resources, no prompts, no logging.
- **`notifications/initialized`** — Received as a notification (no `id` field). The server acknowledges internally that initialization is complete. No response is sent.
- **`ping`** — Responds with an empty result object.
- **`tools/list`** — Responds with the full list of tool definitions. Each tool has a name, description, and input schema describing its expected arguments.
- **`tools/call`** — Dispatches to the named tool, executes it, and returns a result with text content. If the tool name is unknown, returns a result with `isError` set to true.
- **Any other method** — Returns a JSON-RPC error response with "Method not found" and error code -32601.

### Tools Exposed

Three tools, each playing to a classic COBOL strength:

1. **`add`** — Accepts two numeric arguments, adds them, and returns the sum as text. COBOL's COMPUTE verb handles this. The description should wink at the fact that COBOL has been doing this since 1959.

2. **`format_currency`** — Accepts a numeric value and formats it as a US-dollar currency string using COBOL's PICTURE clause formatting. This is the crown jewel — COBOL's edited numeric pictures (with dollar signs, commas, decimal points, and zero suppression) are genuinely superior to most modern formatting approaches. Returns the formatted string as text.

3. **`validate_date`** — Accepts a date string (YYYYMMDD format) and validates whether it represents a real calendar date (correct month range, day range for the given month, leap year handling for February). Returns whether the date is valid and, if not, why. COBOL has been validating dates since before most programmers were born.

## Integration Points

### Stdio Transport (stdin/stdout)

The server's only external interface. The main loop continuously reads one line at a time from stdin. Each line is expected to be a complete JSON-RPC message. After processing, the server writes exactly one line to stdout containing the JSON-RPC response (for requests) or writes nothing (for notifications). Stderr may be used for diagnostic logging if desired but is not required.

### MCP Client Integration

Any MCP-compatible client (Claude Desktop, Cursor, etc.) launches this server as a subprocess via the wrapper script. The client writes JSON-RPC messages to the server's stdin and reads responses from the server's stdout. The wrapper script is the entry point the client invokes.

### Message Flow

1. Client sends `initialize` request → Server responds with capabilities
2. Client sends `initialized` notification → Server notes it, sends nothing back
3. Client sends `tools/list` → Server responds with all three tool definitions
4. Client sends `tools/call` with tool name and arguments → Server executes the tool and responds with the result
5. Client sends `ping` at any time → Server responds with empty result
6. Client closes stdin → Server detects end-of-file and terminates

### JSON Parsing Strategy

The server needs a purpose-built JSON parser that only handles the specific message shapes it expects. It does not need to parse arbitrary JSON. The parser extracts specific known fields from the incoming message by scanning for key strings and extracting their values. For nested params, it only needs to go one or two levels deep (e.g., extracting `params.name` for tool calls, or `params.arguments.a` for the add tool).

### JSON Generation Strategy

Response generation uses COBOL's STRING verb to concatenate pre-defined template fragments with dynamic values. Since the server controls the shape of every response, the templates are fixed and only the dynamic portions (request IDs, tool results, error messages) are interpolated. Special characters in string values that could break JSON (quotes, backslashes) should be escaped during generation.

## Failure Modes

### Malformed JSON Input
If the incoming line cannot be parsed (missing `jsonrpc` field, missing `method`, etc.), the server should return a JSON-RPC error response with code -32700 (Parse error). If the request `id` could be extracted, include it in the error response; otherwise omit it.

### Unknown Method
If the `method` field doesn't match any handled method, return a JSON-RPC error response with code -32601 (Method not found) and include the request `id`.

### Unknown Tool Name
If `tools/call` is received with an unrecognized tool name, return a successful JSON-RPC response (not a protocol error) but with `isError: true` in the result and a text content item explaining that the tool was not found. This follows the MCP spec's distinction between protocol errors and tool execution errors.

### Invalid Tool Arguments
If a tool receives arguments it cannot process (non-numeric input for `add`, non-date-shaped input for `validate_date`), return a tool result with `isError: true` and a descriptive message. The tool should not crash the server.

### Stdin Closed / End of File
When the server detects end-of-file on stdin, it should terminate cleanly with a zero exit code. This is the normal shutdown path.

### Line Too Long
COBOL has fixed-size working storage. If an incoming message exceeds the buffer size, the server should return a parse error. The buffer should be generously sized (at least several thousand characters) to handle realistic MCP messages.

### GnuCOBOL Not Installed
The wrapper script should detect the absence of the `cobc` compiler and print a helpful error message explaining how to install GnuCOBOL, rather than failing with a cryptic error.
