# COBOL MCP Server

Implement a fully functional Model Context Protocol (MCP) server written entirely in COBOL. The server communicates over stdio transport, speaks JSON-RPC 2.0, handles the full MCP lifecycle, and exposes three tools that play to COBOL's historic strengths: arithmetic, currency formatting, and date validation. Includes a wrapper script, protocol conformance test script, and documentation.

## Sub-tasks

| Step | Title | Done | Description |
|------|-------|------|-------------|
| [001](001.md) | COBOL program skeleton with JSON parsing and main loop | [x] | Create the main COBOL source file with all four divisions, working storage for buffers and state, the stdin read loop, hand-rolled JSON field extraction for incoming messages (method, id, params), and JSON response generation via STRING templates. This is the foundation everything else builds on. |
| [002](002.md) | MCP lifecycle and request routing | [ ] | Implement the initialize handshake (respond with server info, protocol version, tools capability), handle the initialized notification silently, respond to ping with an empty result, route to tools/list and tools/call handlers, and return JSON-RPC error responses for unknown methods and malformed input. |
| [003](003.md) | Tool implementations | [ ] | Implement all three tools — add (two-number arithmetic), format_currency (COBOL PICTURE clause dollar formatting), and validate_date (YYYYMMDD calendar validation with leap year support) — plus tools/list response with all tool definitions, and error handling for unknown tool names and invalid arguments. |
| [004](004.md) | Wrapper script, test script, and README | [ ] | Create run.sh (compile with cobc and execute, with GnuCOBOL availability check), test/test-mcp-server.sh (protocol conformance tests that pipe JSON-RPC to the server and validate responses), and README.md (project explanation, build/run instructions, MCP client configuration examples, tool documentation). |
| [005](005.md) | Structural validation and polish | [ ] | Run all structural validation tests (file existence, COBOL division presence, protocol string checks, tool name presence), fix any issues found, and verify the project is complete against all requirements and testing criteria. |
