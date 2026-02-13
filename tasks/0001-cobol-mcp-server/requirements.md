# Requirements

## Problem Statement

The world has thousands of MCP server implementations in Python, TypeScript, Go, and Rust. What it lacks — critically — is an MCP server written in COBOL. This project corrects that oversight by implementing a functional MCP server library in COBOL that communicates over stdio transport, speaks JSON-RPC 2.0, and exposes tools that play to COBOL's historic strengths (currency formatting, date validation, arithmetic).

This is a joke/proof-of-concept project. There is no COBOL compiler environment available, so the code will be written to target GnuCOBOL but will not be compiled or executed during development. Testing will be limited to structural validation and best-effort correctness.

## Success Criteria

- [ ] A COBOL source file (or files with copybooks) that implements an MCP server using stdio transport
- [ ] The server handles the full MCP lifecycle: receives an `initialize` request, responds with server capabilities, receives the `initialized` notification, and enters operational mode
- [ ] The server responds to `ping` requests
- [ ] The server responds to `tools/list` with at least three tool definitions including name, description, and input schema
- [ ] The server responds to `tools/call` for each declared tool, performing the described operation and returning a result
- [ ] The server exposes a tool that formats a number as currency (leveraging COBOL's native PICTURE clause formatting — the one thing COBOL does better than every modern language)
- [ ] The server exposes a tool that adds two numbers
- [ ] The server exposes a tool that validates a date
- [ ] The server returns proper JSON-RPC 2.0 error responses for unknown methods and malformed requests
- [ ] The server handles unknown tool names in `tools/call` with an appropriate error result
- [ ] JSON-RPC messages are read from stdin and written to stdout, one per line, with no embedded newlines
- [ ] The server targets GnuCOBOL (free-format source) so it could theoretically be compiled and run
- [ ] A wrapper shell script is provided that would compile and run the server (for someone who has GnuCOBOL installed)
- [ ] A README exists that explains what this is, why it exists, how to use it, and that it is, in fact, written in COBOL
- [ ] The README includes configuration examples showing how to add this server to Claude Desktop and other MCP clients

## Constraints

- **No COBOL compiler available**: Code will be written but not compiled or executed. Correctness is best-effort based on COBOL language knowledge.
- **Target GnuCOBOL**: Use free-format source (not fixed-format column-80 style) for readability, but the code should be valid GnuCOBOL.
- **Stdio transport only**: No HTTP, no SSE, no Streamable HTTP. The server reads from stdin and writes to stdout.
- **Protocol version**: Target MCP protocol version `2025-11-25`.
- **JSON handling must be hand-rolled**: There are no JSON libraries for COBOL. Parsing and generation must be implemented using native COBOL string operations (`INSPECT`, `STRING`, `UNSTRING`, positional scanning).
- **Single-program architecture preferred**: Keep it in one main COBOL source file with copybooks for organization if needed, rather than a multi-program build. The comedy is better when it's one magnificent monolith.

## Non-Goals

- **Not a general-purpose COBOL MCP SDK**: This is a single self-contained server, not a reusable library/framework for building arbitrary MCP servers in COBOL.
- **Not production-grade JSON parsing**: The JSON parser only needs to handle the specific shapes of MCP JSON-RPC messages, not arbitrary JSON. No need to handle nested objects in params beyond what the declared tools require.
- **Not implementing resources or prompts**: Only the `tools` capability will be supported. No resources, no prompts, no sampling, no completions.
- **No structured content / output schemas**: Tool results will return unstructured text content only.
- **No pagination**: The tools list is small enough to return in a single response.
- **No list change notifications**: The tool list is static.
- **Not worrying about compilation or runtime testing**: If someone with GnuCOBOL wants to try it, great. But we are not setting up a CI pipeline for COBOL.
