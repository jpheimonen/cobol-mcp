# COBOL MCP Server

> An MCP server written in COBOL. Because the world needed this.

A fully functional [Model Context Protocol](https://modelcontextprotocol.io/) (MCP) server written entirely in COBOL, targeting [GnuCOBOL](https://gnucobol.sourceforge.io/). It communicates over stdio transport, speaks JSON-RPC 2.0, and exposes three tools that play to COBOL's historic strengths: arithmetic, currency formatting, and date validation.

COBOL has been running the world's financial systems since 1959. Now it can run your AI tools too.

## What is MCP?

The [Model Context Protocol](https://modelcontextprotocol.io/) (MCP) is an open standard that lets AI assistants like Claude connect to external tools and data sources. MCP servers expose capabilities (tools, resources, prompts) over a simple JSON-RPC 2.0 protocol, and MCP clients (like Claude Desktop) can discover and invoke them. Think of it as a USB port for AI -- plug in a server and the AI gains new abilities. This particular server gives your AI the power of COBOL.

## Tools

This server exposes three tools, each playing to a classic COBOL strength:

### `add`

Add two numbers together using COBOL's `COMPUTE` verb -- the original cloud computing, circa 1959.

| Argument | Type   | Required | Description   |
|----------|--------|----------|---------------|
| `a`      | number | yes      | First number  |
| `b`      | number | yes      | Second number |

**Returns:** The sum as text, with the quiet confidence of a language that has been doing arithmetic for over 60 years.

### `format_currency`

Format a number as US currency using COBOL's `PICTURE` clause. This is the crown jewel -- COBOL's edited numeric pictures with dollar signs, commas, decimal points, and zero suppression are genuinely superior to most modern formatting approaches. The `PIC $$$$,$$$,$$$.99` clause does it all in one declaration.

| Argument | Type   | Required | Description                 |
|----------|--------|----------|-----------------------------|
| `amount` | number | yes      | Amount to format as currency |

**Returns:** A dollar-formatted string (e.g., `$1,234.56`), formatted by the `PICTURE` clause -- you are welcome.

### `validate_date`

Validate a date in `YYYYMMDD` format, including leap year rules. COBOL has been validating dates since before most programmers were born.

| Argument | Type   | Required | Description               |
|----------|--------|----------|---------------------------|
| `date`   | string | yes      | Date in `YYYYMMDD` format |

**Returns:** Whether the date is valid, and if not, why. Checks month ranges, day ranges per month, and full leap year rules (divisible by 4, except centuries, except centuries divisible by 400).

## Prerequisites

You need [GnuCOBOL](https://gnucobol.sourceforge.io/) installed on your system. GnuCOBOL is a free COBOL compiler that translates COBOL to C and then compiles it with your system's C compiler.

### Installation

**Ubuntu / Debian:**
```bash
sudo apt install gnucobol
```

**Fedora / RHEL:**
```bash
sudo dnf install gnucobol
```

**macOS (Homebrew):**
```bash
brew install gnucobol
```

**Arch Linux:**
```bash
sudo pacman -S gnucobol
```

For other platforms, see the [GnuCOBOL website](https://gnucobol.sourceforge.io/).

You can verify the installation by running:
```bash
cobc --version
```

## How to Run

The server communicates over stdio -- it reads JSON-RPC messages from stdin and writes responses to stdout. The `run.sh` wrapper script handles compilation and execution:

```bash
./run.sh
```

This will:
1. Check that GnuCOBOL (`cobc`) is installed
2. Compile the COBOL source (`src/mcp-server.cob`) into a binary in the `build/` directory
3. Execute the binary, passing through stdin/stdout for MCP communication

The server runs as a long-lived process, processing one JSON-RPC message per line until stdin is closed (EOF).

## MCP Client Configuration

### Claude Desktop

Add this to your Claude Desktop configuration file:

- **macOS:** `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows:** `%APPDATA%\Claude\claude_desktop_config.json`

```json
{
  "mcpServers": {
    "cobol": {
      "command": "/absolute/path/to/cobol-mcp-lol/run.sh"
    }
  }
}
```

Replace `/absolute/path/to/cobol-mcp-lol` with the actual path to where you cloned this repository.

### Claude Code (CLI)

```bash
claude mcp add cobol /absolute/path/to/cobol-mcp-lol/run.sh
```

### Other MCP Clients

Any MCP client that supports stdio transport can use this server. The general pattern is:

- **Command:** `/absolute/path/to/cobol-mcp-lol/run.sh`
- **Transport:** stdio
- **Arguments:** none required

The wrapper script handles compilation automatically -- the client just needs to run it.

## How to Test

A protocol conformance test script is included that exercises the full MCP lifecycle:

```bash
./test/test-mcp-server.sh
```

This requires GnuCOBOL and `jq` to be installed. The script will:
1. Compile the COBOL source
2. Run tests covering initialization, tool listing, tool execution, and error handling
3. Report pass/fail for each test case
4. Exit with a non-zero status if any tests fail

## Project Structure

```
.
├── run.sh                      # Wrapper script (compile + run)
├── src/
│   └── mcp-server.cob          # The COBOL source (yes, all of it)
├── test/
│   └── test-mcp-server.sh      # Protocol conformance tests
├── build/                      # Compiled binary (created by run.sh)
└── README.md                   # You are here
```

## License

MIT. Use responsibly. Or irresponsibly. It's COBOL in 2025 -- the irresponsibility ship has sailed.
