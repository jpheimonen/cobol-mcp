#!/usr/bin/env bash
# ============================================================
# COBOL MCP Server -- Wrapper Script
# ============================================================
# Compiles the COBOL source with GnuCOBOL and runs it.
# This script is the entry point for MCP clients.
#
# The compiled binary communicates over stdio:
#   - Reads JSON-RPC messages from stdin
#   - Writes JSON-RPC responses to stdout
# ============================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
COBOL_SOURCE="${SCRIPT_DIR}/src/mcp-server.cob"
BUILD_DIR="${SCRIPT_DIR}/build"
BINARY_NAME="mcp-server"

# ------------------------------------------------------------
# Check for GnuCOBOL compiler
# ------------------------------------------------------------
if ! command -v cobc &> /dev/null; then
    echo "ERROR: GnuCOBOL compiler (cobc) not found." >&2
    echo "" >&2
    echo "This MCP server is written in COBOL and requires GnuCOBOL to compile." >&2
    echo "" >&2
    echo "Install GnuCOBOL:" >&2
    echo "  Ubuntu/Debian:  sudo apt install gnucobol" >&2
    echo "  Fedora/RHEL:    sudo dnf install gnucobol" >&2
    echo "  macOS (Brew):   brew install gnucobol" >&2
    echo "  Arch Linux:     sudo pacman -S gnucobol" >&2
    echo "" >&2
    echo "For other platforms, see: https://gnucobol.sourceforge.io/" >&2
    exit 1
fi

# ------------------------------------------------------------
# Check that the COBOL source file exists
# ------------------------------------------------------------
if [ ! -f "${COBOL_SOURCE}" ]; then
    echo "ERROR: COBOL source file not found at: ${COBOL_SOURCE}" >&2
    exit 1
fi

# ------------------------------------------------------------
# Create build directory (keeps the project root clean)
# ------------------------------------------------------------
mkdir -p "${BUILD_DIR}"

# ------------------------------------------------------------
# Compile the COBOL source
# ------------------------------------------------------------
if ! cobc -x -o "${BUILD_DIR}/${BINARY_NAME}" "${COBOL_SOURCE}"; then
    echo "ERROR: Compilation failed." >&2
    exit 1
fi

# ------------------------------------------------------------
# Run the compiled server, passing through stdin/stdout
# ------------------------------------------------------------
exec "${BUILD_DIR}/${BINARY_NAME}"
