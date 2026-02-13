#!/usr/bin/env bash
# ============================================================
# COBOL MCP Server -- Protocol Conformance Test Script
# ============================================================
# Exercises the compiled MCP server by piping JSON-RPC messages
# to stdin and validating the responses on stdout.
#
# Requires: GnuCOBOL (cobc), jq
#
# This script compiles the COBOL source, then runs a battery
# of tests covering the full MCP lifecycle, tool execution,
# and error handling.
# ============================================================

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
COBOL_SOURCE="${PROJECT_DIR}/src/mcp-server.cob"
BUILD_DIR="${PROJECT_DIR}/build"
BINARY="${BUILD_DIR}/mcp-server"

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Colors (if terminal supports them)
if [ -t 1 ]; then
    GREEN='\033[0;32m'
    RED='\033[0;31m'
    YELLOW='\033[0;33m'
    BOLD='\033[1m'
    NC='\033[0m'
else
    GREEN=''
    RED=''
    YELLOW=''
    BOLD=''
    NC=''
fi

# ============================================================
# Helper functions
# ============================================================

pass() {
    TESTS_RUN=$((TESTS_RUN + 1))
    TESTS_PASSED=$((TESTS_PASSED + 1))
    echo -e "  ${GREEN}PASS${NC} $1"
}

fail() {
    TESTS_RUN=$((TESTS_RUN + 1))
    TESTS_FAILED=$((TESTS_FAILED + 1))
    echo -e "  ${RED}FAIL${NC} $1"
    if [ -n "${2:-}" ]; then
        echo -e "       ${RED}Expected:${NC} $2"
    fi
    if [ -n "${3:-}" ]; then
        echo -e "       ${RED}Got:${NC} $3"
    fi
}

# Send a JSON-RPC message to the server and capture the response.
# Usage: response=$(send_message '{"jsonrpc":"2.0",...}')
# For notifications (no response expected), use send_notification.
send_message() {
    local input="$1"
    echo "${input}" | "${BINARY}" 2>/dev/null | head -1
}

# Send multiple messages, get all responses.
# Each message is a separate argument.
# Returns all output lines.
send_messages() {
    printf '%s\n' "$@" | "${BINARY}" 2>/dev/null
}

# Send a notification (expects no response for the notification itself).
# Sends the notification followed by a ping, and checks that we only
# get one response (the ping response), not two.
check_notification_silent() {
    local notification="$1"
    local ping='{"jsonrpc":"2.0","id":9999,"method":"ping"}'
    local output
    output=$(printf '%s\n%s\n' "${notification}" "${ping}" | "${BINARY}" 2>/dev/null)
    local line_count
    line_count=$(echo "${output}" | wc -l)
    # Should get exactly 1 line (the ping response)
    if [ "${line_count}" -eq 1 ]; then
        # Verify it's the ping response
        if echo "${output}" | jq -e '.id == 9999' > /dev/null 2>&1; then
            return 0
        fi
    fi
    return 1
}

# ============================================================
# Dependency checks
# ============================================================

echo -e "${BOLD}COBOL MCP Server -- Protocol Conformance Tests${NC}"
echo "================================================"
echo ""

# Check for GnuCOBOL
if ! command -v cobc &> /dev/null; then
    echo -e "${YELLOW}SKIP:${NC} GnuCOBOL compiler (cobc) not found."
    echo "      Install GnuCOBOL to run these tests."
    echo "      Ubuntu/Debian: sudo apt install gnucobol"
    echo "      macOS (Brew):  brew install gnucobol"
    exit 0
fi

# Check for jq
if ! command -v jq &> /dev/null; then
    echo -e "${RED}ERROR:${NC} jq is required for test validation."
    echo "      Ubuntu/Debian: sudo apt install jq"
    echo "      macOS (Brew):  brew install jq"
    exit 1
fi

# ============================================================
# Compile the COBOL source
# ============================================================

echo -e "${BOLD}Compiling COBOL source...${NC}"
mkdir -p "${BUILD_DIR}"

if ! cobc -x -o "${BINARY}" "${COBOL_SOURCE}" 2>&1; then
    echo -e "${RED}ERROR:${NC} Compilation failed. Cannot run tests."
    exit 1
fi
echo -e "${GREEN}Compilation successful.${NC}"
echo ""

# ============================================================
# LIFECYCLE TESTS
# ============================================================

echo -e "${BOLD}Lifecycle Tests${NC}"
echo "----------------"

# Test: initialize request
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}')

if echo "${RESPONSE}" | jq -e '.result.protocolVersion' > /dev/null 2>&1; then
    pass "initialize: response contains protocolVersion"
else
    fail "initialize: response contains protocolVersion" "protocolVersion present" "${RESPONSE}"
fi

if echo "${RESPONSE}" | jq -e '.result.serverInfo' > /dev/null 2>&1; then
    pass "initialize: response contains serverInfo"
else
    fail "initialize: response contains serverInfo" "serverInfo present" "${RESPONSE}"
fi

if echo "${RESPONSE}" | jq -e '.result.capabilities.tools' > /dev/null 2>&1; then
    pass "initialize: response contains capabilities.tools"
else
    fail "initialize: response contains capabilities.tools" "capabilities.tools present" "${RESPONSE}"
fi

if echo "${RESPONSE}" | jq -e '.jsonrpc == "2.0"' > /dev/null 2>&1; then
    pass "initialize: response has jsonrpc 2.0"
else
    fail "initialize: response has jsonrpc 2.0" "jsonrpc: 2.0" "${RESPONSE}"
fi

if echo "${RESPONSE}" | jq -e '.id == 1' > /dev/null 2>&1; then
    pass "initialize: response id matches request id"
else
    fail "initialize: response id matches request id" "id: 1" "${RESPONSE}"
fi

# Test: notifications/initialized produces no output
if check_notification_silent '{"jsonrpc":"2.0","method":"notifications/initialized"}'; then
    pass "notifications/initialized: no output produced"
else
    fail "notifications/initialized: no output produced" "only ping response" "got extra output"
fi

# Test: ping request
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":2,"method":"ping"}')

if echo "${RESPONSE}" | jq -e '.result == {}' > /dev/null 2>&1; then
    pass "ping: response has empty result object"
else
    fail "ping: response has empty result object" "result: {}" "${RESPONSE}"
fi

if echo "${RESPONSE}" | jq -e '.id == 2' > /dev/null 2>&1; then
    pass "ping: response id matches request id"
else
    fail "ping: response id matches request id" "id: 2" "${RESPONSE}"
fi

echo ""

# ============================================================
# TOOL LISTING TESTS
# ============================================================

echo -e "${BOLD}Tool Listing Tests${NC}"
echo "-------------------"

RESPONSE=$(send_message '{"jsonrpc":"2.0","id":3,"method":"tools/list"}')

if echo "${RESPONSE}" | jq -e '.result.tools | length == 3' > /dev/null 2>&1; then
    pass "tools/list: returns exactly 3 tools"
else
    TOOL_COUNT=$(echo "${RESPONSE}" | jq -r '.result.tools | length' 2>/dev/null || echo "N/A")
    fail "tools/list: returns exactly 3 tools" "3 tools" "got ${TOOL_COUNT}"
fi

# Check tool names (order-independent)
TOOL_NAMES=$(echo "${RESPONSE}" | jq -r '.result.tools[].name' 2>/dev/null | sort)
EXPECTED_NAMES=$(printf "add\nformat_currency\nvalidate_date")

if [ "${TOOL_NAMES}" = "${EXPECTED_NAMES}" ]; then
    pass "tools/list: contains add, format_currency, validate_date"
else
    fail "tools/list: contains add, format_currency, validate_date" "${EXPECTED_NAMES}" "${TOOL_NAMES}"
fi

if echo "${RESPONSE}" | jq -e '.id == 3' > /dev/null 2>&1; then
    pass "tools/list: response id matches request id"
else
    fail "tools/list: response id matches request id" "id: 3" "${RESPONSE}"
fi

if echo "${RESPONSE}" | jq -e '.jsonrpc == "2.0"' > /dev/null 2>&1; then
    pass "tools/list: response has jsonrpc 2.0"
else
    fail "tools/list: response has jsonrpc 2.0" "jsonrpc: 2.0" "${RESPONSE}"
fi

echo ""

# ============================================================
# TOOL EXECUTION TESTS -- add
# ============================================================

echo -e "${BOLD}Tool Execution Tests -- add${NC}"
echo "----------------------------"

# Test: add two integers
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":10,"method":"tools/call","params":{"name":"add","arguments":{"a":3,"b":4}}}')

if echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null | grep -q "7"; then
    pass "add: 3 + 4 contains 7 in result"
else
    RESULT_TEXT=$(echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null || echo "N/A")
    fail "add: 3 + 4 contains 7 in result" "text containing 7" "${RESULT_TEXT}"
fi

# Test: add decimal values
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":11,"method":"tools/call","params":{"name":"add","arguments":{"a":1.5,"b":2.3}}}')

if echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null | grep -q "3.8"; then
    pass "add: 1.5 + 2.3 contains 3.8 in result"
else
    RESULT_TEXT=$(echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null || echo "N/A")
    fail "add: 1.5 + 2.3 contains 3.8 in result" "text containing 3.8" "${RESULT_TEXT}"
fi

# Test: add with non-numeric input
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":12,"method":"tools/call","params":{"name":"add","arguments":{"a":"hello","b":4}}}')

if echo "${RESPONSE}" | jq -e '.result.isError == true' > /dev/null 2>&1; then
    pass "add: non-numeric input returns isError true"
else
    fail "add: non-numeric input returns isError true" "isError: true" "${RESPONSE}"
fi

if echo "${RESPONSE}" | jq -e '.id == 12' > /dev/null 2>&1; then
    pass "add: response id matches request id"
else
    fail "add: response id matches request id" "id: 12" "${RESPONSE}"
fi

echo ""

# ============================================================
# TOOL EXECUTION TESTS -- format_currency
# ============================================================

echo -e "${BOLD}Tool Execution Tests -- format_currency${NC}"
echo "-----------------------------------------"

# Test: format a basic number
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":20,"method":"tools/call","params":{"name":"format_currency","arguments":{"amount":1234.56}}}')

RESULT_TEXT=$(echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null || echo "N/A")

if echo "${RESULT_TEXT}" | grep -q '\$'; then
    pass "format_currency: result contains dollar sign"
else
    fail "format_currency: result contains dollar sign" "text with \$" "${RESULT_TEXT}"
fi

if echo "${RESULT_TEXT}" | grep -qE '[0-9]+\.[0-9]{2}'; then
    pass "format_currency: result contains decimal with two places"
else
    fail "format_currency: result contains decimal with two places" "text with .XX" "${RESULT_TEXT}"
fi

# Test: format zero
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":21,"method":"tools/call","params":{"name":"format_currency","arguments":{"amount":0}}}')

RESULT_TEXT=$(echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null || echo "N/A")

if echo "${RESULT_TEXT}" | grep -q '\$'; then
    pass "format_currency: zero returns dollar-formatted result"
else
    fail "format_currency: zero returns dollar-formatted result" "text with \$" "${RESULT_TEXT}"
fi

# Test: format large number with commas
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":22,"method":"tools/call","params":{"name":"format_currency","arguments":{"amount":1234567.89}}}')

RESULT_TEXT=$(echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null || echo "N/A")

if echo "${RESULT_TEXT}" | grep -q ','; then
    pass "format_currency: large number includes comma separators"
else
    fail "format_currency: large number includes comma separators" "text with commas" "${RESULT_TEXT}"
fi

echo ""

# ============================================================
# TOOL EXECUTION TESTS -- validate_date
# ============================================================

echo -e "${BOLD}Tool Execution Tests -- validate_date${NC}"
echo "---------------------------------------"

# Test: valid date
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":30,"method":"tools/call","params":{"name":"validate_date","arguments":{"date":"20250115"}}}')

RESULT_TEXT=$(echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null || echo "N/A")

if echo "${RESULT_TEXT}" | grep -iq "valid"; then
    pass "validate_date: 20250115 reports valid"
else
    fail "validate_date: 20250115 reports valid" "text mentioning valid" "${RESULT_TEXT}"
fi

# Test: invalid month
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":31,"method":"tools/call","params":{"name":"validate_date","arguments":{"date":"20251301"}}}')

if echo "${RESPONSE}" | jq -e '.result.isError == true' > /dev/null 2>&1; then
    pass "validate_date: invalid month 13 returns isError true"
else
    RESULT_TEXT=$(echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null || echo "N/A")
    # Also accept if the text mentions invalid
    if echo "${RESULT_TEXT}" | grep -iq "invalid\|error"; then
        pass "validate_date: invalid month 13 returns error indication"
    else
        fail "validate_date: invalid month 13 returns isError true" "isError: true or invalid text" "${RESPONSE}"
    fi
fi

# Test: invalid day (Feb 30)
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":32,"method":"tools/call","params":{"name":"validate_date","arguments":{"date":"20250230"}}}')

if echo "${RESPONSE}" | jq -e '.result.isError == true' > /dev/null 2>&1; then
    pass "validate_date: Feb 30 returns isError true"
else
    fail "validate_date: Feb 30 returns isError true" "isError: true" "${RESPONSE}"
fi

# Test: Feb 29 on leap year (2024 is a leap year)
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":33,"method":"tools/call","params":{"name":"validate_date","arguments":{"date":"20240229"}}}')

RESULT_TEXT=$(echo "${RESPONSE}" | jq -r '.result.content[0].text' 2>/dev/null || echo "N/A")

HAS_ERROR=$(echo "${RESPONSE}" | jq -r '.result.isError // false' 2>/dev/null)

if [ "${HAS_ERROR}" != "true" ] && echo "${RESULT_TEXT}" | grep -iq "valid"; then
    pass "validate_date: Feb 29 on leap year 2024 reports valid"
else
    fail "validate_date: Feb 29 on leap year 2024 reports valid" "valid date" "${RESULT_TEXT}"
fi

# Test: Feb 29 on non-leap year (2025 is not a leap year)
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":34,"method":"tools/call","params":{"name":"validate_date","arguments":{"date":"20250229"}}}')

if echo "${RESPONSE}" | jq -e '.result.isError == true' > /dev/null 2>&1; then
    pass "validate_date: Feb 29 on non-leap year 2025 returns isError true"
else
    fail "validate_date: Feb 29 on non-leap year 2025 returns isError true" "isError: true" "${RESPONSE}"
fi

echo ""

# ============================================================
# ERROR HANDLING TESTS
# ============================================================

echo -e "${BOLD}Error Handling Tests${NC}"
echo "---------------------"

# Test: unknown method returns -32601
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":40,"method":"bogus/method"}')

if echo "${RESPONSE}" | jq -e '.error.code == -32601' > /dev/null 2>&1; then
    pass "unknown method: returns error code -32601"
else
    ERROR_CODE=$(echo "${RESPONSE}" | jq -r '.error.code' 2>/dev/null || echo "N/A")
    fail "unknown method: returns error code -32601" "-32601" "${ERROR_CODE}"
fi

if echo "${RESPONSE}" | jq -e '.id == 40' > /dev/null 2>&1; then
    pass "unknown method: response id matches request id"
else
    fail "unknown method: response id matches request id" "id: 40" "${RESPONSE}"
fi

if echo "${RESPONSE}" | jq -e '.jsonrpc == "2.0"' > /dev/null 2>&1; then
    pass "unknown method: response has jsonrpc 2.0"
else
    fail "unknown method: response has jsonrpc 2.0" "jsonrpc: 2.0" "${RESPONSE}"
fi

# Test: unknown tool name returns isError true (not a protocol error)
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":41,"method":"tools/call","params":{"name":"nonexistent_tool","arguments":{}}}')

if echo "${RESPONSE}" | jq -e '.result.isError == true' > /dev/null 2>&1; then
    pass "unknown tool: returns isError true in result"
else
    fail "unknown tool: returns isError true in result" "isError: true" "${RESPONSE}"
fi

# This should NOT be a protocol error (should have .result, not .error)
if echo "${RESPONSE}" | jq -e '.result' > /dev/null 2>&1; then
    pass "unknown tool: returns result (not protocol error)"
else
    fail "unknown tool: returns result (not protocol error)" ".result present" "${RESPONSE}"
fi

# Test: all responses include jsonrpc 2.0 (spot check with ping)
RESPONSE=$(send_message '{"jsonrpc":"2.0","id":42,"method":"ping"}')

if echo "${RESPONSE}" | jq -e '.jsonrpc == "2.0"' > /dev/null 2>&1; then
    pass "all responses: include jsonrpc 2.0"
else
    fail "all responses: include jsonrpc 2.0" "jsonrpc: 2.0" "${RESPONSE}"
fi

echo ""

# ============================================================
# Summary
# ============================================================

echo "================================================"
echo -e "${BOLD}Results: ${TESTS_RUN} tests, ${GREEN}${TESTS_PASSED} passed${NC}, ${RED}${TESTS_FAILED} failed${NC}"
echo "================================================"

if [ "${TESTS_FAILED}" -gt 0 ]; then
    exit 1
fi

exit 0
