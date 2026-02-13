# COBOL Gotchas for This Project

## ELSE IF is not a thing in COBOL
COBOL does not have `else if` as a single construct like C/Python. Writing `ELSE IF` creates a nested IF inside the ELSE branch, requiring TWO `END-IF` closers. Use `EVALUATE TRUE` with `WHEN` clauses instead for multi-branch logic.

## Signed numeric PIC and STRING output
`PIC S9(6)` with default USAGE DISPLAY uses overpunch sign encoding, which produces garbled characters (not a leading minus sign) when used in STRING operations. To get a human-readable signed number like `-32601`:
- Use an edited PIC like `PIC -(5)9` to format the number
- MOVE the signed numeric to the edited field first
- Then TRIM and use the edited field in STRING

## Backslash in string literals
GnuCOBOL may interpret backslash as escape in string literals depending on compiler settings. Use `X"5C"` (hex literal) for a literal backslash character to be safe.

## Quote character in JSON
Use `X"22"` for double-quote characters in JSON output. Store it in a working storage variable (e.g., `01 WS-QUOTE PIC X VALUE X"22".`).

## File structure
- Main COBOL source: `src/mcp-server.cob`
- GnuCOBOL free-format with `>>SOURCE FORMAT FREE` directive on line 1
- All four divisions present: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
- Uses ACCEPT/DISPLAY for stdio, not file I/O

## JSON parsing approach
Hand-rolled parser using character-by-character scanning:
- `EXTRACT-JSON-VALUE` - top-level key extraction
- `EXTRACT-JSON-VALUE-IN-PARAMS` - one level of nesting (params.key)
- `EXTRACT-JSON-VALUE-IN-ARGUMENTS` - two levels of nesting (params.arguments.key)
- INSPECT TALLYING for quick key existence check, then position scanning

## Currency formatting PIC clause
`PIC $$$$,$$$,$$$.99` is the floating dollar sign form with comma separators and 2 decimal places. Handles values up to ~$10 billion. For zero it produces `$.00`.

## String continuation in free-format
Use `&` to continue string literals across lines in GnuCOBOL free-format:
```cobol
MOVE "First part"
    & " second part"
    TO WS-FIELD
```

## FUNCTION MOD for leap year checks
`FUNCTION MOD(year, 400)` works for modulo arithmetic. With `FUNCTION ALL INTRINSIC` in REPOSITORY, no need to qualify with FUNCTION keyword (but it works either way).

## EXIT PARAGRAPH for early returns
Use `EXIT PARAGRAPH` to bail out early from a paragraph (like a `return` statement). This is how early validation errors work in the tool handlers.
