>>SOURCE FORMAT FREE
*> ============================================================
*> COBOL MCP SERVER
*> ============================================================
*> A fully functional Model Context Protocol (MCP) server
*> written entirely in COBOL. Because the world needed this.
*>
*> Communicates over stdio transport using JSON-RPC 2.0.
*> Exposes tools for arithmetic, currency formatting, and
*> date validation -- playing to COBOL's historic strengths.
*>
*> Target: GnuCOBOL (free-format source)
*> Protocol: MCP 2025-11-25
*> Transport: stdio (stdin/stdout, one JSON-RPC message per line)
*> ============================================================

IDENTIFICATION DIVISION.
PROGRAM-ID. MCP-SERVER.
AUTHOR. A BRAVE SOUL.
DATE-WRITTEN. 2025.

ENVIRONMENT DIVISION.
CONFIGURATION SECTION.
REPOSITORY.
    FUNCTION ALL INTRINSIC.

INPUT-OUTPUT SECTION.
FILE-CONTROL.
*> No file I/O needed -- we use ACCEPT/DISPLAY for stdio.

DATA DIVISION.

WORKING-STORAGE SECTION.

*> ============================================================
*> PROGRAM STATE FLAGS
*> ============================================================
01 WS-SERVER-RUNNING        PIC 9      VALUE 1.
   88 SERVER-RUNNING                    VALUE 1.
   88 SERVER-STOPPED                    VALUE 0.

01 WS-EOF-FLAG              PIC 9      VALUE 0.
   88 EOF-REACHED                       VALUE 1.
   88 EOF-NOT-REACHED                   VALUE 0.

01 WS-INITIALIZED-FLAG      PIC 9      VALUE 0.
   88 SERVER-IS-INITIALIZED             VALUE 1.
   88 SERVER-NOT-INITIALIZED            VALUE 0.

01 WS-RESPONSE-NEEDED       PIC 9      VALUE 0.
   88 RESPONSE-NEEDED                   VALUE 1.
   88 NO-RESPONSE-NEEDED               VALUE 0.

*> ============================================================
*> I/O BUFFERS
*> ============================================================
01 WS-INPUT-BUFFER           PIC X(4096).
01 WS-INPUT-LENGTH           PIC 9(4)   VALUE 0.
01 WS-OUTPUT-BUFFER          PIC X(4096).
01 WS-OUTPUT-LENGTH          PIC 9(4)   VALUE 0.

*> ============================================================
*> QUOTE CHARACTER CONSTANT
*> We need this because COBOL string literals use quotes as
*> delimiters, so embedding a quote requires special handling.
*> ============================================================
01 WS-QUOTE                  PIC X      VALUE X"22".

*> ============================================================
*> PARSED JSON-RPC FIELDS
*> ============================================================
01 WS-JSONRPC-VERSION        PIC X(10)  VALUE SPACES.
01 WS-METHOD                 PIC X(64)  VALUE SPACES.
01 WS-REQUEST-ID             PIC X(64)  VALUE SPACES.
01 WS-REQUEST-ID-NUMERIC     PIC 9      VALUE 0.
   88 ID-IS-NUMERIC                     VALUE 1.
   88 ID-IS-STRING                      VALUE 0.
01 WS-ID-PRESENT             PIC 9      VALUE 0.
   88 ID-IS-PRESENT                     VALUE 1.
   88 ID-NOT-PRESENT                    VALUE 0.

01 WS-PARSE-ERROR-FLAG       PIC 9      VALUE 0.
   88 PARSE-SUCCEEDED                   VALUE 0.
   88 PARSE-FAILED                      VALUE 1.

*> ============================================================
*> PARSED PARAMS FIELDS
*> ============================================================
01 WS-PARAM-NAME             PIC X(64)  VALUE SPACES.
01 WS-PARAM-ARG-A            PIC X(64)  VALUE SPACES.
01 WS-PARAM-ARG-B            PIC X(64)  VALUE SPACES.
01 WS-PARAM-ARG-VALUE        PIC X(256) VALUE SPACES.
01 WS-PARAM-ARG-DATE         PIC X(64)  VALUE SPACES.

*> ============================================================
*> JSON FIELD EXTRACTION WORKING VARIABLES
*> ============================================================
01 WS-SEARCH-KEY             PIC X(128) VALUE SPACES.
01 WS-SEARCH-KEY-LEN         PIC 9(4)   VALUE 0.
01 WS-EXTRACT-VALUE          PIC X(256) VALUE SPACES.
01 WS-EXTRACT-LENGTH         PIC 9(4)   VALUE 0.
01 WS-SCAN-POS               PIC 9(4)   VALUE 0.
01 WS-SCAN-CHAR              PIC X      VALUE SPACE.
01 WS-KEY-POS                PIC 9(4)   VALUE 0.
01 WS-VALUE-START            PIC 9(4)   VALUE 0.
01 WS-VALUE-END              PIC 9(4)   VALUE 0.
01 WS-VALUE-LEN              PIC 9(4)   VALUE 0.
01 WS-FOUND-FLAG             PIC 9      VALUE 0.
   88 KEY-FOUND                         VALUE 1.
   88 KEY-NOT-FOUND                     VALUE 0.
01 WS-IN-STRING-FLAG         PIC 9      VALUE 0.
   88 IN-STRING                         VALUE 1.
   88 NOT-IN-STRING                     VALUE 0.
01 WS-NESTING-DEPTH          PIC 9(2)   VALUE 0.
01 WS-TEMP-STRING            PIC X(256) VALUE SPACES.
01 WS-TEMP-POS               PIC 9(4)   VALUE 0.
01 WS-TEMP-LEN               PIC 9(4)   VALUE 0.
01 WS-CHAR-AT-POS            PIC X      VALUE SPACE.

*> ============================================================
*> JSON RESPONSE GENERATION WORKING VARIABLES
*> ============================================================
01 WS-RESULT-CONTENT         PIC X(2048) VALUE SPACES.
01 WS-ERROR-CODE             PIC S9(6)   VALUE 0.
01 WS-ERROR-CODE-STR         PIC -(5)9   VALUE SPACES.
01 WS-ERROR-CODE-TRIMMED     PIC X(7)    VALUE SPACES.
01 WS-ERROR-MESSAGE          PIC X(256)  VALUE SPACES.
01 WS-RESPONSE-TYPE          PIC 9       VALUE 0.
   88 RESP-TYPE-SUCCESS                  VALUE 1.
   88 RESP-TYPE-ERROR                    VALUE 2.
   88 RESP-TYPE-NONE                     VALUE 0.

*> ============================================================
*> TOOL EXECUTION WORKING VARIABLES
*> ============================================================

*> --- Add tool variables ---
01 WS-ADD-NUM-A              PIC S9(12)V9(4) VALUE 0.
01 WS-ADD-NUM-B              PIC S9(12)V9(4) VALUE 0.
01 WS-ADD-RESULT             PIC S9(12)V9(4) VALUE 0.
01 WS-ADD-RESULT-STR         PIC X(32)   VALUE SPACES.
01 WS-ADD-RESULT-EDITED      PIC -(12)9.9(4) VALUE SPACES.

*> --- Currency formatting tool variables ---
*> The crown jewel: COBOL's PICTURE clause for currency.
*> This is the one thing COBOL does better than every
*> modern language. Dollar sign, commas, decimal point,
*> zero suppression -- all in a single PIC clause.
01 WS-CURRENCY-INPUT         PIC S9(12)V9(2) VALUE 0.
01 WS-CURRENCY-FORMATTED     PIC $$$$,$$$,$$$.99.

*> --- Date validation tool variables ---
01 WS-DATE-INPUT             PIC X(8)    VALUE SPACES.
01 WS-DATE-YEAR              PIC 9(4)    VALUE 0.
01 WS-DATE-MONTH             PIC 9(2)    VALUE 0.
01 WS-DATE-DAY               PIC 9(2)    VALUE 0.
01 WS-DATE-VALID-FLAG        PIC 9       VALUE 0.
   88 DATE-IS-VALID                      VALUE 1.
   88 DATE-IS-INVALID                    VALUE 0.
01 WS-DATE-ERROR-MSG         PIC X(128)  VALUE SPACES.
01 WS-DATE-MAX-DAY           PIC 9(2)    VALUE 0.
01 WS-LEAP-YEAR-FLAG         PIC 9       VALUE 0.
   88 IS-LEAP-YEAR                       VALUE 1.
   88 IS-NOT-LEAP-YEAR                   VALUE 0.
01 WS-YEAR-REMAINDER         PIC 9(4)    VALUE 0.
01 WS-DATE-LEN               PIC 9(2)    VALUE 0.

*> --- Numeric validation variables ---
01 WS-NUMERIC-CHECK-VALUE    PIC X(64)   VALUE SPACES.
01 WS-NUMERIC-VALID-FLAG     PIC 9       VALUE 0.
   88 NUMERIC-IS-VALID                   VALUE 1.
   88 NUMERIC-IS-INVALID                 VALUE 0.
01 WS-NUM-CHECK-POS          PIC 9(4)    VALUE 0.
01 WS-NUM-CHECK-CHAR         PIC X       VALUE SPACE.
01 WS-NUM-CHECK-LEN          PIC 9(4)    VALUE 0.
01 WS-NUM-HAS-DECIMAL        PIC 9       VALUE 0.
01 WS-NUM-HAS-DIGIT          PIC 9       VALUE 0.

*> --- Tool result text building ---
01 WS-TOOL-RESULT-TEXT       PIC X(512)  VALUE SPACES.
01 WS-CURRENCY-TRIMMED       PIC X(32)   VALUE SPACES.

*> ============================================================
*> MISC WORKING VARIABLES
*> ============================================================
01 WS-TRIMMED-ID             PIC X(64)   VALUE SPACES.
01 WS-TRIMMED-RESULT         PIC X(2048) VALUE SPACES.
01 WS-TRIMMED-ERROR          PIC X(256)  VALUE SPACES.
01 WS-TRIMMED-METHOD         PIC X(64)   VALUE SPACES.

PROCEDURE DIVISION.

*> ============================================================
*> MAIN-PROGRAM
*> The main entry point. Runs the server event loop until
*> stdin is closed (EOF) or the server is stopped.
*> ============================================================
MAIN-PROGRAM.
    PERFORM MAIN-LOOP UNTIL NOT SERVER-RUNNING
    STOP RUN RETURNING 0
    .

*> ============================================================
*> MAIN-LOOP
*> Read one line from stdin, parse it, dispatch it, and
*> (if needed) write a response to stdout.
*> ============================================================
MAIN-LOOP.
    PERFORM READ-STDIN-LINE
    IF EOF-REACHED
        SET SERVER-STOPPED TO TRUE
    ELSE
        PERFORM INITIALIZE-PARSE-STATE
        PERFORM PARSE-JSON-RPC-MESSAGE
        IF PARSE-FAILED
            SET RESP-TYPE-ERROR TO TRUE
            MOVE -32700 TO WS-ERROR-CODE
            MOVE "Parse error" TO WS-ERROR-MESSAGE
            PERFORM BUILD-ERROR-RESPONSE
            PERFORM WRITE-STDOUT-LINE
        ELSE
            PERFORM DISPATCH-METHOD
            IF RESPONSE-NEEDED
                PERFORM WRITE-STDOUT-LINE
            END-IF
        END-IF
    END-IF
    .

*> ============================================================
*> READ-STDIN-LINE
*> Read a single line from stdin using ACCEPT.
*> On AT END (EOF), set the EOF flag.
*> ============================================================
READ-STDIN-LINE.
    MOVE SPACES TO WS-INPUT-BUFFER
    ACCEPT WS-INPUT-BUFFER FROM STANDARD-INPUT
        AT END
            SET EOF-REACHED TO TRUE
        NOT AT END
            SET EOF-NOT-REACHED TO TRUE
    END-ACCEPT
    .

*> ============================================================
*> INITIALIZE-PARSE-STATE
*> Reset all parsed fields before processing a new message.
*> ============================================================
INITIALIZE-PARSE-STATE.
    MOVE SPACES TO WS-JSONRPC-VERSION
    MOVE SPACES TO WS-METHOD
    MOVE SPACES TO WS-REQUEST-ID
    SET ID-NOT-PRESENT TO TRUE
    SET ID-IS-STRING TO TRUE
    MOVE 0 TO WS-REQUEST-ID-NUMERIC
    SET PARSE-SUCCEEDED TO TRUE
    SET NO-RESPONSE-NEEDED TO TRUE
    SET RESP-TYPE-NONE TO TRUE
    MOVE SPACES TO WS-PARAM-NAME
    MOVE SPACES TO WS-PARAM-ARG-A
    MOVE SPACES TO WS-PARAM-ARG-B
    MOVE SPACES TO WS-PARAM-ARG-VALUE
    MOVE SPACES TO WS-PARAM-ARG-DATE
    MOVE SPACES TO WS-RESULT-CONTENT
    MOVE 0 TO WS-ERROR-CODE
    MOVE SPACES TO WS-ERROR-MESSAGE
    MOVE SPACES TO WS-OUTPUT-BUFFER
    .

*> ============================================================
*> PARSE-JSON-RPC-MESSAGE
*> Extract the essential fields from a JSON-RPC message:
*>   - jsonrpc (version string)
*>   - method (string)
*>   - id (string or number, may be absent for notifications)
*>   - params.name (for tools/call)
*>   - params.arguments.a, params.arguments.b (for tools)
*>   - params.arguments.value (for format_currency)
*>   - params.arguments.date (for validate_date)
*> ============================================================
PARSE-JSON-RPC-MESSAGE.
    *> Extract "jsonrpc" field
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"jsonrpc"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE
    IF KEY-FOUND
        MOVE WS-EXTRACT-VALUE TO WS-JSONRPC-VERSION
    END-IF

    *> Extract "method" field
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"method"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE
    IF KEY-FOUND
        MOVE WS-EXTRACT-VALUE TO WS-METHOD
    ELSE
        SET PARSE-FAILED TO TRUE
    END-IF

    *> Extract "id" field
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"id"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE
    IF KEY-FOUND
        SET ID-IS-PRESENT TO TRUE
        MOVE WS-EXTRACT-VALUE TO WS-REQUEST-ID
        *> Check if the value is numeric (not quoted in original)
        *> We flagged this during extraction
    ELSE
        SET ID-NOT-PRESENT TO TRUE
    END-IF

    *> Extract "params"."name" field (for tools/call)
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"name"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE-IN-PARAMS
    IF KEY-FOUND
        MOVE WS-EXTRACT-VALUE TO WS-PARAM-NAME
    END-IF

    *> Extract params.arguments.a
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"a"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE-IN-ARGUMENTS
    IF KEY-FOUND
        MOVE WS-EXTRACT-VALUE TO WS-PARAM-ARG-A
    END-IF

    *> Extract params.arguments.b
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"b"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE-IN-ARGUMENTS
    IF KEY-FOUND
        MOVE WS-EXTRACT-VALUE TO WS-PARAM-ARG-B
    END-IF

    *> Extract params.arguments.value
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"value"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE-IN-ARGUMENTS
    IF KEY-FOUND
        MOVE WS-EXTRACT-VALUE TO WS-PARAM-ARG-VALUE
    END-IF

    *> Extract params.arguments.date
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"date"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE-IN-ARGUMENTS
    IF KEY-FOUND
        MOVE WS-EXTRACT-VALUE TO WS-PARAM-ARG-DATE
    END-IF
    .

*> ============================================================
*> EXTRACT-JSON-VALUE
*> Scan WS-INPUT-BUFFER for the key in WS-SEARCH-KEY and
*> extract the corresponding value into WS-EXTRACT-VALUE.
*> Sets KEY-FOUND or KEY-NOT-FOUND.
*>
*> Handles both string values (quoted) and numeric values
*> (unquoted). Sets WS-REQUEST-ID-NUMERIC flag when
*> extracting the "id" field with a numeric value.
*> ============================================================
EXTRACT-JSON-VALUE.
    SET KEY-NOT-FOUND TO TRUE
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE 0 TO WS-EXTRACT-LENGTH

    *> Find the search key in the input buffer
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-SEARCH-KEY))
        TO WS-SEARCH-KEY-LEN
    MOVE 0 TO WS-KEY-POS

    *> Scan for the key string
    INSPECT WS-INPUT-BUFFER TALLYING WS-KEY-POS
        FOR ALL WS-SEARCH-KEY(1:WS-SEARCH-KEY-LEN)

    IF WS-KEY-POS = 0
        SET KEY-NOT-FOUND TO TRUE
    ELSE
        *> Key found at least once. Now locate its position
        *> by scanning character by character
        PERFORM FIND-KEY-POSITION
        IF KEY-FOUND
            PERFORM EXTRACT-VALUE-AFTER-COLON
        END-IF
    END-IF
    .

*> ============================================================
*> FIND-KEY-POSITION
*> Locate the exact position of WS-SEARCH-KEY in the input
*> buffer by scanning character by character.
*> Sets WS-KEY-POS to the starting position (1-based).
*> ============================================================
FIND-KEY-POSITION.
    SET KEY-NOT-FOUND TO TRUE
    MOVE 1 TO WS-SCAN-POS
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-BUFFER))
        TO WS-INPUT-LENGTH

    PERFORM VARYING WS-SCAN-POS FROM 1 BY 1
        UNTIL WS-SCAN-POS > WS-INPUT-LENGTH
           OR KEY-FOUND
        IF WS-INPUT-BUFFER(WS-SCAN-POS:WS-SEARCH-KEY-LEN)
            = WS-SEARCH-KEY(1:WS-SEARCH-KEY-LEN)
            MOVE WS-SCAN-POS TO WS-KEY-POS
            SET KEY-FOUND TO TRUE
        END-IF
    END-PERFORM
    .

*> ============================================================
*> EXTRACT-VALUE-AFTER-COLON
*> Starting from WS-KEY-POS (where the key was found),
*> skip past the key, find the colon, skip whitespace,
*> and extract the value (string or numeric).
*> ============================================================
EXTRACT-VALUE-AFTER-COLON.
    *> Move past the key
    ADD WS-SEARCH-KEY-LEN TO WS-KEY-POS
    MOVE WS-KEY-POS TO WS-SCAN-POS

    *> Skip whitespace and find the colon
    PERFORM SKIP-WHITESPACE
    IF WS-SCAN-POS > WS-INPUT-LENGTH
        SET KEY-NOT-FOUND TO TRUE
    ELSE
        IF WS-INPUT-BUFFER(WS-SCAN-POS:1) = ":"
            ADD 1 TO WS-SCAN-POS
            PERFORM SKIP-WHITESPACE
            IF WS-SCAN-POS > WS-INPUT-LENGTH
                SET KEY-NOT-FOUND TO TRUE
            ELSE
                PERFORM EXTRACT-VALUE-AT-POSITION
            END-IF
        ELSE
            SET KEY-NOT-FOUND TO TRUE
        END-IF
    END-IF
    .

*> ============================================================
*> SKIP-WHITESPACE
*> Advance WS-SCAN-POS past any spaces or tabs.
*> ============================================================
SKIP-WHITESPACE.
    PERFORM UNTIL WS-SCAN-POS > WS-INPUT-LENGTH
        OR (WS-INPUT-BUFFER(WS-SCAN-POS:1) NOT = " "
            AND WS-INPUT-BUFFER(WS-SCAN-POS:1) NOT = X"09")
        ADD 1 TO WS-SCAN-POS
    END-PERFORM
    .

*> ============================================================
*> EXTRACT-VALUE-AT-POSITION
*> At WS-SCAN-POS, determine if the value is a quoted string
*> or an unquoted value (number, boolean, null) and extract it.
*> ============================================================
EXTRACT-VALUE-AT-POSITION.
    MOVE WS-INPUT-BUFFER(WS-SCAN-POS:1) TO WS-SCAN-CHAR

    IF WS-SCAN-CHAR = WS-QUOTE
        *> String value - extract content between quotes
        PERFORM EXTRACT-QUOTED-STRING
    ELSE
        *> Numeric or other unquoted value
        PERFORM EXTRACT-UNQUOTED-VALUE
    END-IF
    .

*> ============================================================
*> EXTRACT-QUOTED-STRING
*> Extract the content between double quotes starting at
*> WS-SCAN-POS. Result goes into WS-EXTRACT-VALUE.
*> Handles escaped quotes (\") by skipping them.
*> ============================================================
EXTRACT-QUOTED-STRING.
    *> Skip the opening quote
    ADD 1 TO WS-SCAN-POS
    MOVE WS-SCAN-POS TO WS-VALUE-START
    MOVE 0 TO WS-VALUE-LEN
    MOVE 0 TO WS-TEMP-POS

    PERFORM UNTIL WS-SCAN-POS > WS-INPUT-LENGTH
        MOVE WS-INPUT-BUFFER(WS-SCAN-POS:1) TO WS-SCAN-CHAR
        EVALUATE TRUE
            *> Check for backslash escape sequence
            WHEN WS-SCAN-CHAR = X"5C" AND
                 WS-SCAN-POS < WS-INPUT-LENGTH
                *> Skip escaped character (backslash + next char)
                ADD 2 TO WS-SCAN-POS
                ADD 2 TO WS-VALUE-LEN
            *> Check for closing quote
            WHEN WS-SCAN-CHAR = WS-QUOTE
                EXIT PERFORM
            *> Regular character
            WHEN OTHER
                ADD 1 TO WS-SCAN-POS
                ADD 1 TO WS-VALUE-LEN
        END-EVALUATE
    END-PERFORM

    IF WS-VALUE-LEN > 0
        MOVE WS-INPUT-BUFFER(WS-VALUE-START:WS-VALUE-LEN)
            TO WS-EXTRACT-VALUE
        MOVE WS-VALUE-LEN TO WS-EXTRACT-LENGTH
        SET KEY-FOUND TO TRUE
    ELSE
        *> Empty string is still a valid value
        MOVE SPACES TO WS-EXTRACT-VALUE
        MOVE 0 TO WS-EXTRACT-LENGTH
        SET KEY-FOUND TO TRUE
    END-IF
    .

*> ============================================================
*> EXTRACT-UNQUOTED-VALUE
*> Extract an unquoted value (number, boolean, null) starting
*> at WS-SCAN-POS. Stops at comma, closing brace, whitespace.
*> Sets ID-IS-NUMERIC when extracting "id" field.
*> ============================================================
EXTRACT-UNQUOTED-VALUE.
    MOVE WS-SCAN-POS TO WS-VALUE-START
    MOVE 0 TO WS-VALUE-LEN

    PERFORM UNTIL WS-SCAN-POS > WS-INPUT-LENGTH
        MOVE WS-INPUT-BUFFER(WS-SCAN-POS:1) TO WS-SCAN-CHAR
        IF WS-SCAN-CHAR = "," OR WS-SCAN-CHAR = "}"
            OR WS-SCAN-CHAR = "]" OR WS-SCAN-CHAR = " "
            OR WS-SCAN-CHAR = X"09"
            EXIT PERFORM
        END-IF
        ADD 1 TO WS-SCAN-POS
        ADD 1 TO WS-VALUE-LEN
    END-PERFORM

    IF WS-VALUE-LEN > 0
        MOVE WS-INPUT-BUFFER(WS-VALUE-START:WS-VALUE-LEN)
            TO WS-EXTRACT-VALUE
        MOVE WS-VALUE-LEN TO WS-EXTRACT-LENGTH
        SET KEY-FOUND TO TRUE
        *> If we were extracting "id", mark it as numeric
        IF WS-SEARCH-KEY(1:WS-SEARCH-KEY-LEN) = '"id"'
            SET ID-IS-NUMERIC TO TRUE
        END-IF
    ELSE
        SET KEY-NOT-FOUND TO TRUE
    END-IF
    .

*> ============================================================
*> EXTRACT-JSON-VALUE-IN-PARAMS
*> Like EXTRACT-JSON-VALUE but scans within the "params"
*> object only (one level of nesting).
*> First finds "params", then searches for the key within it.
*> ============================================================
EXTRACT-JSON-VALUE-IN-PARAMS.
    SET KEY-NOT-FOUND TO TRUE
    MOVE SPACES TO WS-EXTRACT-VALUE

    *> Find the "params" key first
    MOVE 1 TO WS-SCAN-POS
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-BUFFER))
        TO WS-INPUT-LENGTH

    PERFORM VARYING WS-SCAN-POS FROM 1 BY 1
        UNTIL WS-SCAN-POS > WS-INPUT-LENGTH
           OR KEY-FOUND
        IF WS-SCAN-POS + 7 <= WS-INPUT-LENGTH
            IF WS-INPUT-BUFFER(WS-SCAN-POS:8) = '"params"'
                SET KEY-FOUND TO TRUE
                MOVE WS-SCAN-POS TO WS-KEY-POS
            END-IF
        END-IF
    END-PERFORM

    IF KEY-NOT-FOUND
        EXIT PARAGRAPH
    END-IF

    *> Now search for our target key after "params":
    SET KEY-NOT-FOUND TO TRUE
    ADD 8 TO WS-KEY-POS
    MOVE WS-KEY-POS TO WS-SCAN-POS

    *> Skip to the opening brace of the params object
    PERFORM SKIP-WHITESPACE
    IF WS-SCAN-POS > WS-INPUT-LENGTH
        EXIT PARAGRAPH
    END-IF
    IF WS-INPUT-BUFFER(WS-SCAN-POS:1) = ":"
        ADD 1 TO WS-SCAN-POS
        PERFORM SKIP-WHITESPACE
    END-IF

    *> Now scan for the target key within params
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-SEARCH-KEY))
        TO WS-SEARCH-KEY-LEN

    PERFORM VARYING WS-SCAN-POS FROM WS-SCAN-POS BY 1
        UNTIL WS-SCAN-POS > WS-INPUT-LENGTH
           OR KEY-FOUND
        IF WS-SCAN-POS + WS-SEARCH-KEY-LEN - 1
            <= WS-INPUT-LENGTH
            IF WS-INPUT-BUFFER(
                WS-SCAN-POS:WS-SEARCH-KEY-LEN)
                = WS-SEARCH-KEY(1:WS-SEARCH-KEY-LEN)
                MOVE WS-SCAN-POS TO WS-KEY-POS
                SET KEY-FOUND TO TRUE
            END-IF
        END-IF
    END-PERFORM

    IF KEY-FOUND
        ADD WS-SEARCH-KEY-LEN TO WS-KEY-POS
        MOVE WS-KEY-POS TO WS-SCAN-POS
        PERFORM SKIP-WHITESPACE
        IF WS-INPUT-BUFFER(WS-SCAN-POS:1) = ":"
            ADD 1 TO WS-SCAN-POS
            PERFORM SKIP-WHITESPACE
            PERFORM EXTRACT-VALUE-AT-POSITION
        ELSE
            SET KEY-NOT-FOUND TO TRUE
        END-IF
    END-IF
    .

*> ============================================================
*> EXTRACT-JSON-VALUE-IN-ARGUMENTS
*> Searches within the "arguments" sub-object of "params".
*> Two levels of nesting: params -> arguments -> key.
*> ============================================================
EXTRACT-JSON-VALUE-IN-ARGUMENTS.
    SET KEY-NOT-FOUND TO TRUE
    MOVE SPACES TO WS-EXTRACT-VALUE

    *> Find "arguments" in the buffer
    MOVE 1 TO WS-SCAN-POS
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-INPUT-BUFFER))
        TO WS-INPUT-LENGTH

    PERFORM VARYING WS-SCAN-POS FROM 1 BY 1
        UNTIL WS-SCAN-POS > WS-INPUT-LENGTH
           OR KEY-FOUND
        IF WS-SCAN-POS + 10 <= WS-INPUT-LENGTH
            IF WS-INPUT-BUFFER(WS-SCAN-POS:11)
                = '"arguments"'
                SET KEY-FOUND TO TRUE
                MOVE WS-SCAN-POS TO WS-KEY-POS
            END-IF
        END-IF
    END-PERFORM

    IF KEY-NOT-FOUND
        EXIT PARAGRAPH
    END-IF

    *> Found "arguments" - now skip past it to the object
    SET KEY-NOT-FOUND TO TRUE
    ADD 11 TO WS-KEY-POS
    MOVE WS-KEY-POS TO WS-SCAN-POS

    PERFORM SKIP-WHITESPACE
    IF WS-SCAN-POS > WS-INPUT-LENGTH
        EXIT PARAGRAPH
    END-IF
    IF WS-INPUT-BUFFER(WS-SCAN-POS:1) = ":"
        ADD 1 TO WS-SCAN-POS
        PERFORM SKIP-WHITESPACE
    END-IF

    *> Now scan for our target key within arguments
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-SEARCH-KEY))
        TO WS-SEARCH-KEY-LEN

    PERFORM VARYING WS-SCAN-POS FROM WS-SCAN-POS BY 1
        UNTIL WS-SCAN-POS > WS-INPUT-LENGTH
           OR KEY-FOUND
        IF WS-SCAN-POS + WS-SEARCH-KEY-LEN - 1
            <= WS-INPUT-LENGTH
            IF WS-INPUT-BUFFER(
                WS-SCAN-POS:WS-SEARCH-KEY-LEN)
                = WS-SEARCH-KEY(1:WS-SEARCH-KEY-LEN)
                MOVE WS-SCAN-POS TO WS-KEY-POS
                SET KEY-FOUND TO TRUE
            END-IF
        END-IF
    END-PERFORM

    IF KEY-FOUND
        ADD WS-SEARCH-KEY-LEN TO WS-KEY-POS
        MOVE WS-KEY-POS TO WS-SCAN-POS
        PERFORM SKIP-WHITESPACE
        IF WS-INPUT-BUFFER(WS-SCAN-POS:1) = ":"
            ADD 1 TO WS-SCAN-POS
            PERFORM SKIP-WHITESPACE
            PERFORM EXTRACT-VALUE-AT-POSITION
        ELSE
            SET KEY-NOT-FOUND TO TRUE
        END-IF
    END-IF
    .

*> ============================================================
*> DISPATCH-METHOD
*> Route the parsed method to the appropriate handler.
*> Supports the full MCP lifecycle: initialize, initialized
*> notification, ping, tools/list, and tools/call.
*> Unknown methods receive a -32601 error response.
*> ============================================================
DISPATCH-METHOD.
    MOVE FUNCTION TRIM(WS-METHOD) TO WS-TRIMMED-METHOD

    EVALUATE WS-TRIMMED-METHOD
        WHEN "initialize"
            PERFORM HANDLE-INITIALIZE
        WHEN "notifications/initialized"
            PERFORM HANDLE-NOTIFICATIONS-INITIALIZED
        WHEN "ping"
            PERFORM HANDLE-PING
        WHEN "tools/list"
            PERFORM HANDLE-TOOLS-LIST
        WHEN "tools/call"
            PERFORM HANDLE-TOOLS-CALL
        WHEN OTHER
            PERFORM HANDLE-UNKNOWN-METHOD
    END-EVALUATE
    .

*> ============================================================
*> HANDLE-INITIALIZE
*> Respond with MCP server capabilities, protocol version,
*> and server identity. This is the MCP handshake response.
*>
*> Response shape:
*>   {"jsonrpc":"2.0","id":<id>,"result":{
*>     "protocolVersion":"2025-11-25",
*>     "serverInfo":{"name":"cobol-mcp-server","version":"1.0.0"},
*>     "capabilities":{"tools":{}}
*>   }}
*> ============================================================
HANDLE-INITIALIZE.
    IF ID-IS-PRESENT
        STRING
            '{"protocolVersion":"2025-11-25"'
            DELIMITED SIZE
            ',"serverInfo":{"name":"cobol-mcp-server"'
            DELIMITED SIZE
            ',"version":"1.0.0"}'
            DELIMITED SIZE
            ',"capabilities":{"tools":{}}}'
            DELIMITED SIZE
            INTO WS-RESULT-CONTENT
        END-STRING
        PERFORM BUILD-SUCCESS-RESPONSE
        SET SERVER-IS-INITIALIZED TO TRUE
    END-IF
    .

*> ============================================================
*> HANDLE-NOTIFICATIONS-INITIALIZED
*> Notification - no response needed.
*> Just set the initialized flag.
*> ============================================================
HANDLE-NOTIFICATIONS-INITIALIZED.
    SET SERVER-IS-INITIALIZED TO TRUE
    SET NO-RESPONSE-NEEDED TO TRUE
    .

*> ============================================================
*> HANDLE-PING
*> Respond with an empty result object to confirm the server
*> is alive. Can be received at any time during the session.
*> Response: {"jsonrpc":"2.0","id":<id>,"result":{}}
*> ============================================================
HANDLE-PING.
    IF ID-IS-PRESENT
        MOVE "{}" TO WS-RESULT-CONTENT
        PERFORM BUILD-SUCCESS-RESPONSE
    END-IF
    .

*> ============================================================
*> HANDLE-TOOLS-LIST
*> Returns the list of all three available tools with their
*> names, descriptions, and input schemas.
*> Each tool plays to a classic COBOL strength:
*>   - add: COMPUTE verb arithmetic (since 1959)
*>   - format_currency: PICTURE clause formatting
*>   - validate_date: date validation with leap years
*> ============================================================
HANDLE-TOOLS-LIST.
    IF ID-IS-PRESENT
        MOVE SPACES TO WS-RESULT-CONTENT
        STRING
            '{"tools":['

            '{"name":"add",'
            '"description":"Add two numbers together '
            'using COBOL COMPUTE -- the original cloud '
            'computing, circa 1959.",'
            '"inputSchema":{"type":"object",'
            '"properties":{"a":{"type":"number",'
            '"description":"First number"},'
            '"b":{"type":"number",'
            '"description":"Second number"}},'
            '"required":["a","b"]}}'

            ',{"name":"format_currency",'
            '"description":"Format a number as US '
            'currency using COBOL PICTURE clause. '
            'The one thing COBOL does better than every '
            'modern language.",'
            '"inputSchema":{"type":"object",'
            '"properties":{"amount":{"type":"number",'
            '"description":"Amount to format as currency'
            '"}},"required":["amount"]}}'

            ',{"name":"validate_date",'
            '"description":"Validate a date in YYYYMMDD '
            'format, including leap year rules. COBOL has '
            'been validating dates since before most '
            'programmers were born.",'
            '"inputSchema":{"type":"object",'
            '"properties":{"date":{"type":"string",'
            '"description":"Date in YYYYMMDD format"}},'
            '"required":["date"]}}'

            ']}'
            DELIMITED SIZE
            INTO WS-RESULT-CONTENT
        END-STRING
        PERFORM BUILD-SUCCESS-RESPONSE
    END-IF
    .

*> ============================================================
*> HANDLE-TOOLS-CALL
*> Routes to the appropriate tool handler based on params.name.
*> Dispatches to add, format_currency, or validate_date.
*> Returns isError for unknown tool names.
*>
*> The tool name was already extracted from params.name
*> during PARSE-JSON-RPC-MESSAGE into WS-PARAM-NAME.
*> ============================================================
HANDLE-TOOLS-CALL.
    IF ID-IS-PRESENT
        MOVE FUNCTION TRIM(WS-PARAM-NAME)
            TO WS-TRIMMED-METHOD
        EVALUATE WS-TRIMMED-METHOD
            WHEN "add"
                PERFORM HANDLE-TOOL-ADD
            WHEN "format_currency"
                PERFORM HANDLE-TOOL-FORMAT-CURRENCY
            WHEN "validate_date"
                PERFORM HANDLE-TOOL-VALIDATE-DATE
            WHEN OTHER
                PERFORM HANDLE-TOOL-NOT-FOUND
        END-EVALUATE
    END-IF
    .

*> ============================================================
*> HANDLE-TOOL-ADD
*> Add two numbers together using COBOL COMPUTE.
*> Extracts arguments "a" and "b" from the request,
*> validates they are numeric, computes the sum, and
*> returns the result as text.
*> ============================================================
HANDLE-TOOL-ADD.
    *> Validate that argument "a" was provided and is numeric
    MOVE FUNCTION TRIM(WS-PARAM-ARG-A)
        TO WS-NUMERIC-CHECK-VALUE
    PERFORM VALIDATE-NUMERIC-VALUE
    IF NUMERIC-IS-INVALID
        MOVE SPACES TO WS-RESULT-CONTENT
        STRING
            '{"isError":true,"content":[{"type":"text"'
            ',"text":"Invalid input: argument '
            WS-QUOTE DELIMITED SIZE
            'a' DELIMITED SIZE
            WS-QUOTE DELIMITED SIZE
            ' must be numeric"}]}'
            DELIMITED SIZE
            INTO WS-RESULT-CONTENT
        END-STRING
        PERFORM BUILD-SUCCESS-RESPONSE
        EXIT PARAGRAPH
    END-IF
    COMPUTE WS-ADD-NUM-A =
        FUNCTION NUMVAL(FUNCTION TRIM(WS-PARAM-ARG-A))

    *> Validate that argument "b" was provided and is numeric
    MOVE FUNCTION TRIM(WS-PARAM-ARG-B)
        TO WS-NUMERIC-CHECK-VALUE
    PERFORM VALIDATE-NUMERIC-VALUE
    IF NUMERIC-IS-INVALID
        MOVE SPACES TO WS-RESULT-CONTENT
        STRING
            '{"isError":true,"content":[{"type":"text"'
            ',"text":"Invalid input: argument '
            WS-QUOTE DELIMITED SIZE
            'b' DELIMITED SIZE
            WS-QUOTE DELIMITED SIZE
            ' must be numeric"}]}'
            DELIMITED SIZE
            INTO WS-RESULT-CONTENT
        END-STRING
        PERFORM BUILD-SUCCESS-RESPONSE
        EXIT PARAGRAPH
    END-IF
    COMPUTE WS-ADD-NUM-B =
        FUNCTION NUMVAL(FUNCTION TRIM(WS-PARAM-ARG-B))

    *> The big moment: COBOL COMPUTE does what it was born to do
    COMPUTE WS-ADD-RESULT = WS-ADD-NUM-A + WS-ADD-NUM-B

    *> Format the result as a displayable string
    MOVE WS-ADD-RESULT TO WS-ADD-RESULT-EDITED
    MOVE FUNCTION TRIM(WS-ADD-RESULT-EDITED)
        TO WS-ADD-RESULT-STR

    *> Build the tool result response
    MOVE SPACES TO WS-RESULT-CONTENT
    STRING
        '{"content":[{"type":"text","text":'
        '"COBOL COMPUTE says: '
        DELIMITED SIZE
        FUNCTION TRIM(WS-PARAM-ARG-A) DELIMITED SPACES
        ' + ' DELIMITED SIZE
        FUNCTION TRIM(WS-PARAM-ARG-B) DELIMITED SPACES
        ' = ' DELIMITED SIZE
        FUNCTION TRIM(WS-ADD-RESULT-STR) DELIMITED SPACES
        '"}]}' DELIMITED SIZE
        INTO WS-RESULT-CONTENT
    END-STRING
    PERFORM BUILD-SUCCESS-RESPONSE
    .

*> ============================================================
*> HANDLE-TOOL-FORMAT-CURRENCY
*> Format a number as US currency using COBOL's PICTURE clause.
*> This is the crown jewel -- COBOL's edited numeric pictures
*> with dollar signs, commas, decimal points, and zero
*> suppression are genuinely superior to most modern formatting
*> approaches. The PIC $$$$,$$$,$$$.99 clause does it all.
*> ============================================================
HANDLE-TOOL-FORMAT-CURRENCY.
    *> Extract the "amount" argument -- try both "amount" and
    *> "value" for flexibility (spec says "amount")
    MOVE SPACES TO WS-TOOL-RESULT-TEXT

    *> First try params.arguments.amount
    MOVE SPACES TO WS-EXTRACT-VALUE
    MOVE '"amount"' TO WS-SEARCH-KEY
    PERFORM EXTRACT-JSON-VALUE-IN-ARGUMENTS
    IF KEY-FOUND
        MOVE WS-EXTRACT-VALUE TO WS-PARAM-ARG-VALUE
    END-IF

    *> Validate the amount is numeric
    MOVE FUNCTION TRIM(WS-PARAM-ARG-VALUE)
        TO WS-NUMERIC-CHECK-VALUE
    PERFORM VALIDATE-NUMERIC-VALUE
    IF NUMERIC-IS-INVALID
        MOVE SPACES TO WS-RESULT-CONTENT
        STRING
            '{"isError":true,"content":[{"type":"text"'
            ',"text":"Invalid input: '
            'amount must be numeric"}]}'
            DELIMITED SIZE
            INTO WS-RESULT-CONTENT
        END-STRING
        PERFORM BUILD-SUCCESS-RESPONSE
        EXIT PARAGRAPH
    END-IF

    *> Convert to numeric and apply the PICTURE clause
    COMPUTE WS-CURRENCY-INPUT =
        FUNCTION NUMVAL(FUNCTION TRIM(WS-PARAM-ARG-VALUE))

    *> The magic happens here: MOVE to a PIC-edited field
    *> and COBOL's native formatting does all the work
    MOVE WS-CURRENCY-INPUT TO WS-CURRENCY-FORMATTED
    MOVE FUNCTION TRIM(WS-CURRENCY-FORMATTED)
        TO WS-CURRENCY-TRIMMED

    *> Build the tool result response
    MOVE SPACES TO WS-RESULT-CONTENT
    STRING
        '{"content":[{"type":"text","text":"'
        DELIMITED SIZE
        FUNCTION TRIM(WS-CURRENCY-TRIMMED) DELIMITED SPACES
        ' (formatted by COBOL PICTURE clause -- '
        'you are welcome)"}]}'
        DELIMITED SIZE
        INTO WS-RESULT-CONTENT
    END-STRING
    PERFORM BUILD-SUCCESS-RESPONSE
    .

*> ============================================================
*> HANDLE-TOOL-VALIDATE-DATE
*> Validate a date in YYYYMMDD format. Checks that the
*> string is exactly 8 numeric characters, the month is
*> 01-12, and the day is valid for that month, including
*> leap year handling for February.
*>
*> Leap year rules (COBOL has known these since 1959):
*>   - Divisible by 4 -> leap year
*>   - EXCEPT centuries (divisible by 100) -> not leap year
*>   - EXCEPT centuries divisible by 400 -> leap year
*> ============================================================
HANDLE-TOOL-VALIDATE-DATE.
    SET DATE-IS-VALID TO TRUE
    MOVE SPACES TO WS-DATE-ERROR-MSG

    *> Get the date argument
    MOVE FUNCTION TRIM(WS-PARAM-ARG-DATE)
        TO WS-DATE-INPUT

    *> Check that the input is exactly 8 characters
    MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-DATE-INPUT))
        TO WS-DATE-LEN
    IF WS-DATE-LEN NOT = 8
        SET DATE-IS-INVALID TO TRUE
        MOVE "Date must be exactly 8 characters in YYYYMMDD"
            & " format"
            TO WS-DATE-ERROR-MSG
        PERFORM BUILD-DATE-ERROR-RESULT
        EXIT PARAGRAPH
    END-IF

    *> Check that all 8 characters are numeric
    IF WS-DATE-INPUT(1:8) IS NOT NUMERIC
        SET DATE-IS-INVALID TO TRUE
        MOVE "Date must contain only numeric digits"
            & " (YYYYMMDD)"
            TO WS-DATE-ERROR-MSG
        PERFORM BUILD-DATE-ERROR-RESULT
        EXIT PARAGRAPH
    END-IF

    *> Parse into components
    MOVE WS-DATE-INPUT(1:4) TO WS-DATE-YEAR
    MOVE WS-DATE-INPUT(5:2) TO WS-DATE-MONTH
    MOVE WS-DATE-INPUT(7:2) TO WS-DATE-DAY

    *> Validate month range (01-12)
    IF WS-DATE-MONTH < 1 OR WS-DATE-MONTH > 12
        SET DATE-IS-INVALID TO TRUE
        MOVE "Invalid month -- must be between 01 and 12"
            TO WS-DATE-ERROR-MSG
        PERFORM BUILD-DATE-ERROR-RESULT
        EXIT PARAGRAPH
    END-IF

    *> Determine maximum days for the month
    PERFORM DETERMINE-DAYS-IN-MONTH

    *> Validate day range
    IF WS-DATE-DAY < 1 OR WS-DATE-DAY > WS-DATE-MAX-DAY
        SET DATE-IS-INVALID TO TRUE
        EVALUATE TRUE
            WHEN WS-DATE-MONTH = 2 AND WS-DATE-DAY = 29
                MOVE "February 29 is not valid -- "
                    & "not a leap year"
                    TO WS-DATE-ERROR-MSG
            WHEN OTHER
                MOVE "Invalid day for the given month"
                    TO WS-DATE-ERROR-MSG
        END-EVALUATE
        PERFORM BUILD-DATE-ERROR-RESULT
        EXIT PARAGRAPH
    END-IF

    *> If we get here, the date is valid
    MOVE SPACES TO WS-RESULT-CONTENT
    STRING
        '{"content":[{"type":"text","text":"'
        DELIMITED SIZE
        FUNCTION TRIM(WS-DATE-INPUT) DELIMITED SPACES
        ' is a valid date. COBOL has certified this '
        'since 1959."}]}'
        DELIMITED SIZE
        INTO WS-RESULT-CONTENT
    END-STRING
    PERFORM BUILD-SUCCESS-RESPONSE
    .

*> ============================================================
*> HANDLE-TOOL-NOT-FOUND
*> Returns a tool result with isError set to true when
*> an unknown tool name is requested via tools/call.
*> This is a tool execution error, not a protocol error.
*> ============================================================
HANDLE-TOOL-NOT-FOUND.
    STRING
        '{"isError":true,"content":[{"type":"text","text":'
        DELIMITED SIZE
        '"Unknown tool: '
        DELIMITED SIZE
        WS-TRIMMED-METHOD DELIMITED SPACES
        '"}]}' DELIMITED SIZE
        INTO WS-RESULT-CONTENT
    END-STRING
    PERFORM BUILD-SUCCESS-RESPONSE
    .

*> ============================================================
*> VALIDATE-NUMERIC-VALUE
*> Check if WS-NUMERIC-CHECK-VALUE contains a valid numeric
*> value (integer or decimal, optionally negative).
*> Sets NUMERIC-IS-VALID or NUMERIC-IS-INVALID.
*> Accepts: digits, optional leading minus, optional decimal.
*> ============================================================
VALIDATE-NUMERIC-VALUE.
    SET NUMERIC-IS-VALID TO TRUE
    MOVE 0 TO WS-NUM-HAS-DECIMAL
    MOVE 0 TO WS-NUM-HAS-DIGIT
    MOVE FUNCTION LENGTH(FUNCTION TRIM(
        WS-NUMERIC-CHECK-VALUE)) TO WS-NUM-CHECK-LEN

    *> Empty or blank value is invalid
    IF WS-NUM-CHECK-LEN = 0 OR
       FUNCTION TRIM(WS-NUMERIC-CHECK-VALUE) = SPACES
        SET NUMERIC-IS-INVALID TO TRUE
        EXIT PARAGRAPH
    END-IF

    PERFORM VARYING WS-NUM-CHECK-POS FROM 1 BY 1
        UNTIL WS-NUM-CHECK-POS > WS-NUM-CHECK-LEN
           OR NUMERIC-IS-INVALID
        MOVE WS-NUMERIC-CHECK-VALUE(WS-NUM-CHECK-POS:1)
            TO WS-NUM-CHECK-CHAR
        EVALUATE TRUE
            *> Leading minus sign is OK
            WHEN WS-NUM-CHECK-CHAR = "-"
                 AND WS-NUM-CHECK-POS = 1
                CONTINUE
            *> Digits are always OK
            WHEN WS-NUM-CHECK-CHAR >= "0"
                 AND WS-NUM-CHECK-CHAR <= "9"
                MOVE 1 TO WS-NUM-HAS-DIGIT
            *> One decimal point is OK
            WHEN WS-NUM-CHECK-CHAR = "."
                 AND WS-NUM-HAS-DECIMAL = 0
                MOVE 1 TO WS-NUM-HAS-DECIMAL
            *> Anything else is invalid
            WHEN OTHER
                SET NUMERIC-IS-INVALID TO TRUE
        END-EVALUATE
    END-PERFORM

    *> Must have at least one digit
    IF WS-NUM-HAS-DIGIT = 0
        SET NUMERIC-IS-INVALID TO TRUE
    END-IF
    .

*> ============================================================
*> DETERMINE-DAYS-IN-MONTH
*> Sets WS-DATE-MAX-DAY to the maximum valid day for the
*> month in WS-DATE-MONTH, taking into account leap years
*> for February.
*> ============================================================
DETERMINE-DAYS-IN-MONTH.
    EVALUATE WS-DATE-MONTH
        WHEN 1  MOVE 31 TO WS-DATE-MAX-DAY
        WHEN 2  PERFORM CHECK-LEAP-YEAR
                IF IS-LEAP-YEAR
                    MOVE 29 TO WS-DATE-MAX-DAY
                ELSE
                    MOVE 28 TO WS-DATE-MAX-DAY
                END-IF
        WHEN 3  MOVE 31 TO WS-DATE-MAX-DAY
        WHEN 4  MOVE 30 TO WS-DATE-MAX-DAY
        WHEN 5  MOVE 31 TO WS-DATE-MAX-DAY
        WHEN 6  MOVE 30 TO WS-DATE-MAX-DAY
        WHEN 7  MOVE 31 TO WS-DATE-MAX-DAY
        WHEN 8  MOVE 31 TO WS-DATE-MAX-DAY
        WHEN 9  MOVE 30 TO WS-DATE-MAX-DAY
        WHEN 10 MOVE 31 TO WS-DATE-MAX-DAY
        WHEN 11 MOVE 30 TO WS-DATE-MAX-DAY
        WHEN 12 MOVE 31 TO WS-DATE-MAX-DAY
    END-EVALUATE
    .

*> ============================================================
*> CHECK-LEAP-YEAR
*> Determine if WS-DATE-YEAR is a leap year.
*> Rules: divisible by 4, except centuries, except centuries
*> divisible by 400. Sets IS-LEAP-YEAR or IS-NOT-LEAP-YEAR.
*> ============================================================
CHECK-LEAP-YEAR.
    SET IS-NOT-LEAP-YEAR TO TRUE

    *> Divisible by 400 -> leap year
    COMPUTE WS-YEAR-REMAINDER =
        FUNCTION MOD(WS-DATE-YEAR, 400)
    IF WS-YEAR-REMAINDER = 0
        SET IS-LEAP-YEAR TO TRUE
        EXIT PARAGRAPH
    END-IF

    *> Divisible by 100 -> NOT leap year (century rule)
    COMPUTE WS-YEAR-REMAINDER =
        FUNCTION MOD(WS-DATE-YEAR, 100)
    IF WS-YEAR-REMAINDER = 0
        SET IS-NOT-LEAP-YEAR TO TRUE
        EXIT PARAGRAPH
    END-IF

    *> Divisible by 4 -> leap year
    COMPUTE WS-YEAR-REMAINDER =
        FUNCTION MOD(WS-DATE-YEAR, 4)
    IF WS-YEAR-REMAINDER = 0
        SET IS-LEAP-YEAR TO TRUE
    END-IF
    .

*> ============================================================
*> BUILD-DATE-ERROR-RESULT
*> Builds a tool result with isError:true for date validation
*> failures. Uses WS-DATE-ERROR-MSG for the error text.
*> ============================================================
BUILD-DATE-ERROR-RESULT.
    MOVE SPACES TO WS-RESULT-CONTENT
    STRING
        '{"isError":true,"content":[{"type":"text"'
        ',"text":"' DELIMITED SIZE
        FUNCTION TRIM(WS-DATE-ERROR-MSG) DELIMITED SPACES
        '"}]}' DELIMITED SIZE
        INTO WS-RESULT-CONTENT
    END-STRING
    PERFORM BUILD-SUCCESS-RESPONSE
    .

*> ============================================================
*> HANDLE-UNKNOWN-METHOD
*> Return a JSON-RPC error for unrecognized methods.
*> Error code -32601: Method not found.
*> ============================================================
HANDLE-UNKNOWN-METHOD.
    IF ID-IS-PRESENT
        MOVE -32601 TO WS-ERROR-CODE
        MOVE "Method not found" TO WS-ERROR-MESSAGE
        PERFORM BUILD-ERROR-RESPONSE
    END-IF
    .

*> ============================================================
*> BUILD-SUCCESS-RESPONSE
*> Construct a JSON-RPC success response in WS-OUTPUT-BUFFER.
*> Uses the request ID and result content.
*> Format: {"jsonrpc":"2.0","id":<id>,"result":<result>}
*> ============================================================
BUILD-SUCCESS-RESPONSE.
    MOVE SPACES TO WS-OUTPUT-BUFFER
    MOVE FUNCTION TRIM(WS-REQUEST-ID) TO WS-TRIMMED-ID
    MOVE FUNCTION TRIM(WS-RESULT-CONTENT)
        TO WS-TRIMMED-RESULT

    IF ID-IS-NUMERIC
        STRING
            '{"jsonrpc":"2.0","id":'
            DELIMITED SIZE
            WS-TRIMMED-ID DELIMITED SPACES
            ',"result":' DELIMITED SIZE
            WS-TRIMMED-RESULT DELIMITED SPACES
            '}' DELIMITED SIZE
            INTO WS-OUTPUT-BUFFER
        END-STRING
    ELSE
        STRING
            '{"jsonrpc":"2.0","id":"'
            DELIMITED SIZE
            WS-TRIMMED-ID DELIMITED SPACES
            '","result":' DELIMITED SIZE
            WS-TRIMMED-RESULT DELIMITED SPACES
            '}' DELIMITED SIZE
            INTO WS-OUTPUT-BUFFER
        END-STRING
    END-IF

    SET RESPONSE-NEEDED TO TRUE
    .

*> ============================================================
*> BUILD-ERROR-RESPONSE
*> Construct a JSON-RPC error response in WS-OUTPUT-BUFFER.
*> Format: {"jsonrpc":"2.0","id":<id>,
*>          "error":{"code":<code>,"message":"<msg>"}}
*> If no ID present, uses null.
*> ============================================================
BUILD-ERROR-RESPONSE.
    MOVE SPACES TO WS-OUTPUT-BUFFER
    MOVE FUNCTION TRIM(WS-ERROR-MESSAGE) TO WS-TRIMMED-ERROR

    *> Convert error code to displayable string with sign
    MOVE WS-ERROR-CODE TO WS-ERROR-CODE-STR
    MOVE FUNCTION TRIM(WS-ERROR-CODE-STR)
        TO WS-ERROR-CODE-TRIMMED

    IF ID-IS-PRESENT
        MOVE FUNCTION TRIM(WS-REQUEST-ID) TO WS-TRIMMED-ID

        IF ID-IS-NUMERIC
            STRING
                '{"jsonrpc":"2.0","id":'
                DELIMITED SIZE
                WS-TRIMMED-ID DELIMITED SPACES
                ',"error":{"code":' DELIMITED SIZE
                WS-ERROR-CODE-TRIMMED DELIMITED SPACES
                ',"message":"' DELIMITED SIZE
                WS-TRIMMED-ERROR DELIMITED SPACES
                '"}}' DELIMITED SIZE
                INTO WS-OUTPUT-BUFFER
            END-STRING
        ELSE
            STRING
                '{"jsonrpc":"2.0","id":"'
                DELIMITED SIZE
                WS-TRIMMED-ID DELIMITED SPACES
                '","error":{"code":' DELIMITED SIZE
                WS-ERROR-CODE-TRIMMED DELIMITED SPACES
                ',"message":"' DELIMITED SIZE
                WS-TRIMMED-ERROR DELIMITED SPACES
                '"}}' DELIMITED SIZE
                INTO WS-OUTPUT-BUFFER
            END-STRING
        END-IF
    ELSE
        STRING
            '{"jsonrpc":"2.0","id":null'
            DELIMITED SIZE
            ',"error":{"code":' DELIMITED SIZE
            WS-ERROR-CODE-TRIMMED DELIMITED SPACES
            ',"message":"' DELIMITED SIZE
            WS-TRIMMED-ERROR DELIMITED SPACES
            '"}}' DELIMITED SIZE
            INTO WS-OUTPUT-BUFFER
        END-STRING
    END-IF

    SET RESPONSE-NEEDED TO TRUE
    .

*> ============================================================
*> WRITE-STDOUT-LINE
*> Write the output buffer to stdout as a single line.
*> Uses DISPLAY which adds a newline automatically.
*> ============================================================
WRITE-STDOUT-LINE.
    DISPLAY FUNCTION TRIM(WS-OUTPUT-BUFFER)
        UPON STANDARD-OUTPUT
    .

END PROGRAM MCP-SERVER.
