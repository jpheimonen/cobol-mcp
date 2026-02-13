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
*> (Placeholders for step 003 to populate)
*> ============================================================
01 WS-ADD-NUM-A              PIC S9(12)V9(4) VALUE 0.
01 WS-ADD-NUM-B              PIC S9(12)V9(4) VALUE 0.
01 WS-ADD-RESULT             PIC S9(12)V9(4) VALUE 0.
01 WS-ADD-RESULT-STR         PIC X(32)   VALUE SPACES.

01 WS-CURRENCY-INPUT         PIC S9(12)V9(2) VALUE 0.
01 WS-CURRENCY-FORMATTED     PIC $$$,$$$,$$$.99 VALUE SPACES.

01 WS-DATE-INPUT             PIC X(8)    VALUE SPACES.
01 WS-DATE-YEAR              PIC 9(4)    VALUE 0.
01 WS-DATE-MONTH             PIC 9(2)    VALUE 0.
01 WS-DATE-DAY               PIC 9(2)    VALUE 0.
01 WS-DATE-VALID-FLAG        PIC 9       VALUE 0.
   88 DATE-IS-VALID                      VALUE 1.
   88 DATE-IS-INVALID                    VALUE 0.
01 WS-DATE-ERROR-MSG         PIC X(128)  VALUE SPACES.

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
*> In this skeleton step, most handlers are placeholders.
*> Steps 002 and 003 will fill in the real logic.
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
*> Placeholder for initialize handler (step 002).
*> For now, returns a minimal success response.
*> ============================================================
HANDLE-INITIALIZE.
    IF ID-IS-PRESENT
        MOVE "{}" TO WS-RESULT-CONTENT
        PERFORM BUILD-SUCCESS-RESPONSE
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
*> Placeholder for ping handler (step 002).
*> For now, returns a minimal success response.
*> ============================================================
HANDLE-PING.
    IF ID-IS-PRESENT
        MOVE "{}" TO WS-RESULT-CONTENT
        PERFORM BUILD-SUCCESS-RESPONSE
    END-IF
    .

*> ============================================================
*> HANDLE-TOOLS-LIST
*> Placeholder for tools/list handler (step 003).
*> For now, returns an empty tools array.
*> ============================================================
HANDLE-TOOLS-LIST.
    IF ID-IS-PRESENT
        MOVE "{}" TO WS-RESULT-CONTENT
        PERFORM BUILD-SUCCESS-RESPONSE
    END-IF
    .

*> ============================================================
*> HANDLE-TOOLS-CALL
*> Placeholder for tools/call handler (step 003).
*> For now, returns an error indicating not implemented.
*> ============================================================
HANDLE-TOOLS-CALL.
    IF ID-IS-PRESENT
        MOVE "{}" TO WS-RESULT-CONTENT
        PERFORM BUILD-SUCCESS-RESPONSE
    END-IF
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
