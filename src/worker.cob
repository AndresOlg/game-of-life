       IDENTIFICATION DIVISION.
       PROGRAM-ID. worker.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PARAM-NAME PIC X(7).
       01 PARAM-VALUE PIC 9(10).
       01 PARAM-OUTPUT PIC X(10).
       01 PARAM PIC 9(10) BINARY.
       01 PARAM-COUNTER PIC 9(2) VALUE 0.
       01 DREW PIC 9 VALUE 0.
       01 TOTAL-ROWS PIC 9(2) VALUE 20.
       01 TOTAL-COLUMNS PIC 9(2) VALUE 15.
       01 ROW-COUNTER PIC 9(2) VALUE 0.
       01 COLUMN-COUNTER PIC 9(2) VALUE 0.
       01 OLD-WORLD PIC X(300).
       01 NEW-WORLD PIC X(300).
       01 CELL PIC X(1) VALUE "0".
       01 X PIC 9(2) VALUE 0.
       01 Y PIC 9(2) VALUE 0.
       01 POS PIC 9(3).
       01 ROW-OFFSET PIC S9.
       01 COLUMN-OFFSET PIC S9.
       01 NEIGHBORS PIC 9 VALUE 0.
       PROCEDURE DIVISION.
           CALL "get_http_form" USING "state" RETURNING PARAM.
	   IF PARAM = 1 THEN
	      PERFORM VARYING PARAM-COUNTER FROM 1 BY 1 UNTIL PARAM-COUNTER > 30
	         STRING "state" PARAM-COUNTER INTO PARAM-NAME
	         CALL "get_http_form" USING PARAM-NAME RETURNING PARAM-VALUE
		 COMPUTE POS = (PARAM-COUNTER - 1) * 10 + 1
		 MOVE PARAM-VALUE TO NEW-WORLD(POS:10)
	      END-PERFORM
 	  ELSE
	    MOVE "000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001110000000000001010000000000001010000000000000100000000000101110000000000010101000000000000100100000000001010000000000001010000000000000000000000000000000000000000000000000000000000000000000" TO NEW-WORLD.
           PERFORM PRINT-WORLD.
           MOVE NEW-WORLD TO OLD-WORLD.
           PERFORM VARYING ROW-COUNTER FROM 1 BY 1 UNTIL ROW-COUNTER > TOTAL-ROWS
               PERFORM ITERATE-CELL VARYING COLUMN-COUNTER FROM 1 BY 1 UNTIL COLUMN-COUNTER > TOTAL-COLUMNS
	   END-PERFORM.
	   PERFORM PRINT-FORM.
           STOP RUN.
       ITERATE-CELL.
           PERFORM COUNT-NEIGHBORS.
	   COMPUTE POS = (ROW-COUNTER - 1) * TOTAL-COLUMNS + COLUMN-COUNTER.
           MOVE OLD-WORLD(POS:1) TO CELL.
           IF CELL = "1" AND NEIGHBORS < 2 THEN
               MOVE "0" TO NEW-WORLD(POS:1).
           IF CELL = "1" AND (NEIGHBORS = 2 OR NEIGHBORS = 3) THEN
               MOVE "1" TO NEW-WORLD(POS:1).
           IF CELL = "1" AND NEIGHBORS > 3 THEN
               MOVE "0" TO NEW-WORLD(POS:1).
           IF CELL = "0" AND NEIGHBORS = 3 THEN
               MOVE "1" TO NEW-WORLD(POS:1).
       COUNT-NEIGHBORS.
           MOVE 0 TO NEIGHBORS.
	   PERFORM COUNT-NEIGHBOR
	       VARYING ROW-OFFSET FROM -1 BY 1 UNTIL ROW-OFFSET > 1
	          AFTER COLUMN-OFFSET FROM -1 BY 1 UNTIL COLUMN-OFFSET > 1.
       COUNT-NEIGHBOR.
           IF ROW-OFFSET <> 0 OR COLUMN-OFFSET <> 0 THEN
               COMPUTE Y = ROW-COUNTER + ROW-OFFSET
               COMPUTE X = COLUMN-COUNTER + COLUMN-OFFSET
               IF X >= 1 AND X <= TOTAL-ROWS AND Y >= 1 AND Y <= TOTAL-COLUMNS THEN
	       	   COMPUTE POS = (Y - 1) * TOTAL-COLUMNS + X
                   MOVE OLD-WORLD(POS:1) TO CELL
		   IF CELL = "1" THEN
		      COMPUTE NEIGHBORS = NEIGHBORS + 1.
       PRINT-FORM.
           CALL "append_http_body" USING "<form name=frm1 method=POST><input type=hidden name=state value=".
	   CALL "append_http_body" USING DREW.
	   CALL "append_http_body" USING ">".
	   PERFORM VARYING PARAM-COUNTER FROM 1 BY 1 UNTIL PARAM-COUNTER > 30
    	       CALL "append_http_body" USING "<input type=hidden name=state"
	       CALL "append_http_body" USING PARAM-COUNTER
    	       CALL "append_http_body" USING " value="
	       COMPUTE POS = (PARAM-COUNTER - 1) * 10 + 1
	       MOVE NEW-WORLD(POS:10) TO PARAM-OUTPUT
	       CALL "append_http_body" USING PARAM-OUTPUT
    	       CALL "append_http_body" USING ">"
	   END-PERFORM
           CALL "append_http_body" USING "</form>".
       PRINT-WORLD.
           MOVE 0 TO DREW.
           CALL "set_http_status" USING "200".
           CALL "append_http_body" USING "<html><body onload='submit()'>"
           CALL "append_http_body" USING "<script>"
           CALL "append_http_body" USING "function submit() {"
           CALL "append_http_body" USING "function urlencodeFormData(fd){ var s = ''; for(var pair of fd.entries()){ s += (s?'&':'') + pair[0]+'='+pair[1]; } return s; } "
           CALL "append_http_body" USING "fetch('/', { method: 'POST', body: urlencodeFormData(new FormData(document.frm1))}).then(res => res.text()).then(page => { document.body.innerHTML = page; setTimeout(function() { submit() }, 1000)})"
           CALL "append_http_body" USING "}</script>"
           CALL "append_http_body" USING "<style>table { background-color: white; } td { width: 10px; height: 10px}</style>".
           CALL "append_http_body" USING "<table>".
           PERFORM PRINT-ROW VARYING ROW-COUNTER FROM 3 BY 1 UNTIL ROW-COUNTER >= TOTAL-ROWS - 1.
           CALL "append_http_body" USING "</table></body></html>".
       PRINT-ROW.
           CALL "append_http_body" USING "<tr>".
           PERFORM PRINT-CELL VARYING COLUMN-COUNTER FROM 3 BY 1 UNTIL COLUMN-COUNTER >= TOTAL-COLUMNS - 1.
           CALL "append_http_body" USING "</tr>".
       PRINT-CELL.
	   COMPUTE POS = (ROW-COUNTER - 1) * TOTAL-COLUMNS + COLUMN-COUNTER.
	   MOVE NEW-WORLD(POS:1) TO CELL.
           IF CELL = "1" THEN
	       MOVE 1 TO DREW
               CALL "append_http_body" USING "<td bgcolor=blue></td>".
           IF CELL = "0" THEN
               CALL "append_http_body" USING "<td></td>".
