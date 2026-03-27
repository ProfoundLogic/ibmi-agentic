      * description: This is part of CBLCALLP1 Unit test
      * returns: RESULT
      * expected: "OK"
      * parm: {"config": {"elements": {"lkType": {"length": 10, "type": "char"}}
      * inprogress: true
      *--------------------------------------------

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLCALLP2.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01  RESULT                  PIC X(2) VALUE SPACES.
       01  NPARMS  PIC S9(9) BINARY.
       LINKAGE SECTION.
       01  LK-PARM.
           03 LK-TYPE              PIC X(10).
           03 LK-RESP              PIC X(10).
       PROCEDURE DIVISION USING LK-PARM.
       MAIN-LOGIC.
      *----If called as single unit test
            IF LK-TYPE = 'UTEST'
              MOVE 'OK' TO RESULT
              DISPLAY RESULT
              STOP RUN
            END-IF

      *--- IF called by other unit tests
           MOVE 'ER' TO LK-RESP
           IF LK-TYPE = 'TEST1'
              MOVE 'OK1' TO LK-RESP
           ELSE IF LK-TYPE = 'TEST2'
              MOVE 'OK2' TO LK-RESP
           END-IF.

       EOJ.
           GOBACK.
