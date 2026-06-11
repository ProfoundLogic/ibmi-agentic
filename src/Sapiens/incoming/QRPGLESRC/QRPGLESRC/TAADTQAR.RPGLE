     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     F* TAADTQAR - DISPLAY DATA QUEUE - CALLED BY TAADTQAC
     FLISTING   IF   F  132        DISK
     FTAADTQAD  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(RECORD:RRN)
     FTAADTQAP  IF A E           K DISK

     D DTA             S              1    DIM(132)                             LISTING DATA
     D MSG             S             35    DIM(2) CTDATA PERRCD(1)              MESSAGES
     D DSP1            S              4  0 DIM(15)                              HEX DISPLACEME
     D DSP3            S              4  0 DIM(15)                              CHAR DISPLACEM
     D                 DS
     D  DSP                    1     60  0
     D                                     DIM(15)                              DISPLACEMENTS
     D  DSP01                  1      4  0
     D  DSP02                  5      8  0
     D  DSP03                  9     12  0
     D  DSP04                 13     16  0
     D  DSP05                 17     20  0
     D  DSP06                 21     24  0
     D  DSP07                 25     28  0
     D  DSP08                 29     32  0
     D  DSP09                 33     36  0
     D  DSP0A                 37     40  0
     D  DSP0B                 41     44  0
     D  DSP0C                 45     48  0
     D  DSP0D                 49     52  0
     D  DSP0E                 53     56  0
     D  DSP0F                 57     60  0
     D                 DS
     D  FLD1                   1    525
     D                                     DIM(15)                              HEX DATA #1
     D  FLD11                  1     35
     D  FLD12                 36     70
     D  FLD13                 71    105
     D  FLD14                106    140
     D  FLD15                141    175
     D  FLD16                176    210
     D  FLD17                211    245
     D  FLD18                246    280
     D  FLD19                281    315
     D  FLD1A                316    350
     D  FLD1B                351    385
     D  FLD1C                386    420
     D  FLD1D                421    455
     D  FLD1E                456    490
     D  FLD1F                491    525
     D                 DS
     D  FLD2                   1    525
     D                                     DIM(15)                              HEX DATA #2
     D  FLD21                  1     35
     D  FLD22                 36     70
     D  FLD23                 71    105
     D  FLD24                106    140
     D  FLD25                141    175
     D  FLD26                176    210
     D  FLD27                211    245
     D  FLD28                246    280
     D  FLD29                281    315
     D  FLD2A                316    350
     D  FLD2B                351    385
     D  FLD2C                386    420
     D  FLD2D                421    455
     D  FLD2E                456    490
     D  FLD2F                491    525
     D                 DS
     D  FLD3                   1    960
     D                                     DIM(15)                              CHAR DATA
     D  FLD31                  1     64
     D  FLD32                 65    128
     D  FLD33                129    192
     D  FLD34                193    256
     D  FLD35                257    320
     D  FLD36                321    384
     D  FLD37                385    448
     D  FLD38                449    512
     D  FLD39                513    576
     D  FLD3A                577    640
     D  FLD3B                641    704
     D  FLD3C                705    768
     D  FLD3D                769    832
     D  FLD3E                833    896
     D  FLD3F                897    960

     ILISTING   AA  01
     I                                  1  132  DTA

     C     *ENTRY        PLIST                                                  PARM LIST
     C                   PARM                    DTAQ             10            DATA QUEUE
     C                   PARM                    LIB              10            LIBRARY
     C                   PARM                    MAXENT            5 0          MAX ENT SIZE
     C                   PARM                    SEQ               6            SEQUENCE
     C                   PARM                    KEYLEN            5 0          KEY LENGTH
     C                   PARM                    TEXT             50            TEXT
     C                   PARM                    FORCE             4            FORCE
     C                   PARM                    SNDR              4            SENDER
     C                   PARM                    OUTFMT            5            OUTPUT FORMA
     C                   PARM                    RTNCDE            8            RETURN CODE
     C* BLANK OUT RETURN CODE
     C                   MOVE      *BLANKS       RTNCDE                         RETURN CODE
     C* READ LISTING RECORD
     C     FILTG1        TAG                                                    FILTG1 TAG
     C                   READ      LISTING                                H1     READ FILE
     C   H1              GOTO      BADEND                                       GOTO BADEND
     C                   MOVEA     DTA(1)        TST8              8
     C* GET DATA QUEUE AND DATA QUEUE LIBRARY NAME
     C     TST8          IFEQ      'NAME-   '                                   NAME RECORD
     C                   MOVEA     DTA(14)       QUEUE                          DTAQ NAME
     C                   GOTO      FILTG1                                       LOOP BACK
     C                   END                                                    NAME RECORD
     C     TST8          IFEQ      'LIBRARY-'                                   LIB RECORD ?
     C                   MOVEA     DTA(14)       LIB                            LIB NAME
     C                   GOTO      FILTG1                                       LOOP BACK
     C                   END                                                    LIB RECORD
     C* GET DATA QUEUE ATTRIBUTES (SOME ARE PASSED IN AS PARMS)
     C     TST8          CABNE     '  000060'    FILTG1                         ATTR RECORD
     C                   Z-ADD     *ZERO         COUNT                          COUNT ATTRIB
     C                   MOVE      DTA(27)       TST1              1 0          HEX DIGIT #1
     C                   Z-ADD     TST1          TST2              2 0
     C     DTA(27)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      4096          WORK5             5 0          1ST X 4096
     C                   ADD       WORK5         COUNT
     C                   MOVE      DTA(28)       TST1                           HEX DIGIT #2
     C                   Z-ADD     TST1          TST2
     C     DTA(28)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      256           WORK5                          2ND X 256
     C                   ADD       WORK5         COUNT
     C                   MOVE      DTA(30)       TST1                           HEX DIGIT #3
     C                   Z-ADD     TST1          TST2
     C     DTA(30)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      16            WORK5                          3RD X 16
     C                   ADD       WORK5         COUNT
     C                   MOVE      DTA(31)       TST1                           HEX DIGIT #4
     C                   ADD       TST1          COUNT
     C     DTA(31)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             COUNT
     C                   END
     C                   MOVE      COUNT         COUNTX            5
     C* FIRST TIME LOGIC FOR SUBFILE
     C                   Z-ADD     0             RRN               4 0
     C     OUTFMT        COMP      '*HEX '                                96
     C* FILL SUBFILE
     C     FILL          TAG
     C                   Z-ADD     RRN           RRN1                           RRN1 USED FO
     C                   ADD       1             RRN1                             SFLRCDNBR
      *****************************************************************
      * FIX 07/29/90
      *
      * IF THE LAST DISPLAY SHOWED ALL REMAINING ENTRIES, WRITE THE
      * "NO MORE ENTRIES" MESSAGE TO THE SUBFILE AND GO DISPLAY
     C     SFLSW         IFEQ      'EOF'                                        ANY MORE ENT
     C                   MOVE      *BLANKS       ENTRY                          NO, BLANK OU
     C                   MOVE      *BLANKS       EDATE
     C                   MOVE      *BLANKS       ETIME
     C                   MOVE      MSG(2)        EDATA                          "NO MORE" ME
     C                   MOVE      'MSG'         SFLSW                          REMEMBER MSG
     C                   ADD       1             RRN
     C*****************************************************************
     C* FIX 07/30/90
     C*
     C* REMEMBER RRN OF SFL RECORD THAT CONTAINS EOF MESSAGE
     C                   Z-ADD     RRN           EOFRRN            4 0
     C*****************************************************************
     C                   WRITE     RECORD                                       SUBFILE
     C                   GOTO      DISPLY
     C                   END                                                    NO MORE ENTR
      *****************************************************************
     C                   Z-ADD     0             NUMBER            2 0
     C* IF DATA QUEUE IS EMPTY, DISPLAY SUBFILE WITH "EMPTY" MESSAGE
     C     COUNT         IFEQ      *ZERO                                        EMPTY SUBFIL
     C                   MOVE      *BLANKS       ENTRY                          BLANK OUT DA
     C                   MOVE      *BLANKS       EDATE
     C                   MOVE      *BLANKS       ETIME
     C                   MOVE      MSG(1)        EDATA                          "EMPTY" MESS
     C                   GOTO      WRITE
     C                   END                                                    EMPTY SUBFIL
     C* GET DATA QUEUE ENTRIES, WRITE TO SUBFILE
     C* FIND RECORD WITH ENTRY NUMBER ("." IN POSITION 1)
     C     FILTG2        TAG                                                    FILTG2 TAG
     C                   READ      LISTING                                H2     READ FILE
     C   H2              GOTO      BADEND                                       GOTO BADEND
     C     DTA(1)        CABNE     '.'           FILTG2
     C                   MOVEA     DTA(3)        ENTRY                          ENTRY NUMBER
     C* FIND RECORD WITH ENTRY DATE AND TIME (".ENQUEUE" IN POSITION 3)
     C     FILTG3        TAG                                                    FILTG3 TAG
     C                   READ      LISTING                                H2     READ FILE
     C   H2              GOTO      BADEND                                       GOTO BADEND
     C                   MOVEA     DTA(3)        TST8
     C     TST8          CABNE     '.ENQUEUE'    FILTG3
     C                   MOVEA     DTA(31)       EDATE                          ENTRY DATE
     C                   MOVEA     DTA(70)       ETIME                          ENTRY TIME
     C* FIND RECORD WITH FIRST 32 BYTES OF DATA ("000000  " IN POSITION
     C     FILTG4        TAG                                                    FILTG4 TAG
     C                   READ      LISTING                                H2     READ FILE
     C   H2              GOTO      BADEND                                       GOTO BADEND
     C                   MOVEA     DTA(3)        TST8
     C     TST8          CABNE     '000000  '    FILTG4
     C                   Z-ADD     1             EDISP                          DISPLACEMENT
     C                   MOVEA     DTA(12)       EDATA1                         HEX DATA #1
     C                   MOVEA     DTA(50)       EDATA2                         HEX DATA #2
     C                   MOVEA     DTA(88)       EDATA3                         CHAR DATA
     C                   Z-ADD     1             REC#                           KEY SUFFIX
     C                   WRITE     PFREC                                        KEYED PF
     C     OUTFMT        IFEQ      '*HEX'                                       DISPLAY HEX
     C                   MOVE      EDATA1        EDATA                          YES, DISPLAY
     C                   ELSE
     C                   MOVEL     EDATA3        EDATA                          NO, DISPLAY
     C                   END                                                    HEX/CHAR TES
     C* WRITE SUBFILE RECORD
     C     WRITE         TAG                                                    WRITE TAG
     C                   ADD       1             RRN
     C                   Z-ADD     RRN           RRNSAV            4 0          REMEMBER RRN
     C                   ADD       1             NUMBER
     C                   WRITE     RECORD                                       SUBFILE
     C* IF DATA QUEUE IS EMPTY, DISPLAY SUBFILE WITH "EMPTY" MESSAGE
     C     COUNT         CABEQ     *ZERO         DISPLY
     C* READ ALL OTHER RECORDS FOR ENTRY AND WRITE TO PF ("000"
     C*   POSITION 3)
      *****************************************************************
      * FIX 07/29/90
      *   FOLLOWING LINE WAS CHANGED FROM:
      *                    Z-ADD*ZERO     DISP    40
     C                   Z-ADD     32            DISP              4 0
      *   TO FIX PROBLEM WITH DATA QUEUES OF LENGTHS LESS THAN OR
      *   EQUAL TO THE LENGTH OF ONE LINE IN DMPOBJ OUTPUT (IE, 32).
      *****************************************************************
     C     DISP          DOWLT     MAXENT
     C     FILTG5        TAG                                                    FILTG5 TAG
     C                   READ      LISTING                                H2     READ FILE
     C   H2              GOTO      BADEND                                       GOTO BADEND
     C                   MOVEA     DTA(3)        TST3              3
     C                   MOVEA     DTA(7)        TST8
     C                   MOVEA     DTA(40)       TST8A             8
     C* SEE IF RECORD IS "LINES NNN TO NNN SAME AS ABOVE" LINE
     C     TST3          IFNE      *ZEROS
     C     TST8          ANDEQ     '  LINES '
     C     TST8A         ANDEQ     'SAME AS '
     C*****************************************************************
      * FIX 07/29/90
      *
      * WHEN "LINES NNNNNN TO NNNNNN SAME AS ABOVE" IS FOUND, COPY THE
      * APPROPRIATE AMOUNT OF "ABOVE" DATA INTO THE PHYSICAL FILE.
      *
      * THE NUMBER OF RECORDS TO WRITE IS CALCULATED AS FOLLOWS:
      *
      *       CONVERT BEGINNING DISPLACEMENT FROM HEX TO DECIMAL
      *       CONVERT ENDING DISPLACEMENT FROM HEX TO DECIMAL
      *       SUBTRACT BEGINNING DISPLACEMENT FROM ENDING DISPLACEMENT
      *       ADD 1 TO DIFFERENCE
      *       DIVIDE BY THE AMOUNT OF DATA IN ONE RECORD (32)
      *       ADD 1 TO QUOTIENT
      *       PERFORM A DO LOOP TO WRITE THAT MANY COPIES OF THE
      *          "ABOVE" DATA
      *
     C* CALCULATE BEGINNING DISPLACEMENT
     C                   Z-ADD     *ZERO         DISP                           DISPLACEMENT
     C                   MOVE      DTA(18)       TST1                           HEX DIGIT #1
     C                   Z-ADD     TST1          TST2
     C     DTA(18)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      4096          WORK5                          1ST X 4096
     C                   ADD       WORK5         DISPX
     C                   MOVE      DTA(19)       TST1                           HEX DIGIT #2
     C                   Z-ADD     TST1          TST2
     C     DTA(19)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      256           WORK5                          2ND X 256
     C                   ADD       WORK5         DISP
     C                   MOVE      DTA(20)       TST1                           HEX DIGIT #3
     C                   Z-ADD     TST1          TST2
     C     DTA(20)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      16            WORK5                          3RD X 16
     C                   ADD       WORK5         DISP
     C                   MOVE      DTA(21)       TST1                           HEX DIGIT #4
     C                   ADD       TST1          DISP
     C     DTA(21)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             DISP
     C                   END
     C* CALCULATE ENDING DISPLACEMENT
     C                   Z-ADD     *ZERO         DISPX             4 0          DISPLACEMENT
     C                   MOVE      DTA(34)       TST1                           HEX DIGIT #1
     C                   Z-ADD     TST1          TST2
     C     DTA(34)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      4096          WORK5                          1ST X 4096
     C                   ADD       WORK5         DISPX
     C                   MOVE      DTA(35)       TST1                           HEX DIGIT #2
     C                   Z-ADD     TST1          TST2
     C     DTA(35)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      256           WORK5                          2ND X 256
     C                   ADD       WORK5         DISPX
     C                   MOVE      DTA(36)       TST1                           HEX DIGIT #3
     C                   Z-ADD     TST1          TST2
     C     DTA(36)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      16            WORK5                          3RD X 16
     C                   ADD       WORK5         DISPX
     C                   MOVE      DTA(37)       TST1                           HEX DIGIT #4
     C                   ADD       TST1          DISPX
     C     DTA(37)       IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             DISPX
     C                   END
     C     DISPX         SUB       DISP          DIF               4 0
     C     DIF           DIV       32            QUOT              3 0
     C                   ADD       1             QUOT
     C                   DO        QUOT
     C                   Z-ADD     DISP          EDISP                          DISPLACEMENT
     C                   ADD       1             EDISP
     C                   ADD       1             REC#
     C                   WRITE     PFREC                                        KEYED PF
     C                   ADD       32            DISP              4 0
     C                   END
      * END OF FIX FOR "LINES NNNNNN TO NNNNNN SAME AS ABOVE"
      *****************************************************************
     C                   GOTO      FILTG5
     C                   END
     C*
     C* END OF PROCESSING FOR "LINES NNNNNN TO NNNNNN SAME AS ABOVE"
     C*****************************************************************
     C*
     C* IF NOT A RECORD CONTAINING DATA, GO BACK AND READ AGAIN
     C     TST3          IFNE      *ZERO
     C                   GOTO      FILTG5
     C* ELSE PROCESS NEXT 32 BYTES OF DATA
     C                   ELSE
     C* CALCULATE BEGINNING DISPLACEMENT
     C                   Z-ADD     *ZERO         DISP                           DISPLACEMENT
     C                   MOVE      DTA(5)        TST1                           HEX DIGIT #1
     C                   Z-ADD     TST1          TST2
     C     DTA(5)        IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      4096          WORK5                          1ST X 4096
     C                   ADD       WORK5         DISP
     C                   MOVE      DTA(6)        TST1                           HEX DIGIT #2
     C                   Z-ADD     TST1          TST2
     C     DTA(6)        IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      256           WORK5                          2ND X 256
     C                   ADD       WORK5         DISP
     C                   MOVE      DTA(7)        TST1                           HEX DIGIT #3
     C                   Z-ADD     TST1          TST2
     C     DTA(7)        IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             TST2
     C                   END
     C     TST2          MULT      16            WORK5                          3RD X 16
     C                   ADD       WORK5         DISP
     C                   MOVE      DTA(8)        TST1                           HEX DIGIT #4
     C                   ADD       TST1          DISP
     C     DTA(8)        IFLT      *ZERO                                        X'A'-->X'F'
     C                   ADD       9             DISP
     C                   END
     C                   END
     C* WRITE DATA RECORD TO PHYSICAL FILE
     C                   Z-ADD     DISP          EDISP                          DISPLACEMENT
     C                   ADD       1             EDISP
     C                   MOVEA     DTA(12)       EDATA1                         HEX DATA #1
     C                   MOVEA     DTA(50)       EDATA2                         HEX DATA #2
     C                   MOVEA     DTA(88)       EDATA3                         CHAR DATA
     C                   ADD       1             REC#
     C                   WRITE     PFREC                                        KEYED PF
     C                   ADD       32            DISP              4 0
     C                   END
     C*
     C* END OF DO LOOP FOR WRITING ALL THE DATA FOR A SINGLE DATA QUEUE
     C* ENTRY TO THE PHYSICAL FILE
     C*****************************************************************
     C* FIX 07/30/90
     C*
     C* CHECK TO SEE IF LAST ENTRY HAS BEEN WRITTEN TO SUBFILE AND SET
     C* INDICATION OF "END-OF-FILE"
     C     ENTRY         IFEQ      COUNTX
     C                   MOVE      'EOF'         SFLSW             3            REMEMBER EOF
     C                   END
     C*****************************************************************
     C*
     C* ELSE, DETERMINE IF NO MORE ENTRIES OR SUBFILE PAGE IS FULL
     C     NUMBER        IFEQ      15                                           PAGE FULL ?
     C                   GOTO      DISPLY                                       YES, GO DISP
     C                   END                                                    PAGE FULL
     C     SFLSW         IFEQ      'EOF'                                        ANY MORE ENT
     C                   MOVE      *BLANKS       ENTRY                          NO, BLANK OU
     C                   MOVE      *BLANKS       EDATE
     C                   MOVE      *BLANKS       ETIME
     C                   MOVE      MSG(2)        EDATA                          "NO MORE" ME
     C                   MOVE      'MSG'         SFLSW                          REMEMBER MSG
     C                   ADD       1             RRN
     C*****************************************************************
     C* FIX 07/30/90
     C*
     C* REMEMBER RRN OF SFL RECORD THAT CONTAINS EOF MESSAGE
     C                   Z-ADD     RRN           EOFRRN            4 0
     C*****************************************************************
     C                   WRITE     RECORD                                       SUBFILE
     C                   GOTO      DISPLY                                       GO DISPLAY
     C                   END                                                    NO MORE ENTR
     C* ELSE, GO GET MORE ENTRY RECORDS
     C                   GOTO      FILTG2                                       GO GET MORE
     C* DISPLAY SUBFILE
     C     DISPLY        TAG
     C                   Z-ADD     1             POSN
     C                   Z-ADD     1             REC#
     C                   WRITE     CONTROL                                      SUBFILE
     C                   WRITE     KEYS                                         KEY DESCRIPT
     C                   READ      CONTROL                                50    SUBFILE
     C     *INKC         IFEQ      '1'                                          F3 PRESSED ?
     C                   GOTO      ENDPGM                                       YES, QUIT
     C                   END
     C     *INKE         IFEQ      '1'                                          F5 PRESSED ?
     C                   MOVE      'REFRESH '    RTNCDE                         YES, REFRESH
     C                   GOTO      ENDPGM
     C                   END
     C     *IN95         IFEQ      '1'                                          ROLLUP PRESS
     C     COUNTX        IFEQ      *ZERO                                        NO ENTRIES,
     C                   GOTO      DISPLY                                       REDISPLAY SU
     C                   END
     C     SFLSW         IFEQ      'MSG'                                        NO MORE ENTR
     C                   Z-ADD     EOFRRN        RRN1
     C                   GOTO      DISPLY                                       REDISPLAY SU
     C                   END
     C                   Z-ADD     RRNSAV        RRN
     C                   GOTO      FILL                                         GO REFILL SU
     C                   END
     C     READC         TAG
     C                   READC     RECORD                                 50    SELECTION ?
     C     *IN50         CABEQ     '1'           DISPLY                         NO, REDISPLA
     C     OPT           CABEQ     *BLANK        DISPLY
     C                   MOVE      *BLANK        OPT                            YES, BLANK O
     C                   UPDATE    RECORD                                         OPT
     C     ENTRY         IFEQ      *BLANKS                                      IF NO ENTRY
     C                   SETON                                        91          SET ERROR,
     C                   GOTO      DISPLY                                         AND REDISP
     C                   END                                                    ELSE
     C                   MOVE      ENTRY         RRN1                             READ
     C     REPOSN        TAG                                                      RECORDS
     C     KEY           KLIST                                                    FOR
     C                   KFLD                    ENTRY                            SELECTED
     C                   KFLD                    REC#                             ENTRY
     C     KEY           SETLL     TAADTQAP
     C                   Z-ADD     1             N                 2 0
     C                   MOVE      *BLANK        SW
     C     1             DO        15            X                 2 0
     C     ENTRY         READE     TAADTQAP                               50
     C     *IN50         CABEQ     '1'           DSPLY2
     C                   Z-ADD     EDISP         DSP1(X)                        PUT HEX DATA
     C                   MOVE      EDATA1        FLD1(X)                          IN ARRAYS
     C                   MOVE      EDATA2        FLD2(X)
     C     SW            IFEQ      *BLANK                                       PUT CHAR DAT
     C*****************************************************************
      * FIX 07/29/90
      *
      * CLEAR OUT RESIDUE DATA
     C                   MOVE      *BLANK        FLD3(N)
     C*****************************************************************
     C                   Z-ADD     EDISP         DSP3(N)                          IN ARRAY,
     C                   MOVEL     EDATA3        FLD3(N)                          32 BYTES A
     C                   MOVE      'X'           SW                1              A TIME,
     C                   ELSE                                                     ALTERNATIN
     C                   MOVE      EDATA3        FLD3(N)                          BETWEEN
     C                   ADD       1             N                                LEFT AND
     C                   MOVE      *BLANK        SW                               RIGHT SIDE
     C                   END                                                      OF 64-CHAR
     C                   END                                                      FLD3
     C     DSPLY2        TAG                                                    DSPLY2 TAG
     C     SW            IFEQ      'X'
     C*                                                    ADJUST INDEX
     C                   ADD       1             N                                FOR CHAR
     C                   END                                                      ARRAYS
     C     X             IFLE      15                                           IF ARRAYS
     C                   MOVEA     *ZEROS        DSP1(X)                          NOT FILLED
     C                   MOVEA     *BLANKS       FLD1(X)                          BLANK OUT
     C                   MOVEA     *BLANKS       FLD2(X)                          REMAINING
     C                   MOVEA     *ZEROS        DSP3(N)                          ELEMENTS
     C                   MOVEA     *BLANKS       FLD3(N)
     C                   END
     C     *IN96         IFEQ      '1'                                          HEX DISPLAY
     C                   MOVEA     DSP1          DSP                            YES, USE DSP
     C                   ELSE                                                   CHAR DISPLAY
     C                   MOVEA     DSP3          DSP                            YES, USE DSP
     C                   END
     C                   Z-ADD     POSN          POSSAV            4 0          REMEMBER POS
     C     DSPLY3        TAG
     C                   EXFMT     SINGLE                                       NOT SUBFILE
     C     *INKC         IFEQ      '1'                                          F3 PRESSED ?
     C                   GOTO      ENDPGM                                       YES, QUIT
     C                   END
     C     *INKJ         IFEQ      '1'                                          F10 PRESSED
     C     *IN96         IFEQ      '1'                                          NOW HEX ?
     C                   SETOFF                                       96        YES, DISPLAY
     C                   GOTO      DSPLY2                                       GO DISPLAY
     C                   ELSE                                                   NOW CHAR ?
     C                   SETON                                        96        YES, DISPLAY
     C                   GOTO      DSPLY2                                       GO DISPLAY
     C                   END                                                    HEX/CHAR TES
     C                   END                                                    F10 TEST
     C     *INKL         IFEQ      '1'                                          F12 PRESSED
     C                   GOTO      DISPLY                                       YES, PREVIOU
     C                   END                                                    F12 TEST
     C     *IN97         IFEQ      '1'                                          NEW POSITION
     C     POSN          CABEQ     POSSAV        DSPLY3                         NO, GO REDIS
     C     POSN          IFGT      MAXENT                                       INVALID POSN
     C                   Z-ADD     POSSAV        POSN                           YES, RESTORE
     C                   SETON                                        92          SET ERROR,
     C                   GOTO      DSPLY3                                         AND REDISP
     C                   END                                                    INVALID TEST
     C     POSN          DIV       32            REC#                           CALCULATE
     C                   MVR                     REM               2 0            BEGINNING
     C     REM           IFGT      *ZERO                                          RECORD NUM
     C                   ADD       1             REC#                             FOR REQUES
     C                   END                                                      POSITION
     C     REC#          DIV       2             QUOT                           IF AN EVEN
     C                   MVR                     REM               2 0            NUMBER, MA
     C     REM           IFEQ      *ZERO                                          IT NEXT
     C                   SUB       1             REC#                             SMALLER OD
     C                   END                                                      NUMBER
     C                   GOTO      REPOSN                                       REPOSITION
     C                   END                                                    NEW POSITION
     C                   GOTO      READC                                        MORE SELECTI
     C*****************************************************************
     C* END OF PROGRAM ROUTINE
     C     ENDPGM        TAG                                                    END TAG
     C     RTNCDE        IFEQ      *BLANK                                       IF BLANK
     C                   MOVE      'GOOD    '    RTNCDE                         SET GOOD
     C                   END                                                    IF BLANK
     C                   SETON                                        LR        SET LR
     C     BADEND        TAG                                                    BADEND TAG
     C                   RETURN                                                 RETURN
** MESSAGE ARRAY
    ***** NO ENTRIES ON QUEUE *****
   ***** END OF QUEUE ENTRIES *****
