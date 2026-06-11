     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0280R - REPORT DEFINITION MAINTENANCE
     H************************************************************
     H*  MODIFICATIONS:
     H*
     H*
     H* 08/16/99 C:118 PROJECT: WC577
     H*                MADE V3 SCREEN FIXES.
     H*
     H* 08/14/96 C:22 DISABLED THE ABILITY TO INACTIVATE A REPORT
     H* 10/16/96 C:02 ENABLE   THE ABILITY TO INACTIVATE A REPORT
     H* 03/10/97 C:02 ENABLE FUNCTION KEY 21 - SUBMIT REPORT
     H*
     H*     11/17/97    C:58     Support: WA081  #9236
     H*         o Pad  parm XX#PRM with leading zeroes.
     H*         o Scan repetitively for date keywords to detect
     H*           multiple use of the same keyword.
     H************************************************************
     FW0280F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMRDL1    UF A E           K DISK
     F************************************************************

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D SRK             S              1    DIM(8)
     D************************************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  CM                     1    256
     D                                     DIM(256)
     D  XXCOMP                 1    256
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D************************************************************
     D VALID           C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ0123456789#$@ ')

     I************************************************************

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  REPORT DEFINITION FILE
     C*
     C     RDKY1         KLIST
     C                   KFLD                    SCRPKY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     TOP           TAG
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCRPKY
     C*
     C*   DISPLAY ALL FORMATS
     C*
     C     STRT          TAG
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR03'
     C                   EXFMT     SCR03
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*   COMMAND 02 = PRIOR SCREEN
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C     Q2RECF        IFEQ      'SCR03'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C***** BEGIN CHG **** 08/16/99 ***********************************
     C********** COMMAND 05 = SEARCH LIST CALL
     C*
     C********** *INKE     IFEQ '1'
     C*
     C*   COMMAND 10 = SEARCH LIST CALL
     C*
     C     *INKJ         IFEQ      '1'
     C***** BEGIN CHG **** 08/16/99 ***********************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      *BLANK        QQKEY
     C                   MOVEL     SCRPKY        QQKEY             8
     C                   CALL      'W0281R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQKEY
     C                   PARM                    QQRTC             1
     C                   MOVEL     QQKEY         SCRPKY
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C     QQKEY         OREQ      *BLANK
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         IFEQ      'C'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C     RDKY1         CHAIN     PRD01                              40
     C                   MOVEL     RDKEY         SCRPKY
     C                   MOVE      RDDESC        SCDESC
     C                   MOVE      RDPGNM        SCPGNM
     C                   MOVE      RDSTAT        SCSTAT
     C                   MOVE      RDADPM        SCADPM
     C                   MOVE      RDOF01        SCOF01
     C                   MOVE      RDOF02        SCOF02
     C                   MOVE      RDOF03        SCOF03
     C                   MOVE      RDOF04        SCOF04
     C                   MOVE      RDOF05        SCOF05
     C                   MOVE      RDOF06        SCOF06
     C                   MOVE      RDOF07        SCOF07
     C                   MOVE      RDOF08        SCOF08
     C                   MOVE      RDOF09        SCOF09
     C                   MOVE      RDOF10        SCOF10
     C                   MOVE      RDOL01        SCOL01
     C                   MOVE      RDOL02        SCOL02
     C                   MOVE      RDOL03        SCOL03
     C                   MOVE      RDOL04        SCOL04
     C                   MOVE      RDOL05        SCOL05
     C                   MOVE      RDOL06        SCOL06
     C                   MOVE      RDOL07        SCOL07
     C                   MOVE      RDOL08        SCOL08
     C                   MOVE      RDOL09        SCOL09
     C                   MOVE      RDOL10        SCOL10
     C                   MOVE      *BLANKS       SCRPCP
     C                   MOVE      *BLANKS       SCDECP
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT
     C                   END
     C*** BEGIN DELETE 08/14/96 ***************************************
     C*** BEGIN ADD    10/16/96 ***************************************
     C*
     C*   CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C                   MOVE      SCSTAT        QQSTAT            1
     C                   CALL      'W0280R1'
     C                   PARM                    SCRPKY
     C                   PARM                    QQSTAT
     C*
     C                   GOTO      STRT
     C                   END
     C*** END ADD 10/16/96 ********************************************
     C*** END DEL 08/14/96 ********************************************
     C*** BEGIN ADD *** 11/17/94 ***
     C*
     C*   COMMAND 21 - SUBMIT REPORT
     C*   The steps that will be executed are as follows:
     C*   1) Go to SCR02T to edit fields
     C*   2) If edits are passed, within TAG SCR02T, AS/400 command
     C*      SBMJOB will be constructed.  Then a CALL to pgm QCMDEXC will
     C*      execute the command.
     C*
     C     *INKV         IFEQ      *ON
     C                   GOTO      SCR02T
     C                   END
     C*
     C*** END   ADD *** 11/17/94 ***
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        XXRECF           10
     C                   MOVEL     'QHLP    '    FILNAM           10
     C                   MOVEL     '*LIBL   '    LIBNAM           10
     C                   MOVE      Q1PGM         MBRNAM
     C                   MOVE      XXSCR#        MBRNAM           10
     C                   CALL      'M0006C'
     C                   PARM                    FILNAM
     C                   PARM                    LIBNAM
     C                   PARM                    MBRNAM
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C                   MOVE      XXRECF        Q2RECF
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
     C***** BEGIN DEL **** 08/16/99 ***********************************
     C**********           WRITESCR99
     C***** END   DEL **** 08/16/99 ***********************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      TOP
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE REPORT KEY CANNOT BE EQUAL TO ZERO
     C*
     C     SCRPKY        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*** INVALID CHARACTERS WERE KEYED!
     C*
     C     VALID         CHECK     SCRPKY                                 21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C                   MOVEA     SCRPKY        SRK
     C                   Z-ADD     0             XX                2 0
     C*
     C**** NO EMBEDDED BLANKS ARE ALLOWED !!!
     C*
     C     ' '           SCAN      SCRPKY        XX                       22
     C     *IN22         IFEQ      '1'
     C                   MOVEA     *BLANKS       SRK(XX)
     C                   MOVEA     SRK           SCRPKY
     C                   MOVE      '0'           *IN22
     C     SCRPKY        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C                   ENDIF
     C*
     C*
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     RDKY1         CHAIN     PRD01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PRD01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      RDDESC        SCDESC
     C                   MOVE      RDPGNM        SCPGNM
     C                   MOVE      RDADPM        SCADPM
     C                   MOVE      RDSTAT        SCSTAT
     C*                    MOVE RDRPGN    SCRPGN
     C                   MOVE      RDOF01        SCOF01
     C                   MOVE      RDOF02        SCOF02
     C                   MOVE      RDOF03        SCOF03
     C                   MOVE      RDOF04        SCOF04
     C                   MOVE      RDOF05        SCOF05
     C                   MOVE      RDOF06        SCOF06
     C                   MOVE      RDOF07        SCOF07
     C                   MOVE      RDOF08        SCOF08
     C                   MOVE      RDOF09        SCOF09
     C                   MOVE      RDOF10        SCOF10
     C                   MOVE      RDOL01        SCOL01
     C                   MOVE      RDOL02        SCOL02
     C                   MOVE      RDOL03        SCOL03
     C                   MOVE      RDOL04        SCOL04
     C                   MOVE      RDOL05        SCOL05
     C                   MOVE      RDOL06        SCOL06
     C                   MOVE      RDOL07        SCOL07
     C                   MOVE      RDOL08        SCOL08
     C                   MOVE      RDOL09        SCOL09
     C                   MOVE      RDOL10        SCOL10
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *BLANK        SCDESC
     C                   MOVE      *BLANK        SCPGNM
     C                   MOVE      *BLANKS       SCADPM
     C*                    MOVE *BLANKS   SCRPGN
     C                   MOVE      *BLANKS       SCOF01
     C                   MOVE      *BLANKS       SCOF02
     C                   MOVE      *BLANKS       SCOF03
     C                   MOVE      *BLANKS       SCOF04
     C                   MOVE      *BLANKS       SCOF05
     C                   MOVE      *BLANKS       SCOF06
     C                   MOVE      *BLANKS       SCOF07
     C                   MOVE      *BLANKS       SCOF08
     C                   MOVE      *BLANKS       SCOF09
     C                   MOVE      *BLANKS       SCOF10
     C                   MOVE      *BLANKS       SCOL01
     C                   MOVE      *BLANKS       SCOL02
     C                   MOVE      *BLANKS       SCOL03
     C                   MOVE      *BLANKS       SCOL04
     C                   MOVE      *BLANKS       SCOL05
     C                   MOVE      *BLANKS       SCOL06
     C                   MOVE      *BLANKS       SCOL07
     C                   MOVE      *BLANKS       SCOL08
     C                   MOVE      *BLANKS       SCOL09
     C                   MOVE      *BLANKS       SCOL10
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  THE REPORT DESCRIPTION CANNOT BE BLANK
     C*
     C     SCDESC        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE ZROGRAM NAME CANNOT BE BLANK
     C*
     C     SCPGNM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  CHECK THAT PARAMETER LIST IN ENCLOSED IN PARENTHESIS
     C*
     C                   MOVEA     SCADPM        CM
     C     CM(1)         IFNE      '('
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN ADD *** 11/17/94 ***
     C*
     C*   VERIFY ENDING PARENTHESIS ')'
     C*
     C                   Z-ADD     250           N                 3 0
     C     CM(N)         DOWEQ     *BLANK
     C     N             ANDGT     1
     C                   SUB       1             N
     C                   END
     C     CM(N)         IFNE      ')'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*   VERIFY MATCHING QUOTES
     C*
     C                   Z-ADD     *ZERO         QQ                3 0
     C                   Z-ADD     1             N
     C     N             DOWLT     251
     C     CM(N)         IFEQ      ''''
     C                   ADD       1             QQ
     C                   END
     C                   ADD       1             N
     C                   END
     C*
     C     QQ            DIV       2             RES               3 0
     C                   MVR                     REM               3 0
     C     REM           IFGT      *ZERO
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*** BEGIN ADD *** 11/17/94 ***
     C*
     C*  RPG PROGRAM NAME CANNOT BE BLANK
     C*
     C*          SCRPGN    IFEQ *BLANK
     C*                    MOVE '1'       *IN24
     C*                    GOTO STRT
     C*                    END
     C*
     C*  OUTPUT FILE 1 CANNOT BE BLANK
     C*
     C     SCOF01        IFEQ      *BLANK
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C*** BEGIN ADD *** 11/17/94 ***
     C     *IN11         IFEQ      *ON
     C     *INKV         ANDEQ     *ON
     C*   Build Command using Report Definitions
     C                   MOVEA     *BLANKS       CM
     C                   MOVEA     'CALL'        CM
     C                   Z-ADD     6             I1                5 0
     C                   MOVEA     SCPGNM        CM(I1)
     C                   Z-ADD     17            I1
     C                   MOVEA     SCADPM        CM(I1)
     C* SCAN FOR THE FOLLOWING RESERVED WORDS IN THE PARAMETERS LIST
     C* IF FOUND THEN THE PASSED IN PARM DATES WILL BE INSERTED OVER
     C* THE RESERVED WORDS
     C*
     C                   MOVE      DSDATE        QQDATE            8
     C                   Z-ADD     1             X1
     C     X1            DOUEQ     0
     C     'FRDATE'      SCAN      XXCOMP:X1     X1                5 0
     C     X1            IFNE      0
     C     X1            ADD       6             X2                3 0
     C                   SUBST(P)  XXCOMP:X2     EOL             256
     C                   MOVEA     QQDATE        CM(X1)
     C                   ADD       8             X1
     C                   MOVEA     EOL           CM(X1)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   Z-ADD     1             X1
     C     X1            DOUEQ     0
     C     'TODATE'      SCAN      XXCOMP:X1     X1                5 0
     C     X1            IFNE      0
     C     X1            ADD       6             X2
     C                   SUBST(P)  XXCOMP:X2     EOL
     C                   MOVEA     QQDATE        CM(X1)
     C                   ADD       8             X1
     C                   MOVEA     EOL           CM(X1)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   Z-ADD     1             X1
     C     X1            DOUEQ     0
     C     'RNDATE'      SCAN      XXCOMP:X1     X1                5 0
     C     X1            IFNE      0
     C     X1            ADD       6             X2
     C                   SUBST(P)  XXCOMP:X2     EOL
     C                   MOVEA     QQDATE        CM(X1)
     C                   ADD       8             X1
     C                   MOVEA     EOL           CM(X1)
     C                   ENDIF
     C                   ENDDO
     C*
     C*   Execute Command
     C                   MOVE      '00000'       XX#PRM
     C                   MOVE      RES           XX#PRM
     C                   CALL      'W0280C '
     C                   PARM                    XXCOMP
     C                   PARM                    SCRPKY
     C                   PARM                    SCPGNM
     C                   PARM                    XX#PRM            5
     C                   PARM                    XXRTCD            1
     C*
     C     XXRTCD        IFEQ      '1'
     C                   MOVE      *ON           *IN26
     C                   GOTO      STRT
     C***
     C                   ENDIF
     C*
     C                   END
     C*** IF PARMS MATCH....CONTINUE..ELSE ERROR MESSAGE, ALSO, UPDATE RCD
     C*** WITH DATE/TIME IF VALIDATED, ELSE UPDATE WITH *BLANKS!!!!
     C*** DATE/TIME UPDATE HERE DEPENDING ON RTCD/STUFF
     C     XXRTCD        IFEQ      '1'
     C                   MOVE      *ZEROS        RDDT
     C                   MOVE      *ZEROS        RDTIME
     C                   ELSE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDDT
     C                   Z-ADD     DSTIME        RDTIME
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXRTCD
     C*
     C*   Resume Normal Logic Flow
     C*
     C*** END   ADD *** 11/17/94 ***
     C*
     C     *IN09         IFEQ      '0'
     C     RDKY1         CHAIN     PRD01                              40
     C                   MOVE      UPCO#         RDCO#
     C                   MOVE      *BLANK        RDKEY
     C                   MOVEL     SCRPKY        RDKEY
     C                   MOVE      SCDESC        RDDESC
     C                   MOVE      SCPGNM        RDPGNM
     C                   MOVE      SCSTAT        RDSTAT
     C                   MOVE      SCADPM        RDADPM
     C*                    MOVE SCRPGN    RDRPGN
     C                   MOVE      SCOF01        RDOF01
     C                   MOVE      SCOF02        RDOF02
     C                   MOVE      SCOF03        RDOF03
     C                   MOVE      SCOF04        RDOF04
     C                   MOVE      SCOF05        RDOF05
     C                   MOVE      SCOF06        RDOF06
     C                   MOVE      SCOF07        RDOF07
     C                   MOVE      SCOF08        RDOF08
     C                   MOVE      SCOF09        RDOF09
     C                   MOVE      SCOF10        RDOF10
     C                   MOVE      SCOL01        RDOL01
     C                   MOVE      SCOL02        RDOL02
     C                   MOVE      SCOL03        RDOL03
     C                   MOVE      SCOL04        RDOL04
     C                   MOVE      SCOL05        RDOL05
     C                   MOVE      SCOL06        RDOL06
     C                   MOVE      SCOL07        RDOL07
     C                   MOVE      SCOL08        RDOL08
     C                   MOVE      SCOL09        RDOL09
     C                   MOVE      SCOL10        RDOL10
     C                   MOVE      Q1USER        RDUSER
     C                   MOVE      Q1PGM         RDPGM
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRD01
     C                   ELSE
     C                   WRITE     PRD01
     C                   END
     C*
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCRPKY
     C                   GOTO      STRT
     C*
     C     SCR03T        TAG
     C*
     C*  THE NEW REPORT KEY CANNOT BE BLANK
     C*
     C     SCRPCP        IFEQ      *BLANK
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE REPORT DESCRIPTION CANNOT BE BLANK
     C*
     C     SCDECP        IFEQ      *BLANK
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   MOVE      SCRPCP        SCRPKY
     C                   MOVE      SCDECP        SCDESC
     C*
     C     *IN09         IFEQ      '0'
     C     RDKY1         CHAIN     PRD01                              40
     C                   MOVE      UPCO#         RDCO#
     C                   MOVE      *BLANK        RDKEY
     C                   MOVEL     SCRPKY        RDKEY
     C                   MOVE      SCDESC        RDDESC
     C                   MOVE      SCPGNM        RDPGNM
     C                   MOVE      SCSTAT        RDSTAT
     C                   MOVE      SCADPM        RDADPM
     C                   MOVE      SCOF01        RDOF01
     C                   MOVE      SCOF02        RDOF02
     C                   MOVE      SCOF03        RDOF03
     C                   MOVE      SCOF04        RDOF04
     C                   MOVE      SCOF05        RDOF05
     C                   MOVE      SCOF06        RDOF06
     C                   MOVE      SCOF07        RDOF07
     C                   MOVE      SCOF08        RDOF08
     C                   MOVE      SCOF09        RDOF09
     C                   MOVE      SCOF10        RDOF10
     C                   MOVE      SCOL01        RDOL01
     C                   MOVE      SCOL02        RDOL02
     C                   MOVE      SCOL03        RDOL03
     C                   MOVE      SCOL04        RDOL04
     C                   MOVE      SCOL05        RDOL05
     C                   MOVE      SCOL06        RDOL06
     C                   MOVE      SCOL07        RDOL07
     C                   MOVE      SCOL08        RDOL08
     C                   MOVE      SCOL09        RDOL09
     C                   MOVE      SCOL10        RDOL10
     C                   MOVE      Q1USER        RDUSER
     C                   MOVE      Q1PGM         RDPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RDDT
     C                   Z-ADD     DSTIME        RDTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRD01
     C                   ELSE
     C                   WRITE     PRD01
     C                   END
     C*
     C                   END
     C                   MOVEL     'SCR01'       Q2RECF
     C                   MOVE      *BLANK        SCRPKY
     C                   GOTO      STRT
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPRD01     E            PRD01E
**  TABLE UA
-- ADD --
- UPDATE -
