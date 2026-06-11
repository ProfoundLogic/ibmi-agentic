     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7740R - PROMPT FOR AUDIT REVERSAL
     H*                                                               *
     H* PROGRAMER: 65                                                 *
     H* PROJECT:   WC609                                              *
     H* DATE:      5/11/00                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW7740F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F*
     F                                     INFDS(DBFDS2)
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     FWDELP     IF   E             DISK
     F                                     RENAME(PEL01:PEL02)
     F*
     FWDELL     IF   E           K DISK

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  DIVIDEND FILE
     C*
     C     DVKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     STRT01        TAG
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*  GET THE FUND MASTER RECORD
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQPRDS        SCPRDS
     C     QQCALL        COMP      '1'                                    09
     C*
     C                   MOVEL     UPEMP#        SCEMP#
     C                   MOVEL     UPDIV         SCDIV
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
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
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C*   F3 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*  F10 - SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F10 - TABLE FILE SEARCH - POLICY YEAR
     C*
     C     HFLD          IFEQ      'SCFYR'
     C*
     C*  EMPLOYER NUMBER EQUAL TO ZERO
     C*
     C     SCEMP#        IFEQ      *ZEROS
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN33                          SCFYR POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  SCEMP#=0
     C*
     C*  EMPLOYER NUMBER NOT EQUAL TO ZERO
     C*
     C     SCEMP#        IFNE      *ZEROS
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     XXFND         @1RRN
     C                   Z-ADD     DBRRN2        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   MOVE      *ON           *IN41                          SCEFDT POSITION
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     @1RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN33                          SCFYR POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  SCEMP#<>0
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C*  F10 - TABLE FILE SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM                    @@QTE             1
     C                   PARM      1             @@VIEW            1 0
     C                   PARM                    @@RRN3            9 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     PEM02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C*
     C*  GROUP YEAR EQUAL TO ZERO
     C*
     C     SCFYR         IFEQ      *ZEROS
     C*
     C                   MOVE      SCFND         XXFND
     C                   MOVE      SCEMP#        XXEMP#
     C                   MOVE      SCDIV         XXDIV
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      '2'           @@CALL
     C                   Z-ADD     XXFND         @1RRN
     C                   Z-ADD     DBRRN2        @1RRN2
     C*
     C                   CALL      'W0051R'
     C                   PARM                    @1KC              1
     C                   PARM                    @1KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @1RRN             9 0
     C                   PARM                    @1RRN2            9 0
     C                   PARM                    QQQTE             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C*
     C     @1RRN         IFNE      *ZEROS
     C*
     C     @1RRN         CHAIN     PEL02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   MOVE      *ON           *IN41                          SCEFDT POSITION
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  @1RRN<>0
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     @1RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN33                          SCFYR POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  SCFYR=0
     C*
     C*  GROUP YEAR NOT EQUAL TO ZERO
     C*
     C     SCFYR         IFNE      *ZEROS
     C                   MOVE      *ON           *IN41                          SCEFDT POSITION
     C                   ENDIF                                                  SCFYR<>0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN34                          SCEMP# POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C                   ENDIF                                                  *INKJ=ON
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
     C                   WRITE     SCR03
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
     C                   WRITE     SCR03
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C     SCFYR         CABEQ     *ZERO         STRT                     21     FYR REQUIRED
     C     SCEMP#        CABEQ     *ZERO         STRT                     20     EMP# REQUIRED
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              21
     C     *IN21         IFEQ      '1'
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE EMPLOYER NUMBER AND DIVISION
     C*
     C     EMKEY         CHAIN     PEM01                              20
     C     *IN20         CABEQ     '1'           STRT
     C                   Z-ADD     SCFYR         XXFYR
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C                   Z-ADD     UPCO#         XXCO#
     C                   Z-ADD     UPFND         XXFND
     C*
     C* VALIDATE THAT EMPLOYER DETAIL EXISTS.
     C*
     C     ELKY          CHAIN     PEL01                              46
     C     *IN46         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN46 EQ 1
     C*
     C*  EMPLOYER POLICY YEAR MUST BE AUDITED TO BE REVERSED.
     C*
     C     ELFAF         IFNE      'A'
     C                   MOVE      *ON           *IN47
     C                   GOTO      STRT
     C                   ENDIF                                                  ELFAF IFNE
     C*
     C*  WE HAVE PASSED ALL EDITS, SUBMIT AUDIT REVERSAL PROGRAM CALL.
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      XXEMP#        QQEMP#            9
     C                   MOVE      XXDIV         QQDIV             5
     C*
     C                   CALL      'W7740C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*   ADDED TABLE MD FOR SCMODE
     C*
