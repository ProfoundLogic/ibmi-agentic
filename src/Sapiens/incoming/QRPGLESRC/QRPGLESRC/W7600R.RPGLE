     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7600R - PROMPT FOR AUDIT CHANGE LIST
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/08/98 C:85 PROJECT: WC577                                 *
     H*                ADDED SEARCHES FOR SCFYR, SCEMP# & SCDIV.      *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  SCR01:                                                       *
     H*  ERROR:                                                       *
     H*  21 - (WCR0538)                                               *
     H*  22 - (WCR0517)                                               *
     H*  23 - (WCR0957)                                               *
     H*  24 - (WCR0615)                                               *
     H*                                                               *
     H*  POSITION CURSOR:                                             *
     H*  30 - SCFYR POSITION                                          *
     H*  31 - SCEMP# POSITION                                         *
     H*  32 - SCTYPE POSITION                                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     FW7600F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F*** BEGIN ADD *** 01/08/98 **************************************
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 01/08/98 **************************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F*** BEGIN ADD *** 01/08/98 **************************************
     F                                     RENAME(PEM01:PEM02)
     F                                     INFDS(DBFDS2)
     FWMEMP     IF   E             DISK
     FWDELP     IF   E             DISK

     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*** END   ADD *** 01/08/98 **************************************
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
     C                   KFLD                    XXFND             3 0
     C*
     C*  FUND YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C*** BEGIN ADD *** 01/08/98 **************************************
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C*** END   ADD *** 01/08/98 **************************************
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
     C*
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C*** BEGIN ADD *** 01/08/98 **************************************
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
     C*** END   ADD *** 01/08/98 **************************************
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
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
     C*   PROCESS ALL COMMAND KEYS
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
     C*** BEGIN ADD *** 01/08/98 **************************************
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
     C     @@RRN         CHAIN     PEM01                              60
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
     C                   MOVE      *ON           *IN30                          SCFYR POSITION
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
     C     EMKEY         CHAIN     PEM02                              60
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
     C     @1RRN         CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELFYR         SCFYR
     C                   MOVE      *ON           *IN32                          SCTYPE POSITION
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
     C                   MOVE      *ON           *IN30                          SCFYR POSITION
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
     C     @@RRN         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EMFND         SCFND
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
     C                   MOVE      *ON           *IN32                          SCTYPE POSITION
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN31                          SCEMP# POSITION
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
     C*** END   ADD *** 01/08/98 **************************************
     C*
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
     C                   MOVE      XXRECF        Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*   PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      '1'
     C*
     C                   CALL      'W0006R'
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
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR ENTERED.
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C*  VALIDATE EMPLOYER/DIVISION IF ENTERED
     C*
     C     SCEMP#        IFNE      *ZERO
     C     EMKEY         CHAIN     PEM02                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE TYPE - ESTIMATED OR SELF-REPORTING
     C*
     C     SCTYPE        IFNE      'E'
     C     SCTYPE        ANDNE     'S'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      *BLANKS       QQFYR
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      SCTYPE        QQTYPE            1
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C*
     C                   CALL      'W7600C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQTYPE
     C                   PARM                    QQOUTQ
     C                   PARM      'D'           MODE              1
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
