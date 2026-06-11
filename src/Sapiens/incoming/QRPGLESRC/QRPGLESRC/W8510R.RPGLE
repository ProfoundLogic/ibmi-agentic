     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*                                                               *
     H/TITLE W8510R - PROMPT FOR GROUP, POLICY YEAR, EMP#, AND DIV    *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/23/99 C:118 PROJECT: WC577                                *
     H*                 ADD F10=SEARCH ON POLICY YEARS. CHG REF TO    *
     H*                 FILE NAME IN DISPLAY FILE.
     H*                                                               *
     H*  10/31/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW8510F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     F***** BEGIN ADD **** 11/23/99 ***********************************
     F***WDF1P   IF  E                    DISK
     F***            PF101                             KRENAMEPF102
     F***** END   ADD **** 11/23/99 ***********************************
     FWMEML     IF   E           K DISK
     F***** BEGIN ADD **** 11/23/99 ***********************************
     F                                     INFDS(DBFDS2)
     F***** END   ADD **** 11/23/99 ***********************************
     FWMEMP     IF   E             DISK
     F                                     RENAME(PEM01:PEM02)
     D***PY SCOPYWW01I
      /COPY SCOPYWW01D
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5

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
     C*  GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*  POLICY YEAR KEY LIST.
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
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
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
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
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C                   MOVE      UPEMP#        SCEMP#
     C                   MOVE      UPDIV         SCDIV
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
     C     QQCALL        COMP      '1'                                    09
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
     C* F3 = EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
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
      *
      * F10 = SEARCH
      *
     C     *INKJ         IFEQ      *ON
      *
      * EMPLOYER # SEARCH
      *
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
      *
     C                   CALL      'W0050R'
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##MODE            1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      DBRRN         ##RRN2            9 0
     C                   PARM      'W'           ##QTE             1
     C                   PARM      2             ##VIEW            1 0
     C                   PARM      0             ##RRN3            9 0
      *
     C     *INKC         CABEQ     *ON           TERM
      *
     C     *INKL         IFEQ      *OFF
      *
     C     ##RRN         CHAIN     PEM02                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      EMEMP#        SCEMP#
     C                   MOVE      EMDIV         SCDIV
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ELSE
      *
     C                   MOVE      *ON           *IN29
      *
     C                   ENDIF                                                  *INKL IFEQ
      *
     C                   GOTO      STRT
      *
     C                   ENDIF                                                  HFLD IFEQ
     C*
     C***** BEGIN ADD **** 11/23/99 ***********************************
     C     HFLD          IFEQ      'SCFYR'
     C*
     C                   CALL      'W0011R'
     C                   PARM                    ##KC
     C                   PARM                    ##KL
     C                   PARM      '2'           QQCALL
     C                   PARM      DBRRN         ##RRN
     C                   PARM                    XXFYR
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C     XXFYR         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*
     C     XXFYR         IFNE      *ZERO
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      F1FYR         SCFYR
     C                   GOTO      STRT
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF
     C                   GOTO      STRT
     C                   ENDIF
     C                   GOTO      STRT
     C*
     C***** END   ADD **** 11/23/99 ***********************************
      *
     C                   ENDIF                                                  *INKJ IFEQ
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
     C*  VALIDATE THE POLICY YEAR ENTERED.
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C     F1KEY         CHAIN     PF101                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  VALIDATE EMP# & DIV IFNE 0
     C*
     C     SCEMP#        IFNE      0
     C     SCDIV         ORNE      0
     C     EMKEY         CHAIN     PEM01                              21
     C     *IN21         CABEQ     '1'           STRT
     C                   END
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCEMP#        QQEMP#            9
     C                   MOVE      SCDIV         QQDIV             5
     C                   MOVE      UPOUTQ        QQOUTQ           10
     C                   CALL      'W8510C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQOUTQ
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
