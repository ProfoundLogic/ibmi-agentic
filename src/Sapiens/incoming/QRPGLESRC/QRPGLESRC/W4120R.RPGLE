     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W4120R - PROMPT FOR EXPERIENCE BY PAYROLL CLASS REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/17/03 C:61  SUPPORT WA099 40004                            *
     H*                List outfile on prompt screen.                 *
     H*                                                               *
     H* 11/29/99  C:118 PROJECT: WC577                                *
     H*                 V3 screen fixes.                              *
     H*                                                               *
     H* 04/28/99  C:90 PROJ: 96WBK                                    *
     H*           ADD ENTRY OF NEW FIELD 'REPORT OUTPUT'. ONLY ALLOW  *
     H*           ENTRY OF TYPES 'P', 'F', OR 'B'.                    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FW4120F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDF1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMPRL     IF   E           K DISK
     FWMPRP     IF   E             DISK
     F                                     RENAME(PPR01:PPR02)

     D DBFDS           DS
     D  DBRRN                397    400B 0
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
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCCLAS
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
      ***** BEG ADD ******* 11/17/03 **********************************
      *
     C                   CALL      'SA030R'
     C                   PARM                    ##SUFX            3
      *
     C     'PDSRPT'      CAT(P)    ##SUFX        SCRPTL
     C                   CAT(P)    'if':1        SCRPTL
     C                   CAT(P)    'select':1    SCRPTL
     C                   CAT(P)    'ed.':0       SCRPTL
      *
      ***** END ADD ******* 11/17/03 **********************************
      *
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      QQPRDS        SCPRDS
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTE
     C*
     C*  LOAD FUND NAME
     C*
     C                   MOVE      *BLANK        SCNAME
     C                   MOVE      SCFND         XXFND
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        SCNAME
     C                   END
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
     C*   COMMAND 01 = END OF JOB
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     HFLD          IFEQ      'SCFYR'
     C*
     C                   CALL      'W0011R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      DBRRN         @@RRN             9 0
     C                   PARM      *ZEROS        @@YR              3 0
     C*
     C     *INKL         IFEQ      *ON
     C     @@YR          OREQ      *ZEROS
     C*
     C                   SETON                                        70
     C*
     C                   ELSE
     C*
     C                   MOVE      @@YR          SCFYR
     C                   SETON                                        71
     C*
     C                   ENDIF                                                  *INKL EQ *ON
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD SCFYR
     C*
     C     HFLD          IFEQ      'SCCLAS'
     C*
     C                   CALL      'W0630R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      *BLANKS       @@RTCD            3
     C                   PARM      2             @@VIEW            1 0
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C*
     C                   SETON                                        71
     C*
     C                   ELSE
     C*
     C     @@RRN         CHAIN     PPR02                              99
     C*
     C                   MOVE      PRCLAS        SCCLAS
     C                   SETON                                        72
     C*
     C                   ENDIF                                                  *INKL *ON
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD SCCLAS
     C*
      **** BEGIN ADD **** 04/28/99 ************************************
     C     HFLD          IFNE      'SCFYR'
     C     HFLD          ANDNE     'SCCLAS'
     C                   SETON                                        74
     C                   GOTO      STRT
     C                   ENDIF                                                  HFLD NE SCFYR
      **** END   ADD **** 04/28/99 ************************************
     C                   ENDIF                                                  *INKJ
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
     C*
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
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  VALIDATE THE FUND YEAR FOR THE FUND
     C*
     C                   MOVE      SCFYR         XXFYR             3 0
     C     F1KEY         CHAIN     PF101                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*  VALIDATE PAYROLL CLASS IF NOT EQUAL TO 0
     C*
     C     SCCLAS        IFNE      0
     C     PRKEY         CHAIN     PPR01                              22
     C     *IN22         CABEQ     '1'           STRT
     C                   END
     C*
     C*  VALIDATE THE ENTERED DATE
     C*
     C                   Z-ADD     SCDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTE
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
      **** BEGIN ADD **** 04/28/99 ************************************
      *
      * VALIDATE REPORT OUTPUT SELECTION
      *
     C     SCRPT         IFNE      'P'
     C     SCRPT         ANDNE     'F'
     C     SCRPT         ANDNE     'B'
     C                   SETON                                        73
     C                   GOTO      STRT
     C                   ENDIF                                                  SCRPT
      **** END   ADD **** 04/28/99 ************************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#             3
     C                   MOVE      SCFND         QQFND             3
     C                   MOVE      SCFYR         QQFYR             3
     C                   MOVE      SCCLAS        QQCLAS            5
     C                   MOVE      SCDTE         QQDTE             8
      **** BEGIN ADD **** 04/28/99 ************************************
     C                   MOVE      SCRPT         QQRPT             1
      **** END   ADD **** 04/28/99 ************************************
     C                   CALL      'W4120C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCLAS
     C                   PARM                    QQDTE
     C                   PARM                    UPOUTQ
     C                   PARM      'D'           MODE              1
      **** BEGIN ADD **** 04/28/99 ************************************
     C                   PARM                    QQRPT
      **** END   ADD **** 04/28/99 ************************************
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
