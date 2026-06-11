     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7840R - PROMPT FOR AGENT ACTIVITY REPORT- AGT QOUTE SYS.
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/20/99 C:118 SUPPORT: WA071/19344                          *
     H*                 Add Agent# Search to program. Make display    *
     H*                 file changes.                                 *
     H*                                                               *
     H*  04/10/98 C:33 PROJECT: WC395                                 *
     H*                MOVE REQUIRED FIELDS TO TOP.                   *
     H*                                                               *
     H*  10/23/97 C:85 PROJECT: WC577                                 *
     H*                VERSION 3 REPORT STANDARDS.                    *
     H*                DELETED REFERENCES TO SCR99.                   *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW7840F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     F***** BEGIN ADD **** 08/20/99 ***********************************
     F                                     INFDS(DBFDSF)
     F***** END   ADD **** 08/20/99 ***********************************
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     F***** BEGIN ADD **** 08/20/99 ***********************************
     FWMAAP     IF   E             DISK
     F                                     RENAME(PAA01:PAA02)

     D*
     D DBFDSF          DS
     D  DBRRNF               397    400B 0
     D***** END   ADD **** 08/20/99 ***********************************
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
     C*  GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    SCFND             3 0
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    SCAGT#
     C*
     C     *LIKE         DEFINE    FNDT          XXFRDT
     C     *LIKE         DEFINE    FNDT          XXTODT
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
     C                   MOVE      UPFND         SCFND
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
     C*  GET THE GROUP NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL(P)  QQPRDS        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCPRDS
     C*
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
     C***** BEGIN ADD **** 08/20/99 ***********************************
     C*  F10=Search   Agent Numeric Search
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     HFLD          IFEQ      'SCAGT#'
     C*
     C                   CALL      'W0040R'
     C*
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      DBRRNF        ##RRN             9 0
     C                   PARM                    ##VIEW            1 0
     C*
     C     *INKC         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *INKC=ON
     C*
     C     *INKL         IFEQ      *ON
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*
     C     ##RRN         CHAIN     PAA02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AAAGT#        SCAGT#
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  HFLD=SCAGT#
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C***** END   ADD **** 08/20/99 ***********************************
     C*   PROCESS HELP REQUESTS
     C*
     C     *IN10         IFEQ      '1'
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
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C                   MOVE      UPFND         SCFND
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVE      FNNAME        SCNAME
     C                   GOTO      STRT01
     C                   END
     C*
     C                   END
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C***  BEGIN DEL  ***  04/10/98  *********************************
     C**********
     C***VALIDATE AGT# IF ENTERED
     C**********
     C********** SCAGT#    IFNE 0
     C********** AAKEY     CHAINPAA01                20
     C********** *IN20     CABEQ'1'       STRT
     C**********           END
     C***  END   DEL  ***  04/10/98  *********************************
     C*
     C*  EDIT THE FROM DATE
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*  EDIT THE TO DATE
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCTODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  DATES MUST BE IN ASCENDING ORDER
     C*
     C*
     C                   Z-ADD     SCFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     SCTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C     XXFRDT        IFGT      XXTODT
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C***  BEGIN ADD  ***  04/10/98  *********************************
     C*
     C*  VALIDATE AGT# IF ENTERED
     C*
     C     SCAGT#        IFNE      *ZERO
     C*
     C     AAKEY         CHAIN     PAA01                              20
     C*
     C     *IN20         CABEQ     *ON           STRT
     C*
     C                   ENDIF                                                  SCAGT#<>*ZERO
     C***  END   ADD  ***  04/10/98  *********************************
     C*
     C*   CALL THE CLP TO SUBMIT THE LIST.
     C*
     C                   MOVE      UPCO#         QQCO#
     C                   MOVE      SCFND         QQFND
     C                   MOVE      SCAGT#        QQAGT#
     C                   MOVE      SCFRDT        QQFRDT
     C                   MOVE      SCTODT        QQTODT
     C                   CALL      'W7840C'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    UPOUTQ
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
