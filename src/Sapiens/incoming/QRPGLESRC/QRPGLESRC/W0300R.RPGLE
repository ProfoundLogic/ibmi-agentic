     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0300R - REINSURANCE COMPANY FILE MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  09/09/14 C:180 SUPPORT: WA136 86567
     H*                 Handle record in use.
     H*
     H*  12/08/99 C:118 PROJECT: WC577
     H*                 V3 display file changes.
     H*
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0300F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMRCL     UF A E           K DISK

     D*                                                               *
     D*** BEGIN CHG *** 01/28/98 ***********************************************
     D**********          UA      1   2 10
     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
     D*** END   CHG *** 01/28/98 ***********************************************
     D*                                                               *
     D ST              S              2    DIM(52) CTDATA PERRCD(26)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Reinsurance Company -
     D                                     Maintenance')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

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
     C*   KEY LIST FOR THE REINSURANCE COMPANY FILE
     C*
     C     RCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXRCO             3 0
     C*
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
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE QQPRDS    SCPRDS
     C*
     C*  CENTER PROGRAM NAME
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        SCPRDS
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
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
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C*   PROCESS ALL COMMAND KEYS
     C*
     C*
     C     *IN11         IFEQ      '1'
     C*
     C* F3=EXIT
     C*
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C**********
     C**F10=SEARCH
     C**********
     C********** *INKJ     IFEQ '1'
     C********** HFLD      IFEQ 'SCRCO'
     C**********           CALL 'W0301R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQRCO   30
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** QQRCO     OREQ *ZERO
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTC     IFEQ '*'
     C**********           MOVE QQRCO     SCRCO
     C**********           GOTO STRT
     C**********           ENDIF                           QQRTC IFEQ '*'
     C**********           ENDIF                           HFLD IFEQ 'SCRC
     C**********           END
     C*
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*  F10 - SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C*  F10 - SEARCH - COMPANY
     C*
     C     HFLD          IFEQ      'SCRCO'
     C*
     C                   MOVE      *ZEROS        QQRCO
     C*
     C                   CALL      'W0301R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQRCO             3 0
     C                   PARM                    QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      '*'
     C     QQRCO         ANDNE     *ZEROS
     C                   MOVE      QQRCO         SCRCO
     C                   GOTO      SCR01T
     C                   ENDIF                                                  QQRTC=*
     C*
     C     QQRTC         IFEQ      'B'
     C     QQRCO         OREQ      *ZEROS
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON,QQRTC
     C*
     C                   ENDIF                                                  HFLD=SCRCO
     C*
     C                   ENDIF                                                  INKJ=ON
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C* F12=CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     Q2RECF        IFEQ      'SCR01'
     C                   GOTO      TERM
     C                   END
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*   PROCESS HELP REQUESTS
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *IN10         IFEQ      *ON
     C*** END   ADD *** 01/28/98 **************************************
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
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   ENDIF                                                  *IN11=ON
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  COMPANY NUMBER MUST BE GREATER THAN ZERO
     C*
     C     SCRCO         IFLE      *ZERO
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   MOVE      SCRCO         XXRCO
     C     RCKEY         CHAIN     PRC01                              4041
      **** BEGIN ADD *** 09/09/14 ************************************
     C                   IF        *IN41
      /Free
        Exfmt RcdInUse;
        Q2RECF = *Blanks;
      /End-free
     C                   GOTO      TERM
     C                   ENDIF
      **** END ADD   *** 09/09/14 ************************************
     C     *IN40         IFEQ      '0'
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *IN09         IFEQ      *ON
     C                   MOVE      MD(1)         SCMODE
     C*
     C                   ELSE
     C                   MOVE      MD(3)         SCMODE
     C                   ENDIF                                                  *IN09=ON
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C**********           MOVE UA,2      SCMODE
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C                   MOVE      RCNAME        SCNAME
     C                   MOVE      RCSHNM        SCSHNM
     C                   MOVE      RCADR1        SCADR1
     C                   MOVE      RCADR2        SCADR2
     C                   MOVE      RCCTY         SCCTY
     C                   MOVE      RCST          SCST
     C                   MOVEL     RCZIP         SCZIP1
     C                   MOVE      RCZIP         SCZIP2
     C*
     C                   ELSE                                                   *IN40=ON
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           MOVE UA,1      SCMODE
     C                   MOVE      MD(2)         SCMODE
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   MOVE      *BLANK        SCNAME
     C                   MOVE      *BLANK        SCSHNM
     C                   MOVE      *BLANK        SCADR1
     C                   MOVE      *BLANK        SCADR2
     C                   MOVE      *BLANK        SCCTY
     C                   MOVE      *BLANK        SCST
     C                   MOVE      *BLANK        SCZIP1
     C                   MOVE      *BLANK        SCZIP2
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  COMPANY NAME CANNOT BE BLANK
     C*
     C     SCNAME        CABEQ     *BLANK        STRT                     20
     C*
     C*  COMPANY SHORT NAME CANNOT BE BLANK
     C*
     C     SCSHNM        CABEQ     *BLANK        STRT                     21
     C*
     C*  ADDRESS 1 CANNOT BE BLANK
     C*
     C     SCADR1        CABEQ     *BLANK        STRT                     22
     C*
     C*  CITY CANNOT BE BLANK
     C*
     C     SCCTY         CABEQ     *BLANK        STRT                     23
     C*
     C*  STATE MUST BE VALID IF ENTERED
     C*
     C     SCST          IFNE      *BLANKS
     C                   Z-ADD     1             IN                5 0
     C     SCST          LOOKUP    ST(IN)                                 50
     C     *IN50         IFEQ      '0'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ZIP CODE CANNOT BE LESS THAN 0
     C*
     C     SCZIP1        IFLT      *ZERO
     C                   MOVE      '1'           *IN25
     C                   GOTO      STRT
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, UPDATE THE RECORD, GOTO THE
     C*  FIRST SCREEN
     C*
     C                   Z-ADD     UPCO#         RCCO#
     C                   Z-ADD     SCRCO         RCRCO
     C                   MOVE      SCNAME        RCNAME
     C                   MOVE      SCSHNM        RCSHNM
     C                   MOVE      SCADR1        RCADR1
     C                   MOVE      SCADR2        RCADR2
     C                   MOVE      SCCTY         RCCTY
     C                   MOVE      SCST          RCST
     C                   MOVEL     SCZIP1        RCZIP
     C                   MOVE      SCZIP2        RCZIP
     C                   MOVE      *BLANK        RCSTAT
     C                   MOVE      Q1USER        RCUSER
     C                   MOVE      Q1PGM         RCPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RCDT
     C*
     C                   Z-ADD     DSTIME        RCTIME
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRC01
     C                   ELSE
     C                   WRITE     PRC01
     C                   END
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCRCO
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C********** **  TABLE UA
     C********** -- ADD --
     C********** - UPDATE -
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*   ADDED TABLE MD FOR SCMODE
     C*** END   ADD *** 01/28/98 **************************************
     C*
**  TABLE MD
- Display-
-- Add --
- Change -
- Delete -
**  TABLE ST
ALAKAZARCACOCTDEDCFLGAHIIDILINIAKSKYLAMEMDMAMIMNMSWY
MOMTNENVNHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIPR
