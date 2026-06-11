     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0350R - REINSURANCE RECOVERY RECEIVED MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  08/03/92 JJJ  PROJECT: WC157                                 *
     H*                LOAD MULT CLM# TO REQUEST FILE                 *
     H*                                                               *
     H*  07/07/92 JEI  PROJECT: WC157                                 *
     H*                CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR      *
     H*                'BASE' FILES (XXCASE) - CASE NUMBER REMAINS    *
     H*                14 ALPHA FOR REINSURANCE FILES (X@CASE)        *
     H*                                                               *
     H*  04/03/92 LPK  PROJECT: MD027                                 *
     H*                CHANGED CASE NUMBER FROM 7 NUMERIC TO 14       *
     H*                ALPHA FIELD.                                   *
     H*                CHANGED CODE TO USE EMP/DIV ON PROMPT SCREEN   *
     H*                AND POLICY EDIT.                               *
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
     FW0350F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDPFL2    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*** BEGIN ADD *** 01/28/98 **************************************
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 01/28/98 **************************************
     FWMUPL     IF   E           K DISK
     FWMRCL     IF   E           K DISK
     FWMRQL     UF A E           K DISK
     FWMRQL1    UF A E           K DISK
     F                                     RENAME(PRQ01:PRQ02)
     FWDRVL     O    E           K DISK
     FWMRIL1    IF   E           K DISK
     FWMEML     IF   E           K DISK
     F*** BEGIN ADD *** 01/28/98 **************************************
     F                                     RENAME(PEM01:PEM02)
     F                                     INFDS(DBFDS2)
     FWMEMP     IF   E             DISK
     F*** END   ADD *** 01/28/98 **************************************

     D*
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
     D*** END   ADD *** 01/28/98 ***********************************************
     D*
     D*
     D*** BEGIN ADD *** 01/28/98 **************************************
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D DBFDS2          DS
     D  DBRRN2               397    400B 0
     D*** END   ADD *** 01/28/98 **************************************
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
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Reinsurance Recovery-
     D                                      Received Maint.')

     I*** END   ADD *** 01/28/98 ***********************************************
     I*

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  REINSURANCE COMPANY FILE
     C*
     C     RCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    XXRCO             3 0
     C*
     C*  REINSURANCE RECOVERY REQUEST  FILE
     C*
     C     RQKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    SCASFL
     C                   KFLD                    X@CASE
     C                   KFLD                    XXREQ#
     C*
     C*  REFUND REQUEST  FILE  PARTIAL KEY
     C*
     C     RQKEY1        KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    SCASFL
     C                   KFLD                    X@CASE
     C*
     C     RQKY3         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    SCASFL
     C                   KFLD                    X@CASE
     C                   KFLD                    XXREQ#
     C*
     C     RQKY4         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    SCASFL
     C*
     C*  PARTIAL FILING FILE
     C*
     C     PFKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMO FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*
     C*   KEY LIST FOR REINSURANCE POLICY FILE
     C*
     C     RIKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SCASFL
     C                   KFLD                    XXEFDT
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    RQCASE        X@CASE
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDT          XXEFDT
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *LIKE         DEFINE    EMEMP#        Q@EMP#
     C     *LIKE         DEFINE    EMDIV         Q@DIV
     C     *LIKE         DEFINE    SCEFDT        Q@EFDT
     C*** END   ADD *** 01/28/98 **************************************
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
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   Z-ADD     UPFYR         SCFYR
     C                   MOVEL     FNNAME        SCNAME
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
     C                   MOVEL(P)  ##DATA        QQPRDS
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *IN09         IFEQ      *ON
     C                   MOVE      MD(1)         SCMODE                         DISPLAY MODE
     C                   ENDIF                                                  *IN09=ON
     C*** END   ADD *** 01/28/98 **************************************
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
     C                   Z-ADD     *ZERO         XXREQ#            5 0
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   EXFMT     SCR01
     C                   END
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   EXFMT     SCR02
     C                   END
     C*
     C                   MOVE      '0'           *IN30
     C*
     C*   PROCESS ALL COMMAND KEYS
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
     C* F12=CANCEL
     C*
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C*
     C**********
     C**F5*PARTIAL FILE SEARCH
     C**********
     C********** *INKE     IFEQ '1'
     C**********           Z-ADDSCYR      XXFYR
     C**********           Z-ADDUPFND     XXFND   30
     C**********           Z-ADD0         QQRNDT  80
     C**********           Z-ADD0         QQMCLM  70
     C**********           CALL 'W0331R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXFYR
     C**********           PARM           QQRNDT
     C**********           PARM           SCCASE
     C**********           PARM           SCMCLM
     C**********           PARM           XXSEQ   50
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE XXFYR     SCYR
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCYR      OREQ *ZERO
     C**********           GOTO STRT
     C**********           END
     C**********
     C********** QQRTC     CABEQ'*'       SCR01T
     C**********           END
     C*
     C**********
     C***REINSURANCE COMPANY FILE SEARCH
     C**********
     C********** Q2RECF    IFEQ 'SCR02'
     C**********           CALL 'W0301R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           SCRCO
     C**********           PARM           QQRTC   1
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** SCRCO     OREQ *ZERO
     C**********           MOVE '1'       *IN27
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           MOVE '1'       *IN28
     C********** QQRTC     CABEQ'*'       SCR01T
     C**********           END
     C*
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*  SCREEN 1
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F10 = SEARCHES
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C**      ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*
     C     HFLD          COMP      'SCASFL'                               85
     C*
     C*  F10 = SEARCH - PARTIAL FILE SEARCH
     C*
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'W0331R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      SCFYR         QQFYR             3 0
     C                   PARM      *ZEROS        QQRNDT            8 0
     C                   PARM      *ZEROS        QQCASE            7 0
     C                   PARM      *ZEROS        QQMCLM            7 0
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      '*'
     C     QQFYR         ANDNE     *ZEROS
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQMCLM        SCMCLM
     C                   ENDIF                                                  QQRTC=*
     C*
     C     QQRTC         IFEQ      'B'
     C     QQFYR         OREQ      *ZEROS
     C                   MOVE      *ON           *IN82                          SCCASE POSITION
     C                   ENDIF                                                  QQRTC=B
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C*  F10 = SEARCH - EMPLOYER NUMBER OR DIVISION
     C*
     C     HFLD          IFEQ      'SCEMP#'
     C     HFLD          OREQ      'SCDIV'
     C*
     C                   CALL      'W0050R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '2'           @@CALL            1
     C                   PARM      *ZEROS        @@RRN             9 0
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
     C                   Z-ADD     EMFND         SCFND
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C                   MOVE      *ON           *IN84                          SCEFDT POSITION
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKL=OFF,@@RRN
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C     *IN60         OREQ      *ON
     C                   MOVE      *ON           *IN83                          SCEMP# POSITION
     C                   ENDIF                                                  *INKL=ON,@@RRN
     C*
     C                   ENDIF                                                  HFLD=SCEMP#
     C*
     C*  F11 = SEARCH - EFFECTIVE DATE
     C*
     C     HFLD          IFEQ      'SCEFDT'
     C*
     C                   CALL      'W0311R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      SCEMP#        Q@EMP#
     C                   PARM      SCDIV         Q@DIV
     C                   PARM      SCEFDT        Q@EFDT
     C                   PARM      SCASFL        Q@ASFL            1
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      '*'
     C     Q@EFDT        ANDNE     *ZEROS
     C                   Z-ADD     Q@EMP#        SCEMP#
     C                   Z-ADD     Q@DIV         SCDIV
     C                   Z-ADD     Q@EFDT        SCEFDT
     C                   MOVE      Q@ASFL        SCASFL
     C***                  MOVE *ON       *IN80            SCASFL POSITION
     C                   ENDIF                                                  QQRTC=*        F
     C*
     C     QQRTC         IFEQ      'B'
     C     Q@EFDT        OREQ      *ZEROS
     C                   MOVE      *ON           *IN84                          SCEFDT POSITION
     C                   ENDIF                                                  QQRTC=B        F
     C*
     C                   ENDIF                                                  HFLD=SCEFDT
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  *INKJ=ON
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*  SCREEN 2
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*  F10 - SEARCH - COMPANY
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C     HFLD          IFEQ      'SCRCO'
     C*
     C                   CALL      'W0301R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      *ZEROS        QQRCO             3 0
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     QQRTC         ANDEQ     '*'
     C     QQRCO         ANDNE     *ZEROS
     C                   MOVE      QQRCO         SCRCO
     C                   MOVE      *ON           *IN28                          SCCHK# POSITION
     C                   ENDIF                                                  *INKL=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     QQRTC         OREQ      'B'
     C     QQRCO         OREQ      *ZEROS
     C                   MOVE      *ON           *IN27                          SCRCO POSITION
     C                   ENDIF                                                  *INKL=ON,QQRTC
     C*
     C                   ENDIF                                                  HFLD=SCRCO
     C*
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  INKJ=ON
     C*
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   END
     C*
     C* F7=PMT SEARCH
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  VALIDATE THE CLAIM # (CO/FND/YEAR/CASE) IF SPECIFIC
     C*
     C     SCASFL        IFEQ      'S'
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C     PFKEY         CHAIN     PPF01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C                   CALL      'W0351R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    XXFYR
     C                   PARM                    SCASFL
     C                   PARM                    SCCASE
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C                   END
     C*
     C*  VALIDATE THE POLICY IF AGGREGATE
     C*
     C     SCASFL        IFEQ      'A'
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     RIKEY         CHAIN     PRI01                              25
     C     *IN25         CABEQ     '1'           STRT
     C*
     C                   CALL      'W0352R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCASFL
     C                   PARM                    SCEMP#
     C                   PARM                    SCDIV
     C                   PARM                    SCEFDT
     C                   PARM                    XXREQ#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   END
     C*
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
     C                   MOVE      *BLANK        SCCONM
     C                   MOVEL     *BLANK        SCFND
     C                   Z-ADD     *ZERO         SCFYR
     C                   MOVEL     *BLANK        SCNAME
     C*
     C                   ELSE
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           Z-ADDUPFYR     SCFYR
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     UPFYR         IFNE      *ZEROS
     C                   Z-ADD     UPFYR         SCFYR
     C                   ENDIF                                                  UPFYR<>0
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
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
     C*  AGGREGATE SPECIFIC FLAG MUST BE 'A' OR 'S'
     C*
     C     SCASFL        IFNE      'A'
     C     SCASFL        ANDNE     'S'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      *BLANKS       SCENAM
     C                   MOVE      *BLANKS       SCCLNM
     C                   Z-ADD     0             SCMCLM
     C                   MOVE      '0'           *IN78
     C                   MOVE      '0'           *IN79
     C*
     C     SCASFL        IFEQ      'S'
     C*
     C*  FUND YR & CASE MUST EXIST IN THE PARTIAL FILING FILE
     C*  IF SCASFL = S
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C     PFKEY         CHAIN     PPF01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C                   Z-ADD     PFEMP#        XXEMP#
     C                   Z-ADD     PFDIV         XXDIV
     C                   Z-ADD     PFEFDT        XXEFDT
     C                   Z-ADD     PFMCLM        SCMCLM
     C                   MOVE      '0'           *IN78
     C     SCMCLM        IFNE      *ZEROS
     C                   MOVE      '1'           *IN79
     C                   END
     C*
     C*  LOAD CLAIMANT NAME FOR NEXT SCREEN  IF SPECIFIC SELECTED
     C*
     C                   MOVE      SCCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              60
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCLNM
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN78
     C*
     C*  VALIDATE EFFECTIVE DATE
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCEFDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C*  THIS RECORD MUST EXIST IN THE POLICY FILE
     C*
     C                   Z-ADD     SCEMP#        XXEMP#
     C                   Z-ADD     SCDIV         XXDIV
     C*
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     RIKEY         CHAIN     PRI01                              25
     C     *IN25         CABEQ     '1'           STRT
     C                   Z-ADD     0             XXFYR
     C                   MOVE      *ZEROS        X@CASE
     C*
     C*   LOAD EMPLOYER NAME TO SCR02
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C********** EMKY      CHAINPEM01                60
     C     EMKY          CHAIN     PEM02                              60
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        SCENAM
     C                   END
     C*
     C                   END
     C*
     C*  SET FLAG FOR XXASFL VALUE
     C*
     C     SCASFL        COMP      'S'                                    12
     C*
     C*  GET THE REQUEST INQUIRY IF THERE ARE REQUESTS
     C*
     C                   MOVE      'Y'           XXACTV            1
     C                   MOVE      *BLANKS       QQRTC
     C                   Z-ADD     0             SCREQ#
     C*
     C*  SPECIFIC
     C*
     C     SCASFL        IFEQ      'S'
     C*
     C     RQKEY1        CHAIN     PRQ01                              60
     C     *IN60         IFEQ      '0'
     C                   CALL      'W0341R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFYR
     C                   PARM                    SCASFL
     C                   PARM                    SCCASE
     C                   PARM                    SCREQ#
     C                   PARM                    QQRTC
     C                   END
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     SCREQ#        IFEQ      *ZERO
     C*
     C*   FIND NEXT LOGICAL ENTRY FILED
     C*
     C     RQKEY1        SETGT     PRQ01
     C                   READP     PRQ01                                  60
     C     *IN60         IFEQ      '0'
     C     UPCO#         ANDEQ     RQCO#
     C     SCFND         ANDEQ     RQFND
     C     SCFYR         ANDEQ     RQFYR
     C     X@CASE        ANDEQ     RQCASE
     C     RQREQ#        ADD       1             SCREQ#
     C                   EXCEPT    PRQ01E
     C                   ELSE
     C                   Z-ADD     1             SCREQ#
     C                   MOVE      '1'           *IN60
     C                   END
     C*
     C                   Z-ADD     0             SCRCO
     C*
     C                   ELSE
     C*
     C     QQRTC         IFEQ      '*'
     C                   Z-ADD     SCREQ#        XXREQ#
     C     RQKEY         CHAIN     PRQ01                              60
     C                   Z-ADD     RQRCO         SCRCO
     C     RQSTAT        IFEQ      'I'
     C                   MOVE      'N'           XXACTV
     C                   END
     C                   END
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C     RQKY4         CHAIN     PRQ02                              60
     C     *IN60         IFEQ      '0'
     C                   CALL      'W0342R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCASFL
     C                   PARM                    SCEMP#
     C                   PARM                    SCDIV
     C                   PARM                    SCEFDT
     C                   PARM                    SCREQ#
     C                   PARM                    QQRTC
     C                   END
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      'B'
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
     C     SCREQ#        IFEQ      *ZERO
     C*
     C*   FIND NEXT LOGICAL ENTRY FILED
     C*
     C     RQKY4         SETGT     PRQ02
     C                   READP     PRQ02                                  60
     C     *IN60         IFEQ      '0'
     C     UPCO#         ANDEQ     RQCO#
     C     SCFND         ANDEQ     RQFND
     C     XXEMP#        ANDEQ     RQEMP#
     C     XXDIV         ANDEQ     RQDIV
     C     XXEFDT        ANDEQ     RQEFDT
     C     RQREQ#        ADD       1             SCREQ#
     C                   EXCEPT    PRQ02E
     C                   ELSE
     C                   Z-ADD     1             SCREQ#
     C                   MOVE      '1'           *IN60
     C                   END
     C*
     C                   Z-ADD     0             SCRCO
     C*
     C                   ELSE
     C*
     C     QQRTC         IFEQ      '*'
     C                   Z-ADD     SCREQ#        XXREQ#
     C                   MOVE      *ZEROS        X@CASE
     C     RQKY3         CHAIN     PRQ02                              60
     C                   Z-ADD     RQRCO         SCRCO
     C     RQSTAT        IFEQ      'I'
     C                   MOVE      'N'           XXACTV
     C                   END
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     *ZERO         RQRAMT
     C                   Z-ADD     0             RQDTRQ
     C                   Z-ADD     *ZERO         RQRCVD
     C                   Z-ADD     DSDATE        RQDTRC
     C                   MOVE      *BLANK        RQCOM1
     C                   MOVE      *BLANK        RQCOM2
     C                   END
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           Z-ADDUPPRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCPODT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   Z-ADD     *ZERO         SCAMT
     C                   MOVE      *BLANKS       SCCHK#
     C                   MOVE      *BLANKS       SCDESC
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   MOVE      MD(2)         SCMODE
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*  IF IN INQUIRY MODE, SKIP EDITS
     C*
     C     *IN09         CABEQ     '1'           INQ2
     C*
     C*  SEND ERROR IF STATUS ON REQUEST IS INACTIVE
     C*
     C     XXACTV        IFEQ      'N'
     C                   MOVE      '1'           *IN29
     C                   GOTO      STRT
     C                   END
     C*
     C*  EDIT POSTING DATE.
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCPODT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*   VALIDATE REINSURANCE CO#
     C*
     C                   Z-ADD     SCRCO         XXRCO
     C     RCKEY         CHAIN     PRC01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*   CHECK# CANNOT BE LESS THAN OR EQUAL TO 0
     C*
     C     SCCHK#        IFLE      0
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*   AMOUNT RECOVERED CANNOT BE EQUAL TO 0
     C*
     C     SCAMT         CABEQ     *ZERO         STRT                     23
     C*
     C*  ALL EDITS HAVE BEEN PASSED, WRITE THE REFUND PAYMENT
     C*
     C                   Z-ADD     SCREQ#        XXREQ#
     C   12RQKEY         CHAIN     PRQ01                              58
     C  N12RQKY3         CHAIN     PRQ02                              58
     C*
     C                   Z-ADD     UPCO#         RVCO#
     C                   Z-ADD     SCFND         RVFND
     C                   MOVE      SCASFL        RVASFL
     C                   Z-ADD     XXFYR         RVFYR
     C                   MOVE      *ZEROS        RVCASE
     C                   MOVE      X@CASE        RVCASE
     C                   Z-ADD     XXEMP#        RVEMP#
     C                   Z-ADD     XXDIV         RVDIV
     C                   Z-ADD     XXEFDT        RVEFDT
     C                   Z-ADD     SCREQ#        RVREQ#
     C                   Z-ADD     SCRCO         RVRCO
     C                   Z-ADD     SCAMT         RVAMT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RVTRDT
     C*
     C                   Z-ADD     SCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RVPODT
     C*
     C                   Z-ADD     SCCHK#        RVCHK#
     C                   MOVEL     SCDESC        RVDESC
     C                   MOVE      *BLANK        RVSTAT
     C                   MOVE      Q1USER        RVUSER
     C                   MOVE      Q1PGM         RVPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RVDT
     C*
     C                   Z-ADD     DSTIME        RVTIME
     C                   MOVE      SCMCLM        RVMCLM
     C*
     C                   WRITE     PRV01
     C*
     C*   WRITE THE REFUND REQUEST
     C*
     C                   Z-ADD     UPCO#         RQCO#
     C                   Z-ADD     SCFND         RQFND
     C                   Z-ADD     XXFYR         RQFYR
     C                   MOVE      SCASFL        RQASFL
     C                   MOVE      *ZEROS        RQCASE
     C                   MOVE      X@CASE        RQCASE
     C                   Z-ADD     SCREQ#        RQREQ#
     C                   Z-ADD     SCRCO         RQRCO
     C  N58              ADD       SCAMT         RQRCVD
     C   58              Z-ADD     0             RQRAMT
     C   58              Z-ADD     SCAMT         RQRCVD
     C   58              Z-ADD     0             RQDTRQ
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RQDTRC
     C*
     C                   MOVE      Q1USER        RQUSER
     C                   MOVE      Q1PGM         RQPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RQDT
     C*
     C                   Z-ADD     DSTIME        RQTIME
     C                   Z-ADD     XXEMP#        RQEMP#
     C                   Z-ADD     XXDIV         RQDIV
     C                   Z-ADD     XXEFDT        RQEFDT
     C                   MOVE      SCMCLM        RQMCLM
     C*
     C     *IN58         IFEQ      '1'
     C                   MOVE      'A'           RQSTAT
     C   12              WRITE     PRQ01
     C  N12              WRITE     PRQ02
     C                   ELSE
     C   12              UPDATE    PRQ01
     C  N12              UPDATE    PRQ02
     C                   END
     C*
     C     INQ2          TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCFYR
     C                   MOVE      *BLANKS       SCASFL
     C                   MOVE      *BLANKS       SCCASE
     C                   MOVE      *BLANKS       SCEMP#
     C                   MOVE      *BLANKS       SCDIV
     C                   MOVE      *BLANKS       SCEFDT
     C                   MOVE      *BLANKS       SCREQ#
     C                   MOVE      *BLANKS       SCRCO
     C                   MOVE      *BLANKS       SCPODT
     C                   MOVE      *BLANKS       SCCHK#
     C                   Z-ADD     *ZERO         SCAMT
     C                   MOVE      *BLANKS       SCDESC
     C                   GOTO      STRT
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPRQ01     E            PRQ01E
     OPRQ02     E            PRQ02E
     O*
     O*** BEGIN ADD *** 01/28/98 **************************************
     O*   ADDED TABLE MD FOR SCMODE
     O*** END   ADD *** 01/28/98 **************************************
     O*
**  TABLE MD
- Display-
-- Add --
- Change -
- Delete -
