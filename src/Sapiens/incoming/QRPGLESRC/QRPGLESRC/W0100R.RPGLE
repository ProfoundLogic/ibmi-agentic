     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0100R - REQUEST REFUND MAINTENANCE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  11/02/09 C:232 PROJECT: 139WMR                               *
     H*                 OFAC Reporting-Add Pending Status Indicator.  *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                                                               *
     H*  07/06/04 C:53  Support: WC099 43085                          *
     H*                 Correct Date issue when searching.            *
     H*                                                               *
     H*  11/07/03 C:53  Project: WC1163                               *
     H*                 Added ability to go directly to maintain a    *
     H*                 when being called from W0600R via W0070R      *
     H*                                                               *
     H*  08/16/99 C:118 PROJECT: WC577                                *
     H*                 Correct program name display in display file. *
     H*                                                               *
     H*  01/23/98 C:85 PROJECT: WC577                                 *
     H*                ADDED CAPABILITY FOR PROGRAM TO BE CALLED FROM *
     H*                THE WORK WITH CLAIMS SCREEN, (W0070R)          *
     H*                OPTION 54, RECOVERY REQUEST.                   *
     H*                ALSO VERSION 3 STANDARDS.                      *
     H*                                                               *
     H* 02/03/94  C:25  - PROJ: WC037                                 *
     H*                   1. ADDED F9 CLAIM NUMBER SEARCH TO SCR01    *
     H*                                                               *
     H* 04/15/97  C:30  Project: WC607                                *
     H*                 Change F6 PAYEE SRCH program from W0531R to   *
     H*                 W0030R on SCR04 and SCR05.                    *
     H*                                                               *
     H*****************************************************************
     FW0100F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(DBFDSC)
     FWMCDL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWMPDL     IF   E           K DISK
     FWMRRL     UF A E           K DISK
     FWDPAP     IF   E             DISK
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PPA01)

     D UA              S             10    DIM(2) CTDATA PERRCD(1)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D DBFDSC          DS
     D  DBRRNC               397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*** BEGIN ADD *** 01/23/98 ***********************************************
     D OPTNM           C                   CONST('Recovery Request Mai-
     D                                     ntenance')

     I*** END   ADD *** 01/23/98 ***********************************************
     I*

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C*
     C*** BEGIN CHG *** 01/23/98 ***********************************************
     C**********           KFLD           UPCO#
     C                   KFLD                    XXCO#             3 0
     C*** END   CHG *** 01/23/98 ***********************************************
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C*
     C*** BEGIN CHG *** 01/23/98 ***********************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPFND
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*** END   CHG *** 01/23/98 ***********************************************
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  REFUND REQUEST  FILE
     C*
     C     RRKEY         KLIST
     C*
     C*** BEGIN CHG *** 01/23/98 ***********************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPFND
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END   CHG *** 01/23/98 ***********************************************
     C*
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C                   KFLD                    XXREQ#
     C*
     C*  REFUND REQUEST FILE PARTIAL KEY
     C*
     C     RRKY2         KLIST
     C*
     C*** BEGIN CHG *** 01/23/98 ***********************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPFND
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END   CHG *** 01/23/98 ***********************************************
     C*
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*
     C*  WMPDL PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C*
     C*** BEGIN CHG *** 01/23/98 ***********************************************
     C**********           KFLD           UPCO#
     C**********           KFLD           UPFND
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END   CHG *** 01/23/98 ***********************************************
     C*
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMANT DEMO FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR TABLE FILE
     C*
     C     TBKEY         KLIST
     C*
     C*** BEGIN CHG *** 01/23/98 ***********************************************
     C**********           KFLD           UPCO#
     C                   KFLD                    XXCO#
     C*** END   CHG *** 01/23/98 ***********************************************
     C*
     C                   KFLD                    XXKEY            10
     C*
     C     *LIKE         DEFINE    CMEMP#        SCEMP#
     C     *LIKE         DEFINE    CMDIV         SCDIV
     C     *LIKE         DEFINE    CMSS#         SCSS#
     C     *LIKE         DEFINE    PASEQ#        XXSEQ
     C*
     C     *ENTRY        PLIST
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C**********           PARM           QQPRDS 40
     C**********           PARM           QQCALL  1
     C*
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    QQCALL            1
     C                   PARM                    @@RRN             9 0
     C*** BEG ADD *** 11/07/03 **************************************
     C                   PARM                    @@REQ#            5 0
     C*** END ADD *** 11/07/03 **************************************
     C*
     C*** END   CHG *** 01/23/98 **************************************
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
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C*
     C     @@RRN         IFNE      *ZEROS
     C                   MOVE      *ON           *IN54                          OPTION 54
     C                   ENDIF                                                  @@RRN<>0
     C*
     C* IF CALLED FROM WORK WITH CLAIMS OPTION 54
     C*
     C     *IN54         IFEQ      *ON
     C*
     C     @@RRN         CHAIN     PCM02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CMCO#         XXCO#
     C                   MOVE      CMFND         XXFND
     C                   MOVE      CMFND         SCFND
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C                   Z-ADD     0             @@RRN
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *IN54=ON
     C*
     C     *IN54         IFEQ      *OFF
     C*
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C                   ENDIF                                                  *IN60=OFF
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C                   ENDIF                                                  *IN54=OFF
     C*** END   ADD *** 01/23/98 **************************************
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
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        SCNAME
     C*
     C*** BEGIN DEL *** 01/23/98 **************************************
     C**********           MOVELUPFND     SCFND
     C**********           Z-ADDUPFYR     SCYR
     C*** END   DEL *** 01/23/98 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      Q1USER        SCUSER
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
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
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C*** BEG ADD *** 11/07/03 **************************************
     C     Q1PRMS        IFGT      4
     C     @@REQ#        ANDNE     *ZEROS
     C                   Z-ADD     @@REQ#        XXREQ#
     C                   GOTO      SCR01T
     C                   ENDIF
     C*** END ADD *** 11/07/03 **************************************
     C*
     C     QQCALL        COMP      '1'                                    09
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C     *IN54         IFEQ      *ON
     C                   GOTO      OPT54
     C                   ELSE
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C                   ENDIF                                                  *IN54=ON
     C*** END   ADD *** 01/23/98 **************************************
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
     C* ==================================================================
     C*   F3 = EXIT
     C* ==================================================================
     C     *INKC         IFEQ      '1'
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C* ==================================================================
     C*   F12 = CANCEL
     C* ==================================================================
     C     *INKL         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C     *IN54         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF                                                  *IN54=ON
     C*
     C     *IN54         IFEQ      *OFF
     C*
     C*** END   ADD *** 01/23/98 **************************************
     C                   MOVEL     'SCR01'       Q2RECF
     C*** BEGIN ADD *** 01/23/98 **************************************
     C                   ENDIF                                                  *IN54=OFF
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C                   END
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C* ==================================================================
     C*   F13 = CHANGE USER PARAMETERS
     C* ==================================================================
     C     *INKM         IFEQ      *ON
     C*
     C                   MOVE      *ZEROS        SCFND
     C                   MOVE      *ZEROS        SCCASE
     C*
     C                   CALL      'W0006R'
     C*
     C*  GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   MOVE      UPFND         SCFND
     C                   MOVE      UPFYR         SCFYR
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*  GET THE COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C                   ENDIF                                                  *IN60=OFF
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     FNNAME        SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKM=ON
     C*
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C*** BEGIN DEL *** 01/23/98 **************************************
     C*
     C**==================================================================
     C****F1 = RESTART
     C**==================================================================
     C**********
     C********** *INKA     IFEQ '1'
     C********** Q2RECF    IFEQ 'SCR02'
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           MOVE *ZERO     SCCASE
     C**********           Z-ADDUPFYR     SCYR
     C**********           Z-ADDUPFYR     SCFYR
     C**********           END
     C**********
     C**********           GOTO STRT
     C**********           END
     C*
     C*** END   DEL *** 01/23/98 **************************************
     C*
     C* ==================================================================
     C*   F5 = SEARCH PAYEE
     C* ==================================================================
     C*
     C     *INKE         IFEQ      '1'
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C**********           Z-ADDSCYR      XXFYR
     C                   Z-ADD     SCFYR         XXFYR
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C                   MOVE      SCCASE        XXCASE
     C*
     C*  VALIDATE THE CLAIM # (FND/YEAR/CASE)
     C*
     C     CMKEY         CHAIN     PCM01                              22
     C     *IN22         CABEQ     '1'           STRT
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C                   CALL      'W0620R'
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM      '1'           @@CALL            1
     C                   PARM      DBRRNC        @@RRN             9 0
     C                   PARM      'A'           @@TYPE            1
     C*
     C     @@RRN         CHAIN     WDPAP                              60
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     *INKL         IFEQ      *ON
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C********** SCYR      OREQ *ZERO
     C     SCFYR         OREQ      *ZERO
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C     SCCASE        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C                   Z-ADD     PASEQ#        XXSEQ
     C*
     C                   GOTO      SCR01T
     C*
     C                   END
     C*
     C                   GOTO      STRT
     C                   END
     C*
     C* ==================================================================
     C*   F7 = REQUEST REFUND SEARCH
     C* ==================================================================
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  VALIDATE THE CLAIM # (CO/FND/YEAR/CASE)
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C**********           Z-ADDSCYR      XXFYR
     C                   Z-ADD     SCFYR         XXFYR
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C                   MOVE      SCCASE        XXCASE
     C*
     C     CMKEY         CHAIN     PCM01                              22
     C     *IN22         CABEQ     '1'           STRT
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C                   MOVEL(P)  OPTNM         QQPRDS           40
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C                   CALL      'W0600R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C**********           PARM           SCYR
     C                   PARM                    SCFYR
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C                   PARM                    SCCASE
     C                   PARM                    XXREQ#
     C*** BEG CHG *** 11/07/03 **************************************
     C***********          PARM           QQRTC   1
     C                   PARM      'X'           QQRTC             1
     C*** END CHG *** 11/07/03 **************************************
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C********** SCYR      OREQ *ZERO
     C     SCFYR         OREQ      *ZERO
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C     SCCASE        OREQ      *ZERO
     C                   GOTO      STRT
     C                   ENDIF                                                  QQRTC
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   ENDIF                                                  Q2RECF
     C*
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKG
     C*
     C* ==================================================================
     C*   F10= SEARCH
     C* ==================================================================
     C*
     C*** BEGIN DEL *** 01/23/98 **************************************
     C*
     C********** *INKJ     IFEQ '1'
     C*
     C********** HFLD      IFEQ 'SCCASE'
     C********** HFLD      OREQ 'SCYR'
     C*
     C**********           MOVE UPCO#     ##CO#   3
     C**********           MOVE SCFND     ##FND   3
     C*
     C**********           CALL 'W0070R'
     C**********           PARM '2'       @@CALL  1
     C********** *INKC     PARM           @@KC    1
     C********** *INKL     PARM           @@KL    1
     C**********           PARM           @@MODE  1
     C**********           PARM           @@RRN   90
     C**********           PARM 'CN'      @@SORT  2
     C**********           PARM DBRRN     @@RRN2  90
     C**********           PARM SCYR      @@FYR   30
     C*
     C********** *INKC     CABEQ*ON       TERM
     C*
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C*
     C********** *INKL     IFEQ *ON
     C**********           GOTO STRT
     C**********           ELSE
     C*
     C********** @@RRN     CHAINWMCMP                60
     C*
     C**********           MOVE CMCASE    SCCASE
     C**********           GOTO STRT
     C*
     C**********           ENDIF                           *INKL
     C**********           END
     C*
     C**********           GOTO STRT
     C**********           END
     C*
     C* ==================================================================
     C*  CMD 10 - TABLE FILE SEARCH - NCCI CODE
     C* ==================================================================
     C*
     C********** *INKJ     IFEQ '1'
      *
      *** BEGIN CHG *** 04/15/97 *****************************************
     C***********Q2RECF****IFEQ 'SCR02'
     C*********************MOVELSCSTCD    XX2     2
     C***********XX2*******IFEQ '? '
      *
     C**********           SELEC
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C********** HFLD      WHEQ 'SCSTCD'
      *
      *** BEGIN CHG *** 04/15/97 *****************************************
     C*
     C**********           MOVE *BLANK    QQKEY  10
     C**********           CALL 'W2211R'
     C**********           PARM '2'       @@CALL  1
     C********** *INKC     PARM           QQKC    1
     C********** *INKL     PARM           QQKL    1
     C**********           PARM           QQKEY
     C*
     C**********           MOVELQQKEY     SCSTCD
     C********** *INKC     CABEQ*ON       TERM
     C*
     C********** *INKL     IFNE *ON
     C********** QQKEY     ANDNE*BLANKS
     C*
     C**********           MOVE '1'       *IN50
     C**********           END
     C*
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           GOTO STRT
      *
      *** BEGIN DEL *** 04/15/97 *****************************************
     C*********************END
     C*********************END
      *** END   DEL *** 04/15/97 *****************************************
      *** BEGIN CHG *** 04/15/97 ****************************************
     C*********************MOVELSCFEIN    XX2     2
     C***********XX2*******IFEQ '? '
     C*********************MOVE *ZEROS    QQFEIN  90
     C*********************MOVE *ZEROS    QQMOD   30
     C*********************CALL 'W0531R'
     C*********************PARM           QQPRDS
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C********** HFLD      WHEQ 'SCFEIN'
      *
      *    PAYEE SEARCH
      *
     C**********           MOVE *ZEROS    QQFEIN  9
     C**********           MOVE *ZEROS    QQMOD   3
     C**********           MOVE '2'       QQCALL
      *
     C**********           CALL 'W0030R'
      *** END   CHG *** 04/15/97 ****************************************
     C**********           PARM           QQCALL
     C**********           PARM           QQFEIN
     C**********           PARM           QQMOD
     C**********           PARM           QQRTC   1
     C**********           MOVELQQFEIN    SCFEIN
     C**********           MOVELQQMOD     SCMOD
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ '*'
     C**********           MOVE '1'       *IN48
     C**********           END
     C*
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           GOTO STRT
      *
      *** BEGIN CHG *** 04/15/97 ****************************************
     C*********************MOVELSCATTY    XX2     2
     C***********XX2*******IFEQ '? '
     C*********************MOVE *ZEROS    QQFEIN  90
     C*********************MOVE *ZEROS    QQMOD   30
     C*********************CALL 'W0531R'
     C*********************PARM           QQPRDS
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C********** HFLD      WHEQ 'SCATTY'
      *
      *    ATTORNEY SEARCH
      *
     C**********           MOVE *ZEROS    QQFEIN  9
     C**********           MOVE *ZEROS    QQMOD   3
     C**********           MOVE '2'       QQCALL
      *
     C**********           CALL 'W0030R'
      *** END   CHG *** 04/15/97 ****************************************
     C**********           PARM           QQCALL
     C**********           PARM           QQFEIN
     C**********           PARM           QQMOD
     C**********           PARM           QQRTC   1
      *
     C**********           MOVELQQFEIN    SCATTY
     C**********           MOVELQQMOD     SCAMOD
     C********** QQRTC     CABEQ'A'       TERM
     C*
     C********** QQRTC     IFEQ '*'
     C**********           MOVE '1'       *IN49
     C**********           END
     C*
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           GOTO STRT
      *** BEGIN ADD *** 04/15/97 *****************************************
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C**********           OTHER                           FIELD NO SEARCH
     C**********           MOVE *ON       *IN47
      *
     C**********           ENDSL
      *** END   ADD *** 04/15/97 *****************************************
      *
     C**********           GOTO STRT
     C**********           END
     C*
     C*** END   DEL *** 01/23/98 **************************************
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C*
     C*  F10 - SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C*  SCREEN ONE
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C*  F10 - SEARCH - POLICY YEAR OR CASE
     C*
     C     HFLD          IFEQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   MOVE      *ZEROS        @@RRN
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM      'NM'          @@SORT            2
     C                   PARM      DBRRN         @@RRN2            9 0
     C                   PARM      SCFYR         @@FYR             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     @@RRN         ANDNE     *ZEROS
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C                   MOVE      CMFYR         SCFYR
     C                   MOVE      CMCASE        SCCASE
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD=SCFYR
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*  SCREEN TWO
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*** BEG ADD *** 07/05/04 **************************************
     C                   Z-ADD     SCDTFI        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDTFI
     C*
     C                   Z-ADD     SCDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         SCDTRQ
     C*** END ADD *** 07/05/04 **************************************
     C*
     C*  ERROR CHECKING
     C*
     C     HFLD          COMP      'SCDTFI'                               55
     C     HFLD          COMP      'SCEAMT'                               56
     C     HFLD          COMP      'SCRAMT'                               57
     C     HFLD          COMP      'SCDTRQ'                               58
     C     HFLD          COMP      'SCCOM1'                               59
     C     HFLD          COMP      'SCCOM2'                               61
     C*
     C*  F10 - SEARCH - PAYEE AND MODIFIER
     C*
     C     HFLD          IFEQ      'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C*
     C                   MOVE      *ZEROS        QQFEIN            9
     C                   MOVE      *ZEROS        QQMOD             3
     C                   MOVE      '2'           QQCALL
     C*
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQRTC             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     QQRTC         ANDEQ     '*'
     C     QQFEIN        ANDNE     *ZEROS
     C                   MOVEL     QQFEIN        SCFEIN
     C                   MOVEL     QQMOD         SCMOD
     C                   MOVE      *ON           *IN51
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN50
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD=SCFEIN
     C*
     C*
     C*  F10 - SEARCH - STATUS CODE
     C*
     C     HFLD          IFEQ      'SCSTCD'
     C*
     C                   MOVE      *BLANKS       QQKEY            10
     C*
     C                   CALL      'W2211R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    QQKC              1
     C     *INKL         PARM                    QQKL              1
     C                   PARM                    QQKEY
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     QQKEY         ANDNE     *BLANKS
     C                   MOVEL     QQKEY         SCSTCD
     C                   MOVE      *ON           *IN52
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN51
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD=SCSTCD
     C*
     C*
     C*  F10 - SEARCH - ATTORNEY AND MODIFIER
     C*
     C     HFLD          IFEQ      'SCATTY'
     C     HFLD          OREQ      'SCAMOD'
     C*
     C                   MOVE      *ZEROS        QQFEIN            9
     C                   MOVE      *ZEROS        QQMOD             3
     C                   MOVE      '2'           QQCALL
     C*
     C                   CALL      'W0030R'
     C                   PARM                    QQCALL
     C                   PARM                    QQFEIN
     C                   PARM                    QQMOD
     C                   PARM                    QQRTC             1
     C*
     C     *INKC         CABEQ     *ON           TERM
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     *INKL         IFEQ      *OFF
     C     QQRTC         ANDEQ     '*'
     C     QQFEIN        ANDNE     *ZEROS
     C                   MOVEL     QQFEIN        SCATTY
     C                   MOVEL     QQMOD         SCAMOD
     C                   MOVE      *ON           *IN49
     C*
     C                   ELSE
     C                   MOVE      *ON           *IN52
     C*
     C                   ENDIF                                                  *INKL=OFF
     C*
     C                   ENDIF                                                  HFLD=SCATTY
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR02
     C*
     C                   ENDIF                                                  INKJ=ON
     C*
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C* ==================================================================
     C*   CHANGE STATUS
     C* ==================================================================
     C*
     C     *INKU         IFEQ      '1'
     C*** BEGIN CHG *** 01/23/98 **************************************
     C********** *IN40     ANDEQ'0'
     C     *IN40         IFEQ      *OFF
     C     *IN09         ANDEQ     *OFF
     C*** END   CHG *** 01/23/98 **************************************
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT            1
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
     C*** BEGIN ADD *** 01/23/98 **************************************
     C                   ENDIF                                                  *IN40=OFF
     C*** END   ADD *** 01/23/98 **************************************
     C                   GOTO      STRT
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
     C                   END
     C*                                                                ===
     C*** BEGIN DEL *** 01/23/98 **************************************
     C*                                                                ===
     C* ==================================================================
     C*   PROCESS CHANGE OF USER PARAMETERS
     C* ==================================================================
     C**********
     C********** *INKM     IFEQ '1'
     C**********
     C**********           CALL 'W0006R'
     C**********
     C*  GET THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *BLANK    SCCONM
     C**********           MOVEL*BLANK    SCFND
     C**********           Z-ADD*ZERO     SCYR
     C**********           MOVEL*BLANK    SCNAME
     C**********
     C**********           ELSE
     C**********
     C*  GET THE COMPANY RECORD
     C**********
     C********** COKEY     CHAINPCO01                60
     C**********           MOVELCONAME    SRDFLD    P
     C**********           CALL 'SRCENTER'
     C**********           PARM           SRDFLD 80
     C**********           PARM 40        SRDLEN  30
     C**********           MOVELSRDFLD    SCCONM
     C********** FNKEY     CHAINPFN01                60
     C**********           MOVELUPFND     SCFND
     C**********           Z-ADDUPFYR     SCYR
     C**********           MOVELFNNAME    SCNAME
     C**********
     C**********           END
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C*                                                                ===
     C*** END   DEL *** 01/23/98 **************************************
     C*                                                                ===
     C*   GOTO THE APPROPRIATE SCREEN
     C*
     C     Q2RECF        CABEQ     'SCR01'       SCR01T
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C*
     C*   PROCESS REQUEST SCREEN
     C*
     C     SCR01T        TAG
     C*
     C*  THE FUND YEAR CANNOT BE EQUAL TO ZERO
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C********** SCYR      IFLE *ZERO
     C     SCFYR         IFLE      *ZERO
     C*** END   CHG *** 01/23/98 **************************************
     C*                                                                ===
     C                   MOVE      '1'           *IN20
     C                   GOTO      STRT
     C                   END
     C*
     C*  THE CASE # CANNOT BE LESS THAN ZERO
     C*
     C     SCCASE        IFLE      *ZERO
     C                   MOVE      '1'           *IN21
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C**********           Z-ADDSCYR      XXFYR
     C                   Z-ADD     SCFYR         XXFYR
     C*** END   CHG *** 01/23/98 **************************************
     C*                                                                ===
     C                   MOVE      SCCASE        XXCASE
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C*
     C     OPT54         TAG
     C*
     C*** END   ADD *** 01/23/98 **************************************
     C*                                                                ===
     C*  VALIDATE THE CLAIM # (FND/YEAR/CASE)
     C*
     C     CMKEY         CHAIN     PCM01                              22
     C     *IN22         CABEQ     '1'           STRT
      *
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN22
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
     C*
     C*  LOAD CLAIMANT NAME FOR NEXT SCREEN
     C*
     C                   MOVE      *BLANKS       SCCLNM
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
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C     RRKEY         CHAIN     PRR01                              40
     C     *IN40         IFEQ      '0'
     C                   EXCEPT    PRR01E
     C*
     C                   MOVE      UA(2)         SCMODE
     C                   MOVE      RRREQ#        SCREQ#
     C                   MOVE      RRFEIN        SCFEIN
     C                   MOVE      RRMOD         SCMOD
     C                   MOVE      RRRAMT        SCRAMT
     C                   MOVE      RRRCVD        SCRCVD
     C*
     C                   Z-ADD     RRDTRC        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTRC
     C*
     C                   Z-ADD     RRDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTRQ
     C*
     C                   MOVEL     RRCOM1        SCCOM1
     C                   MOVEL     RRCOM2        SCCOM2
     C                   MOVE      RRSTAT        SCSTAT
     C*
     C                   Z-ADD     RRDTRE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTFI
     C*
     C                   MOVE      RRSTCD        SCSTCD
     C     RRATTY        IFNE      *ZERO
     C                   MOVE      RRATTY        SCATTY
     C                   MOVEL     RRAMOD        SCAMOD
     C                   ELSE
     C                   MOVE      *BLANKS       SCATTY
     C                   MOVE      *BLANKS       SCAMOD
     C                   END
     C                   MOVE      RREAMT        SCEAMT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C                   MOVE      UA(1)         SCMODE
     C                   MOVE      *ZERO         SCREQ#
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *ZERO         SCMOD
     C                   MOVE      *ZERO         SCRAMT
     C                   MOVE      *ZERO         SCRCVD
     C                   MOVE      *ZERO         SCDTRC
     C                   MOVE      *ZERO         SCDTRQ
     C                   MOVEL     *BLANK        SCCOM1
     C                   MOVEL     *BLANK        SCCOM2
     C                   MOVE      'A'           SCSTAT
     C                   Z-ADD     *ZERO         SCDTFI
     C                   MOVE      *BLANK        SCSTCD
     C                   MOVE      *BLANKS       SCATTY
     C                   MOVEL     *BLANK        SCAMOD
     C                   MOVE      *ZERO         SCEAMT
     C*
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C*
     C*   VALIDATE DATE FILED
     C*
     C     SCDTFI        IFNE      0
     C*
     C                   Z-ADD     SCDTFI        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTFI
     C*
     C     SRERR         CABEQ     '1'           STRT                     29
     C                   END
     C*
     C*   ESTIMATED REIMBURSEMENT AMT CANNOT BE LESS THAN OR = 0.
     C*
     C     SCEAMT        IFLT      *ZERO
     C                   MOVE      '1'           *IN30
     C                   GOTO      STRT
     C                   END
     C*
     C*   REFUND AMOUNT REQUESTED MUST NOT EQUAL *ZERO.
     C*
     C     SCRAMT        CABLT     *ZERO         STRT                   20
     C*
     C*   CHECK REFUND REQUEST DATE FOR VALID ENTRY.
     C*
     C     SCDTRQ        IFNE      0
     C*
     C                   Z-ADD     SCDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTRQ
     C*
     C     SRERR         CABEQ     '1'           STRT                     21
     C                   END
     C*
     C*   PAYEE AND MODIFIER MUST BE VALID.
     C*
     C*** BEGIN DEL *** 01/23/98 **************************************
     C**********           MOVELSCFEIN    XX1     1
     C********** XX1       IFEQ '?'
     C**********           MOVE '1'       *IN28
     C**********           GOTO STRT
     C**********           END
     C*** END   DEL *** 01/23/98 **************************************
     C*
     C                   MOVE      SCFEIN        XXFEIN            9 0
     C                   MOVE      SCMOD         XXMOD             3 0
     C     PDKEY         CHAIN     PPD01                              28
     C     *IN28         CABEQ     '1'           STRT
     C*
     C*   REFUND STATUS CODE MUST BE A VALID CODE & ACTIVE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     SCSTCD        XXKEY
     C     TBKEY         CHAIN     PRS01                              31
     C     *IN31         CABEQ     '1'           STRT
     C*
     C     RSSTAT        CABEQ     'I'           STRT                     32
     C*
     C*   ATTORNEY AND MODIFIER MUST BE VALID.
     C*
     C     SCATTY        IFNE      *BLANKS
     C*
     C*** BEGIN DEL *** 01/23/98 **************************************
     C**********           MOVELSCATTY    XX1     1
     C********** XX1       IFEQ '?'
     C**********           MOVE '1'       *IN33
     C**********           GOTO STRT
     C**********           END
     C*** END   DEL *** 01/23/98 **************************************
     C*
     C                   MOVE      SCATTY        XXFEIN
     C                   MOVE      SCAMOD        XXMOD
     C     PDKEY         CHAIN     PPD01                              33
     C*** BEG DEL *** 11/02/09 ****************************************
     C****  N33      PDSTAT    COMP 'I'                      33
     C*** END DEL *** 11/02/09 ****************************************
     C     *IN33         IFEQ      '1'
     C*** BEG ADD *** 11/02/09 ****************************************
     C     *IN33         OREQ      *OFF
     C     PDSTAT        ANDNE     'A'
     C     PDSTAT        ANDNE     *BLANKS
     C                   MOVE      *ON           *IN33
     C*** END ADD *** 11/02/09 ****************************************
     C                   GOTO      STRT
     C                   END
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C**********           MOVE SCYR      XXFYR
     C                   MOVE      SCFYR         XXFYR
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C                   MOVE      SCCASE        XXCASE
     C                   MOVE      SCREQ#        XXREQ#
     C*
     C     RRKEY         CHAIN     PRR01                              40
     C     *IN40         IFEQ      '1'
     C     RRKY2         SETGT     PRR01
     C                   READP     PRR01                                  41
     C     *IN41         IFEQ      '0'
     C     RRFYR         ANDEQ     XXFYR
     C     RRCASE        ANDEQ     XXCASE
     C     RRREQ#        ADD       1             RRREQ#
     C                   ELSE
     C                   Z-ADD     1             RRREQ#
     C                   END
     C                   ELSE
     C                   MOVE      SCREQ#        RRREQ#
     C                   END
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C**********           MOVE UPCO#     RRCO#
     C**********           MOVE UPFND     RRFND
     C**********           MOVE SCYR      RRFYR
     C*
     C                   MOVE      XXCO#         RRCO#
     C                   MOVE      SCFND         RRFND
     C                   MOVE      SCFYR         RRFYR
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C                   MOVE      SCCASE        RRCASE
     C                   MOVE      SCFEIN        RRFEIN
     C                   MOVE      SCMOD         RRMOD
     C                   MOVE      SCRAMT        RRRAMT
     C                   MOVE      SCRCVD        RRRCVD
     C*
     C                   Z-ADD     SCDTRC        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDTRC
     C*
     C                   Z-ADD     SCDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDTRQ
     C*
     C                   MOVE      *BLANKS       RRDESC
     C                   MOVE      *BLANKS       RRAFFL
     C                   MOVE      *BLANKS       RREPFL
     C                   MOVE      *BLANKS       RRMEFL
     C                   MOVE      *BLANKS       RRPEFL
     C                   MOVEL     SCCOM1        RRCOM1
     C                   MOVEL     SCCOM2        RRCOM2
     C                   MOVE      SCSTAT        RRSTAT
     C                   MOVE      Q1USER        RRUSER
     C                   MOVE      Q1PGM         RRPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDT
     C*
     C                   Z-ADD     DSTIME        RRTIME
     C*
     C                   Z-ADD     SCDTFI        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RRDTRE
     C*
     C                   MOVE      SCSTCD        RRSTCD
     C                   MOVE      *BLANKS       RRSTTP
     C                   MOVE      SCATTY        RRATTY
     C                   MOVE      SCAMOD        RRAMOD
     C                   Z-ADD     SCEAMT        RREAMT
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PRR01
     C                   ELSE
     C                   WRITE     PRR01
     C                   END
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C     *IN54         IFEQ      *OFF
     C                   MOVE      *BLANK        Q2RECF
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEGIN CHG *** 01/23/98 **************************************
     C**********           Z-ADDUPFYR     SCYR
     C                   Z-ADD     UPFYR         SCFYR
     C*** END   CHG *** 01/23/98 **************************************
     C*
     C                   MOVE      *ZERO         SCCASE
     C*
     C*** BEGIN ADD *** 01/23/98 **************************************
     C*
     C                   ENDIF                                                  *IN54=OFF
     C*
     C     *IN54         IFEQ      *ON
     C*
     C                   MOVE      *BLANKS       SCDTFI
     C                   MOVE      *ZEROS        SCEAMT
     C                   MOVE      *ZEROS        SCRAMT
     C                   MOVE      *BLANKS       SCDTRQ
     C                   MOVE      *BLANKS       SCFEIN
     C                   MOVE      *BLANKS       SCMOD
     C                   MOVE      *BLANKS       SCSTCD
     C                   MOVE      *BLANKS       SCATTY
     C                   MOVE      *BLANKS       SCAMOD
     C                   MOVE      *BLANKS       SCCOM1
     C                   MOVE      *BLANKS       SCCOM2
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ENDIF                                                  *IN54=ON
     C*
     C*** END   ADD *** 01/23/98 **************************************
     C*
     C                   GOTO      STRT
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     OPRR01     E            PRR01E
**  TABLE UA
-- ADD --
- UPDATE -
