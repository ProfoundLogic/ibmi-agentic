     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0330R - PARTIAL FILING MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  09/11/07 C: 53 Project: WC132 - 59375                        *
     H*                 Remove display condition of SCR01 fields      *
     H*                 * changes in screen - recompile only
     H*                                                               *
     H*  08/30/02 C:15 Project: WC577                                 *
     H*                Chgd F23 to F04.                               *
     H*                                                               *
     H*  11/29/99 C:118 PROJECT: WC577                                *
     H*                 Modified program name in display file.        *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  10/24/96 C:36 PROJECT WC507                                  *
     H*                ADD FLAG TO CONTROL THE ALLOCATION OF          *
     H*                EXPENSE PAYMENTS ON SPECIFIC FILINGS           *
     H*                                                               *
     H*  07/07/92 JEI  PROJECT: WC157                                 *
     H*                CHANGED CASE NUMBER TO 7 NUMERIC FOR 'BASE'    *
     H*                FILES (XXCASE) - CASE NUMBER REMAINS 14 ALPHA  *
     H*                FOR REINSURANCE FILES (X@CASE)                 *
     H*                                                               *
     H*  04/03/92 LPK  PROJECT: MD027                                 *
     H*                CHANGED CASE NUMBER FROM 7 NUMERIC TO 14       *
     H*                ALPHA FIELD.                                   *
     H*                CHANGED CODE TO USE EMP/DIV FOR CMD-5.         *
     H*                                                               *
     H*  10/25/90 LJD   - ADDED CMD 14 - DARTS                        *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  99   IF CALL TO PGM SC010R (DARTS) ENDS IN ERROR RETURN TO   *
     H*       SAME SCREEN                                             *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FW0330F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDPFL4    UF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMRIL1    IF   E           K DISK
     FWAMCL1    IF   E           K DISK
     FWDRVL1    IF   E           K DISK

     D*
     D*** BEGIN CHG *** 01/28/98 ***********************************************
     D**********          UA      1   2 10
     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
     D*** END   CHG *** 01/28/98 ***********************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  QCKEY                  1     29  0
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCFYR                  7      9  0
     D  QCRNDT                10     17  0
     D  QCCASE                18     24  0
     D  QCMCLM                25     31  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Specific Partial Fil-
     D                                     ing Maintenance')

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
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND             3 0
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR             3 0
     C*
     C*  PARTIAL FILING FILE
     C*
     C     PFKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXRNDT
     C                   KFLD                    X@CASE
     C                   KFLD                    SCMCLM
     C*
     C*  CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  CLAIMANT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  MULTIPLE CLAIM FILE
     C*
     C     MCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    PFMCLM
     C*
     C*   KEY LIST FOR REINSURANCE POLICY FILE
     C*
     C     RIKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL
     C                   KFLD                    CMACDT
     C*
     C*   KEY LIST FOR THE REIN RECOVERY RECEIVED FILE
     C*
     C     RVKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXASFL
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C*
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    PFCASE        X@CASE
     C     *LIKE         DEFINE    RIEMP#        XXEMP#
     C     *LIKE         DEFINE    RIDIV         XXDIV
     C     *LIKE         DEFINE    RIDT          XXRNDT
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
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPFND         XXFND             3 0
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
     C     FNKEY         CHAIN     PFN01                              60
     C***** BEGIN ADD **** 11/29/99 ***********************************
     C     *IN60         IFEQ      *OFF
     C***** END   ADD **** 11/29/99 ***********************************
     C                   MOVEL     UPFND         SCFND
     C                   MOVEL     FNNAME        SCNAME
     C***** BEGIN ADD **** 11/29/99 ***********************************
     C                   ELSE
     C                   MOVEL     *BLANKS       SCFND
     C                   MOVEL     *BLANKS       SCNAME
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 11/29/99 ***********************************
     C     SCFYR         IFEQ      *ZERO
     C                   Z-ADD     UPFYR         SCFYR
     C                   END
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE *ZEROS    SCRNDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        SCMCLM
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C**********           TIME           DSDTTM
     C*** END   DEL *** 01/28/98 **************************************
     C*
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
     C*** BEG ADD *** 09/11/07 **************************************
     C     *IN09         IFNE      *ON
     C                   MOVE      MD(3)         SCMODE
     C                   ELSE
     C                   MOVE      MD(1)         SCMODE
     C                   ENDIF                                                  *IN09<>ON
     C*** END ADD *** 09/11/07 **************************************
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
     C     Q2RECF        ANDEQ     'SCR02'
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   ENDIF                                                  *INKL=ON
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C*
     C**********
     C**F5=PARTIAL FILE SEARCH
     C**********
     C********** *INKE     IFEQ '1'
     C**********           MOVE SCYR      QQYR    20
     C**********           MOVE SCRNDT    QQRNDT  80
     C**********           MOVE SCCASE    QQCASE  70
     C**********           MOVE SCMCLM    QQMCLM  70
     C**********
     C**********           CALL 'W0331R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           QQYR
     C**********           PARM           QQRNDT
     C**********           PARM           QQCASE
     C**********           PARM           QQMCLM
     C**********           PARM           QQRTC   1
     C**********
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********
     C********** QQRTC     IFEQ 'B'
     C********** QQRNDT    OREQ *ZERO
     C**********           GOTO STRT
     C**********           END
     C**********
     C**********           MOVE QQYR      SCYR
     C**********           MOVE QQRNDT    SCRNDT
     C**********           MOVE QQCASE    SCCASE
     C**********           MOVE QQMCLM    SCMCLM
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
     C*  F10 = PARTIAL FILE SEARCH
     C*
     C     *INKJ         IFEQ      *ON
     C*
     C**      ERROR CHECKING - NOT ON VALID FIELD FOR SEARCH
     C*
     C     HFLD          COMP      'SCRNDT'                               34
     C*
     C     HFLD          IFNE      'SCRNDT'
     C*
     C**      POSITION CURSOR
     C*
     C     HFLD          COMP      'SCFYR'                                30
     C     HFLD          COMP      'SCCASE'                               32
     C     HFLD          COMP      'SCMCLM'                               33
     C*
     C                   CALL      'W0331R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM      *ZEROS        QQFYR             3 0
     C                   PARM      *ZEROS        QQRNDT            8 0
     C                   PARM      *ZEROS        QQCASE            7 0
     C                   PARM      *ZEROS        QQMCLM            7 0
     C                   PARM      *BLANK        QQRTC             1
     C*
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C     QQRTC         IFEQ      '*'
     C     QQRNDT        ANDNE     *ZEROS
     C                   MOVE      QQFYR         SCFYR
     C                   MOVE      QQRNDT        SCRNDT
     C                   MOVE      QQCASE        SCCASE
     C                   MOVE      QQMCLM        SCMCLM
     C                   ENDIF                                                  QQRTC=*
     C*
     C                   ENDIF                                                  HFLD<>SCRNDT
     C*
     C                   ENDIF                                                  INKJ=ON
     C*
     C*  F13 = PROCESS CHANGE OF USER PARAMETERS
     C*
     C     *INKM         IFEQ      *ON
     C*
     C                   CALL      'W0006R'
     C*
     C*    GET THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *BLANK        SCCONM
     C                   MOVEL     *ZERO         SCFND
     C                   Z-ADD     *ZERO         SCFYR
     C                   MOVEL     *BLANK        SCNAME
     C                   ENDIF                                                  *IN60=ON
     C*
     C                   GOTO      STRT01
     C                   ENDIF                                                  *INKM=ON
     C*
     C*  F23 = DELETE PARTIAL FILING (MUST DO F23 TWICE SUCCESSIVELY)
     C*
     C*** Begin dlt *** 08/30/02 **************************************
     C********** *INKX     IFEQ *ON
     C**********           MOVE *OFF      *INKX
     C*** End dlt ***** 08/30/02 **************************************
     C*** Begin add *** 08/30/02 **************************************
     C     *INKD         IFEQ      *ON
     C                   MOVE      *OFF          *INKD
     C*** End add ***** 08/30/02 **************************************
     C                   MOVE      *ON           *IN89                          ERR MSG 1119
     C                   EXFMT     SCR01
     C                   MOVE      *OFF          *IN89
     C*
     C*** Begin chg *** 08/30/02 **************************************
     C********** *INKX     IFEQ *ON
     C     *INKD         IFEQ      *ON
     C*** End chg ***** 08/30/02 **************************************
     C*
     C*  VALIDATE FYR, RNDT, CASE, MULT. CASE
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C*
     C     PFKEY         CHAIN     PPF01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   DELETE    PPF01
     C*
     C                   Z-ADD     *ZEROS        SCFYR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C                   Z-ADD     *ZEROS        SCCASE
     C                   Z-ADD     *ZEROS        SCMCLM
     C*
     C                   ENDIF                                                  *IN60=OFF
     C*
     C                   ENDIF                                                  *INKX=ON
     C*
     C                   ENDIF                                                  *INKX=ON
     C*
     C                   MOVEL(P)  'SCR01'       Q2RECF
     C                   GOTO      STRT
     C*
     C                   ENDIF                                                  Q2RECF=SCR01
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*  DISPLAY POLICY INFORMATION
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C*  IF MULTIPLE CLAIM, GET ACCIDENT DATE TO CHAIN TO POLICY FILE
     C*
     C     PFCASE        IFEQ      *ZEROS
     C     MCKEY         CHAIN     PMC01                              60
     C                   MOVE      MCCASE        XXCASE
     C                   ELSE
     C                   MOVE      SCCASE        XXCASE
     C                   END
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C                   MOVE      'S'           XXASFL
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C     RIKEY         SETGT     PRI01
     C                   READP     PRI01                                  60
     C     *IN60         IFEQ      '1'
     C     RICO#         ORNE      XXCO#
     C     RIFND         ORNE      XXFND
     C     RIEMP#        ORNE      XXEMP#
     C     RIDIV         ORNE      XXDIV
     C     RIASFL        ORNE      'S'
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     RIKEY         SETGT     PRI01
     C                   READP     PRI01                                  60
     C                   END
     C     *IN60         IFEQ      '0'
     C     RICO#         ANDEQ     XXCO#
     C     RIFND         ANDEQ     XXFND
     C     RIEMP#        ANDEQ     XXEMP#
     C     RIDIV         ANDEQ     XXDIV
     C     RIASFL        ANDEQ     'S'
     C                   MOVE      '1'           Q@CALL            1
     C                   MOVE      *BLANKS       QQEMP#            9
     C                   MOVE      XXEMP#        QQEMP#
     C                   MOVE      *BLANKS       QQDIV             5
     C                   MOVE      XXDIV         QQDIV
     C                   MOVE      'S'           QQASFL            1
     C*
     C                   Z-ADD     RIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEDT             8 0
     C*
     C                   MOVE      XXEDT         QQEFDT            8
     C                   CALL      'W0310R'
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   PARM                    QQPRDS
     C                   PARM                    Q@CALL
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQASFL
     C                   PARM                    QQEFDT
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C                   MOVEL(P)  'SCR02'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF                                                  *INKL=ON
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           GOTO STRT
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C                   END
     C                   END
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C*
     C**********           GOTO STRT
     C**********           END
     C****CMD*8*- DELETE PARTIAL FILING
     C**********
     C********** *INKH     IFEQ '1'
     C********** Q2RECF    ANDEQ'SCR01'
     C**********
     C***VALIDATE FYR, RNDT, CASE, MULT. CASE
     C**********
     C**********           Z-ADDSCYR      XXFYR
     C**********           MOVE *ZEROS    X@CASE
     C**********           MOVE SCCASE    X@CASE
     C**********
     C**********           Z-ADDSCRNDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXRNDT
     C**********
     C********** PFKEY     CHAINPPF01                20
     C********** *IN20     CABEQ'1'       STRT
     C**********
     C**********           DELETPPF01
     C**********
     C**********           MOVE *BLANK    Q2RECF
     C**********           MOVEL'SCR01'   Q2RECF
     C**********           GOTO STRT
     C**********           END
     C*
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C***************************************
     C*  CMD 14 - DARTS                     *
     C***************************************
     C     *INKN         IFEQ      '1'
     C*
     C                   MOVE      Q2RECF        HLRECF           10
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      UPCO#         QCCO#
     C                   MOVE      UPFND         QCFND
     C                   MOVE      SCRNDT        QCRNDT
     C                   MOVE      XXFYR         QCFYR
     C                   MOVE      SCCASE        QCCASE
     C                   MOVE      SCMCLM        QCMCLM
     C*
     C                   CALL      'SC010R'                             99
     C                   PARM                    QCKEY
     C                   PARM                    Q1PGM
     C*
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVE      HLRECF        Q2RECF
     C                   GOTO      STRT
     C                   END                                                    END *INKN
     C*
     C*** BEGIN DEL *** 01/28/98 **************************************
     C*
     C**********
     C****PROCESS HELP REQUESTS
     C**********
     C********** *IN10     IFEQ '1'
     C**********
     C**********           MOVE Q2RECF    XXRECF 10
     C**********           MOVEL'QHLP    'FILNAM 10
     C**********           MOVEL'*LIBL   'LIBNAM 10
     C**********           MOVE Q1PGM     MBRNAM
     C**********           MOVE XXSCR#    MBRNAM 10
     C**********           CALL 'M0006C'
     C**********           PARM           FILNAM
     C**********           PARM           LIBNAM
     C**********           PARM           MBRNAM
     C**********           MOVE XXRECF    Q2RECF
     C**********
     C**********           GOTO STRT
     C**********           END
     C****PROCESS CHANGE OF USER PARAMETERS
     C**********
     C********** *INKM     IFEQ '1'
     C**********
     C**********           CALL 'W0006R'
     C**********
     C***GET*THE USER PARAMETER FILE
     C**********
     C********** UPKEY     CHAINPUP01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *ZERO     UPCO#
     C**********           MOVE *BLANK    SCCONM
     C**********           MOVEL*ZERO     SCFND
     C**********           Z-ADD*ZERO     SCYR
     C**********           MOVEL*BLANK    SCNAME
     C**********           END
     C**********           GOTO STRT01
     C**********           END
     C**********           END
     C*
     C*** END   DEL *** 01/28/98 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C                   ENDIF                                                  *IN11=ON
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
     C*  FUND YEAR MUST BE VALID
     C*
     C                   Z-ADD     SCFYR         XXFYR             3 0
     C     F1KEY         CHAIN     PF101                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*  VALIDATE RUN DATE
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*  PARTIAL FILING MUST ALREADY EXIST
     C*
     C                   Z-ADD     SCRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C     PFKEY         CHAIN     PPF01                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*  IF MULTIPLE CLAIM, GET ACCIDENT DATE TO CHAIN TO POLICY FILE
     C*
     C     PFCASE        IFEQ      *ZEROS
     C     MCKEY         CHAIN     PMC01                              60
     C                   MOVE      MCCASE        XXCASE
     C                   ELSE
     C                   MOVE      SCCASE        XXCASE
     C                   END
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C                   MOVE      'S'           XXASFL
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C     RIKEY         SETGT     PRI01
     C                   READP     PRI01                                  60
     C     *IN60         IFEQ      '1'
     C     RICO#         ORNE      XXCO#
     C     RIFND         ORNE      XXFND
     C     RIEMP#        ORNE      XXEMP#
     C     RIDIV         ORNE      XXDIV
     C     RIASFL        ORNE      'S'
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     RIKEY         SETGT     PRI01
     C                   READP     PRI01                                  60
     C                   END
     C     *IN60         IFEQ      '0'
     C     RICO#         ANDEQ     XXCO#
     C     RIFND         ANDEQ     XXFND
     C     RIEMP#        ANDEQ     XXEMP#
     C     RIDIV         ANDEQ     XXDIV
     C     RIASFL        ANDEQ     'S'
     C                   END
     C*
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C                   EXCEPT    PPF01E
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           MOVE UA,2      SCMODE
     C*** BEG DEL *** 09/11/07 **************************************
     C**********           MOVE MD,3      SCMODE
     C*** END DEL *** 09/11/07 **************************************
     C*** END   CHG *** 01/28/98 **************************************
     C*** BEG ADD *** 09/11/07 **************************************
     C     *IN09         IFNE      *ON
     C                   MOVE      MD(3)         SCMODE
     C                   ELSE
     C                   MOVE      MD(1)         SCMODE
     C                   ENDIF                                                  *IN09<>ON
     C*** END ADD *** 09/11/07 **************************************
     C*
     C                   MOVE      PFCOMP        SCCOMP
     C                   MOVE      PFMED         SCMED
     C                   MOVE      PFHOSP        SCHOSP
     C                   MOVE      PFMISC        SCMISC
     C                   MOVE      PFATTY        SCATTY
     C     RIEXPR        IFEQ      'F'
     C                   MOVE      PFDEF         SCDEF1
     C                   Z-ADD     0             SCDEF
     C                   MOVE      '1'           *IN55
     C                   ELSE
     C                   MOVE      PFDEF         SCDEF
     C                   Z-ADD     0             SCDEF1
     C                   END
     C     PFCOMP        ADD       PFMED         SCTOTL
     C                   ADD       PFHOSP        SCTOTL
     C                   ADD       PFMISC        SCTOTL
     C                   ADD       PFATTY        SCTOTL
     C     RIEXPR        IFEQ      'F'
     C                   ADD       PFDEF         SCTOTL
     C                   Z-ADD     SCTOTL        SCPAID
     C                   ELSE
     C     SCTOTL        ADD       PFDEF         SCPAID           11 2
     C                   END
     C                   MOVE      PFRETN        SCRETN
     C                   MOVE      PFFDEF        SCFDEF
     C     SCTOTL        SUB       PFRETN        SCEXCS
     C     RIEXPR        IFNE      'F'
     C     PFDEF         SUB       PFFDEF        XXRDEF           11 2
     C                   ADD       XXRDEF        SCEXCS
     C                   END
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      PFCASE        X@CASE
     C                   Z-ADD     0             XXRCVD           11 2
     C*
     C*  CALCULATE BALANCE DUE
     C*
     C     PFCASE        IFEQ      *ZEROS
     C     MCKEY         CHAIN     PMC01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      MCCASE        X@CASE
     C                   EXSR      PMT
     C     MCKEY         READE     PMC01                                  60
     C                   END
     C                   ELSE
     C                   EXSR      PMT
     C                   END
     C*
     C     SCEXCS        SUB       XXRCVD        SCBAL
     C*
     C*  LOAD CLAIMANT NAME IF NOT A MULTIPLE CLAIM SUMMARY
     C*
     C                   MOVE      '1'           *IN79
     C                   MOVE      *BLANKS       SCCLNM
     C                   MOVE      X@CASE        XXCASE
     C     PFCASE        IFNE      *ZEROS
     C*
     C                   MOVE      '0'           *IN79
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *IN60         IFEQ      *OFF
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *IN60         IFEQ      *OFF
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           MOVELCDLSN     QQLAST 56
     C**********           MOVELCDFSN     QQFRST 15
     C                   MOVEL(P)  CDLSN         QQLAST           56
     C                   MOVEL(P)  CDFSN         QQFRST           15
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        SCCLNM
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   ENDIF                                                  *IN60=OFF
     C                   ENDIF                                                  *IN60=OFF
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   END
     C*
     C*  WE HAVE PASSED ALL EDITS, GOTO THE NEXT SCREEN
     C*
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C     *IN09         CABEQ     '1'           NOEDT
     C*
     C*   AMOUNTS CANNOT BE LESS THAN 0
     C*
     C     SCCOMP        COMP      0                                    20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C     SCMED         COMP      0                                    21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C     SCHOSP        COMP      0                                    22
     C     *IN22         CABEQ     '1'           STRT
     C*
     C     SCMISC        COMP      0                                    23
     C     *IN23         CABEQ     '1'           STRT
     C*
     C     SCATTY        COMP      0                                    24
     C     *IN24         CABEQ     '1'           STRT
     C*
     C     SCDEF         IFLT      0
     C     SCDEF1        ORLT      0
     C                   MOVE      '1'           *IN25
     C                   END
     C     *IN25         CABEQ     '1'           STRT
     C*
     C*  ACCUMULATION OF AMOUNTS LESS DEFENSE MUST EQUAL ORIGINAL TOTAL
     C*
     C     SCCOMP        ADD       SCMED         XXPAID           11 2
     C                   ADD       SCHOSP        XXPAID
     C                   ADD       SCMISC        XXPAID
     C                   ADD       SCATTY        XXPAID
     C     RIEXPR        IFEQ      'F'
     C                   ADD       SCDEF1        XXPAID
     C                   ELSE
     C                   ADD       SCDEF         XXPAID
     C                   END
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C     SCPAID        IFNE      XXPAID
     C                   MOVE      '1'           *IN26
     C                   GOTO      STRT
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C     PFKEY         CHAIN     PPF01                              60
     C*
     C                   MOVE      SCCOMP        PFCOMP
     C                   MOVE      SCMED         PFMED
     C                   MOVE      SCHOSP        PFHOSP
     C                   MOVE      SCMISC        PFMISC
     C                   MOVE      SCATTY        PFATTY
     C     RIEXPR        IFEQ      'F'
     C                   MOVE      SCDEF1        PFDEF
     C                   ELSE
     C                   MOVE      SCDEF         PFDEF
     C                   END
     C                   MOVE      Q1USER        PFUSER
     C                   MOVE      Q1PGM         PFPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PFDT
     C*
     C                   Z-ADD     DSTIME        PFTIME
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PPF01
     C                   END
     C*
     C     NOEDT         TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C*
     C**********           MOVE *ZEROS    SCRNDT
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCRNDT
     C*
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C                   MOVE      *ZEROS        SCFYR
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *BLANKS       SCMCLM
     C                   GOTO      STRT
     C*
     C*
     C*  END THE PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
     C**********************************************************
     C*  SUBROUTINE PMT-GET AMOUNT RECEIVED FROM REINSURER
     C**********************************************************
     C*
     C     PMT           BEGSR
     C*
     C                   MOVE      'S'           XXASFL            1
     C     RVKEY         CHAIN     PRV01                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       RVAMT         XXRCVD
     C     RVKEY         READE     PRV01                                  60
     C                   END
     C*
     C                   ENDSR
     C*
     OPPF01     E            PPF01E
     O*
     O*** BEGIN DEL *** 01/28/98 **************************************
     O********** **  TABLE UA
     O********** -- ADD --
     O********** - UPDATE -
     O*** END   DEL *** 01/28/98 **************************************
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
