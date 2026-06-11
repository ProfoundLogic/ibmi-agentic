     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W0340R - REINSURANCE REQUEST RECOVERY MAINTENANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  08/05/03 C:15 Support: 36765                                 *
     H*                Chgd edit to allow a negative recovery amt     *
     H*                requested.                                     *
     H*                                                               *
     H*  01/11/01 C:15 Project:                                       *
     H*                Corrected the retrieval of the most current    *
     H*                partial filing record.                         *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  10/24/96 C:36 PROJECT WC507                                  *
     H*                ADD FLAG TO CONTROL THE ALLOCATION OF          *
     H*                EXPENSE PAYMENTS ON SPECIFIC FILINGS           *
     H*                                                               *
     H*  08/03/92 JJJ  PROJECT: WC157                                 *
     H*                LOAD MULT CLM# TO REQUEST FILE                 *
     H*                                                               *
     H*  07/07/92 JEI  PROJECT: WC157                                 *
     H*                CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR 'BAS'*
     H*                FILES (XXCASE) - CASE NUMBER REMAINS 14 ALPHA  *
     H*                FOR REINSURANCE FILES (X@CASE)                 *
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
     FW0340F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDPFL2    IF   E           K DISK
     FWDPFL3    IF   E           K DISK
     F                                     RENAME(PPF01:PPF02)
     FWMCDL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F*** BEGIN ADD *** 01/28/98 **************************************
     F                                     INFDS(DBFDS)
     F*** END   ADD *** 01/28/98 **************************************
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     F*** BEGIN ADD *** 01/28/98 **************************************
     F                                     RENAME(PEM01:PEM02)
     F                                     INFDS(DBFDS2)
     FWMEMP     IF   E             DISK
     F*** END   ADD *** 01/28/98 **************************************
     FWMRCL     IF   E           K DISK
     FWMRQL     UF A E           K DISK
     FWMRQL1    UF A E           K DISK
     F                                     RENAME(PRQ01:PRQ02)
     FWMRIL1    IF   E           K DISK
     FWAMCL1    IF   E           K DISK

     D*
     D*** BEGIN CHG *** 01/28/98 ***********************************************
     D**********          UA      1   2 10
     D MD              S             10    DIM(4) CTDATA PERRCD(1)              SCMODE TABLE
     D*** END   CHG *** 01/28/98 ***********************************************
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
     D  CO                     1     30  0
     D                                     DIM(10) ASCEND                       REIN CO#
     D  RICO01                 1      3  0
     D  RICO02                 4      6  0
     D  RICO03                 7      9  0
     D  RICO04                10     12  0
     D  RICO05                13     15  0
     D  RICO06                16     18  0
     D  RICO07                19     21  0
     D  RICO08                22     24  0
     D  RICO09                25     27  0
     D  RICO10                28     30  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Request Recovery Mai-
     D                                     ntenance')

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
     C     RQKY2         KLIST
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
     C     PFKY1         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXMCLM            7 0
     C                   KFLD                    X@CASE
     C                   KFLD                    XXDATE
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
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
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
     C*   KEY LIST FOR THE MULTIPLE CLAIM FILE
     C*
     C     MCKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXMCLM
     C*
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    PFCASE        X@CASE
     C     *LIKE         DEFINE    RIEMP#        XXEMP#
     C     *LIKE         DEFINE    RIDIV         XXDIV
     C     *LIKE         DEFINE    RIDT          XXEFDT
     C     *LIKE         DEFINE    RIDT          XXDATE
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *LIKE         DEFINE    EMEMP#        Q@EMP#
     C     *LIKE         DEFINE    EMDIV         Q@DIV
     C     *LIKE         DEFINE    SCEFDT        Q@EFDT
     C*** END   ADD *** 01/28/98 **************************************
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
     C* F5=PARTIAL FILE SEARCH
     C**********
     C********** *INKE     IFEQ '1'
     C**********           Z-ADDSCYR      XXFYR
     C**********           Z-ADDUPFND     XXFND   30
     C**********           Z-ADD0         QQRNDT  80
     C**********           CALL 'W0331R'
     C**********           PARM           QQPRDS
     C**********           PARM           QQCALL
     C**********           PARM           XXFYR
     C**********           PARM           QQRNDT
     C**********           PARM           SCCASE  70
     C**********           PARM           SCMCLM
     C**********           PARM           XXSEQ   50
     C********** QQRTC     CABEQ'A'       TERM
     C**********
     C**********           Z-ADDXXFYR     SCYR
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
     C**********           MOVEL'SCR02'   Q2RECF
     C**********           GOTO STRT
     C**********
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
     C*  F10 = SEARCH - CASE NUMBER
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
     C*  F10 = SEARCH - EMPLOYER AND DIVISION
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
     C                   MOVE      *ON           *IN32                          SCRAMT POSITION
     C                   ENDIF                                                  *INKL=OFF
     C*
     C     *INKL         IFEQ      *ON
     C     QQRTC         OREQ      'B'
     C     QQRCO         OREQ      *ZEROS
     C                   MOVE      *ON           *IN31                          SCRCO POSITION
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
     C*   COMMAND 07 = REQUEST RECOVERY SEARCH
     C*
     C     *INKG         IFEQ      '1'
     C     Q2RECF        ANDEQ     'SCR01'
     C*
     C     SCASFL        IFEQ      'S'
     C*
     C*  VALIDATE THE CLAIM # (CO/FND/YEAR/CASE) IF SPECIFIC
     C*
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C     PFKEY         CHAIN     PPF01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C                   CALL      'W0341R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCFYR
     C                   PARM                    SCASFL
     C                   PARM                    SCCASE
     C                   PARM                    XXREQ#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCFYR         OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C**
     C     SCASFL        IFEQ      'A'
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
     C*
     C                   CALL      'W0342R'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    SCASFL
     C                   PARM                    SCEMP#
     C                   PARM                    SCDIV
     C                   PARM                    SCEFDT
     C                   PARM                    XXREQ#
     C                   PARM                    QQRTC             1
     C     QQRTC         CABEQ     'A'           TERM
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C*
     C     QQRTC         IFEQ      'B'
     C     SCEFDT        OREQ      *ZERO
     C                   GOTO      STRT
     C                   END
     C*
     C     QQRTC         CABEQ     '*'           SCR01T
     C                   END
     C*
     C                   END
     C*
     C*   CHANGE STATUS
     C*
     C     *INKU         IFEQ      '1'
     C     *IN40         ANDEQ     '0'
     C     SCSTAT        IFEQ      'A'
     C                   MOVE      'I'           SCSTAT            1
     C                   ELSE
     C                   MOVE      'A'           SCSTAT
     C                   END
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
     C                   Z-ADD     UPFYR         SCFYR
     C                   MOVEL     FNNAME        SCNAME
     C*
     C                   END
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   END
     C*
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
     C*  IF SPECIFIC, CLAIM MUST EXIST AS A PARTIAL FILING
     C*
     C                   MOVE      *BLANKS       SCCLNM
     C                   MOVE      *BLANKS       SCENAM
     C                   Z-ADD     0             SCMCLM
     C                   MOVE      '0'           *IN78
     C*** BEGIN CHG *** JJJ *** 08/03/92
     C                   MOVE      '0'           *IN79
     C*** END   CHG *** JJJ *** 08/03/92
     C     SCASFL        IFEQ      'S'
     C*
     C*  FUND YR & CASE MUST EXIST IN THE PARTIAL FILING FILE
     C*
     C                   Z-ADD     SCFYR         XXFYR
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C     PFKEY         CHAIN     PPF01                              21
     C     *IN21         CABEQ     '1'           STRT
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*          *IN21     IFEQ *OFF
     C*                    MOVEL'SCR02'   Q2RECF    P
     C*                    GOTO STRT
     C*                    ENDIF                           *IN21=OFF
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C* SET THE POLICY KEY
     C*
     C                   Z-ADD     PFMCLM        SCMCLM
     C*** BEGIN CHG *** JJJ *** 08/03/92
     C     SCMCLM        IFNE      *ZEROS
     C                   MOVE      '1'           *IN79
     C                   END
     C*** END   CHG *** JJJ *** 08/03/92
     C                   MOVE      '0'           *IN78
     C                   Z-ADD     PFEMP#        XXEMP#
     C                   Z-ADD     PFDIV         XXDIV
     C                   Z-ADD     PFEFDT        XXEFDT
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     RIKEY         CHAIN     PRI01                              25
     C     *IN25         CABEQ     '1'           STRT
     C                   END
     C*
     C*  LOAD CLAIMANT NAME TO SCR02
     C*
     C                   MOVE      SCCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              60
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C     *IN60         IFEQ      *OFF
     C*** END   ADD *** 01/28/98 **************************************
     C*
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
     C*** BEGIN ADD *** 01/28/98 **************************************
     C                   ENDIF                                                  *IN60=OFF
     C*** END   ADD *** 01/28/98 **************************************
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
     C                   MOVE      *ZEROS        XXCASE
     C                   MOVE      *ZEROS        X@CASE
     C*
     C*   LOAD EMPLOYER NAME TO SCR02
     C*
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
     C*  GET THE REQUESTED RECORD, LOAD INFORMATION
     C*
     C   12RQKEY         CHAIN     PRQ01                              40
     C  N12RQKY3         CHAIN     PRQ02                              40
     C     *IN40         IFEQ      '0'
     C   12              EXCEPT    PRQ01E
     C  N12              EXCEPT    PRQ02E
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           MOVE UA,2      SCMODE
     C                   MOVE      MD(3)         SCMODE
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   MOVE      RQREQ#        SCREQ#
     C                   MOVE      RQRCO         SCRCO
     C                   MOVE      RQRAMT        SCRAMT
     C*
     C                   Z-ADD     RQDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTRQ
     C*
     C                   MOVE      RQRCVD        SCRCVD
     C*
     C                   Z-ADD     RQDTRC        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTRC
     C*
     C                   MOVEL     RQCOM1        SCCOM1
     C                   MOVEL     RQCOM2        SCCOM2
     C                   MOVE      RQSTAT        SCSTAT
     C*
     C                   ELSE
     C*
     C     *IN09         IFEQ      '1'
     C                   MOVE      '1'           *IN22
     C                   GOTO      STRT
     C                   END
     C*
     C*** BEGIN CHG *** 01/28/98 **************************************
     C**********           MOVE UA,1      SCMODE
     C                   MOVE      MD(2)         SCMODE
     C*** END   CHG *** 01/28/98 **************************************
     C*
     C                   MOVE      *ZERO         SCREQ#
     C                   MOVE      *ZERO         SCRCO
     C                   MOVE      *ZERO         SCRAMT
     C                   MOVE      *ZERO         SCDTRC
     C                   MOVE      *ZERO         SCRCVD
     C                   MOVE      *ZERO         SCDTRQ
     C                   MOVEL     *BLANK        SCCOM1
     C                   MOVEL     *BLANK        SCCOM2
     C                   MOVE      'A'           SCSTAT
     C*
     C                   END
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C     SCR02T        TAG
     C     *IN09         CABEQ     '1'           INQ2
     C*
     C*   VALIDATE REINSURANCE CO#
     C*
     C                   Z-ADD     SCRCO         XXRCO
     C     RCKEY         CHAIN     PRC01                              20
     C     *IN20         CABEQ     '1'           STRT
     C*
     C*** Begin dlt *** 08/05/03 **************************************
     C*   REFUND AMOUNT REQUESTED MUST NOT BE LESS THAN ZERO
     C**********
     C********** SCRAMT    CABLT*ZERO     STRT         21
     C*** End dlt ***** 08/05/03 **************************************
     C*
     C*   VALIDATE DATE FILED
     C*
     C                   Z-ADD     SCDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCDTRQ
     C*
     C     SRERR         CABEQ     '1'           STRT                     22
     C*
     C*   REINSURANCE CO# MUST BE IN POLICY SET UP FOR THIS TIME PERIOD
     C*
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     1             X                 3 0
     C     XXRCO         LOOKUP    CO(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN23
     C                   GOTO      STRT
     C                   END
     C*
     C*  TOTAL OF ALL AMOUNTS REQUESTED CANT EXCEED TOTAL DUE
     C*
     C     SCASFL        IFEQ      'S'
     C*
     C                   Z-ADD     0             XXEXCS
     C*** BEGIN CHG *** 01/11/01 **************************************
     C******************** Z-ADD999999    XXDATE
     C                   Z-ADD     9999999       XXDATE
     C*** END CHG ***** 01/11/01 **************************************
     C                   MOVE      *ZEROS        X@CASE
     C  N79              MOVE      SCCASE        X@CASE
     C                   Z-ADD     SCMCLM        XXMCLM
     C     PFKY1         SETGT     PPF02
     C                   READP     PPF02                                  60
     C     *IN60         IFEQ      '0'
     C     PFCO#         ANDEQ     UPCO#
     C     PFFND         ANDEQ     UPFND
     C     PFFYR         ANDEQ     XXFYR
     C     PFCASE        ANDEQ     X@CASE
     C     PFMCLM        ANDEQ     XXMCLM
     C*
     C     PFCOMP        ADD       PFMED         XXTOTL           11 2
     C                   ADD       PFHOSP        XXTOTL
     C                   ADD       PFMISC        XXTOTL
     C                   ADD       PFATTY        XXTOTL
     C*** BEGIN ADD *** 10/24/96 **************************************
     C     RIEXPR        IFEQ      'F'
     C                   ADD       PFDEF         XXTOTL
     C                   END
     C*** END ADD ***** 10/24/96 **************************************
     C     XXTOTL        SUB       PFRETN        XXEXCS           11 2
     C*** BEGIN ADD *** 10/24/96 **************************************
     C     RIEXPR        IFNE      'F'
     C     PFDEF         SUB       PFFDEF        XXRDEF           11 2
     C                   ADD       XXRDEF        XXEXCS
     C                   END
     C*** END ADD ***** 10/24/96 **************************************
     C                   END
     C*
     C*  ACCUMULATE ALL REQUESTS
     C*
     C                   Z-ADD     0             XXRQST           11 2
     C     XXMCLM        IFEQ      0
     C*
     C     RQKY2         CHAIN     PRQ01                              60
     C     *IN60         DOWEQ     '0'
     C     RQSTAT        IFEQ      'A'
     C     RQREQ#        ANDNE     SCREQ#
     C                   ADD       RQRAMT        XXRQST
     C                   END
     C     RQKY2         READE     PRQ01                                  60
     C                   END
     C*
     C                   ELSE
     C*
     C     MCKEY         CHAIN     PMC01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      MCCASE        X@CASE
     C     RQKY2         CHAIN     PRQ01                              60
     C     *IN60         DOWEQ     '0'
     C     RQSTAT        IFEQ      'A'
     C     RQREQ#        ANDNE     SCREQ#
     C                   ADD       RQRAMT        XXRQST
     C                   END
     C     RQKY2         READE     PRQ01                                  60
     C                   END
     C     MCKEY         READE     PMC01                                  60
     C                   END
     C*
     C                   END
     C*
     C     XXEXCS        SUB       XXRQST        XXDIFF           11 2
     C     SCRAMT        IFGT      XXDIFF
     C                   MOVE      '1'           *IN24
     C                   GOTO      STRT
     C                   END
     C*
     C                   END
     C*
     C*  ALL EDITS HAVE BEEN PASSED
     C*
     C                   MOVE      SCFYR         XXFYR
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SCCASE        X@CASE
     C                   MOVE      SCREQ#        XXREQ#
     C*
     C*  IF SPECIFIC, GET NEXT REQ# ACCORDING TO FYR/CASE
     C*
     C     SCASFL        IFEQ      'S'
     C*
     C     RQKEY         CHAIN     PRQ01                              40
     C     *IN40         IFEQ      '1'
     C     RQKY2         SETGT     PRQ01
     C                   READP     PRQ01                                  41
     C     *IN41         IFEQ      '0'
     C     RQCO#         ANDEQ     UPCO#
     C     RQFND         ANDEQ     UPFND
     C     RQFYR         ANDEQ     XXFYR
     C     RQCASE        ANDEQ     X@CASE
     C     RQREQ#        ADD       1             RQREQ#
     C                   ELSE
     C                   Z-ADD     1             RQREQ#
     C                   END
     C                   ELSE
     C                   Z-ADD     SCREQ#        RQREQ#
     C                   END
     C*
     C*  IF AGGREGATE, GET NEXT REQ# ACCORDING TO EMP#/DIV/EFF DT
     C*
     C                   ELSE
     C*
     C                   MOVE      *ZEROS        X@CASE
     C     RQKY3         CHAIN     PRQ02                              40
     C     *IN40         IFEQ      '1'
     C     RQKY4         SETGT     PRQ02
     C                   READP     PRQ02                                  41
     C     *IN41         IFEQ      '0'
     C     RQCO#         ANDEQ     UPCO#
     C     RQFND         ANDEQ     UPFND
     C     RQEMP#        ANDEQ     XXEMP#
     C     RQDIV         ANDEQ     XXDIV
     C     RQEFDT        ANDEQ     XXEFDT
     C     RQREQ#        ADD       1             RQREQ#
     C                   ELSE
     C                   Z-ADD     1             RQREQ#
     C                   END
     C                   ELSE
     C                   Z-ADD     SCREQ#        RQREQ#
     C                   END
     C*
     C                   END
     C                   Z-ADD     UPCO#         RQCO#
     C                   Z-ADD     UPFND         RQFND
     C                   Z-ADD     XXFYR         RQFYR
     C                   MOVE      SCASFL        RQASFL
     C                   MOVE      X@CASE        RQCASE
     C                   Z-ADD     SCRCO         RQRCO
     C                   Z-ADD     SCRAMT        RQRAMT
     C*
     C                   Z-ADD     SCDTRQ        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RQDTRQ
     C*
     C                   Z-ADD     SCRCVD        RQRCVD
     C*
     C                   Z-ADD     SCDTRC        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RQDTRC
     C*
     C                   MOVEL     SCCOM1        RQCOM1
     C                   MOVEL     SCCOM2        RQCOM2
     C                   MOVE      SCSTAT        RQSTAT
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
     C*** BEGIN ADD *** JJJ 8/03/92
     C                   MOVE      SCMCLM        RQMCLM
     C*** END   ADD *** JJJ 8/03/92
     C*
     C     *IN40         IFEQ      '0'
     C   12              UPDATE    PRQ01
     C  N12              UPDATE    PRQ02
     C                   ELSE
     C   12              WRITE     PRQ01
     C  N12              WRITE     PRQ02
     C                   END
     C*
     C     INQ2          TAG
     C*
     C                   MOVEL     'SCR01'       Q2RECF
     C                   Z-ADD     0             SCFYR
     C                   MOVE      *BLANKS       SCASFL
     C                   MOVE      *ZEROS        SCCASE
     C                   MOVE      *ZEROS        SCREQ#
     C                   Z-ADD     0             XXREQ#
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
