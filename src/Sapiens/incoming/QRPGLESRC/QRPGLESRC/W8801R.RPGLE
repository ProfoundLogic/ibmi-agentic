     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8801R - SPECIFIC EXCESS CLAIM - PARTIAL FILING
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/11/16 C:062 Support: 40697(45)                             *
     H*                If no claim passed into program do not use     *
     H*                entry parm value for key list.                 *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/05/11  C:062  Support: 77062                               *
     H*                  Update amounts.                              *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 09/09/02  C:15  Support: WC065/34060                          *
     H*                 Chgd the length of QQFYR to 3 digits.         *
     H*                                                               *
     H* 11/15/99  C:118 SUPPORT: WA091/20425                          *
     H*                 Change prompt screen date from 'Run date' to  *
     H*                 'As of date'.  Change routing and report date *
     H*                 to report 'As of date'.                       *
     H*                                                               *
     H* 07/23/98  C:35  PROJECT: SUPPORT WA051                        *
     H*                 CORRECTED UNDICATOR USAGE                     *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  08/20/97 C:77 PROJECT WC731                                  *
     H*                EXCLUDE PAYMENT VALUE 1 CODES WITH A PLACEMENT *
     H*                VALUE OF 'M', 'D', 'K', 'N' FROM COMPENSATION  *
     H*                PAYMENT TOTAL.                                 *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     F*
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMRCL     IF   E           K DISK
     FWMRIL1    IF   E           K DISK
     FWMRQL     IF   E           K DISK
     FWDRVL     IF   E           K DISK
     FWMRQL2    IF   E           K DISK
     F                                     RENAME(PRQ01:PRQ02)
     FWDRVL5    IF   E           K DISK
     F                                     RENAME(PRV01:PRV02)
     FWDPAL5    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWDPFL     UF A E           K DISK
     FWDPFL1    UF A E           K DISK
     F                                     RENAME(PPF01:PPF02)
     FWAMCL     IF   E           K DISK
     FWAMCL1    IF   E           K DISK
     F                                     RENAME(PMC01:PMC02)
     FWTABL     IF   E           K DISK
     F                                     RENAME(PRC01:PRC02)
     FSTCTL1    IF   E           K DISK
     FSDCMLI    IF   E           K DISK
     F                                     RENAME(PCM01:PCM0I)
     FSDCMLD    IF A E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)

     D TOTL            S             11  2 DIM(6)                               TOTAL PMTS
     D MC              S             14    DIM(999)                             MULT CLAIM#
     D MA              S             66    DIM(999)                             MULT CLM AMT'S
     D RC              S              3  0 DIM(10)                              QUOTA CO#
     D SH              S             10    DIM(10)                              QUOTA SHORT NM
     D QT              S              5  2 DIM(10)                              QUOTA %
     D AM              S              9  2 DIM(10)                              QUOTA AMT
     D UNC             S              2    DIM(30)                              UNCOV. CODES
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D HDG             S             30    DIM(20) CTDATA PERRCD(1)             OUTPUT HEADING
     D*  Structure for DARTS field CMKEY
     D                 DS
     D  ZZKEY                  1     40
     D  ZZCO#                  1      3
     D  ZZFND                  4      6
     D  ZZFYR                  7      9
     D  ZZCASE                10     16
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RCZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
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
     D  LM                     1    110  2
     D                                     DIM(10)                              REIN LIMIT
     D  RILM01                 1     11  2
     D  RILM02                12     22  2
     D  RILM03                23     33  2
     D  RILM04                34     44  2
     D  RILM05                45     55  2
     D  RILM06                56     66  2
     D  RILM07                67     77  2
     D  RILM08                78     88  2
     D  RILM09                89     99  2
     D  RILM10               100    110  2
     D                 DS
     D  CV                     1     50  2
     D                                     DIM(10)                              COVERAGE %
     D  RICV01                 1      5  2
     D  RICV02                 6     10  2
     D  RICV03                11     15  2
     D  RICV04                16     20  2
     D  RICV05                21     25  2
     D  RICV06                26     30  2
     D  RICV07                31     35  2
     D  RICV08                36     40  2
     D  RICV09                41     45  2
     D  RICV10                46     50  2
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              CLAIM PMTS
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  PMT                    1     66  2
     D                                     DIM(6)                               PMT
     D  XXCOMP                 1     11  2
     D  XXMED                 12     22  2
     D  XXHOSP                23     33  2
     D  XXMISC                34     44  2
     D  XXATTY                45     55  2
     D  XXDEF                 56     66  2
     D  XXPAMT                 1     66
     D*
     D                 DS
     D  PFAMT                  1     66  2
     D                                     DIM(6)                               FILING PMTS
     D  PFCOMP                 1     11  2
     D  PFMED                 12     22  2
     D  PFHOSP                23     33  2
     D  PFMISC                34     44  2
     D  PFATTY                45     55  2
     D  PFDEF                 56     66  2
     D  XXFAMT                 1     66
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     IPCM0I
     I              CMKEY                       PIKEY
     I              CMSEQ#                      PISEQ#

     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE MULTIPLE CLAIM FILE
     C*
     C     MCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C*
     C     MCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXMCLM
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR THE PAYMENT FILE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE REIN REQUEST RECOVERY FILE
     C*
     C     RQKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXASFL
     C                   KFLD                    X@CASE
     C*
     C     RQKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXASFL
     C                   KFLD                    XXMCLM
     C*
     C*   KEY LIST FOR THE REIN RECOVERY RECEIVED FILE
     C*
     C     RVKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXRCO
     C                   KFLD                    XXASFL
     C                   KFLD                    X@CASE
     C*
     C     RVKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXRCO
     C                   KFLD                    XXASFL
     C                   KFLD                    XXMCLM
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
     C*   KEY LIST FOR PARTIAL FILING FILE
     C*
     C     PFKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXRNDT
     C                   KFLD                    X@CASE
     C*
     C     PFKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXRNDT
     C                   KFLD                    XXMCLM
     C*
     C*   KEY LIST FOR REINSURANCE COMPANY FILE
     C*
     C     RCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRCCO
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  DARTS File Key
     C*
     C     C2KY          KLIST
     C                   KFLD                    ZZKEY
     C                   KFLD                    XXSEQ             5 0
     C*
     C     C2KYD         KLIST
     C                   KFLD                    ZZKEY
     C                   KFLD                    XXRMCD
     C*
     C*   Key List for DARTS Remark Type file
     C*
     C     CTKEY         KLIST
     C                   KFLD                    XXRMCD
     C*
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    MCCASE        X@CASE
     C     *LIKE         DEFINE    MCCASE        SVCASE
     C     *LIKE         DEFINE    B2KEY         XXKEY
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDT          XXRNDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*** Begin chg *** 09/09/02 **************************************
     C******************** PARM           QQFYR   2
     C                   PARM                    QQFYR             3
     C*** End chg ***** 09/09/02 **************************************
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQCASE            7
     C                   PARM                    QQQUE            10
     C                   PARM                    XXRMCD
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQCASE        X@CASE
     C*
     C                   MOVE      QQRNDT        X1RNDT            8 0
     C*
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C***** BEGIN ADD **** 11/15/99 ***********************************
     C* FORMAT 'AS OF DATE' FOR OUTPUT
     C*
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2RNDT            6 0
     C***** END   ADD **** 11/15/99 ***********************************
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        XFNAME           57
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 11/15/99 ***********************************
     C*  LEFT JUSTIFY REPORT TITLE ON ROUTING PAGE
     C*
     C     ORTITL        IFNE      *BLANKS
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        A                 3 0
     C     A             IFLE      60
     C                   SUBST(P)  XXDATA:A      ORTITL
     C                   ENDIF                                                  A<=60
     C*
     C                   ENDIF                                                  ORTITL<>BLKS
     C***** END   ADD **** 11/15/99 ***********************************
     C*
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     C                   END
     C*
     C*   SET THE CONDITIONING INDICATORS & INITIALIZE
     C*
     C     XXCASE        COMP      *ZEROS                                 78
     C*
     C*   START READING CLAIM FILE
     C*
     C   78CMKY          CHAIN     PCM01                              60
     C  N78CMKY1         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     0             CO                             REIN CO#
     C                   Z-ADD     0             CV                             REIN COV %
     C                   Z-ADD     0             LM                             REIN LIMIT
     C                   MOVE      *BLANKS       UNC                            UNCOVERED CD
     C                   MOVE      CMCASE        XXCASE
     C                   MOVE      '0'           *IN12
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C*
     C*  GET REINSURANCE POLICY FILE TO DETERMINE COVERAGE DURING THIS
     C*  ACCIDENT DATE PERIOD. MUST BE THE SPECIFIC POLICY (RIASFL=S).
     C*  REINSURANC COMPANY# 999 REPRESENTS THE FUNDS PORTION OF COV.
     C*
     C                   MOVE      'S'           XXASFL            1
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
     C*
     C                   Z-ADD     0             XXFNDR           11 2          FUND RETEN.
     C                   Z-ADD     0             XXMCLM            7 0          MULT CLAIM#
     C                   Z-ADD     0             PMT                            PMT BREAKOUT
     C                   Z-ADD     0             TOTL                           TOT BREAKOUT
     C                   MOVE      *BLANKS       MC                             MULT CLM#
     C                   MOVE      *BLANKS       MA                             MULT CLM AMT
     C*
     C                   MOVEA     RIUNCV        UNC                            UNCOV. CODES
     C*
     C*  GET FUND RETENTION AMOUNT  (CO# = 999)
     C*
     C                   Z-ADD     1             X                 4 0
     C     *ZERO         LOOKUP    CO(X)                              60
     C     *IN60         IFEQ      '1'
     C     CO(X)         ANDEQ     999
     C                   Z-ADD     LM(X)         XXFNDR                         FUND RETENTION
     C                   END
     C*
     C*  CALCULATE TOTAL PAID TO DATE FROM CLAIM FILE, IF GREATER THAN
     C*  THE FUNDS PORTION(999) OF COVERAGE- CONTINUE, ELSE GET NEXT
     C*  CLAIM (UNLESS THERE HAS BEEN PREVIOUS FILING)
     C*
     C*  IF MULTIPLE CLAIM, ACCUMULATE ALL PAID TO DATE FOR ALL CLAIMS
     C*  LISTED UNDER THE MULTIPLE CLAIM
     C*
     C                   MOVE      '0'           *IN79
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C     MCKEY         CHAIN     PMC01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      MULT
     C                   MOVE      '1'           *IN79
     C                   ELSE
     C                   XFOOT     CMA           XXPTDT           11 2
     C                   END
     C*
     C     XXPTDT        IFGT      XXFNDR
     C*
     C                   MOVE      'N'           XXREV             1
     C*** Begin Add *** 08/05/11 **************************************
     C*
     C* Delete items in WDPFP
     C*
     C                   EXSR      DLTPFP
     C*** End   Add *** 08/05/11 **************************************
     C*
     C*  IF A PARTIAL FILING RECORD ALREADY EXISTS FOR CLAIM, THEN WE
     C*  ARE RERUNNING THE PARTIAL FILING, THEREFORE RECORD(S) WILL BE
     C*  UPDATED (XXREV = Y).
     C*                                (CO,FND,FYR,RNDT,CASE)
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C  N79PFKEY         CHAIN     PPF01                              60
     C*                                (CO,FND,FYR,RNDT,MULT. CASE#)
     C   79PFKEY1        CHAIN     PPF02                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     PFAMT         PMT
     C                   MOVE      'Y'           XXREV
     C  N79              EXCEPT    PPF01E
     C   79              EXCEPT    PPF02E
     C*
     C*  IF MULTIPLE CLAIM LOAD MULTIPLE CLAIM ARRAY FOR OUTPUT
     C*
     C     *IN79         IFEQ      '1'
     C                   Z-ADD     1             X
     C     PFKEY1        READE     PPF02                                  60
     C     *IN60         DOWEQ     '0'
     C     X             ANDLE     999
     C                   MOVE      PFCASE        MC(X)
     C                   MOVE      XXFAMT        MA(X)
     C                   ADD       1             X
     C                   EXCEPT    PPF02E
     C     PFKEY1        READE     PPF02                                  60
     C                   END
     C  N60              EXCEPT    PPF02E
     C                   END
     C*
     C                   ELSE
     C*
     C  N79              EXSR      GETPMT
     C   79              EXSR      MLTPMT
     C*
     C                   END
     C     XXHOSP        COMP      0                                      21
     C     XXMISC        COMP      0                                      22
     C     XXATTY        COMP      0                                      23
     C*
     C*  CALCULATE TOTAL PAID TO DATE  (LESS DEFENSE)
     C*
     C     XXCOMP        ADD       XXMED         XXPAID           11 2
     C                   ADD       XXHOSP        XXPAID
     C                   ADD       XXMISC        XXPAID
     C                   ADD       XXATTY        XXPAID
     C     RIEXPR        IFEQ      'F'
     C                   ADD       XXDEF         XXPAID
     C                   END
     C*
     C     XXDEF         IFEQ      0
     C     RIEXPR        OREQ      'F'
     C                   MOVE      '1'           *IN32
     C                   ELSE
     C                   MOVE      '0'           *IN32
     C                   END
     C*
     C*  CALCULATE TOTAL PREVIOUS FILING AMOUNTS
     C*
     C                   Z-ADD     0             XXTLFI           11 2
     C     *IN79         IFEQ      '0'
     C*
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C     RQKEY         CHAIN     PRQ01                              60
     C     *IN60         DOWEQ     '0'
     C     RQSTAT        IFEQ      'A'
     C                   ADD       RQRAMT        XXTLFI
     C                   END
     C     RQKEY         READE     PRQ01                                  60
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      XXCASE        SVCASE
     C     MCKY1         CHAIN     PMC02                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      MCCASE        X@CASE
     C     RQKEY         CHAIN     PRQ01                              60
     C     *IN60         DOWEQ     '0'
     C     RQSTAT        IFEQ      'A'
     C                   ADD       RQRAMT        XXTLFI
     C                   END
     C     RQKEY         READE     PRQ01                                  60
     C                   END
     C     MCKY1         READE     PMC02                                  60
     C                   END
     C                   MOVE      SVCASE        XXCASE
     C*
     C                   END
     C*
     C*  IF TOTAL PAID EXCEEDS RETENTION OR THERE HAS BEEN A PREVIOUS
     C*  FILING, WRITE OR UPDATE THE PARTIAL FILING INFO TO THE PARTIAL
     C*  FILING FILE.
     C*
     C     XXPAID        IFGT      XXFNDR
     C     XXTLFI        ORGT      0
     C                   EXSR      PRNT
     C                   EXSR      WRIT
     C*
     C*  IF MULTIPLE CLAIM, PRINT OUT EACH CLAIM & UPDATE FILE
     C*
     C                   MOVE      '1'           *IN12
     C   79              EXSR      PRTMLT
     C                   END
     C*
     C                   END                                                    XXPTDT > FND
     C*
     C                   END                                                    RI FILE
     C*
     C*  GET NEXT CLAIM
     C*
     C   78CMKY          READE     PCM01                                  60
     C  N78              MOVE      '1'           *IN60
     C                   END
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C*
     C     *IN62         IFEQ      *OFF
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*** Begin Add *** 08/05/11 **************************************
     C**************************************************************
     C*  DLTPFP - Delete WDPFP records so it can recalculate.
     C**************************************************************
     C     DLTPFP        BEGSR
     C*
     C     *IN79         IFEQ      *OFF
     C*
     C                   MOVE      QQCASE        X@CASE
     C*** Begin Add *** 05/11/16 ***********************************
     C                   If        qqCase = '0000000' Or qqCase = ' '
     C                   Move      CMcase        X@case
     C                   EndIf
     C*** End   Add *** 05/11/16 ***********************************
     C     PFKEY         DELETE    PPF01                              99
     C     *IN99         DOWEQ     *OFF
     C     PFKEY         DELETE    PPF01                              99
     C                   ENDDO
     C*
     C                   ELSE
     C*
     C                   MOVE      QQCASE        XXMCLM
     C*** Begin Add *** 05/11/16 ***********************************
     C                   If        xxMclm = 0
     C                   Eval      xxMclm = MCclm#
     C                   EndIf
     C*** End   Add *** 05/11/16 ***********************************
     C     PFKEY1        DELETE    PPF02                              99
     C     *IN99         DOWEQ     *OFF
     C     PFKEY1        DELETE    PPF02                              99
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*** End   Add *** 08/05/11 **************************************
     C**************************************************************
     C*    MULT SUBROUTINE - GET TOTAL PAID FROM CLAIM FILE FOR ALL
     C*              CLAIMS WITHIN THE MULTIPLE CLAIM. DETERMINE IF
     C*              THE MULTIPLE CLAIM HAS BEEN CALCULATED ALREADY,
     C*              IF SO RETURN WITH XXPTDT = 0.
     C**************************************************************
     C*
     C     MULT          BEGSR
     C*
     C                   Z-ADD     MCCLM#        XXMCLM            7 0
     C                   Z-ADD     0             XXPTDT
     C     MCKY1         CHAIN     PMC02                              60
     C*
     C* IF THE CASE IS THE FIRST CASE LISTED IN THE MULTIPLE CLAIM
     C* FILE, THEN THE TOTAL PAYMENT AMOUNT NEEDS TO BE CALCULATED.
     C*
     C     X@CASE        IFEQ      MCCASE
     C     *IN78         OREQ      '0'
     C*
     C                   MOVE      XXCASE        SVCASE
     C*
     C* ACCUMULATE TOTAL PAYMENTS FOR ALL CASES IN MULTIPLE CLAIM
     C*
     C     *IN60         DOWEQ     '0'
     C                   XFOOT     CMA           XXMPD            11 2
     C                   ADD       XXMPD         XXPTDT
     C     MCKY1         READE     PMC02                                  60
     C  N60              MOVE      MCCASE        XXCASE
     C  N60CMKY1         CHAIN     PCM01                              60
     C                   END
     C*
     C*  RESET POINTER IN CLAIM MASTER FILE TO CONTINUE PROCESSING
     C*
     C                   MOVE      SVCASE        XXCASE
     C     CMKY1         CHAIN     PCM01                              60
     C                   END
     C*
     C                   ENDSR
     C*
     C**************************************************************
     C*    MLTPMT SUBROUTINE - BREAK OUT PAYMENTS TO DATE FOR ALL
     C*              CLAIMS WITHIN A MULTIPLE CLAIM.
     C**************************************************************
     C*
     C     MLTPMT        BEGSR
     C*
     C                   Z-ADD     1             N                 4 0
     C                   MOVE      XXCASE        SVCASE
     C*                                (XXCO# XXFND XXFYR XXMCLM)
     C     MCKY1         CHAIN     PMC02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      MCCASE        XXCASE
     C                   EXSR      GETPMT
     C                   ADD       PMT           TOTL
     C     N             IFLE      999
     C                   MOVE      MCCASE        MC(N)
     C                   MOVE      XXPAMT        MA(N)
     C                   END
     C                   Z-ADD     0             PMT
     C                   ADD       1             N
     C*
     C     MCKY1         READE     PMC02                                  60
     C                   END
     C*
     C                   Z-ADD     TOTL          PMT
     C                   MOVE      SVCASE        XXCASE
     C*
     C                   ENDSR
     C*
     C**************************************************************
     C*    GETPMT SUBROUTINE - BREAK OUT PAYMENTS TO DATE
     C**************************************************************
     C*
     C     GETPMT        BEGSR
     C*
     C*  READ PAYMENT DETAIL FILE UP TO THE RUN DATE
     C*                                (CO FND FYR CASE)
     C     PAKY1         CHAIN     PPA02                              60
     C     *IN60         DOWEQ     '0'
     C     PATRDT        ANDLE     XXRNDT
     C*
     C*   CHECK UNCOVERED CODES TO DETERMINE IF INCLUDED IN PAYMENTS
     C*
     C                   Z-ADD     1             X
     C     PABR2         LOOKUP    UNC(X)                                 60
     C     *IN60         IFEQ      '0'
     C*
     C*   ACCUMULATE AMOUNTS FOR EACH PAYMENT CODE
     C*
     C                   Z-ADD     0             XXAMT
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C                   Z-ADD     PAAMT         XXAMT             9 2
     C                   END
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   Z-SUB     PAAMT         XXAMT             9 2
     C                   END
     C*
     C*  GET PAYMENT VALUE 1 CODE TO DETERMINE PMTS
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     PABR2         XXKEY
     C     TBKY          CHAIN     PB201                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       B2PLCD
     C                   END
     C*
     C* ACCUMULATE AMOUNT INTO COMPENSATION, MEDICAL, LEGAL, OR EXPENSE
     C*
     C*** BEGIN DEL *** 08/20/97 **************************************
     C***********PARETP*** IFEQ 'C'
     C******************** ADD  XXAMT     XXCOMP
     C******************** END
     C*** END DEL ***** 08/20/97 **************************************
     C*
     C     B2PLCD        IFEQ      'D'
     C                   ADD       XXAMT         XXHOSP
     C                   ELSE
     C     B2PLCD        IFEQ      'K'
     C                   ADD       XXAMT         XXMISC
     C                   ELSE
     C     B2PLCD        IFEQ      'M'
     C                   ADD       XXAMT         XXATTY
     C                   ELSE
     C     B2PLCD        IFEQ      'N'
     C                   ADD       XXAMT         XXDEF
     C                   ELSE
     C*** BEGIN ADD *** 08/20/97 **************************************
     C*  ADD TO COMP IF RESERVE TYPE = 'C' EXCLUDING THOSE WITH
     C*  PAYMENT VALUE 1 CODES WITH PLACEMENT VALUE = M, D, K, N
     C*
     C     PARETP        IFEQ      'C'
     C                   ADD       XXAMT         XXCOMP
     C                   ENDIF                                                  PARETP = 'C'
     C*** END ADD ***** 08/20/97 **************************************
     C     PARETP        IFEQ      'M'
     C                   ADD       XXAMT         XXMED
     C                   ELSE
     C     PARETP        IFEQ      'E'
     C                   ADD       XXAMT         XXDEF
     C                   ELSE
     C     PARETP        IFEQ      'L'
     C                   ADD       XXAMT         XXATTY
     C                   END                                                    PARETP = L
     C                   END                                                    PARETP = E
     C                   END                                                    PARETP = M
     C                   END                                                    PABR2  = N
     C                   END                                                           = M
     C                   END                                                           = K
     C                   END                                                           = D
     C*
     C                   END                                                    PABR2  UNC
     C*
     C     PAKY1         READE     PPA02                                  60
     C                   END                                                    PATRDT <=
     C*
     C                   ENDSR
     C**************************************************************
     C*   PRNT SUBROUTINE - OUTPUT PARTIAL FILING
     C**************************************************************
     C*
     C     PRNT          BEGSR
     C*
     C*  GET EMPLOYER NAME
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*  GET CLAIMANT NAME - IF NOT A MULTIPLE CLAIM
     C*
     C                   MOVE      *BLANKS       XXNAME           40
     C     *IN79         IFEQ      '0'
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME
     C                   END
     C                   ELSE
     C                   MOVEL     'MULTIPLE'    XXNAME
     C                   END
     C*
     C*  GET THE FIRST REINSURER
     C*
     C                   Z-ADD     1             X
     C     *ZERO         LOOKUP    CO(X)                              60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     11            X
     C                   ELSE
     C     *IN60         DOWEQ     '1'
     C     CO(X)         IFEQ      999
     C     CO(X)         OREQ      0
     C                   ADD       1             X
     C     X             COMP      11                                   60
     C                   ELSE
     C                   MOVE      '0'           *IN60
     C                   END
     C                   END
     C                   END
     C*
     C*  INTIALIZE TOTAL DEFENSE COVERED BY REINSURERS, AND TOTAL
     C*  FUND RETENTION AMOUNT. WE NEED TO CALCULATE TOTAL FUND
     C*  RETENTION & THE FUND'S PORTION OF DEFENSE PAYMENTS FOR THE
     C*  PARTIAL FILING FILE.
     C*
     C                   Z-ADD     0             XXTOTD           11 2          TOTAL DEF
     C                   Z-ADD     XXFNDR        XXRETN           11 2          FUND RETN
     C                   Z-ADD     0             XXPRV            11 2          PREV FILING
     C*
     C                   MOVE      '0'           *IN33
     C*
     C*  PRINT PARTIAL LISTING FOR ALL LAYERS OF REINSURERS COVERING
     C*  EXCESS
     C*
     C     *IN33         DOWEQ     '0'
     C     X             ANDLT     11
     C*
     C                   Z-ADD     0             XXOVER           11 2          OVER RETN
     C*
     C*  CALCULATE COVERAGE FOR EACH REINSURER
     C*
     C                   EXSR      CALC
     C*
     C*
     C*  IF TOTAL AMOUNT OVER RETENTION HAS BEEN COVERED, OR NO MORE
     C*  REINSURERS ARE LISTED, THEN STOP. EXCESS AMOUNT OVER WILL BE
     C*  ADDED TO THE FUND'S RETENTION AMOUNT.
     C*
     C     XXOVER        IFEQ      0
     C                   MOVE      '1'           *IN33
     C                   END
     C*
     C*  IF PRINTING CREDIT DUE, ACCUMULATE PREVIOUS FILINGS FOR EACH
     C*  LAYER & CONTINUE PROCESSING UNTIL ACCUM. TOTALS = ORIGINAL
     C*  CALCULATED TOTAL.
     C*
     C                   ADD       XXPREV        XXPRV            11 2
     C     XXPRV         IFNE      XXTLFI
     C     XXREIM        ANDLE     0
     C                   MOVE      '0'           *IN33
     C                   END
     C*
     C*  PRINT A PARTIAL LISTING FOR EACH COMPANY IF REIMBURSEMENT IS
     C*  NOT EQUAL TO ZERO.
     C*
     C     XXREIM        IFNE      0
     C*
     C     XXREIM        COMP      0                                    31
     C                   Z-ADD     1             Y                 2 0
     C     RC(Y)         DOWGT     0
     C*
     C     RC(Y)         IFNE      999
     C*
     C                   EXSR      ADDR
     C                   EXCEPT    HEAD
     C     RIEXPR        IFEQ      'F'
     C                   MOVE      '1'           *IN55
     C                   END
     C                   EXCEPT    DET
     C                   EXCEPT    DET2
     C                   MOVE      '0'           *IN55
     C*
     C*** BEGIN ADD *** 01/28/98 **************************************
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   MOVE      *ON           *IN62
     C*
     C*** END   ADD *** 01/28/98 **************************************
     C*
     C*
     C*
     C*  IF THERE IS A PREVIOUS FILING & REIMBURSEMENT DUE IS LESS THAN
     C*  0, THEN PRINT POSSIBLE CREDIT DUE
     C*
     C     XXPREV        IFGT      0
     C     XXREIM        ANDLT     0
     C*
     C*  READ REFUND RECIEVED & PRINT
     C*
     C                   Z-ADD     0             XXPTTL           11 2
     C                   Z-ADD     RC(Y)         XXRCO             3 0
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C*
     C*  IF MULT CLM (*IN79 = '1'), USE MULT CLM# TO GET RECOVERIES
     C*
     C  N79RVKEY         CHAIN     PRV01                              34
     C   79RVKY2         CHAIN     PRV02                              34
     C                   EXCEPT    PMTH
     C     *IN34         DOWEQ     '0'
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    PMTH
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   Z-ADD     RVTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPDAT            6 0
     C*
     C                   ADD       RVAMT         XXPTTL
     C                   EXCEPT    PYMT
     C*
     C  N79RVKEY         READE     PRV01                                  34
     C   79RVKY2         READE     PRV02                                  34
     C                   END
     C*
     C     XXPTTL        IFNE      0
     C                   Z-SUB     AM(Y)         XXWK92            9 2
     C     XXWK92        IFLT      XXPTTL
     C                   Z-ADD     XXWK92        XXCRED           11 2
     C                   ELSE
     C                   Z-ADD     XXPTTL        XXCRED
     C                   END
     C                   EXCEPT    PMTT
     C                   END
     C*
     C                   ELSE
     C*
     C  N32              EXCEPT    DEF
     C*
     C*
     C                   END
     C*
     C                   END                                                    RC,Y NE 999
     C*
     C                   ADD       1             Y
     C*
     C                   END
     C*
     C                   END                                                    REIM NE 0
     C*
     C*  ACCUMULATE FUND RETENTION FOR NEXT REINSURER.
     C*
     C*  IF THE NEXT LAYER OF REINSURANCE IS THE FUND & IT COVERS THE
     C*  AMT OVER PREVIOUS LIMIT, END PRINTING. IF IT DOES NOT COVER
     C*  THE AMOUNT OVER PREV LIMIT, THEN ACCUMULATE LIMIT TO THE FUND
     C*  RETENTION, SUBTRACT THE LIMIT FROM THE AMOUNT OVER, &
     C*  INCREMENT COVERAGE ARRAY INDEX.
     C*
     C                   ADD       XXEXCS        XXFNDR
     C*
     C     *IN33         IFEQ      '0'
     C     X             ANDLT     11
     C  N33CO(X)         DOWEQ     999
     C     X             ANDLT     11
     C*
     C                   ADD       LM(X)         XXFNDR
     C     XXPAID        IFLE      XXFNDR
     C                   MOVE      '1'           *IN33
     C                   ELSE
     C                   ADD       LM(X)         XXRETN
     C                   SUB       LM(X)         XXOVER
     C                   ADD       1             X
     C     X             COMP      11                                     33
     C*
     C*  GET NEXT REINSURANCE ENTRY
     C*
     C  N33*ZERO         LOOKUP    CO(X)                              60
     C  N60              MOVE      '1'           *IN33
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   END                                                    *IN33 ='0'
     C*
     C*  CALCULATE TOTAL FUND RETENTION, & FUND'S PORTION OF DEFENSE
     C*
     C                   ADD       XXOVER        XXRETN
     C     RIEXPR        IFEQ      'P'
     C     XXDEF         SUB       XXTOTD        XXFNDD           11 2
     C                   ELSE
     C                   Z-ADD     0             XXFNDD
     C                   END
     C*
     C                   ENDSR
     C*************************************************************
     C*
     C*   CALC SUBROUTINE - CALCULATE COVERAGE FOR EACH REINSURER
     C*
     C*************************************************************
     C*
     C     CALC          BEGSR
     C*
     C                   Z-ADD     0             XXFPCT            5 2          FUND % DEF
     C                   Z-ADD     0             XXEPCT            5 2          REIN % DEF
     C                   Z-ADD     0             XXFDEF           11 2          FUND AMT DEF
     C                   Z-ADD     0             XXEDEF           11 2          REIN AMT DEF
     C                   Z-ADD     0             XXTLDU           11 2          TOTAL DUE
     C                   MOVE      *BLANKS       SH                             SHORT NAME
     C                   Z-ADD     0             QT                             QUOTE %
     C                   Z-ADD     0             AM                             QUOTE AMT
     C                   Z-ADD     0             RC                             REIN CO#
     C                   Z-ADD     0             XXTPCT            5 2          TOTAL %
     C                   MOVEA     '00000000'    *IN(41)
     C                   MOVEA     '00'          *IN(49)
     C                   Z-ADD     1             Y                 2 0
     C*
     C*  CALC EXCESS - IF EXCESS IS LESS THAN 0, THEN THERE IS A
     C*  PREVIOUS FILING. REINITIALIZE TO 0 & CONTINUE.
     C*
     C     XXPAID        SUB       XXFNDR        XXEXCS           11 2
     C     XXEXCS        IFLT      0
     C                   Z-ADD     0             XXEXCS
     C                   END
     C*
     C*  LOAD OUTPUT ARRAY FOR REINSURERS COVERING THEIR PORTION OF
     C*  REIMBURSEMENT ACCORDING TO LIMIT IN POLICY FILE
     C*
     C     X             DOWLT     11
     C     XXTPCT        ANDNE     100
     C*
     C*  IF THERE IS A LAYER OF COVERAGE & LIMIT DOES NOT COVER EXCESS,
     C*  CALCULATE AMOUNT OVER EXCESS & REINITIALIZE EXCESS AMOUNT.
     C*
     C     LM(X)         IFLT      XXEXCS
     C     Y             ANDEQ     1
     C     XXEXCS        SUB       LM(X)         XXOVER
     C                   Z-ADD     LM(X)         XXEXCS
     C                   END
     C*
     C                   Z-ADD     CO(X)         XXRCCO            3 0
     C     RCKY          CHAIN     PRC01                              60
     C                   Z-ADD     CO(X)         RC(Y)
     C                   MOVE      RCSHNM        SH(Y)
     C                   Z-ADD     CV(X)         QT(Y)
     C                   XFOOT     QT            XXTPCT            5 2
     C     Y             ADD       40            X1                2 0
     C*
     C*  IF COMPANY IS NOT THE FUND, FLAG FOR OUTPUT OTHERWISE ADJUST
     C*  FUND'S RETENTION ACCORDING TO PERCENTAGE COVERED.
     C*
     C     CO(X)         IFNE      999
     C                   MOVE      '1'           *IN(X1)
     C                   ELSE
     C     CV(X)         MULT(H)   XXEXCS        XXADDL           11 2
     C                   ADD       XXADDL        XXRETN
     C                   ADD       XXADDL        XXFNDR
     C                   SUB       XXADDL        XXEXCS
     C                   END
     C*
     C*  GET NEXT REINSURANCE ENTRY
     C*
     C                   ADD       1             Y
     C                   ADD       1             X
     C     X             IFLT      11
     C     *ZERO         LOOKUP    CO(X)                              60
     C  N60              Z-ADD     11            X
     C                   END
     C*
     C                   END
     C*
     C*  CALCULATE PREVIOUS FILINGS FOR THIS LAYER OF REINSURERS
     C*
     C                   Z-ADD     0             XXPREV           11 2
     C                   MOVE      'S'           XXASFL            1
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C*
     C*  IF MULT CLM (*IN79 = '1'), USE MULT CLM# TO GET REQUESTS
     C*
     C  N79RQKEY         CHAIN     PRQ01                              60
      **** BEGIN CHG **** 07/23/98 *********************************************
     C******** 79RQKY2     CHAINPRQ02                34
     C   79RQKY2         CHAIN     PRQ02                              60
      **** END   CHG **** 07/23/98 *********************************************
     C     *IN60         DOWEQ     '0'
     C     RQSTAT        IFEQ      'A'
     C                   Z-ADD     1             Y
     C     RQRCO         LOOKUP    RC(Y)                                  60
     C     *IN60         IFEQ      '1'
     C                   ADD       RQRAMT        XXPREV
     C                   END
     C                   END
     C  N79RQKEY         READE     PRQ01                                  60
     C   79RQKY2         READE     PRQ02                                  60
     C                   END
     C*
     C*  CALC REIMBURSEMENT DUE
     C*
     C     XXOVER        ADD       XXFNDR        XXTOTR           11 2
     C     XXPAID        IFNE      0
     C     XXTOTR        DIV(H)    XXPAID        XXPRCT            5 4
     C                   ELSE
     C                   Z-ADD     0             XXPRCT
     C                   END
     C     XXPRCT        MULT      100           XXFPCT                         FUND % DEF
     C     100           SUB       XXFPCT        XXEPCT                         REIN % DEF
     C     RIEXPR        IFEQ      'P'
     C     XXDEF         MULT(H)   XXPRCT        XXFDEF                         FUND AMT DEF
     C     XXDEF         SUB       XXFDEF        XXEDEF                         REIN AMT DEF
     C     XXEXCS        ADD       XXEDEF        XXTLDU                         TOTAL DUE
     C                   ELSE
     C                   Z-ADD     0             XXFDEF
     C                   Z-ADD     0             XXEDEF
     C                   Z-ADD     XXEXCS        XXTLDU
     C                   END
     C     XXTLDU        SUB       XXPREV        XXREIM           11 2          REIMBURS DUE
     C                   ADD       XXEDEF        XXTOTD                         TTL REIN DEF
     C*
     C*  CALCULATE AMOUNT COVERED BY EACH REINSURER
     C*
     C                   Z-ADD     1             Y
     C     Y             DOWLT     11
     C     RC(Y)         ANDNE     0
     C     QT(Y)         DIV       100           XXWK54            5 4
     C     XXWK54        MULT(H)   XXREIM        AM(Y)
     C                   ADD       1             Y
     C                   END
     C*
     C*  MAKE SURE TOTAL OF ALL COVERAGE AMOUNTS = REIMBURSEMENT AMT
     C*
     C                   XFOOT     AM            XXTOTA           11 2
     C     XXREIM        SUB       XXTOTA        XXDIFF           11 2
     C                   ADD       XXDIFF        AM(1)
     C*
     C*  CALCULATE TOTAL PAID FOR FUND & REINSURER
     C*
     C     XXPAID        ADD       XXDEF         XXTOTL           11 2
     C     XXTOTR        ADD       XXFDEF        XXFTTL           11 2
     C     XXEXCS        ADD       XXEDEF        XXETTL           11 2
     C*
     C                   ENDSR
     C*************************************************************
     C*
     C*      ADDR SUBROUTINE - LOAD THE REINSURERS ADDRESS
     C*************************************************************
     C*
     C     ADDR          BEGSR
     C*
     C                   MOVE      *BLANKS       RCNAME
     C                   MOVE      *BLANKS       RCADR1
     C                   MOVE      *BLANKS       RCADR2
     C                   MOVE      *BLANKS       XXRCSZ           40
     C*
     C*  IF THERE IS NO STATE, THEN ADDRESS IS OUT OF COUNTRY - LOAD
     C*  CITY INTO CITY,STATE,ZIP.
     C*
     C                   Z-ADD     RC(Y)         XXRCCO
     C     RCKY          CHAIN     PRC01                              60
     C     *IN60         IFEQ      '0'
     C     RCST          IFEQ      *BLANKS
     C                   MOVEL     RCCTY         XXRCSZ
     C                   ELSE
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     RCST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZIP1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZIP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZIP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     RCCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXRCSZ           40
     C                   END
     C                   END
     C*
     C     RCADR2        COMP      *BLANKS                                30
     C*
     C                   ENDSR
     C*************************************************************
     C*
     C*      SUBROUTINE WRIT - WRITE TO THE PARIAL FILING FILE
     C*************************************************************
     C*
     C     WRIT          BEGSR
     C*
     C     XXREV         IFEQ      'Y'
     C     *IN79         IFEQ      '1'
     C     *IN12         ANDEQ     '0'
     C*           GET MULTIPLE CLAIM SUMMARY DETAIL RECORD
     C     PFKEY1        CHAIN     PPF02                              60
     C                   ELSE
     C*           UPDATE CLAIM DETAIL (INCLUDING MULTIPLE CLAIM DETAIL)
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C     PFKEY         CHAIN     PPF01                              60
     C                   END
     C                   END
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C*
     C                   Z-ADD     XXRETN        PFRETN
     C                   Z-ADD     XXFNDD        PFFDEF
     C                   MOVE      *BLANKS       PFSTAT
     C                   MOVE      Q1USER        PFUSER
     C                   MOVE      Q1PGM         PFPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PFTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PFDT
     C*
     C     XXREV         IFEQ      'N'
     C                   Z-ADD     XXCO#         PFCO#
     C                   Z-ADD     XXFND         PFFND
     C                   Z-ADD     XXFYR         PFFYR
     C                   MOVE      X@CASE        PFCASE
     C  N12
     CAN 79              MOVE      *ZEROS        PFCASE
     C                   Z-ADD     XXMCLM        PFMCLM
     C                   Z-ADD     CMEMP#        PFEMP#
     C                   Z-ADD     CMDIV         PFDIV
     C                   Z-ADD     XXRNDT        PFRNDT
     C                   Z-ADD     XXCOMP        PFCOMP
     C                   Z-ADD     XXMED         PFMED
     C                   Z-ADD     XXHOSP        PFHOSP
     C                   Z-ADD     XXMISC        PFMISC
     C                   Z-ADD     XXATTY        PFATTY
     C                   Z-ADD     XXDEF         PFDEF
     C                   MOVE      *BLANKS       PFPOL#
     C                   MOVE      *BLANKS       PFCLM#
     C                   MOVE      *BLANKS       PFCLID
     C                   MOVE      *BLANKS       PFCC
     C                   MOVE      *BLANKS       PFVAL
     C                   MOVE      *ZEROS        PFLOC#
     C                   MOVE      *ZEROS        PFPRM#
     C                   MOVE      *BLANKS       PFCOVF
     C                   MOVE      *BLANKS       PFSUBL
     C                   MOVE      *BLANKS       PFCLAS
     C                   MOVE      *BLANKS       PFCOVL
     C                   MOVE      *ZEROS        PFITM#
     C                   MOVE      *BLANKS       PFFRM
     C                   MOVE      *BLANKS       PFRTGP
     C                   Z-ADD     RIEFDT        PFEFDT
     C  N79              WRITE     PPF01
     C   79              WRITE     PPF02
     C                   EXSR      SRDART
     C                   ELSE
     C     *IN79         IFEQ      '1'
     C     *IN12         ANDEQ     '0'
     C*  UPDATE MULTIPLE CLAIM SUMMARY DETAIL RECORD
     C                   UPDATE    PPF02
     C                   ELSE
     C*  UPDATE CLAIM DETAIL (INCLUDING MULTIPLE CLAIM DETAIL)
     C                   UPDATE    PPF01
     C                   END
     C                   END
     C*
     C                   ENDSR
     C*************************************************************
     C*
     C*      PRTMLT SUBROUTINE - PRINT OUT A PARTIAL FILING FOR EACH
     C*      MULTIPLE CLAIM, LISTING THEIR PORTION OF PAYMENTS.  PRINT
     C*      FOR EACH REINSURER. UPDATE PARTIAL FILING FILE.
     C*************************************************************
     C     PRTMLT        BEGSR
     C*
     C                   Z-ADD     1             Y
     C                   MOVE      XXCASE        SVCASE
     C     RC(Y)         DOWGT     0
     C*
     C                   EXSR      ADDR
     C                   Z-ADD     1             X
     C     X             DOWLE     999
     C     MC(X)         ANDNE     *BLANKS
     C*
     C                   MOVE      MC(X)         XXCASE
     C     CMKY1         CHAIN     PCM01                              60
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        XXNAME
     C                   END
     C                   MOVE      MA(X)         XXPAMT
     C     XXCOMP        ADD       XXMED         XXPAID
     C                   ADD       XXHOSP        XXPAID
     C                   ADD       XXMISC        XXPAID
     C                   ADD       XXATTY        XXPAID
     C     RIEXPR        IFEQ      'F'
     C                   ADD       XXDEF         XXPAID
     C                   Z-ADD     XXPAID        XXMTTL
     C                   ELSE
     C     XXPAID        ADD       XXDEF         XXMTTL           11 2
     C                   END
     C     Y             IFEQ      1
     C                   EXSR      WRIT
     C                   END
     C                   EXCEPT    HEAD
     C     RIEXPR        IFEQ      'F'
     C                   MOVE      '1'           *IN55
     C                   END
     C                   EXCEPT    DET
     C                   EXCEPT    MTOT
     C                   MOVE      '0'           *IN55
     C                   ADD       1             X
     C*
     C                   END
     C*
     C                   ADD       1             Y
     C*
     C                   END
     C*
     C*  REPOSITION CLAIM FILE
     C*
     C                   MOVE      SVCASE        XXCASE
     C     CMKY1         CHAIN     PCM01                              60
     C*
     C                   ENDSR
     C*******************************************************
     C*  SRDART - Subroutine to write DARTS remark when a
     C*           claim goes to specific reinsurance
     C*******************************************************
     C     SRDART        BEGSR
     C*
     C*  If processing the Multiple Claim Case Zero record, skip sub.
     C*
     C     PFCASE        IFEQ      *ZERO
     C                   GOTO      EXTDRT
     C                   ENDIF
     C*
     C*  Determine if DARTS Remark for Specific Reinsurance exists.
     C*  Remark Code passed to program as PARM XXRMCD
     C*
     C                   MOVE      *BLANK        ZZKEY
     C                   MOVE      XXCO#         ZZCO#
     C                   MOVE      XXFND         ZZFND
     C                   MOVE      XXFYR         ZZFYR
     C                   MOVE      XXCASE        ZZCASE
     C                   MOVE      ZZKEY         CMKEY
     C*
     C     C2KYD         CHAIN     PCM03                              60
     C     *IN60         IFEQ      *OFF
     C                   GOTO      EXTDRT
     C                   ENDIF
     C*
     C*  Access the DARTS Remark Type file to obtain the description.
     C*
     C     CTKEY         CHAIN     PCT01                              61
     C*
     C*  Assign DARTS file fields
     C*
     C                   MOVE      *BLANK        CMCOMM
     C     *IN61         IFEQ      *OFF
     C                   MOVEL     CTDESC        CMCOMM
     C                   ENDIF
     C*
     C                   MOVE      ZZKEY         CMKEY
     C                   MOVE      *BLANK        CMCSID
     C                   MOVE      'SI'          CMCODE
     C                   MOVE      *BLANK        CMDCID
     C                   MOVEL     'W0070R'      CMPNAM
     C                   MOVE      *ZERO         CMSECL
     C                   MOVE      'N'           CMSBYL
     C                   MOVE      'A'           CMSTAT
     C                   MOVE      Q1USER        CMUSER
     C                   MOVE      Q1PGM         CMPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CMTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     C*
     C                   Z-ADD     CMDT          CMCDAT
     C                   Z-ADD     CMDT          CMTRDT
     C                   Z-ADD     DSTIME        CMTRTM
     C                   MOVE      Q1USER        CMIUSR
     C                   MOVE      *BLANK        CMFOLD
     C                   MOVE      'N'           CMDIFL
     C                   MOVE      *BLANK        CMAPID
     C                   MOVE      *BLANK        CMIDOC
     C                   MOVE      *BLANK        CMIQUE
     C                   MOVE      *BLANK        CMIMGF
     C                   MOVE      *BLANK        CMIPTH
     C                   MOVE      *ZERO         CMPIDX
     C                   MOVE      *BLANK        CMQPTY
     C                   MOVE      *BLANK        CMDCTP
     C                   MOVE      Q1PGM         CMIPGM
     C                   MOVE      *BLANK        CMSMCD
     C                   MOVE      *BLANK        CMFL01
     C                   MOVE      *BLANK        CMFL02
     C                   MOVE      *BLANK        CMFL03
     C                   MOVE      *BLANK        CMFL04
     C                   MOVE      *BLANK        CMFL05
     C                   MOVE      XXRMCD        CMRMCD
     C                   Z-ADD     CMDT          CMIDT
     C*
     C*  Get the Last Sequence Number Used
     C*
     C                   Z-ADD     *HIVAL        XXSEQ
     C     C2KY          SETGT     PCM0I
     C                   READP     PCM0I                                  60
     C     *IN60         IFEQ      '0'
     C     ZZKEY         ANDEQ     PIKEY
     C     PISEQ#        ADD       1             CMSEQ#
     C                   ELSE
     C                   Z-ADD     0             CMSEQ#
     C                   END
     C*
     C                   WRITE     PCM03                                61
     C*
     C     EXTDRT        TAG
     C*
     C                   ENDSR
     C*
     OPPF01     E            PPF01E
     OPPF02     E            PPF02E
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O*  MLS 1/14/88 ADD DISTRIBUTION INFROMATION WITH INDICATORS
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/14/88 ADD DISTRIBUTION INFROMATION WITH INDICATORS
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O*   MLS 1/14/88  ADD INDICATORS AND OUT QUEUE INFORMATION
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUTE       1  2
     O*
     O*** BEGIN CHG *** 01/28/97 **************************************
     O**********                         23 'REPORT SELECTIONS'
     O                                           24 'REPORT SELECTIONS:'
     O*** END   CHG *** 01/28/97 **************************************
     O*
     O          E            ROUTE          1
     O*
     O*** BEGIN CHG *** 01/28/97 **************************************
     O**********                         20 'FUND YEAR  :'
     O                                           20 'POLICY YEAR:'
     O*** END   CHG *** 01/28/97 **************************************
     O*
     O                       XXFYR         Z     25
     O          E            ROUTE          1
     O***** BEGIN CHG **** 11/15/99 ***********************************
     O**********                         20 'RUN DATE   :'
     O**********               DSDATE    31 '  /  /  '
     O                                           20 'AS OF DATE :'
     O                       X2RNDT              31 '  /  /  '
     O***** END   CHG **** 11/15/99 ***********************************
     O          E            ROUTE          1
     O                                           20 'CASE NUMBER:'
     O                       XXCASE              30
     O*
     O*** BEGIN CHG *** 01/28/97 **************************************
     O**********                         23 '(C) COPYRIGHT PROGRESSI'
     O**********                         46 'VE DATA SOLUTIONS. INC,'
     O**********                         51 '.1997'
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*** END   CHG *** 01/28/97 **************************************
     O*
     O          E            HEAD           2 02
     O                                           42 'SPECIFIC EXCESS CLAIM -'
     O                                           59 'PARTIAL FILING'
     O          E            HEAD           1
     O                                           12 'REINSURER:'
     O                       RCNAME              53
     O          E            HEAD           1
     O                       RCADR1              53
     O          E   N30      HEAD           1
     O                       RCADR2              53
     O          E            HEAD           1
     O                       XXRCSZ              53
     O                       XXFYR               63
     O                                           64 '-'
     O                       XXCASE              72
     O              N12 79                       64 ' '
     O              N12 79                       80 '                '
     O          E            HEAD           1
     O*
     O*** BEGIN CHG *** 01/28/97 **************************************
     O**********                         07 'FUND:'
     O                                           08 'GROUP:'
     O*** END   CHG *** 01/28/97 **************************************
     O*
     O                       XXFND         Z     17
     O                       XFNAME              80
     O          E            HEAD           1
     O                                           11 'EMPLOYER:'
     O                       EMEMP#        Z     21
     O                       EMDIV         Z     27
     O                       EMNAME              69
     O          E            HEAD           1
     O                                           11 'CLAIMANT:'
     O                       XXNAME              63
     O          E            HEAD           1
     O                                           19 'DATE OF ACCIDENT:'
     O                       XXACDT        Y     31
     O          E            HEAD           2
     O                  79                       49 '*MULTIPLE CLAIM#:'
     O                  79   XXMCLM        Z     57
     O          E            HEAD           2
     O                                           19 'PAYMENTS THROUGH:'
     O***** BEGIN CHG **** 11/15/99 ***********************************
     O**********               DSDATEY   28
     O                       X2RNDT        Y     30
     O***** END   CHG **** 11/15/99 ***********************************
     O          E            DET            2
     O                       HDG(1)              35
     O                       XXCOMP        J     52
     O          E            DET            2
     O                       HDG(2)              35
     O                       XXMED         J     52
     O          E   N21      DET            2
     O                       HDG(3)              35
     O                       XXHOSP        J     52
     O          E   N22      DET            2
     O                       HDG(4)              35
     O                       XXMISC        J     52
     O          E   N23      DET            2
     O                       HDG(5)              35
     O                       XXATTY        J     52
     O          E            DET            2
     O                       HDG(6)              35
     O                                           52 '_______________'
     O              N55      XXDEF         J     71
     O               55      XXDEF         J     52
     O          E    55      DET            1
     O                                           52 '_______________'
     O          E            DET            3
     O              N55      HDG(7)              35
     O               55      HDG(15)             35
     O                       XXPAID        J     52
     O          E            DET2           1
     O                       HDG(8)              35
     O                       XXFNDR        J     52
     O          E            DET2           0
     O                                           52 '_______________'
     O          E            DET2           1
     O              N33      HDG(9)              35
     O              N33      XXOVER        J     52
     O          E   N55      DET2           2
     O                       HDG(10)             35
     O                       XXEXCS        J     52
     O          E   N55      DET2           1
     O                       HDG(11)             35
     O                       XXEDEF        J     52
     O          E   N55      DET2           1
     O                                           52 '_______________'
     O          E            DET2           2
     O                       HDG(12)             35
     O                       XXTLDU        J     52
     O          E            DET2           1
     O                       HDG(13)             35
     O                       XXPREV        J     52
     O          E            DET2           1
     O                                           52 '_______________'
     O          E            DET2           3
     O                       HDG(14)             35
     O                  31                       37 '('
     O                       XXREIM        J     52
     O                  31                       52 ')'
     O          E            DET2           1
     O                                           14 'QUOTA SHARE:'
     O               41      SH(1)               25
     O               41      QT(1)               32 '  0.  %'
     O               41 31                       34 '('
     O               41      AM(1)         2     46
     O               41 31                       47 ')'
     O               42      SH(2)               58
     O               42      QT(2)               65 '  0.  %'
     O               42 31                       67 '('
     O               42      AM(2)         2     79
     O               42 31                       80 ')'
     O          E            DET2           1
     O               43      SH(3)               25
     O               43      QT(3)               32 '  0.  %'
     O               43 31                       34 '('
     O               43      AM(3)         2     46
     O               43 31                       47 ')'
     O               44      SH(4)               58
     O               44      QT(4)               65 '  0.  %'
     O               44 31                       67 '('
     O               44      AM(4)         2     79
     O               44 31                       80 ')'
     O          E            DET2           1
     O               45      SH(5)               25
     O               45      QT(5)               32 '  0.  %'
     O               45 31                       34 '('
     O               45      AM(5)         2     46
     O               45 31                       47 ')'
     O               46      SH(6)               58
     O               46      QT(6)               65 '  0.  %'
     O               46 31                       67 '('
     O               46      AM(6)         2     79
     O               46 31                       80 ')'
     O          E            DET2           1
     O               47      SH(7)               25
     O               47      QT(7)               32 '  0.  %'
     O               47 31                       34 '('
     O               47      AM(7)         2     46
     O               47 31                       47 ')'
     O               48      SH(8)               58
     O               48      QT(8)               65 '  0.  %'
     O               48 31                       67 '('
     O               48      AM(8)         2     79
     O               48 31                       80 ')'
     O          E            DET2           2
     O               49      SH(9)               25
     O               49      QT(9)               32 '  0.  %'
     O               49 31                       34 '('
     O               49      AM(9)         2     46
     O               49 31                       47 ')'
     O               50      SH(10)              58
     O               50      QT(10)              65 '  0.  %'
     O               50 31                       67 '('
     O               50      AM(10)        2     79
     O               50 31                       80 ')'
     O          E            MTOT        2  2
     O                                           30 'TOTAL PAID ON THIS CLAIM'
     O          E   N55      MTOT           1
     O                                           30 'PAID - LESS EXPENSE:'
     O                       XXPAID        J     52
     O          E   N55      MTOT           1
     O                                           23 'EXPENSE FEES:'
     O                       XXDEF         J     52
     O          E            MTOT           1
     O                                           52 '_______________'
     O          E            MTOT           1
     O                       XXMTTL        J     52
     O          E            DEF            1
     O                                           25 'EXPENSE FEES - PRORATED'
     O          E            DEF            2
     O*
     O*** BEGIN CHG *** 01/28/97 **************************************
     O**********                         44 'TOTAL            FUND'
     O                                           44 'TOTAL           GROUP'
     O*** END   CHG *** 01/28/97 **************************************
     O*
     O                                           69 'EXCESS'
     O          E            DEF            2
     O                                           17 'PAID (LESS DEF)'
     O                       XXPAID        J     32
     O                       XXTOTR        J     47
     O                                           48 '('
     O                       XXFPCT              55 '  0.  %'
     O                                           56 ')'
     O                       XXEXCS        J     71
     O                                           72 '('
     O                       XXEPCT              79 '  0.  %'
     O                                           80 ')'
     O          E            DEF            1
     O                                           15 'EXPENSE COSTS'
     O                       XXDEF         J     32
     O                       XXFDEF        J     47
     O                       XXEDEF        J     71
     O          E            DEF            1
     O                                           32 '______________'
     O                                           47 '______________'
     O                                           71 '______________'
     O          E            DEF            2
     O                                           12 'TOTAL PAID'
     O                       XXTOTL        J     32
     O                       XXFTTL        J     47
     O                       XXETTL        J     71
     O          E            PMTH           1
     O                  34                       23 'YOU HAVE NOT MADE ANY'
     O                  34                       46 'PAYMENTS ON THIS FILE,'
     O                  34                       70 'THEREFORE, NO CREDIT IS'
     O                  34                       75 'DUE.'
     O                 N34                       23 'YOU HAVE MADE THE'
     O                 N34                       43 'FOLLOWING PAYMENTS:'
     O          E            PYMT           1
     O                       XXPDAT        Y     19
     O                                           22 '('
     O                       RVAMT         2     34
     O                                           35 ')'
     O          E            PMTT           1
     O                                           35 '_______________'
     O          E            PMTT           2
     O                                           17 'TOTAL PAID:'
     O                                           20 '('
     O                       XXPTTL        2     34
     O                                           35 ')'
     O          E            PMTT           2
     O                                           42 'TOTAL AMOUNT OF CREDIT'
     O                                           46 'DUE'
     O                       RCSHNM              57
     O                                           58 ': ('
     O                       XXCRED        2     74
     O                                           75 ')'
     O*
     O*** BEGIN ADD *** 01/28/98 **************************************
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*** END   ADD *** 01/28/98 **************************************
     O*
**
TOTAL COMPENSATION
MEDICAL
HOSPITAL
MISCELLANEOUS
CLAIMANT'S ATTORNEY'S FEE
EXPENSE ATTORNEY'S FEE
TOTAL - LESS EXPENSE FEES
SELF INSURED RETENTION
AMOUNT OVER LIMIT
EXCESS - LESS EXPENSE FEES
PRORATA EXPENSE FEES
TOTAL DUE FROM EXC. CARRIER
LESS PREVIOUS FILINGS
REIMBURSEMENT DUE
TOTAL PAYMENTS
