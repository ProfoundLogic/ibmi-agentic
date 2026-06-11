     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8812R -AGGREGATE EXCESS CLAIM REPORT - REINSURANCE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/07/98 C:15 Project: WC770                                 *
     H*                If the 'policy end date' cannot be calculated, *
     H*                add 1 year to the 'policy start date' and load *
     H*                into to 'policy end date'.                     *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  07/19/96 C:36 PROJECT: WC538                                 *
     H*                DO NOT PRINT EXCESS CLAIMS IF NOT IN           *
     H*                PARTIAL FILING FILE (WDPFP)                    *
     H*                                                               *
     H*  08/08/95 C:36 PROJECT: WC434                                 *
     H*                ADD FLAG TO INCLUDE EXPENSE PAYMENTS NOT       *
     H*                COVERED BY SPECIFIC FILINGS                    *
     H*
     H*  09/09/93 C:15 PROJECT WC266                                  *
     H*                CHANGED CODE TO PRINT ON 80 COLUMNS.           *
     H*
     H*  08/04/92 JJJ  PROJECT: WC157                                 *
     H*                CORRECTED QUOTA SHARE AND CREDIT AMOUNTS       *
     H*                                                               *
     H*  07/07/92 JEI  PROJECT: WC157                                 *
     H*                CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR ALL  *
     H*                'BASE' FILES (XXCASE) - CASE NUMBER REMAINS    *
     H*                14 ALPHA FOR REINSURANCE FILES (X@CASE)        *
     H*                                                               *
     H*  04/03/92 LPK  PROJECT: MD027                                 *
     H*                CHANGED CASE NUMBER FROM 7 NUMERIC TO 14       *
     H*                ALPHA FIELD.                                   *
     H*                CHANGED CODE TO USE EMP/DIV INSTEAD OF FYR TO  *
     H*                FIND THE POLICY.                               *
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
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMRCL     IF   E           K DISK
     FWMRIL1    IF   E           K DISK
     FWMRQL1    IF   E           K DISK
     FWDLSL1    IF   E           K DISK
     FWXSEL1    IF   E           K DISK
     FWDRVL3    IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDPAL5    IF   E           K DISK
     FWDPFL2    IF   E           K DISK
     FWDPFL3    IF   E           K DISK
     F                                     RENAME(PPF01:PPF02)
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)

     D*
     D*
     D RC              S              3  0 DIM(10)                              QUOTA CO#
     D PL              S             15    DIM(10)                              QUOTA POL#
     D SH              S             10    DIM(10)                              QUOTA SHORT NM
     D QT              S              5  2 DIM(10)                              QUOTA %
     D AM              S              9  2 DIM(10)                              QUOTA AMT
     D*
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Aggregate Excess Cla-
     D                                     ims')
     D*** END   ADD *** 01/28/98 ***********************************************
     D*
     D*
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
     D  PO                     1    150
     D                                     DIM(10)                              REIN POL#
     D  RIPO01                 1     15
     D  RIPO02                16     30
     D  RIPO03                31     45
     D  RIPO04                46     60
     D  RIPO05                61     75
     D  RIPO06                76     90
     D  RIPO07                91    105
     D  RIPO08               106    120
     D  RIPO09               121    135
     D  RIPO10               136    150
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
     D PSDS          ESDS                  EXTNAME(SZQ1P)

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
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR REINSURANCE POLICY FILE
     C*
     C     RIKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL
     C                   KFLD                    XXRIDT
     C*
     C     RIKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL
     C*
     C*   KEY LIST FOR THE LOSS FILE
     C*
     C     LSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXRNDT
     C*
     C*   KEY LIST FOR THE SPECIFIC EXCESS CLAIM FILE
     C*
     C     SEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXRNDT
     C*
     C*   KEY LIST FOR THE PAYMENT FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    PACASE
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR REINSURANCE COMPANY FILE
     C*
     C     RCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRCCO
     C*
     C*   KEY LIST FOR REINSURANCE REQUEST RECOVERY FILE
     C*
     C     RQKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXASFL
     C*
     C*   KEY LIST FOR THE REIN RECOVERY RECEIVED FILE
     C*
     C     RVKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXRCO
     C                   KFLD                    XXASFL
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR PARTIAL FILING FILE
     C*
     C     PFKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE           14
     C                   KFLD                    XXRNDT
     C*
     C     PFKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXMCLM            7 0
     C                   KFLD                    X@CASE
     C                   KFLD                    XXRNDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQRNDT            8            RUN DATE
     C                   PARM                    QQQUE            10
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C*
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      QQRNDT        X1RNDT            8 0
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C******* LIKE DEFINITIONS *******
     C*
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMDT          XXRIDT
     C     *LIKE         DEFINE    CMDT          XXEFDT
     C     *LIKE         DEFINE    CMDT          XXRNDT
     C*
     C********************************
     C                   EXSR      SR1ST
     C*
     C*  GET AGGREGATE REINSURANCE POLICY FILE
     C*                               (CO# FND FYR XXASFL XXRNDT)
     C                   MOVE      'A'           XXASFL            1
     C                   Z-ADD     XXEFDT        XXRIDT
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  GET LOSS INFORMATION
     C*
     C     LSKEY         CHAIN     PLS01                              60
     C*
     C     RIATTP        DIV(H)    100           XXPCT             5 4
     C     XXPCT         MULT      LSEAPR        XXFPCT           11 2
     C     LSPREV        ADD       LSPMTS        XXPDTD           11 2
     C                   SUB       LSRECV        XXPDTD
     C                   ADD       LSADJ         XXPDTD
     C     XXPDTD        ADD       LSSDF         XXPAID           11 2
     C*
     C     XXPAID        SUB       LSSPPD        XXFAPP           11 2
     C*
     C*  INCLUDE EXPENSE PAYMENTS NOT COVERED BY SPECIFIC FILINGS
     C*
     C     RIEXPF        IFEQ      'Y'
     C*
     C     SEKEY         CHAIN     PSE01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  READ PAYMENT DETAIL FILE UP TO THE RUN DATE
     C*                                (CO FND FYR CASE)
     C                   Z-ADD     0             XXDEF            11 2
     C                   MOVE      SECASE        PACASE
     C                   Z-ADD     SEFYR         XXFYR
     C     PAKEY         CHAIN     PPA01                              75
     C     *IN75         DOWEQ     '0'
     C     PATRDT        ANDLE     XXRNDT
     C*
     C     PARETP        IFEQ      'E'
     C     PARETP        OREQ      'L'
     C                   ADD       PAAMT         XXDEF
     C                   END
     C*
     C     PAKEY         READE     PPA01                                  75
     C                   END                                                    PATRDT <=
     C*
     C*  CALCULATE EXPENSE PORTION NOT COVERED BY SPECIFIC FILINGS
     C*                                (CO FND FYR CASE)
     C     SEPAID        SUB       XXDEF         XXPDLD           11 2
     C     XXPDLD        IFNE      0
     C     SERETN        DIV(H)    XXPDLD        XXPRCT            5 4
     C                   ELSE
     C                   Z-ADD     0             XXPRCT
     C                   END
     C     XXDEF         MULT(H)   XXPRCT        XXFDEF           11 2
     C     XXDEF         SUB       XXFDEF        XXEDEF           11 2
     C     LSSPPD        SUB       XXEDEF        LSSPPD
     C*
     C     SEKEY         READE     PSE01                                  60
     C                   END
     C*
     C     XXPAID        SUB       LSSPPD        XXFAPP           11 2
     C*
     C                   END
     C*
     C*  GET TOTAL RECOVERIES REQUESTED
     C*
     C                   Z-ADD     0             XXTLRQ           11 2
     C     RQKEY         CHAIN     PRQ01                              60
     C     *IN60         DOWEQ     '0'
     C     RQSTAT        IFEQ      'A'
     C                   ADD       RQRAMT        XXTLRQ
     C                   END
     C     RQKEY         READE     PRQ01                                  60
     C                   END
     C*
     C*  GET THE FIRST REINSURER
     C*
     C                   Z-ADD     1             X                 3 0
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
     C*  PRINT PARTIAL LISTING FOR ALL LAYERS OF REINSURERS COVERING
     C*  EXCESS
     C*
     C                   Z-ADD     0             XXPRV            11 2          REQUESTED AM
     C                   MOVE      '0'           *IN33
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
     C*  IF TOTAL AMOUNT OVER RETENTION HAS BEEN COVERED, OR NO MORE
     C*  REINSURERS ARE LISTED, THEN STOP. EXCESS AMOUNT OVER WILL BE
     C*  ADDED TO THE FUND'S RETENTION AMOUNT.
     C*
     C     XXOVER        IFEQ      0
     C                   MOVE      '1'           *IN33
     C                   END
     C*
     C     XXOVER        COMP      0                                  35
     C*
     C*  IF PRINTING CREDIT DUE, ACCUMULATE REQUESTS FOR EACH
     C*  LAYER & CONTINUE PROCESSING UNTIL ACCUM. TOTALS = ORIGINAL
     C*  CALCULATED TOTAL.
     C*
     C                   ADD       XXRQST        XXPRV            11 2
     C     XXPRV         IFNE      XXTLRQ
     C     XXANET        ANDLE     0
     C                   MOVE      '0'           *IN33
     C                   END
     C*
     C*  PRINT A PARTIAL LISTING FOR EACH COMPANY
     C*
     C     XXANET        IFNE      0
     C*
     C                   Z-ADD     1             Y                 2 0
     C     RC(Y)         DOWGT     0
     C     RC(Y)         IFNE      999
     C                   EXSR      PRNT
     C                   END
     C                   ADD       1             Y
     C                   END
     C*
     C                   END
     C*
     C                   ADD       XXAGGR        XXFPCT
     C*
     C*  GET NEXT REINSURER
     C*
     C     *IN33         IFEQ      '0'
     C     X             ANDLT     11
     C  N33CO(X)         DOWEQ     999
     C     X             ANDLT     11
     C*
     C                   ADD       LM(X)         XXFPCT
     C     XXAGGR        IFLE      XXFPCT
     C                   MOVE      '1'           *IN33
     C                   ELSE
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
     C                   END                                                    RI CHAIN
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
     C**************************************************************
     C*    SR1ST SUBROUTINE - GET INITIAL FILES
     C*
     C**************************************************************
     C     SR1ST         BEGSR
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
     C*
     C*  GET THE ENDING DATE OF THIS POLICY PERIOD.  THIS WILL BE USED
     C*  TO VERIFY IF A CLAIM SHOULD BE INCLUDE IN THIS POLICY FOR A
     C*  SPECIFIC EMPLOYER
     C*
     C                   MOVE      QQEFDT        XXSTPO            8 0
     C                   Z-ADD     0             XXENPO            8 0
     C                   MOVE      'A'           XXASFL
     C                   Z-ADD     XXEFDT        XXRIDT
     C*                    (XXCO#/XXFND/XXEMP#/XXDIV/XXASFL/XXRIDT)
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '0'
     C     RIKY3         READE     PRI01                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     RIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXENPO
     C                   END
     C                   END
     C*
     C*** BEGIN ADD *** 04/07/98 ***********************************************
     C*  If the 'policy end date' was not determined, add 1 year to the
     C*    'policy start date' and load into the 'policy end date'.
     C*
     C     XXENPO        IFEQ      0
     C     XXSTPO        ADD       1             XXENPO
     C                   END
     C*** END ADD ***** 04/07/98 ***********************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C*
     C*  CENTER PROGRAM TITLE
     C*
     C                   MOVEL(P)  OPTNM         XXTITL           40
     C*
     C                   MOVEL(P)  XXTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        XXTITL
     C*                                                               *
     C*** END   ADD *** 01/28/98 ***********************************************
     C*
      ***** Beg Del ***** 08/26/14 *************************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     C                   END
     C*
     C*
     C                   ENDSR
     C**************************************************************
     C*    PRNT SUBROUTINE - PRINT AGGREGATE EXCESS CLAIM
     C*
     C**************************************************************
     C     PRNT          BEGSR
     C*
     C*   GET EMPLOYER NAME
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   EXSR      ADDR
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C*
     C*  GET ALL SPECIFIC EXCESS CLAIMS AND PRINT. DON'T PRINT DETAIL
     C*  MULTIPLE CLAIM INFO
     C*
     C     SEKEY         CHAIN     PSE01                              60
     C     *IN60         DOWEQ     '0'
     C*  DO NOT INCLUDE EXCESS CLAIMS IF NOT IN THE PARTIAL FILING
     C*  FILE
     C*
     C                   Z-ADD     SEFYR         XXFYR             3 0
     C     SEMCLM        IFGT      0
     C                   MOVE      *ZEROS        X@CASE
     C                   Z-ADD     SEMCLM        XXMCLM
     C     PFKEY1        CHAIN     PPF02                              78
     C     *IN78         IFEQ      *ON
     C                   GOTO      SKIP
     C                   END
     C                   ELSE
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SECASE        X@CASE
     C     PFKEY         CHAIN     PPF01                              78
     C     *IN78         IFEQ      *ON
     C                   GOTO      SKIP
     C                   END
     C                   END
     C*
     C     SECASE        IFEQ      *ZEROS
     C     SEMCLM        OREQ      0
     C*
     C                   MOVE      *BLANKS       XXNAME           25
     C                   Z-ADD     SEFYR         XXFYR             3 0
     C     SEPAID        SUB       SERETN        XXSPEC           11 2
     C     XXSPEC        IFGT      0
     C     SEMCLM        COMP      0                                  79
     C     *IN79         IFEQ      '1'
     C                   MOVE      SEMCLM        XXCASE
     C                   MOVEL     'MULTIPLE'    XXNAME
     C                   ELSE
     C                   MOVE      SECASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              60
     C*  GET THE CLAIMANT'S NAME
     C     CDKEY         CHAIN     PCD01                              60
     C                   MOVE      *BLANKS       QQLAST
     C                   MOVE      *BLANKS       QQFRST
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME
     C                   END
     C*
     C*  INCLUDE EXPENSE PAYMENTS NOT COVERED BY SPECIFIC FILINGS
     C*
     C     RIEXPF        IFEQ      'Y'
     C*
     C*  READ PAYMENT DETAIL FILE UP TO THE RUN DATE
     C*                                (CO FND FYR CASE)
     C                   Z-ADD     0             XXDEF            11 2
     C                   MOVE      SECASE        PACASE
     C     PAKEY         CHAIN     PPA01                              75
     C     *IN75         DOWEQ     '0'
     C     PATRDT        ANDLE     XXRNDT
     C*
     C     PARETP        IFEQ      'E'
     C     PARETP        OREQ      'L'
     C                   ADD       PAAMT         XXDEF
     C                   END
     C*
     C     PAKEY         READE     PPA01                                  75
     C                   END                                                    PATRDT <=
     C*
     C*  CALCULATE EXPENSE PROTION NOT COVERED BY SPECIFIC FILINGS
     C*                                (CO FND FYR CASE)
     C     SEPAID        SUB       XXDEF         XXPDLD           11 2
     C     XXPDLD        IFNE      0
     C     SERETN        DIV(H)    XXPDLD        XXPRCT            5 4
     C                   ELSE
     C                   Z-ADD     0             XXPRCT
     C                   END
     C     XXDEF         MULT(H)   XXPRCT        XXFDEF           11 2
     C     XXDEF         SUB       XXFDEF        XXEDEF           11 2
     C     XXSPEC        SUB       XXEDEF        XXSPEC
     C*
     C                   END                                                    PATRDT <=
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    EXCS
     C*
     C                   END
     C                   END
     C     SKIP          TAG
     C     SEKEY         READE     PSE01                                  60
     C                   END
     C*
     C                   EXCEPT    DET2
     C*
     C*  IF NET AGGREGATE IS < 0, THEN PRINT POSSIBLE CREDIT DUE
     C*
     C     XXRQST        IFGT      0
     C     XXANET        ANDLT     0
     C*
     C*  READ REFUND RECIEVED & PRINT
     C*
     C                   Z-ADD     0             XXPTTL           11 2
     C                   Z-ADD     RC(Y)         XXRCO             3 0
     C     RVKEY         CHAIN     PRV01                              34
     C                   EXCEPT    PMTH
     C     *IN34         DOWEQ     '0'
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    PMTH
     C                   MOVE      '0'           *INOA
     C                   END
     C                   Z-ADD     RVTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPDAT            8 0
     C                   ADD       RVAMT         XXPTTL
     C                   EXCEPT    PYMT
     C*
     C     RVKEY         READE     PRV01                                  34
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
     C                   MOVE      *BLANKS       SH                             SHORT NAME
     C                   MOVE      *BLANKS       PL                             QUOTE POL#
     C                   Z-ADD     0             QT                             QUOTE %
     C                   Z-ADD     0             AM                             QUOTE AMT
     C                   Z-ADD     0             RC                             REIN CO#
     C                   Z-ADD     0             XXTPCT            5 2          TOTAL %
     C                   MOVEA     '00000000'    *IN(41)
     C                   MOVEA     '00'          *IN(49)
     C                   Z-ADD     1             Y                 2 0
     C*
     C*  CALCULATE NET AGGREGATE CLAIM
     C*
     C     XXFAPP        SUB       XXFPCT        XXAGGR           11 2
     C*
     C*  CALCULATE PORTION OF COVERAGE ON REIMBURSEMENT FOR ALL
     C*  REINSURERS & PRINT A PARTIAL LISTING FOR EACH LAYER OF COV.
     C*
     C     X             DOWLT     11
     C     XXTPCT        ANDNE     100
     C*
     C*  IF THERE IS A LAYER OF COVERAGE & LIMIT DOES NOT COVER EXCESS,
     C*  CALCULATE AMOUNT OVER EXCESS & REINITIALIZE EXCESS AMOUNT.
     C*
     C     LM(X)         IFLT      XXAGGR
     C     Y             ANDEQ     1
     C     XXAGGR        SUB       LM(X)         XXOVER
     C                   Z-ADD     LM(X)         XXAGGR
     C                   END
     C*
     C                   Z-ADD     CO(X)         XXRCCO            3 0
     C     RCKEY         CHAIN     PRC01                              60
     C                   Z-ADD     CO(X)         RC(Y)
     C                   MOVE      RCSHNM        SH(Y)
     C                   MOVE      PO(X)         PL(Y)
     C                   Z-ADD     CV(X)         QT(Y)
     C                   XFOOT     QT            XXTPCT            5 2
     C     Y             ADD       40            X1                2 0
     C*
     C*  IF COMPANY IS NOT FUND FLAG FOR OUTPUT, OTHERWISE ADJUST
     C*  FUND'S PORTION ACCORDING TO PERCENTAGE COVERED.
     C*
     C     CO(X)         IFNE      999
     C                   MOVE      '1'           *IN(X1)
     C                   ELSE
     C     CV(X)         MULT      XXAGGR        XXADDL           11 2
     C                   ADD       XXADDL        XXFPCT
     C                   SUB       XXADDL        XXAGGR
     C                   END
     C*
     C                   ADD       1             Y
     C                   ADD       1             X
     C*
     C     X             IFLT      11
     C     *ZERO         LOOKUP    CO(X)                              60
     C  N60              Z-ADD     11            X
     C                   END
     C*
     C                   END
     C*
     C*  CALCULATE PREVIOUS FILINGS FOR THIS LAYER OF REINSURERS
     C*
     C                   Z-ADD     0             XXRQST           11 2
     C                   MOVE      'A'           XXASFL            1
     C     RQKEY         CHAIN     PRQ01                              60
     C     *IN60         DOWEQ     '0'
     C     RQSTAT        IFEQ      'A'
     C                   Z-ADD     1             Y
     C     RQRCO         LOOKUP    RC(Y)                                  60
     C     *IN60         IFEQ      '1'
     C                   ADD       RQRAMT        XXRQST
     C                   END
     C                   END
     C     RQKEY         READE     PRQ01                                  60
     C                   END
     C*
     C*  CALCULATE NET AGGREGATE CLAIM
     C*
     C     XXAGGR        SUB       XXRQST        XXANET           11 2
     C     XXANET        COMP      0                                    20
     C     *IN20         IFEQ      '1'
     C     XXRQST        ANDLE     0
     C                   MOVE      '1'           *IN21
     C                   END
     C*
     C*  CALCULATE AMOUNT COVERED BY EACH REINSURER
     C*
     C                   Z-ADD     1             Y
     C     Y             DOWLT     11
     C     RC(Y)         ANDNE     0
     C     QT(Y)         DIV       100           XXWK54            5 4
     C     XXWK54        MULT(H)   XXANET        AM(Y)
     C                   ADD       1             Y
     C                   END
     C*
     C*  MAKE SURE TOTAL OF ALL COVERAGE AMOUNTS = NET AGGREGATE
     C*
     C                   XFOOT     AM            XXTOTA           11 2
     C     XXANET        SUB       XXTOTA        XXDIFF           11 2
     C                   ADD       XXDIFF        AM(1)
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
     C     RCKEY         CHAIN     PRC01                              60
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
     O*** CHG 01/28/98 ***     ORTITL    80
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********             XXTITL              60
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
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUTE       1  2
     O                                           23 'REPORT SELECTIONS'
     O          E            ROUTE          1
     O*** CHG 01/28/98 ***               23 'EMPLOYER #    :'
     O                                           23 'EMPLOYER / DIV:'
     O*** CHG 01/28/98 ***     XXEMP#Z   29
     O                       XXEMP#        Z     34
     O*** CHG 01/28/98 ***     XXDIV Z   33
     O                       XXDIV         Z     40
     O          E            ROUTE          1
     O                                           23 'EFFECTIVE DATE:'
     O                       X1EFDT              34 '  /  /    '
     O          E            ROUTE          1
     O                                           23 'RUN DATE  . . :'
     O                       X1RNDT              34 '  /  /    '
     O*
     O*** CHG 01/28/98 ***               23 '(C) COPYRIGHT PROGRESSI'
     O*** CHG 01/28/98 ***               46 'VE DATA SOLUTIONS. INC,'
     O*** CHG 01/28/98 ***               51 '.1997'
     O*
     O*** BEGIN CHG *** 01/28/10 **************************************
     O***     E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           2 02
     O                                           43 'AGGREGATE EXCESS CLAIM'
     O          E            HEAD           1
     O                                           12 'REINSURER:'
     O                       RCNAME              53
     O          E            HEAD           1
     O                       RCADR1              53
     O          E   N30      HEAD           1
     O                       RCADR2              53
     O          E            HEAD           2
     O                       XXRCSZ              53
     O          E            HEAD           1
     O*** CHG 01/28/98 ***               07 'FUND:'
     O                                           08 'GROUP:'
     O                       FNFND         Z     15
     O                       FNNAME              80
     O          E            HEAD           1
     O                                           10 'POLICY#:'
     O                       PL(Y)               35
     O          E            HEAD           1
     O                                            8 'EMPLR:'
     O*** CHG 01/28/98 ***     XXEMP#Z   15
     O                       XXEMP#        Z     18
     O*** CHG 01/28/98 ***     XXDIV Z   19
     O                       XXDIV         Z     24
     O*** CHG 01/28/98 ***     EMNAME    60
     O                       EMNAME              66
     O          E            HEAD           2
     O*** CHG 01/28/98 ***               34 'POLICY PERIOD:'
     O                                           16 'POLICY PERIOD:'
     O*** CHG 01/28/98 ***     XXSTPOY   43
     O                       XXSTPO              27 '  /  /    '
     O*** CHG 01/28/98 ***               45 '-'
     O                                           29 '-'
     O*** CHG 01/28/98 ***     XXENPOY   54
     O                       XXENPO              40 '  /  /    '
     O                                           65 'RUN DATE:'
     O                       X1RNDT              76 '  /  /    '
     O          E            DET1           2
     O                                           23 'EARNED NORMAL PREMIUM'
     O                       LSEAPR        J     74
     O          E            DET1           2
     O                                           22 'CLAIMS FUND  XXX.XX%'
     O                       RIATTP        1     21
     O                       XXFPCT        J     74
     O          E            DET1           1
     O                                           22 'CLAIMS PAID THROUGH:'
     O                       X1RNDT              33 '  /  /    '
     O                       XXPAID        J     74
     O          E            DET1           2
     O                                           23 '(INCLUDES ANY SPECIAL'
     O                                           46 'DISABILITY ASSESSMENT)'
     O          E            DET1           2
     O                                           24 'LESS CLAIMS PAID APPLI'
     O                                           42 'CABLE TO SPECIFIC:'
     O          E            EXCS           1
     O                       SEFYR         Z     04
     O                                           05 '-'
     O                       XXCASE              12
     O                       XXNAME              38
     O                       XXSPEC        J     56
     O          E            DET2           1
     O                       LSSPPD        J     74
     O          E            DET2           1
     O                                           74 '_______________'
     O          E            DET2           2
     O                                           24 'CLAIMS PAID APPLICABLE'
     O                                           39 'TO CLAIMS FUND'
     O                       XXFAPP        J     74
     O          E            DET2           1
     O                                           18 'LESS CLAIMS FUND'
     O                       XXFPCT        J     74
     O          E            DET2           0
     O               35                          19 'AMOUNT OVER LIMIT'
     O               35      XXOVER        J     74
     O          E            DET2           1
     O                                           74 '_______________'
     O          E            DET2           2
     O                                           17 'AGGREGATE CLAIM'
     O                       XXAGGR        J     74
     O          E            DET2           1
     O                                           17 'LESS RECOVERIES'
     O                                           27 'REQUESTED'
     O                       XXRQST        J     74
     O          E            DET2           1
     O                                           74 '_______________'
     O          E            DET2           2
     O                                           21 'NET AGGREGATE CLAIM'
     O               20N21                       59 '('
     O              N21      XXANET        1     73
     O               21                          71 'NO AGG. CLAIM'
     O               21                          79 'TO DATE'
     O               20N21                       74 ')'
     O          E            DET2           1
     O                                           14 'QUOTA SHARE:'
     O               41   N21SH(1)               25
     O               41   N21QT(1)               32 '   .  %'
     O               41 20N21                    34 '('
     O               41   N21AM(1)         2     46
     O               41 20N21                    47 ')'
     O               42   N21SH(2)               58
     O               42   N21QT(2)               65 '   .  %'
     O               42 20N21                    67 '('
     O               42   N21AM(2)         2     79
     O               42 20N21                    80 ')'
     O          E         N21DET2           1
     O               43   N21SH(3)               25
     O               43   N21QT(3)               32 '   .  %'
     O               43 20N21                    34 '('
     O               43   N21AM(3)         2     46
     O               43 20N21                    47 ')'
     O               44   N21SH(4)               58
     O               44   N21QT(4)               65 '   .  %'
     O               44 20N21                    67 '('
     O               44   N21AM(4)         2     79
     O               44 20N21                    80 ')'
     O          E         N21DET2           1
     O               45   N21SH(5)               25
     O               45   N21QT(5)               32 '   .  %'
     O               45 20N21                    34 '('
     O               45   N21AM(5)         2     46
     O               45 20N21                    47 ')'
     O               46   N21SH(6)               58
     O               46   N21QT(6)               65 '   .  %'
     O               46 20N21                    67 '('
     O               46   N21AM(6)         2     79
     O               46 20N21                    80 ')'
     O          E         N21DET2           1
     O               47   N21SH(7)               25
     O               47   N21QT(7)               32 '   .  %'
     O               47 20N21                    34 '('
     O               47   N21AM(7)         2     46
     O               47 20N21                    47 ')'
     O               48   N21SH(8)               58
     O               48   N21QT(8)               65 '   .  %'
     O               48 20N21                    67 '('
     O               48   N21AM(8)         2     79
     O               48 20N21                    80 ')'
     O          E         N21DET2           2
     O               49   N21SH(9)               25
     O               49   N21QT(9)               32 '   .  %'
     O               49 20N21                    34 '('
     O               49   N21AM(9)         2     46
     O               49 20N21                    47 ')'
     O               50   N21SH(10)              58
     O               50   N21QT(10)              65 '   .  %'
     O               50 20N21                    67 '('
     O               50   N21AM(10)        2     79
     O               50 20N21                    80 ')'
     O          E            PMTH           1
     O                  34                       23 'YOU HAVE NOT MADE ANY'
     O                  34                       46 'PAYMENTS ON THIS FILE,'
     O                  34                       70 'THEREFORE, NO CREDIT IS'
     O                  34                       75 'DUE.'
     O                 N34                       23 'YOU HAVE MADE THE'
     O                 N34                       43 'FOLLOWING PAYMENTS:'
     O          E            PYMT           1
     O                       XXPDAT              19 '  /  /    '
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
