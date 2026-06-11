     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8811R -LOSS REPORT & SPECIFIC EXCESS CLAIMS - REINSURANCE
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
     H*  05/12/98 C:15 Project: WC770                                 *
     H*                Added a line to the output to display any      *
     H*                claim transfers which were processed during    *
     H*                the current reporting period.  (*NOTE: If a    *
     H*                claim is transferred to another claim and pmts *
     H*                were made on the from claim within the entered *
     H*                run period, the transfer total will not be     *
     H*                correct.)                                      *
     H*                                                               *
     H*  04/07/98 C:15 Project: WC770                                 *
     H*                If the 'policy end date' cannot be calculated, *
     H*                add 1 year to the 'policy start date' and load *
     H*                into to 'policy end date'.                     *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  07/13/96 C:36 PROJECT: WC538                                 *
     H*                BACK OUT WC352 TO PRINT ALL ACCOUNTS WITH      *
     H*                TOTAL INCURRED OVER RETENTION                  *
     H*
     H*  06/25/96 C:36 CORRECT CALCULATIONS FOR PTD                   *
     H*
     H*  04/19/96 C:36 AGGREGATE LOSS REPORT CHANGES -                *
     H*                   1) SDF AMOUNT <> 0 - REPORT STAYS THE SAME  *
     H*                   2) SDF AMOUNT =  0 -                        *
     H*                        SPECIAL DISABILITY FUND AND TOTAL      *
     H*                        CHARGES TO LOSS FUND LINES WILL BE     *
     H*                        ELIMINATED                             *
     H*                                                               *
     H*  08/10/95 C:36 FOUND PROBLEM WITH CURRENT PERIOD PAYMENT      *
     H*                ACCUMULATION. THE PRIOR RUN DATE WAS BEING     *
     H*                USED AS THE FROM DATE FOR THE CURRENT PERIOD.  *
     H*                THIS WOULD CAUSE ONE DAYS PAYMENTS TO BE ADDED *
     H*                FROM THE LAST RUN IF THE AGGREGATE REPORTS.    *
     H*                SUBROUTINE -CALC- HAS BEEN MODIFIED TO USE ONE *
     H*                DAY AFTER THE PRIOR RUN DATE AS THE CURRENT    *
     H*                PERIOD FROM DATE. (XXCFDT)                     *
     H*                                                               *
     H*  07/13/94 C:22 PROJECT: WC352                                 *
     H*                MODFIED PAYMENT ACCUMULATION ALGORITHM.        *
     H*                MODIFIED PROGRAM TO IGNORE CLAIMS WHO EXCEED   *
     H*                RETENTION BUT DO NOT HAVE A RECORD IN THE      *
     H*                SPECIFIC FILING FILE. THIS IS TO CORRECT THE   *
     H*                SITUATION WHERE PRO-RATED EXPENSES KEPT THE    *
     H*                CLAIM FROM A SPECIFIC FILING, HOWEVER THIS     *
     H*                REPORT LOOKED AT TOTAL INCURRED AND INCLUDED   *
     H*                THE CLAIM.                                     *
     H*                                                               *
     H*  01/27/94 C:24 PROJECT: WC300                                 *
     H*                RE-ALIGN QSYSPRT REPORT TO 80 COLUMNS          *
     H*                                                               *
     H*  08/04/92 JJJ  PROJECT: WC157                                 *
     H*                USE WMRQL2 FOR MULTIPLE CLAIMS                 *
     H*                                                               *
     H*  07/07/92 JEI  PROJECT: WC157                                 *
     H*                CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR      *
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
     FWDF1L     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCML7    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     FWMCDL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWMRIL1    IF   E           K DISK
     FWMRQL     IF   E           K DISK
     FWMRQL2    IF   E           K DISK
     F                                     RENAME(PRQ01:PRQ02)
     FWDPAL5    IF   E           K DISK
     F                                     RENAME(PPA01:PPA02)
     FWDPFL2    IF   E           K DISK
     FWDPFL3    IF   E           K DISK
     F                                     RENAME(PPF01:PPF02)
     FWDLSL1    UF A E           K DISK
     FWXSEL1    UF A E           K DISK
     FWMEML     IF   E           K DISK
     FWAMCL     IF   E           K DISK
     FWAMCL1    IF   E           K DISK
     F                                     RENAME(PMC01:PMC02)
     FWMRRL1    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     F*** BEGIN ADD *** 05/12/98 **************************************
     FWACTL3    IF   E           K DISK
     FWACTL4    IF   E           K DISK
     F                                     RENAME(PCT01:PCT02)
     F*** END ADD ***** 05/12/98 **************************************
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOA)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOV)
     F                                     USROPN

     D*
     D*
     D MC              S             14    DIM(999)                             MULT CASE#
     D MA              S             22    DIM(999)                             MULT CLM AMT'S
     D*
     D*** BEGIN CHG *** 05/12/98 **************************************
     D******************* HDG     1  19 42               HEADINGS
     D HDG             S             42    DIM(20) CTDATA PERRCD(1)             HEADINGS
     D*** END CHG ***** 05/12/98 **************************************
     D*
     D FYR             S              3  0 DIM(20) ASCEND                       POLICY FYR'S
     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM1          C                   CONST('Aggregate Excess Cla-
     D                                     ims')
     D OPTNM2          C                   CONST('Specific Excess Clai-
     D                                     ms')
     D*** END   ADD *** 01/28/98 ***********************************************
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
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
     D  CMR                    1     90  2
     D                                     DIM(10)                              REMAINING RSV
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  PMT                    1     22  2
     D                                     DIM(2)                               PMT AMT'S
     D  SEPAID                 1     11  2
     D  SERESV                12     22  2
     D  XXPAMT                 1     22
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
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1EMP#
     C                   KFLD                    X1DIV
     C                   KFLD                    XXFYR             3 0
     C*
     C     CMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  KEY LIST FOR CLAIM HISTORY MASTER FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    CMCASE
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
     C*  KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE LOSS FILE
     C*
     C     LSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXMNDT
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
     C     SEKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXRNDT
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C*
     C     SEKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXRNDT
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C                   KFLD                    XXMCLM
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
     C*   KEY LIST FOR THE PAYMENT FILE
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    CMCASE
     C*
     C*** BEGIN ADD *** 05/12/98 **************************************
     C     PAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*** END ADD ***** 05/12/98 **************************************
     C*
     C*   KEY LIST FOR PARTIAL FILING FILE
     C*
     C     PFKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C                   KFLD                    XXRNDT
     C*
     C     PFKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXMCLM
     C                   KFLD                    X@CASE
     C                   KFLD                    XXRNDT
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*   KEY LIST FOR REINSURANCE REQUEST RECOVERY FILE
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
     C*   KEY LIST FOR REQUEST REFUND FILE
     C*
     C     RRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    XXCASE
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    RRFEIN
     C                   KFLD                    RRMOD
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHIC FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    RRFEIN
     C                   KFLD                    RRMOD
     C*
     C*** BEGIN ADD *** 05/12/98 **************************************
     C*  Key list for the Claim Transfer file.
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*** END ADD ***** 05/12/98 **************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQRNDT            8            RUN DATE
     C                   PARM                    QQPRDT            8            PRIOR RUN DT
     C                   PARM                    QQSDF            11            SDF  AMT
     C                   PARM                    QQREV             1            REVISION FLAG
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
     C                   MOVE      QQEMP#        X1EMP#
     C                   MOVE      QQDIV         X1DIV
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C*
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C                   MOVE      QQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   MOVE      QQRNDT        X1RNDT            8 0
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   MOVE      QQPRDT        X1PRDT            8 0
     C                   Z-ADD     X1PRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPRDT
     C*
     C                   MOVE      QQSDF         XXSDF            11 2
     C*
     C******* LIKE DEFINITIONS *******
     C*
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    MCCASE        X@CASE
     C     *LIKE         DEFINE    MCCASE        SVCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMEMP#        X1EMP#
     C     *LIKE         DEFINE    CMDIV         X1DIV
     C     *LIKE         DEFINE    CMDT          XXRIDT
     C     *LIKE         DEFINE    CMDT          XXEFDT
     C     *LIKE         DEFINE    CMDT          XXRNDT
     C     *LIKE         DEFINE    CMDT          XXPRDT
     C     *LIKE         DEFINE    CMDT          XXCFDT
     C     *LIKE         DEFINE    CMDT          XXMNDT
     C*** BEGIN ADD *** 05/12/98 **************************************
     C     *LIKE         DEFINE    LSTRNF        XXTRNF
     C     *LIKE         DEFINE    LSTRNF        XX11N2
     C     *LIKE         DEFINE    CMDT          XXTRDT
     C*** END ADD ***** 05/12/98 **************************************
     C********************************
     C                   MOVE      QQPRDT        X1CFDT            8 0
     C                   MOVE      1             SRDAYS            5 0
     C                   MOVE      X1CFDT        SRDATE            8 0
     C                   MOVE      *BLANKS       SRERR             1
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        X1CFDT
     C                   Z-ADD     X1CFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCFDT
     C*
     C                   EXSR      SR1ST
     C*****************************************************************
     C*
     C*  GET PRIOR MONTH ENDING AMOUNTS FROM LOSS FILE
     C*
     C                   Z-ADD     XXPRDT        XXMNDT
     C     LSKEY         CHAIN     PLS01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     LSPREV        XXPREV                         PRIOR MNTH
     C                   Z-ADD     LSPMTS        XXPMTS
     C                   Z-ADD     LSRECV        XXRECV
     C                   Z-ADD     LSADJ         XXADJ
     C*** BEGIN ADD *** 05/12/98 **************************************
     C                   Z-ADD     LSTRNF        XXTRNF
     C*** END ADD ***** 05/12/98 **************************************
     C                   ADD       LSPMTS        LSPREV                         PREV PAID
     C                   SUB       LSRECV        LSPREV
     C                   ADD       LSADJ         LSPREV
     C*** BEGIN ADD *** 05/12/98 **************************************
     C                   ADD       LSTRNF        LSPREV
     C*** END ADD ***** 05/12/98 **************************************
     C                   Z-ADD     LSRSVO        XXRSVO
     C     LSPREV        ADD       XXRSVO        XXPINC                         PREV INC
     C                   Z-ADD     LSTLEX        XXTLEX
     C     XXPINC        SUB       LSTLEX        XXPAGG                         PREV AGG LSS
     C                   Z-ADD     LSSDF         XXPSDF                         PREV SDF
     C     XXPSDF        ADD       XXPAGG        XXPCHG                         PREV CHARGE
     C                   Z-ADD     LSEAPR        XXEAPR
     C     LSATTP        DIV(H)    100           XXPCT             5 4
     C     XXPCT         MULT(H)   XXEAPR        XXPFPC                         PREV FND %
     C     XXPFPC        SUB       XXPCHG        XXPFBA                         PREV FND BAL
     C     XXPFBA        COMP      0                                    21
     C                   Z-ADD     LSAGGR        XXPSAG                         PREV SELF AG
     C     XXPSAG        IFNE      0
     C     XXPSAG        SUB       XXPCHG        XXPABL                         PREV AGG BAL
     C                   END
     C     XXPABL        COMP      0                                    37
     C     LSEAPR        IFNE      0
     C     XXPCHG        DIV(H)    LSEAPR        XXWK64            6 4          PREV LOSS
     C                   ELSE
     C                   Z-ADD     0             XXWK64
     C                   END
     C     XXWK64        MULT      100           XXPLSS
     C                   Z-ADD     LSTLCL        XXTLCL
     C                   Z-ADD     LSOPEN        XXOPEN
     C*
     C                   Z-ADD     0             LSPMTS
     C                   Z-ADD     0             LSRECV
     C                   Z-ADD     0             LSADJ
     C*** BEGIN ADD *** 05/12/98 **************************************
     C                   Z-ADD     0             LSTRNF
     C*** END ADD ***** 05/12/98 **************************************
     C                   Z-ADD     0             LSRSVO
     C                   Z-ADD     0             LSTLCL
     C                   Z-ADD     0             LSOPEN
     C                   Z-ADD     0             LSTLEX
     C                   Z-ADD     0             LSSDF
     C                   Z-ADD     0             LSEAPR
     C                   Z-ADD     0             LSAGGR
     C                   END
     C*
     C*  IF LOSS REPORT HAS ALREADY BEEN RUN, SKIP CALCULATION FOR
     C*  PAYMENTS & PRINT ALL SPECIFIC EXCESS CLAIMS IN THE WORK FILE
     C*
     C                   Z-ADD     XXRNDT        XXMNDT
     C     QQREV         IFEQ      'Y'
     C     LSKEY         CHAIN     PLS01                              60
     C     *IN60         IFEQ      '0'
     C*
     C     SEKEY         CHAIN(N)  PSE01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      '0'           *IN79
     C                   Z-ADD     SEFYR         XXFYR
     C                   MOVE      SECASE        XXCASE
     C     SEPAID        ADD       SERESV        XXINC
     C     SECASE        IFEQ      *ZEROS
     C                   MOVE      '1'           *IN79
     C                   Z-ADD     SEMCLM        XXMCLM
     C     MCKY1         CHAIN     PMC02                              60
     C                   MOVE      MCCASE        XXCASE
     C                   MOVE      MCFYR         XXFYR
     C                   END
     C     CMKEY2        CHAIN     PCM01                              60
     C     SECASE        IFNE      *ZEROS
     C     SEMCLM        ANDNE     0
     C                   EXSR      WRTMLT
     C                   ELSE
     C                   EXSR      WRTEXC
     C                   END
     C*
     C     SEKEY         READE(N)  PSE01                                  60
     C                   END
     C                   END
     C                   END
     C*
     C*  IF NOT A REVISION:
     C*
     C*  NEED TO LOOP THRU ALL EMPLOYERS WHICH DO NOT HAVE A UNIQUE
     C*  POLICY SET UP
     C*
     C*  START READING CLAIM FILE - CALCULATE RESERVES ON OPEN
     C*  CLAIMS(6); TOTAL CLAIMS(18); TOTAL OPEN CLAIMS(19); EXCESS
     C*  OVER RETENTION(8) & PRINT SPECIFIC EXCESS CLAIMS.
     C*
     C     QQREV         IFEQ      'N'
     C*
     C*  LOAD ALL FYR'S WHICH EXIST WITHIN THIS POLICY PERIOD
     C*
     C                   Z-ADD     1             Z                 3 0
     C                   Z-ADD     0             FYR
     C*                    (XXCO#/XXFND)
     C     F1KY1         CHAIN     PF101                              60
     C     *IN60         DOWEQ     '0'
     C     XXEFDT        IFLT      F1ENPO
     C     XXENDT        ANDGT     F1STPO
     C                   Z-ADD     F1FYR         FYR(Z)
     C                   ADD       1             Z
     C                   END
     C     F1KY1         READE     PF101                                  60
     C                   END
     C*
     C* READ THE EMPLOYER MASTER FILE IF EMPLR ENTERED IS 0
     C*
     C                   MOVE      '0'           *IN60
     C     XXEMP#        COMP      0                                      25
     C   25EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  IF EMPLR = 0, CHK FOR UNIQUE ENTRY, IF EXISTS GET NXT EMPLR
     C*
     C                   MOVE      'Y'           CONT              1
     C     *IN25         IFEQ      '1'
     C                   Z-ADD     EMEMP#        X1EMP#
     C                   Z-ADD     EMDIV         X1DIV
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      'A'           XXASFL
     C                   Z-ADD     XXEFDT        XXRIDT
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'N'           CONT
     C                   ELSE
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   END
     C                   END
     C*
     C*  IF CONTINUE, WE MUST READ THE CLAIM MASTER FOR EACH FYR THAT
     C*  EXISTS IN THE POLICY PERIOD AND ALSO VERIFY THE ACCIDENT DATE
     C*  TO THE EFFECTIVE DATES
     C*
     C     CONT          IFEQ      'Y'
     C*
     C                   Z-ADD     1             Z
     C     FYR(Z)        DOWNE     0
     C*
     C                   Z-ADD     FYR(Z)        XXFYR
     C     CMKEY         CHAIN     PCM02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     CMACDT        IFGE      XXEFDT
     C     CMACDT        ANDLT     XXENDT
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      CMCASE        X@CASE
     C     SEKY1         CHAIN     PSE01                              53
     C     *IN53         IFEQ      '1'
     C                   Z-ADD     XXCO#         SECO#
     C                   Z-ADD     XXFND         SEFND
     C                   Z-ADD     XXFYR         SEFYR
     C                   MOVE      *ZEROS        SECASE
     C                   Z-ADD     0             SEMCLM
     C                   Z-ADD     XXEMP#        SEEMP#
     C                   Z-ADD     XXDIV         SEDIV
     C                   Z-ADD     XXEFDT        SEEFDT
     C                   Z-ADD     XXRNDT        SERNDT
     C                   END
     C*
     C* NEW VALUES ON UPDATE
     C*
     C                   MOVE      *BLANKS       SESTAT
     C                   MOVEL     Q1USER        SEUSER
     C                   MOVEL     Q1PGM         SEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SETIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SEDT
     C*
     C*  RESET VALUES TO BE RECALCULATED
     C*
     C                   Z-ADD     0             SEPAID
     C                   Z-ADD     0             SERESV
     C                   Z-ADD     0             SERETN
     C                   Z-ADD     0             XXMCLM
     C                   MOVE      '0'           *IN79
     C*
     C*
     C*  CHECK FOR MULTIPLE CLAIM
     C*
     C     MCKEY         CHAIN     PMC01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      '1'           *IN79
     C                   EXSR      MLTPMT
     C                   ELSE
     C                   EXSR      CALC
     C                   END
     C*
     C*  GET RETENTION ACCORDING TO THE CLAIM'S ACCIDENT DATE
     C*
     C                   Z-ADD     0             SERETN           11 2          FUND RETEN.
     C                   EXSR      RETN
     C*
     C*  IF TOTAL INCURRED FOR CLAIM IS > OR = TO RETENTION, PRINT ON
     C*  EXCESS REPORT  & BUILD WORKFILE
     C*
     C     SEPAID        ADD       SERESV        XXINC            11 2
     C     XXINC         IFGE      SERETN
     C*
     C     *IN79         IFEQ      '1'
     C                   EXSR      GETMLT
     C                   ELSE
     C                   MOVE      CMCASE        SECASE
     C                   Z-ADD     0             SEMCLM
     C                   END
     C                   EXSR      WRTEXC
     C     *IN53         IFEQ      '0'
     C                   UPDATE    PSE01
     C                   ELSE
     C                   WRITE     PSE01
     C                   END
     C*
     C                   END
     C*
     C*  GET ESTIMATED AND ACTUAL SDF REIMBURSEMENTS TO DATE
     C*
     C                   EXSR      SDF
     C*
     C                   END                                                    DATE CHK
     C*
     C*  GET NEXT CLAIM
     C*
     C     CMKEY         READE     PCM02                                  60
     C                   END
     C*
     C*  GET NEXT FYR
     C*
     C                   ADD       1             Z
     C                   END                                                    FYR,Z NE 0
     C*
     C*** BEGIN ADD *** 05/12/98 **************************************
     C*  Calculate total paid for claim transfers.
     C*
     C                   EXSR      CLMTRN
     C*** END ADD ***** 05/12/98 **************************************
     C*
     C*  GET THE NEXT EMPLOYER
     C*
     C                   END                                                    XXCONT = Y
     C     *IN25         IFEQ      '0'
     C                   MOVE      '1'           *IN60
     C                   ELSE
     C     EMKY          READE     PEM01                                  60
     C                   END
     C*
     C                   END                                                    60 DOWEQ 0
     C*
     C*  CALCULATE CLAIMS PAID APPLICABLE TO SPECIFIC
     C*
     C     XXEXPD        SUB       XXEXRE        LSSPPD
     C*
     C                   END                                                    QQREV = N
     C*
     C*  PRINT TOTAL LINE FOR SPECIFIC EXCESS CLAIM
     C*
     C                   EXCEPT    TOT
     C*
     C*****************************************************************
     C*
     C*  GET REMAINING LOSS INFORMATION FOR LOSS REPORT
     C*
     C*
     C*  GET AGGREGATE REINSURANCE POLICY FILE
     C*                               (CO# FND FYR XXASFL XXRNDT)
     C                   MOVE      'A'           XXASFL
     C                   Z-ADD     XXEFDT        XXRIDT
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             RIATTP
     C                   MOVE      *BLANKS       RIPRTP
     C                   Z-ADD     0             RIPAMT
     C                   Z-ADD     0             RIMNLS
     C                   END
     C*
     C*  CALCULATE TOTAL PAID TO DATE
     C*
     C     LSPREV        ADD       LSPMTS        XXPDTD
     C                   SUB       LSRECV        XXPDTD
     C                   ADD       LSADJ         XXPDTD
     C*** BEGIN ADD *** 05/12/98 **************************************
     C                   ADD       LSTRNF        XXPDTD
     C*** END ADD ***** 05/12/98 **************************************
     C*
     C     XXPDTD        ADD       LSRSVO        XXTLIN
     C     XXTLIN        SUB       LSTLEX        XXAGLS
     C*
     C*   GET EARNED PREMIUM AMOUNT
     C*
     C                   Z-ADD     RIPAMT        LSEAPR
     C*
     C                   Z-ADD     XXSDF         LSSDF
     C     LSSDF         ADD       XXAGLS        XXCHRG
     C                   Z-ADD     RIATTP        LSATTP
     C     RIATTP        DIV(H)    100           XXPCT
     C     XXPCT         MULT(H)   LSEAPR        XXFPCT
     C*
     C*  IF FYR HAS BEEN AUDITED, CHECK MINIMUM LOSS FUND
     C*
     C                   MOVE      '0'           *IN38
     C     RIAUDF        IFEQ      'Y'
     C     RIMNLS        ANDGT     XXFPCT
     C                   Z-ADD     RIMNLS        XXFPCT
     C                   MOVE      '1'           *IN38
     C                   END
     C*
     C     XXFPCT        SUB       XXCHRG        XXFBAL
     C     XXFBAL        COMP      0                                    20
     C     RIIPCT        COMP      0                                  35
     C     *IN35         IFEQ      '1'
     C     RIIPCT        DIV(H)    100           XXPCT
     C     XXPCT         MULT(H)   LSEAPR        LSAGGR
     C                   MOVE      '0'           *IN39
     C     RIAUDF        IFEQ      'Y'
     C     RIMNLS        ANDGT     LSAGGR
     C                   Z-ADD     RIMNLS        LSAGGR
     C                   MOVE      '1'           *IN39
     C                   END
     C                   END
     C     LSAGGR        SUB       XXCHRG        XXABAL
     C     XXABAL        COMP      0                                    36
     C     LSEAPR        IFNE      0
     C     XXCHRG        DIV(H)    LSEAPR        XXWK64            6 4
     C                   ELSE
     C                   Z-ADD     0             XXWK64
     C                   END
     C     XXWK64        MULT      100           XXLOSS
     C*
     C     XXSDF         COMP      *ZEROS                             6565
     C                   EXCEPT    DET
     C*
     C*  WRITE OR UPDATE LOSS REPORT FOR CURRENT MONTH
     C*
     C* MOVE DATA TO CHAIN TO LS RECORD
     C                   Z-ADD     XXCO#         LSCO#
     C                   Z-ADD     XXFND         LSFND
     C                   Z-ADD     XXFYR         LSFYR
     C                   Z-ADD     XXRNDT        LSRNDT
     C                   Z-ADD     XXRNDT        XXMNDT
     C                   Z-ADD     XXEMP#        LSEMP#
     C                   Z-ADD     XXDIV         LSDIV
     C                   Z-ADD     XXEFDT        LSEFDT
     C*  SAVE LS DATA IN CASE MATCHING RECORD IS TO BE UPDATED
     C                   Z-ADD     LSPREV        TPPREV           11 2
     C                   Z-ADD     LSPMTS        TPPMTS           11 2
     C                   Z-ADD     LSRECV        TPRECV           11 2
     C                   Z-ADD     LSADJ         TPADJ            11 2
     C*** BEGIN ADD *** 05/12/98 **************************************
     C                   Z-ADD     LSTRNF        TPTRNF           11 2
     C*** END ADD ***** 05/12/98 **************************************
     C                   Z-ADD     LSRSVO        TPRSVO           11 2
     C                   Z-ADD     LSTLEX        TPTLEX           11 2
     C                   Z-ADD     LSSDF         TPSDF            11 2
     C                   Z-ADD     LSEAPR        TPEAPR           11 2
     C                   Z-ADD     LSAGGR        TPAGGR           11 2
     C                   Z-ADD     LSATTP        TPATTP            5 2
     C                   Z-ADD     LSTLCL        TPTLCL            7 0
     C                   Z-ADD     LSOPEN        TPOPEN            7 0
     C                   Z-ADD     LSSPPD        TPSPPD           11 2
     C*
     C     LSKEY         CHAIN     PLS01                              60
     C                   MOVEL     Q1USER        LSUSER
     C                   MOVEL     Q1PGM         LSPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        LSTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LSDT
     C     *IN60         IFEQ      '0'
     C* MOVE DATA BACK FROM TEMPORARY FIELDS FOR UPDATE
     C                   Z-ADD     TPPREV        LSPREV           11 2
     C                   Z-ADD     TPPMTS        LSPMTS           11 2
     C                   Z-ADD     TPRECV        LSRECV           11 2
     C                   Z-ADD     TPADJ         LSADJ            11 2
     C*** BEGIN ADD *** 05/12/98 **************************************
     C                   Z-ADD     TPTRNF        LSTRNF
     C*** END ADD ***** 05/12/98 **************************************
     C                   Z-ADD     TPRSVO        LSRSVO           11 2
     C                   Z-ADD     TPTLEX        LSTLEX           11 2
     C                   Z-ADD     TPSDF         LSSDF            11 2
     C                   Z-ADD     TPEAPR        LSEAPR           11 2
     C                   Z-ADD     TPAGGR        LSAGGR           11 2
     C                   Z-ADD     TPATTP        LSATTP            5 2
     C                   Z-ADD     TPTLCL        LSTLCL            7 0
     C                   Z-ADD     TPOPEN        LSOPEN            7 0
     C                   Z-ADD     TPSPPD        LSSPPD           11 2
     C                   UPDATE    PLS01
     C                   ELSE
     C                   MOVE      *BLANKS       LSSTAT
     C                   WRITE     PLS01
     C                   END
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       xxPrt = '1';
       Chain orKey POR01;
       If %found;
          Except Rout1;
       Endif;
       If *In88 = '0';
          Open Qsysprt2;
          *In88 = '1';
          xxPrt = '2';
          Chain orKey POR01;
          If %found;
             Except Rout2;
          Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
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
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C*
     C*   CENTER COMPANY NAME
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C*** END   ADD *** 01/28/98 ***********************************************
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
     C                   Z-ADD     9999999       XXENDT            7 0
     C                   MOVE      'A'           XXASFL
     C                   Z-ADD     XXEFDT        XXRIDT
     C*                    (XXCO#/XXFND/XXEMP#/XXDIV/XXASFL/XXRIDT)
     C     RIKEY         CHAIN     PRI01                              60
     C     *IN60         IFEQ      '0'
     C     RIKY3         READE     PRI01                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     RIEFDT        XXENDT
     C                   Z-ADD     RIEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
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
     C                   Z-ADD     XXENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
     C                   END
     C*** END ADD ***** 04/07/98 ***********************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL(P)  ORTITL        XXTITL           40
     C                   MOVEL(P)  XXTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        XXTITL
      ***** Beg Del ***** 08/26/14 *************************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR PRINTER FILE 1
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL     ORTITL        X1TITL           40
     C                   MOVEL(P)  X1TITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        X1TITL
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         EXCEPT    ROUT1
      ***** End Del ***** 08/26/14 *************************************************
     C                   END
     C*
     C                   MOVE      *BLANKS       XXNAME           40
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXNAME
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C*  INITIALIZE TOTAL FIELDS FOR LOSS REPORT
     C*
     C****** CURRENT TOTALS
     C                   Z-ADD     0             XXPDTD           11 2          PAID TO DATE
     C                   Z-ADD     0             XXTLIN           11 2          TOTAL INC
     C                   Z-ADD     0             XXAGLS           11 2          AGG. LOSSES
     C                   Z-ADD     0             XXCHRG           11 2          TOTAL CHARGE
     C                   Z-ADD     0             XXFPCT           11 2          FUND %
     C                   Z-ADD     0             XXFBAL           11 2          FUND BALANCE
     C                   Z-ADD     0             XXSFA            11 2          SELF FND AGG
     C                   Z-ADD     0             XXABAL           11 2          AGGR. BAL
     C                   Z-ADD     0             XXLOSS            6 2          LOSS RATIO
     C****** PRIOR MONTH TOTALS
     C                   Z-ADD     0             XXPREV           11 2          PREV MO PMT
     C                   Z-ADD     0             XXPMTS           11 2          PREV PMTS
     C                   Z-ADD     0             XXRECV           11 2          PREV RECOV.
     C                   Z-ADD     0             XXADJ            11 2          PREV ADJ
     C                   Z-ADD     0             XXRSVO           11 2          RSV OPEN
     C                   Z-ADD     0             XXPINC           11 2          PREV INC
     C                   Z-ADD     0             XXTLEX           11 2          TOTAL EXCESS
     C                   Z-ADD     0             XXPAGG           11 2          PREV AGG LS
     C                   Z-ADD     0             XXPSDF           11 2          PREV SDF
     C                   Z-ADD     0             XXPCHG           11 2          PREV CHARGE
     C                   Z-ADD     0             XXEAPR           11 2          EARNED PREM
     C                   Z-ADD     0             XXPFPC           11 2          PREV FND %
     C                   Z-ADD     0             XXPFBA           11 2          PREV FND BAL
     C                   Z-ADD     0             XXPSAG           11 2          PREV SELF AG
     C                   Z-ADD     0             XXPABL           11 2          PREV AGG BAL
     C                   Z-ADD     0             XXPLSS            6 2          PREV LOSS
     C                   Z-ADD     0             XXTLCL            6 0          TOTAL CLM
     C                   Z-ADD     0             XXOPEN            6 0          TOTAL OPEN
     C*** BEGIN ADD *** 05/12/98 **************************************
     C                   Z-ADD     0             XXTRNF
     C*** END ADD ***** 05/12/98 **************************************
     C*
     C*  INITIALIZE TOTALS FOR SPECIFIC EXCESS CLAIMS
     C*
     C                   Z-ADD     0             XXTOTP           11 2          TOTAL PAID
     C                   Z-ADD     0             XXTOTR           11 2          TOTAL OUTST
     C                   Z-ADD     0             XXTOTI           11 2          TOTAL INCUR
     C                   Z-ADD     0             XXTLRE           11 2          TOTAL RETEN
     C                   Z-ADD     0             XXEXPD           11 2          EXCESS PAID
     C                   Z-ADD     0             XXEXRE           11 2          EXCESS RETN
     C*
     C*  INITIALIZE TOTALS FOR SDF TOTALS
     C*
     C                   Z-ADD     0             XXAMTE           11 2          ESTIMATED
     C                   Z-ADD     0             XXAMTC           11 2          COLLECTED
     C*
     C*  INITIALIZE TOTALS FOR SDF TOTALS
     C*
     C                   Z-ADD     0             XXAMTE           11 2          ESTIMATED
     C                   Z-ADD     0             XXAMTC           11 2          COLLECTED
     C*
     C                   ENDSR
     C**************************************************************
     C*    MLTPMT SUBROUTINE - ACCUMULATE TOTAL PAID & RESERVES FOR
     C*    MULTIPLE CLAIMS
     C**************************************************************
     C*
     C     MLTPMT        BEGSR
     C*
     C                   MOVE      *BLANKS       MC                             MULT CLM#
     C                   MOVE      *BLANKS       MA                             MULT CLM AMT
     C                   Z-ADD     1             X
     C                   Z-ADD     0             XXPAID           11 2
     C                   Z-ADD     0             XXRESV           11 2
     C                   Z-ADD     MCCLM#        XXMCLM            7 0
     C                   MOVE      CMCASE        SVCASE
     C     MCKY1         CHAIN     PMC02                              60
     C*
     C*  IF THE CASE IS THE 1ST LISTED IN THE MULT CLAIM FILE, THEN THE
     C*  PAYMENTS NEED TO BE CALCULATED & MULTIPLE ARRAY FILLED
     C*
     C     MCCASE        IFEQ      X@CASE
     C     *IN60         DOWEQ     '0'
     C*
     C                   EXSR      CALC
     C                   ADD       SERESV        XXRESV
     C                   ADD       SEPAID        XXPAID
     C                   MOVE      MCCASE        MC(X)
     C                   MOVE      XXPAMT        MA(X)
     C                   ADD       1             X
     C*
     C     MCKY1         READE     PMC02                                  60
     C  N60              MOVE      MCCASE        XXCASE
     C  N60CMKEY2        CHAIN     PCM01                              55
     C                   END
     C*
     C*  RESET POINTER IN CLAIM MASTER FILE TO CONTINUE PROCESSING
     C*
     C                   MOVE      SVCASE        XXCASE
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SVCASE        X@CASE
     C     CMKEY2        CHAIN     PCM01                              60
     C*
     C                   Z-ADD     XXRESV        SERESV
     C                   Z-ADD     XXPAID        SEPAID
     C                   END
     C*
     C                   ENDSR
     C**************************************************************
     C*    GETMLT SUBROUTINE - GET ALL MULTIPLE CLAIMS IN ARRAY &
     C*    PRINT EACH ON REPORT & WRITE EACH TO THE SPECIFIC FILE
     C**************************************************************
     C*
     C     GETMLT        BEGSR
     C*
     C*  SAVE TOTAL PAID & RESERVE AMOUNT
     C*
     C                   Z-ADD     SERETN        SVRETN           11 2
     C                   Z-ADD     SEPAID        SVPAID           11 2
     C                   Z-ADD     SERESV        SVRESV           11 2
     C                   MOVE      CMCASE        SVCASE
     C                   Z-ADD     1             X
     C     X             DOWLE     999
     C     MC(X)         ANDNE     *BLANKS
     C*
     C*  CHECK TO SEE IF WXSEP RECORD ALREADY EXISTS FOR THE CASE
     C*
     C*
     C                   MOVE      MC(X)         X@CASE
     C     SEKY1         CHAIN     PSE01                              53
     C     *IN53         IFEQ      '1'
     C                   Z-ADD     XXCO#         SECO#
     C                   Z-ADD     XXFND         SEFND
     C                   Z-ADD     XXFYR         SEFYR
     C                   MOVE      MC(X)         SECASE
     C                   Z-ADD     XXMCLM        SEMCLM
     C                   Z-ADD     XXEMP#        SEEMP#
     C                   Z-ADD     XXDIV         SEDIV
     C                   Z-ADD     XXEFDT        SEEFDT
     C                   Z-ADD     XXRNDT        SERNDT
     C                   END
     C*
     C* NEW VALUES ON UPDATE
     C*
     C                   MOVE      *BLANKS       SESTAT
     C                   MOVEL     Q1USER        SEUSER
     C                   MOVEL     Q1PGM         SEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SETIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SEDT
     C*
     C*  LOAD NEW VALUES
     C*
     C                   Z-ADD     SVRETN        SERETN
     C                   MOVE      MA(X)         XXPAMT
     C     SEPAID        ADD       SERESV        XXINC
     C                   MOVE      MC(X)         XXCASE
     C     CMKEY2        CHAIN     PCM01                              60
     C                   EXSR      WRTMLT
     C     *IN53         IFEQ      '0'
     C                   UPDATE    PSE01
     C                   ELSE
     C                   WRITE     PSE01
     C                   END
     C                   ADD       1             X
     C*
     C                   END
     C*
     C* RE-INITIALIZE FIELDS FOR THE MULT CLM SUMMARY RECORD, THIS
     C* RECORD WILL BE WRITTEN AFTER THE WRTEXC ROUTINE
     C*
     C                   MOVE      *ZEROS        X@CASE
     C     SEKY2         CHAIN     PSE01                              53
     C     *IN53         IFEQ      '1'
     C                   Z-ADD     XXCO#         SECO#
     C                   Z-ADD     XXFND         SEFND
     C                   Z-ADD     XXFYR         SEFYR
     C                   MOVE      *ZEROS        SECASE
     C                   Z-ADD     XXMCLM        SEMCLM
     C                   Z-ADD     XXEMP#        SEEMP#
     C                   Z-ADD     XXDIV         SEDIV
     C                   Z-ADD     XXEFDT        SEEFDT
     C                   Z-ADD     XXRNDT        SERNDT
     C                   END
     C*
     C* NEW VALUES ON UPDATE
     C*
     C                   MOVE      *BLANKS       SESTAT
     C                   MOVEL     Q1USER        SEUSER
     C                   MOVEL     Q1PGM         SEPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        SETIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SEDT
     C*
     C                   Z-ADD     SVPAID        SEPAID
     C                   Z-ADD     SVRESV        SERESV
     C     SEPAID        ADD       SERESV        XXINC
     C*
     C*  RESET POINTER IN CLAIM MASTER FILE TO CONTINUE PROCESSING
     C*
     C                   MOVE      SVCASE        XXCASE
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SVCASE        X@CASE
     C     CMKEY2        CHAIN     PCM01                              60
     C*
     C                   ENDSR
     C**************************************************************
     C*    CALC SUBROUTINE - CALC CLAIM TOTALS: TOTAL CLAIMS; TOTAL
     C*    OPEN CLAIMS; PAYMENTS; RESERVES.
     C**************************************************************
     C*
     C     CALC          BEGSR
     C*
     C*  ACCUMULATE TOTAL CLAIMS & RESERVES ON OPEN CLAIMS.  IF CLAIM
     C*  IS CLOSED, THEN RESERVES WILL BE 0.  CALCULATE TOTAL PAID &
     C*  TOTAL INCURRED FOR THE CLAIM.
     C*
     C                   ADD       1             LSTLCL
     C*
     C                   XFOOT     CMR           SERESV
     C                   ADD       SERESV        LSRSVO
     C                   XFOOT     CMA           SEPAID
     C*
     C* GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     CHCO#         IFEQ      CMCO#
     C     CHFND         ANDEQ     CMFND
     C     CHFYR         ANDEQ     CMFYR
     C     CHCASE        ANDEQ     CMCASE
     C*
     C     CHOCST        IFNE      'C'
     C                   ADD       1             LSOPEN
     C                   END
     C*
     C                   ELSE
     C*
     C*  IF NO CLAIM STATUS RECORD FOUND USE CMSTAT
     C*
     C     CMSTAT        IFNE      'C'
     C                   ADD       1             LSOPEN
     C                   END
     C*
     C                   END
     C*
     C*  GET CURRENT MONTHS PAYMENTS FOR THE CLAIM
     C*
     C                   EXSR      GETPMT
     C*
     C                   ENDSR
     C**************************************************************
     C*    GETPMT SUBROUTINE - BREAK OUT PAYMENTS TO DATE
     C**************************************************************
     C*
     C     GETPMT        BEGSR
     C*
     C*  READ PAYMENT DETAIL FILE FOR CURRENT MONTH
     C*                                (CO FND FYR CASE)
     C     PAKY1         CHAIN     PPA02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATRDT        IFGE      XXCFDT
     C     PATRDT        ANDLE     XXRNDT
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C                   ADD       PAAMT         LSPMTS
     C                   END
     C     PATYPE        IFEQ      'V'
     C     PATYPE        OREQ      'S'
     C                   SUB       PAAMT         LSPMTS
     C                   END
     C     PATYPE        IFEQ      'A'
     C                   ADD       PAAMT         LSADJ
     C                   END
     C     PATYPE        IFEQ      'R'
     C                   ADD       PAAMT         LSRECV
     C                   END
     C                   END
     C*
     C     PAKY1         READE     PPA02                                  60
     C                   END
     C*
     C                   ENDSR
     C**************************************************************
     C*    RETENTION SUBROUTINE - CALC RETENTION FOR THE CLAIM
     C**************************************************************
     C     RETN          BEGSR
     C*
     C*  GET REINSURANCE POLICY FILE TO DETERMINE RETENTION FOR THIS
     C*  ACCIDENT DATE PERIOD. MUST BE THE SPECIFIC POLICY (RIASFL=S).
     C*  REINSURANC COMPANY# 999 REPRESENTS THE FUNDS PORTION OF COV.
     C*
     C*                          (CO# FND EMP# DIV 'S' CMACDT)
     C                   MOVE      'S'           XXASFL            1
     C                   Z-ADD     CMACDT        XXRIDT
     C     RIKEY         SETGT     PRI01
     C                   READP     PRI01                                  60
     C     *IN60         IFEQ      '0'
     C     RICO#         ANDEQ     XXCO#
     C     RIFND         ANDEQ     XXFND
     C     RIEMP#        ANDEQ     XXEMP#
     C     RIDIV         ANDEQ     XXDIV
     C     RIASFL        ANDEQ     'S'
     C*
     C*  GET FUND RETENTION AMOUNT  (CO# = 999)
     C*
     C                   Z-ADD     1             X                 4 0
     C     *ZERO         LOOKUP    CO(X)                              60
     C     *IN60         IFEQ      '1'
     C     CO(X)         ANDEQ     999
     C                   Z-ADD     LM(X)         SERETN                         FUND RETENTION
     C                   END
     C*
     C                   END
     C*
     C*  IF CLAIM THE EXISTS IN THE PARTIAL FILING, OVERRIDE FUND
     C*  RETENTION AMT
     C*
     C                   MOVE      *ZEROS        X@CASE
     C  N79              MOVE      CMCASE        X@CASE
     C  N79PFKEY         SETGT     PPF01                              60
     C  N79              READP     PPF01                                  60
     C   79              MOVE      MCCLM#        XXMCLM
     C   79PFKEY1        SETGT     PPF02                              60
     C   79              READP     PPF02                                  60
     C     *IN60         IFEQ      '0'
     C     PFCO#         ANDEQ     XXCO#
     C     PFFND         ANDEQ     XXFND
     C     PFFYR         ANDEQ     XXFYR
     C     PFCASE        ANDEQ     X@CASE
     C     PFMCLM        ANDEQ     XXMCLM
     C*
     C                   Z-ADD     PFRETN        SERETN
     C*
     C*  IF CLAIM HAS BEEN FILED, INCLUDE FUNDS PORTION OF DEFENSE
     C*  IN RETENTION.
     C*
     C                   MOVE      'S'           XXASFL            1
     C*
     C*  IF MULT CLAIM (*IN79 = '1'), USE MULT CLM# TO GET REQUEST
     C*
     C  N79RQKEY         CHAIN     PRQ01                              60
     C   79RQKY2         CHAIN     PRQ02                              60
     C     *IN60         IFEQ      '0'
     C                   ADD       PFFDEF        SERETN
     C                   END
     C*
     C                   ELSE
     C*                    MOVE '1'       *IN86
     C                   END
     C*
     C                   ENDSR
     C**************************************************************
     C*    WRTEXC SUBROUTINE - WRITE EXCESS CLAIM
     C*
     C**************************************************************
     C     WRTEXC        BEGSR
     C*
     C                   MOVE      SECASE        XXCASE
     C     XXINC         SUB       SERETN        XXEXCS           11 2          Excess Amt
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C*  GET THE CLAIMANT'S NAME
     C*
     C                   MOVE      *BLANKS       XXNAME           40
     C     *IN79         IFEQ      '0'
     C     CDKEY         CHAIN     PCD01                              60
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME
     C                   ELSE
     C                   MOVEL     'MULTIPLE'    XXNAME
     C                   END
     C*
     C*  PRINT SPECIFIC EXCESS CLAIM
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    EXCS
     C*
     C*  ACCUMULATE TOTALS
     C*
     C                   ADD       SEPAID        XXTOTP
     C                   ADD       SERESV        XXTOTR
     C                   ADD       XXINC         XXTOTI
     C                   ADD       SERETN        XXTLRE
     C     QQREV         IFEQ      'N'
     C                   ADD       XXEXCS        LSTLEX
     C                   END
     C     SEPAID        IFGT      SERETN
     C                   ADD       SEPAID        XXEXPD
     C                   ADD       SERETN        XXEXRE
     C                   END
     C*
     C                   ENDSR
     C**************************************************************
     C*    WRTMLT SUBROUTINE - WRITE MULTIPLE CLAIMS ONLY
     C*
     C**************************************************************
     C     WRTMLT        BEGSR
     C*
     C     *IN88         IFEQ      '0'
     C                   OPEN      QSYSPRT2
     C                   MOVE      '1'           *IN88
     C*   GET THE DISTRIBUTION INFORMATION FOR PRINTER FILE 1
     C                   MOVE      '2'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVEL(P)  ORTITL        X2TITL           40
     C                   MOVEL(P)  X2TITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        X2TITL
      ***** Beg Del ***** 08/26/14 *************************************************
     C*********          EXCEPT    ROUT2
      ***** End Del ***** 08/26/14 *************************************************
     C                   END
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT
     C*
     C*  GET THE CLAIMANT'S NAME
     C*
     C                   MOVE      *BLANKS       XXNAME           40
     C     CDKEY         CHAIN     PCD01                              60
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME
     C*
     C*  PRINT SPECIFIC EXCESS CLAIM
     C*
     C     *INOV         IFEQ      '1'
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOV
     C                   END
     C                   MOVE      SECASE        XXCASE
     C                   EXCEPT    MLT
     C*
     C                   ENDSR
     C**************************************************************
     C*    SDF SUBROUTINE - GET ESTIMATE & ACTUAL SDF RECOVERIES
     C*
     C**************************************************************
     C     SDF           BEGSR
     C*
     C*  START READING REQUEST REFUND FILE - EXCLUDE INACTIVE RECORDS
     C*                                 (CO# FND CMFYR CMCASE)
     C                   MOVE      CMCASE        XXCASE
     C     RRKEY         CHAIN     PRR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     RRSTAT        IFNE      'I'
     C     RRSTCD        ANDNE     '99'
     C*
     C*  MAKE SURE PAYEE TYPE IS MED, SDF, OR OTHER.
     C*
     C     PDKEY         CHAIN     PPD01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     PDTYPE        IFEQ      '2'
     C     PDTYPE        OREQ      '3'
     C     PDTYPE        OREQ      '5'
     C*
     C*  CHECK FOR MORE THAN 1 REQUEST FOR SAME CLAIM & PROVIDER
     C*
     C                   MOVE      '0'           *IN50
     C     *IN50         DOWEQ     '0'
     C*
     C     RRSTAT        IFNE      'I'
     C     RRSTCD        ANDNE     '99'
     C                   ADD       RRRAMT        XXAMTE
     C                   ADD       RRRCVD        XXAMTC
     C                   Z-ADD     RREAMT        XXEAMT           11 2
     C                   END
     C*                              (CO# FND FYR CMCASE RRFEIN RRMOD)
     C     RRKY2         READE     PRR01                                  50
     C                   END
     C*
     C                   ADD       XXEAMT        XXAMTE                         ESTIMATED
     C     RRKY2         SETGT     PRR01
     C*
     C                   END
     C                   END
     C                   END
     C     RRKEY         READE     PRR01                                  60
     C                   END
     C*
     C                   ENDSR
     C*
     C*** BEGIN ADD *** 05/12/98 **************************************
     C*****************************************************************
     C* CLMTRN - This subroutine determines the amount of payments
     C*   which should be added or subtracted to/from the total paid
     C*   due to claim transfers.  Only transfers which occrred within
     C*   the current reporting period will be processed.
     C*****************************************************************
     C     CLMTRN        BEGSR
     C*
     C*  Only include transfers processed within the appropriate FYRs.
     C*
     C                   Z-ADD     1             Z
     C     FYR(Z)        DOWNE     0
     C*
     C*  Identify any claims which were transfered 'TO' the current
     C*    employer, division or FYR.
     C*
     C     CTKY          SETGT     PCT01
     C     CTKY          READPE    PCT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CTTRDT        IFLE      XXRNDT
     C     CTTRDT        ANDGT     XXPRDT
     C*
     C     EMEMP#        IFNE      CTXEMP
     C     EMDIV         ORNE      CTXDIV
     C     XXFYR         ORNE      CTXFYR
     C*
     C     CTAMT         IFNE      0
     C                   ADD       CTAMT         LSTRNF
     C                   ELSE
     C                   Z-ADD     CTCASE        XXCASE
     C                   Z-ADD     CTTRDT        XXTRDT
     C                   EXSR      TRNPMT
     C                   ADD       XX11N2        LSTRNF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CTKY          READPE    PCT01                                  60
     C                   ENDDO
     C*
     C*  Identify any claims which were transfered 'FROM' the current
     C*    employer, division or FYR.
     C*
     C     CTKY          SETGT     PCT02
     C     CTKY          READPE    PCT02                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     CTTRDT        IFLE      XXRNDT
     C     CTTRDT        ANDGT     XXPRDT
     C*
     C     EMEMP#        IFNE      CTEMP#
     C     EMDIV         ORNE      CTDIV
     C     XXFYR         ORNE      CTFYR
     C*
     C     CTAMT         IFNE      0
     C                   SUB       CTAMT         LSTRNF
     C                   ELSE
     C                   Z-ADD     CTCASE        XXCASE
     C                   Z-ADD     CTTRDT        XXTRDT
     C                   EXSR      TRNPMT
     C                   SUB       XX11N2        LSTRNF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CTKY          READPE    PCT02                                  60
     C                   ENDDO
     C*
     C                   ADD       1             Z
     C                   END                                                    FYR,Z NE 0
     C*
     C                   ENDSR
     C*****************************************************************
     C* CLMTRN - This subroutine calculates the amount paid on a claim
     C*   up to the prior run date.
     C*****************************************************************
     C     TRNPMT        BEGSR
     C*
     C                   Z-ADD     0             XX11N2
     C     PAKY2         CHAIN     PPA02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATRDT        IFLE      XXPRDT
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C                   ADD       PAAMT         XX11N2
     C                   ELSE
     C                   SUB       PAAMT         XX11N2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     PAKY2         READE     PPA02                                  60
     C                   ENDDO
     C*
     C                   ENDSR
     C*** END ADD ***** 05/12/98 ***********************************
     O**************************************************************
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
     O*** CHG 01/28/98 ***     XXTITL    80
     O                       XXTITL              60
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
     O                       XXEMP#        Z     37
     O*** CHG 01/28/98 ***     XXDIV Z   33
     O                       XXDIV         Z     43
     O          E            ROUTE          1
     O                                           23 'EFFECTIVE DATE:'
     O                       X1EFDT              37 '  /  /    '
     O          E            ROUTE          1
     O                                           23 'RUN DATE  . . :'
     O                       X1RNDT              37 '  /  /    '
     O          E            ROUTE          1
     O                                           23 'PRIOR RUN DATE:'
     O                       X1PRDT              37 '  /  /    '
     O          E            ROUTE          1
     O                                           23 'REVISION  . . :'
     O                       QQREV               37
     O          E            ROUTE          1
     O                                           23 'SDF AMOUNT  . :'
     O                       XXSDF         J     38
     O*
     O*** CHG 01/28/98 ***               23 '(C) COPYRIGHT PROGRESSI'
     O*** CHG 01/28/98 ***               46 'VE DATA SOLUTIONS. INC,'
     O*** CHG 01/28/98 ***               51 '.1997'
     O*
     O*** BEGIN CHG *** 01/28/10 **************************************
     O*****   E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*** CHG 01/28/98 ***
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              60
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O*** CHG 01/28/98 ***     XXTITL    71
     O                       XXTITL              60
     O                                           75 'PAGE:'
     O                       PAGE                80
     O          E            HEAD           1
     O*** CHG 01/28/98 ***                5 'FUND:'
     O                                            6 'GROUP:'
     O                       XXFND         Z     12
     O*** CHG 01/28/98 ***     FNNAME    80
     O                       FNNAME              84
     O          E            HEAD           1
     O                                            6 'EMPLR:'
     O*** CHG 01/28/98 ***     XXEMP#Z   12
     O                       XXEMP#        Z     16
     O*** CHG 01/28/98 ***     XXDIV Z   16
     O                       XXDIV         Z     22
     O*** CHG 01/28/98 ***     XXNAME    60
     O                       XXNAME              64
     O          E            HEAD           2
     O*** CHG 01/28/98 ***               34 'POLICY PERIOD:'
     O                                           14 'POLICY PERIOD:'
     O*** CHG 01/28/98 ***     XXSTPOY   43
     O                       XXSTPO              25 '  /  /    '
     O*** CHG 01/28/98 ***               45 '-'
     O                                           27 '-'
     O*** CHG 01/28/98 ***     XXENPOY   54
     O                       XXENPO              38 '  /  /    '
     O                                           69 'RUN DATE:'
     O                       X1RNDT              80 '  /  /    '
     O          E            HEAD           2
     O                                           60 'THIS MONTH'
     O                                           78 'LAST MONTH'
     O          E            DET            2
     O                                            3 ' 1.'
     O                       HDG(1)              46
     O                       LSPREV        J     61
     O                       XXPREV        J     79
     O          E            DET            2
     O                                            3 ' 2.'
     O                       HDG(2)              46
     O                       LSPMTS        J     61
     O                       XXPMTS        J     79
     O          E            DET            2
     O                                            3 ' 3.'
     O                       HDG(3)              46
     O                                           46 '('
     O                       LSRECV        J     61
     O                                           64 ') ('
     O                                           80 ')'
     O                       XXRECV        J     79
     O          E            DET            2
     O                                            3 ' 4.'
     O                       HDG(4)              46
     O                       LSADJ         J     61
     O                       XXADJ         J     79
     O*** BEGIN ADD *** 05/12/98 **************************************
     O          E            DET            2
     O                                            3 ' 5.'
     O                       HDG(20)             46
     O                       LSTRNF        J     61
     O                       XXTRNF        J     79
     O*** END ADD ***** 05/12/98 **************************************
     O          E            DET            2
     O                                            3 ' 6.'
     O                       HDG(5)              46
     O                       XXPDTD        J     61
     O                       LSPREV        J     79
     O          E            DET            2
     O                                            3 ' 7.'
     O                       HDG(6)              46
     O                       LSRSVO        J     61
     O                       XXRSVO        J     79
     O          E            DET            2
     O                                            3 ' 8.'
     O                       HDG(7)              46
     O                       XXTLIN        J     61
     O                       XXPINC        J     79
     O          E            DET            2
     O                                            3 ' 9.'
     O                       HDG(8)              46
     O                       LSTLEX        J     61
     O                       XXTLEX        J     79
     O          E            DET            2
     O                                            3 '10.'
     O                       HDG(9)              46
     O                       XXAGLS        J     61
     O                       XXPAGG        J     79
     O          E    65      DET            2
     O                                            3 '11.'
     O                       HDG(10)             46
     O                       LSSDF         J     61
     O                       XXPSDF        J     79
     O          E    65      DET            2
     O                                            3 '12.'
     O                       HDG(11)             46
     O                       XXCHRG        J     61
     O                       XXPCHG        J     79
     O          E            DET            2
     O               65                           3 '13.'
     O              N65                           3 '11.'
     O                       HDG(12)             46
     O                       LSEAPR        J     61
     O                       XXEAPR        J     79
     O          E            DET            2
     O               65                           3 '14.'
     O              N65                           3 '12.'
     O                       HDG(13)             46
     O                       LSATTP        1     28
     O                       XXFPCT        J     61
     O                  38                       43 '**MINIMUM AMT'
     O                       XXPFPC        J     79
     O          E            DET            2
     O               65                           3 '15.'
     O              N65                           3 '13.'
     O                       HDG(14)             46
     O                  20                       46 '('
     O                       XXFBAL        J     61
     O                  20                       61 ')'
     O                  21                       64 '('
     O                       XXPFBA        J     79
     O                  21                       79 ')'
     O          E   N35      DET            2
     O               65                           3 '16.'
     O              N65                           3 '14.'
     O                       HDG(17)             46
     O                       XXLOSS              61 '   0.  %'
     O                       XXPLSS              79 '   0.  %'
     O          E    35      DET            1
     O               65                           3 '16.'
     O              N65                           3 '14.'
     O                       HDG(15)             46
     O                       RIIPCT        1     45
     O                       LSAGGR        J     61
     O                       XXPSAG        J     79
     O          E    35      DET            1
     O                  39                       45 '**MINIMUM AMT'
     O          E            DET            2
     O               65                           3 '17.'
     O              N65                           3 '15.'
     O                  35   HDG(16)             46
     O               35 36                       46 '('
     O                  35   XXABAL        J     61
     O               35 36                       61 ')'
     O               35 37                       64 '('
     O                  35   XXPABL        J     79
     O               35 37                       79 ')'
     O                 N35   HDG(18)             46
     O                 N35   LSTLCL        Z     61
     O                 N35   XXTLCL        Z     79
     O          E            DET            2
     O               65                           3 '18.'
     O              N65                           3 '16.'
     O                  35   HDG(17)             46
     O                  35   XXLOSS              61 '   0.  %'
     O                  35   XXPLSS              80 '   0.  %'
     O                 N35   HDG(19)             46
     O                 N35   LSOPEN        Z     61
     O                 N35   XXOPEN        Z     79
     O          E    35      DET            2
     O               65                           3 '19.'
     O              N65                           3 '17.'
     O                       HDG(18)             46
     O                       LSTLCL        Z     61
     O                       XXTLCL        Z     79
     O          E    35      DET            2
     O               65                           3 '20.'
     O              N65                           3 '18.'
     O                       HDG(19)             46
     O                       LSOPEN        Z     61
     O                       XXOPEN        Z     79
     O          E    35      DET            2
     O               65                          23 '* TOTAL "ESTIMATED" SDF'
     O               65                          46 'REIMBURSEMENTS TO DATE'
     O               65      XXAMTE        J     61
     O          E    35      DET            2
     O               65                          23 '* ACTUAL SDF RECOVERIES'
     O               65                          33 'COLLECTED'
     O               65      XXAMTC        J     61
     O          E    35      DET            2
     OQSYSPRT1  E            ROUT1          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT1          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT1          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUT1          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT1          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT1          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT1          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT1          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT1          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUT1          1
     O**********     08      ORDIS2              60
     O**********E            ROUT1          1
     O**********     08      ORDIS3              60
     O**********E            ROUT1          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUT1          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUT1          1
     O               08      ORDIS2              60
     O          E            ROUT1          1
     O               08      ORDIS3              60
     O          E            ROUT1          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUT1          2
     O                                           19 'REPORT TITLE:'
     O*** CHG 01/28/98 ***     X1TITL    80
     O                       X1TITL              59
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUT1          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUT1          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUT1       1  2
     O                                           23 'REPORT SELECTIONS'
     O          E            ROUT1          1
     O*** CHG 01/28/98 ***               23 'EMPLOYER #    :'
     O                                           23 'EMPLOYER / DIV:'
     O*** CHG 01/28/98 ***     XXEMP#Z   29
     O                       XXEMP#        Z     37
     O*** CHG 01/28/98 ***     XXDIV Z   33
     O                       XXDIV         Z     43
     O          E            ROUT1          1
     O                                           23 'EFFECTIVE DATE:'
     O                       X1EFDT              37 '  /  /    '
     O          E            ROUT1          1
     O                                           23 'RUN DATE  . . :'
     O                       X1RNDT              37 '  /  /    '
     O          E            ROUT1          1
     O                                           23 'PRIOR RUN DATE:'
     O                       X1PRDT              37 '  /  /    '
     O          E            ROUT1          1
     O                                           23 'REVISION  . . :'
     O                       QQREV               37
     O          E            ROUT1          1
     O                                           23 'SDF AMOUNT  . :'
     O                       XXSDF         J     38
     O*** CHG 01/28/98 ***               23 '(C) COPYRIGHT PROGRESSI'
     O*** CHG 01/28/98 ***               46 'VE DATA SOLUTIONS. INC,'
     O*** CHG 01/28/98 ***               51 '.1997'
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUT1
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUT1          1 59
     O                       #BRND1
     O          E            ROUT1          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O*** CHG 01/28/98 ***     CONAME    80
     O                       CONAME              82
     O                       DSDATE             132 '  /  /    '
     O          E            HEAD1          2
     O                       Q1PGM               10
     O*** CHG 01/28/98 ***     X1TITL    90
     O                       X1TITL              82
     O                                          126 'PAGE:'
     O                       PAGE1              132
     O          E            HEAD1          1
     O*** CHG 01/28/98 ***                5 'FUND:'
     O                                            6 'GROUP:'
     O                       XXFND         Z     12
     O*** CHG 01/28/98 ***     FNNAME    80
     O                       FNNAME              84
     O          E            HEAD1          1
     O                                            6 'EMPLR:'
     O*** CHG 01/28/98 ***     XXEMP#Z   12
     O                       XXEMP#        Z     16
     O*** CHG 01/28/98 ***     XXDIV Z   16
     O                       XXDIV         Z     22
     O*** CHG 01/28/98 ***     XXNAME    60
     O                       XXNAME              64
     O          E            HEAD1          2
     O*** CHG 01/28/98 ***               34 'POLICY PERIOD:'
     O                                           14 'POLICY PERIOD:'
     O*** CHG 01/28/98 ***     XXSTPOY   43
     O                       XXSTPO              25 '  /  /    '
     O*** CHG 01/28/98 ***               45 '-'
     O                                           27 '-'
     O*** CHG 01/28/98 ***     XXENPOY   54
     O                       XXENPO              38 '  /  /    '
     O                                          121 'RUN DATE:'
     O                       X1RNDT             132 '  /  /    '
     O          E            HEAD1          1
     O                                            8 'CLAIMANT'
     O                                           70 'PAID            RESERVE'
     O                                          104 'TOTAL         INDIVIDUAL'
     O                                          121 'EXCESS OVER'
     O          E            HEAD1          2
     O                                           04 'YEAR'
     O                                           09 'CASE'
     O                                           35 'ACCIDENT DATE'
     O                                           48 'MULT CLAIM#'
     O                                          104 'RETENTION'
     O                                          120 'RETENTION'
     O          E            EXCS           1
     O                       XXNAME              40
     O                       SEPAID        J     56
     O                       SERESV        J     72
     O                       XXINC         J     89
     O                       SERETN        J    106
     O                       XXEXCS        J    123
     O          E            EXCS           2
     O                       SEFYR         Z     03
     O              N79      XXCASE              14
     O                       XXACDT              33 '  /  /    '
     O                       SEMCLM        Z     46
     O          E            TOT         1  2
     O                                           38 'TOTALS:'
     O                       XXTOTP        J     56
     O                       XXTOTR        J     72
     O                       XXTOTI        J     89
     O                       XXTLRE        J    106
     O          E            TOT            1
     O                                           94 'SPECIFIC EXCESS OVER'
     O                                          104 'RETENTION'
     O                       LSTLEX        J    123
     OQSYSPRT2  E            ROUT2          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT2          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT2          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT2          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT2          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUT2          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUT2          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT2          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUT2          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT2          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUT2          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUT2          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUT2          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUT2          1
     O**********     08      ORDIS2              60
     O**********E            ROUT2          1
     O**********     08      ORDIS3              60
     O**********E            ROUT2          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *************************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUT2          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUT2          1
     O               08      ORDIS2              60
     O          E            ROUT2          1
     O               08      ORDIS3              60
     O          E            ROUT2          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUT2          2
     O                                           19 'REPORT TITLE:'
     O                       X2TITL              60
      ***** Beg Del ***** 08/26/14 *************************************************
     O**********E            ROUT2          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUT2          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O          E            ROUT2       1  2
     O                                           23 'REPORT SELECTIONS'
     O          E            ROUT2          1
     O*** CHG 01/28/98 ***               23 'EMPLOYER #    :'
     O                                           23 'EMPLOYER / DIV:'
     O*** CHG 01/28/98 ***     XXEMP#Z   29
     O                       XXEMP#        Z     37
     O*** CHG 01/28/98 ***     XXDIV Z   33
     O                       XXDIV         Z     43
     O          E            ROUT2          1
     O                                           23 'EFFECTIVE DATE:'
     O                       X1EFDT              37 '  /  /    '
     O          E            ROUT2          1
     O                                           23 'RUN DATE  . . :'
     O                       X1RNDT              37 '  /  /    '
     O          E            ROUT2          1
     O                                           23 'PRIOR RUN DATE:'
     O                       X1PRDT              37 '  /  /    '
     O          E            ROUT2          1
     O                                           23 'REVISION  . . :'
     O                       QQREV               37
     O          E            ROUT2          1
     O                                           23 'SDF AMOUNT  . :'
     O                       XXSDF         J     38
     O*** CHG 01/28/98 ***               23 '(C) COPYRIGHT PROGRESSI'
     O*** CHG 01/28/98 ***               46 'VE DATA SOLUTIONS. INC,'
     O*** CHG 01/28/98 ***               51 '.1997'
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUT2
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUT2          1 59
     O                       #BRND1
     O          E            ROUT2          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O*** CHG 01/28/98 ***     CONAME    80
     O                       CONAME              84
     O                       DSDATE        Y    123
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                       X2TITL              80
     O                                          117 'PAGE:'
     O                       PAGE2              123
     O          E            HEAD2          1
     O*** CHG 01/28/98 ***                5 'FUND:'
     O                                            6 'GROUP:'
     O                       XXFND         Z     12
     O*** CHG 01/28/98 ***     FNNAME    80
     O                       FNNAME              84
     O          E            HEAD2          1
     O                                            6 'EMPLR:'
     O*** CHG 01/28/98 ***     XXEMP#Z   12
     O                       XXEMP#        Z     16
     O*** CHG 01/28/98 ***     XXDIV Z   16
     O                       XXDIV         Z     22
     O*** CHG 01/28/98 ***     XXNAME    60
     O                       XXNAME              64
     O          E            HEAD2          2
     O*** CHG 01/28/98 ***               34 'POLICY PERIOD:'
     O                                           14 'POLICY PERIOD:'
     O*** CHG 01/28/98 ***     XXSTPOY   43
     O                       XXSTPO              25 '  /  /    '
     O*** CHG 01/28/98 ***               45 '-'
     O                                           25 '-'
     O*** CHG 01/28/98 ***     XXENPOY   54
     O                       XXENPO              38 '  /  /    '
     O                                          121 'RUN DATE:'
     O                       X1RNDT             132 '  /  /    '
     O          E            HEAD2          1
     O                                            8 'CLAIMANT'
     O                                           70 'PAID            RESERVE'
     O                                           85 'TOTAL'
     O          E            HEAD2          2
     O                                           04 'YEAR'
     O                                           09 'CASE'
     O                                           35 'ACCIDENT DATE'
     O                                           48 'MULT CLAIM#'
     O          E            MLT            1
     O                       XXNAME              40
     O                       SEPAID        J     56
     O                       SERESV        J     72
     O                       XXINC         J     89
     O          E            MLT            2
     O                       SEFYR         Z     03
     O                       XXCASE              14
     O                       XXACDT              33 '  /  /    '
     O                       SEMCLM        Z     46
**
PAYMENTS MADE THRU END OF PREVIOUS MONTH
PAYMENTS MADE THIS MONTH
RECOVERIES THIS MONTH
ADJUSTMENTS
TOTAL CLAIMS PAID TO DATE
RESERVES ON OPEN CLAIMS
TOTAL INCURRED LOSS
LESS EXCESS OVER RETENTION
LOSSES SUBJECT TO AGGREGATE LOSS FUND
SPECIAL DISABILITY FUND
TOTAL CHARGES TO LOSS FUND
EARNED NORMAL PREMIUM TO DATE
PURE CLAIMS FUND  XXX.XX%
PURE CLAIMS FUND BALANCE
CLAIMS FUND INCL SELF FUNDED AGGR. XXX.XX%          CHG 01/27/94
CLAIMS FUND BAL. INCL. SELF FUNDED AGGR.
LOSS RATIO
CLAIMS (#) REPORTED TO DATE
NUMBER OF OPEN CLAIMS
CLAIM TRANSFERS (TOTAL PAID)
