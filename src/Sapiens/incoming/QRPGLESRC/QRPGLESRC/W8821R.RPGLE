     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8821R - EXCESS CLAIMS REPORT (VARIABLE % OF RETENTION)
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  11/13/98 C:61 PROJECT WC577                                  *
     H*                Correct passing of FYR parm.                   *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  07/07/92  JEI  PROJECT: WC157                                *
     H*                 CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR ALL *
     H*                 'BASE' FILES (XXCASE) - CASE NUMBER REMAINS   *
     H*                 14 ALPHA FOR REINSURANCE FILES (X@CASE)       *
     H*                                                               *
     H*  04/03/92  LPK  PROJECT: MD027                                *
     H*                 CHANGED CASE NUMBER FROM 7 NUMERIC TO 14      *
     H*                 ALPHA FIELD.                                  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*     *IN60 - FILE CONDITION                                    *
     H*     *IN78 - ENTERED FYR = 0                                   *
     H*     *IN79 - MULTIPLE CLAIM FLAG                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMRIL1    IF   E           K DISK
     FWMRHL1    IF   E           K DISK
     FWXECL1    UF A E           K DISK
     FWAMCL     IF   E           K DISK
     FWAMCL1    IF   E           K DISK
     F                                     RENAME(PMC01:PMC02)

     D*
     D MC              S             14    DIM(500)                             MULT CLAIM#
     D MA              S             11  2 DIM(500)                             MULT CLM AMT'S
     D DT              S              6  0 DIM(500)                             MC LAST RSV DT
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
     D                                     DIM(10)                              CLAIM RSVS
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
     D  RHR                    1     90  2
     D                                     DIM(10)                              RSV HIST RSVS
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
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
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE WORK FILE
     C*
     C     ECKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    X@CASE
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
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
     C*   KEY LIST FOR THE CLAIM HISTORY FILE
     C*
     C     CHKEY         KLIST
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
     C*   KEY LIST FOR THE RESERVE HISTORY FILE
     C*
     C     RHKEY         KLIST
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
     C                   KFLD                    CMACDT
     C*
     C*   KEY LIST FOR CLAIMANT DEMOGRAPHICS FILE.
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C     *LIKE         DEFINE    RIEMP#        XXEMP#
     C     *LIKE         DEFINE    RIDIV         XXDIV
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMACDT        XXRNDT
     C     *LIKE         DEFINE    CMCASE        XXCASE                         CASE#
     C     *LIKE         DEFINE    MCCASE        X@CASE
     C     *LIKE         DEFINE    MCCLM#        XXMCLM                         MULT CLM#
     C     *LIKE         DEFINE    RILM01        XXFNDR                         FND RETENTION
     C     *LIKE         DEFINE    RILM01        XXHLDR                         HOLD RETN
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
      ***** BEG DEL ******* 11/13/98 **********************************
     C*********            PARM           QQFYR   2
      ***** END DEL ******* 11/13/98 **********************************
      ***** BEG ADD ******* 11/13/98 **********************************
     C                   PARM                    QQFYR             3
      ***** END ADD ******* 11/13/98 **********************************
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQPCT             5
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
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   Z-ADD     0             XXFYR
     C                   MOVE      QQFYR         XXFYR
     C*
     C                   MOVE      QQRNDT        X1RNDT            8 0
     C*
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRNDT
     C*
     C                   MOVE      QQPCT         XXPCNT            5 2
     C     XXPCNT        DIV       100           XXPCT             5 4
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C*   SET THE CONDITIONING INDICATORS & INITIALIZE
     C*
     C     XXFYR         COMP      0                                      78
     C*
     C*   START READING CLAIM FILE
     C*
     C   78F1KEY         CHAIN     PF101                              60
     C  N78F1KY1         CHAIN     PF101                              60
     C     *IN60         DOWEQ     '0'
     C   78              Z-ADD     F1FYR         XXFYR
     C*
     C*   START READING CLAIM FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     0             CO                             REIN CO#
     C                   Z-ADD     0             LM                             REIN LIMIT
     C                   MOVE      CMCASE        XXCASE
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C*
     C*  CLIAM MAY ALREADY EXIST IN WORK FILE AS A MULTIPLE CLAIM
     C*                                (CO,FND,FYR,CASE)
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      XXCASE        X@CASE
     C     ECKEY         CHAIN     PEC01                              60
     C     *IN60         IFEQ      '1'
     C*
     C*  GET REINSURANCE POLICY FILE TO DETERMINE RETENTION DURING THIS
     C*  ACCIDENT DATE PERIOD. MUST BE THE SPECIFIC POLICY (RIASFL=S).
     C*  REINSURANC COMPANY# 999 REPRESENTS THE FUNDS PORTION OF COV.
     C*
     C*                               (CO# FND EMP# DIV XXASFL CMACDT)
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
     C                   MOVE      *BLANKS       MC                             MULT CLM#
     C                   MOVE      *BLANKS       MA                             MULT CLM AMT
     C                   Z-ADD     0             XXFNDR                         FUND RETEN.
     C                   Z-ADD     0             XXMCLM                         MULT CLAIM#
     C                   Z-ADD     0             ECTOTI                         TOT INC
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
     C*  CALCULATE % OF RETENTION
     C*
     C                   Z-ADD     XXFNDR        XXHLDR                         HOLD RETN
     C                   MULT      XXPCT         XXFNDR
     C*
     C*  CALCULATE TOTAL INCURRED FROM CLAIM FILE, IF GREATER THAN
     C*  THE FUNDS PORTION(999) OF COVERAGE- CONTINUE, ELSE GET NEXT
     C*  CLAIM
     C*
     C*  IF MULTIPLE CLAIM, ACCUMULATE TOTAL INCURRED FOR ALL CLAIMS
     C*  LISTED UNDER THE MULTIPLE CLAIM
     C*
     C                   MOVE      '0'           *IN79
     C     MCKEY         CHAIN     PMC01                              60
     C     *IN60         IFEQ      '0'
     C                   EXSR      MULT
     C                   MOVE      '1'           *IN79
     C                   ELSE
     C                   XFOOT     CMA           XXPMT            11 2
     C                   XFOOT     CMR           XXRSV            11 2
     C     XXPMT         ADD       XXRSV         ECTOTI
     C                   END
     C*
     C                   Z-ADD     0             ECRTDT
     C                   Z-ADD     0             ECRSDT
     C*
     C     ECTOTI        IFGE      XXFNDR
     C*
     C  N79              EXSR      GETINC
     C   79              EXSR      MLTINC
     C*
     C*  IF RETENTION LEVEL % REACHED BEFOR RUN DATE, WRITE TO FILE
     C*
     C     ECTOTI        IFGE      XXFNDR
     C*
     C                   Z-ADD     XXCO#         ECCO#
     C                   Z-ADD     XXFND         ECFND
     C                   Z-ADD     XXFYR         ECFYR
     C                   Z-ADD     XXRNDT        ECRNDT
     C                   Z-ADD     XXPCNT        ECPCT
     C  N79              MOVE      X@CASE        ECCASE
     C   79              MOVE      *ZEROS        ECCASE
     C                   MOVE      *BLANKS       ECNAME
     C  N79              EXSR      GETNME
     C                   Z-ADD     CMACDT        ECACDT
     C                   MOVE      *BLANKS       ECOCST
     C  N79              EXSR      GETSTS
     C                   Z-ADD     XXMCLM        ECCLM#
     C                   Z-ADD     XXHLDR        ECRETN
     C   79              Z-ADD     0             ECRTDT
     C   79              Z-ADD     0             ECRSDT
     C                   MOVE      *BLANKS       ECSTAT
     C                   MOVE      Q1USER        ECUSER
     C                   MOVE      Q1PGM         ECPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ECTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ECDT
     C*
     C                   WRITE     PEC01
     C*
     C*  IF MULTIPLE CLAIM, WRITE DETAIL RECORD FOR EACH CLAIM
     C*
     C     *IN79         IFEQ      '1'
     C*
     C                   Z-ADD     1             N
     C     MC(N)         DOWNE     *BLANKS
     C*
     C                   MOVE      MC(N)         XXCASE
     C     CMKY1         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      XXCASE        ECCASE
     C                   MOVE      *BLANKS       ECNAME
     C                   EXSR      GETNME
     C                   EXSR      GETSTS
     C                   Z-ADD     0             ECRTDT
     C                   Z-ADD     DT(N)         ECRSDT
     C                   Z-ADD     MA(N)         ECTOTI
     C                   WRITE     PEC01
     C                   END                                                    CMKY1
     C*
     C                   ADD       1             N
     C     N             IFEQ      501
     C                   SUB       1             N
     C                   Z-ADD     0             MA(N)
     C                   END
     C*
     C                   END                                                    MC,N DOWNE 0
     C*
     C                   END                                                    *IN79 EQ '1'
     C*
     C                   END                                                    ECTOTI >=
     C*
     C                   END                                                    ECTOTI >=
     C*
     C                   END                                                    RIKEY
     C*
     C                   END                                                    ECKEY
     C*
     C*  GET NEXT CLAIM
     C*
     C     CMKEY         READE     PCM01                                  60
     C                   END
     C*
     C*  GET NEXT FYR
     C*
     C   78F1KEY         READE     PF101                                  60
     C  N78              MOVE      '1'           *IN60
     C                   END
     C*
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************
     C*    MULT SUBROUTINE - GET TOTAL INCURRED FROM CLAIM FILE FOR ALL
     C*              CLAIMS WITHIN THE MULTIPLE CLAIM.
     C**************************************************************
     C*
     C     MULT          BEGSR
     C*
     C                   Z-ADD     MCCLM#        XXMCLM
     C                   MOVE      X@CASE        SVCASE           14
     C                   Z-ADD     0             XXPMT
     C                   Z-ADD     0             XXRSV
     C*
     C* ACCUMULATE TOTAL PAYMENTS FOR ALL CASES IN MULTIPLE CLAIM
     C* THE FIRST CASE HIT IN THE CHAIN TO MULT CLAIM FILE WILL BE IN
     C* SAME ORDER AS CLAIM MASTER, THEREFORE, IF WE DO NOT HIT THE
     C* CASE ON THE FIRST CHAIN, WE DON'T HAVE TO RECALC.
     C*
     C     MCKY1         CHAIN     PMC02                              60
     C     *IN60         IFEQ      '0'
     C     MCCASE        ANDEQ     SVCASE
     C*
     C     *IN60         DOWEQ     '0'
     C                   MOVE      MCCASE        XXCASE
     C     CMKY1         CHAIN     PCM01                              60
     C                   XFOOT     CMA           XXMPD            11 2
     C                   ADD       XXMPD         XXPMT
     C                   XFOOT     CMR           XXMRSV           11 2
     C                   ADD       XXMRSV        XXRSV
     C     MCKY1         READE     PMC02                                  60
     C                   END
     C*
     C                   END
     C     XXPMT         ADD       XXRSV         ECTOTI
     C*
     C*  RESET POINTER IN CLAIM MASTER FILE TO CONTINUE PROCESSING
     C*
     C                   MOVE      SVCASE        XXCASE
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SVCASE        X@CASE
     C     CMKY1         CHAIN     PCM01                              60
     C*
     C                   ENDSR
     C*
     C**************************************************************
     C*    GETINC SUBROUTINE - GET TOTAL INCURRED AS OF RUN DATE
     C**************************************************************
     C*
     C     GETINC        BEGSR
     C*
     C                   Z-ADD     0             ECTOTI
     C                   Z-ADD     0             ECRTDT
     C                   Z-ADD     0             ECRSDT
     C*
     C*  READ RESERVE HISTORY FILE UP TO RUN DATE & RECORD:
     C*       DATE RETENTION % REACHED
     C*       DATE OF LAST RESERVE CHANGE
     C*                                (CO FND FYR CASE)
     C     RHKEY         CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C     RHEFDT        ANDLE     XXRNDT
     C*
     C                   Z-ADD     RHEFDT        ECRSDT
     C                   XFOOT     RHR           XXRSV
     C                   ADD       XXRSV         ECTOTI
     C     ECTOTI        IFGT      XXFNDR
     C     ECRTDT        ANDEQ     0
     C                   Z-ADD     RHEFDT        ECRTDT
     C                   END
     C*
     C     RHKEY         READE     PRH01                                  60
     C                   END
     C*
     C                   ENDSR
     C*
     C**************************************************************
     C*    MLTINC SUBROUTINE - BREAK OUT INCURRED AMTS FOR ALL
     C*              CLAIMS WITHIN A MULTIPLE CLAIM.
     C**************************************************************
     C*
     C     MLTINC        BEGSR
     C*
     C                   Z-ADD     1             N                 4 0
     C                   MOVE      X@CASE        SVCASE
     C*                                (XXCO# XXFND XXFYR XXMCLM)
     C     MCKY1         CHAIN     PMC02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      MCCASE        X@CASE
     C                   EXSR      GETINC
     C*
     C     N             IFLE      500
     C                   MOVE      MCCASE        MC(N)
     C                   Z-ADD     ECTOTI        MA(N)
     C                   Z-ADD     ECRSDT        DT(N)
     C                   END
     C                   ADD       1             N
     C*
     C     MCKY1         READE     PMC02                                  60
     C                   END
     C*
     C                   XFOOT     MA            ECTOTI
     C                   MOVE      SVCASE        XXCASE
     C                   MOVE      *ZEROS        X@CASE
     C                   MOVE      SVCASE        X@CASE
     C*
     C                   ENDSR
     C*
     C**************************************************************
     C*    GETNME SUBROUTINE - GET CLAIMANT NAME
     C**************************************************************
     C*
     C     GETNME        BEGSR
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLNAM           56
     C                   MOVEL     CDFSN         QQFNAM           15
     C                   MOVEL     ' '           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLNAM
     C                   PARM                    QQFNAM
     C                   PARM                    QQTERM
     C                   MOVEL     QQLNAM        ECNAME
     C                   END
     C*
     C                   ENDSR
     C*
     C**************************************************************
     C*    GETSTS SUBROUTINE - GET CLAIM STATUS
     C**************************************************************
     C*
     C     GETSTS        BEGSR
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHCO#         ANDEQ     XXCO#
     C     CHFND         ANDEQ     XXFND
     C     CHFYR         ANDEQ     XXFYR
     C     CHCASE        ANDEQ     XXCASE
     C                   MOVE      CHOCST        ECOCST
     C                   ELSE
     C                   MOVE      CMSTAT        ECOCST
     C                   END
     C*
     C                   ENDSR
