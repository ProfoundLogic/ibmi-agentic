     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *****************************************************************
      /TITLE WA010R - EXCESS REINSURANCE
      *****************************************************************
      *  DESCRIPTION:  THIS PROGRAM WILL GET THE SPECIFIC RETENTION   *
      *                LEVEL FOR A CLAIM, IF IT EXISTS.               *
      *                                                               *
      *  CALLED FROM: W0070R06, 07, 08, 09, 10, 11, 12, WA009R        *
      *                                                               *
      *  CALLS TO: NONE                                               *
      *                                                               *
      *                                                               *
      *  CREATED: 06/18/97  PROGRAMMER: C:47  PROJECT: 60ANC          *
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *****************************************************************
      *  EXAMPLE OF CODE TO USE FOR CALLING PROGRAM:                  *
      *
     F*WMCM*   IF  E           K        DISK
     F*                                              KINFDS DBFDS
     F*DBFDS       DS
     F*                                    B 397 4000DBRRN
     F*           CMKY      CHAINPCM01                60
     F*                     Z-ADDDBRRN     @@RRN
      *
      *  GET EXCESS REINSURANCE INFO
      *
     F*           CMEXCS    IFNE 'Y'
     F*           @@RRN     IFNE *ZERO
     F*                     CALL 'WA010R'
     F*                     PARM           @@RRN            FOR WMCMP
     F*                     PARM           XXFNDR 132
     F*                     PARM           XXEXRE  1
     F*                     END
     F*                     END
      *
     F*           XXEXRE    IFEQ 'Y'
     F*           CMEXCS    OREQ 'Y'
     F*                     MOVELXCEXRE    SCMSG1    P
     F*                     MOVE *ON       *IN69
     F*                     END
      *                                                               *
      *****************************************************************
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:DBRCDF)
     F                                     INFDS(DBFDS)
      *  EXCS REIN
     FWMRIL1    IF   E           K DISK
     FWAMCL     IF   E           K DISK
     FWAMCL1    IF   E           K DISK
     F                                     RENAME(PMC01:PMC02)
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
      *  GROUP DETAIL FILE
     FWDF1L     IF   E           K DISK
      *
      *

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D                 DS                  INZ
     D  AR                     1     90  2
     D                                     DIM(10)                              PMT AGAINST RR
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
     D                 DS                  INZ
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
     D                 DS                  INZ
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

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0          FOR WMCMP
     C                   PARM                    XXFNDR
     C                   PARM                    XXEXRE
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  CLAIM MASTER RECORD FORMAT PCM02
     C*
     C     CMKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     F1KY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C*
     C*   Key List for the Multiple Claim file
     C*
     C     MCKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    X@CASE           14
     C*
     C     MCKY2         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    XXMCLM
     C*
     C*   KEY LIST FOR REINSURANCE POLICY FILE
     C*
     C     RIKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL
     C                   KFLD                    CMACDT
     C*
     C     RIKY1         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXASFL
     C*
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCASE        SVCASE
     C     *LIKE         DEFINE    MCCLM#        XXMCLM
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C                   EXSR      SRREIN
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
     C* **********************************************************
     C*  SRREIN - Subroutine to Determine if Claim is in         *
     C*           Specific Reinsurance. (Total Paid > Retention) *
     C* **********************************************************
     C     SRREIN        BEGSR
     C*
     C*  GET REINSURANCE POLICY FILE TO DETERMINE RETENTION DURING THIS
     C*  ACCIDENT DATE PERIOD. MUST BE THE SPECIFIC POLICY (RIASFL=S).
     C*  REINSURANCE COMPANY# 999 REPRESENTS THE FUND'S PORTION OF COV.
     C*
     C*                               (CO# FND EMP# DIV XXASFL CMACDT)
     C*
     C*  IF NOT DEFINED ON EMPLOYER LEVEL, GET FROM FUND LEVEL;
      *  FIRST, TRY TO GET SPECIFIC RETENTION FROM WMRIP USING EMP#/DIV.
      *    ELSE, TRY TO GET SPECIFIC RET FROM WMRIP WITHOUT USING EMP#/DIV.
      *      ELSE, TRY TO GET SPECIFIC RET FROM WDF1P:F1EMRT.
     C*
     C                   Z-ADD     CMEMP#        XXEMP#
     C                   Z-ADD     CMDIV         XXDIV
     C                   Z-ADD     CMCASE        SVCASE
     C                   MOVE      'S'           XXASFL            1
     C     RIKY          SETGT     PRI01
     C     RIKY1         READPE    PRI01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C     RIKY          SETGT     PRI01
     C     RIKY1         READPE    PRI01                                  60
     C     *IN60         IFEQ      *ON                                           NOT FOUND
      *
     C     F1KY          CHAIN(N)  PF101                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     F1EMRT        XXFNDR                         FUND RETEN.
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 CHAIN
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     0             XXFNDR                         FUND RETEN.
     C*
     C*  GET FUND RETENTION AMOUNT  (CO# = 999)
     C*
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    CO(X)                              60
     C     *IN60         IFEQ      *ON
     C     CO(X)         ANDEQ     999
     C                   Z-ADD     LM(X)         XXFNDR           13 2          FUND RETENTION
     C                   ENDIF
     C                   ENDIF
     C*
     C*  IF MULTIPLE CLAIM, ACCUMULATE TOTAL INCURRED FOR ALL CLAIMS
     C*  LISTED UNDER THE MULTIPLE CLAIM
     C*
     C                   MOVE      *ZERO         X@CASE
     C                   MOVE      CMCASE        X@CASE
     C     MCKY          CHAIN     PMC01                              60
     C     *IN60         IFEQ      *OFF
     C                   EXSR      MULT
     C                   ELSE
     C                   XFOOT     AR            PAYMNT           11 2
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        XXEXRE            1
     C     PAYMNT        IFGT      *ZERO
     C     XXFNDR        ANDGT     *ZERO
     C     PAYMNT        ANDGT     XXFNDR
     C                   MOVEL     'Y'           XXEXRE
     C                   ENDIF
      *
      * ALSO CHECK THE EXCESS REIN FLAG IN THE CLAIM MASTER
      *
     C     CMEXCS        IFEQ      'Y'
     C                   MOVEL     'Y'           XXEXRE
     C                   END
     C*
     C                   ENDSR
     C*
     C* ************************************************************
     C*    MULT SUBROUTINE - GET TOTAL PAYMENTS FROM CLAIM FILE FOR ALL
     C*              CLAIMS WITHIN THE MULTIPLE CLAIM.
     C* ************************************************************
     C*
     C     MULT          BEGSR
     C*
     C                   Z-ADD     MCCLM#        XXMCLM
     C                   Z-ADD     *ZERO         PAYMNT
     C                   Z-ADD     *ZERO         N                 3 0
     C*
     C* ACCUMULATE TOTAL PAYMENTS FOR ALL CASES IN MULTIPLE CLAIM FILE.
     C* THE FIRST CASE HIT IN THE CHAIN TO MULT CLAIM FILE WILL BE IN
     C* SAME ORDER AS CLAIM MASTER, THEREFORE, IF WE DO NOT HIT THE
     C* CASE ON THE FIRST CHAIN, WE DON'T HAVE TO RECALC.
     C*
     C     MCKY2         CHAIN     PMC02                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      MCCASE        XXCASE
     C     CMKY2         CHAIN     PCM02                              60
     C                   XFOOT     AR            XXMPD            11 2
     C                   ADD       XXMPD         PAYMNT
     C                   ADD       1             N
     C     MCKY2         READE     PMC02                                  60
     C                   ENDDO
     C*
     C*  RESET POINTER IN CLAIM MASTER FILE TO CONTINUE PROCESSING
     C*
     C                   Z-ADD     SVCASE        XXCASE
     C     CMKY2         CHAIN     PCM02                              60
     C*
     C                   ENDSR
     C*
