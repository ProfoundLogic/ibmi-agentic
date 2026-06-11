     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H* 01/27/00 C:62  Project: Y2K                                   *
     H*                CHANGE DATES TO MDYY.                          *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS:                                                 **
     H*****************************************************************
     H*
     H*  ALL SUMMARY REPORT TOTALS ARE KEPT IN A MULTIPLE OCCURENCE
     H*  DATA STRUCTURES SYSVAL, ALLTOT, FINTOT, AND TOTTOT
     H*
     H*  MULT. OCCUR. DATA STRUCTURE SYSTOT
     H*  (SUBSCRIBER TOTALS)
     H*                       SYS 1 (P)   SYS 2 (H)  SYS 3 (N) . . .
     H*      TOTALS               1           2          3     . . .
     H*
     H*  MULT. OCCUR. DATA STRUCTURE FINTOT:
     H*  (FINAL TOTALS)
     H*                       SYS 1 (P)   SYS 2 (H)  SYS 3 (N) . . .
     H*      TOTALS               1           2          3     . . .
     H*
     H*  MULT. OCCUR. DATA STRUCTURE ALLTOT:
     H*  (ALL SYSTEMS SUM FOR SUBSCRIBER TOTALS)
     H*                       ALL SYSTEMS
     H*      TOTALS               1
     H*
     H*  MULT. OCCUR. DATA STRUCTURE TOTTOT:
     H*  (ALL SYSTEMS SUM FOR FINAL TOTALS)
     H*                     ALL SYSTEMS
     H*      TOTALS               1
     H*
     H*
     H*  EACH TOTAL IS STORED IN THE RESPECTIVE OCCURENCE OF EACH
     H*  DATA STRUCTURE. THE PROGRAM IS DESIGNED TO HANDLE UP TO
     H*  10 SYSTEM TYPES.
     H*
     H*****************************************************************
     H/TITLE F7150R - EOB DELETE EDIT REPORT
     F* * * ROUTING FILE
     FSMORL     IF   E           K DISK
     F* * * COMPANY FILE
     FSMCOL     IF   E           K DISK
     F* * * FUND FILE
     FFMFNL     IF   E           K DISK
     F* * * USER PARAMETER FILE
     FSMUPL     IF   E           K DISK
     F* * * DELETE EOB RECORD FILE
     FFDEOL     IF   E           K DISK
     F* * * CLAIM FILE
     FFMCML1    IF   E           K DISK
     F* * * DEMOGRAPHIC FILE
     FFMCDL     IF   E           K DISK
     F* * * SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F* * * PRINTER FILES
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOF)

     D SY              S              1    DIM(10) ASCEND
     D LD              S             18    DIM(1) CTDATA PERRCD(1)
     D HD              S             18    DIM(2) CTDATA PERRCD(1)
     D SYSVAL          DS                  OCCURS(10)
     D  SYSUM                  1     30  2
     D  SYAMTC                 1      5P 2
     D  SYMRED                 6     10P 2
     D  SYOTHR                11     15P 2
     D  SYNDIS                16     20P 2
     D  SYTPD                 21     25P 2
     D  SYSMUS                26     30P 2
     D FINTOT          DS                  OCCURS(10)
     D  LRSUM                  1     30  2
     D  LRAMTC                 1      5P 2
     D  LRMRED                 6     10P 2
     D  LROTHR                11     15P 2
     D  LRNDIS                16     20P 2
     D  LRTPD                 21     25P 2
     D  LRSMUS                26     30P 2
     D ALLTOT          DS
     D  ALAMTC                 1      5P 2
     D  ALMRED                 6     10P 2
     D  ALOTHR                11     15P 2
     D  ALNDIS                16     20P 2
     D  ALTPD                 21     25P 2
     D  ALSMUS                26     30P 2
     D TOTTOT          DS
     D  TLAMTC                 1      5P 2
     D  TLMRED                 6     10P 2
     D  TLOTHR                11     15P 2
     D  TLNDIS                16     20P 2
     D  TLTPD                 21     25P 2
     D  TLSMUS                26     30P 2
     D PRTDS1          DS
     D  FIL11                  1    200
     D  FIL12                201    366
     D  XXLIN1               367    368B 0
     D  XXPAG1               369    372B 0
     D PRTDS2          DS
     D  FIL21                  1    200
     D  FIL22                201    366
     D  XXLIN2               367    368B 0
     D  XXPAG2               369    372B 0
     D                 DS
     D*** BEGIN CHG *** 01/27/00 **************************************
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   CHG *** 01/27/00 **************************************
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XF#                    1     26
     D  XFSUB                  1      5  0
     D  XFHYP1                 6      6
     D  XFFND                  7      9  0
     D  XFHYP2                10     10
     D  XFYR                  11     12  0
     D  XFHYP3                13     13
     D  XFCASE                14     20  0
     D  XFHYP4                21     21
     D  XFEOB#                22     26  0
     D                 DS
     D  XXSS#                  1      9
     D  XXPRE                  1      3
     D  XXMID                  4      5
     D  XXSUF                  6      9

     I*

     C*
     C*   KEY LIST FOR EOB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE CLAIMAMT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE HBA FILE
     C*
     C     HBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*   KEY LIST FOR THE NETWORK TRANSACTION FILE
     C*
     C     TRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    SMNAME        LASTSN
     C     *LIKE         DEFINE    EOSUB#        LASTSB
     C     *LIKE         DEFINE    EODT          LASTDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQBTYP            1
     C                   PARM                    QQRTYP            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C                   DO        10            X
     C     X             OCCUR     SYSVAL
     C                   Z-ADD     0             SYAMTC
     C                   Z-ADD     0             SYMRED
     C                   Z-ADD     0             SYOTHR
     C                   Z-ADD     0             SYNDIS
     C                   Z-ADD     0             SYTPD
     C                   Z-ADD     0             SYSMUS
     C                   END
     C*
     C                   DO        10            X
     C     X             OCCUR     FINTOT
     C                   Z-ADD     0             LRAMTC
     C                   Z-ADD     0             LRMRED
     C                   Z-ADD     0             LROTHR
     C                   Z-ADD     0             LRNDIS
     C                   Z-ADD     0             LRTPD
     C                   Z-ADD     0             LRSMUS
     C                   END
     C*
     C*
     C                   Z-ADD     0             ALAMTC
     C                   Z-ADD     0             ALMRED
     C                   Z-ADD     0             ALOTHR
     C                   Z-ADD     0             ALNDIS
     C                   Z-ADD     0             ALTPD
     C                   Z-ADD     0             ALSMUS
     C*
     C                   Z-ADD     0             TLAMTC
     C                   Z-ADD     0             TLMRED
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLTPD
     C                   Z-ADD     0             TLSMUS
     C*
     C*
     C                   Z-ADD     0             NS                3 0
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1FRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFRDT  60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1TODT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTODT  60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C                   MOVE      '-'           XFHYP4
     C                   MOVE      '0'           *IN70
     C                   MOVE      HD(1)         HEADER           18
     C                   MOVE      LD(1)         DESCR            18
     C*
     C*   SET INDICATORS TO DETERMINE WHICH REPORTS TO PRINT
     C*   SUMMARY/DETAIL
     C*
     C     QQRTYP        IFEQ      'B'
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN11
     C                   ELSE
     C     QQRTYP        IFEQ      'D'
     C                   MOVE      '1'           *IN10
     C                   ELSE
     C     QQRTYP        IFEQ      'S'
     C                   MOVE      '1'           *IN11
     C                   END
     C                   END
     C                   END
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              30
     C     *IN30         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   END
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ORTITL        XXTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*******   10              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT1
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ORTITL        YYTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********   11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   END
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C*   READ EOB FILE
     C*
     C     EOKEY         SETLL     PEO01
     C                   READ      PEO01                                  90
     C*
     C     *IN90         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   11              EXCEPT    THEAD
     C   10              EXCEPT    ERRMSG
     C   11              EXCEPT    ERRMS1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFYR         XXFYR
     C                   MOVE      EOCASE        XXCASE
     C                   MOVE      EOEOB#        XXEOB#
     C*
     C*  CHAIN TO SUBSRCIBER FILE AND GET SUBSCRIBER NAME
     C*
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C*  CHAIN TO FUND FILE TO GET FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C                   Z-ADD     EOSUB#        XFSUB
     C                   Z-ADD     EOFND         XFFND
     C                   Z-ADD     EOFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EOCASE        XFCASE
     C                   Z-ADD     EOEOB#        XFEOB#
     C*
     C                   MOVE      EODT          LASTDT
     C                   MOVE      EOSUB#        LASTSB
     C                   MOVE      XXSBNM        LASTSN
     C*
     C*   PRINT THE HEADINGS
     C*
     C   10              EXCEPT    HEAD
     C*
     C     *IN90         DOWEQ     '0'
     C*
     C*
     C                   Z-ADD     EODT          SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXDT    60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT              8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXTRDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*
     C     EOAMTC        SUB       EOAMT         XXMRED            9 2
     C     XXMRED        SUB       EOOTHR        XXMRED
     C     EOAMT         SUB       EONDIS        XXTPD             9 2
     C*
     C*  GET ISMUS AMOUNT
     C*
     C                   Z-ADD     EOBAMT        XISMUS            9 2
     C*
     C*  ACCUMLATE SYSTEM TOTALS AND FINAL TOTALS
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C*
     C     EOSYS         LOOKUP    SY(X)                                  30
     C     *IN30         IFEQ      '0'
     C                   ADD       1             NS
     C                   MOVE      EOSYS         SY(NS)
     C                   MOVE      NS            X
     C                   END
     C     X             OCCUR     SYSVAL
     C                   ADD       EOAMTC        SYAMTC
     C                   ADD       XXMRED        SYMRED
     C                   ADD       EOOTHR        SYOTHR
     C                   ADD       EONDIS        SYNDIS
     C                   ADD       XXTPD         SYTPD
     C                   ADD       XISMUS        SYSMUS
     C*
     C*
     C     EOFLG4        IFEQ      'P'
     C                   ADD       EOAMTC        INAMTC            9 2
     C                   ADD       XXMRED        INMRED            9 2
     C                   ADD       EOOTHR        INOTHR            9 2
     C                   ADD       EONDIS        INNDIS            9 2
     C                   ADD       XXTPD         INAMT             9 2
     C                   ADD       XISMUS        INSMUS            9 2
     C                   ELSE
     C                   ADD       EOAMTC        UNAMTC            9 2
     C                   ADD       XXMRED        UNMRED            9 2
     C                   ADD       EOOTHR        UNOTHR            9 2
     C                   ADD       EONDIS        UNNDIS            9 2
     C                   ADD       XXTPD         UNAMT             9 2
     C                   ADD       XISMUS        UNSMUS            9 2
     C                   END
     C                   ADD       EOAMTC        DAAMTC            9 2
     C                   ADD       XXMRED        DAMRED            9 2
     C                   ADD       EOOTHR        DAOTHR            9 2
     C                   ADD       EONDIS        DANDIS            9 2
     C                   ADD       XXTPD         DAAMT             9 2
     C                   ADD       XISMUS        DASMUS            9 2
     C*
     C*   GET SOCIAL SECURITY NUMBER, CLAIMANT NAME, AND ACCIDENT DATE
     C*
     C     CMKEY         CHAIN     PCM01                              95
     C     *IN95         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#             9
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXACDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              96
     C     *IN96         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVELQQLAST    XXNAME 30
     C                   MOVEL     QQLAST        XXNAME           25
     C*** END   CHG *** 01/27/00 **************************************
     C                   ELSE                                                   ELSEIF *IN96
     C                   MOVE      *BLANKS       XXNAME
     C                   END                                                    END IF *IN96
     C*
     C                   ELSE                                                   ELSEIF *IN95
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C                   END                                                    END IF *IN95
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C   10              EXCEPT    DET
     C*
     C                   Z-ADD     *ZEROS        XISMUS
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C*
     C                   READ      PEO01                                  90
     C*
     C*
     C                   MOVE      EOCO#         XXCO#
     C                   MOVE      EOSUB#        XXSUB#
     C                   MOVE      EOFND         XXFND
     C                   MOVE      EOFYR         XXFYR
     C                   MOVE      EOCASE        XXCASE
     C                   MOVE      EOEOB#        XXEOB#
     C*
     C                   Z-ADD     EOSUB#        XFSUB
     C                   Z-ADD     EOFND         XFFND
     C                   Z-ADD     EOFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EOCASE        XFCASE
     C                   Z-ADD     EOEOB#        XFEOB#
     C*
     C     EOSUB#        IFNE      LASTSB
     C                   EXSR      SRTOT
     C                   EXSR      SRSBBK
     C                   ELSE
     C     EODT          IFNE      LASTDT
     C                   EXSR      SRTOT
     C                   END
     C                   END
     C*
     C                   END                                                     END *IN90
     C*
     C                   EXSR      SRTOT
     C*
     C                   EXSR      SRSBBK
     C*
     C                   EXSR      FNTOT
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (qqrtyp = 'B' or qqrtyp = 'D');
          Except Route;
       Endif;
       xxprt = '1';
       Chain Orkey Por01;
       If %found and (qqrtyp = 'B' or qqrtyp = 'S');
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*****************************************************************
     C*                                                               *
     C* THIS SUBROUTINE PRINTS THE DAILY TOTALS                       *
     C*                                                               *
     C*****************************************************************
     C     SRTOT         BEGSR
     C*
     C*
     C*
     C                   Z-ADD     LASTDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     DAYDT   60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DAYDT             8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C   10              EXCEPT    DATOT
     C*
     C                   MOVE      EODT          LASTDT
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*                                                               *
     C* SUBSRCRIBER BREAK SUBROUTINE                                  *
     C*                                                               *
     C*****************************************************************
     C*
     C     SRSBBK        BEGSR
     C*
     C   11              EXCEPT    THEAD
     C*
     C                   DO        NS            X
     C*
     C*  ONLY PRINT SYSTEM TOTALS IF ONE OF THE SYSTEM TOTALS IS
     C*  NON-ZERO
     C*
     C                   MOVE      '0'           PRTSYS            1
     C                   EXSR      CHKSYS
     C     PRTSYS        IFEQ      '0'
     C*
     C     X             OCCUR     SYSVAL
     C     X             OCCUR     FINTOT
     C                   ADD       SYAMTC        LRAMTC
     C                   ADD       SYMRED        LRMRED
     C                   ADD       SYOTHR        LROTHR
     C                   ADD       SYNDIS        LRNDIS
     C                   ADD       SYTPD         LRTPD
     C                   ADD       SYSMUS        LRSMUS
     C                   ADD       SYAMTC        ALAMTC
     C                   ADD       SYMRED        ALMRED
     C                   ADD       SYOTHR        ALOTHR
     C                   ADD       SYNDIS        ALNDIS
     C                   ADD       SYTPD         ALTPD
     C                   ADD       SYSMUS        ALSMUS
     C   11              EXCEPT    SYTOT
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    THEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   END                                                    PRTSYS ?
     C*
     C                   END                                                    DO X
     C*
     C   10              EXCEPT    SUBTOT
     C*
     C   11              EXCEPT    ALTOT
     C*
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C                   MOVE      EOSUB#        LASTSB
     C                   MOVE      XXSBNM        LASTSN
     C*
     C   10
     CANN90              EXCEPT    HEAD
     C*
     C                   ENDSR
     C*****************************************************************
     C*                                                               *
     C* THIS SUBROUTINE PRINTS ALL FINAL TOTALS FOR ALL SYSTEMS       *
     C*                                                               *
     C*****************************************************************
     C*
     C     FNTOT         BEGSR
     C*
     C                   MOVE      '1'           *IN70
     C                   MOVE      HD(2)         HEADER
     C*
     C   11              EXCEPT    THEAD
     C*
     C                   DO        NS            X
     C*
     C*  ONLY PRINT SYSTEM TOTALS IF ONE OF THE SYSTEM TOTALS IS
     C*  NON-ZERO
     C*
     C                   MOVE      '0'           PRTSYS
     C                   EXSR      CHKSYS
     C     PRTSYS        IFEQ      '0'
     C*
     C     X             OCCUR     FINTOT
     C                   ADD       LRAMTC        TLAMTC
     C                   ADD       LRMRED        TLMRED
     C                   ADD       LROTHR        TLOTHR
     C                   ADD       LRNDIS        TLNDIS
     C                   ADD       LRTPD         TLTPD
     C                   ADD       LRSMUS        TLSMUS
     C   11              EXCEPT    LRTOT
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    THEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   END                                                    PRTSYS ?
     C*
     C                   END                                                    DO X
     C*
     C   10              EXCEPT    LSTTOT
     C*
     C   11              EXCEPT    TLTOT
     C*
     C                   ENDSR
     C*****************************************************************
     C*****************************************************************
     C*                                                               *
     C* THIS SUBROUTINE DETERMINES IF ANY OF THE TOTALS FOR THE       *
     C* CURRENT SYSTEM ARE NON-ZERO. IF ALL TOTALS ARE ZERO DON'T     *
     C* PRINT THIS SYSTEM                                             *
     C*                                                               *
     C*****************************************************************
     C     CHKSYS        BEGSR
     C*
     C     *IN70         IFEQ      '0'
     C*
     C     X             OCCUR     SYSVAL
     C     SYAMTC        IFEQ      *ZEROS
     C     SYMRED        ANDEQ     *ZEROS
     C     SYOTHR        ANDEQ     *ZEROS
     C     SYNDIS        ANDEQ     *ZEROS
     C     SYTPD         ANDEQ     *ZEROS
     C     SYSMUS        ANDEQ     *ZEROS
     C                   MOVE      '1'           PRTSYS
     C                   END
     C                   ELSE
     C     X             OCCUR     FINTOT
     C     LRAMTC        IFEQ      *ZEROS
     C     LRMRED        ANDEQ     *ZEROS
     C     LROTHR        ANDEQ     *ZEROS
     C     LRNDIS        ANDEQ     *ZEROS
     C     LRTPD         ANDEQ     *ZEROS
     C     LRSMUS        ANDEQ     *ZEROS
     C                   MOVE      '1'           PRTSYS
     C                   END
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********        09   Q1USER              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   Q1USER              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
      **** BEGIN ADD **** 05/11/98 ************************************
     O          E            ROUTE       2  2
     O                                           19 'SUBSCRIBER:'
     O                       EOSUB#              25
     O          E            ROUTE          2
     O                                           19 'DATE RANGE:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    28 '  /  /  '
     O**********                         31 'TO'
     O**********               XXTODT    40 '  /  /  '
     O                       X1FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X1TODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ROUTE          2
     O                                           19 'SYSTEM:'
     O                       QQBTYP              21
     O          E            ROUTE          2
     O                                           19 'REPORT TYPE:'
     O                       QQRTYP              21
      **** END   ADD **** 05/11/98 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      **** BEGIN CHG **** 05/07/98 ************************************
     O********************************** 62 'INC. 1997     '
      **** BEGIN DLT **** 05/07/98 ************************************
     O****                               46 'INC.'
     O****                     *YEAR   +  1
      **** END   DLT **** 05/07/98 ************************************
      **** END   CHG **** 05/07/98 ************************************
     O*****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               DSDATEY  198
     O                       DSDATE             198 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O**
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O*
     O          E            HEAD           3
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFRDTY   99
     O**********                        104 'TO:'
     O**********               XXTODTY  113
     O                                           84 'FROM:'
     O                       X1FRDT              95 '  /  /    '
     O                                          100 'TO:'
     O                       X1TODT             111 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD           2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O          E            HEAD           1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          7 'DELETE'
     O**********                         89 'TRANS'
     O**********                        101 'ACCIDENT'
     O**********                        113 'AMOUNT'
     O**********                        123 'MRA'
     O**********                        139 'OTHER'
     O**********                        157 'NETWORK DISC'
     O**********                        170 'RECOMMENDED'
     O**********                        182 'REVIEWER'
     O**********                        192 'CHRGS'
     O**********                        197 'SYS'
     O                                            7 'DELETE'
     O                                           89 'TRANS'
     O                                          101 'ACCIDENT'
     O                                          113 'AMOUNT'
     O                                          123 'MRA'
     O                                          139 'OTHER'
     O                                          157 'NETWORK DISC'
     O                                          170 'RECOMMENDED'
     O                                          182 'REVIEWER'
     O                                          192 'CHRGS'
     O                                          197 'SYS'
     O*** END   CHG *** 01/27/00 **************************************
     O**
     O**
     O          E            HEAD           2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          5 'DATE'
     O**********                         29 'FILE #'
     O**********                         58 'CLAIMANT NAME'
     O**********                         78 'S.S. #'
     O**********                         89 'DATE'
     O**********                         99 'DATE'
     O**********                        113 'CHARGED'
     O**********                        127 'REDUCTIONS'
     O**********                        141 'REDUCTIONS'
     O**********                        156 'REDUCTION'
     O**********                        169 'ALLOWANCE'
     O**********                        182 'FEE'
     O**********                        192 'DLTD?'
     O                                            5 'DATE'
     O                                           29 'FILE #'
     O                                           58 'CLAIMANT NAME'
     O                                           78 'S.S. #'
     O                                           89 'DATE'
     O                                           99 'DATE'
     O                                          113 'CHARGED'
     O                                          127 'REDUCTIONS'
     O                                          141 'REDUCTIONS'
     O                                          156 'REDUCTION'
     O                                          169 'ALLOWANCE'
     O                                          182 'FEE'
     O                                          192 'DLTD?'
     O*** END   CHG *** 01/27/00 **************************************
     O**
     O**
     O          E            DET            1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXDT       8 '  /  /  '
     O**********               XF#       36
     O**********               XXNAME    68
     O**********               XXPRE     73
     O**********                         74 '-'
     O**********               XXMID     76
     O**********                         77 '-'
     O**********               XXSUF     81
     O**********               XXTRDT    91 '  /  /  '
     O**********               XXACDT   101 '  /  /  '
     O**********               EOAMTCJ  115
     O**********               XXMREDJ  129
     O**********               EOOTHRJ  143
     O**********               EONDISJ  157
     O**********               XXTPD J  171
     O**********               XISMUSJ  186
     O**********               EOFLG9   190
     O**********               EOSYS    196
     O                       XXDT                10 '  /  /    '
     O                       XF#                 38
     O                       XXNAME              65
     O                       XXPRE               70
     O                                           71 '-'
     O                       XXMID               73
     O                                           74 '-'
     O                       XXSUF               78
     O                       XXTRDT              90 '  /  /    '
     O                       XXACDT             102 '  /  /    '
     O                       EOAMTC        J    116
     O                       XXMRED        J    130
     O                       EOOTHR        J    144
     O                       EONDIS        J    158
     O                       XXTPD         J    172
     O                       XISMUS        J    187
     O                       EOFLG9             191
     O                       EOSYS              197
     O*** END   CHG *** 01/27/00 **************************************
     O          E            DATOT          1
     O          E            DATOT          2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               DAYDT     92 '  /  /  '
     O                       DAYDT               92 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O                                          102 'TOTALS:'
     O                       DAAMTC        JB   115
     O                       DAMRED        JB   129
     O                       DAOTHR        JB   143
     O                       DANDIS        JB   157
     O                       DAAMT         JB   171
     O                       DASMUS        JB   186
     O*
     O          E            SUBTOT         2
     O                                           88 'SUBSCRIBER'
     O                       LASTSB        Z     94
     O                                          102 'TOTALS:'
     O                       ALAMTC        J    115
     O                       ALMRED        J    129
     O                       ALOTHR        J    143
     O                       ALNDIS        J    157
     O                       ALTPD         J    171
     O                       ALSMUS        J    186
     O          E            LSTTOT         2
     O                                          102 'FINAL TOTALS:'
     O                       TLAMTC        J    115
     O                       TLMRED        J    129
     O                       TLOTHR        J    143
     O                       TLNDIS        J    157
     O                       TLTPD         J    171
     O                       TLSMUS        J    186
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O**
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   Q1USER              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   Q1USER              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       YYTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      **** BEGIN ADD **** 05/11/98 ************************************
     O          E            ROUTE1      2  2
     O                                           19 'SUBSCRIBER:'
     O                       EOSUB#              25
     O          E            ROUTE1         2
     O                                           19 'DATE RANGE:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    28 '  /  /  '
     O**********                         31 'TO'
     O**********               XXTODT    40 '  /  /  '
     O                       X1FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X1TODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ROUTE1         2
     O                                           19 'SYSTEM:'
     O                       QQBTYP              21
     O          E            ROUTE1         2
     O                                           19 'REPORT TYPE:'
     O                       QQRTYP              21
      **** END   ADD **** 05/11/98 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      **** BEGIN CHG **** 05/07/98 ************************************
     O********************************** 62 'INC. 1997     '
      **** BEGIN DLT **** 05/07/98 ************************************
     O****                               46 'INC.'
     O****                     *YEAR   +  1
      **** END   DLT **** 05/07/98 ************************************
      **** END   CHG **** 05/07/98 ************************************
     O*****************************************************************
     O          E            THEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               DSDATEY  198
     O                       DSDATE             198 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O**
     O          E            THEAD          1
     O                       Q1PGM               10
     O                       YYTITL             129
     O                                          194 'PAGE:'
     O                       PAGE1         Z    198
     O*
     O          E            THEAD          3
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFRDTY   99
     O**********                        104 'TO:'
     O**********               XXTODTY  113
     O                                           84 'FROM:'
     O                       X1FRDT              93 '  /  /    '
     O                                           98 'TO:'
     O                       X1TODT             107 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E   N70      THEAD          2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O*
     O          E            THEAD          3
     O                       HEADER             106
     O          E            THEAD          2
     O                                           71 'AMOUNT CHARGED'
     O                                           85 'AMOUNT PAID'
     O                                          102 'MRA REDUCTIONS'
     O                                          121 'OTHER REDUCTIONS'
     O                                          147 'NETWORK DISC REDUCTIONS'
     O                                          162 'REVIEWER FEE'
     O          E            SYTOT          2
     O                                           44 'SYSTEM '
     O                       SY(X)               45
     O                                           53 'TOTALS:'
     O                       SYAMTC        JB    71
     O                       SYTPD         JB    85
     O                       SYMRED        JB   102
     O                       SYOTHR        JB   121
     O                       SYNDIS        JB   147
     O                       SYSMUS        JB   162
     O          E            ALTOT          2
     O                       DESCR               53
     O                       ALAMTC        JB    71
     O                       ALTPD         JB    85
     O                       ALMRED        JB   102
     O                       ALOTHR        JB   121
     O                       ALNDIS        JB   147
     O                       ALSMUS        JB   162
     O          E            LRTOT          2
     O                                           44 'SYSTEM '
     O                       SY(X)               45
     O                                           53 'TOTALS:'
     O                       LRAMTC        J     71
     O                       LRTPD         J     85
     O                       LRMRED        J    102
     O                       LROTHR        J    121
     O                       LRNDIS        J    147
     O                       LRSMUS        J    162
     O*
     O          E            TLTOT          2
     O                       DESCR               53
     O                       TLAMTC        J     71
     O                       TLTPD         J     85
     O                       TLMRED        J    102
     O                       TLOTHR        J    121
     O                       TLNDIS        J    147
     O                       TLSMUS        J    162
     O**
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
** FINAL TOTAL DESCRIPTION
FINAL TOTALS     :
** HEADER DESCRIPTIONS
SUBSCRIBER TOTALS
   FINAL TOTALS
