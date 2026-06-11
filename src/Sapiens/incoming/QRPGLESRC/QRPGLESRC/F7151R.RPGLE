     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* PROGRAM:  F7151R                                              *
     H* DESCRIPTION:  EOMB DELETE REPORT - PRINT PROGRAM - PBILLS     *
     H*                                                               *
     H* CREATED: 03/31/00 C:62 PROJ: MC044 22280                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING                           *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      GENERAL USAGE (CHAIN)                           *
     H*       60      READ/DOWEQ OF OPEN QUERY FILE                   *
     H*       80      ALL COMPANIES TO BE PROCESSED                   *
     H*      N80      ONE COMPANY TO BE PROCESSED                     *
     H*       81      PROVIDER MODIFIER = 0                           *
     H*       89      NEW CASE - PRINT CASE INFO ON DETAIL LINE       *
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
     FFAEOL4    IF   E           K DISK
     F* * * CLAIM FILE
     FFMCML1    IF   E           K DISK
     F* * * DEMOGRAPHIC FILE
     FFMCDL     IF   E           K DISK
     F* * * SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     FWARPL     IF   E           K DISK    USROPN
     FWDPAL4    IF   E           K DISK    USROPN
     FFMPDL     IF   E           K DISK
     FFAEAL1    IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FFAESL5    IF   E           K DISK
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
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  XF#                    1     25
     D  XFFND                  1      3  0
     D  XFHYP1                 4      4
     D  XFYR                   5      7  0
     D  XFHYP2                 8      8
     D  XFCASE                 9     15  0
     D  XFHYP3                16     16
     D  XFEOB#                17     25  0
     D                 DS
     D  XXSS#                  1      9
     D  XXPRE                  1      3
     D  XXMID                  4      5
     D  XXSUF                  6      9

     I*
      *

     C*
     C*   KEY LIST FOR EOB FILE
     C*
     C     EOKY          KLIST
     C                   KFLD                    ESCO#
     C                   KFLD                    ESSUB#
     C                   KFLD                    ESFND
     C                   KFLD                    ESFYR
     C                   KFLD                    ESCASE
     C                   KFLD                    ESEOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    ESCO#
     C                   KFLD                    ESSUB#
     C                   KFLD                    ESFND
     C                   KFLD                    ESFYR
     C                   KFLD                    ESCASE
     C                   KFLD                    ESEOB#
     C                   KFLD                    ESRCN
     C*** BEGIN ADD *** 06/07/00 **************************************
     C*
     C     ESKY          KLIST
     C                   KFLD                    XXCO#
     C*** END   ADD *** 06/07/00 **************************************
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
     C*  WCMS CHECK REQUEST FILE(WARPP)
     C*
     C     RPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    XXFYR
     C*
     C*  WCMS PAYMENT DETAIL FILE(WDPAP)
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWCF#
     C                   KFLD                    XXCHK#
     C*
     C*  PROVIDER MASTER FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C*  FS BASE CONTROL FILE
     C*
     C     FBKY          KLIST
     C                   KFLD                    XSKEY             2
     C*
     C     *LIKE         DEFINE    ESCO#         XXCO#
     C     *LIKE         DEFINE    ESSUB#        XXSUB#
     C     *LIKE         DEFINE    ESFND         XXFND
     C     *LIKE         DEFINE    ESFYR         XXFYR
     C     *LIKE         DEFINE    ESCASE        XXCASE
     C     *LIKE         DEFINE    ESEOB#        XXEOB#
     C     *LIKE         DEFINE    SMNAME        LASTSN
     C     *LIKE         DEFINE    EOSUB#        LASTSB
     C     *LIKE         DEFINE    EODT          LASTDT
     C     *LIKE         DEFINE    EOFND         LASTFN
     C     *LIKE         DEFINE    FNWCF#        XXWCF#
     C     *LIKE         DEFINE    EAWCPC        XXCHK#
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    RPCHK#        X1CHK#
     C     *LIKE         DEFINE    RPCHK#        X2CHK#
     C     *LIKE         DEFINE    RPCHK#        X3CHK#
     C     *LIKE         DEFINE    RPAMT         X1AMT
     C     *LIKE         DEFINE    RPAMT         X2AMT
     C     *LIKE         DEFINE    RPAMT         X3AMT
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
     C                   MOVEL(P)  'SY'          XSKEY
     C     FBKY          CHAIN     SSY01                              61
     C                   MOVEL     SYFL05        XXNPWC            1
     C*
     C* VERIFY INSTALLED SYSTEMS INFORMATION FROM SYSTEM CONTROL FILE
     C*
     C                   MOVE      *BLANKS       XSKEY
     C                   MOVEL     'FB'          XSKEY
     C     FBKY          CHAIN     SFB01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* CHECK TO SEE IF WCMS INTERFACE INSTALLED
     C*
     C     FBFL01        IFEQ      'Y'
     C     XXNPWC        ANDNE     'Y'                                          ADD 08/20/99
     C                   MOVE      '1'           *IN71
     C                   OPEN      WARPL
     C                   OPEN      WDPAL4
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   END                                                    END FBFL01
     C*
     C                   END                                                    END *IN60
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
     C                   MOVE      QQFRDT        XXFRDT            8 0
     C                   MOVE      QQTODT        XXTODT            8 0
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
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
     C                   MOVE      *BLANK        UPUSNM
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
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ORTITL        XXTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C***10              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
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
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C***11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 ************************************
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
     C     ESKY          SETLL     PES01
     C                   READ      PES01                                  90
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
     C                   MOVE      ESCO#         XXCO#
     C                   MOVE      ESSUB#        XXSUB#
     C                   MOVE      ESFND         XXFND
     C                   MOVE      ESFYR         XXFYR
     C                   MOVE      ESCASE        XXCASE
     C                   MOVE      ESEOB#        XXEOB#
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
     C                   Z-ADD     FNWCF#        XXWCF#
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   Z-ADD     *ZEROS        XXWCF#
     C                   END
     C*
     C                   Z-ADD     ESFND         XFFND
     C                   Z-ADD     ESFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     ESCASE        XFCASE
     C                   Z-ADD     ESEOB#        XFEOB#
     C*
     C                   MOVE      ESIDT         LASTDT
     C                   MOVE      ESSUB#        LASTSB
     C                   MOVE      ESFND         LASTFN                         ADD 09/07/93
     C                   MOVE      XXSBNM        LASTSN
     C*
     C*   PRINT THE HEADINGS
     C*
     C   10              EXCEPT    HEAD
     C*
     C     *IN90         DOWEQ     '0'
     C*
     C                   Z-ADD     ESIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT              8 0
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C     EOCHRG        SUB       EOPAID        XXMRED            9 2
     C     XXMRED        SUB       EONDIS        XXMRED
     C     XXMRED        SUB       EOOTHR        XXMRED
     C     XXMRED        SUB       EOOTH2        XXMRED
     C                   Z-ADD     EOPAID        XXTPD             9 2
     C*
     C*  GET REVIEWER AMOUNT
     C*
     C                   Z-ADD     EOBAMT        XISMUS            9 2
     C*
     C*  ACCUMLATE SYSTEM TOTALS AND FINAL TOTALS
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C     EOSYS         LOOKUP    SY(X)                                  30
     C     *IN30         IFEQ      '0'
     C                   ADD       1             NS
     C                   MOVE      EOSYS         SY(NS)
     C                   MOVE      NS            X
     C                   END
     C     X             OCCUR     SYSVAL
     C                   ADD       EOCHRG        SYAMTC
     C                   ADD       XXMRED        SYMRED
     C                   ADD       EOOTHR        SYOTHR
     C                   ADD       EONDIS        SYNDIS
     C                   ADD       XXTPD         SYTPD
     C                   ADD       XISMUS        SYSMUS
     C*
LJD  C     EOBLFL        IFEQ      'P'
     C                   ADD       EOCHRG        INAMTC            9 2
     C                   ADD       XXMRED        INMRED            9 2
     C                   ADD       EOOTHR        INOTHR            9 2
     C                   ADD       EONDIS        INNDIS            9 2
     C                   ADD       XXTPD         INAMT             9 2
     C                   ADD       XISMUS        INSMUS            9 2
     C                   ELSE
     C                   ADD       EOCHRG        UNAMTC            9 2
     C                   ADD       XXMRED        UNMRED            9 2
     C                   ADD       EOOTHR        UNOTHR            9 2
     C                   ADD       EONDIS        UNNDIS            9 2
     C                   ADD       XXTPD         UNAMT             9 2
     C                   ADD       XISMUS        UNSMUS            9 2
     C                   END
     C                   ADD       EOCHRG        DAAMTC            9 2
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
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
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
     C                   MOVEL     QQLAST        XXNAME           20            ADD 02/22/93
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
     C     *IN71         IFEQ      '1'
     C*
     C                   MOVE      '0'           *IN72
     C                   MOVE      '0'           *IN73
     C                   MOVE      '0'           *IN74
     C                   Z-ADD     *ZEROS        XXPYCT            5 0
     C     RPKEY         CHAIN     PRP01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C* VERIFY RPEOB# IS THE SAME AS EOEOB#
     C*
     C     RPEOB#        IFEQ      XXEOB#
     C*
     C     XXPYCT        IFEQ      0
     C                   ADD       1             XXPYCT
     C                   MOVE      '1'           *IN72
     C                   Z-ADD     RPCHK#        X1CHK#
     C                   Z-ADD     RPAMT         X1AMT
     C*
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1CKDT            8 0
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS MADE
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        X1PAY            20
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       X1PAY
     C*
     C                   END                                                    END *IN60 PD
     C*
     C                   ELSE                                                   ELSE XXPYCT = 0
     C*
     C     XXPYCT        IFEQ      1
     C                   ADD       1             XXPYCT
     C                   MOVE      '1'           *IN73
     C                   Z-ADD     RPCHK#        X2CHK#
     C                   Z-ADD     RPAMT         X2AMT
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2CKDT            8 0
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS MADE
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        X2PAY            20
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       X2PAY
     C*
     C                   END                                                    END *IN60 PD
     C*
     C                   ELSE                                                   ELSE XXPYCT = 1
     C*
     C     XXPYCT        IFEQ      2
     C                   ADD       1             XXPYCT
     C                   MOVE      '1'           *IN74
     C                   Z-ADD     RPCHK#        X3CHK#
     C                   Z-ADD     RPAMT         X3AMT
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X3CKDT            8 0
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS MADE
     C*
     C                   Z-ADD     RPFEIN        XXFEIN
     C                   Z-ADD     RPMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        X3PAY            20
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       X3PAY
     C*
     C                   END                                                    END *IN60 PD
     C*
     C                   END                                                    END XXPYCT=2
     C                   END                                                    END XXPYCT=1
     C                   END                                                    END XXPYCT=0
     C*
     C                   END                                                    END RPEOB#
     C*
     C     RPKEY         READE     PRP01                                  50
     C                   END                                                    END *IN50 RP
     C*
     C* CHAIN TO EOMB AUXILIARY FILE TO SEE IF ANY OTHER CHECKS FOR THIS
     C* EOMB HAVE BEEN PROCESSED
     C*
     C     EAKY          CHAIN     PEA01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C     EAWCPC        IFNE      *ZEROS
     C*
     C                   Z-ADD     EAWCPC        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN72
     C                   Z-ADD     PACHK#        X1CHK#
     C                   Z-ADD     PAAMT         X1AMT
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1CKDT
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS MADE
     C*
     C                   Z-ADD     PAFEIN        XXFEIN
     C                   Z-ADD     PAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        X1PAY
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       X1PAY
     C*
     C                   END                                                    END *IN60 PD
     C*
     C                   END                                                    END *IN60 PA
     C*
     C                   END                                                    END EAWCPC
     C*
     C* IF AN OUTSIDE NETWORK CHECK HAS BEEN PROCESSED GET THE NAME TO
     C* WHOM THE CHECK WAS PROCESSED FOR
     C*
     C     EAWCNC        IFNE      *ZEROS
     C*
     C                   Z-ADD     EAWCNC        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN73
     C                   Z-ADD     PACHK#        X2CHK#
     C                   Z-ADD     PAAMT         X2AMT
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2CKDT
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS AMDE
     C*
     C                   Z-ADD     PAFEIN        XXFEIN
     C                   Z-ADD     PAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        X2PAY
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       X2PAY
     C*
     C                   END                                                    END *IN60 PD
     C*
     C                   END                                                    END *IN60 PA
     C*
     C                   END                                                    END EAWCNC
     C*
     C* IF THE BILL REVIEW CHECK HAS BEEN PROCESSED CHAIN TO PROVDIER
     C* FILE TO GET NAME TO WHOM CHECK WAS PROCESSED
     C*
     C     EAWCBC        IFNE      *ZEROS
     C*
     C                   Z-ADD     EAWCBC        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN74
     C                   Z-ADD     PACHK#        X3CHK#
     C                   Z-ADD     PAAMT         X3AMT
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X3CKDT
     C*
     C* CHAIN TO PROVIDER FILE TO FIND NAME TO WHOM THE PAYMENT WAS AMDE
     C*
     C                   Z-ADD     PAFEIN        XXFEIN
     C                   Z-ADD     PAMOD         XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END                                                    END NE *BLANK
     C                   MOVEL     XXLAST        X3PAY
     C                   ELSE                                                   ELSE *IN60 PD
     C                   MOVE      *BLANKS       X3PAY
     C*
     C                   END                                                    END *IN60 PD
     C*
     C                   END                                                    END *IN60 PA
     C*
     C                   END                                                    END EAWCBC
     C*
     C                   END                                                    END *IN60 E1
     C*
     C     *IN72         IFEQ      '1'
     C     *IN73         OREQ      '1'
     C     *IN74         OREQ      '1'
     C   10              EXCEPT    PAY
     C                   END                                                    END 72,73,74
     C*
     C                   END                                                    END *IN71
     C*
     C                   READ      PES01                                  90
     C*
     C                   MOVE      ESCO#         XXCO#
     C                   MOVE      ESSUB#        XXSUB#
     C                   MOVE      ESFND         XXFND
     C                   MOVE      ESFYR         XXFYR
     C                   MOVE      ESCASE        XXCASE
     C                   MOVE      ESEOB#        XXEOB#
     C*
     C                   Z-ADD     ESFND         XFFND
     C                   Z-ADD     ESFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     ESCASE        XFCASE
     C                   Z-ADD     ESEOB#        XFEOB#
     C*
     C     ESSUB#        IFNE      LASTSB
     C                   EXSR      SRTOT
     C                   EXSR      SRSBBK
     C                   EXSR      SRFNBK                                       ADD 09/07/93
     C                   ELSE
     C     ESFND         IFNE      LASTFN
     C                   EXSR      SRFNBK                                       ADD 09/07/93
     C                   ELSE
     C     ESIDT         IFNE      LASTDT
     C                   EXSR      SRTOT
     C                   END
     C                   END                                                     ADD 09/07/93
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
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       xxprt = '1';
       Chain ORKEY POR01;
       If %found;
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*
     C     TERM          TAG
     C*
     C*****************************************************************
     C*                                                               *
     C* THIS SUBROUTINE PRINTS THE DAILY TOTALS                       *
     C*                                                               *
     C*****************************************************************
     C     SRTOT         BEGSR
     C*
     C*
     C                   Z-ADD     LASTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DAYDT             8 0
     C*
     C     *INOA         IFEQ      '1'
     C*
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C*
     C                   ENDIF
     C*
     C   10              EXCEPT    DATOT
     C*
     C                   MOVE      ESIDT         LASTDT
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*                                                               *
     C* FUND BREAK SUBROUTINE                                         *
     C*                                                               *
     C*****************************************************************
     C*
     C     SRFNBK        BEGSR
     C*
     C*  CHAIN TO FUND FILE TO GET FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   Z-ADD     FNWCF#        XXWCF#
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   Z-ADD     *ZEROS        XXWCF#
     C                   END
     C                   MOVE      EOFND         LASTFN
     C*
     C                   ENDSR
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
     C*  CHAIN TO FUND FILE TO GET FUND NAME
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   Z-ADD     FNWCF#        XXWCF#
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   Z-ADD     *ZEROS        XXWCF#
     C                   END
     C*
     C                   MOVE      ESSUB#        LASTSB
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
      ***** Beg Del ***** 08/26/14 ************************************
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
      ***** End Del ***** 08/26/14 ************************************
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
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
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
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                63 'INC. 2000            '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
     O**
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O*
     O          E            HEAD           3
     O                                           81 'FROM:'
     O                       XXFRDT        Y     92
     O                                           97 'TO:'
     O                       XXTODT        Y    108
     O          E            HEAD           2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O          E            HEAD           1
     O                                          172 'TOTAL'
     O          E            HEAD           1
     O                                            7 'DELETE'
     O                                           82 'TRANS'
     O                                           95 'ACCIDENT'
     O                                          109 'AMOUNT'
     O                                          119 'MRA'
     O                                          133 'OTHER'
     O                                          149 'NETWORK DISC'
     O                                          162 'RECOMMENDED'
     O                                          173 'REVIEWER'
     O                                          181 'CHRGS'
     O          E            HEAD           2
     O                                            5 'DATE'
     O                                           31 'FILE #'
     O                                           53 'CLAIMANT NAME'
     O                                           69 'S.S. #'
     O                                           82 'DATE'
     O                                           93 'DATE'
     O                                          109 'CHARGED'
     O                                          123 'REDUCTIONS'
     O                                          135 'REDUCTIONS'
     O                                          148 'REDUCTION'
     O                                          161 'ALLOWANCE'
     O                                          170 'FEE'
     O                                          181 'DLTD?'
     O                                          186 'SYS'
     O          E            DET            1
     O                       XXDT                10 '  /  /    '
     O                       XF#                 38
     O                       XXNAME              60
     O                       XXPRE               65
     O                                           66 '-'
     O                       XXMID               68
     O                                           69 '-'
     O                       XXSUF               73
     O                       XXTRDT              84 '  /  /    '
     O                       XXACDT              96 '  /  /    '
     O                       EOCHRG        J    111
     O                       XXMRED        J    123
     O                       EOOTHR        J    135
     O                       EONDIS        J    149
     O                       XXTPD         J    162
     O                       XISMUS        J    174
LJD  O                       EOFLG8             178
     O                       EOSYS              185
     O          E            PAY            1
     O                                           26 'PAYMENTS MADE TO:'
     O                                           38 'CHECK #'
     O                                           48 'CHK DATE'
     O                                           61 'AMOUNT'
     O          E    72      PAY            1
     O                       X1PAY               29
     O                       X1CHK#        Z     38
     O                       X1CKDT              50 '  /  /    '
     O                       X1AMT         J     64
     O          E    73      PAY            1
     O                       X2PAY               29
     O                       X2CHK#        Z     38
     O                       X2CKDT              50 '  /  /    '
     O                       X2AMT         J     64
     O          E    74      PAY            1
     O                       X3PAY               29
     O                       X3CHK#        Z     38
     O                       X3CKDT              50 '  /  /    '
     O                       X3AMT         J     64
     O          E            DATOT          1
     O          E            DATOT          2
     O                       DAYDT               90 '  /  /    '
     O                                           99 'TOTALS:'
     O                       DAAMTC        JB   111
     O                       DAMRED        JB   123
     O                       DAOTHR        JB   135
     O                       DANDIS        JB   149
     O                       DAAMT         JB   162
     O                       DASMUS        JB   174
     O*
     O          E            SUBTOT         2
     O                                           85 'SUBSCRIBER'
     O                       LASTSB        Z     91
     O                                           99 'TOTALS:'
     O                       ALAMTC        J    111
     O                       ALMRED        J    123
     O                       ALOTHR        J    135
     O                       ALNDIS        J    149
     O                       ALTPD         J    162
     O                       ALSMUS        J    174
     O          E            LSTTOT         2
     O                                           99 'FINAL TOTALS:'
     O                       TLAMTC        J    111
     O                       TLMRED        J    123
     O                       TLOTHR        J    135
     O                       TLNDIS        J    149
     O                       TLTPD         J    162
     O                       TLSMUS        J    174
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
     O**
     OQSYSPRT1  E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
     O**********        09   UPUSNM              67
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       YYTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
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
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE1
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                63 'INC. 2000            '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            THEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
     O**
     O          E            THEAD          1
     O                       Q1PGM               10
     O                       YYTITL             129
     O                                          194 'PAGE:'
     O                       PAGE1         Z    198
     O*
     O          E            THEAD          3
     O                                           81 'FROM:'
     O                       XXFRDT        Y     92
     O                                           97 'TO:'
     O                       XXTODT        Y    108
     O          E   N70      THEAD          2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O*
     O          E            THEAD          3
     O                       HEADER             106
     O          E            THEAD          1
     O                                          158 'TOTAL'
     O          E            THEAD          2
     O                                           71 'AMOUNT CHARGED'
     O                                           85 'AMOUNT PAID'
     O                                          102 'MRA REDUCTIONS'
     O                                          121 'OTHER REDUCTIONS'
     O                                          147 'NETWORK DISC REDUCTIONS'
     O                                          163 'REVIEWER FEE'
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
