     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* PROGRAM:  F7150R                                              *
     H* DESCRIPTION:  EOMB DELETE REPORT - PRINT PROGRAM              *
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
     H*  08/15/00 C:62  PROJECT: MC215                                *
     H*                 REPLACE SFBPF REFERENCES WITH APIS.           *
     H*                                                               *
     H*  08/20/98   C:53    REQ/PROJ: MCMNT                           *
     H*                     ADD NON PDS WCMS FLAG FOR FILES           *
     H*                                                               *
     H*  09/07/93   C:20    REQ/PROJ: 123/MC100                       *
     H*                     1. IF WCMC INTERFACE INSTALLED SEE IF ANY *
     H*                        CHECK/CHECK REQUEST HAS BEEN PROCESSED *
     H*                        FOR THE DELETED EOMB IF IT HAS PRINT   *
     H*                        THE CHECK INFORMATION UNDER THE DETAIL *
     H*                        LINE FOR EOMB BEING DELETED.           *
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
     FFDEOL     IF   E           K DISK
     F* * * CLAIM FILE
     FFMCML1    IF   E           K DISK
     F* * * DEMOGRAPHIC FILE
     FFMCDL     IF   E           K DISK
     F* * * SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F*** BEGIN ADD *** 09/07/93 **************************************
     FWARPL     IF   E           K DISK    USROPN
     FWDPAL4    IF   E           K DISK    USROPN
     FFMPDL     IF   E           K DISK
     FFAE1L     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 **************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 **************************************
     F*** END ADD *** 09/07/93 ****************************************
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
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** BEGIN CHG *** 09/07/93 **************************************
     D**********  DS
     D**********                              1  26 XF#
     D**********                              1   50XFSUB
     D**********                              6   6 XFHYP1
     D**********                              7   90XFFND
     D**********                             10  10 XFHYP2
     D**********                             11  120XFYR
     D**********                             13  13 XFHYP3
     D**********                             14  200XFCASE
     D**********                             21  21 XFHYP4
     D**********                             22  260XFEOB#
     D                 DS
     D  XF#                    1     25
     D  XFFND                  1      3  0
     D  XFHYP1                 4      4
     D  XFYR                   5      7  0
     D  XFHYP2                 8      8
     D  XFCASE                 9     15  0
     D  XFHYP3                16     16
     D  XFEOB#                17     25  0
     D*** END CHG *** 09/07/93 ****************************************
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
     C*** BEGIN ADD *** 09/07/93 **************************************
     C*
     C*  EOMB AUXILIARY FILE
     C*
     C     E1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
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
     C*** BEGIN DEL *** 08/15/00 **************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 **************************************
     C*** END ADD *** 09/07/93 ****************************************
     C*
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    EOWCCK        XXWCCK                         ADD 02/22/93
     C     *LIKE         DEFINE    SMNAME        LASTSN
     C     *LIKE         DEFINE    EOSUB#        LASTSB
     C     *LIKE         DEFINE    EODT          LASTDT
     C*** BEGIN ADD *** 09/07/93 **************************************
     C     *LIKE         DEFINE    EOFND         LASTFN
     C     *LIKE         DEFINE    FNWCF#        XXWCF#
     C     *LIKE         DEFINE    EOWCCK        XXCHK#
     C     *LIKE         DEFINE    PDFEIN        XXFEIN
     C     *LIKE         DEFINE    PDMOD         XXMOD
     C     *LIKE         DEFINE    RPCHK#        X1CHK#
     C     *LIKE         DEFINE    RPCHK#        X2CHK#
     C     *LIKE         DEFINE    RPCHK#        X3CHK#
     C     *LIKE         DEFINE    RPAMT         X1AMT
     C     *LIKE         DEFINE    RPAMT         X2AMT
     C     *LIKE         DEFINE    RPAMT         X3AMT
     C*** END ADD ***9/07/93   ****************************************
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
     C*** BEGIN ADD *** 08/15/00 **************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*
     C*** END   ADD *** 08/15/00 **************************************
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN DEL *** 08/15/00 **************************************
     C*** BEG ADD *** 08/20/99 **************************************
     C**********           MOVEL'SY'      XSKEY     P
     C********** FBKY      CHAINSSY01                61
     C**********           MOVELSYFL05    XXNPWC  1
     C*** END ADD *** 08/20/99 **************************************
     C*** BEGIN ADD *** 09/07/93 **************************************
     C*
     C*
     C* VERIFY INSTALLED SYSTEMS INFORMATION FROM SYSTEM CONTROL FILE
     C*
     C**********           MOVE *BLANKS   XSKEY
     C**********           MOVEL'FB'      XSKEY
     C********** FBKY      CHAINSFB01                60
     C********** *IN60     IFEQ '0'
     C*** END   DEL *** 08/15/00 **************************************
     C*
     C* CHECK TO SEE IF WCMS INTERFACE INSTALLED
     C*
     C*** BEGIN CHG *** 08/15/00 **************************************
     C********** FBFL01    IFEQ 'Y'
     C********** XXNPWC    ANDNE'Y'                        ADD 08/20/99
     C     ##WCIN        IFEQ      'Y'
     C     ##NPWC        ANDNE     'Y'                                          ADD 08/20/99
     C*** END   CHG *** 08/15/00 **************************************
     C                   MOVE      '1'           *IN71
     C                   OPEN      WARPL
     C                   OPEN      WDPAL4
     C                   ELSE
     C                   MOVE      '0'           *IN71
     C                   END                                                    END FBFL01
     C*
     C*** BEGIN DEL *** 08/15/00 **************************************
     C**********           END                             END *IN60
     C*** END   DEL *** 08/15/00 **************************************
     C*** END ADD *** 09/07/93 ****************************************
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
     C                   Z-ADD     X1FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTODT            6 0
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
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
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
     C                   Z-ADD     EOWCCK        XXWCCK                         ADD 02/22/93
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
     C*** END ADD *** 09/07/93 ****************************************
     C                   Z-ADD     FNWCF#        XXWCF#
     C*** END ADD *** 09/07/93 ****************************************
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C*** END ADD *** 09/07/93 ****************************************
     C                   Z-ADD     *ZEROS        XXWCF#
     C*** END ADD *** 09/07/93 ****************************************
     C                   END
     C*
     C*** BEGIN DEL *** 09/07/93 **************************************
     C**********           Z-ADDEOSUB#    XFSUB
     C*** END DEL *** 09/07/93 ****************************************
     C                   Z-ADD     EOFND         XFFND
     C                   Z-ADD     EOFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EOCASE        XFCASE
     C                   Z-ADD     EOEOB#        XFEOB#
     C*
     C                   MOVE      EODT          LASTDT
     C                   MOVE      EOSUB#        LASTSB
     C                   MOVE      EOFND         LASTFN                         ADD 09/07/93
     C                   MOVE      XXSBNM        LASTSN
     C*
     C*   PRINT THE HEADINGS
     C*
     C   10              EXCEPT    HEAD
     C*
     C     *IN90         DOWEQ     '0'
     C*
     C                   Z-ADD     EODT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDT              6 0
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C     EOAMTC        SUB       EOAMT         XXMRED            9 2
     C     XXMRED        SUB       EOOTHR        XXMRED
     C     EOAMT         SUB       EONDIS        XXTPD             9 2
     C*
     C*  GET REVIEWER AMOUNT
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
LJD  C     EOBLFL        IFEQ      'P'
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
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
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
     C*********************MOVELQQLAST    XXNAME 30        DEL 02/22/93
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
     C*** BEGIN ADD *** 09/07/93 **************************************
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
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1CKDT            6 0
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
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2CKDT            6 0
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
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X3CKDT            6 0
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
     C     EOWCCK        IFNE      *ZEROS
     C                   Z-ADD     EOWCCK        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN72
     C                   Z-ADD     PACHK#        X1CHK#
     C                   Z-ADD     PAAMT         X1AMT
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
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
     C                   END                                                    END EOWCCK
     C*
     C* CHAIN TO EOMB AUXILIARY FILE TO SEE IF ANY OTHER CHECKS FOR THIS
     C* EOMB HAVE BEEN PROCESSED
     C*
     C     E1KEY         CHAIN     PE101                              60
     C     *IN60         IFEQ      '0'
     C*
     C* IF AN OUTSIDE NETWORK CHECK HAS BEEN PROCESSED GET THE NAME TO
     C* WHOM THE CHECK WAS PROCESSED FOR
     C*
     C     E110A1        IFNE      *BLANKS
     C                   MOVEL     E110A1        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN73
     C                   Z-ADD     PACHK#        X2CHK#
     C                   Z-ADD     PAAMT         X2AMT
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
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
     C                   END                                                    END E110A1
     C*
     C* IF THE BILL REVIEW CHECK HAS BEEN PROCESSED CHAIN TO PROVDIER
     C* FILE TO GET NAME TO WHOM CHECK WAS PROCESSED
     C*
     C     E110A2        IFNE      *BLANKS
     C                   MOVEL     E110A2        XXCHK#
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      '1'           *IN74
     C                   Z-ADD     PACHK#        X3CHK#
     C                   Z-ADD     PAAMT         X3AMT
     C                   Z-ADD     PATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
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
     C                   END                                                    END E110A2
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
     C*** END ADD *** 09/07/93 ****************************************
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
     C                   Z-ADD     EOWCCK        XXWCCK                         ADD 02/22/93
     C*
     C*** BEGIN DEL *** 09/07/93 **************************************
     C**********           Z-ADDEOSUB#    XFSUB
     C*** END DEL *** 09/07/93 ****************************************
     C                   Z-ADD     EOFND         XFFND
     C                   Z-ADD     EOFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EOCASE        XFCASE
     C                   Z-ADD     EOEOB#        XFEOB#
     C*
     C     EOSUB#        IFNE      LASTSB
     C                   EXSR      SRTOT
     C                   EXSR      SRSBBK
     C                   EXSR      SRFNBK                                       ADD 09/07/93
     C                   ELSE
     C*** BEGIN ADD *** 09/07/93 **************************************
     C     EOFND         IFNE      LASTFN
     C                   EXSR      SRFNBK                                       ADD 09/07/93
     C                   ELSE
     C*** END ADD *** 09/07/93 ****************************************
     C     EODT          IFNE      LASTDT
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
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         DAYDT             6 0
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
     C*** BEGIN ADD *** 09/07/93 **************************************
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
     C*** END ADD *** /07/93   ****************************************
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
     C*** BEGIN ADD *** 09/07/93 **************************************
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
     C*** END ADD *** 09/07/93 ****************************************
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
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                63 'INC. 1991, 1992, 1993'
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
     O                                           90 'FROM:'
     O                       XXFRDT        Y     99
     O                                          104 'TO:'
     O                       XXTODT        Y    113
     O          E            HEAD           2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O          E            HEAD           1
     O**********************************179 'TOTAL'
     O                                          164 'TOTAL'
     O          E            HEAD           1
     O                                            7 'DELETE'
     O***********************************89 'TRANS'
     O                                           79 'TRANS'
     O**********************************101 'ACCIDENT'
     O                                           91 'ACCIDENT'
     O**********************************113 'AMOUNT'
     O                                          101 'AMOUNT'
     O**********************************123 'MRA'
     O                                          111 'MRA'
     O**********************************139 'OTHER'
     O                                          125 'OTHER'
     O**********************************157 'NETWORK DISC'
     O                                          141 'NETWORK DISC'
     O**********************************170 'RECOMMENDED'
     O                                          154 'RECOMMENDED'
     O**********************************182 'REVIEWER'
     O                                          165 'REVIEWER'
     O**********************************192 'CHRGS'
     O                                          173 'CHRGS'
     O**********************************197 'SYS'
     O**
     O**
     O          E            HEAD           2
     O                                            5 'DATE'
     O                                           29 'FILE #'
     O***********************************58 'CLAIMANT NAME'
     O                                           51 'CLAIMANT NAME'
     O***********************************78 'S.S. #'
     O                                           67 'S.S. #'
     O***********************************89 'DATE'
     O                                           79 'DATE'
     O***********************************99 'DATE'
     O                                           89 'DATE'
     O**********************************113 'CHARGED'
     O                                          101 'CHARGED'
     O**********************************127 'REDUCTIONS'
     O                                          115 'REDUCTIONS'
     O**********************************141 'REDUCTIONS'
     O                                          127 'REDUCTIONS'
     O**********************************156 'REDUCTION'
     O                                          140 'REDUCTION'
     O**********************************169 'ALLOWANCE'
     O                                          153 'ALLOWANCE'
     O**********************************179 'FEE'
     O                                          162 'FEE'
     O**********************************192 'DLTD?'
     O                                          173 'DLTD?'
     O                                          178 'SYS'
     O*** BEGIN DEL *** 09/07/93 **************************************
     O**********                        190 'WC CHECK #'
     O*** END DEL *** 09/07/93 ****************************************
     O**
     O**
     O          E            DET            1
     O                       XXDT                 8 '  /  /  '
     O*************************XF#       36
     O                       XF#                 36
     O*************************XXNAME    68
     O                       XXNAME              58
     O*************************XXPRE     73
     O                       XXPRE               63
     O                                           64 '-'
     O*************************XXMID     76
     O                       XXMID               66
     O                                           67 '-'
     O*************************XXSUF     81
     O                       XXSUF               71
     O*************************XXTRDT    91 '  /  /  '
     O                       XXTRDT              80 '  /  /  '
     O*************************XXACDT   101 '  /  /  '
     O                       XXACDT              90 '  /  /  '
     O*************************EOAMTCJ  115
     O                       EOAMTC        J    103
     O*************************XXMREDJ  129
     O                       XXMRED        J    115
     O*************************EOOTHRJ  143
     O                       EOOTHR        J    127
     O*************************EONDISJ  157
     O                       EONDIS        J    141
     O*************************XXTPD J  171
     O                       XXTPD         J    154
     O*************************XISMUSJ  186
     O                       XISMUS        J    166
LJD  O*************************EOFLG8   190
LJD  O                       EOFLG8             170
     O*************************EOSYS    196
     O                       EOSYS              177
     O*** BEGIN DEL *** 09/07/93 **************************************
     O**********               EOWCCKZ  190
     O*** END DEL *** 09/07/93 ****************************************
     O*** BEGIN ADD *** 09/07/93 **************************************
     O          E            PAY            1
     O                                           26 'PAYMENTS MADE TO:'
     O                                           38 'CHECK #'
     O                                           48 'CHK DATE'
     O                                           61 'AMOUNT'
     O          E    72      PAY            1
     O                       X1PAY               29
     O                       X1CHK#        Z     38
     O                       X1CKDT              48 '  /  /  '
     O                       X1AMT         J     62
     O          E    73      PAY            1
     O                       X2PAY               29
     O                       X2CHK#        Z     38
     O                       X2CKDT              48 '  /  /  '
     O                       X2AMT         J     62
     O          E    74      PAY            1
     O                       X3PAY               29
     O                       X3CHK#        Z     38
     O                       X3CKDT              48 '  /  /  '
     O                       X3AMT         J     62
     O*** END ADD *** 09/07/93 ****************************************
     O          E            DATOT          1
     O          E            DATOT          2
     O*************************DAYDT     92 '  /  /  '
     O                       DAYDT               80 '  /  /  '
     O**********************************102 'TOTALS:'
     O                                           91 'TOTALS:'
     O*************************DAAMTCJB 115
     O                       DAAMTC        JB   103
     O*************************DAMREDJB 129
     O                       DAMRED        JB   115
     O*************************DAOTHRJB 143
     O                       DAOTHR        JB   127
     O*************************DANDISJB 157
     O                       DANDIS        JB   141
     O*************************DAAMT JB 171
     O                       DAAMT         JB   154
     O*************************DASMUSJB 186
     O                       DASMUS        JB   166
     O*
     O          E            SUBTOT         2
     O***********************************88 'SUBSCRIBER'
     O                                           77 'SUBSCRIBER'
     O*************************LASTSBZ   94
     O                       LASTSB        Z     83
     O**********************************102 'TOTALS:'
     O                                           91 'TOTALS:'
     O*************************ALAMTCJ  115
     O                       ALAMTC        J    103
     O*************************ALMREDJ  129
     O                       ALMRED        J    115
     O*************************ALOTHRJ  143
     O                       ALOTHR        J    127
     O*************************ALNDISJ  157
     O                       ALNDIS        J    141
     O*************************ALTPD J  171
     O                       ALTPD         J    154
     O*************************ALSMUSJ  186
     O                       ALSMUS        J    166
     O          E            LSTTOT         2
     O**********************************102 'FINAL TOTALS:'
     O                                           91 'FINAL TOTALS:'
     O*************************TLAMTCJ  115
     O                       TLAMTC        J    103
     O*************************TLMREDJ  129
     O                       TLMRED        J    115
     O*************************TLOTHRJ  143
     O                       TLOTHR        J    127
     O*************************TLNDISJ  157
     O                       TLNDIS        J    141
     O*************************TLTPD J  171
     O                       TLTPD         J    154
     O*************************TLSMUSJ  186
     O                       TLSMUS        J    166
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
      ***** End Add ***** 08/05/15 ************************************
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
     O***                                63 'INC. 1991, 1992, 1993'
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
     O                                           90 'FROM:'
     O                       XXFRDT        Y     99
     O                                          104 'TO:'
     O                       XXTODT        Y    113
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
