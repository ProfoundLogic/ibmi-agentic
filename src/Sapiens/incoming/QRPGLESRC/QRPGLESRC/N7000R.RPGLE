     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*  REVISIONS:                                                  **
      *                                                                 *
      *  06/09/20  C:212 Project : SY420                                *
      *                  Mask SSN                                       *
     H*                                                              **
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H*  01/27/00 C:62 Project: Y2K                                  **
     H*                CHANGE DATES TO MDYY.                         **
     H*                                                              **
     H*  01/07/99 C:94 PROJECT: MC152                                **
     H*                CHANGE PROGRAM TO CHECK IF PROVIDER BILLS IS  **
     H*                BEING USED.  IF YES, THEN USE FAEAP INSTEAD   **
     H*                OF NATRP, NAMBP, NAPBP, FAHBP.                **
     H*                                                              **
     H*  07/05/95 C:35  REQ/PROJ: NW008                              **
     H*                 ADDED PROCESSING OF MEDICAL MANGEMENT BILLING**
     H*                                                              **
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
     H/TITLE F7000R - UNBILLED CHARGES REPORT
     F* * * ROUTING FILE
     FSMORL     IF   E           K DISK
     F* * * COMPANY FILE
     FSMCOL     IF   E           K DISK
     F* * * FUND FILE
     FFMFNL     IF   E           K DISK
     F* * * USER PARAMETER FILE
     FSMUPL     IF   E           K DISK
     F* * * EOB FILE
     FTAEOL     IF   E           K DISK
     F* * * HBA FILE
     FFAHBL     IF   E           K DISK
     F* * * PRE/RE-CERT BILLING FILE
     FNAPBL1    IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F* * * MEDICAL MANGEMENT BILLING FILE
     FNAMBL1    IF   E           K DISK
     F* * * TIME/MATERIALS BILLING FILE
     FNDTML1    IF   E           K DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F* * * CLAIM FILE
     FFMCML1    IF   E           K DISK
     F* * * DEMOGRAPHIC FILE
     FFMCDL     IF   E           K DISK
     F* * * SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F* * * NETWORK TRANSACTION FILE
     FNATRL     IF   E           K DISK
     F*** BEG ADD *** 01/07/99 ***************************************
     F* * * BILL HEADER AMOUNT FILE
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/07/99 ***************************************
     F* * * PRINTER FILES
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D SY              S              1    DIM(10) ASCEND
     D LD              S             18    DIM(1) CTDATA PERRCD(1)
     D HD              S             18    DIM(2) CTDATA PERRCD(1)
     D SYSVAL          DS                  OCCURS(10)
     D  SYSUM                  1     72
     D  SYAMTC                 1      9  2
     D  SYMRED                10     18  2
     D  SYOTHR                19     27  2
     D  SYNDIS                28     36  2
     D  SYTPD                 37     45  2
     D  SYTTM                 46     54  2
     D  SYCRFE                55     63  2
     D  SYSMUS                64     72  2
     D FINTOT          DS                  OCCURS(10)
     D  LRSUM                  1     72
     D  LRAMTC                 1      9  2
     D  LRMRED                10     18  2
     D  LROTHR                19     27  2
     D  LRNDIS                28     36  2
     D  LRTPD                 37     45  2
     D  LRTTM                 46     54  2
     D  LRCRFE                55     63  2
     D  LRSMUS                64     72  2
     D ALLTOT          DS
     D  ALAMTC                 1      5P 2
     D  ALMRED                 6     10P 2
     D  ALOTHR                11     15P 2
     D  ALNDIS                16     20P 2
     D  ALTPD                 21     25P 2
     D  ALTTM                 26     30P 2
     D  ALCRFE                31     35P 2
     D  ALSMUS                36     40P 2
     D TOTTOT          DS
     D  TLAMTC                 1      5P 2
     D  TLMRED                 6     10P 2
     D  TLOTHR                11     15P 2
     D  TLNDIS                16     20P 2
     D  TLTPD                 21     25P 2
     D  TLTTM                 26     30P 2
     D  TLCRFE                31     35P 2
     D  TLSMUS                36     40P 2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
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
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR EOB FILE
     C*
     C     EOKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
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
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXFYR             3 0
     C                   KFLD                    XXCASE            7 0
     C*
     C*   KEY LIST FOR THE CLAIMAMT DEMOGRAPHIC FILE
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE PRE/RE-CERT BILLING FILE
     C*
     C     PBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*   KEY LIST FOR THE MEDICAL MANAGEMENT BILLING FILE
     C*
     C     MBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*   KEY LIST FOR THE TIME/MATERIALS BILLING FILE
     C*
     C     TMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
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
     C*** BEG ADD *** 01/07/99 ***************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*** END ADD *** 01/07/99 ***************************************
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQFRD2            8
     C                   PARM                    QQTOD2            8
     C                   PARM                    QQRTYP            1
     C                   PARM                    QQFLAG            1
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
     C*  INITIALIZATION SECTION
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C*
     C*** BEG ADD *** 01/07/99 ***************************************
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
     C*
     C*** END ADD *** 01/07/99 ***************************************
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVE QQFRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFR    60
     C                   MOVE      QQFRDT        XXFR              8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVE QQTODT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTO    60
     C                   MOVE      QQTODT        XXTO              8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVE QQFRD2    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFR2   60
     C                   MOVE      QQFRD2        XXFR2             8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVE QQTOD2    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTO2   60
     C                   MOVE      QQTOD2        XXTO2             8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C                   DO        10            X
     C     X             OCCUR     SYSVAL
     C                   Z-ADD     0             SYAMTC
     C                   Z-ADD     0             SYMRED
     C                   Z-ADD     0             SYOTHR
     C                   Z-ADD     0             SYNDIS
     C                   Z-ADD     0             SYTPD
     C                   Z-ADD     0             SYTTM
     C                   Z-ADD     0             SYCRFE
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
     C                   Z-ADD     0             LRTTM
     C                   Z-ADD     0             LRCRFE
     C                   Z-ADD     0             LRSMUS
     C                   END
     C*
     C                   Z-ADD     0             ALAMTC
     C                   Z-ADD     0             ALMRED
     C                   Z-ADD     0             ALOTHR
     C                   Z-ADD     0             ALNDIS
     C                   Z-ADD     0             ALTPD
     C                   Z-ADD     0             ALTTM
     C                   Z-ADD     0             ALCRFE
     C                   Z-ADD     0             ALSMUS
     C*
     C                   Z-ADD     0             TLAMTC
     C                   Z-ADD     0             TLMRED
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLTPD
     C                   Z-ADD     0             TLTTM
     C                   Z-ADD     0             TLCRFE
     C                   Z-ADD     0             TLSMUS
     C*
     C*
     C                   Z-ADD     0             NS                3 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      '0'           *IN70
     C                   MOVE      HD(1)         HEADER           18
     C                   MOVE      LD(1)         DESCR            18
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
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
     C                   TIME                    DSDTTM
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
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
     C*   SET INDICATOR IF DATE RANGE IS TO BE PRINTED
     C*
     C     XXFR          IFNE      *ZEROS
     C     XXTO          ORNE      *ZEROS
     C                   MOVE      '1'           *IN12
     C                   END
     C*
     C*   SET INDICATOR IF THERE EXISTS A SECOND SET OF DATES FROM
     C*   BATCH TO PRINT WHEN ALL UNBILLED RECORDS ARE PROCESSED
     C*
     C     XXFR2         IFNE      *ZEROS
     C     XXTO2         ANDNE     *ZEROS
     C                   MOVE      '1'           *IN13
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
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********   10              EXCEPT    ROUTE
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
     C********   11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   END
     C*
     C*   READ EOB FILE
     C*
     C     EOKEY         SETLL     PEO01
     C                   READ      PEO01                                  90
     C*
     C     *IN90         IFEQ      '1'
     C   10              EXCEPT    HEAD1
     C   11              EXCEPT    THEAD
     C   10              EXCEPT    ERRMSG
     C   11              EXCEPT    ERRMS1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C                   MOVE      EOSUB#        XXSUB#            5 0
     C                   MOVE      EOFND         XXFND             3 0
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C                   Z-ADD     EOSUB#        LASTSB            5 0
     C                   MOVE      XXSBNM        LASTSN           40
     C                   Z-ADD     EOFND         LASTFN            3 0
     C                   Z-ADD     EOCASE        LASTCA            7 0
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C*   PRINT THE HEADINGS FOR UNBILLED CHARGES REPORT
     C*
     C   10              EXCEPT    HEAD1
     C   10              EXCEPT    SUBSC
     C   10              EXCEPT    FUND
     C   10              EXCEPT    HEAD
     C*
     C     *IN90         IFEQ      '1'
     C   10              EXCEPT    HEAD1
     C   10              EXCEPT    ERRMSG
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C**
B1   C     *IN60         DOWEQ     '0'
     C*
     C     EOAMTC        SUB       EOAMT         XXMRED            9 2
     C     XXMRED        SUB       EOOTHR        XXMRED
     C     EOAMT         SUB       EONDIS        XXTPD             9 2
     C**
     C                   Z-ADD     EOTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C     EOCASE        IFNE      LASTCA
     C                   ADD       1             COUNTG            7 0           GROUP COUNT
     C                   ADD       1             COUNTS            7 0           SUBSC COUNT
     C                   ADD       1             COUNTT            7 0           FINAL COUNT
     C   10              EXCEPT    CATOT
     C                   END
     C*
     C     EOFND         IFNE      LASTFN
     C   10              EXCEPT    FNTOT
     C                   END
     C*
     C     EOSUB#        IFNE      LASTSB
     C*
     C*  IF SUBSCRIBER CHANGES, BUT FUND DOES NOT, MUST STILL PRINT THE
     C*     PREVIOUS FUND TOTALS ...
     C*
     C     EOFND         IFEQ      LASTFN
     C   10              EXCEPT    FNTOT
     C                   END
     C*
     C                   EXSR      SRSBBK
     C*
     C                   Z-ADD     EOSUB#        XXSUB#
     C                   Z-ADD     EOFND         XXFND
     C                   Z-ADD     EOFND         LASTFN
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C   10              EXCEPT    HEAD1
     C   10              EXCEPT    SUBSC
     C   10              EXCEPT    FUND
     C   10              EXCEPT    HEAD
     C                   END
     C*
     C     EOFND         IFNE      LASTFN
     C*
     C                   Z-ADD     EOSUB#        XXSUB#            5 0
     C                   Z-ADD     EOFND         XXFND             3 0
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C   10              EXCEPT    HEAD1
     C   10              EXCEPT    SUBSC
     C   10              EXCEPT    FUND
     C   10              EXCEPT    HEAD
     C                   END
     C*
     C*  GET REVIEWER AMOUNT
     C*
     C                   Z-ADD     EOSUB#        XXSUB#
     C                   Z-ADD     EOFND         XXFND
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOCASE        XXCASE
     C                   Z-ADD     EOEOB#        XXEOB#
     C*
     C     EOSYS         IFEQ      'P'
     C                   Z-ADD     EOBAMT        XISMUS            9 2
     C                   END                                                    SYS = P
     C*
     C     EOSYS         IFEQ      'U'
     C*** BEG CHG *** 01/07/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EABAMT        XISMUS
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     PBKEY         CHAIN     PPB01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     PBBAMT        XISMUS            9 2
     C                   END                                                    *IN65
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/07/99 ***************************************
     C                   END                                                    SYS = U
     C*
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     EOSYS         IFEQ      'M'
     C*** BEG CHG *** 01/07/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EABAMT        XISMUS
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     MBKEY         CHAIN     PMB01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     MBBAMT        XISMUS            9 2
     C                   END                                                    *IN65
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/07/99 ***************************************
     C                   END                                                    SYS = M
     C*
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     EOSYS         IFEQ      'H'
     C*** BEG CHG *** 01/07/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EABAMT        XISMUS
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     HBKEY         CHAIN     PHB01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     HB1AAM        XISMUS            9 2
     C                   END                                                    *IN65
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/07/99 ***************************************
     C                   END                                                    SYS = H
     C*
     C     EOSYS         IFEQ      'N'
     C*** BEG CHG *** 01/07/99 ***************************************
     C     ##PRVB        IFEQ      'Y'
     C     EAKEY         CHAIN     PEA01                              65
     C     *IN65         IFEQ      *OFF
     C                   Z-ADD     EANAMT        XISMUS
     C                   ENDIF
     C                   ELSE                                                   ##PRVB = Y
     C     TRKEY         CHAIN     PTR01                              65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     TRBAMT        XISMUS
     C                   END                                                    *IN65
     C                   ENDIF                                                  ##PRVB = Y
     C*** END CHG *** 01/07/99 ***************************************
     C                   END                                                    SYS = N
     C*
     C* IF TIME/MATERIALS CHARGES HAVE BEEN ENTERED FOR THIS EOMB
     C* (EOFLG5 = "Y") THEN ACCUMULATE THE T/M CHARGES TOTALS
     C*
     C                   Z-ADD     *ZEROS        TOTTTM            9 2
     C                   Z-ADD     *ZEROS        XXHOUR            9 2
     C                   Z-ADD     *ZEROS        TOTHR             9 2
     C                   Z-ADD     *ZEROS        XXMILE            9 2
     C                   Z-ADD     *ZEROS        TOTML             9 2
     C                   Z-ADD     *ZEROS        XXMSCG            9 2
     C                   Z-ADD     *ZEROS        TOTCRF            9 2
     C*
     C     EOFLG5        IFEQ      'Y'
     C*
     C* WE ONLY WANT TO GET T/M CHARGES IF THIS IS NOT A NETWORK
     C* EOB#
     C*
     C     EOSYS         IFNE      'N'
     C*
     C     TMKEY         CHAIN     PTM01                              65
     C     *IN65         DOWEQ     '0'
     C     TMHRS         MULT      TMHRRT        XXHOUR
     C                   ADD       XXHOUR        TOTHR
     C     TMMILE        MULT      TMMIRT        XXMILE
     C                   ADD       XXMILE        TOTML
     C                   ADD       TMMSCG        XXMSCG
     C     TMKEY         READE     PTM01                                  65
     C                   END                                                    END DO = 65
     C     TOTHR         ADD       TOTML         TOTTTM
     C     XXMSCG        ADD       TOTTTM        TOTTTM
     C                   END                                                    END EOSYS
     C                   END                                                    END EOFLG5
     C*
     C* ACCUMULATE THE TOTAL CALCULATED REVIEWER FEE
     C*
     C     XISMUS        SUB       TOTTTM        TOTCRF
     C*
     C*  ACCUMULATE REVIEWER CHARGE TOTALS
     C*
     C                   ADD       XISMUS        LISMUS           11 2          LR TOTAL
     C                   ADD       XISMUS        SISMUS           10 2          SUBSC TOTAL
     C                   ADD       XISMUS        FISMUS           10 2          FUND TOTAL
     C                   ADD       XISMUS        CAISMU           10 2          CASE TOTAL
     C*
     C*  ACCUMLATE SYSTEM TOTALS
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C     EOSYS         LOOKUP    SY(X)                                  30
     C*
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
     C                   ADD       TOTTTM        SYTTM
     C                   ADD       TOTCRF        SYCRFE
     C                   ADD       XISMUS        SYSMUS
     C*
     C*   FIRST CHAIN TO CASE FILE TO GET SS#, THEN TO DEMO FILE TO GET
     C*     CLAIMANTS NAME ...
     C*
     C                   Z-ADD     EOSUB#        XXSUB#
     C                   Z-ADD     EOFND         XXFND
     C                   Z-ADD     EOFYR         XXFYR
     C                   Z-ADD     EOCASE        XXCASE
     C     CMKEY         CHAIN     PCM01                              95
B1   C     *IN95         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#             9
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     PCD01                              96
B2   C     *IN96         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME           15
X2   C                   ELSE                                                   ELSEIF *IN96
     C                   MOVE      *BLANKS       XXNAME
E2   C                   END                                                    END IF *IN96
     C*
X1   C                   ELSE                                                   ELSEIF *IN95
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
E1   C                   END                                                    END IF *IN95
     C*
     C     EOMOD         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN75
     C                   ELSE
     C                   MOVE      '0'           *IN75
     C                   END
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       xxSS# = MaskData('SSN':'XXSS#':xxSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C   10              EXCEPT    DET
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       xxSS# = UnMaskData('SSN':'XXSS#':xxSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ADD       1             LRTOTL            7 0
     C*  SUBSCRIBER TOTALS
     C                   ADD       EOAMTC        SBAMTC           10 2
     C                   ADD       XXTPD         SBAMTP           10 2
     C                   ADD       XXMRED        SBAMTR           10 2
     C                   ADD       EOOTHR        SBAMTO           10 2
     C                   ADD       EONDIS        SBAMTN           10 2
     C                   ADD       TOTTTM        SBAMTT           10 2
     C                   ADD       TOTCRF        SBAMTF           10 2
     C                   ADD       1             SBTOTL            5 0
     C*  FUND TOTALS
     C                   ADD       EOAMTC        FNAMTC           10 2
     C                   ADD       XXTPD         FNAMTP           10 2
     C                   ADD       XXMRED        FNAMTR           10 2
     C                   ADD       EOOTHR        FNAMTO           10 2
     C                   ADD       EONDIS        FNAMTN           10 2
     C                   ADD       TOTTTM        FNAMTT           10 2
     C                   ADD       TOTCRF        FNAMTF           10 2
     C                   ADD       1             FNTOTL            5 0
     C*  CASE TOTALS
     C                   ADD       EOAMTC        CAAMTC           10 2
     C                   ADD       XXTPD         CAAMTP           10 2
     C                   ADD       XXMRED        CAAMTR           10 2
     C                   ADD       EOOTHR        CAAMTO           10 2
     C                   ADD       EONDIS        CAAMTN           10 2
     C                   ADD       TOTTTM        CAAMTT           10 2
     C                   ADD       TOTCRF        CAAMTF           10 2
     C                   ADD       1             CATOTL            5 0
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD1
     C   10              EXCEPT    SUBSC
     C   10              EXCEPT    FUND
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C*  SAVE KEY TO CHECK FOR LEVEL BREAKS ...
     C*
     C                   Z-ADD     EOSUB#        LASTSB            5 0
     C                   MOVE      XXSBNM        LASTSN           40
     C                   Z-ADD     EOFND         LASTFN            3 0
     C                   Z-ADD     EOCASE        LASTCA            7 0
     C*
     C*  CLEAR WORK FIELDS FOR NEXT RECORD
     C*
     C                   Z-ADD     *ZEROS        XISMUS
     C                   Z-ADD     *ZEROS        XXMRED
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C*
     C                   READ      PEO01                                  60
     C*
E2   C                   END                                                     END *IN60
     C*
     C                   MOVE      '1'           *INLR
     C*
     C                   ADD       1             COUNTG            7 0           GROUP COUNT
     C                   ADD       1             COUNTS            7 0           SUBSC COUNT
     C                   ADD       1             COUNTT            7 0           FINAL COUNT
     C*
     C   10              EXCEPT    CATOT
     C   10              EXCEPT    FNTOT
     C*
     C                   EXSR      SRSBBK
     C*
     C                   EXSR      SRFIN
     C*
     C     TERM          TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found and (qqRtyp = 'B' or qqrtyp = 'D');
          Except Route;
       Endif;
       xxprt = '1';
       Chain Orkey Por01;
       If %found and (qqRtyp = 'B' or qqrtyp = 'S');
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
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
     C                   ADD       SYTTM         LRTTM
     C                   ADD       SYCRFE        LRCRFE
     C                   ADD       SYSMUS        LRSMUS
     C                   ADD       SYAMTC        ALAMTC
     C                   ADD       SYMRED        ALMRED
     C                   ADD       SYOTHR        ALOTHR
     C                   ADD       SYNDIS        ALNDIS
     C                   ADD       SYTPD         ALTPD
     C                   ADD       SYTTM         ALTTM
     C                   ADD       SYCRFE        ALCRFE
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
     C   10              EXCEPT    SBTOT
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
     C                   ENDSR
     C*****************************************************************
     C*                                                               *
     C* THIS SUBROUTINE PRINTS ALL FINAL TOTALS FOR ALL SYSTEMS       *
     C*                                                               *
     C*****************************************************************
     C*
     C     SRFIN         BEGSR
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
     C                   ADD       LRTTM         TLTTM
     C                   ADD       LRCRFE        TLCRFE
     C                   ADD       LRSMUS        TLSMUS
     C   11              EXCEPT    ENDTOT
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
     C   10              EXCEPT    ALLSUB
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
     C     SYTTM         ANDEQ     *ZEROS
     C     SYCRFE        ANDEQ     *ZEROS
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
     C     LRTTM         ANDEQ     *ZEROS
     C     LRCRFE        ANDEQ     *ZEROS
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                62 'INC. 1994,1995'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
     O*
     O          E            HEAD1          1
     O                       Q1PGM               10
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXTITL   129
     O                       XXTITL             122
     O*** END   CHG *** 01/27/00 **************************************
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O*
     O          E    12      HEAD1          1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFR  Y   99
     O**********                        104 'TO:'
     O**********               XXTO  Y  113
     O                                           81 'FROM:'
     O                       XXFR                92 '  /  /    '
     O                                           97 'TO:'
     O                       XXTO               108 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*
     O          E   N12 13   HEAD1          1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFR2 Y   99
     O**********                        104 'TO:'
     O**********               XXTO2 Y  113
     O                                           81 'FROM:'
     O                       XXFR2               92 '  /  /    '
     O                                           97 'TO:'
     O                       XXTO2              108 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD1          2
     O*
     O          E            HEAD           1
     O                                          188 'TOTAL'
     O          E            HEAD           1
     O                                           49 'ACCIDENT'
     O                                           65 'TRANS'
     O                                          112 'AMOUNT'
     O                                          125 'AMOUNT'
     O                                          135 'MRA'
     O                                          149 'OTHER'
     O                                          164 'NET DISC'
     O                                          176 'T/M'
     O                                          190 'REVIEWER'
     O          E            HEAD           2
     O                                            4 'YEAR'
     O                                           12 'CASE'
     O                                           27 'CLAIMANT NAME'
     O                                           37 'S.S. #'
     O                                           46 'DATE'
     O                                           57 'EOB#'
     O                                           65 'DATE'
     O                                           78 'FROM DATE'
     O                                           86 'TO DATE'
     O                                           95 'PROV ID#'
     O                                          100 'MOD'
     O                                          112 'CHARGED'
     O                                          124 'PAID'
     O                                          139 'REDUCTIONS'
     O                                          152 'REDUCTIONS'
     O                                          165 'REDUCTIONS'
     O                                          178 'CHARGES'
     O                                          187 'FEE'
     O                                          195 'SY'
     O                                          198 'ST'
     O*
     O          E            SUBSC          1
     O                                           12 'SUBSCRIBER :'
     O                       XXSUB#        Z     17
     O                       XXSBNM              60
     O*
     O          E            FUND           2
     O                                           12 'GROUP      :'
     O                       EOFND         Z     17
     O                       XXFNNM              60
     O*
     O          E            DET            1
     O                       EOFYR         Z      3
     O                       EOCASE        Z     12
     O                       XXNAME              29
     O                       XXSS#               40
     O                       XXACDT        Y     49
     O                       EOEOB#        Z     59
     O                       XXTRDT        Y     68
     O                       XXFRDT        Y     77
     O                       XXTODT        Y     86
     O                       EOFEIN              96
     O              N75                          97 '-'
     O              N75      EOMOD              100
     O* AMT CHARGED
     O                       EOAMTC        J    113
     O* AMT PAID
     O                       XXTPD         J    126
     O* REDUCTION AMT
     O                       XXMRED        J    139
     O* OTHER REDUCTIONS
     O                       EOOTHR        J    152
     O* NETWORK DISCOUNT REDUCTIONS
     O                       EONDIS        J    165
     O* TIME/MATERIALS CHARGES
     O                       TOTTTM        J    178
     O* REVIEWER CHARGE AMT
     O                       XISMUS        J    191
     O                       EOSYS              194
     O                       EOHRFL             197
     O*
     O          E            FNTOT          1
     O                                           27 'GROUP TOTALS :'
     O                                           45 'ITEMS...'
     O                       FNTOTL        JB    55
     O                                           70 'CASES...'
     O                       COUNTG        JB    80
     O                       FNAMTC        JB   113
     O                       FNAMTP        JB   126
     O                       FNAMTR        JB   139
     O                       FNAMTO        JB   152
     O                       FNAMTN        JB   165
     O                       FNAMTT        JB   178
     O                       FISMUS        JB   191
     O*
     O          E            CATOT          2
     O                                           27 'CASE TOTALS :'
     O                                           45 'ITEMS...'
     O                       CATOTL        JB    55
     O                       CAAMTC        JB   113
     O                       CAAMTP        JB   126
     O                       CAAMTR        JB   139
     O                       CAAMTO        JB   152
     O                       CAAMTN        JB   165
     O                       CAAMTT        JB   178
     O                       CAISMU        JB   191
     O*
     O*   SUBSCRIBER TOTALS
     O*
     O          E            SBTOT          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
     O*
     O          E            SBTOT          1
     O                       Q1PGM               10
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXTITL   129
     O                       XXTITL             122
     O*** END   CHG *** 01/27/00 **************************************
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O*
     O          E    12      SBTOT          1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFR  Y   99
     O**********                        104 'TO:'
     O**********               XXTO  Y  113
     O                                           81 'FROM:'
     O                       XXFR                92 '  /  /    '
     O                                           97 'TO:'
     O                       XXTO               108 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*
     O          E   N12 13   SBTOT          1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFR2 Y   99
     O**********                        104 'TO:'
     O**********               XXTO2 Y  113
     O                                           81 'FROM:'
     O                       XXFR2               92 '  /  /    '
     O                                           97 'TO:'
     O                       XXTO2              108 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            SBTOT          2
     O*
     O          E            SBTOT          2
     O                                           10 'SUBSCRIBER'
     O                       XXSUB#        Z     17
     O                       XXSBNM              60
     O*
     O          E            SBTOT          2
     O                                           27 'SUBSCRIBER TOTALS:'
     O                                           50 'ITEMS...'
     O                       SBTOTL        J     80
     O                                          110 'NUMBER OF CASES...'
     O                       COUNTS        J    120
     O*
     O          E            SBTOT          2
     O                                           50 'AMT CHARGED:'
     O                       SBAMTC        JB    80
     O*
     O          E            SBTOT          2
     O                                           50 'AMT PAID:'
     O                       SBAMTP        JB    80
     O*
     O          E            SBTOT          2
     O                                           50 'MRA REDUCTIONS:'
     O                       SBAMTR        JB    80
     O*
     O          E            SBTOT          2
     O                                           50 'OTHER REDUCTIONS:'
     O                       SBAMTO        JB    80
     O*
     O          E            SBTOT          2
     O                                           50 'NET. DISC REDUCTIONS:'
     O                       SBAMTN        JB    80
     O*
     O          E            SBTOT          2
     O                                           50 'TIME/MATERIALS CHARGES:'
     O                       SBAMTT        JB    80
     O*
     O          E            SBTOT          2
     O                                           50 'CALCULATED REVIEWER FEE:'
     O                       SBAMTF        JB    80
     O*
     O          E            SBTOT          2
     O                                           50 'TOTAL REVIEWER FEE :'
     O                       SISMUS        JB    80
     O*
     O*   ALL SUBSCRIBER TOTALS
     O*
     O          E            ALLSUB         1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE        Y    198
     O*
     O          E            ALLSUB         1
     O                       Q1PGM               10
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXTITL   129
     O                       XXTITL             122
     O*** END   CHG *** 01/27/00 **************************************
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O*
     O          E    12      ALLSUB         1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFR  Y   99
     O**********                        104 'TO:'
     O**********               XXTO  Y  113
     O                                           81 'FROM:'
     O                       XXFR                92 '  /  /    '
     O                                           97 'TO:'
     O                       XXTO               108 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*
     O          E   N12 13   ALLSUB         1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFR2 Y   99
     O**********                        104 'TO:'
     O**********               XXTO2 Y  113
     O                                           81 'FROM:'
     O                       XXFR2               92 '  /  /    '
     O                                           97 'TO:'
     O                       XXTO2              108 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ALLSUB         2
     O*
     O          E            ALLSUB         2
     O                                           27 'ALL SUBSCRIBER TOTALS:'
     O                                           50 'ITEMS...'
     O                       LRTOTL        J     80
     O                                          110 'NUMBER OF CASES...'
     O                       COUNTT        J    120
     O*
     O          E            ALLSUB         2
     O                                           50 'AMT CHARGED:'
     O                       TLAMTC        J     80
     O*
     O          E            ALLSUB         2
     O                                           50 'AMT PAID:'
     O                       TLTPD         J     80
     O*
     O          E            ALLSUB         2
     O                                           50 'MRA REDUCTIONS:'
     O                       TLMRED        J     80
     O*
     O          E            ALLSUB         2
     O                                           50 'OTHER REDUCTIONS:'
     O                       TLOTHR        J     80
     O*
     O          E            ALLSUB         2
     O                                           50 'NET. DISC REDUCTIONS:'
     O                       TLNDIS        J     80
     O*
     O          E            ALLSUB         2
     O                                           50 'TIME/MATERIALS CHARGES:'
     O                       TLTTM         J     80
     O*
     O          E            ALLSUB         2
     O                                           50 'CALCULATED REVIEWER FEE:'
     O                       TLCRFE        J     80
     O*
     O          E            ALLSUB         2
     O                                           50 'TOTAL REVIEWER FEE :'
     O                       TLSMUS        J     80
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE1
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                62 'INC. 1994,1995'
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
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               YYTITL   129
     O                       YYTITL             122
     O*** END   CHG *** 01/27/00 **************************************
     O                                          194 'PAGE:'
     O                       PAGE1         Z    198
     O*
     O          E    12      THEAD          1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFR  Y   99
     O**********                        104 'TO:'
     O**********               XXTO  Y  113
     O                                           81 'FROM:'
     O                       XXFR                92 '  /  /    '
     O                                           97 'TO:'
     O                       XXTO               108 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*
     O          E   N12 13   THEAD          1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFR2 Y   99
     O**********                        104 'TO:'
     O**********               XXTO2 Y  113
     O                                           81 'FROM:'
     O                       XXFR2               92 '  /  /    '
     O                                           97 'TO:'
     O                       XXTO2              108 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            THEAD          2
     O          E   N70      THEAD          2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O*
     O          E            THEAD          3
     O                       HEADER             106
     O          E            THEAD          1
     O                                          177 'CALCULATED'
     O                                          190 'TOTAL'
     O          E            THEAD          2
     O                                           71 'AMOUNT CHARGED'
     O                                           85 'AMOUNT PAID'
     O                                          102 'MRA REDUCTIONS'
     O                                          121 'OTHER REDUCTIONS'
     O                                          147 'NETWORK DISC REDUCTIONS'
     O                                          162 'T/M CHARGES'
     O                                          178 'REVIEWER FEE'
     O                                          194 'REVIEWER FEE'
     O          E            SYTOT          2
     O                                           44 'SYSTEM '
     O                       SY(X)               45
     O                                           53 'TOTALS:'
     O                       SYAMTC        JB    71
     O                       SYTPD         JB    85
     O                       SYMRED        JB   102
     O                       SYOTHR        JB   121
     O                       SYNDIS        JB   147
     O                       SYTTM         JB   162
     O                       SYCRFE        JB   178
     O                       SYSMUS        JB   194
     O          E            ALTOT          2
     O                       DESCR               53
     O                       ALAMTC        JB    71
     O                       ALTPD         JB    85
     O                       ALMRED        JB   102
     O                       ALOTHR        JB   121
     O                       ALNDIS        JB   147
     O                       ALTTM         JB   162
     O                       ALCRFE        JB   178
     O                       ALSMUS        JB   194
     O          E            ALTOT          2
     O                                          110 'NUMBER OF ITEMS...'
     O                       SBTOTL        JB   120
     O          E            ALTOT          2
     O                                          110 'NUMBER OF CASES...'
     O                       COUNTS        JB   120
     O          E            ENDTOT         2
     O                                           44 'SYSTEM '
     O                       SY(X)               45
     O                                           53 'TOTALS:'
     O                       LRAMTC        J     71
     O                       LRTPD         J     85
     O                       LRMRED        J    102
     O                       LROTHR        J    121
     O                       LRNDIS        J    147
     O                       LRTTM         J    162
     O                       LRCRFE        J    178
     O                       LRSMUS        J    194
     O*
     O          E            TLTOT          2
     O                       DESCR               53
     O                       TLAMTC        J     71
     O                       TLTPD         J     85
     O                       TLMRED        J    102
     O                       TLOTHR        J    121
     O                       TLNDIS        J    147
     O                       TLTTM         J    162
     O                       TLCRFE        J    178
     O                       TLSMUS        J    194
     O          E            TLTOT          2
     O                                          110 'NUMBER OF ITEMS...'
     O                       LRTOTL        J    120
     O          E            TLTOT          2
     O                                          110 'NUMBER OF CASES...'
     O                       COUNTT        J    120
     O**
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
** FINAL TOTAL DESCRIPTION
FINAL TOTALS     :
** HEADER DESCRIPTIONS
SUBSCRIBER TOTALS
   FINAL TOTALS
