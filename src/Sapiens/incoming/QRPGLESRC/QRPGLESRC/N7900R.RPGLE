     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10 C:249 Project: SY215                                 *
     H*          C:189 Retrieve brand Name                            *
     H*                                                               *
     H* 01/27/00  C:62  Project: Y2K
     H*                 CHANGE DATES TO MDYY.
     H*                                                              **
     H*  01/11/99  C:94  PROJECT: MC152
     H*                  CHANGE PROGRAM TO USE FAEAP INSTEAD OF
     H*                  NAPBP, FAHBP, NATRP, FAMBP
     H*
     H*  07/05/95 C:35  REQ/PROJ: NW008                              **
     H*                 ADDED MEDICAL MANAGEMENT BILLING FILE        **
     H*                                                              **
     H*  04/15/96 C:20  PROJ: NWMNT                                  **
     H*                 1. MODIFICATION TO GET THE BILLED FLAG FROM  **
     H*                    THE APPROPRIATE FILE DEPENDING ON SYSTEM  **
     H*                    TYPE.  CURRENTLY CODE IS GETTING BILLING  **
     H*                    FLAG FROM FAEOP AND THIS IS NOT ALWAYS    **
     H*                    ACCURATE.                                 **
     H*****************************************************************
     H*  INDICATORS:                                                 **
     H*****************************************************************
     H*  PREFIX DESIGNATIONS FOR LEVEL BREAK TOTAL VARIABLES
     H*
     H*  INXXX = DAILY INVOICED TOTAL
     H*  UNXXX = DAILY UNINVOICED TOTAL
     H*  DAXXX = DAILY TOTAL
     H*
     H*
     H*  ALL SUMMARY REPORT TOTALS ARE KEPT IN A MULTIPLE OCCURENCE
     H*  DATA STRUCTURES SYSVAL, ALLTOT, FINTOT, AND TOTTOT
     H*
     H*  MULT. OCCUR. DATA STRUCTURE SYSTOT
     H*  (SUBSCRIBER TOTALS)
     H*                       SYS 1 (P)   SYS 2 (H)  SYS 3 (N) . . .
     H*      INVOICED             1           2          3     . . .
     H*    UNINVOICED            11          12         13     . . .
     H*           SUM            21          22         23     . . .
     H*
     H*  MULT. OCCUR. DATA STRUCTURE FINTOT:
     H*  (FINAL TOTALS)
     H*                       SYS 1 (P)   SYS 2 (H)  SYS 3 (N) . . .
     H*      INVOICED             1           2          3     . . .
     H*    UNINVOICED            11          12         13     . . .
     H*           SUM            21          22         23     . . .
     H*
     H*  MULT. OCCUR. DATA STRUCTURE ALLTOT:
     H*  (ALL SYSTEMS SUM FOR SUBSCRIBER TOTALS)
     H*                       ALL SYSTEMS
     H*      INVOICED             1
     H*    UNINVOICED             2
     H*           SUM             3
     H*
     H*  MULT. OCCUR. DATA STRUCTURE TOTTOT:
     H*  (ALL SYSTEMS SUM FOR FINAL TOTALS)
     H*                     ALL SYSTEMS
     H*      INVOICED             1
     H*    UNINVOICED             2
     H*           SUM             3
     H*
     H*
     H*  EACH TOTAL IS STORED IN THE RESPECTIVE OCCURENCE OF EACH
     H*  DATA STRUCTURE. THE PROGRAM IS DESIGNED TO HANDLE UP TO
     H*  10 SYSTEM TYPES.
     H*
     H*****************************************************************
     H/TITLE N7900R - EOB EDIT REPORT
     F* * * ROUTING FILE
     FSMORL     IF   E           K DISK
     F* * * COMPANY FILE
     FSMCOL     IF   E           K DISK
     F* * * FUND FILE
     FFMFNL     IF   E           K DISK
     F* * * USER PARAMETER FILE
     FSMUPL     IF   E           K DISK
     F* * * COMBINED EOB WORK FILE AND EOB DELETE FILE
     FTAEOL     IF   E           K DISK
     F* * * HBA FILE
     FFAHBL     IF   E           K DISK
     F* * * PRE/RE-CERT FILE
     FNAPBL1    IF   E           K DISK
     F*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F* * * MEDICAL MANAGEMENT
     FNAMBL1    IF   E           K DISK
     F*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     F* * * TIME/MATERIALS BILLING FILE
     FNDTML1    IF   E           K DISK
     F* * * CLAIM FILE
     FFMCML1    IF   E           K DISK
     F* * * DEMOGRAPHIC FILE
     FFMCDL     IF   E           K DISK
     F* * * SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F* * * NETWORK TRANSACTION FILE
     FNATRL     IF   E           K DISK
     F*** BEG ADD *** 01/11/99 ***************************************
     F* * * BILL HEADER AMOUNT FILE
     FFAEAL1    IF   E           K DISK
     F*** END ADD *** 01/11/99 ***************************************
     F* * * PRINTER FILES
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOF)

     D SY              S              1    DIM(10) ASCEND
     D TD              S             18    DIM(3) CTDATA PERRCD(1)
     D LD              S             18    DIM(3) CTDATA PERRCD(1)
     D HD              S             18    DIM(2) CTDATA PERRCD(1)
     D SYSVAL          DS                  OCCURS(30)
     D  SYSUM                  1     72
     D  SYAMTC                 1      9  2
     D  SYMRED                10     18  2
     D  SYOTHR                19     27  2
     D  SYNDIS                28     36  2
     D  SYTPD                 37     45  2
     D  SYTTM                 46     54  2
     D  SYCRFE                55     63  2
     D  SYSMUS                64     72  2
     D FINTOT          DS                  OCCURS(30)
     D  LRSUM                  1     72
     D  LRAMTC                 1      9  2
     D  LRMRED                10     18  2
     D  LROTHR                19     27  2
     D  LRNDIS                28     36  2
     D  LRTPD                 37     45  2
     D  LRTTM                 46     54  2
     D  LRCRFE                55     63  2
     D  LRSMUS                64     72  2
     D ALLTOT          DS                  OCCURS(3)
     D  ALAMTC                 1      5P 2
     D  ALMRED                 6     10P 2
     D  ALOTHR                11     15P 2
     D  ALNDIS                16     20P 2
     D  ALTPD                 21     25P 2
     D  ALTTM                 26     30P 2
     D  ALCRFE                31     35P 2
     D  ALSMUS                36     40P 2
     D TOTTOT          DS                  OCCURS(3)
     D  TLAMTC                 1      5P 2
     D  TLMRED                 6     10P 2
     D  TLOTHR                11     15P 2
     D  TLNDIS                16     20P 2
     D  TLTPD                 21     25P 2
     D  TLTTM                 26     30P 2
     D  TLCRFE                31     35P 2
     D  TLSMUS                36     40P 2
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
     D*
     D*** BEG ADD *** 01/11/99 ***************************************
     D*  WRITE/UPDATE DATA STRUCTURE.
     D                 DS
     D  KKWRUP                 1      7
     D  XXFAEO                 1      1
     D  XXFAE1                 2      2
     D  XXFAHB                 3      3
     D  XXNAPB                 4      4
     D  XXNAMB                 5      5
     D  XXNATR                 6      6
     D  XXFAEA                 7      7
     D*** END ADD *** 01/11/99 ***************************************
     D*
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
      **** BEGIN ADD **** 05/08/98 ************************************
     D DETAIL          C                   CONST('-DETAIL')
     D SUMMRY          C                   CONST('-SUMMARY')
      **** END   ADD **** 05/08/98 ************************************

     I*
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
     C*   KEY LIST FOR THE PRE/RE-CERT FILE
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
     C*** BEG ADD *** 01/11/99 ***************************************
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXEOB#
     C*
     C*** END ADD *** 01/11/99 ***************************************
     C     *LIKE         DEFINE    EOCO#         XXCO#
     C     *LIKE         DEFINE    EOSUB#        XXSUB#
     C     *LIKE         DEFINE    EOFND         XXFND
     C     *LIKE         DEFINE    EOFYR         XXFYR
     C     *LIKE         DEFINE    EOCASE        XXCASE
     C     *LIKE         DEFINE    EOEOB#        XXEOB#
     C     *LIKE         DEFINE    SMNAME        LASTSN
     C     *LIKE         DEFINE    EOSUB#        LASTSB
     C     *LIKE         DEFINE    EOTRDT        LASTDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQBSTA            1
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
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
     C                   TIME                    DSDTTM
     C*
     C*** BEG ADD *** 01/11/99 ***************************************
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
     C*** END ADD *** 01/11/99 ***************************************
     C*
     C                   DO        30            X
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
     C                   DO        30            X
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
     C*
     C                   DO        3             X
     C     X             OCCUR     ALLTOT
     C                   Z-ADD     0             ALAMTC
     C                   Z-ADD     0             ALMRED
     C                   Z-ADD     0             ALOTHR
     C                   Z-ADD     0             ALNDIS
     C                   Z-ADD     0             ALTPD
     C                   Z-ADD     0             ALTTM
     C                   Z-ADD     0             ALCRFE
     C                   Z-ADD     0             ALSMUS
     C                   END
     C*
     C*
     C                   DO        3             X
     C     X             OCCUR     TOTTOT
     C                   Z-ADD     0             TLAMTC
     C                   Z-ADD     0             TLMRED
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLTPD
     C                   Z-ADD     0             TLTTM
     C                   Z-ADD     0             TLCRFE
     C                   Z-ADD     0             TLSMUS
     C                   END
     C*
     C                   Z-ADD     0             NS                3 0
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVE QQFRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFRDT  60
     C                   MOVE      QQFRDT        XXFRDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           MOVE QQTODT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTODT  60
     C                   MOVE      QQTODT        XXTODT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQMODE        XXMODE            1
     C                   MOVE      '-'           XFHYP1
     C                   MOVE      '-'           XFHYP2
     C                   MOVE      '-'           XFHYP3
     C                   MOVE      '-'           XFHYP4
     C                   MOVE      HD(1)         HEADER           18
     C                   MOVE      '0'           *IN70
     C*
     C*   SET INDICATORS TO DETERMINE WHICH TOTAL LINE(S) TO PRINT
     C*
     C     QQBSTA        IFEQ      'B'
     C                   MOVE      '1'           *IN05
     C                   MOVE      '1'           *IN06
     C                   ELSE
     C     QQBSTA        IFEQ      'I'
     C                   MOVE      '1'           *IN05
     C                   ELSE
     C     QQBSTA        IFEQ      'U'
     C                   MOVE      '1'           *IN06
     C                   END
     C                   END
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
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      ORTITL        XXTITL           60
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
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********   10              EXCEPT    ROUTE
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
      **** BEGIN ADD **** 05/08/98 ************************************
     C     'SCRTYP'      IFEQ      'D'
     C     ORTITL        CAT       DETAIL:1      YYTITL
     C                   ELSE
     C     ORTITL        CAT       SUMMRY:1      YYTITL
     C                   ENDIF
      **** END   ADD **** 05/08/98 ************************************
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
     C                   MOVE      EOTRDT        LASTDT
     C                   MOVE      EOSUB#        LASTSB
     C                   MOVE      XXSBNM        LASTSN
     C*
     C*   PRINT THE HEADINGS
     C*
     C   10              EXCEPT    HEAD
     C*
     C     *IN90         DOWEQ     '0'
     C*
     C                   Z-ADD     EOTRDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXTRDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C     EOAMTC        SUB       EOAMT         XXTRED            9 2
     C     XXTRED        SUB       EOOTHR        XXMRED            9 2
     C     EOAMT         SUB       EONDIS        XXTPD             9 2
     C*
     C*  SET INDICATOR IF THIS IS A DELETED RECORD
     C*
     C                   MOVE      '0'           *IN07
     C     EOSTAT        IFEQ      'D'
     C*
     C                   MOVE      EOBAMT        XISMUS            9 2
     C                   MOVE      '1'           *IN07
     C*
     C                   ELSE
     C*
     C*  GET REVEIWER AMOUNT
     C*
     C     EOSYS         IFEQ      'P'
     C                   Z-ADD     EOBAMT        XISMUS            9 2
     C                   END                                                    SYS = P
     C*
     C     EOSYS         IFEQ      'U'
     C*** BEG CHG *** 01/11/99 ***************************************
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
     C*** END CHG *** 01/11/99 ***************************************
     C                   END                                                    SYS = U
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'M'
     C*** BEG CHG *** 01/11/99 ***************************************
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
     C*** END CHG *** 01/11/99 ***************************************
     C                   END                                                    SYS = M
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     EOSYS         IFEQ      'H'
     C*** BEG CHG *** 01/11/99 ***************************************
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
     C*** END CHG *** 01/11/99 ***************************************
     C                   END                                                    SYS = H
     C*
     C     EOSYS         IFEQ      'N'
     C*** BEG CHG *** 01/11/99 ***************************************
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
     C*** END CHG *** 01/11/99 ***************************************
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
     C                   END                                                    STAT = D
     C*
     C*  ACCUMLATE SYSTEM TOTALS, TRANS DATE TOTALS AND SUBSCRIBER
     C*  TOTALS
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C     EOBLFL        IFEQ      'P'
     C                   Z-ADD     10            INORUN            3 0
     C                   ELSE
     C                   Z-ADD     20            INORUN            3 0
     C                   END
     C*
     C                   EXSR      SRSYS
     C*
     C                   EXSR      SRTRD
     C*
     C*   GET SOCIAL SECURITY NUMBER, CLAIMANT NAME, AND ACCIDENT DATE
     C*
     C     CMKEY         CHAIN     PCM01                              95
     C     *IN95         IFEQ      '0'
     C                   MOVEL     CMSS#         XXSS#             9
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
     C**********           MOVELQQLAST    XXNAME 20
     C                   MOVEL     QQLAST        XXNAME           15
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
     C*** BEGIN CHG *** 04/15/96 ***********************************************
     C********** EOBLFL    IFEQ 'P'
     C**********           MOVE 'I'       XXBSTA  1
     C**********           ELSE
     C**********           MOVE 'U'       XXBSTA
     C**********           END
     C     EOSYS         IFEQ      'P'
     C     EOBLFL        IFEQ      'P'
     C                   MOVE      'I'           XXBSTA            1
     C                   ELSE                                                   ELSE EOBLFL EQ 'P'
     C                   MOVE      'U'           XXBSTA
     C                   END                                                    END EOBLFL EQ 'P'
     C                   ELSE                                                   ELSE EOSYS EQ 'P'
     C     EOSYS         IFEQ      'H'
     C     HBBLFL        IFEQ      'P'
     C                   MOVE      'I'           XXBSTA            1
     C                   ELSE                                                   ELSE HBBLFL EQ 'P'
     C                   MOVE      'U'           XXBSTA
     C                   END                                                    END HBBLFL EQ 'P'
     C                   ELSE                                                   ELSE EOSYS EQ 'H'
     C     EOSYS         IFEQ      'U'
     C     PBBLFL        IFEQ      'P'
     C                   MOVE      'I'           XXBSTA            1
     C                   ELSE                                                   ELSE PBBLFL EQ 'P'
     C                   MOVE      'U'           XXBSTA
     C                   END                                                    END PBBLFL EQ 'P'
     C                   ELSE                                                   ELSE EOSYS EQ 'U'
     C     EOSYS         IFEQ      'N'
     C     TRBLFL        IFEQ      'P'
     C                   MOVE      'I'           XXBSTA            1
     C                   ELSE                                                   ELSE TRBLFL EQ 'P'
     C                   MOVE      'U'           XXBSTA
     C                   END                                                    END TRBLFL EQ 'P'
     C                   ELSE                                                   ELSE EOSYS EQ 'N'
     C     EOSYS         IFEQ      'M'
     C     MBBLFL        IFEQ      'P'
     C                   MOVE      'I'           XXBSTA            1
     C                   ELSE                                                   ELSE MBBLFL EQ 'P'
     C                   MOVE      'U'           XXBSTA
     C                   END                                                    END MBBLFL EQ 'P'
     C                   END                                                    END EOSYS EQ 'M'
     C                   END                                                    END EOSYS EQ 'N'
     C                   END                                                    END EOSYS EQ 'U'
     C                   END                                                    END EOSYS EQ 'H'
     C                   END                                                    END EOSYS EQ 'P'
     C*** END ADD *** 04/15/96 *************************************************
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
     C*
     C     EOSUB#        IFNE      LASTSB
     C                   EXSR      SRTOT
     C                   EXSR      SRSBBK
     C                   ELSE
     C     EOTRDT        IFNE      LASTDT
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
     C                   Z-ADD     LASTDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     DAYDT   60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DAYDT             8 0
     C*** END   CHG *** 01/27/00 **************************************
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C   10              EXCEPT    DATOT
     C                   MOVE      EOTRDT        LASTDT
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C*****************************************************************
     C*                                                               *
     C* ACCUMLATE PARTICULAR SYSTEM TOTALS                            *
     C*                                                               *
     C*****************************************************************
     C     SRSYS         BEGSR
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
     C                   ADD       INORUN        X
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
     C                   ENDSR
     C*
     C*****************************************************************
     C*                                                               *
     C* ACCUMLATE DAILY (TRANSACTION DATE) TOTALS                     *
     C*                                                               *
     C*****************************************************************
     C*
     C     SRTRD         BEGSR
     C*
     C     EOBLFL        IFEQ      'P'
     C                   ADD       EOAMTC        INAMTC            9 2
     C                   ADD       XXMRED        INMRED            9 2
     C                   ADD       EOOTHR        INOTHR            9 2
     C                   ADD       EONDIS        INNDIS            9 2
     C                   ADD       XXTPD         INTPD             9 2
     C                   ADD       TOTTTM        INTTM             9 2
     C                   ADD       TOTCRF        INCRF             9 2
     C                   ADD       XISMUS        INSMUS            9 2
     C                   ELSE
     C                   ADD       EOAMTC        UNAMTC            9 2
     C                   ADD       XXMRED        UNMRED            9 2
     C                   ADD       EOOTHR        UNOTHR            9 2
     C                   ADD       EONDIS        UNNDIS            9 2
     C                   ADD       XXTPD         UNTPD             9 2
     C                   ADD       TOTTTM        UNTTM             9 2
     C                   ADD       TOTCRF        UNCRF             9 2
     C                   ADD       XISMUS        UNSMUS            9 2
     C                   END
     C                   ADD       EOAMTC        DAAMTC            9 2
     C                   ADD       XXMRED        DAMRED            9 2
     C                   ADD       EOOTHR        DAOTHR            9 2
     C                   ADD       EONDIS        DANDIS            9 2
     C                   ADD       XXTPD         DATPD             9 2
     C                   ADD       TOTTTM        DATTM             9 2
     C                   ADD       TOTCRF        DACRF             9 2
     C                   ADD       XISMUS        DASMUS            9 2
     C*
     C                   ENDSR
     C*****************************************************************
     C*                                                               *
     C* SUBSRCRIBER BREAK SUBROUTINE                                  *
     C*                                                               *
     C*****************************************************************
     C*
     C     SRSBBK        BEGSR
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
     C   11              EXCEPT    SYHEAD
     C*
     C     X             ADD       10            Y1                3 0
     C     Y1            OCCUR     SYSVAL
     C     Y1            OCCUR     FINTOT
     C     1             OCCUR     ALLTOT
     C                   MOVEL     TD(1)         DESCR            18
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
     C     X             ADD       20            Y1
     C     Y1            OCCUR     SYSVAL
     C     Y1            OCCUR     FINTOT
     C     2             OCCUR     ALLTOT
     C                   MOVEL     TD(2)         DESCR
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
     C     X             OCCUR     SYSVAL
     C     X             OCCUR     FINTOT
     C     3             OCCUR     ALLTOT
     C                   MOVEL     TD(3)         DESCR
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
     C     3             OCCUR     ALLTOT
     C   10              EXCEPT    SUBTOT
     C   11              EXCEPT    TLHEAD
     C*
     C                   DO        3             X
     C*
     C                   MOVE      LD(X)         DESCR
     C     X             OCCUR     ALLTOT
     C   11              EXCEPT    ALTOT
     C*
     C                   END
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
     C*
     C   11              EXCEPT    SYHEAD
     C*
     C     X             ADD       10            Y1                3 0
     C     Y1            OCCUR     FINTOT
     C     1             OCCUR     TOTTOT
     C                   MOVEL     TD(1)         DESCR            18
     C                   ADD       LRAMTC        TLAMTC
     C                   ADD       LRMRED        TLMRED
     C                   ADD       LROTHR        TLOTHR
     C                   ADD       LRNDIS        TLNDIS
     C                   ADD       LRTPD         TLTPD
     C                   ADD       LRTTM         TLTTM
     C                   ADD       LRCRFE        TLCRFE
     C                   ADD       LRSMUS        TLSMUS
     C   11              EXCEPT    LRTOT
     C*
     C     X             ADD       20            Y1
     C     Y1            OCCUR     FINTOT
     C     2             OCCUR     TOTTOT
     C                   MOVEL     TD(2)         DESCR
     C                   ADD       LRAMTC        TLAMTC
     C                   ADD       LRMRED        TLMRED
     C                   ADD       LROTHR        TLOTHR
     C                   ADD       LRNDIS        TLNDIS
     C                   ADD       LRTPD         TLTPD
     C                   ADD       LRTTM         TLTTM
     C                   ADD       LRCRFE        TLCRFE
     C                   ADD       LRSMUS        TLSMUS
     C   11              EXCEPT    LRTOT
     C*
     C     X             OCCUR     FINTOT
     C     3             OCCUR     TOTTOT
     C                   MOVEL     TD(3)         DESCR
     C                   ADD       LRAMTC        TLAMTC
     C                   ADD       LRMRED        TLMRED
     C                   ADD       LROTHR        TLOTHR
     C                   ADD       LRNDIS        TLNDIS
     C                   ADD       LRTPD         TLTPD
     C                   ADD       LRTTM         TLTTM
     C                   ADD       LRCRFE        TLCRFE
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
     C   11              EXCEPT    TLHEAD
     C*
     C     3             OCCUR     TOTTOT
     C   10              EXCEPT    LSTTOT
     C*
     C                   DO        3             X
     C*
     C                   MOVE      LD(X)         DESCR            18
     C     X             OCCUR     TOTTOT
     C   11              EXCEPT    TLTOT
     C*
     C                   END
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
     O                       XXFRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       XXTODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ROUTE          2
     O                                           19 'STATUS:'
     O                       QQBSTA              21
     O          E            ROUTE          2
     O                                           19 'SYSTEM:'
     O                       QQBTYP              21
     O          E            ROUTE          2
     O                                           19 'REPORT TYPE:'
     O                       QQRTYP              21
      **** END   ADD **** 05/11/98 ************************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
      **** BEGIN CHG **** 05/08/98 ************************************
     O********************************** 62 'INC. 1995,1996'
     O****                               46 'INC.'
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      **** END   CHG **** 05/08/98 ************************************
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
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXTITL   129
     O                       XXTITL             123
     O*** END   CHG *** 01/27/00 **************************************
     O                                          194 'PAGE:'
     O                       PAGE          Z    198
     O*
     O          E            HEAD           2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         90 'FROM:'
     O**********               XXFRDTY   99
     O**********                        104 'TO:'
     O**********               XXTODTY  113
     O                                           84 'FROM:'
     O                       XXFRDT              95 '  /  /    '
     O                                          100 'TO:'
     O                       XXTODT             111 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD           2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O          E            HEAD           1
     O                                          183 'TOTAL'
     O          E            HEAD           1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                         11 'TRANSACTION'
     O**********                         81 'ACCIDENT'
     O**********                         92 'AMOUNT'
     O**********                        103 'MRA'
     O**********                        118 'OTHER'
     O**********                        131 'NET DISC'
     O**********                        146 'RECOMMENDED'
     O**********                        155 'T/M'
     O**********                        171 'CALCULATED'
     O**********                        184 'REVIEWER'
     O**********                        193 'STATUS'
     O**********                        197 'SYS'
     O                                           11 'TRANSACTION'
     O                                           80 'ACCIDENT'
     O                                           91 'AMOUNT'
     O                                          102 'MRA'
     O                                          117 'OTHER'
     O                                          131 'NET DISC'
     O                                          145 'RECOMMENDED'
     O                                          154 'T/M'
     O                                          170 'CALCULATED'
     O                                          184 'REVIEWER'
     O                                          192 'STATUS'
     O                                          197 'SYS'
     O*** END   CHG *** 01/27/00 **************************************
     O**
     O**
     O          E            HEAD           2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          7 'DATE'
     O**********                         29 'FILE #'
     O**********                         51 'CLAIMANT NAME'
     O**********                         68 'S.S. #'
     O**********                         79 'DATE'
     O**********                         92 'CHARGED'
     O**********                        107 'REDUCTIONS'
     O**********                        120 'REDUCTIONS'
     O**********                        133 'REDUCTION'
     O**********                        145 'ALLOWANCE'
     O**********                        157 'CHARGES'
     O**********                        172 'REVIEWER FEE'
     O**********                        181 'FEE'
     O**********                        193 '(I,U)'
     O                                            7 'DATE'
     O                                           31 'FILE #'
     O                                           53 'CLAIMANT NAME'
     O                                           65 'S.S. #'
     O                                           78 'DATE'
     O                                           91 'CHARGED'
     O                                          106 'REDUCTIONS'
     O                                          119 'REDUCTIONS'
     O                                          132 'REDUCTION'
     O                                          144 'ALLOWANCE'
     O                                          156 'CHARGES'
     O                                          171 'REVIEWER FEE'
     O                                          182 'FEE'
     O                                          192 '(I,U)'
     O*** END   CHG *** 01/27/00 **************************************
     O**
     O          E            HEAD           2
     O                                          187 '(* = RECORD HAS BEEN'
     O                                          197 ' DELETED)'
     O**
     O          E            DET            1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXTRDT     8 '  /  /  '
     O**********               XF#       36
     O**********               XXNAME    58
     O**********               XXPRE     63
     O**********                         64 '-'
     O**********               XXMID     66
     O**********                         67 '-'
     O**********               XXSUF     71
     O**********               XXACDTY   81
     O**********               EOAMTCJ   94
     O**********               XXMREDJ  107
     O**********               EOOTHRJ  120
     O**********               EONDISJ  133
     O**********               XXTPD J  146
     O**********               TOTTTMJ  159
     O**********               TOTCRFJ  172
     O**********               XISMUSJ  185
     O**********               XXBSTA   190
     O**********               EOSYS    196
     O**********       07               197 '*'
     O                       XXTRDT              10 '  /  /    '
     O                       XF#                 38
     O                       XXNAME              55
     O                       XXPRE               60
     O                                           61 '-'
     O                       XXMID               63
     O                                           64 '-'
     O                       XXSUF               68
     O                       XXACDT              80 '  /  /    '
     O                       EOAMTC        J     94
     O                       XXMRED        J    107
     O                       EOOTHR        J    120
     O                       EONDIS        J    133
     O                       XXTPD         J    146
     O                       TOTTTM        J    159
     O                       TOTCRF        J    172
     O                       XISMUS        J    185
     O                       XXBSTA             190
     O                       EOSYS              196
     O               07                         197 '*'
     O*** END   CHG *** 01/27/00 **************************************
     O          E            DATOT          1
     O          E    05      DATOT          1
     O                                           81 'INVOICED   TOTALS:'
     O                       INAMTC        JB    94
     O                       INMRED        JB   107
     O                       INOTHR        JB   120
     O                       INNDIS        JB   133
     O                       INTPD         JB   146
     O                       INTTM         JB   159
     O                       INCRF         JB   172
     O                       INSMUS        JB   185
     O          E    06      DATOT          1
     O                                           81 'UNINVOICED TOTALS:'
     O                       UNAMTC        JB    94
     O                       UNMRED        JB   107
     O                       UNOTHR        JB   120
     O                       UNNDIS        JB   133
     O                       UNTPD         JB   146
     O                       UNTTM         JB   159
     O                       UNCRF         JB   172
     O                       UNSMUS        JB   185
     O          E            DATOT          2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               DAYDT     72 '  /  /  '
     O                       DAYDT               72 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O                                           81 'TOTALS:'
     O                       DAAMTC        JB    94
     O                       DAMRED        JB   107
     O                       DAOTHR        JB   120
     O                       DANDIS        JB   133
     O                       DATPD         JB   146
     O                       DATTM         JB   159
     O                       DACRF         JB   172
     O                       DASMUS        JB   185
     O          E            SUBTOT         2
     O                                           67 'SUBSCRIBER'
     O                       LASTSB        Z     73
     O                                           81 'TOTALS:'
     O                       ALAMTC        J     94
     O                       ALMRED        J    107
     O                       ALOTHR        J    120
     O                       ALNDIS        J    133
     O                       ALTPD         J    146
     O                       ALTTM         J    159
     O                       ALCRFE        J    172
     O                       ALSMUS        J    185
     O          E            LSTTOT         2
     O                                           81 'FINAL TOTALS:'
     O                       TLAMTC        J     94
     O                       TLMRED        J    107
     O                       TLOTHR        J    120
     O                       TLNDIS        J    133
     O                       TLTPD         J    146
     O                       TLTTM         J    159
     O                       TLCRFE        J    172
     O                       TLSMUS        J    185
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
     O                       XXFRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       XXTODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ROUTE1         2
     O                                           19 'STATUS:'
     O                       QQBSTA              21
     O          E            ROUTE1         2
     O                                           19 'SYSTEM:'
     O                       QQBTYP              21
     O          E            ROUTE1         2
     O                                           19 'REPORT TYPE:'
     O                       QQRTYP              21
      **** END   ADD **** 05/11/98 ************************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE1
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
      ****** END DLT ***** 01/28/10 **********************************
      **** BEGIN CHG **** 05/08/98 ************************************
     O********************************** 62 'INC. 1995,1996'
      ****** BEG CHG ***** 01/28/10 **********************************
     O***                                46 'INC.'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      **** END   CHG **** 05/08/98 ************************************
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
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               YYTITL   129
     O                       YYTITL             120
     O*** END   CHG *** 01/27/00 **************************************
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
     O                       XXFRDT              93 '  /  /    '
     O                                           98 'TO:'
     O                       XXTODT             107 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
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
     O          E            SYHEAD         2
     O                                           44 'SYSTEM: '
     O                       SY(X)
     O          E            SYTOT          2
     O                       DESCR               53
     O                       SYAMTC        JB    71
     O                       SYTPD         JB    85
     O                       SYMRED        JB   102
     O                       SYOTHR        JB   121
     O                       SYNDIS        JB   147
     O                       SYTTM         JB   162
     O                       SYCRFE        JB   178
     O                       SYSMUS        JB   194
     O          E            TLHEAD         2
     O                                           53 'ALL SYSTEM TOTALS :'
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
     O          E            LRTOT          2
     O                       DESCR               53
     O                       LRAMTC        J     71
     O                       LRTPD         J     85
     O                       LRMRED        J    102
     O                       LROTHR        J    121
     O                       LRNDIS        J    147
     O                       LRTTM         J    162
     O                       LRCRFE        J    178
     O                       LRSMUS        J    194
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
     O**
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
** SYSTEM TOTAL DESCRIPTIONS
  INVOICED       :
  UNINVOICED     :
  SYSTEM TOTALS  :
** FINAL TOTAL DESCRIPTIONS
  INVOICED       :
  UNINVOICED     :

** HEADER DESCRIPTIONS
SUBSCRIBER TOTALS
   FINAL TOTALS
