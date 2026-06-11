     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*
     H/TITLE F7153R - EOB Delete Edit Report - Report Program
     H*
     H*****************************************************************
     H*  CREATED 09/10/01 BY C:62 FOR PROJECT MC239                  **
     H*****************************************************************
     H*  REVISIONS:                                                  **
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
     FFMCDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFAEAL29   IF   E           K DISK
     FFMSML     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FQSYSPRT   O    F  206        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOF)

     D SY              S              1    DIM(10) ASCEND
     D LD              S             18    DIM(1) CTDATA PERRCD(1)
     D HD              S             18    DIM(2) CTDATA PERRCD(1)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D SYSVAL          DS                  OCCURS(10)
     D  SYCHRG                 1      6P 2
     D  SYMRA                  7     12P 2
     D  SYOTHR                13     18P 2
     D  SYOTH2                19     24P 2
     D  SYNDIS                25     30P 2
     D  SYPAID                31     36P 2
     D  SYBAMT                37     42P 2
     D FINTOT          DS                  OCCURS(10)
     D  LRCHRG                 1      6P 2
     D  LRMRA                  7     12P 2
     D  LROTHR                13     18P 2
     D  LROTH2                19     24P 2
     D  LRNDIS                25     30P 2
     D  LRPAID                31     36P 2
     D  LRBAMT                37     42P 2
     D ALLTOT          DS                  OCCURS(10)
     D  ALCHRG                 1      6P 2
     D  ALMRA                  7     12P 2
     D  ALOTHR                13     18P 2
     D  ALOTH2                19     24P 2
     D  ALNDIS                25     30P 2
     D  ALPAID                31     36P 2
     D  ALBAMT                37     42P 2
     D TOTTOT          DS                  OCCURS(10)
     D  TLCHRG                 1      6P 2
     D  TLMRA                  7     12P 2
     D  TLOTHR                13     18P 2
     D  TLOTH2                19     24P 2
     D  TLNDIS                25     30P 2
     D  TLPAID                31     36P 2
     D  TLBAMT                37     42P 2
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
     D                 DS
     D  XF#                    1     34
     D  XFSUB                  1      5  0
     D  XFHYP1                 6      6    INZ('-')
     D  XFFND                  7      9  0
     D  XFHYP2                10     10    INZ('-')
     D  XFYR                  11     12  0
     D  XFHYP3                13     13    INZ('-')
     D  XFCASE                14     20  0
     D  XFHYP4                21     21
     D  XFEOB#                22     30  0
     D  XFHYP5                31     31    INZ('-')
     D  XFRCN                 32     34  0
     D                 DS
     D  XXSS#                  1      9
     D  XXPRE                  1      3
     D  XXMID                  4      5
     D  XXSUF                  6      9

     I*

     C*****************************************************************
     C* PARAMETER LIST
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FRDT            7
     C                   PARM                    @@TODT            7
     C                   PARM                    @@BTYP            1
     C                   PARM                    @@RTYP            1
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
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
     C*****************************************************************
     C* KEY LIST
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE CLAIMAMT DEMOGRAPHIC FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXSS#
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*   KEY LIST EOB HEADER FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*****************************************************************
     C* LIKE DEFINES
     C*****************************************************************
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EASUB#        KKSUB#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EACASE        KKCASE
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         KKRCN
     C     *LIKE         DEFINE    EASS#         KKSS#
     C     *LIKE         DEFINE    SMNAME        LASTSN
     C     *LIKE         DEFINE    EASUB#        LASTSB
     C     *LIKE         DEFINE    EADT          LASTDT
     C     *LIKE         DEFINE    EASUB#        XXSUB#
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C                   DO        10            X
     C     X             OCCUR     SYSVAL
     C                   Z-ADD     0             SYCHRG
     C                   Z-ADD     0             SYMRA
     C                   Z-ADD     0             SYOTHR
     C                   Z-ADD     0             SYOTH2
     C                   Z-ADD     0             SYNDIS
     C                   Z-ADD     0             SYPAID
     C                   Z-ADD     0             SYBAMT
     C                   ENDDO
     C*
     C                   DO        10            X
     C     X             OCCUR     FINTOT
     C                   Z-ADD     0             LRCHRG
     C                   Z-ADD     0             LRMRA
     C                   Z-ADD     0             LROTHR
     C                   Z-ADD     0             LROTH2
     C                   Z-ADD     0             LRNDIS
     C                   Z-ADD     0             LRPAID
     C                   Z-ADD     0             LRBAMT
     C                   ENDDO
     C*
     C*
     C                   Z-ADD     0             ALCHRG
     C                   Z-ADD     0             ALMRA
     C                   Z-ADD     0             ALOTHR
     C                   Z-ADD     0             ALOTH2
     C                   Z-ADD     0             ALNDIS
     C                   Z-ADD     0             ALPAID
     C                   Z-ADD     0             ALBAMT
     C*
     C                   Z-ADD     0             TLCHRG
     C                   Z-ADD     0             TLMRA
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLOTH2
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLPAID
     C                   Z-ADD     0             TLBAMT
     C*
     C*
     C                   Z-ADD     0             NS                3 0
     C                   MOVE      @@FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FRDT            8 0
     C                   MOVE      @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TODT            8 0
     C*
     C                   MOVE      @@SUB#        XXSUB#
     C                   SETOFF                                       31
     C     XXSUB#        IFNE      *ZEROS
     C                   SETON                                        31
     C                   ENDIF
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@MODE        XXMODE            1
     C                   SETOFF                                       70
     C                   MOVE      HD(1)         HEADER           18
     C                   MOVE      LD(1)         DESCR            18
     C*
     C*   SET INDICATORS TO DETERMINE WHICH REPORTS TO PRINT
     C*   SUMMARY/DETAIL
     C*
     C                   SELECT
     C     @@RTYP        WHENEQ    'B'
     C                   SETON                                        1011
     C     @@RTYP        WHENEQ    'D'
     C                   SETON                                        10
     C     @@RTYP        WHENEQ    'S'
     C                   SETON                                        11
     C                   ENDSL
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   ENDIF
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT
     C*
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C     *IN10         IFEQ      *ON
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ORTITL        X1TITL           60
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXTITL           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT1
     C*
     C     *IN11         IFEQ      *ON
     C                   MOVE      '1'           XXPRT
     C     ORKY          CHAIN     POR01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      ORTITL        Y1TITL           60
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        YYTITL           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C                   ENDIF
     C                   ENDIF
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   ENDIF
     C*
     C*   READ EOB FILE
     C*
     C     EAKY          CHAIN     PEA01                              98
     C*
     C     *IN98         IFEQ      *ON
     C     *IN10         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    ERRMSG
     C                   ENDIF
     C     *IN11         IFEQ      *ON
     C                   EXCEPT    THEAD
     C                   EXCEPT    ERRMS1
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     EACO#         KKCO#
     C                   Z-ADD     EASUB#        KKSUB#
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN
     C*
     C*  CHAIN TO SUBSRCIBER FILE AND GET SUBSCRIBER NAME
     C*
     C*
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   ENDIF
     C*
     C*  CHAIN TO FUND FILE TO GET FUND NAME
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   ENDIF
     C*
     C                   Z-ADD     EASUB#        XFSUB
     C                   Z-ADD     EAFND         XFFND
     C                   Z-ADD     EAFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EACASE        XFCASE
     C                   Z-ADD     EAEOB#        XFEOB#
     C                   Z-ADD     EARCN         XFRCN
     C*
     C                   MOVE      EADT          LASTDT
     C                   MOVE      EASUB#        LASTSB
     C                   MOVE      XXSBNM        LASTSN
     C*
     C*   PRINT THE HEADINGS
     C*
     C     *IN10         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   ENDIF
     C*
     C     *IN98         DOWEQ     *OFF
     C*
     C                   Z-ADD     EADT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT              8 0
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*
     C*  ACCUMLATE SYSTEM TOTALS AND FINAL TOTALS
     C*
     C                   Z-ADD     1             X                 3 0
     C*
     C*
     C     EASYS         LOOKUP    SY(X)                                  99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             NS
     C                   MOVEL(P)  EASYS         SY(NS)
     C                   Z-ADD     NS            X
     C                   ENDIF
     C*
     C     X             OCCUR     SYSVAL
     C*
     C                   ADD       EACHRG        SYCHRG
     C                   ADD       EAMRA         SYMRA
     C                   ADD       EAOTHR        SYOTHR
     C                   ADD       EAOTH2        SYOTH2
     C                   ADD       EANDIS        SYNDIS
     C                   ADD       EAPAID        SYPAID
     C                   ADD       EABAMT        SYBAMT
     C*
     C                   ADD       EACHRG        DACHRG           11 2
     C                   ADD       EAMRA         DAMRA            11 2
     C                   ADD       EAOTHR        DAOTHR           11 2
     C                   ADD       EAOTH2        DAOTH2           11 2
     C                   ADD       EANDIS        DANDIS           11 2
     C                   ADD       EAPAID        DAPAID           11 2
     C                   ADD       EABAMT        DABAMT           11 2
     C*
     C*   GET SOCIAL SECURITY NUMBER, CLAIMANT NAME, AND ACCIDENT DATE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     CMSS#         KKSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXACDT            8 0
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     CDLSN         ##LAST           56
     C                   MOVEL     CDFSN         ##FRST           15
     C                   MOVEL     '1'           ##TERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST
     C                   PARM                    ##FRST
     C                   PARM                    ##TERM
     C                   MOVEL     ##LAST        XXNAME           15
     C                   ELSE                                                   ELSEIF *IN99
     C                   MOVE      *BLANKS       XXNAME
     C                   ENDIF                                                  END IF *IN99
     C                   ELSE                                                   ELSEIF *IN99
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C                   ENDIF                                                  END IF *IN99
     C*
     C     *IN10         IFEQ      *ON
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C                   ENDIF
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXSS#
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C*
     C     EAKY          READE     PEA01                                  98
     C*
     C                   Z-ADD     EACO#         KKCO#
     C                   Z-ADD     EASUB#        KKSUB#
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C                   Z-ADD     EARCN         KKRCN
     C*
     C                   Z-ADD     EASUB#        XFSUB
     C                   Z-ADD     EAFND         XFFND
     C                   Z-ADD     EAFYR         XFYR                           FILE NUMBER
     C                   Z-ADD     EACASE        XFCASE
     C                   Z-ADD     EAEOB#        XFEOB#
     C                   Z-ADD     EARCN         XFRCN
     C*
     C     EASUB#        IFNE      LASTSB
     C                   EXSR      SRTOT
     C                   EXSR      SRSBBK
     C                   ELSE
     C     EADT          IFNE      LASTDT
     C                   EXSR      SRTOT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDDO                                                   *IN98
     C*
     C                   EXSR      SRTOT
     C*
     C                   EXSR      SRSBBK
     C                   EXSR      FNTOT
     C*
     C                   ENDIF                                                   *IN98
     C*
     C                   SETON                                        LR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orky Por01;
       If %found and (@@rtyp = 'B' or @@rtyp = 'D');
          Except Route;
       Endif;
       xxprt = '1';
       Chain Orky Por01;
       If %found and (@@rtyp = 'B' or @@rtyp = 'S');
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*****************************************************************
     C* SRTOT - THIS SUBROUTINE PRINTS THE DAILY TOTALS               *
     C*****************************************************************
     C     SRTOT         BEGSR
     C*
     C                   Z-ADD     LASTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DAYDT             8 0
     C*
     C     *IN10         IFEQ      *ON
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C                   ENDIF
     C                   EXCEPT    DATOT
     C                   ENDIF
     C*
     C                   MOVE      EADT          LASTDT
     C*
     C                   ENDSR
     C*****************************************************************
     C* SRSBBK - SUBSRCRIBER BREAK SUBROUTINE                         *
     C*****************************************************************
     C     SRSBBK        BEGSR
     C*
     C     *IN11         IFEQ      *ON
     C                   EXCEPT    THEAD
     C                   ENDIF
     C*
     C                   DO        NS            X
     C*
     C*  ONLY PRINT SYSTEM TOTALS IF ONE OF THE SYSTEM TOTALS IS
     C*  NON-ZERO
     C*
     C                   MOVE      '0'           PRTSYS            1
     C                   EXSR      CHKSYS
     C*
     C     PRTSYS        IFEQ      '0'
     C*
     C     X             OCCUR     SYSVAL
     C     X             OCCUR     FINTOT
     C*
     C                   ADD       SYCHRG        LRCHRG
     C                   ADD       SYMRA         LRMRA
     C                   ADD       SYOTHR        LROTHR
     C                   ADD       SYOTH2        LROTH2
     C                   ADD       SYNDIS        LRNDIS
     C                   ADD       SYPAID        LRPAID
     C                   ADD       SYBAMT        LRBAMT
     C                   ADD       SYCHRG        ALCHRG
     C                   ADD       SYMRA         ALMRA
     C                   ADD       SYOTHR        ALOTHR
     C                   ADD       SYOTH2        ALOTH2
     C                   ADD       SYNDIS        ALNDIS
     C                   ADD       SYPAID        ALPAID
     C                   ADD       SYBAMT        ALBAMT
     C*
     C     *IN11         IFEQ      *ON
     C                   EXCEPT    SYTOT
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    THEAD
     C                   SETOFF                                       OF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  PRTSYS ?
     C                   ENDDO                                                  DO X
     C*
     C     *IN10         IFEQ      *ON
     C                   EXCEPT    SUBTOT
     C                   ENDIF
     C*
     C     *IN11         IFEQ      *ON
     C                   EXCEPT    ALTOT
     C                   ENDIF
     C*
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   ENDIF
     C*
     C                   MOVE      EASUB#        LASTSB
     C                   MOVE      XXSBNM        LASTSN
     C*
     C     *IN10         IFEQ      *ON
     C     *IN98         ANDEQ     *OFF
     C                   EXCEPT    HEAD
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* FNTOT-THIS SUBROUTINE PRINTS ALL FINAL TOTALS FOR ALL SYSTEMS *
     C*****************************************************************
     C     FNTOT         BEGSR
     C*
     C                   SETON                                        70
     C                   MOVE      HD(2)         HEADER
     C*
     C     *IN11         IFEQ      *ON
     C                   EXCEPT    THEAD
     C                   ENDIF
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
     C                   ADD       LRCHRG        TLCHRG
     C                   ADD       LRMRA         TLMRA
     C                   ADD       LROTHR        TLOTHR
     C                   ADD       LROTH2        TLOTH2
     C                   ADD       LRNDIS        TLNDIS
     C                   ADD       LRPAID        TLPAID
     C                   ADD       LRBAMT        TLBAMT
     C*
     C     *IN11         IFEQ      *ON
     C                   EXCEPT    LRTOT
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    THEAD
     C                   SETOFF                                       OF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  PRTSYS ?
     C                   ENDDO                                                  DO X
     C*
     C     *IN10         IFEQ      *ON
     C                   EXCEPT    LSTTOT
     C                   ENDIF
     C*
     C     *IN11         IFEQ      *ON
     C                   EXCEPT    TLTOT
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* CHKSYS - THIS SUBROUTINE DETERMINES IF ANY OF THE TOTALS FOR  *
     C* THE CURRENT SYSTEM ARE NON-ZERO. IF ALL TOTALS ARE ZERO DON'T *
     C* PRINT THIS SYSTEM                                             *
     C*****************************************************************
     C     CHKSYS        BEGSR
     C*
     C     *IN70         IFEQ      *OFF
     C     X             OCCUR     SYSVAL
     C     SYCHRG        IFEQ      *ZEROS
     C     SYMRA         ANDEQ     *ZEROS
     C     SYOTHR        ANDEQ     *ZEROS
     C     SYOTH2        ANDEQ     *ZEROS
     C     SYNDIS        ANDEQ     *ZEROS
     C     SYPAID        ANDEQ     *ZEROS
     C     SYBAMT        ANDEQ     *ZEROS
     C                   MOVE      '1'           PRTSYS
     C                   ENDIF
     C                   ELSE
     C     X             OCCUR     FINTOT
     C     LRCHRG        IFEQ      *ZEROS
     C     LRMRA         ANDEQ     *ZEROS
     C     LROTHR        ANDEQ     *ZEROS
     C     LROTH2        ANDEQ     *ZEROS
     C     LRNDIS        ANDEQ     *ZEROS
     C     LRPAID        ANDEQ     *ZEROS
     C     LRBAMT        ANDEQ     *ZEROS
     C                   MOVE      '1'           PRTSYS
     C                   ENDIF
     C                   ENDIF
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
     O**********     08      ORDIS1            +001
     O**********        09   Q1USER            +001
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
     O               08      ORDIS1            +001
     O                  09   Q1USER            +001
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       X1TITL              80
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
     O**********     08      OROUTQ            +001
     O**********        09   @@QUE             +001
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE       2  2
     O                                           19 'SUBSCRIBER:'
     O               31      XXSUB#              25
     O              N31                          24 '*ALL'
     O          E            ROUTE          2
     O                                           19 'DATE RANGE:'
     O                       X1FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X1TODT              44 '  /  /    '
     O          E            ROUTE          2
     O                                           19 'SYSTEM:'
     O                       @@BTYP              21
     O          E            ROUTE          2
     O                                           19 'REPORT TYPE:'
     O                       @@RTYP              21
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                46 'INC.'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             123
     O                       DSDATE             206 '  /  /    '
     O**
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             133
     O                                          202 'PAGE:'
     O                       PAGE          Z    206
     O*
     O          E            HEAD           3
     O                                           92 'FROM:'
     O                       X1FRDT             103 '  /  /    '
     O                                          107 'TO:'
     O                       X1TODT             118 '  /  /    '
     O          E            HEAD           2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O          E            HEAD           1
     O                                            9 'DELETE'
     O                                           81 'TRANS'
     O                                           95 'DATE OF '
     O                                          192 'REVIEWER'
     O**
     O          E            HEAD           2
     O                                            8 'DATE'
     O                                           23 'CASE #'
     O                                           42 'EOMB #'
     O                                           61 'CLAIMANT NAME'
     O                                           71 'S.S. #'
     O                                           81 'DATE'
     O                                           93 'INJURY'
     O                                          108 'CHARGED'
     O                                          122 'MRA'
     O                                          136 'OTHER 1'
     O                                          150 'OTHER 2'
     O                                          164 'NETWORK'
     O                                          178 'PAID'
     O                                          192 'FEE'
     O                                          197 'SYS'
     O**
     O          E            DET            1
     O                       XXDT                10 '  /  /    '
     O                       XF#               +001
     O                       XXNAME            +001
     O                       XXPRE             +001
     O                                         +000 '-'
     O                       XXMID             +000
     O                                         +000 '-'
     O                       XXSUF             +000
     O                       XXTRDT            +001 '  /  /    '
     O                       XXACDT            +001 '  /  /    '
     O                       EACHRG        L   +001
     O                       EAMRA         L   +001
     O                       EAOTHR        L   +001
     O                       EAOTH2        L   +001
     O                       EANDIS        L   +001
     O                       EAPAID        L   +001
     O                       EABAMT        L   +003
     O                       EASYS             +002
     O          E            DATOT          1
     O          E            DATOT          2
     O                       DAYDT               86 '  /  /    '
     O                                           95 'TOTALS:'
     O                       DACHRG        LB  +001
     O                       DAMRA         LB  +001
     O                       DAOTHR        LB  +001
     O                       DAOTH2        LB  +001
     O                       DANDIS        LB  +001
     O                       DAPAID        LB  +001
     O                       DABAMT        LB  +001
     O*
     O          E            SUBTOT         2
     O                                           80 'SUBSCRIBER'
     O                       LASTSB        Z     86
     O                                           95 'TOTALS:'
     O                       ALCHRG        L   +001
     O                       ALMRA         L   +001
     O                       ALOTHR        L   +001
     O                       ALOTH2        L   +001
     O                       ALNDIS        L   +001
     O                       ALPAID        L   +001
     O                       ALBAMT        L   +001
     O          E            LSTTOT         2
     O                                           95 'FINAL TOTALS:'
     O                       TLCHRG        L   +001
     O                       TLMRA         L   +001
     O                       TLOTHR        L   +001
     O                       TLOTH2        L   +001
     O                       TLNDIS        L   +001
     O                       TLPAID        L   +001
     O                       TLBAMT        L   +001
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
     O**********     08      ORDIS1            +001
     O**********        09   Q1USER            +001
     O**********E            ROUTE1         1
     O**********     08      ORDIS2              60
     O**********E            ROUTE1         1
     O**********     08      ORDIS3              60
     O**********E            ROUTE1         2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 *********************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1            +001
     O                  09   Q1USER            +001
     O          E            ROUTE1         1
     O               08      ORDIS2              60
     O          E            ROUTE1         1
     O               08      ORDIS3              60
     O          E            ROUTE1         2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 *********************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       Y1TITL              80
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
     O**********        09   @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE1      2  2
     O                                           19 'SUBSCRIBER:'
     O               31      XXSUB#              25
     O              N31                          24 '*ALL'
     O          E            ROUTE1         2
     O                                           19 'DATE RANGE:'
     O                       X1FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X1TODT              44 '  /  /    '
     O          E            ROUTE1         2
     O                                           19 'SYSTEM:'
     O                       @@BTYP              21
     O          E            ROUTE1         2
     O                                           19 'REPORT TYPE:'
     O                       @@RTYP              21
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               46 'INC.'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            THEAD          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             119
     O                       DSDATE             198 '  /  /    '
     O**
     O          E            THEAD          1
     O                       Q1PGM               10
     O                       YYTITL             129
     O                                          194 'PAGE:'
     O                       PAGE1         Z    198
     O*
     O          E            THEAD          3
     O                                           90 'FROM:'
     O                       X1FRDT             100 '  /  /    '
     O                                          104 'TO:'
     O                       X1TODT             114 '  /  /    '
     O          E   N70      THEAD          2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O*
     O          E            THEAD          3
     O                       HEADER             106
     O          E            THEAD          2
     O                                           68 'CHARGED'
     O                                           84 'PAID'
     O                                          100 'MRA'
     O                                          116 'OTHER 1'
     O                                          132 'OTHER 2'
     O                                          148 'NETWORK'
     O                                          164 'REVIEWER FEE'
     O          E            SYTOT          2
     O                                           44 'SYSTEM '
     O                       SY(X)               45
     O                                           53 'TOTALS:'
     O                       SYCHRG        JB  +001
     O                       SYPAID        JB  +001
     O                       SYMRA         JB  +001
     O                       SYOTHR        JB  +001
     O                       SYOTH2        JB  +001
     O                       SYNDIS        JB  +001
     O                       SYBAMT        JB  +001
     O          E            ALTOT          2
     O                       DESCR               53
     O                       ALCHRG        JB  +001
     O                       ALPAID        JB  +001
     O                       ALMRA         JB  +001
     O                       ALOTHR        JB  +001
     O                       ALOTH2        JB  +001
     O                       ALNDIS        JB  +001
     O                       ALBAMT        JB  +001
     O          E            LRTOT          2
     O                                           44 'SYSTEM '
     O                       SY(X)               45
     O                                           53 'TOTALS:'
     O                       LRCHRG        J   +001
     O                       LRPAID        J   +001
     O                       LRMRA         J   +001
     O                       LROTHR        J   +001
     O                       LROTH2        J   +001
     O                       LRNDIS        J   +001
     O                       LRBAMT        J   +001
     O*
     O          E            TLTOT          2
     O                       DESCR               53
     O                       TLCHRG        J   +001
     O                       TLPAID        J   +001
     O                       TLMRA         J   +001
     O                       TLOTHR        J   +001
     O                       TLOTH2        J   +001
     O                       TLNDIS        J   +001
     O                       TLBAMT        J   +001
     O**
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
** FINAL TOTAL DESCRIPTION
FINAL TOTALS     :
** HEADER DESCRIPTIONS
SUBSCRIBER TOTALS
   FINAL TOTALS
