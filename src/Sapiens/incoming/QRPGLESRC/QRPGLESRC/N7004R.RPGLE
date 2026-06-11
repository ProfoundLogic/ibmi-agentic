     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H/TITLE F7004R - UNBILLED CHARGES REPORT:PBILLS
     H*****************************************************************
     H*  CREATED 04/05/01 BY C:62 FOR PROJECT AR034                  **
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
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
     F* * * ROUTING FILE
     FSMORL     IF   E           K DISK
     F* * * COMPANY FILE
     FSMCOL     IF   E           K DISK
     F* * * FUND FILE
     FFMFNL     IF   E           K DISK
     F* * * USER PARAMETER FILE
     FSMUPL     IF   E           K DISK
     F* * * TIME/MATERIALS BILLING FILE
     FNDTML1    IF   E           K DISK
     F* * * CLAIM FILE
     FFMCML1    IF   E           K DISK
     F* * * DEMOGRAPHIC FILE
     FFMCDL     IF   E           K DISK
     F* * * SUBSCRIBER FILE
     FFMSML     IF   E           K DISK
     F* * * BILL HEADER AMOUNT FILE
     FFAEAL28   IF   E           K DISK
     FFAEOL4    IF   E           K DISK
     F* * * PRINTER FILES
     FQSYSPRT   O    F  206        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOF)

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

     D SY              S              1    DIM(10) ASCEND
     D LD              S             18    DIM(1) CTDATA PERRCD(1)
     D HD              S             18    DIM(2) CTDATA PERRCD(1)
     D SYSVAL          DS                  OCCURS(10)
     D  SYSUM                  1     99
     D  SYCHRG                 1     11  2
     D  SYMRA                 12     22  2
     D  SYOTHR                23     33  2
     D  SYOTH2                34     44  2
     D  SYNDIS                45     55  2
     D  SYPAID                56     66  2
     D  SYTTM                 67     77  2
     D  SYCRFE                78     88  2
     D  SYSMUS                89     99  2
     D*
     D FINTOT          DS                  OCCURS(10)
     D  LRSUM                  1     99
     D  LRCHRG                 1     11  2
     D  LRMRA                 12     22  2
     D  LROTHR                23     33  2
     D  LROTH2                34     44  2
     D  LRNDIS                45     55  2
     D  LRPAID                56     66  2
     D  LRTTM                 67     77  2
     D  LRCRFE                78     88  2
     D  LRSMUS                89     99  2
     D ALLTOT          DS
     D  ALCHRG                 1      6P 2
     D  ALMRA                  7     12P 2
     D  ALOTHR                13     18P 2
     D  ALOTH2                19     24P 2
     D  ALNDIS                25     30P 2
     D  ALPAID                31     36P 2
     D  ALTTM                 37     42P 2
     D  ALCRFE                43     48P 2
     D  ALSMUS                49     54P 2
     D TOTTOT          DS
     D  TLCHRG                 1      6P 2
     D  TLMRA                  7     12P 2
     D  TLOTHR                13     18P 2
     D  TLOTH2                19     24P 2
     D  TLNDIS                25     30P 2
     D  TLPAID                31     36P 2
     D  TLTTM                 37     42P 2
     D  TLCRFE                43     48P 2
     D  TLSMUS                49     54P 2
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
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@SUB#            5
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C                   PARM                    @@RTYP            1
     C                   PARM                    @@FLAG            1
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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE SUBSCRIBER FILE
     C*
     C     SMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
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
     C                   KFLD                    KKPRT             1
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
     C*   KEY LIST FOR THE CLAIMAMT DEMOGRAPHIC FILE
     C*
     C     CDKY          KLIST
     C                   KFLD                    KKSS#
     C*
     C*   KEY LIST FOR THE TIME/MATERIALS BILLING FILE
     C*
     C     TMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C*  *LIKE DEFINES
     C*
     C     *LIKE         DEFINE    EACO#         KKCO#
     C     *LIKE         DEFINE    EASUB#        KKSUB#
     C     *LIKE         DEFINE    EASUB#        XXSUB#
     C     *LIKE         DEFINE    EAFND         KKFND
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EACASE        KKCASE
     C     *LIKE         DEFINE    EAEOB#        KKEOB#
     C     *LIKE         DEFINE    EARCN         KKRCN
     C     *LIKE         DEFINE    CDSS#         KKSS#
     C*
     C                   MOVE      @@SUB#        XXSUB#
     C                   SETON                                        31
     C     XXSUB#        IFNE      *ZEROS
     C                   SETOFF                                       31
     C                   ENDIF
     C*
     C                   SETON                                        32
     C     @@FLAG        IFNE      *BLANKS
     C                   SETOFF                                       32
     C                   ENDIF
     C*
     C                   MOVE      @@FRDT        XXFR              8 0
     C                   MOVE      @@TODT        XXTO              8 0
     C                   SETON                                        33
     C     XXFR          IFNE      *ZEROS
     C                   SETOFF                                       33
     C                   ENDIF
     C*
     C                   DO        10            X
     C     X             OCCUR     SYSVAL
     C                   Z-ADD     0             SYCHRG
     C                   Z-ADD     0             SYMRA
     C                   Z-ADD     0             SYOTHR
     C                   Z-ADD     0             SYOTH2
     C                   Z-ADD     0             SYNDIS
     C                   Z-ADD     0             SYPAID
     C                   Z-ADD     0             SYTTM
     C                   Z-ADD     0             SYCRFE
     C                   Z-ADD     0             SYSMUS
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
     C                   Z-ADD     0             LRTTM
     C                   Z-ADD     0             LRCRFE
     C                   Z-ADD     0             LRSMUS
     C                   ENDDO
     C*
     C                   Z-ADD     0             ALCHRG
     C                   Z-ADD     0             ALMRA
     C                   Z-ADD     0             ALOTHR
     C                   Z-ADD     0             ALOTH2
     C                   Z-ADD     0             ALNDIS
     C                   Z-ADD     0             ALPAID
     C                   Z-ADD     0             ALTTM
     C                   Z-ADD     0             ALCRFE
     C                   Z-ADD     0             ALSMUS
     C*
     C                   Z-ADD     0             TLCHRG
     C                   Z-ADD     0             TLMRA
     C                   Z-ADD     0             TLOTHR
     C                   Z-ADD     0             TLOTH2
     C                   Z-ADD     0             TLNDIS
     C                   Z-ADD     0             TLPAID
     C                   Z-ADD     0             TLTTM
     C                   Z-ADD     0             TLCRFE
     C                   Z-ADD     0             TLSMUS
     C*
     C*
     C                   Z-ADD     0             NS                3 0
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@SUB#        KKSUB#
     C                   MOVE      @@MODE        XXMODE            1
     C                   MOVE      '0'           *IN70
     C                   MOVE      HD(1)         HEADER           18
     C                   MOVE      LD(1)         DESCR            18
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              99
     C     *IN99         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   ENDIF
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKY          CHAIN     PCO01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   ENDIF
     C                   TIME                    DSDTTM
     C*
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
     C                   ENDIF
     C*
     C*   SET INDICATORS TO DETERMINE WHICH REPORTS TO PRINT
     C*   SUMMARY/DETAIL
     C*
     C     @@RTYP        IFEQ      'B'
     C                   MOVE      '1'           *IN10
     C                   MOVE      '1'           *IN11
     C                   ELSE
     C     @@RTYP        IFEQ      'D'
     C                   MOVE      '1'           *IN10
     C                   ELSE
     C     @@RTYP        IFEQ      'S'
     C                   MOVE      '1'           *IN11
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              99
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVEL(P)  ORTITL        X1TITL           60
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********   10              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   ENDIF
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR QSYSPRT1
     C*
     C                   MOVE      '1'           KKPRT
     C     ORKY          CHAIN     POR01                              99
     C     *IN99         IFEQ      '0'
     C*
     C                   MOVEL(P)  ORTITL        Y1TITL           60
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        YYTITL           60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********   11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C                   ENDIF
     C*
     C*   READ EOB FILE
     C*
     C                   READ      PEA01                                  98
     C*
     C     *IN98         IFEQ      '1'
     C   10              EXCEPT    HEAD1
     C   11              EXCEPT    THEAD
     C   10              EXCEPT    ERRMSG
     C   11              EXCEPT    ERRMS1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C*
     C                   MOVE      EASUB#        KKSUB#
     C                   MOVE      EAFND         KKFND
     C*
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   ENDIF
     C*
     C                   Z-ADD     EASUB#        LASTSB            5 0
     C                   MOVE      XXSBNM        LASTSN           40
     C                   Z-ADD     EAFND         LASTFN            3 0
     C                   Z-ADD     EACASE        LASTCA            7 0
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   ENDIF
     C*
     C*   PRINT THE HEADINGS FOR UNBILLED CHARGES REPORT
     C*
     C   10              EXCEPT    HEAD1
     C   10              EXCEPT    SUBSC
     C   10              EXCEPT    FUND
     C   10              EXCEPT    HEAD
     C*
     C     *IN98         IFEQ      '1'
     C   10              EXCEPT    HEAD1
     C   10              EXCEPT    ERRMSG
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   ENDIF
     C*
B1   C     *IN98         DOWEQ     '0'
     C*
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C     EOKY          CHAIN     PEO01                              99
     C*
     C                   Z-ADD     EOTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   Z-ADD     EOFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C     EACASE        IFNE      LASTCA
     C                   ADD       1             COUNTG            7 0           GROUP COUNT
     C                   ADD       1             COUNTS            7 0           SUBSC COUNT
     C                   ADD       1             COUNTT            7 0           FINAL COUNT
     C   10              EXCEPT    CATOT
     C                   ENDIF
     C*
     C     EAFND         IFNE      LASTFN
     C   10              EXCEPT    FNTOT
     C                   ENDIF
     C*
     C     EASUB#        IFNE      LASTSB
     C*
     C*  IF SUBSCRIBER CHANGES, BUT FUND DOES NOT, MUST STILL PRINT THE
     C*     PREVIOUS FUND TOTALS ...
     C*
     C     EAFND         IFEQ      LASTFN
     C   10              EXCEPT    FNTOT
     C                   ENDIF
     C*
     C                   EXSR      SRSBBK
     C*
     C                   Z-ADD     EASUB#        KKSUB#
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFND         LASTFN
     C*
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   END
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
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
     C     EAFND         IFNE      LASTFN
     C*
     C                   Z-ADD     EASUB#        KKSUB#
     C                   Z-ADD     EAFND         KKFND
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      '0'
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
     C                   Z-ADD     EASUB#        KKSUB#
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C                   Z-ADD     EAEOB#        KKEOB#
     C*
     C     EASYS         IFEQ      'P'
     C                   Z-ADD     EABAMT        XISMUS           10 2
     C                   ENDIF                                                  SYS = P
     C*
     C     EASYS         IFEQ      'U'
     C                   Z-ADD     EABAMT        XISMUS
     C                   ENDIF                                                  SYS = U
     C*
     C     EASYS         IFEQ      'M'
     C                   Z-ADD     EABAMT        XISMUS
     C                   ENDIF                                                  SYS = M
     C*
     C     EASYS         IFEQ      'H'
     C                   Z-ADD     EABAMT        XISMUS
     C                   ENDIF                                                  SYS = H
     C*
     C     EASYS         IFEQ      'N'
     C                   Z-ADD     EANAMT        XISMUS
     C                   ENDIF                                                  SYS = N
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
     C     EATMFL        IFEQ      'Y'
     C*
     C* WE ONLY WANT TO GET T/M CHARGES IF THIS IS NOT A NETWORK
     C* EOB#
     C*
     C     EASYS         IFNE      'N'
     C*
     C     TMKY          CHAIN     PTM01                              99
     C     *IN99         DOWEQ     *OFF
     C     TMHRS         MULT      TMHRRT        XXHOUR
     C                   ADD       XXHOUR        TOTHR
     C     TMMILE        MULT      TMMIRT        XXMILE
     C                   ADD       XXMILE        TOTML
     C                   ADD       TMMSCG        XXMSCG
     C     TMKY          READE     PTM01                                  99
     C                   ENDDO                                                  END DO = 99
     C     TOTHR         ADD       TOTML         TOTTTM
     C     XXMSCG        ADD       TOTTTM        TOTTTM
     C                   ENDIF                                                  END EASYS
     C                   ENDIF                                                  END EATMFL
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
     C     EASYS         LOOKUP    SY(X)                                  30
     C*
     C     *IN30         IFEQ      '0'
     C                   ADD       1             NS
     C                   MOVE      EASYS         SY(NS)
     C                   MOVE      NS            X
     C                   ENDIF
     C     X             OCCUR     SYSVAL
     C                   ADD       EACHRG        SYCHRG
     C                   ADD       EAMRA         SYMRA
     C                   ADD       EAOTHR        SYOTHR
     C                   ADD       EAOTH2        SYOTH2
     C                   ADD       EANDIS        SYNDIS
     C                   ADD       EAPAID        SYPAID
     C                   ADD       TOTTTM        SYTTM
     C                   ADD       TOTCRF        SYCRFE
     C                   ADD       XISMUS        SYSMUS
     C*
     C*   FIRST CHAIN TO CASE FILE TO GET SS#, THEN TO DEMO FILE TO GET
     C*     CLAIMANTS NAME ...
     C*
     C                   Z-ADD     EASUB#        KKSUB#
     C                   Z-ADD     EAFND         KKFND
     C                   Z-ADD     EAFYR         KKFYR
     C                   Z-ADD     EACASE        KKCASE
     C     CMKY          CHAIN     PCM01                              99
B1   C     *IN99         IFEQ      '0'
     C                   MOVEL     CMSS#         KKSS#
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C*  GET CLAIMANT NAME
     C*
     C     CDKY          CHAIN     PCD01                              99
B2   C     *IN99         IFEQ      '0'
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVEL     '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME           15
X2   C                   ELSE
     C                   MOVE      *BLANKS       XXNAME
E2   C                   ENDIF
X1   C                   ELSE
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
E1   C                   ENDIF
     C*
     C     EAMOD         IFEQ      *ZEROS
     C                   MOVE      '1'           *IN75
     C                   ELSE
     C                   MOVE      '0'           *IN75
     C                   ENDIF
     C*
      ***** Beg Add *** 06/09/20 *******************************************
      /Free
       kkSS# = MaskData('SSN':'KKSS#':KKSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************

     C   10              EXCEPT    DET

      ***** End Add *** 06/09/20 *******************************************
      /Free
       kkSS# = UnMaskData('SSN':'KKSS#':KKSS#);
      /End-free
      ***** End Add *** 06/09/20 *******************************************
     C*
     C                   ADD       1             LRTOTL            7 0
     C*  SUBSCRIBER TOTALS
     C                   ADD       EACHRG        SBCHRG           11 2
     C                   ADD       EAPAID        SBPAID           11 2
     C                   ADD       EAMRA         SBMRA            11 2
     C                   ADD       EAOTHR        SBOTHR           11 2
     C                   ADD       EAOTH2        SBOTH2           11 2
     C                   ADD       EANDIS        SBNDIS           11 2
     C                   ADD       TOTTTM        SBAMTT            9 2
     C                   ADD       TOTCRF        SBAMTF            9 2
     C                   ADD       1             SBTOTL            5 0
     C*  FUND TOTALS
     C                   ADD       EACHRG        FNCHRG           11 2
     C                   ADD       EAPAID        FNPAID           11 2
     C                   ADD       EAMRA         FNMRA            11 2
     C                   ADD       EAOTHR        FNOTHR           11 2
     C                   ADD       EAOTH2        FNOTHR
     C                   ADD       EANDIS        FNNDIS           11 2
     C                   ADD       TOTTTM        FNAMTT            9 2
     C                   ADD       TOTCRF        FNAMTF            9 2
     C                   ADD       1             FNTOTL            5 0
     C*  CASE TOTALS
     C                   ADD       EACHRG        CACHRG           11 2
     C                   ADD       EAPAID        CAPAID           11 2
     C                   ADD       EAMRA         CAMRA            11 2
     C                   ADD       EAOTHR        CAOTHR           11 2
     C                   ADD       EAOTH2        CAOTHR
     C                   ADD       EANDIS        CANDIS           11 2
     C                   ADD       TOTTTM        CAAMTT            9 2
     C                   ADD       TOTCRF        CAAMTF            9 2
     C                   ADD       1             CATOTL            5 0
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD1
     C   10              EXCEPT    SUBSC
     C   10              EXCEPT    FUND
     C   10              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   ENDIF
     C*
     C*  SAVE KEY TO CHECK FOR LEVEL BRZAKS ...
     C*
     C                   Z-ADD     EASUB#        LASTSB            5 0
     C                   MOVE      XXSBNM        LASTSN           40
     C                   Z-ADD     EAFND         LASTFN            3 0
     C                   Z-ADD     EACASE        LASTCA            7 0
     C*
     C*  CLEAR WORK FIELDS FOR NEXT RECORD
     C*
     C                   Z-ADD     *ZEROS        XISMUS
     C*
     C                   MOVE      *BLANKS       KKSS#
     C                   MOVE      *BLANKS       XXNAME
     C                   Z-ADD     *ZERO         XXACDT
     C*
     C                   READ      PEA01                                  98
     C*
E2   C                   ENDDO                                                   END *IN98
     C*
     C                   SETON                                        LR
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
       kkprt = '0';
       Chain Orky Por01;
       If %found and (@@rtyp = 'B' or @@rtyp = 'D');
          Except Route;
       Endif;
       kkprt = '1';
       Chain Orky Por01;
       If %found and (@@rtyp = 'B' or @@rtyp = 'S');
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
     C                   ADD       SYCHRG        LRCHRG
     C                   ADD       SYMRA         LRMRA
     C                   ADD       SYOTHR        LROTHR
     C                   ADD       SYOTH2        LROTH2
     C                   ADD       SYNDIS        LRNDIS
     C                   ADD       SYPAID        LRPAID
     C                   ADD       SYTTM         LRTTM
     C                   ADD       SYCRFE        LRCRFE
     C                   ADD       SYSMUS        LRSMUS
     C                   ADD       SYCHRG        ALCHRG
     C                   ADD       SYMRA         ALMRA
     C                   ADD       SYOTHR        ALOTHR
     C                   ADD       SYOTH2        ALOTH2
     C                   ADD       SYNDIS        ALNDIS
     C                   ADD       SYPAID        ALPAID
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
     C     SMKY          CHAIN     PSM01                              99
     C     *IN99         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSBNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSBNM
     C                   ENDIF
     C*
     C                   MOVE      EASUB#        LASTSB
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
     C                   ADD       LRCHRG        TLCHRG
     C                   ADD       LRMRA         TLMRA
     C                   ADD       LROTHR        TLOTHR
     C                   ADD       LROTH2        TLOTH2
     C                   ADD       LRNDIS        TLNDIS
     C                   ADD       LRPAID        TLPAID
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
     C     SYCHRG        IFEQ      *ZEROS
     C     SYMRA         ANDEQ     *ZEROS
     C     SYOTHR        ANDEQ     *ZEROS
     C     SYOTH2        ANDEQ     *ZEROS
     C     SYNDIS        ANDEQ     *ZEROS
     C     SYPAID        ANDEQ     *ZEROS
     C     SYTTM         ANDEQ     *ZEROS
     C     SYCRFE        ANDEQ     *ZEROS
     C     SYSMUS        ANDEQ     *ZEROS
     C                   MOVE      '1'           PRTSYS
     C                   END
     C                   ELSE
     C     X             OCCUR     FINTOT
     C     LRCHRG        IFEQ      *ZEROS
     C     LRMRA         ANDEQ     *ZEROS
     C     LROTHR        ANDEQ     *ZEROS
     C     LROTH2        ANDEQ     *ZEROS
     C     LRNDIS        ANDEQ     *ZEROS
     C     LRPAID        ANDEQ     *ZEROS
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
     O**********     08      OROUTQ              30
     O**********        09   @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O*
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
     O*
     O          E            ROUTE          2
     O                                           27 'SUBSCRIBER . . . :'
     O              N31      XXSUB#        Z   +001
     O               31                          32 '*ALL'
     O          E            ROUTE          2
     O                                           27 'HELD/RELEASED    :'
     O              N32      @@FLAG            +001
     O               32                          32 '*ALL'
     O          E            ROUTE          2
     O                                           27 'DATE RANGE . . . :'
     O              N33      XXFR              +000 '0  /  /    '
     O              N33                        +001 '-'
     O              N33      XXTO              +000 '0  /  /    '
     O               33                          32 '*ALL'
     O          E            ROUTE          2
     O                                           27 'TYPE OF OUTPUT   :'
     O                       @@RTYP            +001
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               62 'INC. 2001     '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*****************************************************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             123
     O                       DSDATE        Y    206
     O*
     O          E            HEAD1          1
     O                       Q1PGM               10
     O                       XXTITL             133
     O                                          202 'PAGE:'
     O                       PAGE          Z    206
     O*
     O          E   N33      HEAD1          1
     O                                           88 'FROM:'
     O                       XXFR                99 '  /  /    '
     O                                          104 'TO:'
     O                       XXTO               115 '  /  /    '
     O*
     O          E            HEAD1          2
     O*
     O          E            SUBSC          1
     O                                           12 'SUBSCRIBER :'
     O                       KKSUB#        Z     17
     O                       XXSBNM              60
     O*
     O          E            FUND           2
     O                                           12 'GROUP      :'
     O                       EAFND         Z     17
     O                       XXFNNM              60
     O*
     O          E            HEAD           1
     O                                          198 'TOTAL'
     O          E            HEAD           1
     O                                           49 'ACCIDENT'
     O                                           70 'TRANS'
     O                                           79 'FROM'
     O                                           87 'TO'
     O                                          117 'AMOUNT'
     O                                          131 'AMOUNT'
     O                                          145 'MRA'
     O                                          159 'OTHER 1 & 2'
     O                                          173 'NET DISC'
     O                                          185 'T/M'
     O                                          198 'REVIEWER'
     O          E            HEAD           2
     O                                            4 'YEAR'
     O                                           12 'CASE'
     O                                           27 'CLAIMANT NAME'
     O                                           37 'S.S. #'
     O                                           47 'DATE'
     O                                           59 'EOB#'
     O                                           63 'RCN'
     O                                           70 'DATE'
     O                                           79 'DATE'
     O                                           88 'DATE'
     O                                          100 'PROV ID#'
     O                                          104 'MOD'
     O                                          117 'CHARGED'
     O                                          131 'PAID'
     O                                          145 'REDUCTIONS'
     O                                          159 'REDUCTIONS'
     O                                          173 'REDUCTIONS'
     O                                          185 'CHARGES'
     O                                          198 'FEE'
     O                                          203 'SY'
     O                                          206 'ST'
     O*
     O          E            DET            1
     O                       EAFYR         Z      3
     O                       EACASE        Z     12
     O                       XXNAME              29
     O                       KKSS#               40
     O                       XXACDT        Y     49
     O                       EAEOB#        Z   +001
     O                       EARCN             +001 ' 0 '
     O                       XXTRDT        Y   +001
     O                       XXFRDT        Y   +001
     O                       XXTODT        Y   +001
     O                       EAFEIN            +001
     O                                         +000 '-'
     O                       EAMOD             +000
     O* AMT CHARGED
     O                       EACHRG        L   +001
     O* AMT PAID
     O                       EAPAID        L   +001
     O* REDUCTION AMT
     O                       EAMRA         L   +001
     O* OTHER REDUCTIONS
     O                       EAOTHR        L   +001
     O* NETWORK DISCOUNT REDUCTIONS
     O                       EANDIS        L   +001
     O* TIME/MATERIALS CHARGES
     O                       TOTTTM        L   +001
     O* REVIEWER CHARGE AMT
     O                       XISMUS        L   +001
     O                       EASYS             +002
     O                       EABHRF            +002
     O*
     O          E            FNTOT          1
     O                                           27 'GROUP TOTALS :'
     O                                           45 'ITEMS...'
     O                       FNTOTL        LB    55
     O                                           70 'CASES...'
     O                       COUNTG        LB    80
     O                       FNCHRG        LB   118
     O                       FNPAID        LB  +001
     O                       FNMRA         LB  +001
     O                       FNOTHR        LB  +001
     O                       FNNDIS        LB  +001
     O                       FNAMTT        LB  +001
     O                       FISMUS        LB  +001
     O*
     O          E            CATOT          2
     O                                           27 'CASE TOTALS :'
     O                                           45 'ITEMS...'
     O                       CATOTL        LB    55
     O                       CACHRG        LB   118
     O                       CAPAID        LB  +001
     O                       CAMRA         LB  +001
     O                       CAOTHR        LB  +001
     O                       CANDIS        LB  +001
     O                       CAAMTT        LB  +001
     O                       CAISMU        LB  +001
     O*
     O*   SUBSCRIBER TOTALS
     O*
     O          E            SBTOT          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             123
     O                       DSDATE        Y    206
     O*
     O          E            SBTOT          1
     O                       Q1PGM               10
     O                       XXTITL             133
     O                                          202 'PAGE:'
     O                       PAGE          Z    206
     O*
     O          E   N33      SBTOT          1
     O                                           88 'FROM:'
     O                       XXFR                99 '  /  /    '
     O                                          104 'TO:'
     O                       XXTO               115 '  /  /    '
     O*
     O          E            SBTOT          2
     O*
     O          E            SBTOT          2
     O                                           10 'SUBSCRIBER'
     O                       KKSUB#        Z     17
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
     O                       SBCHRG        JB  +002
     O*
     O          E            SBTOT          2
     O                                           50 'AMT PAID:'
     O                       SBPAID        JB  +002
     O*
     O          E            SBTOT          2
     O                                           50 'MRA REDUCTIONS:'
     O                       SBMRA         JB  +002
     O*
     O          E            SBTOT          2
     O                                           50 'OTHER REDUCTIONS:'
     O                       SBOTHR        JB  +002
     O*
     O          E            SBTOT          2
     O                                           50 'OTHER 2 REDUCTIONS:'
     O                       SBOTH2        JB  +002
     O*
     O          E            SBTOT          2
     O                                           50 'NET. DISC REDUCTIONS:'
     O                       SBNDIS        JB  +002
     O*
     O          E            SBTOT          2
     O                                           50 'TIME/MATERIALS CHARGES:'
     O                       SBAMTT        JB  +004
     O*
     O          E            SBTOT          2
     O                                           50 'CALCULATED REVIEWER FEE:'
     O                       SBAMTF        JB  +004
     O*
     O          E            SBTOT          2
     O                                           50 'TOTAL REVIEWER FEE :'
     O                       SISMUS        JB  +003
     O*
     O*   ALL SUBSCRIBER TOTALS
     O*
     O          E            ALLSUB         1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             123
     O                       DSDATE        Y    206
     O*
     O          E            ALLSUB         1
     O                       Q1PGM               10
     O                       XXTITL             133
     O                                          202 'PAGE:'
     O                       PAGE          Z    206
     O*
     O          E   N33      ALLSUB         1
     O                                           88 'FROM:'
     O                       XXFR                99 '  /  /    '
     O                                          104 'TO:'
     O                       XXTO               115 '  /  /    '
     O*
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
     O                       TLCHRG        J   +002
     O*
     O          E            ALLSUB         2
     O                                           50 'AMT PAID:'
     O                       TLPAID        J   +002
     O*
     O          E            ALLSUB         2
     O                                           50 'MRA REDUCTIONS:'
     O                       TLMRA         J   +002
     O*
     O          E            ALLSUB         2
     O                                           50 'OTHER REDUCTIONS:'
     O                       TLOTHR        J   +002
     O*
     O          E            ALLSUB         2
     O                                           50 'OTHER 2 REDUCTIONS:'
     O                       TLOTH2        J   +002
     O*
     O          E            ALLSUB         2
     O                                           50 'NET. DISC REDUCTIONS:'
     O                       TLNDIS        J   +002
     O*
     O          E            ALLSUB         2
     O                                           50 'TIME/MATERIALS CHARGES:'
     O                       TLTTM         J   +002
     O*
     O          E            ALLSUB         2
     O                                           50 'CALCULATED REVIEWER FEE:'
     O                       TLCRFE        J   +002
     O*
     O          E            ALLSUB         2
     O                                           50 'TOTAL REVIEWER FEE :'
     O                       TLSMUS        J   +002
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
      ***** Beg Add ***** 08/05/15 ************************************
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
      ***** End Add ***** 08/05/15 ************************************
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
     O*
     O          E            ROUTE1      2  2
     O                                           24 'REPORT SELECTIONS:'
     O*
     O          E            ROUTE1         2
     O                                           27 'SUBSCRIBER . . . :'
     O              N31      XXSUB#        Z   +001
     O               31                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           27 'HELD/RELEASED    :'
     O              N32      @@FLAG            +001
     O               32                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           27 'DATE RANGE . . . :'
     O              N33      XXFR              +000 '0  /  /    '
     O              N33                        +001 '-'
     O              N33      XXTO              +000 '0  /  /    '
     O               33                          32 '*ALL'
     O          E            ROUTE1         2
     O                                           27 'TYPE OF OUTPUT   :'
     O                       @@RTYP            +001
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               62 'INC. 2001     '
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
     O          E   N33      THEAD          1
     O                                           88 'FROM:'
     O                       XXFR                99 '  /  /    '
     O                                          104 'TO:'
     O                       XXTO               115 '  /  /    '
     O*
     O          E            THEAD          2
     O*
     O          E   N70      THEAD          2
     O                                           10 'SUBSCRIBER'
     O                       LASTSB        Z     17
     O                       LASTSN              60
     O*
     O          E            THEAD          3
     O                       HEADER             106
     O          E            THEAD          1
     O                                           49 'AMOUNT'
     O                                           66 'AMOUNT'
     O                                           83 'MRA'
     O                                          100 'OTHER'
     O                                          117 'OTHER 2'
     O                                          134 'NETWORK DISC'
     O                                          168 'CALCULATED'
     O                                          185 'TOTAL'
     O          E            THEAD          2
     O                                           49 'CHARGED'
     O                                           66 'PAID'
     O                                           83 'REDUCTIONS'
     O                                          100 'REDUCTIONS'
     O                                          117 'REDUCTIONS'
     O                                          134 'REDUCTIONS'
     O                                          151 'T/M CHARGES'
     O                                          168 'REVIEWER FEE'
     O                                          185 'REVIEWER FEE'
     O          E            SYTOT          2
     O                                           24 'SYSTEM '
     O                       SY(X)               25
     O                                           33 'TOTALS:'
     O                       SYCHRG        JB  +002
     O                       SYPAID        JB  +002
     O                       SYMRA         JB  +002
     O                       SYOTHR        JB  +002
     O                       SYOTH2        JB  +002
     O                       SYNDIS        JB  +002
     O                       SYTTM         JB  +002
     O                       SYCRFE        JB  +002
     O                       SYSMUS        JB  +002
     O*
     O          E            ALTOT          2
     O                       DESCR               33
     O                       ALCHRG        JB  +002
     O                       ALPAID        JB  +002
     O                       ALMRA         JB  +002
     O                       ALOTHR        JB  +002
     O                       ALOTH2        JB  +002
     O                       ALNDIS        JB  +002
     O                       ALTTM         JB  +002
     O                       ALCRFE        JB  +002
     O                       ALSMUS        JB  +002
     O          E            ALTOT          2
     O                                          110 'NUMBER OF ITEMS...'
     O                       SBTOTL        JB   120
     O          E            ALTOT          2
     O                                          110 'NUMBER OF CASES...'
     O                       COUNTS        JB   120
     O          E            ENDTOT         2
     O                                           24 'SYSTEM '
     O                       SY(X)               25
     O                                           33 'TOTALS:'
     O                       LRCHRG        J   +002
     O                       LRPAID        J   +002
     O                       LRMRA         J   +002
     O                       LROTHR        J   +002
     O                       LROTH2        J   +002
     O                       LRNDIS        J   +002
     O                       LRTTM         J   +002
     O                       LRCRFE        J   +002
     O                       LRSMUS        J   +002
     O*
     O          E            TLTOT          2
     O                       DESCR               33
     O                       TLCHRG        J   +002
     O                       TLPAID        J   +002
     O                       TLMRA         J   +002
     O                       TLOTHR        J   +002
     O                       TLOTH2        J   +002
     O                       TLNDIS        J   +002
     O                       TLTTM         J   +002
     O                       TLCRFE        J   +002
     O                       TLSMUS        J   +002
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
