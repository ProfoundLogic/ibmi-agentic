     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4011R - CHAPTER SUMMARY REPORT
     F*
     F*****************************************************************
     F*  REVISIONS
     F*
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     F* 08/26/14 C:298 Project: SY354                                 *
     F*                -Implement Routing Page Changes.               *
     F*
     F* 01/28/10  C:249  Project: SY215                               *
     F*           C:189  Retrieve brand Name                          *
     F*                                                               *
     F*  04/10/00 C:118 PROJECT: WC987
     F*                 USE ALTERNATE NAME FOR COMPANY FROM WMFNP
     F*                 IF AVAILABLE.
     F*
     F*  08/20/98 C:87  SUPPORT WA033/13257
     F*                 CHANGE LOSS RATIO AND FREQUENCY RATIO
     F*                   COMPARRISON TO INCLUDE EQUAL
     F*
     F*  12/18/97 C:77  PROJECT WC577
     F*                 CHANGE OUTPUT FORMATTING.
     F*                -CHANGE EMPLOYER #S TO 9 LONG.
     F*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEML5    IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMCML     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOF)
     FQSYSPRT1  O    F  196        PRINTER OFLIND(*INOA)

     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
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
     D                                     DIM(10)                              RESERVE AMOUNT
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
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXDATE                 1      8  0
     D  XXMMDD                 1      4  0
     D  XXYR                   7      8  0
     D                 DS
     D  X1DATE                 1      6  0
     D  X1MMDD                 1      4  0
     D  X1YR                   5      6  0
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE CLAIMS FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR             3 0
     C*
     C*   KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*                                                                8
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR GROUP FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CHAPTER FILE
     C*
     C     CPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    HCHAP
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQLPCT            7
     C                   PARM                    QQFPCT            7
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C     *LIKE         DEFINE    EMEMP#        HEMP#
     C     *LIKE         DEFINE    EMDIV         HDIV
     C     *LIKE         DEFINE    EMCHAP        HCHAP
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDTE         XXDATE
     C                   MOVEL     XXMMDD        X1MMDD
     C                   MOVEL     XXYR          X1YR
     C                   MOVE      QQLPCT        XXLPCT            7 2
     C                   MOVE      QQFPCT        XXPCT             7 2
     C                   MOVE      QQMODE        XXMODE            1
     C     XXPCT         DIV       100           XXFPCT            5 2
     C                   Z-ADD     0             HCHAP
     C                   Z-ADD     0             HEMP#
     C                   Z-ADD     0             HDIV
     C                   Z-ADD     0             XXECNT            5 0
     C                   Z-ADD     0             L2ACCT            6 0
     C                   Z-ADD     0             L2WALS            5 0
     C                   MOVE      'Y'           XX1ST             1
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  READ EMPLOYER FILE
     C*
     C     EMKEY         CHAIN     PEM01                              60
B01  C     *IN60         DOWEQ     '0'
     C*                                                                8
     C     ELKEY         CHAIN     PEL01                              60
B02  C     *IN60         IFEQ      '0'
     C*
B03  C     XX1ST         IFNE      'Y'
     C     HCHAP         ANDNE     EMCHAP
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   GOTO      DETCLC
E03  C                   END
     C*
B03  C     XX1ST         IFEQ      'Y'
     C                   MOVE      'N'           XX1ST
E03  C                   END
     C*
B03  C     HEMP#         IFNE      0
     C     HEMP#         ANDNE     EMEMP#
     C                   EXSR      SUBL1
     C                   GOTO      DETCLC
E03  C                   END
     C*
B03  C     HEMP#         IFNE      0
     C     HEMP#         ANDEQ     EMEMP#
     C     HDIV          ANDNE     EMDIV
     C                   EXSR      SUBL1
E03  C                   END
     C*
     C     DETCLC        TAG
     C*
     C                   MOVE      EMCHAP        HCHAP
     C                   MOVE      EMEMP#        HEMP#
     C                   MOVE      EMDIV         HDIV
     C                   MOVEL     EMNAME        XXNAME           40
     C                   ADD       1             L2ACCT
B03  C     EMWALS        IFEQ      'Y'
     C                   ADD       1             L2WALS
E03  C                   END
     C*
     C*  READ CLAIM FILE
     C*
     C     CMKEY         CHAIN     PCM01                              60
B03  C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     *ZERO         TOTPMT
     C                   Z-ADD     *ZERO         TOTRES
     C*
     C*   ACCUMULATE PAYMENT AND RESERVE AMOUNTS
     C*
     C                   XFOOT     CMA           TOTPMT            9 2
     C                   XFOOT     CMR           TOTRES            9 2
     C*
     C                   ADD       TOTRES        L1ITOT           11 2
     C                   ADD       TOTPMT        L1ITOT
     C                   ADD       TOTRES        L2OUTS           11 2
     C                   ADD       TOTPMT        L1PTOT           11 2
     C                   ADD       1             L1TOTC            5 0
     C*
     C     CMKEY         READE     PCM01                                  60
E03  C                   END
     C*
E02  C                   END
     C     EMKEY         READE     PEM01                                  60
E01  C                   END
     C**
     C     END           TAG
     C                   EXSR      SUBL1
     C                   EXSR      SUBL2
     C                   EXSR      SUBL3
     C*
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
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
B01  C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
X01  C                   ELSE
     C                   SETON                                        08        BATCH MODE
E01  C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR CHAPTER SUMMARY & PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
B01  C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXTITL           60
     C                   MOVEL     ORTITL        XXTITL
      ***** Beg Del ***** 08/26/14 ************************************
     C************       EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
E01  C                   END
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR SELECTION REPORT & PRINT
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
B01  C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       X1TITL           60
     C                   MOVEL     ORTITL        X1TITL
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          EXCEPT    ROUT1
      ***** End Del ***** 08/26/14 ************************************
E01  C                   END
     C*  GET GROUP NAME
     C     FNKEY         CHAIN     PFN01                              40
B01  C     *IN40         IFEQ      '0'
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C*  CENTER COMPANY NAME IN FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C***** END   ADD **** 04/10/00 ***********************************
     C                   MOVEL     FNNAME        XFNAME           60
X01  C                   ELSE
     C                   MOVE      *BLANK        XFNAME
E01  C                   END
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   MOVE      '1'           *INOF
     C                   MOVE      '1'           *INOA
     C*
     C                   ENDSR
     C*
     C**
      *==================================================================*
      *   Premium Calculations             ** Subroutine **   S U B P R  *
      *==================================================================*
     C*
     CSR   SUBPR         BEGSR
     C**
     C**    CALL PGM TO CALCULATE EARNED PREMIUM YEAR-TO-DATE
     C**                      AND EARNED PREMIUM THIS PERIOD
     C**
     C                   MOVEL     XXCO#         SRCO#             3
     C                   MOVEL     XXFND         SRFND             3
     C                   MOVEL     XXFYR         SRFYR             3
     C*** BEGIN CHG *** 12/18/97 **************************************
     C*********************MOVELHEMP#     SREMP#  5
     C*********************MOVELHDIV      SRDIV   3
     C                   MOVEL     HEMP#         SREMP#            9
     C                   MOVEL     HDIV          SRDIV             5
     C*** END CHG ***** 12/18/97 **************************************
     C                   MOVEL     XXDATE        QQDTE             8
     C                   MOVE      *BLANK        SRAMT            11
     C                   MOVE      *BLANK        SRAMT2           11
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRPRT1'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    QQDTE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    QQTERM
     C                   MOVE      SRAMT         PRMYTD           11 2
     C                   MOVE      SRAMT2        PRMCUR           11 2
     C*
     C**   CALCULATE LOSS RATIO
B01  C     PRMYTD        IFNE      *ZERO
     C     L1ITOT        MULT      100           XXINCU           13 2
     C     XXINCU        DIV(H)    PRMYTD        LOSS              7 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         LOSS
E01  C                   END
     C*
     C**   CALCULATE FREQUENCY RATIO
     C     PRMYTD        DIV(H)    1000          THPREM            9 2
     C*
B01  C     THPREM        IFNE      *ZERO
     C     L1TOTC        DIV(H)    THPREM        FREQ              5 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         FREQ
E01  C                   END
     C*
     C                   ENDSR
     C**
      *==================================================================*
      *   Level 1 Calculations             ** Subroutine **   S U B L 1  *
      *==================================================================*
     C*
     C     SUBL1         BEGSR
     C*
     C*  CALCULATE EARNED PREMIUM TO DATE, LOSS RATIO & FREQ RATIO
     C*
     C                   EXSR      SUBPR
     C                   ADD       PRMYTD        L2PREM           11 2
     C                   Z-ADD     LOSS          L1LOSS            7 2
     C                   Z-ADD     FREQ          L1FREQ            5 2
B01  C     LOSS          IFGE      XXLPCT
     C                   ADD       1             L2OVRL            6 0
E01  C                   END
B01  C     FREQ          IFGE      XXFPCT
     C                   ADD       1             L2OVRF            6 0
E01  C                   END
     C                   ADD       L1TOTC        L2TOTC            6 0
     C                   ADD       L1PTOT        L2PTOT           11 2
     C                   ADD       L1ITOT        L2ITOT           11 2
     C*
B01  C     *INOA         IFEQ      '1'
     C     CPKEY         CHAIN     PCP01                              60
B02  C     *IN60         IFEQ      '0'
     C                   MOVEL     CPDESC        XXCHAP           15
X02  C                   ELSE
     C                   MOVE      *BLANKS       XXCHAP
E02  C                   END
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
E01  C                   END
     C*
     C*** BEGIN CHG *** 12/18/97 **************************************
     C***********L1LOSS    IFGE XXLPCT
     C***********L1FREQ    ORGE XXFPCT
     C*** BEGIN CHG *** 08/20/98 **************************************
     C********** L1LOSS    IFGT XXLPCT
     C********** L1FREQ    ORGT XXFPCT
B01  C     L1LOSS        IFGE      XXLPCT
     C     L1FREQ        ORGE      XXFPCT
     C*** END CHG ***** 08/20/98 **************************************
     C*** END CHG ***** 12/18/97 **************************************
     C                   ADD       1             XXECNT            5 0
     C                   ADD       L1TOTC        XXCHTL            6 0
     C                   ADD       L1PTOT        XXCHPD           11 2
     C                   ADD       L1ITOT        XXCHIN           11 2
     C                   ADD       PRMYTD        XXCHPR           11 2
     C                   EXCEPT    DETL1
E01  C                   END
     C*
     C                   Z-ADD     0             L1TOTC
     C                   Z-ADD     0             L1PTOT
     C                   Z-ADD     0             L1ITOT
     C                   Z-ADD     0             L1LOSS
     C                   Z-ADD     0             L1FREQ
     C*
     C                   ENDSR
     C**
      *==================================================================*
      *   Level 2 Calculations             ** Subroutine **   S U B L 2  *
      *==================================================================*
     C*
     C     SUBL2         BEGSR
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C   OA              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C*
     C*  CALCULATE LOSS RATIO FOR SUMMARY REPORT
     C*
B01  C     L2PREM        IFNE      *ZERO
     C     L2ITOT        MULT      100           XXINCU
     C     XXINCU        DIV(H)    L2PREM        L2LOSS            7 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         L2LOSS
E01  C                   END
     C*
     C*  CALCULATE LOSS RATIO FOR SELECTION REPORT
     C*
B01  C     XXCHPR        IFNE      *ZERO
     C     XXCHIN        MULT      100           XXINCU
     C     XXINCU        DIV(H)    XXCHPR        XXCHLR            7 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         XXCHLR
E01  C                   END
     C*
     C*  CALCULATE FREQUENCY RATIO FOR SUMMARY REPORT
     C*
     C     L2PREM        DIV(H)    1000          THPREM            9 2
     C*
B01  C     THPREM        IFNE      *ZERO
     C     L2TOTC        DIV(H)    THPREM        L2FREQ            5 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         L2FREQ
E01  C                   END
     C*
     C*  CALCULATE FREQUENCY RATIO FOR SELECTION REPORT
     C*
     C     XXCHPR        DIV(H)    1000          THPREM            9 2
     C*
B01  C     THPREM        IFNE      *ZERO
     C     XXCHTL        DIV(H)    THPREM        XXCHFR            5 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         XXCHFR
E01  C                   END
     C*
     C                   EXCEPT    DETL2
     C*       SUMMARY REPORT
     C                   ADD       L2ACCT        L3ACCT            6 0
     C                   ADD       L2PREM        L3PREM           11 2
     C                   ADD       L2TOTC        L3TOTC            6 0
     C                   ADD       L2PTOT        L3PTOT           11 2
     C                   ADD       L2OUTS        L3OUTS           11 2
     C                   ADD       L2ITOT        L3ITOT           11 2
     C                   ADD       L2OVRL        L3OVRL            6 0
     C                   ADD       L2OVRF        L3OVRF            6 0
     C                   ADD       L2WALS        L3WALS            6 0
     C*       SELECTION REPORT
     C                   ADD       XXECNT        XXFCNT            6 0
     C                   ADD       XXCHTL        XXFTOT            6 0
     C                   ADD       XXCHPD        XXFPD            11 2
     C                   ADD       XXCHIN        XXFINC           12 2
     C                   ADD       XXCHPR        XXFPRE           11 2
     C*       SUMMARY REPORT
     C                   Z-ADD     *ZERO         L2ACCT
     C                   Z-ADD     *ZERO         L2PREM
     C                   Z-ADD     *ZERO         L2TOTC
     C                   Z-ADD     *ZERO         L2PTOT
     C                   Z-ADD     *ZERO         L2OUTS
     C                   Z-ADD     *ZERO         L2ITOT
     C                   Z-ADD     *ZERO         L2LOSS
     C                   Z-ADD     *ZERO         L2FREQ
     C                   Z-ADD     *ZERO         L2OVRL
     C                   Z-ADD     *ZERO         L2OVRF
     C                   Z-ADD     *ZERO         L2WALS
     C*       SELECTION REPORT
     C                   Z-ADD     0             XXECNT
     C                   Z-ADD     0             XXCHTL
     C                   Z-ADD     0             XXCHPD
     C                   Z-ADD     0             XXCHIN
     C                   Z-ADD     0             XXCHLR
     C                   Z-ADD     0             XXCHFR
     C                   Z-ADD     0             XXCHPR
     C                   MOVE      '1'           *INOA
     C**
     C                   ENDSR
     C**
      *==================================================================*
      *   Level 3 Calculations             ** Subroutine **   S U B L 3  *
      *==================================================================*
     C*
     C     SUBL3         BEGSR
     C*
     C*    CALCULATE LOSS RATIO FOR SUMMARY REPORT
     C*
B01  C     L3PREM        IFNE      *ZERO
     C     L3ITOT        MULT      100           XXINCU
     C     XXINCU        DIV(H)    L3PREM        L3LOSS            7 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         L3LOSS
E01  C                   END
     C*
     C*    CALCULATE LOSS RATIO FOR SELECTION REPORT
     C*
B01  C     XXFPRE        IFNE      *ZERO
     C     XXFINC        MULT      100           XXINCU
     C     XXINCU        DIV(H)    XXFPRE        XXFLR             7 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         XXFLR
E01  C                   END
     C*
     C*    CALCULATE FREQUENCY RATIO FOR SUMMARY REPORT
     C*
     C     L3PREM        DIV(H)    1000          THPREM            9 2
     C*
B01  C     THPREM        IFNE      *ZERO
     C     L3TOTC        DIV(H)    THPREM        L3FREQ            5 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         L3FREQ
E01  C                   END
     C*
     C*    CALCULATE FREQUENCY RATIO FOR SELECTION REPORT
     C*
     C     XXFPRE        DIV(H)    1000          THPREM            9 2
     C*
B01  C     THPREM        IFNE      *ZERO
     C     XXFTOT        DIV(H)    THPREM        XXFFR             5 2
X01  C                   ELSE
     C                   Z-ADD     *ZERO         XXFFR
E01  C                   END
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C   OA              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTL
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
     O                       ORTITL              80
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
     O*** BEGIN ADD *** 12/18/97 **************************************
     O          E            ROUTE       3  1
     O                                           30 'LOSS RATIO PERCENTAGE  .'
     O                                              ' . . . :'
     O                       XXLPCT        L   +  1
     O          E            ROUTE       1  1
     O                                           30 'FREQUENCY RATIO PERCENTA'
     O                                              'GE . . :'
     O                       XXFPCT        L   +  1
     O*** END ADD ***** 12/18/97 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O                       *YEAR             +  1
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                           55 'AS OF'
     O                       X1DATE        Y     64
     O                                         +  2 'POLICY YEAR'
     O                       XXFYR         Z   +  1
     O          E            HEAD           1
     O                                           54 'LOSS RATIO %'
     O                       XXLPCT        L     63
     O                                           82 'FREQUENCY RATIO %'
     O                       XXFPCT        L     89
     O*** BEGIN CHG *** 12/18/97 **************************************
     O********E**2*************HEAD
     O          E            HEAD        1  2
     O*** END CHG ***** 12/18/97 **************************************
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  1
     O          E            HEAD           1
     O                                         + 18 '# OF'
     O                                         +  4 'EARNED PREM'
     O                                         +  3 'TOTAL'
     O                                         + 11 'TOTAL'
     O                                         +  5 'OUTSTANDING'
     O                                         +  9 'TOTAL'
     O                                         +  7 'LOSS'
     O                                         +  3 'FREQ.'
     O                                         +  1 'OVER'
     O                                         +  4 'OVER'
     O                                         +  4 'WATCH'
     O          E            HEAD           2
     O                                              'CHAPTER'
     O                                         + 10 'ACCTS'
     O                                         +  6 'TO DATE'
     O                                         +  4 'CLAIMS'
     O                                         + 12 'PAID'
     O                                         +  6 'RESERVES'
     O                                         + 10 'INCURRED'
     O                                         +  5 'RATIO'
     O                                         +  2 'RATIO'
     O                                         +  1 'LOSS %'
     O                                         +  2 'FREQ %'
     O                                         +  2 'LIST'
     O          E            DETL2          1
     O                       XXCHAP              15
     O                       L2ACCT        Z     22
     O                       L2PREM        J     38
     O                       L2TOTC        Z     45
     O                       L2PTOT        J     62
     O                       L2OUTS        J     78
     O                       L2ITOT        J     94
     O                       L2LOSS        L    104
     O                       L2FREQ        L    111
     O                       L2OVRL        Z    117
     O                       L2OVRF        Z    125
     O                       L2WALS        Z    132
     O          E            TOTL        1  2
     O                                              'GROUP TOTALS:'
     O                       L3ACCT        Z     22
     O                       L3PREM        J     38
     O                       L3TOTC        Z     45
     O                       L3PTOT        J     62
     O                       L3OUTS        J     78
     O                       L3ITOT        J     94
     O                       L3LOSS        L    104
     O                       L3FREQ        L    111
     O                       L3OVRL        Z    117
     O                       L3OVRF        Z    125
     O                       L3WALS        Z    132
     C*
     OQSYSPRT1  E            ROUT1          1 06
      ***** Beg Del ***** 08/26/14 ************************************
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
      ***** End Del ***** 08/26/14 ************************************
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
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
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
      ***** End Del ***** 08/26/14 ************************************
     O*** BEGIN ADD *** 12/18/97 **************************************
     O          E            ROUT1       3  1
     O                                           30 'LOSS RATIO PERCENTAGE  .'
     O                                              ' . . . :'
     O                       XXLPCT        L   +  1
     O          E            ROUT1       1  1
     O                                           30 'FREQUENCY RATIO PERCENTA'
     O                                              'GE . . :'
     O                       XXFPCT        L   +  1
     O*** END ADD ***** 12/18/97 **************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUT1
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUT1          1 59
     O                       #BRND1
     O          E            ROUT1          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD2          1
     O                       Q1PGM               10
     O                       X1TITL              96
     O                                          126 'PAGE:'
     O*** BEGIN CHG *** 09/25/98 **************************************
     O************************ PAGE  Z  132
     O                       PAGE1         Z    132
     O*** END   CHG *** 09/25/98 **************************************
     O          E            HEAD2          1
     O                                           55 'AS OF'
     O                       X1DATE        Y     64
     O                                         +  2 'POLICY YEAR'
     O                       XXFYR         Z   +  1
     O          E            HEAD2          1
     O                                           54 'LOSS RATIO %'
     O                       XXLPCT        L     63
     O                                           82 'FREQUENCY RATIO %'
     O                       XXFPCT        L     89
     O*** BEGIN CHG *** 12/18/97 **************************************
     O********E**2*****        HEAD2
     O          E            HEAD2       1  2
     O*** END CHG ***** 12/18/97 **************************************
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  1
     O                                           79 'CHAPTER:'
     O*** BEGIN CHG *** 12/18/97 **************************************
     O********************     CPCHAPZ   83
     O                       CPCHAP        L     84
     O*** END CHG ***** 12/18/97 **************************************
     O                       XXCHAP             100
     O          E            HEAD2          1
     O                                         + 58 'TOTAL'
     O                                         + 11 'TOTAL'
     O                                         + 11 'TOTAL'
     O                                         +  7 'LOSS'
     O                                         +  5 'FREQ'
     O          E            HEAD2          2
     O                                         +  1 'EMPLOYER'
     O                                         +  3 'DIV'
     O                                         +  1 'EMPLOYER NAME'
     O                                         + 28 'CLAIMS'
     O                                         + 12 'PAID'
     O                                         +  9 'INCURRED'
     O                                         +  6 'RATIO'
     O                                         +  4 'RATIO'
     O          E            DETL1          1
     O                       HEMP#         Z
     O                       HDIV          Z   +  1
     O                       XXNAME            +  1
     O                       L1TOTC        Z   +  2
     O                       L1PTOT        J   +  2
     O                       L1ITOT        J   +  2
     O                       L1LOSS        L   +  2
     O                       L1FREQ        L   +  2
     O          E            DETL2       1  2
     O                                           15 'EMPLOYER COUNT:'
     O                       XXECNT        Z     21
     O                                           44 'CHAPTER TOTALS:'
     O                       XXCHTL        Z   + 13
     O                       XXCHPD        J   +  2
     O                       XXCHIN        J   +  2
     O                       XXCHLR        L   +  2
     O                       XXCHFR        L   +  2
     O          E            TOTL        1  2
     O                                           15 'EMPLOYER COUNT:'
     O                       XXFCNT        Z     21
     O                                           44 'GROUP TOTALS:'
     O                       XXFTOT        Z   + 13
     O                       XXFPD         J   +  2
     O                       XXFINC        J   +  2
     O                       XXFLR         L   +  2
     O                       XXFFR         L   +  2
