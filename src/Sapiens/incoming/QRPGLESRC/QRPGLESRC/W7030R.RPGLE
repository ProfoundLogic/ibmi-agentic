     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7030R - MONTHLY CHECK REGISTER, ALL ACTIVITY
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 11/23/20 C:062 Project: WC2749                                *
     H*                Change check# heading to REF #                 *
     H*                                                               *
     F* 03/20/17 C:062 Support: 46247(99)                             *
     F*                Don't jump to new page.                        *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*  02/10/10 C:246  PROJECT: C133WIJ                             *
     H*           C:189  ADD JURISDICTION STATE IN QSYSPRT            *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                 IF AVAILABLE.                                 *
     H*                                                               *
     H*  05/05/99 C:03 PROJECT: WC021/#17941                          *
     H*                PRINT ADJ IN FYR IS ANY COLUMN <> 0            *
     H*                                                               *
     H*  09/25/96 C:56 PROJECT: WC566                                 *
     H*                PREVENT CATEGORY HEADINGS FROM BEING CARRIED   *
     H*                OVER TO NEXT FUND DURING DAILY BATCH WITH      *
     H*                MONTHLY FUNCTIONS                              *
     H*                                                               *
     H*  09/17/96 C:56 PROJECT: WC565                                 *
     H*                PREVENT SUBTOTALS FROM BEING CARRIED OVER TO   *
     H*                NEXT FUND DURING DAILY BATCH WITH MONTHLY      *
     H*                FUNCTIONS
     H*                                                               *
     H*  09/30/94 C:32 PROJECT: WC372                                 *
     H*                ADDED RUN DATE TO REPORT HEADING               *
     H*                                                               *
     H*  05/06/93 C:10 PROJECT: WC237                                 *
     H*                CHANGED ARRAYS FROM 9.2 TO 11.2 AND CHANGED    *
     H*                TOTAL FIELDS FOR 9.2 TO 11.2.                  *
     H*****************************************************************
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWXMCL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOF)
     F*

     D PT              S             11  2 DIM(5)                               PMT TOTAL
     D VT              S             11  2 DIM(5)                               VOID TOTAL
     D ST              S             11  2 DIM(5)                               STOP PAY TOTAL
     D ZT              S             11  2 DIM(5)                               REV.STOP PAY
     D MT              S             11  2 DIM(5)                               MANUAL TOTAL
     D SB              S             11  2 DIM(5)                               SUB TOT, PMT
     D YR              S              3  0 DIM(50)                              YEARS
     D YX              S              3  0 DIM(50)                              SORTED YEARS
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D SUMDS           DS                  OCCURS(50)
     D  PS                     1     55  2
     D                                     DIM(5)                               PMT TOTAL
     D  VS                    56    110  2
     D                                     DIM(5)                               VOID TOTAL
     D  SS                   111    165  2
     D                                     DIM(5)                               STOP PAY TOTAL
     D  ZS                   166    220  2
     D                                     DIM(5)                               REV.STOP PAY
     D  MS                   221    275  2
     D                                     DIM(5)                               MANUAL TOTAL
     D  RS                   276    330  2
     D                                     DIM(5)                               RECOVERY TOTAL
     D  AS                   331    385  2
     D                                     DIM(5)                               ADJUST TOTAL
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C                   MOVE      '1'           *IN09                          DEMAND MODE
     C                   MOVE      '0'           *IN08                          BATCH  MODE
     C*
     C*   KEY LIST FOR THE USER PARAMETERS FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    MCFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  ENTRY PARM LIST
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C*
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXRNDT            6 0
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO USER PARAMETER FILE
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C     COKY          CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C*
     C* START READING THE PAYMENT WORK FILE
     C*
     C                   READ      PMC01                                  60
     C                   EXSR      SR1ST
     C     *IN60         DOWEQ     '0'
     C*
     C* LEVEL BREAK ON FUND, GET DESCRIPTION
     C*
     C******BEG ADD ******* 02/10/10 **********************
     C                   MOVE      MCJRST        XXST              2
     C******END ADD ******* 02/10/10 **********************
     C     MCFND         IFNE      SVFND
     C*
     C                   EXSR      L1TOT
     C                   EXSR      L2TOT
     C                   MOVE      *BLANKS       XFNAME
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        XFNAME
     C  N60              Z-ADD     FNFND         XXFND
     C                   MOVE      *ZEROS        FTOT1            11 2
     C                   MOVE      *ZEROS        FTOT2            11 2
     C                   MOVE      *ZEROS        FTOT3            11 2
     C                   MOVE      *ZEROS        FTOT4            11 2
     C                   MOVE      *ZEROS        FTOT5            11 2
     C*
     C                   MOVE      *ZEROS        TTOT1            11 2
     C                   MOVE      *ZEROS        TTOT2            11 2
     C                   MOVE      *ZEROS        TTOT3            11 2
     C                   MOVE      *ZEROS        TTOT4            11 2
     C                   MOVE      *ZEROS        TTOT5            11 2
     C*
     C                   MOVE      *ZEROS        PT
     C                   MOVE      *ZEROS        VT
     C                   MOVE      *ZEROS        ST
     C                   MOVE      *ZEROS        ZT
     C                   MOVE      *ZEROS        MT
     C*
     C                   MOVE      *ZEROS        YR
     C                   DO        50            X                 3 0
     C     X             OCCUR     SUMDS
     C                   MOVE      *ZEROS        PS
     C                   MOVE      *ZEROS        VS
     C                   MOVE      *ZEROS        SS
     C                   MOVE      *ZEROS        ZS
     C                   MOVE      *ZEROS        MS
     C                   MOVE      *ZEROS        RS
     C                   MOVE      *ZEROS        AS
     C                   END
     C*
     C     MCMCSQ        COMP      1                                      50
     C     MCMCSQ        COMP      11                                     51
     C     MCMCSQ        COMP      18                                     54
     C     MCMCSQ        COMP      21                                     52
     C     MCMCSQ        COMP      31                                     53
     C*
     C                   MOVE      '1'           *INOF
     C                   MOVE      '1'           *IN23
     C                   Z-ADD     MCFND         SVFND
     C                   Z-ADD     MCMCSQ        SVMCSQ
     C                   END
     C*
     C* LEVEL BREAK ON TYPE
     C*
     C     MCMCSQ        IFNE      SVMCSQ
     C     MCMCSQ        IFNE      18
     C*
     C                   EXSR      L1TOT
     C                   MOVE      *ZEROS        TTOT1            11 2
     C                   MOVE      *ZEROS        TTOT2            11 2
     C                   MOVE      *ZEROS        TTOT3            11 2
     C                   MOVE      *ZEROS        TTOT4            11 2
     C                   MOVE      *ZEROS        TTOT5            11 2
     C                   MOVE      *ZEROS        PT
     C                   MOVE      *ZEROS        VT
     C                   MOVE      *ZEROS        ST
     C                   MOVE      *ZEROS        ZT
     C                   MOVE      *ZEROS        MT
     C                   END
     C     MCMCSQ        COMP      1                                      50
     C     MCMCSQ        COMP      11                                     51
     C     MCMCSQ        COMP      18                                     54
     C     MCMCSQ        COMP      21                                     52
     C     MCMCSQ        COMP      31                                     53
     C                   MOVE      '1'           *IN23
     C                   Z-ADD     MCMCSQ        SVMCSQ
     C                   END
     C*
     C* DETERMINE OCCURANCE NUMBER OF FUND YEAR
     C*
     C                   Z-ADD     *ZERO         XXZERO            3 0
     C                   Z-ADD     MCFYR         FYR               3 0
     C                   Z-ADD     1             I                 3 0
     C     FYR           LOOKUP    YR(I)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             I
     C     XXZERO        LOOKUP    YR(I)                                  61
     C     *IN61         IFEQ      '0'
     C                   Z-ADD     50            I
     C                   ELSE
     C                   Z-ADD     FYR           YR(I)
     C                   END
     C                   END
     C*
     C     I             OCCUR     SUMDS
     C*
     C* ACCUMULATE TOTALS
     C*
     C                   Z-ADD     MCAMT         XCAMT             9 2
     C     MCTYPE        IFEQ      'V'
     C     MCTYPE        OREQ      'R'
     C     MCTYPE        OREQ      'S'
     C                   Z-SUB     MCAMT         XCAMT             9 2
     C                   END
     C*
     C                   Z-ADD     MCTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         TRDATE            6 0
     C*
     C                   Z-ADD     MCPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         PODATE            6 0
     C*
     C                   ADD       XCAMT         TTOT5
     C                   ADD       XCAMT         FTOT5
     C     MCRETP        COMP      'C'                                    40
     C     MCRETP        COMP      'M'                                    41
     C     MCRETP        COMP      'L'                                    42
     C     MCRETP        COMP      'E'                                    43
     C                   Z-ADD     *ZERO         Z
     C*
     C* PROCESS COMPENSATION RECORDS
     C*
     C     MCRETP        IFEQ      'C'
     C                   ADD       XCAMT         TTOT1
     C                   ADD       XCAMT         FTOT1
     C                   Z-ADD     1             Z                 2 0
     C                   END
     C*
     C* PROCESS MEDICAL RECORDS
     C*
     C     MCRETP        IFEQ      'M'
     C                   ADD       XCAMT         TTOT2
     C                   ADD       XCAMT         FTOT2
     C                   Z-ADD     2             Z                 2 0
     C                   END
     C*
     C* PROCESS LEGAL RECORDS
     C*
     C     MCRETP        IFEQ      'L'
     C                   ADD       XCAMT         TTOT3
     C                   ADD       XCAMT         FTOT3
     C                   Z-ADD     3             Z                 2 0
     C                   END
     C*
     C* PROCESS EXPENSE RECORDS
     C*
     C     MCRETP        IFEQ      'E'
     C                   ADD       XCAMT         TTOT4
     C                   ADD       XCAMT         FTOT4
     C                   Z-ADD     4             Z                 2 0
     C                   END
     C*
     C* LOAD SUMMARY ARRAYS
     C*
     C     MCTYPE        IFEQ      'P'
     C     Z             IFNE      *ZERO
     C                   ADD       XCAMT         PT(Z)
     C                   ADD       XCAMT         PS(Z)
     C                   END
     C                   ADD       XCAMT         PT(5)
     C                   ADD       XCAMT         PS(5)
     C                   END
     C     MCTYPE        IFEQ      'V'
     C     Z             IFNE      *ZERO
     C                   ADD       XCAMT         VT(Z)
     C                   ADD       XCAMT         VS(Z)
     C                   END
     C                   ADD       XCAMT         VT(5)
     C                   ADD       XCAMT         VS(5)
     C                   END
     C     MCTYPE        IFEQ      'S'
     C     Z             IFNE      *ZERO
     C                   ADD       XCAMT         ST(Z)
     C                   ADD       XCAMT         SS(Z)
     C                   END
     C                   ADD       XCAMT         ST(5)
     C                   ADD       XCAMT         SS(5)
     C                   END
     C     MCTYPE        IFEQ      'Z'
     C     Z             IFNE      *ZERO
     C                   ADD       XCAMT         ZT(Z)
     C                   ADD       XCAMT         ZS(Z)
     C                   END
     C                   ADD       XCAMT         ZT(5)
     C                   ADD       XCAMT         ZS(5)
     C                   END
     C     MCTYPE        IFEQ      'M'
     C     Z             IFNE      *ZERO
     C                   ADD       XCAMT         MT(Z)
     C                   ADD       XCAMT         MS(Z)
     C                   END
     C                   ADD       XCAMT         MT(5)
     C                   ADD       XCAMT         MS(5)
     C                   END
     C     MCTYPE        IFEQ      'R'
     C     Z             IFNE      *ZERO
     C                   ADD       XCAMT         RS(Z)
     C                   END
     C                   ADD       XCAMT         RS(5)
     C                   END
     C     MCTYPE        IFEQ      'A'
     C     Z             IFNE      *ZERO
     C                   ADD       XCAMT         AS(Z)
     C                   END
     C                   ADD       XCAMT         AS(5)
     C                   END
     C*
     C* OUTPUT DETAIL
     C*
     C   OF              EXCEPT    HEAD
     C   23              EXCEPT    AHEAD
     C                   MOVE      '0'           *IN23
     C                   EXCEPT    DETL
     C*
     C* GET THE NEXT PAYMENT RECORD
     C*
     C                   READ      PMC01                                  60
     C                   END
     C*
     C* PROCESS TOTALS
     C*
     C                   EXSR      L1TOT
     C                   EXSR      L2TOT
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C     SR1ST         BEGSR
     C*
     C*   GET THE FUND NAME
     C*
     C                   MOVE      *BLANKS       XFNAME           60
     C     FNKY          CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        XFNAME
     C                   Z-ADD     FNFND         XXFND             3 0
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
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
     C*
     C*  INITIALIZE FIELDS
     C*
     C                   DO        50            X                 3 0
     C     X             OCCUR     SUMDS
     C                   MOVE      *ZEROS        PS
     C                   MOVE      *ZEROS        VS
     C                   MOVE      *ZEROS        SS
     C                   MOVE      *ZEROS        ZS
     C                   MOVE      *ZEROS        MS
     C                   MOVE      *ZEROS        RS
     C                   MOVE      *ZEROS        AS
     C                   END
     C*
     C                   MOVE      *ZEROS        PT
     C                   MOVE      *ZEROS        VT
     C                   MOVE      *ZEROS        ST
     C                   MOVE      *ZEROS        ZT
     C                   MOVE      *ZEROS        MT
     C*
     C                   Z-ADD     MCFND         SVFND             3 0
     C                   Z-ADD     MCMCSQ        SVMCSQ            5 0
     C     MCMCSQ        COMP      1                                      50
     C     MCMCSQ        COMP      11                                     51
     C     MCMCSQ        COMP      18                                     54
     C     MCMCSQ        COMP      21                                     52
     C     MCMCSQ        COMP      31                                     53
     C*** Begin Chg *** 03/20/17 *************************************
     C**********         MOVE      '1'           *INOF
     C**********         MOVE      '1'           *IN23
     C                   EXCEPT    HEAD
     C                   EXCEPT    AHEAD
     C*** End   Chg *** 03/20/17 *************************************
     C*
     C                   ENDSR
      *==================================================================*
      *   PRINT ACTIVITY TOTALS AND TYPE SUMMARY                         *
      *==================================================================*
     C     L1TOT         BEGSR
     C*
     C     *IN50         IFEQ      '1'
     C     *IN51         OREQ      '1'
     C     *IN54         OREQ      '1'
     C                   EXCEPT    ATOT
     C     PT(5)         IFNE      *ZERO
     C                   EXCEPT    PMT
     C                   END
     C     VT(5)         IFNE      *ZERO
     C                   EXCEPT    VOID
     C                   END
     C     ST(5)         IFNE      *ZERO
     C                   EXCEPT    STOP
     C                   END
     C     ZT(5)         IFNE      *ZERO
     C                   EXCEPT    REV
     C                   END
     C     MT(5)         IFNE      *ZERO
     C                   EXCEPT    MAN
     C                   END
     C                   END
     C                   EXCEPT    TTOT
     C*
     C                   ENDSR
      *==================================================================*
      *   PRINT FUND TOTALS AND FUND YEAR SUMMARY                        *
      *==================================================================*
     C     L2TOT         BEGSR
     C*
     C                   EXCEPT    FTOT
     C                   EXCEPT    HEAD
     C                   EXCEPT    FSUM
     C*
     C* CHECK EACH OCCURANCE FOR OUTPUT
     C*
     C                   Z-ADD     1             J                 2 0
     C                   MOVE      YR            YX
     C                   SORTA     YX
     C     J             DOUGT     50
     C     YX(J)         IFNE      *ZERO
     C                   Z-ADD     1             I
     C                   Z-ADD     YX(J)         XXLOK             3 0
     C     XXLOK         LOOKUP    YR(I)                                  60
     C                   MOVE      *ZEROS        SB
     C                   Z-ADD     YR(I)         X4YEAR            4 0
     C                   EXCEPT    FYEAR
     C                   SETOFF                                       60
     C     I             OCCUR     SUMDS
     C     PS(5)         IFNE      *ZERO
     C                   ADD       PS            SB
     C                   EXCEPT    PMTS
     C                   SETON                                        60
     C                   END
     C     VS(5)         IFNE      *ZERO
     C                   ADD       VS            SB
     C                   EXCEPT    VOIDS
     C                   SETON                                        60
     C                   END
     C     SS(5)         IFNE      *ZERO
     C                   ADD       SS            SB
     C                   EXCEPT    STOPS
     C                   SETON                                        60
     C                   END
     C     ZS(5)         IFNE      *ZERO
     C                   ADD       ZS            SB
     C                   EXCEPT    REVS
     C                   SETON                                        60
     C                   END
     C     MS(5)         IFNE      *ZERO
     C                   ADD       MS            SB
     C                   EXCEPT    MANS
     C                   SETON                                        60
     C                   END
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    PSUB
     C                   END
     C*
     C     RS(5)         IFNE      *ZERO
     C                   EXCEPT    REC
     C                   ADD       RS            SB
     C                   SETON                                        60
     C                   END
     C**** BGN CHG 05/05/99 ***************************************
     C********** AS,5      IFNE *ZERO
     C     AS(1)         IFNE      *ZERO
     C     AS(2)         ORNE      *ZERO
     C     AS(3)         ORNE      *ZERO
     C     AS(4)         ORNE      *ZERO
     C**** BGN CHG 05/05/99 ***************************************
     C                   EXCEPT    ADJ
     C                   ADD       AS            SB
     C                   SETON                                        60
     C                   END
     C     *IN60         IFEQ      '1'
     C                   EXCEPT    YTOT
     C                   SETOFF                                       60
     C                   END
     C                   END
     C*
     C                   ADD       1             J
     C   OF              EXCEPT    HEAD
     C                   END
     C*
     C                   EXCEPT    FTOT
     C                   ENDSR
      *==================================================================*
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
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             115
     O                       DSDATE        Y    196
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             126
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       XFNAME            +  1
     O                                           98 'RUN DATE:'
     O                       XXRNDT        Y    108
     O          E            HEAD           1
     O***BEG CHG 02/10/10*********************************
     O**                                 55 'TRANSACTION'
     O**                                 72 'TRANS      POST'
     O                                           61 'TRANSACTION'
     O                                           79 'TRANS      POST'
     O***END CHG 02/10/10*********************************
     O          E            HEAD           2
     O                                            3 'YR'
     O                                           11 'CASE #'
     O***BEG CHG 02/10/10*********************************
     O**                                 25 'PAYABLE TO:'
     O**                                 55 'CODE  TYPE'
     O**                                 72 'DATE      DATE'
     O**                                 82 'CHECK#'
     O**                                101 'COMPENSATION'
     O**                                123 'M E D I C A L'
     O**                                144 'L E G A L'
     O**                                168 'E X P E N S E'
     O**                                188 'T O T A L'
     O                                           21 'STATE'
     O                                           38 'PAYABLE TO:'
     O                                           62 'CODE  TYPE'
     O                                           79 'DATE      DATE'
     O*** Begin Chg *** 11/23/20 *******************************************
     O**********                                 89 'CHECK#'
     O                                           89 ' REF #'
     O*** End   Chg *** 11/23/20 *******************************************
     O                                          108 'COMPENSATION'
     O                                          130 'M E D I C A L'
     O                                          151 'L E G A L'
     O                                          175 'E X P E N S E'
     O                                          195 'T O T A L'
     O***END CHG 02/10/10*********************************
     O          E            AHEAD       1  2
     O                     50                    35 '*** PREVIOUS ACTIVITY **'
     O                     50                    36 '*'
     O***END CHG 02/10/10*********************************
     O***                    51          35 '*** CURRENT ACTIVITY ***'
     O***                    52          35 '*** RECOVERIES ***      '
     O***                    53          35 '*** ADJUSTMENTS ***'
     O***                    54          35 '***   MANUAL    ***'
     O                     51                    51 '*** CURRENT ACTIVITY ***'
     O                     52                    51 '*** RECOVERIES ***      '
     O                     53                    51 '*** ADJUSTMENTS ***'
     O                     54                    51 '***   MANUAL    ***'
     O***END CHG 02/10/10*********************************
     O          E            DETL           1
     O                       MCFYR         Z      3
     O                       MCCASE        Z     11
     O***** BEG CHG 02/10/10*******************************
     O**                       MCDESC    43
     O**                       MCTRCD    47
     O**                       MCTYPE    53
     O**                       TRDATEY   64
     O**                       PODATEY   74
     O**                       MCCHK#Z   82
     O**                     40XCAMT B  103
     O**                     41XCAMT B  125
     O**                     42XCAMT B  147
     O**                     43XCAMT B  169
     O**                       XCAMT B  191
     O                       XXST                20
     O                       MCDESC              53
     O                       MCTRCD              55
     O                       MCTYPE              62
     O                       TRDATE        Y     69
     O                       PODATE        Y     82
     O                       MCCHK#        Z     89
     O                     40XCAMT         B    108
     O                     41XCAMT         B    130
     O                     42XCAMT         B    155
     O                     43XCAMT         B    176
     O                       XCAMT         B    196
     O***** END CHG 02/10/10*******************************
     O          E            ATOT        1  2
     O                     50                    35 '*** TOTAL PREVIOUS ACTIV'
     O                     50                    42 'ITY ***'
     O                     51                    35 '*** TOTAL CURRENT ACTIVI'
     O                     51                    41 'TY ***'
     O                     54                    35 '*** TOTAL CURRENT ACTIVI'
     O                     54                    41 'TY ***'
     O          E            PMT            1
     O***** BEG CHG 02/10/10*******************************
     O**                                 81 'P PAYMENT'
     O                                           85 'P PAYMENT'
     O**                       PT,1  B  103
     O                       PT(1)         B    108
     O**                       PT,2  B  125
     O                       PT(2)         B    130
     O**                       PT,3  B  147
     O                       PT(3)         B    152
     O**                       PT,4  B  169
     O                       PT(4)         B    176
     O**                       PT,5  B  191
     O                       PT(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            VOID           1
     O***** BEG CHG 02/10/10*******************************
     O**                                 81 'V VOID   '
     O**                       VT,1  B  103
     O**                       VT,2  B  125
     O**                       VT,3  B  147
     O**                       VT,4  B  169
     O**                       VT,5  B  191
     O                                           85 'V VOID   '
     O                       VT(1)         B    108
     O                       VT(2)         B    130
     O                       VT(3)         B    152
     O                       VT(4)         B    176
     O                       VT(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            STOP           1
     O***** BEG CHG 02/10/10*******************************
     O**                                 82 'S STOP PAY'
     O**                       ST,1  B  103
     O**                       ST,2  B  125
     O**                       ST,3  B  147
     O**                       ST,4  B  169
     O**                       ST,5  B  191
     O                                           86 'S STOP PAY'
     O                       ST(1)         B    108
     O                       ST(2)         B    130
     O                       ST(3)         B    152
     O                       ST(4)         B    176
     O                       ST(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            REV            1
     O***** BEG CHG 02/10/10*******************************
     O**                                 86 'Z REV STOP PAY'
     O**                       ZT,1  B  103
     O**                       ZT,2  B  125
     O**                       ZT,3  B  147
     O**                       ZT,4  B  169
     O**                       ZT,5  B  191
     O                                           90 'Z REV STOP PAY'
     O                       ZT(1)         B    108
     O                       ZT(2)         B    130
     O                       ZT(3)         B    152
     O                       ZT(4)         B    176
     O                       ZT(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            MAN            1
     O***** BEG CHG 02/10/10*******************************
     O**                                 84 'M MANUAL PAY'
     O**                       MT,1  B  103
     O**                       MT,2  B  125
     O**                       MT,3  B  147
     O**                       MT,4  B  169
     O**                       MT,5  B  191
     O                                           88 'M MANUAL PAY'
     O                       MT(1)         B    108
     O                       MT(2)         B    130
     O                       MT(3)         B    152
     O                       MT(4)         B    176
     O                       MT(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            TTOT           2
     O***** END CHG 02/10/10*******************************
     O**                                 69 '*** TOTAL ***'
     O**                       TTOT1 B  103
     O**                       TTOT2 B  125
     O**                       TTOT3 B  147
     O**                       TTOT4 B  169
     O**                       TTOT5 B  191
     O                                           69 '*** TOTAL ***'
     O                       TTOT1         B    108
     O                       TTOT2         B    130
     O                       TTOT3         B    152
     O                       TTOT4         B    176
     O                       TTOT5         B    196
     O***** END CHG 02/10/10*******************************
     O          E            FSUM        1  1
     O                                           36 '*** SUMMARY BY POLICY'
     O                                         +  1 'YEAR ***'
     O          E            FYEAR       1  0
     O                                           58 '***      POLICY YEAR ***'
     O                       X4YEAR        Z     42
     O          E            PMTS           1
     O***** BEG CHG 02/10/10*******************************
     O**                                 81 'P PAYMENT'
     O**                       PS,1  B  103
     O**                       PS,2  B  125
     O**                       PS,3  B  147
     O**                       PS,4  B  169
     O**                       PS,5  B  191
     O                                           85 'P PAYMENT'
     O                       PS(1)         B    108
     O                       PS(2)         B    130
     O                       PS(3)         B    152
     O                       PS(4)         B    176
     O                       PS(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            VOIDS          1
     O**                                 81 'V VOID   '
     O**                       VS,1  B  103
     O**                       VS,2  B  125
     O**                       VS,3  B  147
     O**                       VS,4  B  169
     O**                       VS,5  B  191
     O                                           85 'V VOID   '
     O                       VS(1)         B    108
     O                       VS(2)         B    130
     O                       VS(3)         B    152
     O                       VS(4)         B    176
     O                       VS(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            STOPS          1
     O***** BEG CHG 02/10/10*******************************
     O**                                 82 'S STOP PAY'
     O**                       SS,1  B  103
     O**                       SS,2  B  125
     O**                       SS,3  B  147
     O**                       SS,4  B  169
     O**                       SS,5  B  191
     O                                           86 'S STOP PAY'
     O                       SS(1)         B    108
     O                       SS(2)         B    130
     O                       SS(3)         B    152
     O                       SS(4)         B    176
     O                       SS(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            REVS           1
     O***** BEG CHG 02/10/10*******************************
     O**                                 86 'Z REV STOP PAY'
     O**                       ZS,1  B  103
     O**                       ZS,2  B  125
     O**                       ZS,3  B  147
     O**                       ZS,4  B  169
     O**                       ZS,5  B  191
     O                                           90 'Z REV STOP PAY'
     O                       ZS(1)         B    108
     O                       ZS(2)         B    130
     O                       ZS(3)         B    152
     O                       ZS(4)         B    176
     O                       ZS(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            MANS           1
     O***** END CHG 02/10/10*******************************
     O**                                 84 'M MANUAL PAY'
     O**                       MS,1  B  103
     O**                       MS,2  B  125
     O**                       MS,3  B  147
     O**                       MS,4  B  169
     O**                       MS,5  B  191
     O                                           88 'M MANUAL PAY'
     O                       MS(1)         B    108
     O                       MS(2)         B    130
     O                       MS(3)         B    152
     O                       MS(4)         B    176
     O                       MS(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            PSUB           2
     O***** BEG CHG 02/10/10*******************************
     O                                           58 '*** SUBTOTAL PAYMENTS **'
     O                                           59 '*'
     O**                       SB,1  B  103
     O**                       SB,2  B  125
     O**                       SB,3  B  147
     O**                       SB,4  B  169
     O**                       SB,5  B  191
     O                       SB(1)         B    108
     O                       SB(2)         B    130
     O                       SB(3)         B    152
     O                       SB(4)         B    176
     O                       SB(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            ADJ            1
     O                                           75 'A ADJUSTMENTS'
     O***** BEG CHG 02/10/10*******************************
     O**                       AS,1  B  103
     O**                       AS,2  B  125
     O**                       AS,3  B  147
     O**                       AS,4  B  169
     O**                       AS,5  B  191
     O                       AS(1)         B    108
     O                       AS(2)         B    130
     O                       AS(3)         B    152
     O                       AS(4)         B    176
     O                       AS(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            REC            1
     O                                           75 'R RECOVERIES '
     O***** BEG CHG 02/10/10*******************************
     O**                       RS,1  B  103
     O**                       RS,2  B  125
     O**                       RS,3  B  147
     O**                       RS,4  B  169
     O**                       RS,5  B  191
     O                       RS(1)         B    108
     O                       RS(2)         B    130
     O                       RS(3)         B    152
     O                       RS(4)         B    176
     O                       RS(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            YTOT           3
     O                                           52 '***      TOTAL ***'
     O                       X4YEAR        Z     42
     O***** BEG CHG 02/10/10*******************************
     O**                       SB,1  B  103
     O**                       SB,2  B  125
     O**                       SB,3  B  147
     O**                       SB,4  B  169
     O**                       SB,5  B  191
     O                       SB(1)         B    108
     O                       SB(2)         B    130
     O                       SB(3)         B    152
     O                       SB(4)         B    176
     O                       SB(5)         B    196
     O***** END CHG 02/10/10*******************************
     O          E            FTOT           1
     O                                           59 '*** TOTAL ***'
     O***** BEG CHG 02/10/10*******************************
     O**                       FTOT1 B  103
     O**                       FTOT2 B  125
     O**                       FTOT3 B  147
     O**                       FTOT4 B  169
     O**                       FTOT5 B  191
     O                       FTOT1         B    108
     O                       FTOT2         B    130
     O                       FTOT3         B    152
     O                       FTOT4         B    176
     O                       FTOT5         B    196
     O***** END CHG 02/10/10*******************************
