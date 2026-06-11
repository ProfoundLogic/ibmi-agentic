     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE N7735R - CASH RECEIPTS/ADJ JOURNAL - REPORT PROGRAM
     H***************************************************************
     H** REVISIONS:
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
     H*  01/27/00 C:62  Project: Y2K
     H*                 CHANGE DATES TO MDYY.
     H*
     H*  01/04/99 C:31  Project: MC152
     H*                 1. Change size of invoice number
     H***************************************************************
     H*
     FSMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FNMARL9    IF   E           K DISK
     FFMSML     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FQSYSPRT   O    F  127        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOA)
     FQSYSPRT1  O    F  127        PRINTER INFDS(PRTDS1)
     F                                     OFLIND(*INOF)
     F*

     D*
     D* Define A/R Type Summary Arrays
     D*
     D XCOD            S              2    DIM(999)                             A/R Code
     D XCNT            S             11    DIM(999)                             Total Amount
     D COMB            S             13    DIM(999)                             Combination
     D*
     D* Define arrays for A/R Code selection criteria
     D COD             S              2    DIM(30) ASCEND                       A/R code
     D DES             S             30    DIM(30)                              A/R description
     D CDE             S              2    DIM(30)                              A/R output code
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XC#                    1     22
     D  XXSUB#                 1      5  0
     D  XXHYP1                 6      6
     D  XXFND                  7      9  0
     D  XXHYP2                10     10
     D  XXYR                  11     12  0
     D  XXHYP3                13     13
     D  XXCASE                14     20  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PRTDS1          DS
     D  FIL21                  1    200
     D  FIL22                201    366
     D  XXLIN2               367    368B 0
     D  XXPAG2               369    372B 0
     D*
     D* Combination Array
     D*
     D                 DS
     D  XCOMB                  1     13
     D  XCODE                  1      2
     D  XYCNT                  3     13  2
     D*
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C*
     C*  OUTPUT ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE SUBSCRIBER MASTER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C*
     C*  A/R TYPE CODE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   PARAMETER LIST FOR CALLED PROGRAM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQRTYP            1
     C                   PARM                    QQCODE           60
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFDT         X1FDT             8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1FDT     SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFDT   60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQTDT         X1TDT             8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1TDT     SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTDT   60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C                   Z-ADD     0             XPADET           10 2
     C                   Z-ADD     0             XAADET           10 2
     C                   Z-ADD     0             XPAGRP           10 2
     C                   Z-ADD     0             XPCGRP            5 0
     C                   Z-ADD     0             XPASUB           10 2
     C                   Z-ADD     0             XPCSUB            5 0
     C                   Z-ADD     0             XPAPRO           10 2
     C                   Z-ADD     0             XPCPRO            5 0
     C                   Z-ADD     0             XPAFIN           10 2
     C                   Z-ADD     0             XPCFIN            5 0
     C                   Z-ADD     0             XAAGRP           10 2
     C                   Z-ADD     0             XACGRP            5 0
     C                   Z-ADD     0             XAASUB           10 2
     C                   Z-ADD     0             XACSUB            5 0
     C                   Z-ADD     0             XAAPRO           10 2
     C                   Z-ADD     0             XACPRO            5 0
     C                   Z-ADD     0             XAAFIN           10 2
     C                   Z-ADD     0             XACFIN            5 0
     C                   MOVEA     QQCODE        COD
     C*
     C* Initialize A/R Summary Fields
     C*
     C                   MOVEA     *BLANKS       XCNT
     C                   MOVEA     *BLANKS       XCOD
     C                   MOVEA     *BLANKS       COMB
     C                   Z-ADD     0             XXCNT            11 2
     C*
     C*   LOAD THE OUTPUT CONSTANTS FROM THE PARMS
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C                   MOVE      '-'           XXHYP2            1
     C                   MOVE      '-'           XXHYP3            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
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
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*  Load the A/R description array so that A/R selection codes
     C*  are printed on the routing page
     C*
     C*
     C*   SORT THE CODE ARRAY
     C*
     C                   SORTA     COD
     C*
     C*   FILL THE CODE DESCRIPTION ARRAY CORRESPONDING TO CODE ARRAY
     C*
     C                   MOVE      *BLANKS       CDE
     C                   MOVE      *BLANKS       DES
     C                   Z-ADD     1             Y
     C                   Z-ADD     1             Z                 2 0
     C     Y             DOUEQ     31
     C*
     C*   INDICATORS 20 TO 49 ARE USED FOR OUTPUT PURPOSES.
     C*
     C     COD(Y)        IFNE      *BLANKS
     C     Z             ADD       19            Z1                2 0
     C                   MOVE      '1'           *IN(Z1)
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     COD(Y)        XXKEY
     C     TBKEY         CHAIN     PAC01                              60
     C     *IN60         IFEQ      '0'
     C     ACSTAT        ANDNE     'I'
     C                   MOVEL     COD(Y)        CDE(Z)
     C                   MOVEL     ACDESC        DES(Z)
     C                   ADD       1             Z
     C                   END
     C                   END
     C*
     C                   ADD       1             Y
     C                   END
     C*
     C*   GET ROUTING INFORMATION FOR REPORT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        XXTITL           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********   10              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*   GET ROUTING INFORMATION FOR REPORT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      ORTITL        YYTITL           60
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********   11              EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C                   END                                                    END IFEQ '0'
     C*
     C*  READ A/R OPEN QUERY FILE (ALL 'I' RECORDS IN DATE RANGE)
     C*
     C     *LOVAL        SETLL     PAR01
     C                   READ      PAR01                                  50
     C*
     C     *IN50         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C   10              EXCEPT    ERRMSG
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C   11              EXCEPT    ERRMS1
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN50         IFEQ      '0'
     C*
     C                   Z-ADD     ARCO#         XXCO#             3 0
     C                   Z-ADD     ARSUB#        XXSUB#            5 0
     C                   Z-ADD     ARFND         XXFND             3 0
     C                   Z-ADD     ARFYR         XXFYR             3 0
     C                   Z-ADD     ARFYR         XXYR              2 0
     C                   Z-ADD     ARCASE        XXCASE            7 0
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADDARINV#    XXINV#  50
     C                   Z-ADD     ARINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C                   MOVE      ARSYS         XXSYS             1
     C                   MOVEL     ARTRCD        XXTRCD            2
     C*
     C*  GET COMPANY NAME/GROUP NAME
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XXCONM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXCONM
     C                   END
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C*
     C*  CHAIN TO SUBSCRIBER MASTER FILE - GET SUBSC NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBM
     C                   END
     C*
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   END                                                    IF *IN50 = 0
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C* Check for code in the array.
     C*
     C                   Z-ADD     1             Y                 2 0
     C     ARTRCD        LOOKUP    COD(Y)                                 65
     C     *IN65         IFEQ      '1'
     C     ARTRCD        ANDNE     *BLANKS
     C*
     C*
     C     ARSYS         IFNE      XXSYS
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   EXSR      SRSUB                                        SUB  TOTALS
     C                   EXSR      SRPRO                                        PROD TOTALS
     C                   ELSE
     C     ARSUB#        IFNE      XXSUB#
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   EXSR      SRSUB                                        SUB  TOTALS
     C                   ELSE
     C     ARFND         IFNE      XXFND
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP TOTALS
     C                   ELSE
     C     ARCASE        IFNE      XXCASE
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   ELSE
     C     ARINV#        IFNE      XXINV#
     C                   EXSR      SRINV                                        INV CHG
     C                   END                                                    ENDNE XXINV
     C                   END                                                    ENDNE XXCASE
     C                   END                                                    ENDNE XXFND
     C                   END                                                    ENDNE XXSUB#
     C                   END                                                    ENDNE XXSYS
     C*
     C*  MOVE FIELDS TO REPORT FIELDS
     C*
     C*** BEGIN CHG *** 01/04/99 **************************************
     C********             Z-ADDARINV#    XXINV#  50
     C                   Z-ADD     ARINV#        XXINV#            9 0
     C*** END CHG *** 01/04/99 ****************************************
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXDATE  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C*
     C                   MOVEL     ARDESC        XXDESC           30
     C                   Z-ADD     ARCK#         XXCHK#            7 0          ADD 12/17/93
     C     ARTRTP        IFEQ      'A'
     C                   Z-ADD     ARAMT         XAADET
     C                   ADD       ARAMT         XAAGRP
     C                   ADD       1             XACGRP
     C                   ELSE
     C                   Z-ADD     ARAMT         XPADET
     C                   ADD       ARAMT         XPAGRP
     C                   ADD       1             XPCGRP
     C                   END
     C*
     C*  PRINT DETAIL RECORD
     C*
     C     *INOA         IFEQ      '1'
     C   10              EXCEPT    HEAD
     C   10              EXCEPT    HEAD2
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C   10              EXCEPT    DETAIL
     C*
     C                   Z-ADD     *ZEROS        XPADET
     C                   Z-ADD     *ZEROS        XAADET
     C*
     C                   EXSR      ARSUM
     C*
     C                   END                                                    END DOWEQ 65
     C*                                                                AME
     C**********************************
     C*  READ NEXT RECORD              *
     C**********************************
     C*
     C                   READ      PAR01                                  50
     C*
     C                   MOVEL     ARTRCD        XXTRCD            2
     C*
     C                   END                                                    END DOWEQ 50
     C*                                                                AME
     C**********************************
     C*     END THE PROGRAM            *
     C**********************************
     C                   MOVE      '1'           *INLR
     C*
     C                   EXSR      SRINV                                        INV CHG
     C                   EXSR      SRCASE                                       CASE CHG
     C                   EXSR      SRGRP                                        GRP CHG
     C                   EXSR      SRSUB                                        SUB CHG
     C                   EXSR      SRPRO                                        PRO CHG
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    GRTOT1
     C*
     C                   EXSR      ARDETL
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
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
     C*****************************************************************--*
     C*  PERFORM INVOICE BREAK CALCULATIONS
     C*****************************************************************--*
     C*
     C     SRINV         BEGSR
     C*
     C                   Z-ADD     ARINV#        XXINV#
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************--*
     C*  PERFORM CASE TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRCASE        BEGSR
     C*
     C                   Z-ADD     ARFYR         XXFYR
     C                   Z-ADD     ARFYR         XXYR
     C                   Z-ADD     ARCASE        XXCASE
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM GROUP TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRGRP         BEGSR
     C*
     C                   ADD       XPAGRP        XPASUB
     C                   ADD       XPCGRP        XPCSUB
     C                   ADD       XAAGRP        XAASUB
     C                   ADD       XACGRP        XACSUB
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    GRPTL1
     C*
     C                   Z-ADD     0             XPAGRP
     C                   Z-ADD     0             XPCGRP
     C                   Z-ADD     0             XAAGRP
     C                   Z-ADD     0             XACGRP
     C*
     C                   Z-ADD     ARFND         XXFND
     C*
     C     *INLR         IFEQ      '0'
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XXFNNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXFNNM
     C                   END
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM SUBSCRIBER TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRSUB         BEGSR
     C*
     C                   ADD       XPASUB        XPAPRO
     C                   ADD       XPCSUB        XPCPRO
     C                   ADD       XAASUB        XAAPRO
     C                   ADD       XACSUB        XACPRO
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   11              EXCEPT    SBTOT1
     C*
     C                   Z-ADD     0             XPASUB
     C                   Z-ADD     0             XPCSUB
     C                   Z-ADD     0             XAASUB
     C                   Z-ADD     0             XACSUB
     C*
     C                   Z-ADD     ARSUB#        XXSUB#
     C*
     C     *INLR         IFEQ      '0'
     C*
     C*  CHAIN TO SUBSCRIBER MASTER FILE - GET SUBSCRIBER NAME
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     SMNAME        XXSUBM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XXSUBM
     C                   END
     C*
     C                   END                                                    END *INLR
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*  PERFORM PRODUCT TOTAL CALCULATIONS & OUTPUT
     C*****************************************************************--*
     C*
     C     SRPRO         BEGSR
     C*
     C                   ADD       XPAPRO        XPAFIN
     C                   ADD       XPCPRO        XPCFIN
     C                   ADD       XAAPRO        XAAFIN
     C                   ADD       XACPRO        XACFIN
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    HEAD12
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C   10              EXCEPT    PRTOT
     C   11              EXCEPT    PRTOT1
     C*
     C                   Z-ADD     0             XPAPRO
     C                   Z-ADD     0             XPCPRO
     C                   Z-ADD     0             XAAPRO
     C                   Z-ADD     0             XACPRO
     C*
     C                   MOVE      ARSYS         XXSYS
     C*
     C                   ENDSR
     C*****************************************************************--*
     C* LOAD A/R TYPES INTO ARRAYS                                    *
     C*****************************************************************--*
     C*
     CSR   ARSUM         BEGSR
     C*
     C*
     C*  CHECK TO SEE IF CODE IS ALREADY IN ARRAY
     C*
     C                   Z-ADD     1             V                 5 0
     C                   MOVE      '1'           *IN33
     C*
     C     ARTRCD        LOOKUP    XCOD(V)                                33
     C*
     C     *IN33         IFEQ      '0'                                          NEW CODE-A  DD
     C                   Z-ADD     1             V
     C*
     C*  FIND NEXT AVAILABLE POSITION
     C*
     C     XCOD(V)       DOWNE     *BLANKS
     C                   ADD       1             V
     C                   END
     C*
     C*  ADD FIELDS TO THE ARRAY
     C*
     C                   Z-ADD     ARAMT         XXCNT
     C                   MOVE      *BLANKS       X1CNT
     C*
     C                   MOVEA     ARTRCD        XCOD(V)
     C                   MOVE      XXCNT         X1CNT            11
     C                   MOVEA     X1CNT         XCNT(V)
     C*
     C*   IF CODE IS IN ARRAY ADD TO EXISTING TOTALS
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             XXCNT
     C                   MOVE      *BLANKS       X1CNT
     C                   MOVEA     XCNT(V)       X1CNT
     C                   MOVE      X1CNT         XXCNT
     C                   ADD       ARAMT         XXCNT
     C                   MOVE      XXCNT         X1CNT
     C*
     C                   MOVEA     X1CNT         XCNT(V)
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************--*
     C*   PRINT A/R CODE SUMMARY                                      *  *
     C*****************************************************************--*
     C*
     CSR   ARDETL        BEGSR
     C*
     C* MOVE CODE AND TOTAL ARRAYS TO COMBINATION ARRAY
     C*
     C                   Z-ADD     1             V
     C                   Z-ADD     0             SUMTTL           11 2
     C*
     C     XCOD(V)       DOWNE     *BLANKS
     C                   MOVEL     XCOD(V)       COMB(V)
     C                   MOVE      XCNT(V)       COMB(V)
     C                   ADD       1             V
     C                   END
     C*
     C* IF THERE ARE RECORDS IN THE FILE CONTINUE.
     C*
     C     V             IFNE      1
     C*
     C* PRINT SUMMARY HEADING.                                         DES
     C*
     C   11              EXCEPT    ARHEAD
     C*
     C*  SORT THE ARRAY
     C*
     C                   SORTA     COMB
     C*
     C*  FIND THE FIRST RECORD
     C*
     C                   Z-ADD     1             X1                5 0
     C     COMB(X1)      DOWEQ     *BLANKS
     C                   ADD       1             X1
     C                   END
     C*
     C     X1            DOWLE     999
     C                   MOVE      *BLANKS       XCOMB
     C                   MOVEA     COMB(X1)      XCOMB
     C*
     C*  RETRIEVE CAUSE CODE DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVE      *BLANKS       XXDES            30
     C                   MOVE      *BLANKS       XDES
     C                   MOVE      *BLANKS       XTYP
     C*
     C                   MOVE      XCODE         XXCD              2
     C                   MOVEL     XXCD          XXKEY
     C     TBKEY         CHAIN     PAC01                              60
     C                   MOVE      ACDESC        XDES             30
     C                   MOVE      ACTYPE        XTYP              1
     C*
     C*  PRINT DETAIL LINE
     C*
     C*
     C     *INOF         IFEQ      '1'
     C   11              EXCEPT    HEAD1
     C   11              EXCEPT    ARHEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C* ADD AMOUNT TO SUMMARY TOTAL FIELD
     C*
     C                   ADD       XYCNT         SUMTTL
     C*
     C   11              EXCEPT    ARDTL
     C                   ADD       1             X1
     C                   END
     C*
     C* PRINT SUMMARY TOTAL LINE
     C*
     C   11              EXCEPT    ARTTL
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     O**
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
     O**********        09   UPUSNM              67
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
     O          E            ROUTE          2 38
     O                                           30 'A/R CODES INCLUDED IN'
     O                                           42 'CALCULATION'
     O          E            ROUTE          1
     O                  20                       11 '1.'
     O                       CDE(1)              14
     O                       DES(1)              45
     O                  30                       50 '11.'
     O                       CDE(11)             53
     O                       DES(11)             84
     O                  40                       89 '21.'
     O                       CDE(21)             92
     O                       DES(21)            123
     O          E            ROUTE          1
     O                  21                       11 '2.'
     O                       CDE(2)              14
     O                       DES(2)              45
     O                  31                       50 '12.'
     O                       CDE(12)             53
     O                       DES(12)             84
     O                  41                       89 '22.'
     O                       CDE(22)             92
     O                       DES(22)            123
     O          E            ROUTE          1
     O                  22                       11 '3.'
     O                       CDE(3)              14
     O                       DES(3)              45
     O                  32                       50 '13.'
     O                       CDE(13)             53
     O                       DES(13)             84
     O                  42                       89 '23.'
     O                       CDE(23)             92
     O                       DES(23)            123
     O          E            ROUTE          1
     O                  23                       11 '4.'
     O                       CDE(4)              14
     O                       DES(4)              45
     O                  33                       50 '14.'
     O                       CDE(14)             53
     O                       DES(14)             84
     O                  43                       89 '24.'
     O                       CDE(24)             92
     O                       DES(24)            123
     O          E            ROUTE          1
     O                  24                       11 '5.'
     O                       CDE(5)              14
     O                       DES(5)              45
     O                  34                       50 '15.'
     O                       CDE(15)             53
     O                       DES(15)             84
     O                  44                       89 '25.'
     O                       CDE(25)             92
     O                       DES(25)            123
     O          E            ROUTE          1
     O                  25                       11 '6.'
     O                       CDE(6)              14
     O                       DES(6)              45
     O                  35                       50 '16.'
     O                       CDE(16)             53
     O                       DES(16)             84
     O                  45                       89 '26.'
     O                       CDE(26)             92
     O                       DES(26)            123
     O          E            ROUTE          1
     O                  26                       11 '7.'
     O                       CDE(7)              14
     O                       DES(7)              45
     O                  36                       50 '17.'
     O                       CDE(17)             53
     O                       DES(17)             84
     O                  46                       89 '27.'
     O                       CDE(27)             92
     O                       DES(27)            123
     O          E            ROUTE          1
     O                  27                       11 '8.'
     O                       CDE(8)              14
     O                       DES(8)              45
     O                  37                       50 '18.'
     O                       CDE(18)             53
     O                       DES(18)             84
     O                  47                       89 '28.'
     O                       CDE(28)             92
     O                       DES(28)            123
     O          E            ROUTE          1
     O                  28                       11 '9.'
     O                       CDE(9)              14
     O                       DES(9)              45
     O                  38                       50 '19.'
     O                       CDE(19)             53
     O                       DES(19)             84
     O                  48                       89 '29.'
     O                       CDE(29)             92
     O                       DES(29)            123
     O          E            ROUTE          1
     O                  29                       11 '10.'
     O                       CDE(10)             14
     O                       DES(10)             45
     O                  39                       50 '20.'
     O                       CDE(20)             53
     O                       DES(20)             84
     O                  49                       89 '30.'
     O                       CDE(30)             92
     O                       DES(30)            123
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                57 'INC. 1997     '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE               8 '  /  /  '
     O                       XXCONM              84
     O                       DSTIME             127 '  :  :  '
     O          E            HEAD           1
     O                       ORPNAM              10
     O                       XXTITL              94
     O                       PAGE               127
     O                                          121 'PAGE:'
     O*
     O          E            HEAD           2
     O                                           54 'FROM:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFDT Y   63
     O**********                         67 'TO:'
     O**********               XXTDT Y   76
     O                       X1FDT               65 '  /  /    '
     O                                           69 'TO:'
     O                       X1TDT               80 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD           2
     O                                            8 'PRODUCT:'
     O                       XXSYS               10
     O          E            HEAD2          2
     O                                            8 'CASE #'
     O                                           33 'INV #'
     O                                           44 'DATE'
     O                                           55 'CHECK#'
     O                                           61 'CODE'
     O                                           76 'DESCRIPTION'
     O                                          108 'PAYMENTS'
     O                                          123 'ADJUSTMENTS'
     O          E            PRTOT       1  1
     O                                            8 'PRODUCT'
     O                       XXSYS               10
     O                                           18 'TOTALS:'
     O                                           49 'NUMBER OF PAYMENTS:'
     O                       XPCPRO        1     59
     O                                           96 'TOTAL AMOUNT: '
     O                       XPAPRO        J    108
     O          E            PRTOT          2
     O                                           49 'NUMBER OF ADJUSTMENTS:'
     O                       XACPRO        1     59
     O                                           96 'TOTAL AMOUNT: '
     O                       XAAPRO        J    123
     O          E   NLRNOA   PRTOT          2
     O                                            8 'PRODUCT:'
     O                       ARSYS               10
     O          E            DETAIL         1
     O                       XC#                 22
     O                       XXINV#        Z     33
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXDATE    46 '  /  /  '
     O                       XXDATE              46 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O                       XXCHK#        Z     55
     O                       XXTRCD              60
     O                       XXDESC              95
     O                       XPADET        J    108
     O                       XAADET        J    123
     O**
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
     O**********        09   UPUSNM              67
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
     O****    E   60           ROUTE1
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               57 'INC. 1997     '
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT1  E            HEAD1          1 06
     O                       DSDATE               8 '  /  /  '
     O                       XXCONM              84
     O                       DSTIME             127 '  :  :  '
     O          E            HEAD1          1
     O                       ORPNAM              10
     O                       YYTITL              94
     O                       PAGE1         Z    127
     O                                          121 'PAGE:'
     O*
     O          E            HEAD1          2
     O                                           54 'FROM:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFDT Y   63
     O**********                         67 'TO:'
     O**********               XXTDT Y   76
     O                       X1FDT               65 '  /  /    '
     O                                           69 'TO:'
     O                       X1TDT               80 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O*
     O          E            HEAD12         1
     O                                            7 'PRODUCT'
     O                                           20 'SUBSCRIBER'
     O                                           28 'GROUP'
     O                                           72 'NUMBER OF'
     O                                           91 'ADJUSTMENT'
     O                                          109 'NUMBER OF'
     O                                          127 'PAYMENT'
     O          E            HEAD12         2
     O                                           73 'ADJUSTMENTS'
     O                                           89 'AMOUNT'
     O                                          108 'PAYMENTS'
     O                                          127 'AMOUNT'
     O          E            GRPTL1         2
     O                       XXSYS                4
     O                       XXSUB#              17
     O                       XXFND               27
     O                       XACGRP        1     68
     O                       XAAGRP        J     90
     O                       XPCGRP        1    106
     O                       XPAGRP        J    127
     O          E            SBTOT1         2
     O                                           35 'SUBSCRIBER SUBTOTAL:'
     O                       XACSUB        1     68
     O                       XAASUB        J     90
     O                       XPCSUB        1    106
     O                       XPASUB        J    127
     O          E            PRTOT1         2
     O                                           35 'PRODUCT TOTAL:'
     O                       XACPRO        1     68
     O                       XAAPRO        J     90
     O                       XPCPRO        1    106
     O                       XPAPRO        J    127
     O          E            GRTOT1      1  2
     O                                           35 'GRAND TOTAL:'
     O                       XACFIN        1     68
     O                       XAAFIN        J     90
     O                       XPCFIN        1    106
     O                       XPAFIN        J    127
     O          E            ARHEAD      3  2
     O                                           29 '* * *   A / R   T Y P E'
     O                                           53 'S U M M A R Y   * * * '
     O          E            ARHEAD         2
     O                                           17 'CODE    TYPE'
     O                                           31 'DESCRIPTION'
     O                                           67 'TOTAL'
     O          E            ARDTL          1
     O                       XCODE                8
     O                       XTYP                15
     O                       XDES                50
     O                       XYCNT         B     69
     O          E            ARTTL       1  1
     O                                           54 'TOTAL:'
     O                       SUMTTL        B     69
     O**
     O          E            ERRMS1      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           45 'SELECTED CRITERIA ***'
