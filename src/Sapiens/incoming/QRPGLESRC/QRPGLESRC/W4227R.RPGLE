     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4227R - Check Reconciliation Report
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
     H* 04/10/00  C:118 PROJECT: WC987                                *
     H*                 LOAD COMPANY ALTERNATE NAME FROM FUND MASTER  *
     H*                 IF AVAILABLE.                                 *
     H*                                                               *
     H* 02/02/00  C:118 PROJECT: WC577                                *
     H*                 Change 'fund' to 'group' and 'fund year' to   *
     H*                 'policy year'.                                *
     H*                                                               *
     H*  08/02/99 C:118 PROJECT: WC577                                *
     H*                 If CSSTAT=I, continue to READP PCS01.         *
     H*                                                               *
     H*  09/04/97 C:30 Project:  WC659                                *
     H*           Version 3.  Replace WMBAP with WMBIP.               *
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *** BEGIN CHG *** 09/04/97 ********************************
     F****WMBAL1  IF  E           K        DISK
      *
      *  Bank Information file
     FWMBIL4    IF   E           K DISK
      *** END   CHG *** 09/04/97 ********************************
     FWMFNL     IF   E           K DISK
     FWXCXL     IF   E           K DISK
     FWMCSL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D TP              S              1    DIM(12) CTDATA PERRCD(12)
     D DS              S             11    DIM(12) CTDATA PERRCD(1)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the Output Routing File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
      *** BEGIN CHG *** 09/04/97 ***********************************************
     C**Key List for the Bank Account Master File
     C*****
     C*****      BAKEY     KLIST
     C*****                KFLD           XXCO#   30
     C*****                KFLD           XXACCT 20
      *
      *  Bank Information file
      *
     C     BIKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXAPPL            2
     C                   KFLD                    XXACCT           20
      *
      *** END   CHG *** 09/04/97 ***********************************************
     C*
     C* KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#             3 0
      *** BEGIN CHG *** 09/04/97 **************************************
     C*********************KFLD***********BAFND
     C                   KFLD                    BIFND
      *** BEGIN CHG *** 09/04/97 **************************************
     C*
     C* Key list for the Check Reconciliation WORK File
     C*
     C     CXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C*
     C* Key List for the Check Summary Master File
     C*
     C     CSKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXACCT           20
     C                   KFLD                    XXTODT
     C*
     C     *LIKE         DEFINE    CSDT          XXTODT
     C     *LIKE         DEFINE    CSEDAT        XXFRDT
     C*
     C* Entry Parms
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQACCT           20
     C                   PARM                    QQTDT             8
     C                   PARM                    QQQUE            10
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
     C* Set-Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVEL     QQACCT        XXACCT           20
     C                   MOVE      QQTDT         X1TODT            8 0
     C*
     C                   Z-ADD     X1TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDATE            6 0
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C* Initialize work fields.
     C*
     C                   Z-ADD     0             NEWBAL            9 2           NEW BALANCE
     C                   MOVE      *BLANK        SVTYPE            1             SAVE TYPE
     C*
     C* GET THE CHECK SUMMARY RECORD PRIOR TO THIS RECONCILIATION
     C* IN ORDER TO GET PREVIOUS BALANCE
     C*
     C     CSKEY         CHAIN     PCS01                              60
     C                   READP     PCS01                                  60
     C***** BEGIN ADD **** 08/02/99 ***********************************
     C     *IN60         DOWEQ     '0'
     C     CSSTAT        ANDEQ     'I'
     C                   READP     PCS01                                  60
     C                   ENDDO                                                  ENDDO=0
     C***** END   ADD **** 08/02/99 ***********************************
     C     *IN60         IFEQ      '0'
     C     CSCO#         ANDEQ     XXCO#
     C     CSACCT        ANDEQ     XXACCT
     C                   Z-ADD     CSEBAL        OLDBAL            9 2
     C                   Z-ADD     CSEDAT        XXFRDT
     C                   ELSE
     C                   Z-ADD     0             OLDBAL
     C                   Z-ADD     0             XXFRDT
     C                   END
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* Get the Routing Record
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C* Get the Bank Account Master Record
     C*
      *** BEGIN DEL *** 09/04/97 ****************************************
     C***        BAKEY     CHAINPBA01                60
     C***        *IN60     DOWEQ'0'
     C***        BASTAT    ANDEQ'I'
     C***        BAKEY     READEPBA01                    60
     C***                  END
      *** END   DEL *** 09/04/97 ****************************************
      *** BEGIN ADD *** 09/04/97 *************************************
      *
     C                   MOVE      'WC'          XXAPPL
      *
     C     BIKEY         CHAIN     PBI01                              60
      *
     C     *IN60         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKEY         READE     PBI01                                  60
     C                   ENDDO
      *
      *** END   ADD *** 09/04/97 *************************************
     C*
     C* GET THE FUND MASTER RECORD
     C*
     C                   MOVE      *BLANKS       XFNAME           60
     C  N60FNKEY         CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        XFNAME
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C*  LOAD ALTERNATE COMPANY NAME IF AVAILABLE
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C***** END   ADD **** 04/10/00 ***********************************
     C*   PRINT ALL DETAIL RECORDS FOR THIS RECONCILIATION, EXCEPT
     C*   BANK ERRORS WHICH ARE GREATER THAN $10.00. BREAK ON TYPE.
     C*
     C     CXKEY         CHAIN     PCX01                              60
     C*
     C*  THE FIRST RECORD IS THE SUMMARY RECORD - SAVE AMOUTS & CALC
     C*                                           NEW BALANCE
     C     *IN60         IFEQ      '0'
     C     CXTYPE        ANDEQ     *BLANK
     C                   Z-ADD     OLDBAL        NEWBAL
     C                   ADD       CXDEP         NEWBAL
     C                   SUB       CXCHK         NEWBAL
     C                   ADD       CXTRN         NEWBAL
     C                   SUB       CXNSF         NEWBAL
     C                   ADD       CXADJ         NEWBAL
     C                   ADD       CXREC         NEWBAL
     C                   ADD       CXPSTP        NEWBAL
     C                   ADD       CXCSTP        NEWBAL
     C                   SUB       CXPREV        NEWBAL
     C                   SUB       CXCREV        NEWBAL
     C                   ADD       CXPVOD        NEWBAL
     C                   ADD       CXCVOD        NEWBAL
     C                   SUB       CXCHG         NEWBAL
     C                   ADD       CXERR         NEWBAL
     C                   ADD       CXINT         NEWBAL
     C                   Z-ADD     CXADJ         XXADJ             9 2           ADJUSTMENT
     C                   Z-ADD     CXCHG         XXCHG             9 2           BANK CHARGE
     C                   Z-ADD     CXCHK         XXCHK             9 2           CHECK
     C                   Z-ADD     CXDEP         XXDEP             9 2           DEPOSIT
     C                   Z-ADD     CXERR         XXERR             9 2           BANK ERROR
     C                   Z-ADD     CXINT         XXINT             9 2           INTEREST
     C                   Z-ADD     CXNSF         XXNSF             9 2           NSF
     C                   Z-ADD     CXPSTP        XXPSTP            9 2           STOP PAY
     C                   Z-ADD     CXCSTP        XXCSTP            9 2           STOP PAY
     C                   Z-ADD     CXREC         XXREC             9 2           RECOVERY
     C                   Z-ADD     CXTRN         XXTRN             9 2           TRANSFER
     C                   Z-ADD     CXPVOD        XXPVOD            9 2           VOID
     C                   Z-ADD     CXCVOD        XXCVOD            9 2           VOID
     C                   Z-ADD     CXPREV        XXPREV            9 2           REVERSAL
     C                   Z-ADD     CXCREV        XXCREV            9 2           REVERSAL
     C                   END
     C*
     C*  READ DETAIL TRANSACTIONS AND PRINT
     C*
     C  N60CXKEY         READE     PCX01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*  BREAK ON TYPE, GET NEW TRANSACTION HEADING
     C*
     C     CXTYPE        IFNE      SVTYPE
     C     CXTYPE        ANDNE     *BLANK
     C*
     C                   MOVEA     '00'          *IN(36)
     C                   EXCEPT    HEAD
     C                   MOVE      CXTYPE        SVTYPE
     C                   Z-ADD     1             X                 2 0
     C     CXTYPE        LOOKUP    TP(X)                                  50
     C                   MOVE      *BLANKS       XXTRNS           11
     C                   MOVEL     DS(X)         XXTRNS
     C*
     C     CXTYPE        IFEQ      'S'
     C     CXTYPE        OREQ      'V'
     C     CXTYPE        OREQ      'Z'
     C     CXCKDT        IFLE      XXFRDT
     C                   MOVE      '1'           *IN36
     C                   ELSE
     C                   MOVE      '1'           *IN37
     C                   END
     C                   END
     C*
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C*  BREAK ON PRIOR - CURRENT STOPS, VOIDS, & REVERSALS
     C*
     C     *IN36         IFEQ      '1'
     C     CXCKDT        ANDGT     XXFRDT
     C                   MOVE      '1'           *IN37
     C                   MOVE      '0'           *IN36
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   Z-ADD     CXCKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCKDT            6 0
     C*
     C                   MOVEL     CXKY          XXKEY            10
     C                   MOVEL     XXKEY         XXFYR             3 0
     C                   MOVE      XXKEY         XXCASE            7 0
     C*
     C                   Z-ADD     CXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C     XXACDT        COMP      0                                      35
     C     SVTYPE        IFEQ      'S'
     C     SVTYPE        OREQ      'V'
     C     SVTYPE        OREQ      'Z'
     C                   MOVE      'C'           XXTYPE            1
     C                   ELSE
     C                   MOVE      SVTYPE        XXTYPE
     C                   END
     C                   EXCEPT    DET
     C*
     C     CXKEY         READE     PCX01                                  60
     C                   END
     C*
     C*  PRINT SUMMARY
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    SUMM
     C*
     C* End the Program
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1990'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O*
     O* Report Heading
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              85
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O***** BEGIN CHG **** 02/02/00 ***********************************
     O**********                         05 'FUND:'
     O**********               FNFND Z   09
     O**********               XFNAME    70
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       XFNAME              71
     O***** END   CHG **** 02/02/00 ***********************************
     O          E            HEAD           2
     O                                           09 'ACCOUNT#:'
     O                       XXACCT              30
      *** BEGIN CHG *** 09/04/97 **************************************
     O*************************BANAME****72
     O                       BIAI01              72
      *** BEGIN CHG *** 09/04/97 **************************************
     O                                           93 'RECONCILIATION'
     O                                          106 'ENDING DATE:'
     O                       XXDATE        Y    116
     O          E            HEAD2          2
     O                                           20 'TYPE OF TRANSACTION:'
     O                       XXTRNS              32
     O                  36                       47 '- PRIOR PERIOD'
     O                  37                       49 '- CURRENT PERIOD'
     O          E            HEAD2          1
     O***** BEGIN CHG **** 02/02/00 ***********************************
     O**********                         31 'CHECK            FUND'
     O                                           31 'CHECK            POLICY'
     O***** END   CHG **** 02/02/00 ***********************************
     O                                           56 'RECORD  ACTIVITY'
     O                                           65 'ACTIVITY'
     O          E            HEAD2          2
     O                                           31 'DATE     CHECK#  YEAR'
     O                                           54 'CASE   TYPE     TYPE'
     O                                           63 'DATE'
     O                                           84 'PAYEE/DESCRIPTION'
     O                                          109 'AMOUNT'
     O          E            DET            1
     O                       XXCKDT        Y     16
     O                       CXCHK#        Z     25
     O                       XXFYR         Z     30
     O                       XXCASE        Z     39
     O                       XXTYPE              43
     O                       CXACTP              52
     O                 N35   XXACDT        Y     65
     O                       CXDESC              97
     O                       CXCKAM        J    112
     O          E            SUMM           3
     O                       OLDBAL        J     20
     O                                           43 'BEGINNING BALANCE  '
     O          E            SUMM           2
     O                       XXDEP         J     20
     O                                           43 'DEPOSITS      (+)  '
     O          E            SUMM           2
     O                       XXCHK         J     20
     O                                           43 'CHECKS        (-)  '
     O          E            SUMM           2
     O                       XXTRN         J     20
     O                                           43 'TRANSFERS     (+,-)'
     O          E            SUMM           2
     O                       XXNSF         J     20
     O                                           43 'NSFs          (-)  '
     O          E            SUMM           2
     O                       XXADJ         J     20
     O                                           43 'ADJUSTMENTS   (+,-)'
     O          E            SUMM           2
     O                       XXREC         J     20
     O                                           43 'RECOVERIES    (+)  '
     O          E            SUMM           2
     O                       XXPSTP        J     20
     O                                           43 'STOP PAYMENTS (+)  '
     O                                           59 '(PRIOR PERIOD)'
     O          E            SUMM           2
     O                       XXCSTP        J     20
     O                                           43 'STOP PAYMENTS (+)  '
     O                                           61 '(CURRENT PERIOD)'
     O          E            SUMM           2
     O                       XXPREV        J     20
     O                                           43 'REVERSALS     (-)  '
     O                                           59 '(PRIOR PERIOD)'
     O          E            SUMM           2
     O                       XXCREV        J     20
     O                                           43 'REVERSALS     (-)  '
     O                                           61 '(CURRENT PERIOD)'
     O          E            SUMM           2
     O                       XXPVOD        J     20
     O                                           43 'VOIDS         (+)  '
     O                                           59 '(PRIOR PERIOD)'
     O          E            SUMM           2
     O                       XXCVOD        J     20
     O                                           43 'VOIDS         (+)  '
     O                                           61 '(CURRENT PERIOD)'
     O          E            SUMM           2
     O                       XXCHG         J     20
     O                                           43 'BANK CHARGES  (-)  '
     O          E            SUMM           2
     O                       XXERR         J     20
     O                                           43 'BANK ERRORS   (+,-)'
     O          E            SUMM           3
     O                       XXINT         J     20
     O                                           43 'INTEREST      (+)  '
     O          E            SUMM           1
     O                       NEWBAL        J     20
     O                                           43 'ENDING BALANCE     '
     O************
**
ABCDEINRSTVZ
**
ADJUSTMENTS
BANK CHARGE
CHECKS
DEPOSITS
BANK ERRORS
INTEREST
NSF
RECOVERIES
STOP PAY
TRANSFERS
VOIDS
REVERSALS
