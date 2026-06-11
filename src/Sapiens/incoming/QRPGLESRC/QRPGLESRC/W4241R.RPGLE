     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4241R - BANK RECONCILIATION REPORT
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/17/00 C:15  Support: WC072/22572                          *
     H*                 Chgd process which repositions WDCRP after    *
     H*                 code which checks for a future reversal, to   *
     H*                 prevent an infinite loop from occurring.      *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER
     H*                 IF AVAILABLE.
     H*
     H*  12/17/99 C:118 PROJECT: WC577                                *
     H*                 Chg fund to group and fund year to policy     *
     H*                 year.                                         *
     H*                                                               *
     H*  09/04/97 C:30 Project: WC659                                 *
     H*           Version 3.  Replace WMBAP with WMBIP.               *
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *** BEGIN CHG *** 09/04/97 ********************************
     F****WMBAL1  IF  E           K        DISK
      *
      *  Bank Information file
     FWMBIL4    IF   E           K DISK
      *** END   CHG *** 09/04/97 ********************************
     FWDCRL3    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

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
     C* KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#             3 0
      *** BEGIN CHG *** 09/04/97 ***********************************************
     C*********************KFLD           BAFND
     C                   KFLD                    BIFND
      *** END   CHG *** 09/04/97 ***********************************************
     C*
     C* Key list for the Check Reconciliation Detail File
     C*
     C     CRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXACCT
     C                   KFLD                    XXRTP
     C*
     C     CRKEY1        KLIST                                                  JJJ 1-12-90
     C                   KFLD                    XXCO#                          |
     C                   KFLD                    XXACCT                         |
     C                   KFLD                    XXTYPE                         |
     C                   KFLD                    CRCHK#                         |
     C                   KFLD                    CRCKDT                         |
     C*
     C     *LIKE         DEFINE    CRDT          XXACDT
     C     *LIKE         DEFINE    CRDT          XXTODT
     C*
     C* Entry Parms
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQACCT           20
     C                   PARM                    QQBAL             9
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
     C                   MOVE      QQBAL         OLDBAL            9 2
     C                   Z-ADD     OLDBAL        XXBAL             9 2
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
     C***** BEGIN ADD **** 12/17/99 ***********************************
     C*
     C* LEFT ALIGN TITLE FOR ROUTING PAGE
     C*
     C                   MOVEL(P)  ORTITL        XXTITL           60
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF                                                  N<=60
     C                   ENDIF                                                  ORTITL<>*BLK
     C*
     C***** END   ADD **** 12/17/99 ***********************************
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE
      ***** Beg Del ***** 08/26/14 ************************************
     C*
     C* Get the Bank Account Master Record
     C*
      *** BEGIN CHG *** 09/04/97 *************************************
     C*******    BAKEY     CHAINPBA01                60
     C*******    *IN60     DOWEQ'0'
     C*******    BASTAT    ANDEQ'I'
     C*******    BAKEY     READEPBA01                    60
     C*******              END
      *
     C                   MOVE      'WC'          XXAPPL
      *
     C     BIKEY         CHAIN     PBI01                              60
      *
     C     *IN60         DOWEQ     '0'
     C     BISTAT        ANDEQ     'I'
     C     BIKEY         READE     PBI01                                  60
     C                   ENDDO
      *** END   CHG *** 09/04/97 *************************************
     C*
     C* GET THE FUND MASTER FILE
     C*
     C                   MOVE      *BLANKS       XFNAME           60
     C  N60FNKEY         CHAIN     PFN01                              60
     C  N60              MOVEL     FNNAME        XFNAME
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C*  LOAD ALTERNATE COMPANY NAME IF AVAILABLE IN FUND MASTER
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>0
     C*
     C***** END   ADD **** 04/10/00 ***********************************
     C*  LOOP & PRINT CHECKS, DEPOSITS, AND BANK ERRORS.
     C*
     C                   MOVE      'Y'           XX1ST             1
     C                   MOVE      '1'           *INOF
     C                   MOVE      'C'           XXRTP             1
     C                   MOVE      '0'           *IN50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*   INITIALIZE INDICATORS FOR OUTPUT PURPOSES.
     C*
     C     XXRTP         COMP      'C'                                    25
     C     XXRTP         COMP      'D'                                    26
     C     XXRTP         COMP      'E'                                    27
     C                   MOVE      'N'           XXPRNT
     C                   Z-ADD     0             XXCNT             5 0
     C                   Z-ADD     0             XXTOTL            9 2
     C*
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   PRINT ALL OUTSTANDING CHECKS & DEPOSITS. ALSO PRINT ALL
     C*   OUTSTANDING ERRORS WHICH ARE > OR = $10.00.  OUTSTANDING
     C*   TRANSACTIONS WILL HAVE CRSTAT = ' '.
     C*   INCLUDE ONLY UP TO THE LAST RECONCILIATION DATE.
     C*
     C     CRRDT1        IFLE      XXTODT
     C     CRRDT1        ANDNE     0
     C     CRSTAT        ANDEQ     ' '
     C*
     C*   MAKE SURE OUTSTANDING CHECK IS NOT DUE TO A FUTURE REVERSAL
     C*
     C                   Z-ADD     0             XXACDT                         JJJ 1-12-90
     C                   MOVE      'Z'           XXTYPE            1            |
     C     CRKEY1        CHAIN     PCR01                              60        |
     C     *IN60         IFEQ      '0'                                          |
     C     CRRVDT        IFLE      XXTODT                                       *JJJ 2-27-90
     C                   Z-ADD     CRACDT        XXACDT                         |
     C                   END                                                    *JJJ 2-27-90
     C*                                                    |
     C*          REPOSITION FILE                           |
     C*                                                    |
     C                   MOVE      XXRTP         XXTYPE                         |
     C*** BEGIN CHG *** 04/17/00 **************************************
     C********** CRKEY1    CHAINPCR01                60    |
     C     CRKEY1        SETGT     PCR01
     C     CRKEY         READPE    PCR01                                  60
     C*** END CHG ***** 04/17/00 **************************************
     C                   END                                                    |
     C*                                                    |
     C     XXACDT        IFLE      XXTODT                                       |
     C*
     C                   MOVE      '0'           *IN61
     C     CRRTP         IFEQ      'E'
     C     CRCKAM        ANDLE     10
     C     CRCKAM        ANDGE     -10
     C                   MOVE      '1'           *IN61
     C                   END
     C*
     C     *IN61         IFEQ      '0'
     C*
     C                   ADD       1             XXCNT
     C                   ADD       CRCKAM        XXTOTL
     C     XXRTP         IFEQ      'C'
     C                   SUB       CRCKAM        XXBAL
     C                   ELSE
     C                   ADD       CRCKAM        XXBAL
     C                   END
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C     XX1ST         IFEQ      'Y'
     C                   EXCEPT    BBAL
     C                   MOVE      'N'           XX1ST
     C                   END
     C                   EXCEPT    HTYPE
     C                   EXCEPT    HDETL
     C                   MOVE      '0'           *INOF
     C                   ELSE
     C     XXPRNT        IFEQ      'N'
     C                   EXCEPT    HTYPE
     C                   END
     C                   END
     C                   Z-ADD     CRCKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCKDT            6 0
     C*
     C                   MOVEL     CRKY          XXKEY            10
     C                   MOVEL     XXKEY         XXFYR             3 0
     C                   MOVE      XXKEY         XXCASE            7 0
     C                   EXCEPT    DETL
     C                   MOVE      'Y'           XXPRNT            1
     C*
     C                   END
     C*
     C                   END                                                    JJJ 1-12-90
     C*
     C                   END
     C*
     C     CRKEY         READE     PCR01                                  60
     C                   END
     C*
     C*  PRINT BALANCE FOR PREVIOUS TYPE IF ANY DETAIL WAS WRITTEN.
     C*
     C     XXPRNT        IFEQ      'Y'
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HTYPE
     C                   EXCEPT    HDETL
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    BAL
     C                   END
     C*
     C*  CONTINUE THRU LOOP FOR THE NEXT TYPE (DEPOSIT OR BANK ERRORS)
     C*  IF PREVIOUS TYPE = 'E' END THE LOOP.
     C*
     C   25              MOVE      'D'           XXRTP
     C   26              MOVE      'E'           XXRTP
     C   27              MOVE      '1'           *IN50
     C                   MOVEA     '000'         *IN(25)
     C                   END
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
     C*****************************************************************
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
     O***     E  160           ROUTE
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
     O***** BEGIN CHG **** 12/17/99 ***********************************
     O**********               ORTITL    96
     O                       XXTITL              96
     O***** END   CHG **** 12/17/99 ***********************************
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O***** BEGIN CHG **** 12/17/99 ***********************************
     O**********                         05 'FUND:'
     O                                           06 'GROUP:'
     O***** END   CHG **** 12/17/99 ***********************************
     O                       FNFND         Z     09
     O                       XFNAME              70
     O          E            HEAD           2
     O                                           09 'ACCOUNT#:'
     O                       XXACCT              30
      *** BEGIN CHG *** 09/04/97 *************************************
     O*************************BANAME    72
     O                       BIAI01              72
      *** END   CHG *** 09/04/97 *************************************
     O                                           98 'LAST RECONCILIATION'
     O                                          104 'DATE:'
     O                       XXDATE        Y    116
     O          E            BBAL           1
     O                                           67 'BEGINNING BALANCE:'
     O                       OLDBAL        J     82
     O          E            HTYPE          2
     O                  25                        6 'CHECKS'
     O                  26                        8 'DEPOSITS'
     O                  27                       11 'BANK ERRORS'
     O          E            HDETL          1
     O                                           18 'CHECK'
     O***** BEGIN CHG **** 12/17/99 ***********************************
     O**********                         26 'FUND'
     O                                           27 'POLICY'
     O***** END   CHG **** 12/17/99 ***********************************
     O          E            HDETL          2
     O                                           26 'CHECK#   DATE    YEAR'
     O                                           53 'CASE   PAYEE/DESCRIPTION'
     O                                           80 'AMOUNT'
     O          E            DETL           1
     O                       CRCHK#        Z     11
     O                       XXCKDT        Y     20
     O                       XXFYR         Z     25
     O                       XXCASE        Z     34
     O                       CRDESC              66
     O                       CRCKAM        J     82
     O          E            BAL         1  2
     O                  25                       20 'TOTAL CHECKS:'
     O                  26                       22 'TOTAL DEPOSITS:'
     O                  27                       25 'TOTAL BANK ERRORS:'
     O                       XXCNT         Z     31
     O                  25                       59 'TOTAL AMOUNT OUTSTANDING'
     O                  25                       67 'CHECKS:'
     O                  26                       57 'TOTAL AMOUNT OUTSTANDING'
     O                  26                       67 'DEPOSITS:'
     O                  27                       54 'TOTAL AMOUNT'
     O                  27                       67 'BANK ERRORS:'
     O                       XXTOTL        J     82
     O          E            BAL            2
     O                                           67 'BALANCE:'
     O                       XXBAL         J     82
