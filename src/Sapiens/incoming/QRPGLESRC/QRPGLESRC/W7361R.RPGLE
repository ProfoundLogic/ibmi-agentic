     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7361R - PREMIUM WITH EXPENSE CONSTANT BREAKOUT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 02/07/02 C:65 PROJECT WC1067                                  *
     H*               BREAK OUT EXPENSE CONSTANT ON A MIN PREM POLICY *
     H*               IN A/R IF INDICATED.                            *
     H*                                                               *
     H*  12/14/99  C:118 PROJECT: WC577                               *
     H*                  MODIFY 'REPORT TITLE' TO PRINT PROPERLY.     *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS, AND CENTER NAME.   *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F*** BEGIN ADD ***  02/07/02  ************************************
     FWMEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
     F*** END ADD *****  02/07/02  ************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQQUE            10            MLS 1/18/88
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C*          XXFYR     ADD  1900      XXYEAR  40
     C*                                                    MLS 1/18/88
     C*   KEY LIST FOR THE USER PARAMETER FILE                  "
     C*                                                         "
     C     UPKEY         KLIST                                                       "
     C                   KFLD                    Q1USER                              "
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C                   TIME                    DSDTTM
     C*                                                    MLS 1/18/88
     C***** BEGIN ADD **** 12/14/99 ***********************************
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C***** END   ADD **** 12/14/99 ***********************************
     C*
     C*   SET-UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*                                                    MLS 1/18/88
     C*   CHAIN TO THE USER PARAMETER FILE                      "
     C*                                                         "
     C     UPKEY         CHAIN     PUP01                              60             "
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*** BEGIN ADD *** 10/27/97 *******************************
     C     *IN60         IFEQ      '0'
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C*** END ADD ***** 10/27/97 ********************************
     C*
     C*   CHAIN TO THE FUND DETAIL
     C*
     C     F1KEY         CHAIN     PF101                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C***** BEGIN ADD **** 12/14/99 ***********************************
     C* LEFT JUSTIFY TITLE FOR ROUTING PAGE
     C*
     C                   MOVE      ORTITL        XXTITL
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF                                                  N<=60
     C                   ENDIF                                                  ORTITL<>BLK
     C*
     C***** END   ADD **** 12/14/99 ***********************************
     C*
      ***** Beg Del ***** 08/26/14  **********************************
     C*********     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C*********          EXCEPT    ROUTE
     C*********          END
      ***** End Del ***** 08/26/14  **********************************
     C*
     C                   MOVE      '1'           *INOA
     C*
     C*   START READING THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*
     C*   GET THE EMPLOYER HEADER FILE
     C*
     C                   Z-ADD     0             NOREC             1 0
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   MOVE EMPLOYER NAME TO SMALLER PRINTING FIELD
     C*
     C                   MOVE      *BLANK        XXNAME
     C                   MOVEL     EMNAME        XXNAME           23
     C*
     C                   Z-ADD     ELPRTD        XXEXCO            9 2
     C*
     C     ELFAF         IFEQ      'A'
     C*
     C                   Z-ADD     ELAAPR        XXAPR            11 2
     C     ELAAPR        SUB       XXEXCO        XXTNOR           11 2
     C*
     C     ELAMNP        IFEQ      'Y'
     C                   Z-ADD     ELAAPR        XXTNOR
     C*** BEGIN ADD ***  02/07/02  ************************************
     C     FNKEY         CHAIN     PEX02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EXBEXP        ANDEQ     'Y'
     C                   SUB       XXEXCO        XXTNOR
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  02/07/02  ************************************
     C                   END
     C*
     C                   ADD       1             XXTAM             5 0
     C*
     C                   ELSE
     C*
     C*   ESTIMATED ACCOUNTS, NO AUDIT RUN
     C*
     C                   Z-ADD     ELANPR        XXAPR
     C     ELANPR        SUB       XXEXCO        XXTNOR
     C*
     C     ELEMNP        IFEQ      'Y'
     C                   Z-ADD     ELANPR        XXTNOR
     C*** BEGIN ADD ***  02/07/02  ************************************
     C     FNKEY         CHAIN     PEX02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EXBEXP        ANDEQ     'Y'
     C                   SUB       XXEXCO        XXTNOR
     C                   ENDIF                                                  *IN60 IFEQ
     C*** END ADD *****  02/07/02  ************************************
     C                   END
     C*
     C                   END
     C*
     C     XXAPR         IFEQ      *ZERO
     C                   Z-ADD     0             XXTNOR
     C                   END
     C*
     C*
     C*   PRINT THE LINE
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   ADD       1             NOREC
     C                   EXCEPT    DET
     C*
     C*   INCREMENT THE TOTALS
     C*
     C                   ADD       1             XXFECT            5 0
     C                   ADD       XXAPR         XXFPRE           11 2
     C                   ADD       XXEXCO        XXFEXC            9 2
     C                   ADD       XXTNOR        XXFDIF           11 2
     C                   END
     C*
     C     ELKEY         READE     PEL01                                  60
     C     *IN60         IFEQ      '1'
     C     NOREC         ANDEQ     0
     C                   EXCEPT    DETL2
     C                   END
     C                   END
     C*
     C*   PRINT THE TOTALS
     C*
     C     *IN80         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *IN80
     C                   END
     C*
     C*   DETERMINE THE % IN AUDIT
     C*
     C     XXTAM         IFNE      *ZERO
     C     XXTAM         DIV(H)    XXFECT        XXPIA             3 2
     C                   END
     C*
     C                   EXCEPT    FNDTT
     C*
      /Free
       // ***** Beg Add ***** 08/26/14 ********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14  **********************************
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
     O*  MLS 1/18/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14  **********************************
      ***** Beg Add ***** 08/05/15 ***********************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/18/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ***********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG **** 12/14/99 ***********************************
     O**********               ORTITL    76
     O                       ORTITL              80
     O***** BEGIN CHG **** 12/14/99 ***********************************
      ***** Beg Del ***** 08/26/14  **********************************
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
     O*  MLS 1/18/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14  **********************************
     O          E            ROUTE          2
     O                                         +  6 'POLICY YEAR:'
     O                       XXFYR         Z   +  2
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME              80
     O                       DSDATE        Y    100
     O          E            HEAD           1
     O                       Q1PGM
     O***** BEGIN CHG **** 12/14/99 ***********************************
     O**********               ORTITL    80
     O                       XXTITL              80
     O***** END   CHG **** 12/14/99 ***********************************
     O                                           95 'PAGE'
     O                       PAGE          Z    100
     O          E            HEAD           2
     O                                           53 'POLICY YEAR'
     O                       XXFYR         Z   +  3
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              72
     O          E            HEAD           2
     O                                           09 'EMPLOYER'
     O                                           15 'DIV'
     O                                           24 'EMPLOYER'
     O                                           29 'NAME'
     O                                           52 'PREMIUM'
     O                                           57 'EXP'
     O                                           66 'CONSTANT'
     O                                           81 'DIFFERENCE'
     O                                           91 'EST/AUD'
     O                                          100 'PRORATA'
     O          E            DETL2          1
     O                                           54 '***   NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA   ***'
     O          E            DET            1
     O                       ELEMP#        Z     09
     O                       ELDIV         Z     15
     O                       XXNAME              39
     O                       XXAPR         J     53
     O                       XXEXCO        J     67
     O                       XXTNOR        J     82
     O                       ELFAF               89
     O                       ELPROR        J     99
     O          E            FNDTT       1  2
     O                                           05 'FINAL'
     O                                           12 'TOTALS'
     O                                           25 'EMPLOYER'
     O                                           31 'COUNT'
     O                       XXFECT        Z     37
     O                       XXFPRE        J     53
     O                       XXFEXC        J     67
     O                       XXFDIF        J     82
     O          E            FNDTT       1
     O                                           30 'TOTAL AUDITED:'
     O                       XXTAM         Z     37
     O                       XXPIA               48 ' .  %'
