     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7545R - PROVIDER PAYMENTS BY FYR
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 03/23/05 C:15  Project: WC1269                                *
     H*                Added code to load the Payee out of country    *
     H*                address.                                       *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER. *
     H*                                                               *
     H*  04/02/99 C:90 PROJECT: WC889                                 *
     H*                ADD USAGE OF PDADR2 WITH PDADR1.               *
     H*                                                               *
     H*  06/11/98 C:33 SEQ#: WC033/12195                              *
     H*                PRINT THE 'ROUTE TO:' NAME IN DEMAND MODE      *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDPAL3    IF   E           K DISK
     FWMPDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   Key List for the Company File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   Key List for the Override File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   Key List for the User Parameters File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   Key List for the Claims Check Payment File
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR PAYEE DEMOGRAPHICE FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C     PDKEY1        KLIST
     C                   KFLD                    XXFEIN
     C*
     C*   KEY LIST FOR FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   Key List for the Claims MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFDT             8            MM/DD/YY
     C                   PARM                    QQTDT             8            MM/DD/YY
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
     C*   Set Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C                   MOVE      QQMOD         XXMOD             3 0
     C                   MOVE      QQFDT         X1FDT             8 0          MM/DD/YY
     C*
     C                   Z-ADD     X1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXFDT             6 0
     C*
     C                   MOVE      QQTDT         X1TDT             8 0          MM/DD/YY
     C*
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXTDT             6 0
     C*
     C     XXFEIN        COMP      0                                      25
     C     XXFYR         COMP      0                                      21
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   Get the Company Record
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
     C***  BEGIN DEL  ***  06/11/98  *********************************
     C****Get*the Routing Record
     C****Print*the Distribution Information
     C**********
     C**********           MOVE '0'       XXPRT   1
     C********** ORKEY     CHAINPOR01                60
     C**N60*****           EXCPTROUTE
     C***  END   DEL  ***  06/11/98  *********************************
     C*
     C*   Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C***  BEGIN ADD  ***  06/11/98  *********************************
     C*   Get the Routing Record
     C*   Print the Distribution Information
     C*
     C                   MOVE      '0'           XXPRT             1
     C*
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C*
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C*
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF                                                  N<=60
     C*
     C                   ENDIF                                                  ORTITL<>*BLANKS
     C*
      ***** Beg Del ***** 08/26/14 ***********************************
     C********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ***********************************
     C*
     C*  Center Text in Field
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        ORTITL
     C***  END   ADD  ***  06/11/98  *********************************
     C*
     C*  Get the Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C   60              MOVE      *BLANK        FNNAME           60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C                   EXCEPT    HEAD
     C*
     C*   Read DETAIL FOR FUND YEAT IF FYR = 0
     C*
     C                   Z-ADD     0             XXGAMT           11 2
     C   21F1KEY         CHAIN     PF101                              60
     C     *IN60         DOWEQ     '0'
     C   21              Z-ADD     F1FYR         XXFYR
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    FYRH
     C*
     C                   Z-ADD     0             XXTAMT           11 2
     C                   Z-ADD     0             XXAMT            11 2
     C                   Z-ADD     0             XXCONT            6 0
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C     PAFEIN        IFNE      0
     C*
     C*  CHECK TO MAKE SURE THAT THIS IS NOT A PAYMENT
     C*  TO THE CLAIMANT
     C*
     C     CMKEY         CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      CMSS#         XXSS#             9 0
     C     XXSS#         IFNE      PAFEIN
     C*
     C     XXFEIN        IFNE      PAFEIN
     C     XXMOD         ORNE      PAMOD
     C*
     C     XXAMT         IFNE      0
     C                   EXSR      PDLOAD
     C                   END
     C                   MOVE      PAFEIN        XXFEIN
     C                   MOVE      PAMOD         XXMOD
     C                   END
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   ADD       PAAMT         XXAMT
     C                   ELSE
     C                   SUB       PAAMT         XXAMT
     C                   END
     C                   ADD       1             XXCONT
     C                   END
     C                   END
     C                   END
     C*
     C     PAKEY         READE     PPA01                                  60
     C                   END
     C*
     C     XXAMT         IFNE      0
     C     *IN25         OREQ      '0'
     C                   EXSR      PDLOAD
     C                   END
     C*
     C*  PRINT FUNDVYEAR TOTAL LINE
     C*
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    FYTOTL
     C*
     C   21              ADD       XXTAMT        XXGAMT
     C*
     C   21F1KEY         READE     PF101                                  60
     C  N21              MOVE      '1'           *IN60
     C                   END
     C*
     C* PRINT GRAND TOTAL IF MOORE THEN 1 FYR
     C   21
     CAN OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C   21              EXCEPT    GRTOTL
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C****************************************************************
     C*        SR TO LOAD PATEE DEMOGRAPHICS    "PDLOAD"
     C*
     C     PDLOAD        BEGSR
     C                   MOVE      *BLANK        QQLAST           56
     C                   MOVE      *BLANK        QQFRST           15
     C                   MOVE      *BLANK        XXNAME           25
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C     PDKEY1        CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       PDFSN
     C                   MOVE      *ALL'*'       PDNAME
     C                   MOVE      *ALL'*'       PDADR1
      **** BEGIN ADD **** 04/02/99 ************************************
     C                   MOVE      *ALL'*'       PDADR2
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVE      *ALL'*'       PDST
     C                   MOVE      *ZERO         PDZIP
     C                   MOVE      *ALL'*'       PDCTY
     C                   END
     C                   END
     C*
     C                   MOVEL     PDNAME        QQLAST
     C                   MOVEL     PDFSN         QQFRST
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        XXNAME
     C*
     C*   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
     C*
     C                   MOVE      *BLANKS       XXADR1           25
      **** BEGIN ADD **** 04/02/99 ************************************
     C                   MOVE      *BLANKS       XXADR3           25
      **** END   ADD **** 04/02/99 ************************************
     C                   MOVE      *BLANKS       XXADR2           30
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1             5
     C                   MOVE      *BLANKS       XXZP2             4
     C                   MOVEL     PDADR1        XXADR1
      **** BEGIN ADD **** 04/02/99 ************************************
     C                   MOVEL     PDADR2        XXADR3
      **** END   ADD **** 04/02/99 ************************************
     C*
     C*** Begin add *** 03/23/05 **************************************
     C     PDADR3        IFNE      *BLANKS
     C                   MOVEL     PDADR3        XXADR2
     C                   ELSE
     C*** End add ***** 03/23/05 **************************************
     C                   MOVEL     PDST          XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVEL     PDZIP         XXZP1
     C                   MOVE      PDZIP         XXZPP2            4 0
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPP2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPP2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDCTY         XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXADR2           30
     C*** Begin add *** 03/23/05 **************************************
     C                   ENDIF
     C*** End add ***** 03/23/05 **************************************
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    FYRH
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DETL
     C*
     C                   ADD       XXAMT         XXTAMT
     C*
     C                   Z-ADD     0             XXAMT
     C                   Z-ADD     0             XXCONT
     C*
     C                   ENDSR
     C****************************************************************
     O*
     O*****************************************************************
     O*                        Output Specs                           *
     O*****************************************************************
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *****************************************
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
      ***** End Del ***** 08/26/14 *****************************************
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
      ***** Beg Del ***** 08/26/14 *****************************************
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
      ***** End Del ***** 08/26/14 *****************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 02
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              97
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z   +  1
     O                       FNNAME            +  1
     O                                           91 'DATE RANGE:'
     O                       XXFDT         Y    100
     O                                          102 '-'
     O                       XXTDT         Y    111
     O          E            HEAD           2
     O                                           15 'PROVIDER TYPES:'
     O                                           33 '1-MEDICAL; 2-SDF;'
     O                                           53 '3-OTHER(INCLUDED IN'
     O                                           72 'REFUND REPORTING);'
     O                                           93 '4-HOSPITAL; 5-LEGAL;'
     O                                          113 '6-OTHER(NOT INC. IN'
     O                                          131 'REFUND REPORTING)'
     O          E            HEAD           1
     O                                          105 'PVDR'
     O                                          115 'NUMBER'
     O                                          130 'TOTAL'
     O          E            HEAD           2
     O                                           10 'FEDERAL ID'
     O                                           28 'PROVIDER NAME'
     O                                           48 'ADDRESS'
     O                                           86 'CITY, STATE, ZIP'
     O                                          105 'TYPE'
     O                                          116 'OF PMTS'
     O                                          132 'AMOUNT PAID'
     O          E            FYRH           1
     O                                              'POLICY YEAR:'
     O                       XXFYR         Z   +  1
     O          E            DETL           1
     O                       XXFEIN              09
     O                       XXMOD         Z     13
     O                       XXNAME              40
     O                       XXADR1              66
     O                       XXADR2             100
     O                       PDTYPE             103
     O                       XXCONT        K    116
     O                       XXAMT         J    132
     O          E            FYTOTL      1  3
      **** BEGIN ADD **** 04/02/99 ************************************
     O                       XXADR3              66
      **** END   ADD **** 04/02/99 ************************************
     O                                           95 'TOTAL AMOUNT PAID FOR'
     O                                         +  1 'POLICY YEAR'
     O                       XXFYR             +  1 '   :'
     O                       XXTAMT        J    132
     O          E            GRTOTL         3
     O                                           88 'GRAND TOTAL PAID'
     O                                          104 'FOR DATE RANGE:'
     O                       XXGAMT        J    132
