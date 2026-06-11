     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7201R - PAYROLL BY CLASS REPORT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Add column to display the new Payroll Extension*
     H*                field and to adjust display.                   *
     H*                                                               *
     H* 02/21/02 C:136 SUPPORT: WA071 31351                           *
     H*                EXPAND THE FIELDS XXAMT AND GTAMT TO ACCOMODATE*
     H*                LARGER PAYROLL AMOUNTS                         *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                IF AVAILABLE.                                  *
     H*                                                               *
     H* 12/09/99 C:118 PROJECT: WC577                                 *
     H*                CLASSIFICATION DESCRIPTION NOT PRINTING.       *
     H*                                                               *
     H* 09/08/92 LPK  PROJECT: WC172                                  *
     H*               EXPANDED GRAND TOTAL TO 13.2 NUMERIC FIELD.     *
     H*                                                               *
     H* 11/28/95 C:33 PROJECT: CM010                                  *
     H*               REMOVE 'POLICY PERIOD:  XX/XX/XX  -  XX/XX/XX'  *
     H*               FROM REPORT                                     *
     H*                                                               *
     H* 10/18/96 C:62 PROJECT: WC587                                  *
     H*               ADD RATE CODE TO GET PRDESC.                    *
     H*                                                               *
     H*****************************************************************
     H*
     F******BEG CHG*****04/20/09***************************************
     F******WMEPL4  IF  E           K        DISK
     FWMEPL4E   IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F***** END CHG **** 04/20/09 *************************************
     F******BEG ADD*****04/20/09***************************************
     FWMPXL1    IF   E           K DISK
     F***** END   ADD **** 04/20/09 ***********************************
     F***** BEGIN CHG **** 12/09/99 ***********************************
     F*****WMPRL1  IF  E           K        DISK
     FWMPRL     IF   E           K DISK
     F***** END   CHG **** 12/09/99 ***********************************
     FWDELL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE GROUP MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   POLICY YEAR KEY LIST
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C***** BEGIN DEL **** 12/09/99 ***********************************
     C**********           KFLD           XXRTCD
     C***** END   DEL **** 12/09/99 ***********************************
     C                   KFLD                    XXCLAS            5 0
     C                   KFLD                    XXPTDT
     C******BEG ADD*****04/20/09***************************************
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST              2
     C                   KFLD                    XXPTDT
     C*
     C     PXKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C*
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
     C***** END   ADD **** 04/20/09 ***********************************
     C*
     C     *LIKE         DEFINE    PRDT          XXPTDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQTYPE            1
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   CHAIN TO THE GROUP MASTER FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C*   CHAIN TO THE POLICY YEAR DETAIL FILE
     C*
     C     F1KEY         CHAIN     PF101                              60
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXSTPO            6 0
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXENPO            6 0
     C*
     C*   CHAIN TO THE COMPANY FILE.
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
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
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
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C*
     C     *IN60         IFEQ      '0'
     C     Q1USER        CHAIN     PUP01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C* BUILD THE TYPE OF REPORT HEADING
     C*
     C     QQTYPE        COMP      'Y'                                    31
     C*
     C*   PRINT THE REPORT HEADING.                                    D.
     C*
     C                   TIME                    DSDTTM
     C                   EXCEPT    HEAD
     C*
     C     EPKEY         CHAIN     PEP01                              50
     C*
     C*   SAVE THE KEY FIELDS FOR LATER COMPS.
     C*
     C                   MOVE      EPSPEC        XXSPEC            1
     C                   MOVE      EPCLAS        XXCLAS            5 0
     C******BEG ADD*****04/20/09***************************************
     C                   MOVEL     EPEXT         XXEXT             5
     C******END ADD*****04/20/09***************************************
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELPTDT        XXPTDT
     C                   MOVEL     ELRTCD        XXRTCD            3
     C     *IN50         DOWEQ     '0'
     C*
     C* END OF THE SPECIAL DESIGNATION OR CLASS.
     C*
     C     EPSPEC        IFNE      XXSPEC
     C     EPCLAS        ORNE      XXCLAS
     C******BEG ADD*****04/20/09***************************************
     C     EPEXT         ORNE      XXEXT
     C******END ADD*****04/20/09***************************************
     C                   EXSR      SUBCB
     C                   END                                                    CLASS BRK
     C*
     C*
     C*   IF TYPE OF REPORT IS 'Y' USE ESTMD P/R ELSE USE AUDITED.
     C*
     C     QQTYPE        IFEQ      'Y'
     C                   ADD       EPEAPR        XXAMT
     C                   ADD       EPEAPR        GTAMT
     C                   ELSE
      *** BEG ADD **** 02/21/02 **********************************************
     C******************** ADD  EPAAPR    XXAMT  112
     C******************** ADD  EPAAPR    GTAMT  132
     C                   ADD       EPAAPR        XXAMT            13 2
     C                   ADD       EPAAPR        GTAMT            15 2
      *** END ADD **** 02/21/02 **********************************************
     C                   END
     C*
     C*
     C     EPKEY         READE     PEP01                                  50
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELPTDT        XXPTDT
     C                   MOVEL     ELRTCD        XXRTCD            3
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C     *IN60         IFEQ      '0'
     C                   SETON                                        61
     C                   ENDIF
     C*
     C                   END                                                    *IN50
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C*
     C     *IN61         IFEQ      '0'
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
     C*   END OF JOB.
     C*
     C                   EXSR      SUBCB
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    TOTL
     C                   MOVE      '1'           *INLR
      ***** Beg Add ***** 08/26/14 ***********************************
      /Free
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
      ***** End Add ***** 08/26/14 ***********************************
     C*
     C*==================================================================*
     C*   Process special designation or   ** Subroutine **   S U B C B  *
     C*   class break                                                    *
     C*==================================================================*
     C*
     CSR   SUBCB         BEGSR
     C******BEG ADD*****04/20/09***************************************
     C                   MOVE      EPEXT         XXCODE           11
     C******END ADD*****04/20/09***************************************
     C******BEG CHG*****04/20/09***************************************
     C*************        MOVELXXCLAS    XXCODE  6
     C*************        MOVE XXSPEC    XXCODE
     C                   MOVEL     XXCLAS        X@CLAS            5
     C     X@CLAS        CAT       EPSPEC        XXCODE
     C******END CHG*****04/20/09***************************************
     C*
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '1'
     C     PRCLAS        ORNE      XXCLAS
     C                   MOVE      *BLANK        PRDESC
     C                   END
     C***  BEG   ADD  ***  04/20/09  *********************************
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       PRDESC
     C                   MOVE      *BLANKS       XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL     PXDESC        PRDESC
     C     RSKY          CHAIN     PRS01                              61
     C  N61              MOVE      RSJRST        XXST
     C  N61PXKY          SETGT     PPX01
     C  N61PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL     PXDESC        XXDESC
     C                   ENDIF                                                  EPEXT IFNE
     C     EPEXT         IFEQ      *BLANKS
     C                   MOVEL     PRDESC        XXDESC           30
     C                   ENDIF                                                  EPEXT IFEQ
     C***  END   ADD  ***  04/20/09  *********************************
     C*
     C***  BEG   DLT  ***  04/20/09  *********************************
     C**************       MOVELPRDESC    XXDESC 30
     C***  END   DLT  ***  04/20/09  *********************************
     C***  BEG   ADD  ***  04/20/09  *********************************
     C                   MOVE      XXCODE        XEXT              5
     C***  END   ADD  ***  04/20/09  *********************************
     C***  BEG   CHG  ***  04/20/09  *********************************
     C*************        MOVELXXCODE    XCODE   50
     C*************        MOVE XXCODE    XSPEC   1
     C                   MOVEL     XXCLAS        XCODE             5 0
     C                   MOVE      EPSPEC        XSPEC             1
     C***  END   CHG  ***  04/20/09  *********************************
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DETL
     C                   ADD       1             XXRCDS            5 0
     C                   Z-ADD     0             XXAMT
     C                   MOVE      EPCLAS        XXCLAS
     C                   MOVE      EPSPEC        XXSPEC
     C******BEG ADD*****04/20/09***************************************
     C                   MOVEL     EPEXT         XXEXT
     C******END ADD*****04/20/09***************************************
     C                   MOVE      ELPTDT        XXPTDT
     C                   MOVEL     ELRTCD        XXRTCD
     C*
     C                   ENDSR
     C**
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
     O*  MLS 1/18/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
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
     O*  MLS 1/18/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
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
     O**********E            ROUTE          3
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O*  MLS 1/18/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O*  SELECTION CRITERIA ADDED TO ROUTING PAGE
     O*
     O          E            ROUTE          2
     O                                           25 'SELECTION CRITERIA:'
     O          E            ROUTE          1
     O                                           24 'GROUP  . . . . . :'
     O                       FNFND         Z     28
     O                       FNNAME            +  2
     O          E            ROUTE          1
     O                                           24 'POLICY YEAR  . . :'
     O                       XXFYR         Z     28
     O          E            ROUTE          1
     O                                           24 'ESTIMATED PAYROLL:'
     O                       QQTYPE              26
     O*
     O*
     O*
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O****    E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *****END   CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              66
     O                       UDATE         Y     84
     O          E            HEAD           1
     O                                            6 'W7201R'
     O                                           80 'PAGE:'
     O                       PAGE          Z     84
     O                       ORTITL              70
     O          E            HEAD        1  1
     O                                           06 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              72
     O                     31                    84 ' ESTIMATED'
     O                    N31                    84 ' ACTUAL'
     O          E            HEAD           2
     O                                           06 'YEAR :'
     O                       XXFYR         Z     10
     O          E            HEAD           2
     O                                           17 'CODE'
     O***********BEG ADD*****04/20/09**********************************
     O                                           23 'EXT'
     O***********END ADD*****04/20/09**********************************
     O***********BEG CHG*****04/20/09**********************************
     O*****************                  37 'CLASSIFICATION'
     O                                           41 'CLASSIFICATION'
     O***********END CHG*****04/20/09**********************************
     O                                           77 'TOTAL PAYROLL'
     O          E            DETL           1
     O                       XCODE         Z     16
     O                       XSPEC               17
     O***********BEG ADD*****04/20/09**********************************
     O                       XEXT                25
     O*******                  XXDESC    53
     O                       XXDESC              57
     O***********END ADD*****04/20/09**********************************
     O                       XXAMT         B     79
     O          E            TOTL        2  2
     O                       XXRCDS        Z     30
     O                                           53 'G R A N D  T O T A L'
     O                       GTAMT         B     79
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC          1
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
