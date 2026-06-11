     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W4700R6 - DETAIL LISTING  REPORT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*      THIS PROGRAM WILL LIST ALL THE ACCOUNTS IN THE AUDIT     *
     H*   TRACKING MASTER FILE THAT MEET THE SELECTED CRITERIA.       *
     H*   THE DETAIL LISTING INFO IS RETREIVED FROM WMAVP WHICH IS    *
     H*   LOADED WHEN AN AUDIT REPORT(F21) IS RUN.                    *
     H*                                                               *
     H* CALLED FROM: W4700C6                                          *
     H* CALLS TO:    NONE
     H*                                                               *
     H*                                                               *
     H**  CREATED: 03/04/98  PROGRAMMER: C:87  PROJECT: WC630         *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*     OA -  OVERFLOW FOR PRINTER                                *
     H*     08 -  DEMAND MODE                                         *
     H*     09 -  BATCH  MODE                                         *
     H*     50 -  MAIN LOOP - DO WHILE                                *
     H*     60 -  GENERAL CHAIN INDICATOR                             *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*                  Retrieve brand Name                          *
     H*                                                               *
     H* 06/15/06  C:132 PROJECT: WC1412                               *
     H*                 Print Policy FYR, Effective & Expiration      *
     H*                 Dates on Report. Project started by 065, this *
     H*                 Program changes done by 132. No 05/22/06 code *
     H*                 changes, all code in program is all 06/15/06. *
     H*                                                               *
     H* 05/22/06  C:65  PROJECT: WC1412                               *
     H*                 PRINT POLICY FYR, EFFECTIVE, AND EXPIRATION   *
     H*                 DATES ON REPORT.                              *
     H*                                                               *
     H* 04/10/00  C:118 PROJECT: WC987                                *
     H*                 LOAD FUND ALTERNATE NAME FROM FUND MASTER     *
     H*                 IF AVAILABLE.                                 *
     H*                                                               *
     H* 04/19/99  C:118 PROJECT: WC630                                *
     H*                 Changed 'Fund' to 'Group', modified routing   *
     H*                 page.                                         *
     H*                                                               *
     H* 04/09/99  C:87  PROJECT: SUPP WC095 #17291                    *
     H*                 If no routing record existed, then the first  *
     H*                 page did not have page headings.  Moved over- *
     H*                 flow indicator processing to correct.         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMA2L     IF   E           K DISK
     FWMAVL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D TTL             S             40    DIM(7) CTDATA PERRCD(1)              TITLE
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  XXCOMM                 1    160
     D  AVCOM1                 1     40
     D  AVCOM2                41     80
     D  AVCOM3                81    120
     D  AVCOM4               121    160
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C****************************************************************
     C*      KEY LISTINGS
     C****************************************************************
     C*
     C*  Company Master file
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  Fund Master
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Audit Detail file key list
     C     A2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Audit Status Detail Listing
     C     AVKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSEL
     C*
     C*  User parameter file
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  Key list for the Override file
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C****************************************************************
     C*     *LIKE DEFINES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    AVCO#         XXCO#
     C     *LIKE         DEFINE    AVFND         XXFND
     C*
     C****************************************************************
     C*      *ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQSEL             1
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C*
     C                   MOVE      QQSEL         XXSEL             1 0
     C                   Z-ADD     XXSEL         X                 1 0
     C*
     C     QQMODE        IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*
     C****************************************************************
     C*   SET UP AND INITIALIZE                                 "
     C****************************************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C***** BEGIN CHG **** 04/19/99 ***********************************
     C**********           MOVE *BLANKS   XXTITL 40        REPORT TITLE
     C                   MOVE      *BLANKS       XXTITL           60            REPORT TITLE
     C***** END CHG ****** 04/19/99 ***********************************
     C*
     C                   Z-ADD     0             XXCNT             7 0          # OF RECDS PRINTED
     C*
     C*  Chain to Audit Detail for last run date and exp date range
     C     A2KY          CHAIN     PA201                              60
     C*
     C                   MOVE      A2RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRNDT            8 0
     C*
     C                   MOVE      A2FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   MOVE      A2TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C*
     C*  Chain to company file
     C     COKY          CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*  Chain to fund master file
     C     FNKY          CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*  Chain to the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  Get the distribution information
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C***** BEGIN ADD **** 04/19/99 ***********************************
     C*  Left justify title
     C*
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXTITL
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      XXTITL
     C                   ENDIF
     C                   ENDIF
     C***** END ADD ****** 04/19/99 ***********************************
     C*
     C*  Print the distribution information
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C***  BEGIN DEL  ***  04/09/99  **********************************
     C*******************  MOVE *ON       *INOA
     C***  END   DEL  ***  04/09/99  **********************************
     C                   ENDIF
     C*
     C***** BEGIN ADD **** 04/19/99 ***********************************
     C*   CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                             99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        XXTITL
     C*
     C***** END ADD ****** 04/19/99 ***********************************
     C***  BEGIN ADD  ***  04/09/99  **********************************
     C                   MOVE      *ON           *INOA
     C***  END   ADD  ***  04/09/99  **********************************
     C*
     C***** BEGIN DEL **** 04/19/99 ***********************************
     C**********           MOVELTTL,X     XXTITL
     C***** END DEL ****** 04/19/99 ***********************************
     C*
     C*  MAIN LOOP - Read Audit Status Detail Listing records for
     C*              the option passed (QQSEL)
     C*
     C     AVKY          SETLL     PAV01
     C     AVKY          READE     PAV01                                  50
     C     *IN50         DOWEQ     *OFF
     C*
     C                   ADD       1             XXCNT
     C*
     C                   Z-ADD     AVRQDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRQDT            8 0
     C*
     C                   Z-ADD     AVRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRCVD            8 0
     C*
      ***** BEG ADD ***** 06/15/06 ***************************************
      * New Policy From and To Effective Dates
     C                   Z-ADD     AVPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C                   Z-ADD     AVPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTDT            8 0
      ***** END ADD ***** 06/15/06 ***************************************
     C*
     C                   SELECT
     C*
     C*  Option 1 - Audit Received
     C     XXSEL         WHENEQ    1
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET1
     C*
     C*  Option 2 - Audit Completed
     C     XXSEL         WHENEQ    2
     C*
     C                   Z-ADD     AVPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPRDT            8 0
     C*
     C                   Z-ADD     AVMAIL        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXMAIL            8 0
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD2
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET2
     C*
     C*  Option 3 - Audit Outstanding
     C     XXSEL         WHENEQ    3
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD3
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET3
     C*
     C*  Option 4 - Audits In House to process
     C     XXSEL         WHENEQ    4
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD4
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET4
     C*
     C*  Option 5 - Non-productive accounts
     C     XXSEL         WHENEQ    5
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD5
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET5
     C*
     C*  Option 6 - Re-audits
     C     XXSEL         WHENEQ    6
     C*
     C                   Z-ADD     AVPRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPRDT
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD6
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET6
     C*
     C*  Option 7 - Audits contested
     C     XXSEL         WHENEQ    7
     C*
     C                   Z-ADD     AVCNTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCNTD            8 0
     C*
     C                   MOVEL     XXCOMM        XXCOM1           80
     C                   MOVE      XXCOMM        XXCOM2           80
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD7
     C                   MOVE      *OFF          *INOA
     C                   ENDIF
     C                   EXCEPT    DET7
     C*
     C                   ENDSL
     C*
     C     AVKY          READE     PAV01                                  50
     C                   ENDDO                                                  *IN50 = *OFF
     C*
     C                   EXCEPT    TOTAL
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
     C                   RETURN
     C*
     O******** BEGIN OUTPUT SPECS ****************************************
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
     O***** BEGIN ADD ******** 04/19/99 *******************************
     O**********        09   UPUSNM              67
     O***** END ADD ********** 04/19/99 *******************************
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
     O***** BEGIN ADD ******** 04/19/99 *******************************
     O                  09   UPUSNM              67
     O***** END ADD ********** 04/19/99 *******************************
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********               ORTITL    80
     O                       XXTITL              80
     O***** END CHG ********** 04/19/99 *******************************
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
     O          E            ROUTE          2 34
     O***** BEGIN ADD ******** 04/19/99 *******************************
     O          E            ROUTE          2
     O                                           24 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O***** END ADD ********** 04/19/99 *******************************
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********                          8 'COMPANY:'
     O**********               XXCO# Z   26
     O                                           18 'COMPANY:'
     O                       XXCO#         Z     24
     O          E            ROUTE          2
     O**********                          5 'FUND:'
     O                                           16 'GROUP:'
     O**********               XXFND Z   26
     O                       XXFND         Z     24
     O          E            ROUTE          2
     O**********                         22 'POLICY EXPIRATION DATE'
     O**********                         28 'RANGE:'
     O**********               XXFRDT    39 '  /  /    '
     O**********                         41 '-'
     O**********               XXTODT    52 '  /  /    '
     O          E            ROUTE          2
     O**********                          7 'REPORT:'
     O**********               TTL,X     50
     O                                           32 'POLICY EXPIRATION DATE'
     O                                           39 'RANGE:'
     O                       XXFRDT              51 '  /  /    '
     O                                           53 '-'
     O                       XXTODT              64 '  /  /    '
     O          E            ROUTE          2
     O                                           17 'REPORT:'
     O                       TTL(X)              61
     O********E  170           ROUTE
     O**********                         24 '(C) PROGRESSIVE DATA'
     O**********                         45 'SOLUTIONS, INC. 1991'
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS, INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O***** END CHG ********** 04/19/99 *******************************
     O*
     O*****************
     O*  MAIN HEADING
     O*****************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********               ORTITL    96
     O                       XXTITL              95
     O***** END CHG ********** 04/19/99 *******************************
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********               XXTITL    87
     O                       TTL(X)              87
     O***** END CHG ********** 04/19/99 *******************************
     O          E            HEAD           1
     O***** BEGIN CHG ******** 04/19/99 *******************************
     O**********                          5 'FUND:'
     O                                            6 'GROUP:'
     O**********               XXFND Z   09
     O**********               FNNAME    70
     O          E            HEAD           1
     O**********                         22 'POLICY EXPIRATION DATE'
     O**********                         28 'RANGE:'
     O**********               XXFRDT    39 '  /  /    '
     O**********                         41 '-'
     O**********               XXTODT    52 '  /  /    '
     O                       XXFND         Z     11
     O                       FNNAME              72
     O          E            HEAD           1
     O                                           22 'POLICY EXPIRATION DATE'
     O                                           29 'RANGE:'
     O                       XXFRDT              40 '  /  /    '
     O                                           42 '-'
     O                       XXTODT              53 '  /  /    '
     O***** END CHG ********** 04/19/99 *******************************
     O          E            HEAD           2
     O                                           14 'LAST RUN DATE:'
     O                       XXRNDT              25 '  /  /    '
     O*
     O********************************
     O*  OPTION 1 HEADING AND DETAIL
     O********************************
     O          E            HEAD1       1  1
      *
      ***** BEG CHG ***** 06/15/06 ***************************************
     O**********                         59 'AUDIT'
     O**********                         70 'DATE'
     O**********                         82 'DATE'
     O                                           57 'POL'
     O                                           67 'POLICY'
     O                                           78 'POLICY'
     O                                           87 'AUDIT'
     O                                           96 'DATE'
     O                                          108 'DATE'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            HEAD1          2
     O                                           10 'EMPLOYER'
     O                                           16 'DIV'
     O                                           24 'NAME'
      ***** BEG CHG ***** 06/15/06 ***************************************
     O************                       60 'COMPANY'
     O************                       72 'REQUESTED'
     O************                       84 'RECEIVED'
     O                                           57 'YR '
     O                                           69 'FROM DATE'
     O                                           79 'TO DATE'
     O                                           88 'COMPANY'
     O                                           99 'REQUESTED'
     O                                          110 'RECEIVED'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            DET1           1
     O                       AVEMP#        Z     10
     O                       AVDIV         Z     16
     O                       AVNAME              50
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********              AVACO     58
     O***********              XXRQDT    72 '  /  /    '
     O***********              XXRCVD    84 '  /  /    '
     O                       AVFYR               57
     O                       XXPFDT              69 '  /  /    '
     O                       XXPTDT              80 '  /  /    '
     O                       AVACO               86
     O                       XXRQDT              99 '  /  /    '
     O                       XXRCVD             111 '  /  /    '
      ***** END CHG ***** 06/15/06 ***************************************
     O*
     O********************************
     O*  OPTION 2 HEADING AND DETAIL
     O********************************
     O          E            HEAD2       1  1
      ***** BEG CHG ***** 06/15/06 ***************************************
     O*************                      59 'AUDIT'
     O*************                      70 'DATE'
     O*************                      82 'DATE'
     O*************                      97 'FINAL AUDIT'
     O*************                     109 'FINAL AUDIT'
     O                                           54 'POL'
     O                                           64 'POLICY'
     O                                           74 'POLICY'
     O                                           84 'AUDIT'
     O                                           93 'DATE'
     O                                          105 'DATE'
     O                                          120 'FINAL AUDIT'
     O                                          132 'FINAL AUDIT'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            HEAD2          2
     O                                           10 'EMPLOYER'
     O                                           16 'DIV'
     O                                           24 'NAME'
      ***** BEG CHG ***** 06/15/06 ***************************************
     O*************                      60 'COMPANY'
     O*************                      72 'REQUESTED'
     O*************                      84 'RECEIVED'
     O*************                      97 'PRINT DATE'
     O*************                     109 'MAIL  DATE'
     O                                           54 'YR '
     O                                           66 'FROM DATE'
     O                                           75 'TO DATE'
     O                                           85 'COMPANY'
     O                                           96 'REQUESTED'
     O                                          107 'RECEIVED'
     O                                          120 'PRINT DATE'
     O                                          131 'MAIL DATE'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            DET2           1
     O                       AVEMP#        Z     10
     O                       AVDIV         Z     16
     O                       AVNAME              50
      *************
      ***** BEG CHG ***** 06/15/06 ***************************************
     O*************            AVACO     58
     O*************            XXRQDT    72 '  /  /    '
     O*************            XXRCVD    84 '  /  /    '
     O*************            XXPRDT    97 '  /  /    '
     O*************            XXMAIL   109 '  /  /    '
     O                       AVFYR               54
     O                       XXPFDT              66 '  /  /    '
     O                       XXPTDT              77 '  /  /    '
     O                       AVACO               83
     O                       XXRQDT              96 '  /  /    '
     O                       XXRCVD             108 '  /  /    '
     O                       XXPRDT             120 '  /  /    '
     O                       XXMAIL             132 '  /  /    '
      ***** END CHG ***** 06/15/06 ***************************************
     O*
     O********************************
     O*  OPTION 3 HEADING AND DETAIL
     O********************************
     O          E            HEAD3       1  1
      ***** BEG CHG ***** 06/15/06 ***************************************
     O************                       59 'AUDIT'
     O************                       70 'DATE'
     O                                           57 'POL'
     O                                           68 'POLICY'
     O                                           78 'POLICY'
     O                                           87 'AUDIT'
     O                                           96 'DATE'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            HEAD3          2
     O                                           10 'EMPLOYER'
     O                                           16 'DIV'
     O                                           24 'NAME'
      ***** BEG CHG ***** 06/15/06 ***************************************
     O************                       60 'COMPANY'
     O************                       72 'REQUESTED'
     O                                           57 'YR '
     O                                           69 'FROM DATE'
     O                                           79 'TO DATE'
     O                                           88 'COMPANY'
     O                                           99 'REQUESTED'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            DET3           1
     O                       AVEMP#        Z     10
     O                       AVDIV         Z     16
     O                       AVNAME              50
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********              AVACO     58
     O***********              XXRQDT    72 '  /  /    '
     O                       AVFYR               57
     O                       XXPFDT              69 '  /  /    '
     O                       XXPTDT              80 '  /  /    '
     O                       AVACO               86
     O                       XXRQDT              99 '  /  /    '
      ***** END CHG ***** 06/15/06 ***************************************
     O*
     O********************************
     O*  OPTION 4 HEADING AND DETAIL
     O********************************
     O          E            HEAD4       1  1
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********                        58 'AUDIT'
     O***********                        70 'DATE'
     O***********                        82 'DATE'
     O                                           57 'POL'
     O                                           68 'POLICY'
     O                                           78 'POLICY'
     O                                           87 'AUDIT'
     O                                           96 'DATE'
     O                                          108 'DATE'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            HEAD4          2
     O                                           10 'EMPLOYER'
     O                                           16 'DIV'
     O                                           24 'NAME'
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********                        59 'COMPANY'
     O***********                        72 'REQUESTED'
     O***********                        84 'RECEIVED'
     O                                           57 'YR '
     O                                           69 'FROM DATE'
     O                                           79 'TO DATE'
     O                                           88 'COMPANY'
     O                                           99 'REQUESTED'
     O                                          110 'RECEIVED'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            DET4           1
     O                       AVEMP#        Z     10
     O                       AVDIV         Z     16
     O                       AVNAME              50
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********              AVACO     58
     O***********              XXRQDT    72 '  /  /    '
     O***********              XXRCVD    84 '  /  /    '
     O                       AVFYR               57
     O                       XXPFDT              69 '  /  /    '
     O                       XXPTDT              80 '  /  /    '
     O                       AVACO               86
     O                       XXRQDT              99 '  /  /    '
     O                       XXRCVD             111 '  /  /    '
      ***** END CHG ***** 06/15/06 ***************************************
     O*
     O********************************
     O*  OPTION 5 HEADING AND DETAIL
     O********************************
     O          E            HEAD5       1  1
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********                        59 'AUDIT'
     O***********                        70 'DATE'
     O***********                        82 'NUMBER OF'
     O                                           57 'POL'
     O                                           68 'POLICY'
     O                                           78 'POLICY'
     O                                           87 'AUDIT'
     O                                           96 'DATE'
     O                                          112 'NUMBER OF'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            HEAD5          2
     O                                            8 'EMPLOYER'
     O                                           14 'NAME'
      ***********
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********                        57 'COMPANY'
     O***********                        72 'REQUESTED'
     O***********                        82 'ATTEMPTS'
     O                                           57 'YR '
     O                                           69 'FROM DATE'
     O                                           79 'TO DATE'
     O                                           88 'COMPANY'
     O                                           99 'REQUESTED'
     O                                          111 'ATTEMPTS'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            DET5           1
     O                       AVEMP#        Z     10
     O                       AVDIV         Z     16
     O                       AVNAME              50
      ***** BEG CHG ***** 06/15/06 ***************************************
     O*********                AVACO     58
     O*********                XXRQDT    72 '  /  /    '
     O*********                AVATTM2   82
     O                       AVFYR               57
     O                       XXPFDT              69 '  /  /    '
     O                       XXPTDT              80 '  /  /    '
     O                       AVACO               86
     O                       XXRQDT              99 '  /  /    '
     O                       AVATTM        2    109
      ***** END CHG ***** 06/15/06 ***************************************
     O*
     O********************************
     O*  OPTION 6 HEADING AND DETAIL
     O********************************
     O          E            HEAD6       1  1
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********                        59 'AUDIT'
     O***********                        70 'DATE'
     O***********                        82 'DATE'
     O***********                        95 'REVISED'
     O                                           57 'POL'
     O                                           68 'POLICY'
     O                                           78 'POLICY'
     O                                           87 'AUDIT'
     O                                           97 'DATE'
     O                                          109 'DATE'
     O                                          122 'REVISED'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            HEAD6          2
     O                                           10 'EMPLOYER'
     O                                           16 'DIV'
     O                                           24 'NAME'
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********                        60 'COMPANY'
     O***********                        72 'REQUESTED'
     O***********                        84 'RECEIVED'
     O***********                        96 'PRINT DATE'
     O                                           57 'YR '
     O                                           69 'FROM DATE'
     O                                           79 'TO DATE'
     O                                           88 'COMPANY'
     O                                          100 'REQUESTED'
     O                                          111 'RECEIVED'
     O                                          124 'PRINT DATE'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            DET6           1
     O                       AVEMP#        Z     10
     O                       AVDIV         Z     16
     O                       AVNAME              50
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********              AVACO     58
     O***********              XXRQDT    72 '  /  /    '
     O***********              XXRCVD    84 '  /  /    '
     O***********              XXPRDT    96 '  /  /    '
     O                       AVFYR               57
     O                       XXPFDT              69 '  /  /    '
     O                       XXPTDT              80 '  /  /    '
     O                       AVACO               86
     O                       XXRQDT             100 '  /  /    '
     O                       XXRCVD             112 '  /  /    '
     O                       XXPRDT             124 '  /  /    '
      ***** END CHG ***** 06/15/06 ***************************************
     O*
     O********************************
     O*  OPTION 7 HEADING AND DETAIL
     O********************************
     O          E            HEAD7       1  1
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********                        58 'AUDIT'
     O***********                        70 'DATE'
     O***********                        82 'DATE'
     O***********                        94 'DATE'
     O***********                       103 'RE-'
     O                                           57 'POL'
     O                                           68 'POLICY'
     O                                           78 'POLICY'
     O                                           87 'AUDIT'
     O                                           96 'DATE'
     O                                          108 'DATE'
     O                                          120 'DATE'
     O                                          129 'RE-'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            HEAD7          1
     O                                           10 'EMPLOYER'
     O                                           16 'DIV'
     O                                           24 'NAME'
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********                        59 'COMPANY'
     O***********                        72 'REQUESTED'
     O***********                        84 'RECEIVED'
     O***********                        96 'CONTESTED'
     O***********                       106 'ASSIGNED'
     O                                           57 'YR '
     O                                           69 'FROM DATE'
     O                                           79 'TO DATE'
     O                                           88 'COMPANY'
     O                                           99 'REQUESTED'
     O                                          110 'RECEIVED'
     O                                          122 'CONTESTED'
     O                                          132 'ASSIGNED'
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            HEAD7          2
     O                                           10 'COMMENTS:'
     O          E            DET7           1
     O                       AVEMP#        Z     10
     O                       AVDIV         Z     16
     O                       AVNAME              50
      ***********
      ***** BEG CHG ***** 06/15/06 ***************************************
     O***********              AVACO     58
     O***********              XXRQDT    72 '  /  /    '
     O***********              XXRCVD    84 '  /  /    '
     O***********              XXCNTD    96 '  /  /    '
     O***********              AVRSGN   102
     O                       AVFYR               57
     O                       XXPFDT              69 '  /  /    '
     O                       XXPTDT              80 '  /  /    '
     O                       AVACO               86
     O                       XXRQDT              99 '  /  /    '
     O                       XXRCVD             111 '  /  /    '
     O                       XXCNTD             122 '  /  /    '
     O                       AVRSGN             128
      ***** END CHG ***** 06/15/06 ***************************************
     O          E            DET7           1
     O                       XXCOM1             112
     O          E            DET7           2
     O                       XXCOM2             112
     O*
     O********************************
     O*  TOTALS
     O********************************
     O          E            TOTAL       2  2
     O                                           35 'TOTAL NUMBER OF'
     O                                           53 'RECORDS SELECTED:'
     O                       XXCNT         K     64
     O*
**   TTL
    LISTING OF ALL AUDITS RECEIVED
    LISTING OF ALL AUDITS COMPLETED
   LISTING OF ALL AUDITS OUTSTANDING
  LISTING OF ALL AUDITS STILL INHOUSE
  LISTING OF ALL NON-PRODUCTIVE AUDITS
       LISTING OF ALL RE-AUDITS
    LISTING OF ALL CONTESTED AUDITS
