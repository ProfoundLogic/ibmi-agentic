     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W2103R - CLEAR BILLING REQUEST FILE
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*      THIS PROGRAM WILL DELETE ALL BILLING REQUEST RECORDS FOR *
     H*      THE FUND YEAR ENTERED ON THE PROMPT.                     *
     H*                                                               *
     H*  CALLED FROM:  W2103C - OVERRIDE                              *
     H*  CALLS TO:     NONE                                           *
     H*                                                               *
     H*  PROGRAMMER: C:24                                             *
     H*  CREATED:    04/06/95                PROJECT:WC407            *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H*     OF -  OVERFLOW FOR PRINTER                                *
     H*     08 -  DEMAND MODE                                         *
     H*     09 -  BATCH  MODE                                         *
     H*     60 -  GENERAL READS/CHAINS                                *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   SMORL   POR01: PGM/PRT                      Output Override *
     H*   WMFNL   PFN01: CO/FND                           Fund Master *
     H*   WABLL   PBL01: CO/FND/FYR/EMP/DIV           Billing Request *
     H*   WMEML   PEM01: CO/FND/EMP#/DIV              Employer Master *
     H*****************************************************************
     H* REVISIONS:                                                    *
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
     H* 10/09/03 C:61  PROJECT WC1150                                 *
     H*                Select by Audited / Estimated / Both.          *
     H*                                                               *
     H*  01/22/96 C:48 PROJECT WC407                                  *
     H*                REMOVE DIVISION ENTRY OFF THE SCREEN AND REMOVE*
     H*                SELECTION CRITERIA ON DIV.                     *
     H*                                                               *
     H*  11/21/96 C:65 PROJECT: CM023                                 *
     H*                PROCESS NOW RUNS OVER A POLICY EFFECTIVE DATE  *
     H*                RANGE INSTEAD OF A FYR.                        *
     H*****************************************************************
     H*                                                               *
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWABLL5    UF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C****************************************************************
     C*      KEY LISTINGS
     C****************************************************************
     C*
     C*    USER PARAMETERS FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*    COMPANY MASTER FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*    FUND MASTER
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   BILLING REQUEST FILE
     C*
     C     BLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     BLKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEFDT
     C*
     C*   EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    BLEMP#
     C*
     C****************************************************************
     C*     *LIKE DEFINES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    BLCO#         XXCO#
     C     *LIKE         DEFINE    BLFND         XXFND
     C     *LIKE         DEFINE    BLDT          XXEFDT
     C     *LIKE         DEFINE    BLDT          XXTODT
     C*
     C                   MOVE      *ZEROS        XXCNT             7 0          EMPLOYER COUNT
     C*
     C****************************************************************
     C*      *ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
      ***** BEG ADD ******* 10/09/03 **********************************
     C                   PARM                    QQEAB             1
      ***** END ADD ******* 10/09/03 **********************************
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEFDT        XXDTF             8 0
     C                   MOVE      QQTODT        XXDTT             8 0
     C*
     C                   Z-ADD     XXDTF         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C                   Z-ADD     XXEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DTF             6 0
     C*
     C                   Z-ADD     XXDTT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1DTT             6 0
     C*
     C****************************************************************
     C*   SET UP AND INITIALIZE
     C****************************************************************
     C*
     C                   TIME                    DSDTTM
     C     QQMODE        IFNE      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   GET COMPANY, FUND, AND USER INFORMATION
     C*
     C     COKY          CHAIN     PCO01                              60
     C     FNKY          CHAIN     PFN01                              60
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION & PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C                   MOVE      '1'           *INOF
     C****************************************************************
     C*     BEGIN MAIN LINE PROGRAM
     C****************************************************************
     C*
     C*  DELETE ALL BILLING REQUESTS FOR THE POLICY DATE RANGE.
     C*
     C     BLKY2         SETLL     PBL01
     C     BLKY          READE     PBL01                                  60
     C     *IN60         DOWEQ     '0'
     C     BLPFDT        IFGE      XXEFDT
     C     BLPFDT        ANDLE     XXTODT
      ***** BEG ADD ******* 10/09/03 **********************************
      *
     C                   SELECT
      *
      *  Both Audited and Estimated
      *
     C     QQEAB         WHENEQ    *BLANK
     C                   EXSR      SRDLT
      *
      *  Audited
      *
     C     QQEAB         WHENEQ    'A'
     C     BLFLG2        ANDEQ     'Y'
     C                   EXSR      SRDLT
      *
      *  Estimated
      *
     C     QQEAB         WHENEQ    'E'
     C     BLFLG2        ANDNE     'Y'
     C                   EXSR      SRDLT
      *
     C                   ENDSL
      *
      ***** END ADD ******* 10/09/03 **********************************
      ***** BEG DEL ******* 10/09/03 **********************************
     C*********
     C***GET*EMPLOYER NAME & PRINT EDIT REPORT DETAIL BEFORE DELETING
     C*********
     C*********            MOVE *BLANK    EMNAME
     C*********  EMKY      CHAINPEM01                60
     C***OF****            EXCPTHEAD
     C***OF****            MOVE *OFF      *INOF
     C*********            EXCPTDETAIL
     C*********            ADD  1         XXCNT
     C*********
     C*********            DELETPBL01
      ***** END DEL ******* 10/09/03 **********************************
      *
     C                   ENDIF                                                  EFDT <= BLDT <= TODT
     C     BLKY          READE     PBL01                                  60
     C     BLPFDT        IFGT      XXTODT
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  BLDT > TODT
     C                   END                                                    *IN60
     C*
     C*  PRINT TOTAL OR MESSAGE IF NO DELETION OCCURRED
     C*
     C     XXCNT         IFEQ      *ZERO
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ELSE
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOTAL
     C                   END
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
      *
      *
      *
      ***** BEG ADD ******* 10/09/03 **********************************
      *===============================================================
      *  SRDLT - Process deletion
      *===============================================================
     C     SRDLT         BEGSR
      *
      *
      *  GET EMPLOYER NAME & PRINT EDIT REPORT DETAIL BEFORE DELETING
      *
     C                   MOVE      *BLANK        EMNAME
     C     EMKY          CHAIN     PEM01                              60
     C   OF              EXCEPT    HEAD
     C   OF              MOVE      *OFF          *INOF
     C                   EXCEPT    DETAIL
     C                   ADD       1             XXCNT
      *
     C                   DELETE    PBL01
      *
     C                   ENDSR                                                  SRDLT
      *
      *
      *
      ***** END ADD ******* 10/09/03 **********************************
      *
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2 34
     O                                            8 'COMPANY:'
     O                       XXCO#         Z     26
     O          E            ROUTE          2
     O                                            5 'FUND:'
     O                       XXFND         Z     26
     O          E            ROUTE          2
     O                                           11 'START DATE:'
     O                       X1DTF               26 '  /  /  '
     O          E            ROUTE          2
     O                                           09 'END DATE:'
     O                       X1DTT               26 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  159           ROUTE
     O***                                24 '(C) PROGRESSIVE DATA'
     O***                                45 'SOLUTIONS, INC. 1995'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ***** END CHG ****** 01/28/10 ***********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              10 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    130
     O          E            HEAD           1
     O                       Q1PGM               12
     O                                           51 'BILLING REQUESTS DELETED'
     O                                           76 'FROM FILE FOR POLICY DAT'
     O                                           79 'ES:'
     O                       X1DTF               88 '  /  /  '
     O                                           90 '-'
     O                       X1DTT               99 '  /  /  '
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD        1  1
     O                                            5 'FUND:'
     O                       XXFND         Z     09
     O                       FNNAME              70
     O*
     O          E            HEAD        1  2
     O*
     O                                           70 'BILLING FLAG'
     O                                           88 'UPDATE A/R FLAG'
     O                                          101 'AUDIT FLAG'
     O          E            DETAIL         1
     O                       BLEMP#        Z     13
     O                       EMNAME              60
     O                       BLFLG1              69
     O                       BLFLG3              87
     O                       BLFLG2             100
     O*
     O          E            TOTAL          1
     O                                           17 'TOTAL NUMBER OF'
     O                                           28 'EMPLOYERS:'
     O                       XXCNT         1     38
     O*
     O          E            NOREC       3  1
     O                                           28 '*** NO RECORD MEETS THE'
     O                                           51 'SELECTED CRITERIA.  NO'
     O                                           75 'CLEARANCE OCCURRED. ***'
