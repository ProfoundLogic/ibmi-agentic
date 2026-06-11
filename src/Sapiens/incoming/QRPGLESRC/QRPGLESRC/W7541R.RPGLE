     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W7541R - MONIES PAID TO SELECTED PAYEE REPORT
     H*****************************************************************
     H* REVISIONS                                                     *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 03/11/14 C:282 PROJECT 129WSC Priss 28                        *
     H*                Aligning the 'Heading for Legal Reserve'       *
     H*                with the other reserves.                       *
     H*                                                               *
     H* 11/25/13 C:282 PROJECT 129WSC                                 *
     H*                Replace legal literal with control field       *
     H*                for cltid 129 only.                            *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/01/00  C:15   Support: WC065/24856                         *
     H*                  Changed pgm to use QQMOD to determine if the *
     H*                  report is to be run for one or all modifiers.*
     H*                                                               *
     H* 04/10/00  C:118  PROJECT: WC987                               *
     H*                  LOAD ALTERNATE COMPANY NAME FROM FUND MASTER *
     H*                                                               *
     H* 12/15/99  C:118  PROJECT: WC577                               *
     H*                  Modify output specs so page headings will    *
     H*                  print completely.                            *
     H*                                                               *
     H*****************************************************************
     F*
     F* Company Master File
     F*
     FSMCOL     IF   E           K DISK
     F*
     F* Override File
     F*
     FSMORL     IF   E           K DISK
     F*
     F* User Parameters File
     F*
     FWMUPL     IF   E           K DISK
     F*
     F* Fund Master File
     F*
     FWMFNL     IF   E           K DISK
     F*
     F* Claims Check Payment File
     F*
     FWDPAL4    IF   E           K DISK
     F*
      *** Beg Add ***  11/25/13  ******************************************************
     FWMCXL     IF   E           K DISK
      *** End Add ***  11/25/13  ***********************************************HEADING
     F* Printer File
     F*
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)
     F*

     D*****************************************************************
     D*                      Input Specs                              *
     D*****************************************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Beg Add ***  11/25/13  ******************************************************
     D charLGRH        s             10                                         LEGAL RESERVE HEAD
     D CltId           s              3
      *
     d RtvCltID        pr                  ExtPgm('RTVCLTID')
     d cltID                          3    const
      *** End Add ***  11/25/13  ***********************************************HEADING
      *** Beg Add ***  03/11/14  ******************************************************
     D/copy sprString
      *** End Add ***  03/11/14  ******************************************************

     C*
     C*****************************************************************
     C*                      Calculation Specs                        *
     C*****************************************************************
     C*
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
     C*   Key List for the Fund File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*   Key List for the Claims Check Payment File
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *** Beg Add ***  11/25/13  ******************************************************
     C*   Claim/DARTS Control Definition File
     C*
     C     keyWMcxl      KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C     *LIKE         DEFINE    PADT          XXFDT
     C     *LIKE         DEFINE    PADT          XXTDT
     C*
     C*  Entry Parameters List
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQFDT             8
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*   Set Up and Initialize
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       RtvCltID(CltId);
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFEIN        XXFEIN            9 0
     C*** BEGIN ADD *** 11/01/00 **************************************
     C     QQMOD         IFNE      *BLANKS
     C*** END ADD ***** 11/01/00 **************************************
     C                   MOVE      QQMOD         XXMOD             3 0
     C*** BEGIN ADD *** 11/01/00 **************************************
     C                   ENDIF
     C*** END ADD ***** 11/01/00 **************************************
     C                   MOVE      QQFDT         X1FDT             8 0
     C*
     C                   Z-ADD     X1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDT
     C*
     C                   MOVE      QQFDT         Z1FDT             8 0
     C*
     C                   Z-ADD     Z1FDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         ZZFDT             6 0
     C*
     C                   MOVE      QQTDT         X1TDT             8 0
     C*
     C                   Z-ADD     X1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT
     C*
     C                   MOVE      QQTDT         Z1TDT             8 0
     C*
     C                   Z-ADD     Z1TDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         ZZTDT             6 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     0             XXCOMP           11 2
     C                   Z-ADD     0             XXMED            11 2
     C                   Z-ADD     0             XXLEG            11 2
     C                   Z-ADD     0             XXEXP            11 2
     C                   Z-ADD     0             XXTTL            11 2
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
     C*
     C*   Get the Routing Record
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C*
     C***** BEGIN ADD **** 12/15/99 ***********************************
     C* LEFT ALIGN TITLE FOR ROUTING PAGE
     C*
     C                   MOVE(P)   ORTITL        XXTITL           60
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF                                                  N<=60
     C                   ENDIF                                                  ORTITL<>BLKS
     C*
     C***** END   ADD **** 12/15/99 ***********************************
     C*   Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C*   Print the Distribution Information
     C*
      ***** Beg Del ***** 08/26/14 **************************************************
     C**********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 **************************************************
     C*
     C*  Get the Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C   60              MOVE      *BLANK        FNNAME
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*** Beg Add ***  11/25/13  ******************************************************
      /Free
       charLGRH = *Blanks;
       *IN71 = *Off;
       If CltId = '129';
          Chain keyWMcxl pcx01;
          If %Found(wmcxl);
             If CXlgrh <> *Blanks;
                charLGRH = CXlgrh;
             Else;
                charLGRH = 'LEGAL';
             EndIf;
       // *** Beg Add ***  03/11/14  **************************************
             charLGRH = RightJstfy(charLGRH:10);
       // *** End Add ***  03/11/14  ***************************************
             *IN71 = *On;
          Endif;
       EndIf;
      /End-Free
     C*** End Add ***  11/25/13  ******************************************************
     C*
     C*   Check the location modifier; if modifier is equal to 0
     C*   then process all the payments made to that provider
     C*   regardless of the location.  If location modifier is not
     C*   equal to 0 then process only payments made to the provider
     C*   at that specfic location.
     C*
     C*** BEGIN CHG *** 11/01/00 **************************************
     C********** XXMOD     IFNE 0
     C     QQMOD         IFNE      *BLANKS
     C*** END CHG ***** 11/01/00 **************************************
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C*
     C*   Read the Claims Check Payment File
     C*
     C     PAKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   Check to be sure record is within the date range
     C*
     C     PATRDT        IFGE      XXFDT
     C     PATRDT        ANDLE     XXTDT
     C*
     C*   If a specific location is chosen, test to make sure
     C*   we have the correct record.
     C*
     C     *IN20         IFEQ      '1'
     C     PAMOD         IFNE      XXMOD
     C                   GOTO      NEXT
     C                   END
     C                   END
     C*
     C*   Add the Amount to the Appropiate Buckets
     C*
     C                   Z-ADD     0             XXAMT            11 2
     C*
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'Z'
     C     PATYPE        OREQ      'A'
     C                   ADD       PAAMT         XXAMT
     C                   ELSE
     C                   SUB       PAAMT         XXAMT
     C                   END
     C*
     C     PARETP        IFEQ      'C'                                          COMPENSATION
     C                   ADD       XXAMT         XXCOMP
     C                   END
     C*
     C     PARETP        IFEQ      'M'                                          MEDICAL
     C                   ADD       XXAMT         XXMED
     C                   END
     C*
     C     PARETP        IFEQ      'L'                                          LEGAL
     C                   ADD       XXAMT         XXLEG
     C                   END
     C*
     C     PARETP        IFEQ      'E'                                          EXPENSE
     C                   ADD       XXAMT         XXEXP
     C                   END
     C*
     C*
     C*   End date range check
     C*
     C                   END
     C*
     C*   Get the next Claims Check Payment Record
     C*
     C     NEXT          TAG
     C*
     C     PAKEY         READE     PPA01                                  60
     C*
     C*   End Do-While
     C*
     C                   END
     C*
     C*   Print heading
     C*
     C                   EXCEPT    HEAD
     C*
     C*   Add buckets to total
     C*
     C                   ADD       XXCOMP        XXTTL
     C                   ADD       XXMED         XXTTL
     C                   ADD       XXLEG         XXTTL
     C                   ADD       XXEXP         XXTTL
     C*
     C*   Print the Total Line
     C*
     C                   EXCEPT    DETL
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Chain upKey PUP01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C                   SETON                                        LR
     C*
     O*
     O*****************************************************************
     O*                        Output Specs                           *
     O*****************************************************************
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 **************************************************
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
      ***** End Del ***** 08/26/14 **************************************************
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
      ***** Beg Del ***** 08/26/14 **************************************************
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
      ***** End Del ***** 08/26/14 **************************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O***** BEGIN CHG **** 12/15/99 ***********************************
     O**********               DSTIME    08 '  :  :  '
     O                       DSTIME               9 '  :  :  '
     O                       CONAME              62
     O**********               DSDATEY   82
     O                       DSDATE        Y     81
     O          E            HEAD           2
     O**********               Q1PGM     10
     O                       Q1PGM               11
     O**********               ORTITL    72
     O                       XXTITL              72
     O**********                         78 'PAGE:'
     O                                           77 'PAGE:'
     O**********               PAGE  Z
     O                       PAGE          Z     81
     O***** END   CHG **** 12/15/99 ***********************************
     O          E            HEAD           2
     O                                           15 'GROUP:'
     O                       XXFND         Z     19
     O                       FNNAME              80
     O          E            HEAD           2
     O                                           33 'FROM DATE:'
     O                       ZZFDT         Y     42
     O                                           53 'TO DATE:'
     O                       ZZTDT         Y     62
     O          E            HEAD           2
     O                                            9 'FEIN#:'
     O                       XXFEIN        Z     19
     O                                           39 'LOCATION MODIFIER:'
     O                       XXMOD         Z     43
     O                       XXMOD         Z     41
     O          E            HEAD           2
     O                                           16 'COMPENSATION'
     O                                           32 'MEDICAL'
      *** Beg Add ***  11/25/13  ******************************************************
     O*****                                      48 'LEGAL'
     O              N71                          48 'LEGAL'
     O               71      charLGRH            48
      *** End Add ***  11/25/13  ******************************************************
     O                                           64 'EXPENSE'
     O                                           81 'TOTAL'
     O          E            DETL           1
     O                       XXCOMP        J     17
     O                       XXMED         J     33
     O                       XXLEG         J     49
     O                       XXEXP         J     65
     O                       XXTTL         J     82
