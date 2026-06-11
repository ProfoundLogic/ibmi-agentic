     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7455R - REPORT - Agent Account Current Deposit Slip
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION:                                         *
     H*     Report lists agency payments received within a given      *
     H*     date range.  It will exclude transactions if the deposit  *
     H*     flag assigned to the A/R transaction table value          *
     H*     indicates to do so.                                       *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     W7455C - OVERRIDE                                         *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*     NONE                                                      *
     H*                                                               *
     H*  04/09/99        PROGRAMMER C:61       PROJECT 96WAD          *
     H*  07/19/99        IN BASE    C:33       PROJECT WC899          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
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
     H*  05/03/06  C:132 PROJECT: WA089 - 52435                       *
     H*                  Change to blank Agent Name if do not find    *
     H*                  Agent in Agent Assignment file.              *
     H*                                                               *
     H*  09/13/00  C:33  PROJECT: WC899                               *
     H*                  ADD DATE RANGE TO HEADER ON GRAND TOTAL PAGE *
     H*                                                               *
     H*  07/17/00  C:33  SEQ#: WA096/23562                            *
     H*                  ALLOW FOR ALL PAYMENT TYPES                  *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  91       Read Work File                                      *
     H*  92       Last record processing                              *
     H*****************************************************************
      *
      * System Company
     FSMCOL     IF   E           K DISK
      *
      * System Output Routing
     FSMORL     IF   E           K DISK
      *
      * User Parameter
     FWMUPL     IF   E           K DISK
      *
      * Agency Billing Detail
     FWDABL3    IF   E           K DISK
      *
      * Agent Assignment File
     FWMAAL1    IF   E           K DISK
      *
      * Agent Master File
     FWMAGL     IF   E           K DISK
      *
      * Table File  (Transaction Type Code)
     FWTABL     IF   E           K DISK
      *
      *
     FQSYSPRT   O    F   98        PRINTER OFLIND(*INOA)
      *

     D XTITL           C                   CONST('AGENT ACCOUNT CURRE-
     D                                     NT DEPOSIT SLIP')
      ***** BEG ADD ***** 05/03/06 **************************************
     D AGTERR          C                   CONST('**** AGENT NOT FOUN-
     D                                     D - ERROR ****')
      ***** END ADD ***** 05/03/06 **************************************
      *
     D                 DS
     D DSDTTM                  1     14  0
     D DSTIME                  1      6  0
     D DSDATE                  7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D/COPY SCOPYDTPL
      *
      *   User Parameter
      *

     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *   Output Routing
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
      *
      *   Company
      *
     C     COKY          KLIST
     C                   KFLD                    KKCO#
      *
      *   Agency Billing Detail
      *
     C     ABKY1         KLIST
     C                   KFLD                    KKCO#
      *
      *  Agent Assignment
      *
     C     AAKY          KLIST
     C                   KFLD                    ABCO#
     C                   KFLD                    ABFND
     C                   KFLD                    ABAGT#
      *
      *  Agent Master File
      *
     C     AGKY          KLIST
     C                   KFLD                    ABCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
      *
      *  Transaction Type
      *
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
      *
      *   *LIKE Define Variables
      *
     C     *LIKE         DEFINE    ABCO#         KKCO#
     C     *LIKE         DEFINE    ABTRDT        KKTRDT
     C     *LIKE         DEFINE    ABAGT#        KKAGT#
      *
     C     *LIKE         DEFINE    ABTRDT        X7FRDT
     C     *LIKE         DEFINE    ABTRDT        X7TODT
     C     *LIKE         DEFINE    ABTRDT        SVTRDT
      *
     C     *LIKE         DEFINE    AGNAME        X1AGNM
     C     *LIKE         DEFINE    TTKEY         KKKEY
      *
     C*****/COPY SCOPYDTPL
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FRDT            8
     C                   PARM                    @@TODT            8
     C                   PARM                    @@QUE            10
      *
      *
      *   Initialize
      *
     C                   TIME                    DSDTTM
     C                   MOVE      '1'           *INOA
     C                   MOVE      *ALL'_'       LINE             98
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
      *  PGMSTR executes first
      *
     C                   EXSR      PGMSTR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *   Print Routing Page
      *
     C                   EXSR      SRRTE
      *
      *  Read 1st record
      *
     C                   EXSR      READ1
      *
      *  Initialize break criteria
      *
     C                   EXSR      IZL1BK
      *
     C     *IN91         DOWEQ     '0'
      *
      *  If subset criteria met
      *
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
      *
     C     MEETSS        IFEQ      *ON
      *
      *  L1 Break test
      *
     C                   MOVE      *ON           MEETL1            1
     C                   EXSR      L1TST
      *
      *  L1 Break action
      *
     C     MEETL1        IFEQ      *OFF
      *
     C     L1CNT         IFNE      *ZERO
     C                   EXSR      L1BK
     C                   ENDIF                                                  L1CNT
      *
     C                   EXSR      IZL1BK
     C                   ENDIF                                                  MEETL1
      *
      *  Write detail record
      *
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
      *
      *  Accumulate Totals
      *
     C                   EXSR      ACCTOT
      *
     C                   ENDIF                                                  MEETSS
      *
      *  Read next record
      *
     C                   EXSR      READN
      *
     C                   ENDDO                                                  WRK RECORD
      *
      *  Last Record processing
      *
     C                   EXSR      LRBK
      *
      *  End of Processing
      *
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
      *
      *
      *****************************************************************
      *
      *
      *
      *===============================================================
      * PGMSTR - Set-up and Initialize
      *===============================================================
      *
     CSR   PGMSTR        BEGSR
      *
      *  Entry parm processing
      *
     C                   MOVE      @@CO#         KKCO#
      *
     C                   MOVE      @@FRDT        X8FRDT            8 0
     C                   MOVE      @@TODT        X8TODT            8 0
      *
     C                   Z-ADD     X8FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7FRDT
      *
     C                   Z-ADD     X8TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7TODT
      *
     C     Q1PRMS        IFGE      4
     C     @@QUE         ANDNE     *BLANK
     C                   MOVE      *ON           *IN08
     C                   ELSE
     C                   MOVE      *ON           *IN09
     C                   ENDIF                                                     Q1PRMS>=4
      *
     C                   ENDSR                                                  PGMSTR
      *
      *===============================================================
      * READ1 - Read first record from file
      *===============================================================
      *
     CSR   READ1         BEGSR
      *
     C     ABKY1         CHAIN     PAB01                              91
      *
     C                   ENDSR                                                  READ1
      *
      *===============================================================
      * READN - Read next record from file
      *===============================================================
      *
     CSR   READN         BEGSR
      *
     C     ABKY1         READE     PAB01                                  91
      *
     C                   ENDSR                                                  READN
      *
      *===============================================================
      * IZL1BK - Initialize break criteria
      *===============================================================
      *
     CSR   IZL1BK        BEGSR
      *
      *  Re initialize L1 Break criteria
      *
     C                   MOVE      ABTRDT        SVTRDT
      *
     C                   ENDSR                                                  IZL1BK
      *
      *===============================================================
      * SUBSET - Check if member of records processed subset
      *===============================================================
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if not to be processed
      *****************************************************************
      *
      *  Only process payment transactions
      *
     C     ABARTY        IFNE      'P'
     C***  BEGIN ADD  ***  07/17/00  *********************************
     C     ABARTY        ANDNE     'I'
     C     ABARTY        ANDNE     'Q'
     C***  BEGIN ADD  ***  07/17/00  *********************************
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  ABARTY<>P/I/Q
      *
      *  If Transaction Date NOT in range
      *
     C     ABTRDT        IFLT      X7FRDT
     C     ABTRDT        ORGT      X7TODT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      *  Exclude transactions if the deposit flag assigned to the
      *  A/R transaction table value indicates to do so.
      *  Exclude Deposit Slip (WTTTP/TTEXDS = 'Y')
      *
     C                   MOVEL(P)  ABARCD        KKKEY
     C     TBKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     TTEXDS        ANDEQ     'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  TTEXDS
      *
      *
      *
     C                   ENDSR                                                  SUBSET
      *
      *===============================================================
      * L1TST - First Break TEST criteria
      *===============================================================
      *
     CSR   L1TST         BEGSR
      *
      *  MEETL1 = *OFF if L1BK processing to proceed
      *****************************************************************
      *
      *  Transaction Date change
      *
     C     ABTRDT        IFNE      SVTRDT
     C                   MOVE      *OFF          MEETL1
     C                   ENDIF
      *
     C                   ENDSR                                                  L1TST
      *
      *===============================================================
      * L1BK  - First Break action
      *===============================================================
      *
     CSR   L1BK          BEGSR
      *
      *  Output L1 totals
      *
     C                   EXCEPT    L1TOT
     C                   MOVE      '1'           *INOA
      *
      *  Re initialize L1 totals = 0, if 'B' not used in output spec
      *
     C                   ENDSR                                                  L1BK
      *
      *===============================================================
      * SRDET - Write Detail Record
      *===============================================================
      *
     CSR   SRDET         BEGSR
      *
      *  Get Agent Name
      *
      ***** BEG ADD ***** 05/03/06 **************************************
     C                   MOVE      *BLANK        X1AGNM
      ***** END ADD ***** 05/03/06 **************************************
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      '0'
     C     AGKY          CHAIN     PAG01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     AGNAME        X1AGNM
      ***** BEG CHG ***** 05/03/06 **************************************
     C                   ELSE
     C***************      MOVE *BLANK    X1AGNM
     C                   MOVEL(P)  AGTERR        X1AGNM
      ***** END CHG ***** 05/03/06 **************************************
     C                   ENDIF
      ***** BEG ADD ***** 05/03/06 **************************************
     C                   ELSE
     C                   MOVEL(P)  AGTERR        X1AGNM
      ***** END ADD ***** 05/03/06 **************************************
     C                   ENDIF
      *
      *  Transaction Date
      *
     C                   Z-ADD     ABTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TRDT            8 0
      *
      *  Detail record print
      *
      *    NOTE: any exception/elimination criteria should be in SUBSET
      *
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DETAIL
      *
     C                   ENDSR                                                  SRDET
      *
      *===============================================================
      * ACCTOT - Accumulate Totals
      *===============================================================
      *
     CSR   ACCTOT        BEGSR
      *
      *  Accumulate L1 Totals
      *
     C                   ADD       1             L1CNT             9 0
     C                   ADD       ABAMT         L1AMT            13 2
      *
      *  Accumulate LR Totals
      *
     C                   ADD       1             LRCNT             9 0
     C                   ADD       ABAMT         LRAMT            13 2
      *
     C                   ENDSR                                                  ACCTOT
      *
      *===============================================================
      * LRBK - Last Record Processing
      *===============================================================
      *
     CSR   LRBK          BEGSR
      *
     C     MEETPR        IFEQ      *ON
     C                   SETON                                        OA92
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
      *
     C                   ELSE
      *
     C                   EXSR      L1BK
     C                   SETON                                        OA92
     C                   EXCEPT    HEAD
     C                   EXCEPT    LRTOT
     C                   ENDIF                                                  MEETPR
      *
     C                   ENDSR                                                  LRBK
      *
      *===============================================================
      * SRRTE - Print routing page
      *===============================================================
      *
     CSR   SRRTE         BEGSR
      *
      *   Output Routing - detail report 0
      *
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
      *
     C     *IN60         IFEQ      '0'
      *
      * Left Justify Title in Field
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
      *  User Name if no Distribution specified in routing
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
      *
     C                   ELSE
      *
     C                   MOVEL(P)  XTITL         ORTITL
      *
     C                   ENDIF
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
      * Center Company name in display field
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
      *
     C                   ENDSR                                                  SRRTE
      *_______________________________________________________________
      *
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
      **********
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              56
     O**********        09   UPUSNM              67
      **********
     O**********E            ROUTE          1
     O**********     08      ORDIS2              56
      **********
     O**********E            ROUTE          1
     O**********     08      ORDIS3              56
      **********
     O**********E            ROUTE          1
     O**********     08      ORDIS4              56
      **********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              56
     O                  09   UPUSNM              67
      **********
     O          E            ROUTE          1
     O               08      ORDIS2              56
      **********
     O          E            ROUTE          1
     O               08      ORDIS3              56
      **********
     O          E            ROUTE          1
     O               08      ORDIS4              56
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
      **********
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
      **********
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      *
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
      *
     O          E            ROUTE          2
     O                                           20 'COMPANY   :'
     O                       KKCO#         Z   +001
      *
     O          E            ROUTE          2
     O                                           20 'DATE RANGE:'
     O                       X8FRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       X8TODT            +001 '  /  /    '
      *
      ***** BEG CHG ***** 05/03/06 **************************************
     O******************                 25 '(C) COPYRIGHT PROGRESSI'
     O******************               +000 'VE DATA SOLUTIONS. INC.,'
      ****** BEG DLT ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O***                                25 '(C) COPYRIGHT FISERV IN'
     O***                              +000 'SURANCE SOLUTIONS. INC.,'
     O***                      *YEAR   +001
      ****** END DLT ***** 01/28/10 **********************************
      ***** END CHG ***** 05/03/06 **************************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      *
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              69
     O                       DSDATE        Y     98
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              79
     O                                           92 'PAGE:'
     O                       PAGE          Z     98
     O***  BEGIN ADD  ***  09/13/00  *********************************
     O          E    92      HEAD           2
     O                                         + 31 'DATE RANGE:'
     O                       X8FRDT            +001 '  /  /    '
     O                                         +001 '-'
     O                       X8TODT            +001 '  /  /    '
     O***  END   ADD  ***  09/13/00  *********************************
      *
     O          E   N92      HEAD           2
     O                                         +000 'TRANSACTION DATE:'
     O                       X1TRDT            +001 '  /  /    '
      *
     O          E   N92      HEAD           1
     O                                         +004 'Transaction'
     O                                         +068 'Check'
      *
     O          E            HEAD           0
     O                       LINE              +000
      *
     O          E   N92      HEAD           2
     O                                         +004 '   Date    '
     O                                         +002 'Agent#'
     O                                         +002 'Agent name'
     O                                         +032 'Check#'
     O                                         +010 'Amount'
      *
      *
     O          E            DETAIL         1
     O                       X1TRDT            +004 '  /  /    '
     O                       ABAGT#        Z   +004
     O                       X1AGNM            +002
     O                       ABCHK#        Z   +002
     O                       ABAMT         J   +002
      *
      *
     O          E            L1TOT       1  1
     O                                         +004 'SUB TOTAL:'
      *
     O          E            L1TOT          1
     O                                         +009 'Number of checks:'
     O                       L1CNT         JB  +004
      *
     O          E            L1TOT          1
     O                                         +009 'Amount  . . . . :'
     O                       L1AMT         JB  +001
      *
      *
     O          E            LRTOT       2  1
     O                                         +004 'GRAND TOTAL:'
      *
     O          E            LRTOT          1
     O                                         +009 'Number of checks:'
     O                       LRCNT         JB  +004
      *
     O          E            LRTOT          1
     O                                         +009 'Amount  . . . . :'
     O                       LRAMT         JB  +001
      *
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
      *
      *
