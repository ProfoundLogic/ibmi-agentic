     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXWC1333 - Display records from WXFMP1 for WC040306
     H*****************************************************************
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*     command line.                                             *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name
     H*                                                               *
     H*  06/09/06 C:61 PROJECT WC1333                                 *
     H*                Created.                                       *
     H*                                                               *
     H*  PLEASE ADD TO TOP                                            *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  91       Read main File                                      *
     H*  92       Last record processing                              *
     H*  93       Print Group sub heading                             *
     H*  94       Print User sub heading                              *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
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
      * Employer Policy Year
     FWDELL     IF   E           K DISK
      *
      * OLD WAIVER DATA FORMAT
     FWXFML11   IF   E           K DISK
      *
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *

     D XTITL           C                   CONST('*** CA WAIVER RECOR-
     D                                     DS ***')
      *
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *   User Parameter
      *

     C     UPKY1         KLIST
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
     C                   KFLD                    F3CO#
      *
      *   Employer Policy Year
      *
     C     ELKY          KLIST
     C                   KFLD                    F3CO#
     C                   KFLD                    F3FND
     C                   KFLD                    F3EMP#
     C                   KFLD                    F3DIV
     C                   KFLD                    F3FYR
      *
      *   *LIKE Define Variables
      *
     C     *LIKE         DEFINE    F3CO#         KKCO#
     C     *LIKE         DEFINE    F3FND         SVFND
     C     *LIKE         DEFINE    F3USER        SVUSER
     C     *LIKE         DEFINE    UPUSNM        SVUSNM
      *
      *   ENTRY PARAMETERS
      *
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      *   Initialize
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            132
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
      *   Print Routing Page
      *
     C                   EXSR      SRRTE
      *
      *  Read 1st record
      *
     C     *LOVAL        SETLL     PFM01
     C                   READ      PFM01                                  91
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
      *  Write Detail Record
      *
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
      *
      *  Accumulate Totals
      *
     C                   ADD       1             L1CNT             9 0
      *
     C                   ENDIF                                                  MEETSS
      *
      *  Read next record
      *
     C                   READ      PFM01                                  91
      *
     C                   ENDDO                                                  WRK RECORD
      *
      *  Last Record processing
      *
     C     MEETPR        IFEQ      *ON
     C     L1CNT         OREQ      *ZERO
     C                   SETON                                        OA92
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF                                                  MEETPR
      *
      *
      *  End of Processing
     C                   EXCEPT    LRTOT
      *
     C     END           TAG
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       KkPrt = '0';
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
      *
      *
      *****************************************************************
      * SUBSET - Check if member of records processed subset
      *****************************************************************
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if not to be processed
      *****************************************************************
      *
     C                   MOVE      DSDATE        X1DATE            8 0
      *
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKTRDT            7 0
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     ELPTDT        IFLT      KKTRDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     F3C401        IFEQ      *BLANKS
     C     F3C402        ANDEQ     *BLANKS
     C     F3C403        ANDEQ     *BLANKS
     C     F3C404        ANDEQ     *BLANKS
     C     F3C405        ANDEQ     *BLANKS
     C     F3C406        ANDEQ     *BLANKS
     C     F3C407        ANDEQ     *BLANKS
     C     F3C408        ANDEQ     *BLANKS
     C     F3C409        ANDEQ     *BLANKS
     C     F3C410        ANDEQ     *BLANKS
     C     F3C411        ANDEQ     *BLANKS
     C     F3C412        ANDEQ     *BLANKS
     C     F3C413        ANDEQ     *BLANKS
     C     F3C414        ANDEQ     *BLANKS
     C     F3C415        ANDEQ     *BLANKS
     C     F3C416        ANDEQ     *BLANKS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     F3C201        IFEQ      *BLANKS
     C     F3C202        ANDEQ     *BLANKS
     C     F3C203        ANDEQ     *BLANKS
     C     F3C204        ANDEQ     *BLANKS
     C     F3C205        ANDEQ     *BLANKS
     C     F3C206        ANDEQ     *BLANKS
     C     F3C207        ANDEQ     *BLANKS
     C     F3C208        ANDEQ     *BLANKS
     C     F3C209        ANDEQ     *BLANKS
     C     F3C210        ANDEQ     *BLANKS
     C     F3C211        ANDEQ     *BLANKS
     C     F3C212        ANDEQ     *BLANKS
     C     F3C213        ANDEQ     *BLANKS
     C     F3C214        ANDEQ     *BLANKS
     C     F3C215        ANDEQ     *BLANKS
     C     F3C216        ANDEQ     *BLANKS
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
      *****************************************************************
      * SRDET - Write Detail Record
      *****************************************************************
      *
     CSR   SRDET         BEGSR
      *
      *  Detail record print
      *
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DETAIL
      *
     C                   ENDSR                                                  SRDET
      *
      *
      *
      *
      *
      *****************************************************************
      * SRRTE - Print routing page
      *****************************************************************
      *
     CSR   SRRTE         BEGSR
      *
     C*** BEG ADD *** 01/28/10 ****************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C****END ADD *** 01/28/10 ****************************************
     C*
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY1         CHAIN     PUP01                              60
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
     C*********          EXCEPT    ROUTE
      ***** Beg Del ***** 08/26/14 ************************************
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
     C                   SETON                                        OA
      *
     C                   ENDSR                                                  SRRTE
      *
      *
      *
      *****************************************************************
      *
      *
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
     O**********             ORDIS1              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS2              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS3              56
      **********
     O**********E            ROUTE          1
     O**********             ORDIS4              56
      **********
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              56

     O          E            ROUTE          1
     O                       ORDIS2              56

     O          E            ROUTE          1
     O                       ORDIS3              56

     O          E            ROUTE          1
     O                       ORDIS4              56
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      *
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
      *
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      *
      *
      *
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                25 '(C) COPYRIGHT FISERV IN'
     O***                              +000 'SURANCE SOLUTIONS. INC.,'
     O***                      *YEAR   +001
      ****** END DLT ***** 01/28/10 **********************************
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
     O                       DSDATE        Y    132
      *
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
      *
     O          E            DETAIL      2  1
     O                                           20 'Company      :'
     O                       F3CO#         Z   +002
      *
     O          E            DETAIL         1
     O                                           20 'Group        :'
     O                       F3FND         Z   +002
      *
     O          E            DETAIL         1
     O                                           20 'Employer     :'
     O                       F3EMP#        Z   +002
      *
     O          E            DETAIL         1
     O                                           20 'Division     :'
     O                       F3DIV         Z   +002
      *
     O          E            DETAIL         3
     O                                           20 'Policy Year  :'
     O                       F3FYR         Z   +002
      *
     O          E            DETAIL         1
     O                       F3C401              50
     O                       F3C201            + 10
      *
     O          E            DETAIL         1
     O                       F3C402              50
     O                       F3C202            + 10
      *
     O          E            DETAIL         1
     O                       F3C403              50
     O                       F3C203            + 10
      *
     O          E            DETAIL         1
     O                       F3C404              50
     O                       F3C204            + 10
      *
     O          E            DETAIL         1
     O                       F3C405              50
     O                       F3C205            + 10
      *
     O          E            DETAIL         1
     O                       F3C406              50
     O                       F3C206            + 10
      *
     O          E            DETAIL         1
     O                       F3C407              50
     O                       F3C207            + 10
      *
     O          E            DETAIL         1
     O                       F3C408              50
     O                       F3C208            + 10
      *
     O          E            DETAIL         1
     O                       F3C409              50
     O                       F3C209            + 10
      *
     O          E            DETAIL         1
     O                       F3C410              50
     O                       F3C210            + 10
      *
     O          E            DETAIL         1
     O                       F3C411              50
     O                       F3C211            + 10
      *
     O          E            DETAIL         1
     O                       F3C412              50
     O                       F3C212            + 10
      *
     O          E            DETAIL         1
     O                       F3C413              50
     O                       F3C213            + 10
      *
     O          E            DETAIL         1
     O                       F3C414              50
     O                       F3C214            + 10
      *
     O          E            DETAIL         1
     O                       F3C415              50
     O                       F3C215            + 10
      *
     O          E            DETAIL         1
     O                       F3C416              50
     O                       F3C216            + 10
      *
      *
     O          E            LRTOT       2  1
     O                                         +010 'Count:'
     O                       L1CNT         AB  +001
      *
      *
     O          E            NOREC       2
     O                                           27 '*** NO RECORDS MEET THE'
     O                                           50 'SELECTED CRITERIA ***'
      *
      *
