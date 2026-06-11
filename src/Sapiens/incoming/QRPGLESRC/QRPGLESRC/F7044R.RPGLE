     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H/TITLE F7044R - DRG FEDERAL TABLE LISTING
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* PRINTS DRG FEDERAL TABLE LISTING                             *
     H*                                                               *
     H* CALLED BY: F7043C                                             *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 06/07/2001                                           *
     H*                                                               *
     H* 06/04/01 C:136 PROJECT 114MCG                                 *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*****************************************************************
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
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*  80       XXTO = BLANKS                                       *
     H*  92       PRINT "NO RECORDS" MESSAGE                         *
     H*                                                               *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
      *
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFTFELPA   IF   E           K DISK
      *
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *
     FSTABL     IF   E           K DISK
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
      *========================================================================
      * KLISTS
      *========================================================================
      *
      *
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE DRG CODE FILE
      *
     C     FEKEY         KLIST
     C                   KFLD                    KKDRG#            5 0
      *
      *   KEY LIST FOR THE OVERRIDE FILE
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *   KEYLIST FOR STATE FILE
      *
     C     STKY          KLIST
     C                   KFLD                    KKST             10
      *
      *========================================================================
      * PLISTS
      *========================================================================
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFROM            5
     C                   PARM                    QQTO              5
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQST              2
      *
      *========================================================================
      * BEGIN
      *========================================================================
      *
     C                   MOVE      *BLANKS       XXFROM
     C                   MOVE      *BLANKS       XXTO
     C                   MOVE      QQFROM        XXFROM            5 0
     C                   MOVE      QQTO          XXTO              5 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVEL     QQST          XXST              2
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            124
     C                   MOVE      *ON           MEETPR            1            PRINTED
      *
     C     XXFROM        IFEQ      *ZEROS
     C     XXTO          ANDEQ     *ZEROS
     C                   MOVE      '1'           *IN80
     C                   MOVE      *HIVAL        XXTO
     C                   END
      *
      *   GET STATE INFORMATION
      *
     C                   MOVEL     XXST          KKST
     C     STKY          CHAIN     PST01                              60
      *
      *   CENTER STATE NAME IN PRINTOUT
      *
     C     XXST          CAT       STNAME:2      XXSTFD           50
     C                   MOVEL(P)  XXSTFD        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXSTFD
      *
      *   PRINT ROUTING PAGE
      *
     C                   EXSR      SRRTE
      *
      *  Begin reading file
      *
     C                   SETON                                        OA
     C                   READ      PFE01                                  91
      *
     C     *IN91         DOWEQ     '0'
      *
      *  If subset criteria met
      *
     C                   MOVE      *ON           MEETSS            1
     C                   EXSR      SUBSET
      *
      *  Write detail record
      *
     C     MEETSS        IFEQ      *ON
      *
     C                   EXSR      SRDET
     C                   MOVE      *OFF          MEETPR                         PRINTED
     C                   ADD       1             LRCNT             9 0
     C                   ENDIF                                                  MEETSS
      *
      *  Read next record
      *
     C                   READ      PFE01                                  91
     C                   ENDDO                                                  WRK RECORD
      *
      *
      *  Message if no records printed
      *
     C     MEETPR        IFEQ      *ON
     C     LRCNT         OREQ      *ZERO
     C                   SETON                                        OA92
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF                                                  MEETPR
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain Orky Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *   END PROGRAM
      *
     C                   MOVE      '1'           *INLR
      *================================================================
      * SUBSET - Check if record meets selection criteria
      *================================================================
     CSR   SUBSET        BEGSR
      *
      *  MEETSS = *OFF, if not to be processed
      *
     C     XXFROM        IFNE      *ZERO
     C     XXFROM        ANDGT     FEDRG#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     XXTO          IFNE      *ZERO
     C     XXTO          ANDLT     FEDRG#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDSR                                                  SUBSET
      *
      *================================================================
      * SRDET - Write Detail Record
      *================================================================
      *
     CSR   SRDET         BEGSR
      *
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   MOVEL     FEDESC        XXDESC           35
     C                   EXCEPT    DET
      *
     C                   ENDSR                                                  SRDET
      *
      *================================================================
      * SRRTE - Print routing page
      *================================================================
      *
     CSR   SRRTE         BEGSR
      *
      *   GET COMPANY, USER, INFO.
      *
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
      *
      *   Output Routing - detail report 0
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
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
      *  Route to:
      *
     C     QQMODE        IFEQ      'D'
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   MOVEL(P)  *BLANKS       ORDIS2
     C                   MOVEL(P)  *BLANKS       ORDIS3
     C                   MOVEL(P)  *BLANKS       ORDIS4
      *
     C                   ELSE
      *
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
      *
     C                   ENDIF                                                  MODE
      *
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   ENDIF
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
      *
     OQSYSPRT   E            ROUTE          1 06
      *
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
      *
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
      *
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
      *
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
      *
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
      *
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
      *
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
      *
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
      *
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
      *
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
      *
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
      *
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
      *
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********             UPUSNM              67
      *
     O**********E            ROUTE          1
     O**********             ORDIS2              60
      *
     O**********E            ROUTE          1
     O**********             ORDIS3              60
      *
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O                       UPUSNM              67
      *
     O          E            ROUTE          1
     O                       ORDIS2              60
      *
     O          E            ROUTE          1
     O                       ORDIS3              60
      *
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
      *
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      *
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
      *
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
      *
     O          E            ROUTE          2
     O                                           28 'STATE     :'
     O                       XXST              +001
     O                       STNAME            +001
      *
     O          E            ROUTE          2
     O                                           28 'DRG# RANGE:'
     O               80                          34 '*ALL'
     O              N80      XXFROM        4     35
     O                                           40 'TO:'
     O               80                          46 '*ALL'
     O              N80      XXTO          4     47
      *
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                25 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS, INC.,'
     O***                      *YEAR   +001
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              88
     O                       DSDATE        Y    122
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             117
     O                                          116 'PAGE:'
     O                       PAGE          Z    122
      *
     O          E            HEAD           1
     O                                           73 'DRG FEDERAL REGISTER'
     O                                         +000 ' TABLE'
      *
     O          E            HEAD           2
     O                       XXSTFD            +041
      *
     O          E            HEAD           1
     O                                         +061 'REL'
     O                                         +036 'OUTLIER'
      *
     O          E            HEAD           0
     O                       LINE              +000
      *
     O          E            HEAD           2
     O                                         +001 'DRG#'
     O                                         +006 'DESCRIPTION'
     O                                         +036 'WEIGHT'
     O                                         +006 'GEO LOS'
     O                                         +006 'ARITH LOS'
     O                                         +006 'THRESHOLD'
     O                                         +006 'STATUS'
      *
     O          E            DET            1
     O                       FEDRG#        4   +000
     O                       XXDESC            +006
     O                       FERLWT        4   +011
     O                       FEGLOS        4   +008
     O                       FEALOS        4   +010
     O                       FEOUTL        4   +009
     O                       FESTAT            +008
      *
     O          E            NOREC          2
     O                                           67 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTED CRITERIA ***'
