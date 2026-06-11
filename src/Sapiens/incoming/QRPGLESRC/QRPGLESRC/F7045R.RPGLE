     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE F7045R - DRG PROSPECTIVE PAYMENT SYSTEM TABLE
     H*****************************************************************
     H*                                                               *
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* PRINTS DRG PROSPECTIVE PAYMENT SYSTEM TABLE                  *
     H*                                                               *
     H* CALLED BY: F7045C                                             *
     H*                                                               *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H* CREATED: 06/12/2001                                           *
     H*                                                               *
     H* 06/12/01 C:136 PROJECT 114MCG                                 *
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
     H*  80       XXTO AND XXFROM = BLANKS, PRINT 'ALL'               *
     H*  81       @@EFDT = BLANKS, PRINT 'ALL'                        *
     H*                                                               *
     H*****************************************************************
     F*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFAPOLPA   IF   E           K DISK
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
      *   KEY LIST FOR THE COMPANY FILE
      *

     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE DRG PAYMENT SYSTEM TABLE FILE
      *
     C     POKY1         KLIST
     C                   KFLD                    KKMED#           10
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
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FROM           10
     C                   PARM                    @@TO             10
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@ST              2
     C                   PARM                    @@EFDT            7
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
      *========================================================================
      * BEGIN
      *========================================================================
      *
     C                   MOVE      *BLANKS       XXFROM
     C                   MOVE      *BLANKS       XXTO
     C                   MOVEL     @@FROM        XXFROM           10
     C                   MOVEL     @@TO          XXTO             10
     C                   MOVE      @@CO#         XXCO#             3 0
     C                   MOVEL     @@ST          XXST              2
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            128
     C                   MOVE      *ON           MEETPR            1            PRINTED
     C                   MOVE      @@EFDT        X2EFDT            7 0
      *
     C     XXFROM        IFEQ      *BLANKS
     C     XXTO          ANDEQ     *BLANKS
     C                   MOVE      '1'           *IN80
     C                   MOVE      *HIVAL        XXTO
     C                   END
     C*
     C     @@EFDT        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN81
     C                   ELSE
     C                   MOVE      @@EFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C                   ENDIF
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
     C                   READ      PPO01                                  91
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
     C                   READ      PPO01                                  91
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
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain Orky Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
      *
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
     C     XXFROM        ANDGT     POMED#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     XXTO          IFNE      *ZERO
     C     XXTO          ANDLT     POMED#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
     C     X2EFDT        IFNE      *ZERO
     C     X2EFDT        ANDNE     POEFDT
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
     C                   MOVE      POEFDT        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT            8 0
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
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
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
      *  Route to:
      *
     C     @@MODE        IFEQ      'D'
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
     O**********             @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      *
     O          E            ROUTE          1
     O                                           23 'REPORT SELECTION:'
      *
     O          E            ROUTE          1
     O                                           28 'STATE . . :'
     O                       XXST              +001
     O                       STNAME            +001
      *
     O          E            ROUTE          1
     O                                           28 'DRG# RANGE:'
     O               80                          34 '*ALL'
     O              N80      XXFROM              39
     O                                           44 'TO:'
     O               80                          50 '*ALL'
     O              N80      XXTO                55
      *
     O          E            ROUTE          1
     O                                           28 'EFF. DATE :'
     O               81                          34 '*ALL'
     O              N81      XXEFDT        Y     39
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
     O                                           70 'DRG PROSPECTIVE PAYMENT'
     O                                         +000 ' SYSTEM TABLE'
      *
     O          E            HEAD           2
     O                       XXSTFD            +041
      *
     O          E            HEAD           1
     O                                         +057 'HOLD'
     O                                         +003 'INDIRECT'
     O                                         +005 'DIRECT'
     O                                         +005 'DISP'
     O                                         +004 'PARAMED'
      *
     O          E            HEAD           1
     O                                         +041 'CAPITAL'
     O                                         +005 'HARMLESS'
     O                                         +004 'MED EDU'
     O                                         +004 'MED EDU'
     O                                         +004 'SHARE'
     O                                         +008 'EDU'
     O                                         +008 'AGG'
      *
     O          E            HEAD           0
     O                       LINE              +000
      *
     O          E            HEAD           2
     O                                         +000 'MEDICARE#'
     O                                         +004 'EFF. DATE'
     O                                         +005 'DRG RATE'
     O                                         +009 'RATE'
     O                                         +009 'RATE'
     O                                         +005 'ADD-ON'
     O                                         +005 'ADD-ON'
     O                                         +003 'ADD-ON'
     O                                         +005 'ADD-ON'
     O                                         +008 'RCC'
     O                                         +004 'STATUS'
      *
     O          E            DET            1
     O                       POMED#              10
     O                       X1EFDT        Y   +002
     O                       PODRRT        4   +003
     O                       POCPRT        4   +003
     O                       POHHRT        4   +003
     O                       POINAD        4   +003
     O                       PODIAD        4   +003
     O                       PODSAD        4   +003
     O                       POPMAD        4   +003
     O                       POARCC        4   +003
     O                       POSTAT            +006
      *
     O          E            NOREC          2
     O                                           67 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTED CRITERIA ***'
