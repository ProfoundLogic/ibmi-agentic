     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F7085R - Medicare Rates - Listing
     H*****************************************************************
     H* Created on 02/28/2007 by c:62 for Project 114MMZ              *
     H*****************************************************************
     H*                                                               *
     H* Called by: F7085C                                             *
     H* Calls to : None                                               *
     H*
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMMCL1    IF   E           K DISK
     FSTABL     IF   E           K DISK
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*================================================================
     I*  Constants
     I*================================================================

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*=======================================================================
     C*  KLISTS
     C*=======================================================================
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C*====================================================================
     C* PLISTS
     C*====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@ST              2
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*========================================================================
     C* BEGIN
     C*========================================================================
     C*
     C                   MOVE      @@CO#         XXCO#             3 0
     C                   MOVEL     @@ST          XXST              2
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            145
     C                   MOVE      *ON           MEETPR            1            PRINTED
     C*
     C*
     C* GET STATE INFORMATION
     C*
     C                   MOVEL     XXST          KKST
     C     STKY          CHAIN     PST01                              60
     C*
     C* CENTER STATE NAME IN PRINTOUT
     C*
     C     XXST          CAT(P)    STNAME:2      XXSTFD           50
     C*
     C*   PRINT ROUTING PAGE
     C*
     C                   EXSR      SRRTE
     C                   EXCEPT    HEAD
     C*
     C*  Begin reading file
     C*
     C                   READ      PMC01                                  99
     C*
     C                   Z-ADD     *ZEROS        LRCNT             9 0
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C                   MOVEL     MCDSC1        M1DSC1           20
     C                   Z-ADD     MCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         M1EFDT            8 0
     C*
     C                   EXSR      SRDET
     C                   ADD       1             LRCNT
     C*
     C*  Read next record
     C*
     C                   READ      PMC01                                  99
     C*
     C                   ENDDO                                                  WRK RECORD
     C*
     C*  Message if no records printed
     C*
     C     LRCNT         IFEQ      *ZERO
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
     C*
     C* END PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*================================================================
     C* SRDET - Write Detail Record
     C*================================================================
     CSR   SRDET         BEGSR
     C*
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DET
     C*
     C                   ENDSR                                                  SRDET
     C*================================================================
     C* SRRTE - Print routing page
     C*================================================================
     CSR   SRRTE         BEGSR
     C*
     C*   GET COMPANY, USER, INFO.
     C*
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*   Output Routing - detail report 0
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C*
     C* Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Route to:
     C*
     C     @@MODE        IFEQ      'D'
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   MOVEL(P)  *BLANKS       ORDIS2
     C                   MOVEL(P)  *BLANKS       ORDIS3
     C                   MOVEL(P)  *BLANKS       ORDIS4
     C*
     C                   ELSE
     C*
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
     C*
     C                   ENDIF                                                  MODE
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   ENDIF
     C*
     C* Center Company name in display field
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C                   ENDSR                                                  SRRTE
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
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
     O                                           26 'STATE . . . . . :'
     O                       XXST              +001
     O                       STNAME            +001
      *
     O          E            ROUTE          1 60
     O                                           25 '(C) COPYRIGHT PROGRESSI'
     O                                         +000 'VE DATA SOLUTIONS. INC.,'
     O                       *YEAR             +001
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME             130
     O                       DSDATE        Y    198
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                          119 'TABLE FILE LISTING'
     O                                          190 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O                                          118 '  MEDICARE RATES '
     O          E            HEAD           2
     O                       XXSTFD            +002
     O          E            HEAD           0
     O                       LINE              +001
     O          E            HEAD           2
     O                                         +002 ' CODE'
     O                                         +002 ' EFF. DATE'
     O                                         +002 'DESCRIPTION'
     O          E            DET            1
     O                       MCPRCD            +002
     O                       M1EFDT        Y   +002
     O                       MCDSC1            +002
     O                       MCDSC2            +000
     O          E            DET            1
     O                                         +001 'LOCATION 01 - FAC:'
     O                       MCFA01        M   +002
     O                                         +001 'NON-FAC:'
     O                       MCNF01        M   +001
     O                                         +001 'MOD 53 FAC:'
     O                       MCF501        M   +001
     O                                         +001 'MOD 53 NON-FAC:'
     O                       MCN501        M   +001
     O                                         +001 'MOD 26:'
     O                       MC2601        M   +001
     O                                         +001 'MOD TC:'
     O                       MCTC01        M   +001
     O          E            DET            1
     O                                         +001 'LOCATION 99 - FAC:'
     O                       MCFA99        M   +002
     O                                         +001 'NON-FAC:'
     O                       MCNF99        M   +001
     O                                         +001 'MOD 53 FAC:'
     O                       MCF599        M   +001
     O                                         +001 'MOD 53 NON-FAC:'
     O                       MCN599        M   +001
     O                                         +001 'MOD 26:'
     O                       MC2699        M   +001
     O                                         +001 'MOD TC:'
     O                       MCTC99        M   +001
      *
     O          E            NOREC          2
     O                                           67 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTED CRITERIA ***'
