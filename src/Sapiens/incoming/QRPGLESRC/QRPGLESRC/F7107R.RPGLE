     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F7107R - RUG IV Codes Print Program
     H*****************************************************************
     H* Created on 10/06/2011    C:251    Project 114MZO              *
     H*****************************************************************
     H*                                                               *
     H* Called by: F7107C1                                            *
     H* Calls to : None                                               *
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
     FFMRGL1    IF   E           K DISK
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
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
     C*====================================================================
     C* PLISTS
     C*====================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@EFDT            7
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
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            106
     C                   MOVE      *ON           MEETPR            1            PRINTED
     C*
     C                   Z-ADD     *ZEROS        TMEFDT            7 0
     C*
     C     @@EFDT        IFNE      *BLANKS
     C                   MOVE      @@EFDT        TMEFDT
     C                   ENDIF
     C*
     C*   PRINT ROUTING PAGE
     C*
     C                   EXSR      SRRTE
     C                   EXCEPT    HEAD
     C*
     C                   READ      PRG01                                  99
     C*
     C                   Z-ADD     *ZEROS        LRCNT             9 0
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C     TMEFDT        IFEQ      *ZEROS
     C     TMEFDT        OREQ      RGEFDT
     C*
     C                   Z-ADD     RGEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A1EFDT            8 0
     C*
     C                   EXSR      SRDET
     C                   ADD       1             LRCNT
     C*
     C                   ENDIF
     C*                    ENDIF
     C*
     C*  Read next record
     C*
     C                   READ      PRG01                                  99
     C*
     C                   ENDDO                                                  WRK RECORD
     C*
     C*  Message if no records printed
     C*
     C     LRCNT         IFEQ      *ZERO
     C                   EXCEPT    NOREC
     C                   ENDIF
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
     C*** GET COPYRIGHT INFORMATION
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
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
     O                                           26 'Effective Date  :'
     O                       TMEFDT            +001
      *
     O          E            ROUTE          1 60
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              82
     O                       DSDATE        Y    121
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           67 'RUG IV Codes'
     O                                          117 'PAGE:'
     O                       PAGE          Z    121
     O          E            HEAD           1
     O                                         +002 'RUG IV Code'
     O                                         +014 'Eff. Date'
     O                                         +014 'Total Rate Urban'
     O                                         +014 'Total Rate Rural'
     O                                         +003 'Status'
     O*
     O          E            HEAD           1
     O                       LINE              +001
     O*
     O          E            DET            1
     O                       RGCAT             +002
     O                       A1EFDT        Y   +019
     O                       RGUTOT        4   +022
     O                       RGRTOT        4   +022
     O                       RGSTAT            +006
      *
     O          E            NOREC          2
     O                                           62 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTED CRITERIA ***'
