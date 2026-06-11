     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved.-
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H**********************************************************************
    H* PROGRAM: F7108R
    H* DESCRIPTION: Print CMG Codes
    H* CALLED FROM: F7108C1
    H**********************************************************************
    H*  CREATED: 10/17/11  C:274  PROJECT: 114MAR
    H**********************************************************************
    H*  REVISIONS:
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
    H**********************************************************************
    H*  INDICATORS
    H*
    H**********************************************************************
    H*
     FSMORL     IF   E           K DISK
    H*
    H* output routing file by program
    H*
     FSMCOL     IF   E           K DISK
    H*
    H* COMPANY FILE
    H*
     FSMUPL     IF   E           K DISK
    H*
    H* USER PARAMETERS FILE
    H*
     FFMMGL1    IF   E           K DISK
    H*
    H* CMG Code Master logical by code/tier/efdt
    H*
    F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
    H*
    H* CMG Code Listing
    H*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
    D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
    I*
    I*================================================================
    I*  Constants
    I*================================================================
    C*
    C*   KEY LIST FOR THE COMPANY FILE
    C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
    C*
    C*=======================================================================
    C*  KLISTS
    C*=======================================================================
    C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
    C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
    C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
    C*
    C*====================================================================
    C* PLISTS
    C*====================================================================
    C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@EFDT            7
    C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*
    C*========================================================================
    C* BEGIN
    C*========================================================================
    C*
     C                   MOVE      @@CO#         XXCO#             3 0
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE             98
     C                   MOVE      *ON           MEETPR            1            PRINTED
    C*
     C                   Z-ADD     0             XXEFDT            7 0
    C*
     C                   IF        @@EFDT <> *BLANKS
     C                   MOVE      @@EFDT        XXEFDT
     C                   ENDIF
    C*
    C*   PRINT ROUTING PAGE
    C*
     C                   EXSR      SRRTE
    C*
     C                   EXCEPT    HEAD
    C*
    C*  Begin reading file
    C*
     C                   READ      PMG01                                  99
    C*
     C                   Z-ADD     *ZEROS        LRCNT             9 0
    C*
     C     *IN99         DOWEQ     *off
    C*
     C                   IF         XXEFDT = MGEFDT
    C*
     C                   MOVEL(p)  MGDESC        DSC40            40
    C*
     C                   Z-ADD     MGEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         FMEFDT            8 0
    C*
     C                   EXSR      SRDET
    C*
     C                   ENDIF
    C*
    C*  Read next record
    C*
     C                   READ      PMG01                                  99
    C*
     C                   ENDDO                                                  WRK RECORD
    C*
    C*  Message if no records printed
    C*
     C     LRCNT         IFEQ      *ZERO
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
    C*
    C* END PROGRAM
    C*
     C                   EVAL      *INLR = *ON
    C*
     C*================================================================
     C* SRDET - Write Detail Record
     C*================================================================
    C*
     C     SRDET         BEGSR
    C*
     C                   IF        *INOF
     C                   EXCEPT    HEAD
     C                   EVAL      *INOF = *OFF
     C                   ENDIF
     C                   EVAL      LRCNT = LRCNT + 1
    C*
     C                   EXCEPT    DETAIL
    C*
     C                   ENDSR                                                  SRDET
    C*
    C*================================================================
    C* SRRTE - Print routing page
    C*================================================================
    C*
     C     SRRTE         BEGSR
    C*
    C*   GET COMPANY, USER, INFO.
    C*
     C     COKY          CHAIN     PCO01                              60
     C     UPKY          CHAIN     PUP01                              60
    C*
    C*   Output Routing - detail report 0
    C*
     C                   MOVE      '0'           KKPRT
    C*
     C     ORKY          CHAIN     POR01                              60
    C*
     C                   IF        *IN60 = *OFF
    C*
    C* Left Justify Title in Field
    C*
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
    C*
    C*  Route to:
    C*
     C     @@MODE        IFEQ      'D'
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   MOVEL(P)  *BLANKS       ORDIS2
     C                   MOVEL(P)  *BLANKS       ORDIS3
     C                   MOVEL(P)  *BLANKS       ORDIS4
     C*
     C                   ELSE
    C*
     C     ORDIS1        IFEQ      *BLANKS
     C     ORDIS2        ANDEQ     *BLANKS
     C     ORDIS3        ANDEQ     *BLANKS
     C     ORDIS4        ANDEQ     *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
    C*
     C                   ENDIF                                                  MODE
    C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
    C*
     C                   ENDIF
    C*
    C* Center Company name in display field
    C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        CONAME
    C*
     C                   ENDSR                                                  SRRTE
    C*
    C*================================================================
    C*
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
     O                                           26 'EFFECTIVE DATE  :'
     O                       XXEFDT            +001
     O*
     O          E            ROUTE          1 59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE          1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
     O*
     O* HEADING
     O*
     O          E            HEAD           1 03
     O                       DSTIME               8 '  :  :  '
     O                       CONAME              68
     O                       DSDATE        Y     98
     O*
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           55 'CMG Code Listing'
     O                                           94 'Page:'
     O                       PAGE          Z     98
     O*
     O          E            HEAD           1
     O                                            5 'CMG'
     O          E            HEAD           1
     O                                            6 'Code'
     O                                           18 'Eff. Date'
     O                                           27 'Tier'
     O                                           41 'Description'
     O                                           82 'Rel Weight'
     O                                           90 'ALOS'
     O                                           98 'Status'
     O*
     O          E            HEAD           2
     O                       LINE              +001
     O*
     O          E            DETAIL         1
     O                       MGCODE               7
     O                       FMEFDT        Y     18
     O                       MGTIER              26
     O                       DSC40               70
     O                       MGRELW        P     82
     O                       MGALOS        P     90
     O                       MGSTAT              96
     O*
     O          E            NOREC          2
     O                                           25 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTED CRITERIA ***'
