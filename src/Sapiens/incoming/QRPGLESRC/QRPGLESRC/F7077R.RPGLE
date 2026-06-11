     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H/TITLE F7077R - DRG FACTORS AND CCR'S TABLE LIST
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H* PRINTS DRG FACTORS AND CCR'S TABLE LISTING                    *
     H*                                                               *
     H* CALLED BY: F7077C                                             *
     H* CALLS TO:  NONE                                               *
     H*                                                               *
     H*****************************************************************
     H* CREATED ON 09/30/2002 BY C:62 FOR PROJECT CA001               *
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
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFADFL     IF   E           K DISK    USROPN
     FFADFL1    IF   E           K DISK    USROPN
     F                                     RENAME(PDF01:PDF02)
     FSTABL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS                  INZ
     D  XXCMD                  1     56
     D  XXST                  38     39
     D*
     D                 DS                  INZ
     D  YYCMD                  1     56
     D  YYST                  40     41
      *
      *
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*========================================================================
     C* KLISTS
     C*========================================================================
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C     DFKY1         KLIST
     C                   KFLD                    KKMED#
     C*
     C     DFKY2         KLIST
     C                   KFLD                    KKEFDT
     C*
     C     DFKY3         KLIST
     C                   KFLD                    KKEFDT
     C                   KFLD                    KKMED#
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C*================================================================
     C* LIKE DEFINES
     C*================================================================
     C     *LIKE         DEFINE    UPCO#         KKCO#
     C     *LIKE         DEFINE    DFMED#        KKMED#
     C     *LIKE         DEFINE    DFEFDT        KKEFDT
     C     *LIKE         DEFINE    STKEY         KKST
     C     *LIKE         DEFINE    @@FROM        XXFROM
     C     *LIKE         DEFINE    @@TO          XXTO
     C*========================================================================
     C* PLISTS
     C*========================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FROM           10
     C                   PARM                    @@TO             10
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@ST              2
     C                   PARM                    @@EFDT            7
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*========================================================================
     C* BEGIN
     C*========================================================================
     C                   MOVEL(P)  @@FROM        XXFROM
     C                   MOVEL(P)  @@TO          XXTO
     C                   MOVE      @@CO#         KKCO#
     C                   MOVEL     @@ST          XXST              2
     C                   MOVEL(P)  @@FROM        KKMED#
     C                   MOVE      @@EFDT        KKEFDT
     C                   Z-ADD     *ZEROS        LRCNT             9 0
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            128
     C*
     C                   SETOFF                                       16
     C     KKEFDT        IFNE      *ZEROS
     C                   SETON                                        16
     C                   Z-ADD     KKEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C                   ENDIF
     C*
     C                   SETOFF                                       17
     C     XXFROM        IFEQ      *BLANKS
     C     XXTO          ANDEQ     *BLANKS
     C                   SETON                                        17
     C                   MOVE      *HIVAL        XXTO
     C                   END
     C*
     C*   GET STATE INFORMATION
     C*
     C                   MOVEL     XXST          KKST
     C     STKY          CHAIN     PST01                              99
     C*
     C*   CENTER STATE NAME IN PRINTOUT
     C*
     C     XXST          CAT       STNAME:2      XXSTFD           50
     C                   MOVEL(P)  XXSTFD        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXSTFD
     C*
     C*   PRINT ROUTING PAGE
     C*
     C                   EXSR      SRRTE
     C*
     C*  Begin reading file
     C*
     C                   SETON                                        OA
     C     *IN16         IFEQ      *OFF
     C*
     C                   MOVEL     QCA(1)        XXCMD
     C                   MOVEL     @@ST          XXST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    XXCMD
     C                   PARM      56            LENGTH           15 5
     C*
     C                   OPEN      FADFL
     C*
     C     DFKY1         SETLL     PDF01
     C                   READ      PDF01                                  99
     C                   ELSE
     C*
     C                   MOVEL     QCA(2)        YYCMD
     C                   MOVEL     @@ST          YYST
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    YYCMD
     C                   PARM      56            LENGTH
     C*
     C                   OPEN      FADFL1
     C*
     C     DFKY3         SETLL     PDF02
     C     DFKY2         READE     PDF02                                  99
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     '0'
     C*
     C*  Write detail record
     C*
     C     *IN17         IFEQ      *OFF
     C     DFMED#        ANDGT     XXTO
     C                   LEAVE
     C                   ELSE
     C                   EXSR      SRDET
     C                   ADD       1             LRCNT
     C                   ENDIF
     C*
     C*  Read next record
     C*
     C     *IN16         IFEQ      *OFF
     C                   READ      PDF01                                  99
     C                   ELSE
     C     DFKY2         READE     PDF02                                  99
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C     *IN16         IFEQ      *OFF
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
     C                   PARM      56            LENGTH
     C                   CLOSE     FADFL
     C                   ELSE
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM      56            LENGTH
     C                   CLOSE     FADFL1
     C                   ENDIF
     C*
     C*  Message if no records printed
     C*
     C     LRCNT         IFEQ      *ZERO
     C                   SETON                                        OA
     C                   EXCEPT    HEAD
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
     C*   END PROGRAM
     C*
     C                   SETON                                        LR
     C*================================================================
     C* SRDET - Write Detail Record
     C*================================================================
     CSR   SRDET         BEGSR
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   SETOFF                                       OA
     C                   ENDIF
     C*
     C                   Z-ADD     DFEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1EFDT            8 0
     C*
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
     C     COKY          CHAIN     PCO01                              99
     C     UPKY          CHAIN     PUP01                              99
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*   Output Routing - detail report 0
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              99
     C*
     C     *IN99         IFEQ      '0'
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
     C                   ENDIF                                                  MODE
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
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
     C*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O*
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O*
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O*
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O*
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O*
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O*
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O*
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O*
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O*
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O*
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O*
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O*
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********             UPUSNM              67
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O*
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O                       UPUSNM              67
     O
     O          E            ROUTE          1
     O                       ORDIS2              60
     O
     O          E            ROUTE          1
     O                       ORDIS3              60
     O*
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O*
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O*
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
     O*
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********             @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O*
     O          E            ROUTE          1
     O                                           23 'REPORT SELECTION:'
     O*
     O          E            ROUTE          1
     O                                           28 'STATE . . :'
     O                       XXST              +001
     O                       STNAME            +001
     O*
     O          E            ROUTE          1
     O                                           28 'MED# RANGE:'
     O               17                          34 '*ALL'
     O              N17      XXFROM              39
     O                                           44 'TO:'
     O               17                          50 '*ALL'
     O              N17      XXTO                55
     O*
     O          E            ROUTE          1
     O                                           28 'EFF. DATE :'
     O              N16                          34 '*ALL'
     O               16      XXEFDT        Y     39
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                25 '(C) COPYRIGHT PROGRESSI'
     O***                              +000 'VE DATA SOLUTIONS, INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              88
     O                       DSDATE        Y    122
     O*
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             117
     O                                          116 'PAGE:'
     O                       PAGE          Z    122
     O*
     O          E            HEAD           1
     O                                           70 'DRG FACTORS AND CCR''S'
     O                                         +000 ' SYSTEM TABLE'
     O*
     O          E            HEAD           2
     O                       XXSTFD            +041
     O*
     O          E            HEAD           1
     O                                         +068 'COMPOSITE'
     O                                         +002 'CAPITAL'
     O                                         +001 'OPERATING'
     O                                         +004 'TOTAL'
     O                                         +006 'OUTLIER'
     O*
     O          E            HEAD           0
     O                       LINE              +000
     O*
     O          E            HEAD           2
     O                                         +000 'MEDICARE#'
     O                                         +003 'NAME'
     O                                         +039 'EFF. DATE'
     O                                         +007 'FACTOR'
     O                                         +006 'CCR'
     O                                         +007 'CCR'
     O                                         +006 'CCR'
     O                                         +007 'FACTOR'
     O                                         +002 'STS'
     O*
     O          E            DET            1
     O                       DFMED#              10
     O                       DFNAME            +002
     O                       X1EFDT        Y   +002
     O                       DFCOMP        4   +003
     O                       DFCCCR        4   +003
     O                       DFOCCR        4   +004
     O                       DFTCCR        4   +003
     O                       DFOUTL        4   +003
     O                       DFSTAT            +003
     O*
     O          E            NOREC          2
     O                                           67 '*** NO RECORDS MEET THE'
     O                                         +001 'SELECTED CRITERIA ***'
**  (QCMDEX)
OVRDBF FILE(FADFL) TOFILE(*LIBL/FADFL  )
OVRDBF FILE(FADFL1) TOFILE(*LIBL/FADFL1  )
DLTOVR FILE(FADFL)
DLTOVR FILE(FADFL1)
