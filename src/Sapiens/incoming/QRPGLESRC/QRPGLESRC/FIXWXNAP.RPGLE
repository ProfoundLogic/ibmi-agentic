     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWXNAP - LIST POLICIES WITH UNWANTED WXNAP RECORDS      DS
     H*****************************************************************
     H*                                                               *
     H*   CALLED FROM:        THIS PROGRAM WILL LIST OFF ANY POLICIES *
     H*     COMMAND ENTRY     THAT HAVE PROCESSED FOR WCPOLS AND MAY  *
     H*                       HAVE UNWANTED WXNAP RECORDS FROM CX'D   *
     H*   CALLS TO:           DIVISIONS.                              *
     H*     NONE                                                      *
     H*                                                               *
     H*   PROGRAMMER: C:65                                            *
     H*   CREATED:    11/12/03    PROJECT: SUPP#39756                 *
     H*                                                               *
     H*****************************************************************
     FWMSHL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  SHUNID                 1     12  0
     D  XXTIME                 1      6  0
     D  XXDATE                 7     12  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFCTV DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     EMKY          KLIST
     C                   KFLD                    SHCO#
     C                   KFLD                    SHFND
     C                   KFLD                    SHEMP#
      *
     C                   TIME                    DSDTTM
     C                   MOVE      *ON           *INOF
      *
      * READ ALL SUBMISSION HISTORY RECORDS
      *
     C                   READ      PSH01                                  60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     SHTYPE        IFEQ      'P'
      *
     C     SHSTAT        IFEQ      '3'
     C     SHSTAT        OREQ      '4'
     C     SHSTAT        OREQ      '5'
      *
     C                   MOVE      *OFF          *IN33
      *
     C     EMKY          CHAIN     PEM01                              61
     C     *IN61         DOWEQ     *OFF
     C     *IN33         ANDEQ     *OFF
      *
     C     EMDIV         IFNE      SHDIV
     C                   Z-ADD     5             XL                1 0
      *
     C                   DO        5
      *
     C     CD(XL)        IFNE      0
     C     ED(XL)        ANDNE     0
      *
     C     CD(XL)        IFLE      SHPFDT
     C                   MOVE      *ON           *IN33
     C                   ENDIF                                                  CD,XL IFLE
     C                   LEAVE
      *
     C                   ELSE
      *
     C                   SUB       1             XL
     C                   ENDIF                                                  CD,XL IFNE
      *
     C                   ENDDO                                                  DO 5
      *
     C                   ENDIF                                                  EMDIV IFNE
      *
     C     EMKY          READE     PEM01                                  61
     C                   ENDDO                                                  *IN61 DOWEQ OFF
      *
      * REPORT EMPLOYER IF A CX'D DIVISION IS FOUND
      *
     C     *IN33         IFEQ      *ON
      *
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF                                                  *INOF IFEQ
     C                   EXCEPT    DET
      *
     C                   ENDIF                                                  *IN33 IFEQ
      *
     C                   ENDIF                                                  SHSTAT IFEQ
      *
     C                   ENDIF                                                  SHTYPE IFEQ
      *
     C                   READ      PSH01                                  60
      *
     C                   ENDDO                                                  *IN60=*OFF
      *
     C                   MOVE      *ON           *INLR
      *
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           24 'FIXWXNAP'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           31 'PGM TO LIST POLICIES W'
     O                                           53 'HICH MAY HAVE UNWANTED'
     O                                           68 ' WXNAP RECORDS.'
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           36 'STATUS'
     O                                           46 'DATE'
     O                                           56 'TIME'
     O          E            DET            1
     O                       SHCO#         4      5
     O                       SHFND         4     11
     O                       SHFYR         4     16
     O                       SHEMP#        4     27
     O                       SHSTAT              33
     O                       XXDATE              46 '  /  /  '
     O                       XXTIME              56 '  :  :  '
