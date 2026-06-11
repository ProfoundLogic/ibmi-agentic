     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKWMMUP - CHECK FOR EMPLOYERS THAT STILL HAVE WC000404
     H*
     FWDELL1    IF   E           K DISK
     FWMMUL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0
     D XXFORM          C                   CONST('WC000404  ')
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      *
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0

     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XKFORM
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    MUFORM        XKFORM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRTCD            3
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVEL     XXFORM        XKFORM
     C*
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXCEPT    HEAD
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     ELRTCD        IFEQ      QQRTCD
     C*
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
      *
      *  MAKE SURE EMPLOYER IS ACTIVE
      *
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      *OFF          *IN42
     C                   Z-ADD     5             X                 3 0
     C                   DO        5
     C     EF(X)         IFNE      *ZEROS
     C     CX(X)         IFNE      *ZEROS
     C                   MOVE      *ON           *IN42
     C                   LEAVE
     C                   ENDIF                                                  CX,X IFNE
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  EF,X IFNE
     C                   ENDDO                                                  DO 5
     C*
     C     *IN42         IFEQ      *OFF
     C*
     C     MUKY          CHAIN     PMU01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     MUEXFL        ANDNE     'E'
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN42 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELRTCD IFEQ
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           24 'CHKWMMUP'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           1
     O                                           35 'PGM TO CHECK IF WMMUP '
     O                                           53 'STILL INCLUDES THE'
     O                                           74 'WC000404 ENDORSEMENT'
     O          E            HEAD           2
     O                                           30 'CO#:'
     O                       XXCO#         4     34
     O                                           42 'FUND#:'
     O                       XXFND         4     46
     O                                           53 'FYR#:'
     O                       XXFYR         4     57
     O                                           64 'RTCD:'
     O                       QQRTCD              68
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O          E            DET            1
     O                       ELCO#         4      5
     O                       ELFND         4     11
     O                       ELFYR         4     16
     O                       ELEMP#        4     27
     O                       ELDIV         4     34
