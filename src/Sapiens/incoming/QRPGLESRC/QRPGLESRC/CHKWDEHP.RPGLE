     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CHKWDEHP - CHECK FOR WDEHP RECORDS NOT IN BALANCE
     H*
     FWDEHL6    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSDATE                 7     12  0

     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *LIKE         DEFINE    EHCO#         XXCO#
     C     *LIKE         DEFINE    EHFND         XXFND
     C     *LIKE         DEFINE    EHFYR         XMFYR
     C     *LIKE         DEFINE    EHFYR         XXFYR
     C     *LIKE         DEFINE    EHEMP#        XXEMP#
     C     *LIKE         DEFINE    EHDIV         XXDIV
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
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
     C                   MOVE      QQFYR         XMFYR
     C*
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   Z-ADD     0             XXFYR
     C*
     C                   TIME                    DSDTTM
     C*
     C                   EXCEPT    HEAD
     C*
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EHFYR         IFGE      XMFYR
     C*
     C     EHFYR         IFNE      XXFYR
     C     EHEMP#        ORNE      XXEMP#
     C     EHDIV         ORNE      XXDIV
     C*
     C                   Z-ADD     EHFYR         XXFYR
     C                   Z-ADD     EHEMP#        XXEMP#
     C                   Z-ADD     EHDIV         XXDIV
     C                   Z-ADD     EHWAPR        XXWAPR           11 2
     C                   ENDIF
     C*
     C     EHWAPR        SUB       XXWAPR        XXAMT            11 2
     C*
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C     XXAMT         IFNE      EHXAPR
     C                   Z-ADD     EHTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C                   Z-ADD     EHWAPR        XXWAPR
     C*
     C                   ENDIF                                                  EHFYR IFGE
     C*
     C     EHKY          READE     PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     OQSYSPRT   E            HEAD           2 06
     O                                           14 'PROGRAM NAME:'
     O                                           24 'CHKWDEHP'
     O                                           70 'RUN DATE:'
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                                           24 'PGM TO CHECK IF WDEHP '
     O                                           42 'CHANGE # IS WRONG'
     O                                           50 'CO#:'
     O                       XXCO#         4     54
     O                                           62 'FUND#:'
     O                       XXFND         4     66
     O                                           73 'FYR#:'
     O                       XMFYR         4     77
     O          E            HEAD           2
     O                                            5 'CO#'
     O                                           11 'FUND'
     O                                           16 'FYR'
     O                                           27 'EMPLOYER#'
     O                                           34 'DIV'
     O                                           42 'STATUS'
     O                                           59 'EHWAPR'
     O                                           76 'EHXAPR'
     O                                           93 'PRIOR WAPR'
     O                                          110 'CORRECT XAPR'
     O                                          124 'EHTRDT'
     O          E            DET            1
     O                       EHCO#         4      5
     O                       EHFND         4     11
     O                       EHFYR         4     16
     O                       EHEMP#        4     27
     O                       EHDIV         4     34
     O                       EHFAF               39
     O                       EHWAPR        J     59
     O                       EHXAPR        J     76
     O                       XXWAPR        J     93
     O                       XXAMT         J    110
     O                       XXTRDT             124 '  /  /    '
