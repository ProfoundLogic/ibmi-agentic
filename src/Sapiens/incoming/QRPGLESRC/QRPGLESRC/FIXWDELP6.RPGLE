     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE FIXWDELP5 - UPDATE MAINTENANCE FIELDS TO LAST BILL RUN
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FWDELL     UF   E           K DISK
     FWDEHL6    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C*
     C* EMPLOYER DETAIL KEY
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  EMPLOYER HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EHKY          SETGT     PEH01
     C     EHKY          READPE    PEH01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EHFAF         ANDEQ     'A'
     C     EHKY          READPE    PEH01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C     ELPTDT        ANDNE     EHPTDT
     C                   Z-ADD     ELPTDT        ELIDT
     C                   Z-ADD     EHPTDT        ELPTDT
     C                   MOVEL(P)  Q1PGM         ELPGM
     C                   UPDATE    PEL01
     C                   ENDIF
     C*
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   MOVE      *ON           *INLR
     C*
