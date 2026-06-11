     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *****************************************************************
      *
      *  API - FA200R - VALIDATE DIAGNOSIS CODE & TYPE
      *
      *  CREATED    : 09/17/97  C:47
      * ***************************************************************
      *  REVISIONS:                                                   *
      * ***************************************************************
     FFMDIL     IF   E           K DISK
      *

     C     DIKY          KLIST
     C                   KFLD                    XXICD9
     C                   KFLD                    XXLOST
      *
     C     *ENTRY        PLIST
     C                   PARM                    XXICD9
     C                   PARM                    XXLOST
     C                   PARM                    QQDESC
     C                   PARM                    QQRTCD            1
      *
     C     *LIKE         DEFINE    DIICD9        XXICD9
     C     *LIKE         DEFINE    DILOST        XXLOST
     C     *LIKE         DEFINE    DIDSC1        QQDESC
     C                   MOVEL     'N'           QQRTCD                         NOT VALID
      *
      *   VALIDATE DIAGNOSIS CODE AND TYPE
      *
     C     XXICD9        IFNE      *BLANK
     C     XXLOST        ANDNE     *BLANK
      *
     C     DIKY          CHAIN     PDI01                              99
     C     *IN99         IFEQ      *OFF
     C     DISTAT        ANDEQ     'A'
     C                   MOVEL(P)  DIDSC1        QQDESC
     C                   MOVEL(P)  'Y'           QQRTCD                         VALID
     C                   ENDIF
      *
     C                   ENDIF                                                  XXICD9 NE BLANK
     C                   MOVE      *ON           *INLR
