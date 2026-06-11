     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      *****************************************************************
      *
      *  API - DA200R - Validate DARTS Remark Type code & return desc
      *
      *  CREATED    : 01/12/98  C:05
      * ***************************************************************
      *  REVISIONS:                                                   *
      * ***************************************************************
     FSTCTL1    IF   E           K DISK
      *

     C     CTKY          KLIST
     C                   KFLD                    XXRMCD
      *
     C     *ENTRY        PLIST
     C                   PARM                    XXRMCD
     C                   PARM                    QQDESC
     C                   PARM                    QQRTCD            1
      *
     C     *LIKE         DEFINE    CTRMCD        XXRMCD
     C     *LIKE         DEFINE    CTDESC        QQDESC
     C                   MOVEL     'N'           QQRTCD                         NOT VALID
      *
      *   VALIDATE DARTS REMARK TYPE CODE
     C     XXRMCD        IFNE      *BLANK
      *
     C     CTKY          CHAIN     PCT01                              99
     C     *IN99         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  CTDESC        QQDESC
     C                   MOVEL(P)  'Y'           QQRTCD                         VALID
     C                   ENDIF
      *
     C                   ENDIF                                                  XXRMCD NE BLANK
     C                   MOVE      *ON           *INLR
