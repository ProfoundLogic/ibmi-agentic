     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  WA045R - Retrieve EDI Log next available seq#.               *
     H*                                                               *
     H*  Called from: W0070R6                                         *
     H*               W8070R3                                         *
     H*               W8070R4                                         *
     H*                                                               *
     H*  Created: 09/22/00  C:15   Project: WC468                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  00/00/00 C:00 Project: xxxxxx                                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FWMLGL6    IF   E           K DISK
     F*

     I*

     C*----------------------------------------------------------------
     C*  Key list for the EDI Log History File
     C*
     C     LGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@SEQ#            5
     C                   PARM                    @@RTCD            1
     C*
     C     *LIKE         DEFINE    LGCO#         XXCO#
     C     *LIKE         DEFINE    LGFND         XXFND
     C     *LIKE         DEFINE    LGFYR         XXFYR
     C     *LIKE         DEFINE    LGCASE        XXCASE
     C     *LIKE         DEFINE    LGDSQH        XXSEQ#
     C*
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C*
     C*  Retrieve the last EDI log record, based on seq#.
     C*
     C     LGKY          SETGT     PLG01
     C     LGKY          READPE    PLG01                                  99
     C*
     C     *IN99         IFEQ      *ON
     C                   Z-ADD     0             XXSEQ#
     C                   ELSE
     C                   Z-ADD     LGDSQH        XXSEQ#
     C                   ENDIF
     C*
     C*  Add 1 to the seq# and move into the parm to pass back to the
     C*    calling pgm.
     C*
     C                   ADD       1             XXSEQ#
     C                   MOVE      XXSEQ#        @@SEQ#
     C*
     C*  End of pgm
     C*
     C                   MOVE      *ON           *INLR
     C*
