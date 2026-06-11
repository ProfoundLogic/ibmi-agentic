     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  WA051R - Find out if EOMB has any prev recon payments pending*
     H*                                                               *
     H*  Called from: F0395R01                                        *
     H*                                                               *
     H*  Created: 11/06/03  C:79   Project: FS269                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  00/00/00 C:00 Project: xxxxxx                                *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     FWARPLA    IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*----------------------------------------------------------------
     C*  Key list for the Payment File
     C*
     C     RPKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@EOB#            9
     C                   PARM                    @@FEIN            9
     C                   PARM                    @@MOD             3
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@RCN#            3
     C*
     C     *LIKE         DEFINE    RPCO#         KKCO#
     C     *LIKE         DEFINE    RPFND         KKFND
     C     *LIKE         DEFINE    RPFYR         KKFYR
     C     *LIKE         DEFINE    RPCASE        KKCASE
     C     *LIKE         DEFINE    RPEOB#        KKEOB#
     C     *LIKE         DEFINE    RPEOBS        KKRCN#
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@EOB#        KKEOB#
     C                   MOVE      @@RCN#        KKRCN#
     C                   MOVE      @@FEIN        XXFEIN            9 0
     C                   MOVE      @@MOD         XXMOD             3 0
     C*
     C*  See if there is a Payment at a previous recon
     C*
     C     KKRCN#        DOWGE     0
     C     RPKY          CHAIN     PRP01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C     RPSTAT        ANDNE     'D'
     C     RPFEIN        ANDEQ     XXFEIN
     C     RPMOD         ANDEQ     XXMOD
     C                   MOVEL     'P'           @@RTCD
     C                   LEAVE
     C                   ENDIF
     C*
     C                   SUB       1             KKRCN#
     C                   ENDDO
     C*
     C*  End of pgm
     C*
     C                   MOVE      *ON           *INLR
     C*
