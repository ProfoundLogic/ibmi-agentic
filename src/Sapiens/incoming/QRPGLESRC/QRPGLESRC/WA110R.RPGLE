     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* API - VALIDATE CHECK# BY CASE                                   WCMC
     H*
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA110R                                            *
     H*    CREATED: 02/22/01  C:62                                    *
     H*    DESCRIPTION: VALIDATE CHECK# BY CASE                       *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       99      GENERAL USAGE (CHAIN)                           *
     H*                                                               *
     H*****************************************************************
     F*
     FWDPALB    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  MASTER FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCHK#
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#                           I
     C                   PARM                    @@FND                           I
     C                   PARM                    @@FYR                           I
     C                   PARM                    @@CASE                          I
     C                   PARM                    @@CHK#                          I
     C                   PARM                    @@RTCD            1             I/O
      *
     C     *LIKE         DEFINE    PACO#         KKCO#
     C     *LIKE         DEFINE    PAFND         KKFND
     C     *LIKE         DEFINE    PAFYR         KKFYR
     C     *LIKE         DEFINE    PACASE        KKCASE
     C     *LIKE         DEFINE    PACHK#        KKCHK#
     C     *LIKE         DEFINE    PACO#         @@CO#
     C     *LIKE         DEFINE    PAFND         @@FND
     C     *LIKE         DEFINE    PAFYR         @@FYR
     C     *LIKE         DEFINE    PACASE        @@CASE
     C     *LIKE         DEFINE    PACHK#        @@CHK#
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C                   Z-ADD     @@CHK#        KKCHK#
     C*
     C     PAKY          CHAIN     PPA01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL     'F'           @@RTCD
     C                   ENDIF
     C*
     C                   MOVE      '1'           *INLR
