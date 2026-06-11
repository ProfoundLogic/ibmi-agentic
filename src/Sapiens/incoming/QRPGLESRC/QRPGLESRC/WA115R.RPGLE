     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - to check Employer Policy Type                             WCMC
     H*
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: WA111R                                            *
     H*    CREATED: 12/12/01  C:62                                    *
     H*    DESCRIPTION: API to check Employer Policy Type             *
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
     FWMCML1    IF   E           K DISK
     FWDELL     IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C* Parameter Lists
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#                           I
     C                   PARM                    @@FND                           I
     C                   PARM                    @@FYR                           I
     C                   PARM                    @@CASE                          I
     C                   PARM                    @@EXMF                          I/O
     C                   PARM                    @@RTCD            1             I/O
     C*****************************************************************
     C* Key  Lists
     C*****************************************************************
     C*
     C* Case Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C* Employer Detail File
     C*
     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C*
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    CMEMP#        KKEMP#
     C     *LIKE         DEFINE    CMDIV         KKDIV
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
     C     *LIKE         DEFINE    ELEXMF        @@EXMF
     C*
     C                   Z-ADD     @@CO#         KKCO#
     C                   Z-ADD     @@FND         KKFND
     C                   Z-ADD     @@FYR         KKFYR
     C                   Z-ADD     @@CASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     CMEMP#        KKEMP#
     C                   Z-ADD     CMDIV         KKDIV
     C     ELKY          CHAIN     PEL01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  ELEXMF        @@EXMF
     C                   ELSE
     C                   MOVEL     'F'           @@RTCD
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     'F'           @@RTCD
     C                   ENDIF
     C*
     C                   RETURN
