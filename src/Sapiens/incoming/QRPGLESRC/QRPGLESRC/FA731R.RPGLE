     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA731R - Checks to see if there is a Log Record;         DATA
     H*                Returns LGSTS
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA731R                                            *
     H*    CREATED: 03/11/00  C:117                                   *
     H*    CALLED FROM: F0350R                                        *
     H*    CALLES TO: NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*****************************************************************
     FFELGL4    IF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0           1
     C                   PARM                    @@SUB#            5 0           2
     C                   PARM                    @@FND             3 0           3
     C                   PARM                    @@FYR             3 0           4
     C                   PARM                    @@CASE            7 0           5
     C                   PARM                    @@EOB#            9 0           6
     C                   PARM                    @@RCN             3 0           7
     C                   PARM                    @@LSEQ            5 0           8
     C                   PARM                    @@STS             2             9
     C*
     C     LGKY1         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C                   KFLD                    @@RCN
     C                   KFLD                    @@LSEQ
     C*
     C     LGKY2         KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@EOB#
     C                   KFLD                    @@RCN
     C*
     C                   MOVE      *HIVAL        @@LSEQ
     C*
     C     LGKY1         SETGT     PLG01
     C     LGKY2         READPE    PLG01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     LGSTS         @@STS
     C                   ELSE
     C                   MOVE      *BLANKS       @@STS
     C                   ENDIF
     C*
     C*  END OF PGM
     C*
     C                   MOVE      '1'           *INLR
