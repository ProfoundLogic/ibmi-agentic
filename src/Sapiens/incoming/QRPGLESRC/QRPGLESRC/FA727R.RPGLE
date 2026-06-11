     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA727R - Validates Fnd; Returns FNFND & FNNAME           DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA727R                                            *
     H*    CREATED: 03/14/00  C:117                                   *
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
     FFMFNP     IF   E             DISK
     FFMFNL     IF   E           K DISK
     F                                     RENAME(PFN01:PFN02)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@NAME           40
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ERR             1
     C*
     C* key list for subscriber master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C*
     C     @@ERR         IFEQ      '1'
     C     @@RRN         CHAIN     PFN01                              99
     C  N99              Z-ADD     FNFND         @@FND
     C  N99              MOVEL(P)  FNNAME        @@NAME
     C                   ENDIF
     C*
     C     FNKY          CHAIN     PFN02                              99
     C  N99              MOVE      '1'           @@ERR
     C  N99              MOVEL     FNNAME        @@NAME
     C   99              MOVE      '0'           @@ERR
     C*
     C*  END OF PGM
     C*
     C                   MOVE      '1'           *INLR
