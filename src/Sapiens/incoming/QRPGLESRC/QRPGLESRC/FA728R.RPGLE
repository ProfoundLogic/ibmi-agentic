     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA728R - Validates Bill Status; Returns BLST & BSDS      DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA728R                                            *
     H*    CREATED: 03/13/00  C:117                                   *
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
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@TBID            4
     C                   PARM                    @@KEY            10
     C                   PARM                    @@DESC           40
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ERR             1
     C*
     C     GTKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@TBID
     C                   KFLD                    @@KEY
     C*
     C     @@ERR         IFEQ      '1'
     C     @@RRN         CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTKEY         @@KEY
     C  N99              MOVEL(P)  GTDESC        @@DESC
     C                   ENDIF
     C*
     C     GTKY          CHAIN     PGT02                              99
     C  N99              MOVE      '1'           @@ERR
     C   99              MOVE      '0'           @@ERR
     C*
     C*  END OF PGM
     C*
     C                   MOVE      '1'           *INLR
