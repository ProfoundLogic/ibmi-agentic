     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA730R - Returns BEDEPG (detail pgm from BEAST record)   DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA730R                                            *
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
     FFMBEL     IF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@AST             2             1
     C                   PARM                    @@SYS             1             2
     C                   PARM                    @@BTYP            1             3
     C                   PARM                    @@DEPG           10             4
     C*
     C     BEKY          KLIST
     C                   KFLD                    @@AST
     C                   KFLD                    @@SYS
     C                   KFLD                    @@BTYP
     C*
     C     BEKY          CHAIN     PBE01                              99
     C                   MOVEL     BEDEPG        @@DEPG
     C*
     C*  END OF PGM
     C                   MOVE      '1'           *INLR
