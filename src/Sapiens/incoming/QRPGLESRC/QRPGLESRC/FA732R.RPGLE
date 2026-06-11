     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA731R - Checks the Pre-Cert master; Returns RRN         DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA732R                                            *
     H*    CREATED: 03/11/00  C:117                                   *
     H*    CALLED FROM: F0350R                                        *
     H*    CALLS TO: NONE                                            *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*****************************************************************
     FUMPCL11   IF   E           K DISK
     F                                     INFDS(DBFDB)

     D DBFDB           DS
     D  DBRRNB               397    400B 0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@CERT           15
     C                   PARM                    @@EXT             3 0
     C                   PARM                    @@RRN             9 0
     C*
     C     PCKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
     C                   KFLD                    @@CERT
     C                   KFLD                    @@EXT
     C*
     C     PCKY          CHAIN     PPC01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNB        @@RRN
     C                   ELSE
     C                   Z-ADD     0             @@RRN
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C* END OF PGM
     C                   MOVE      '1'           *INLR
