     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA729R - Checks to see if the Fund has expired           DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA729R                                            *
     H*    CREATED: 03/16/00  C:117                                   *
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
     FFMFNL     IF   E           K DISK

     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@NAME           40
     C                   PARM                    @@ERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C* key list for WCMC fund/group master file
     C*
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@SUB#
     C                   KFLD                    @@FND
     C*
     C     FNKY          CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         HLSYDT            7 0
     C     FNEXDT        IFNE      *ZEROS
     C     FNEXDT        ANDLT     HLSYDT
     C                   MOVE      '1'           @@ERR
     C                   ELSE
     C                   MOVE      '0'           @@ERR
     C                   ENDIF                                                  FNEXDT LT HLSYDT
     C                   ENDIF                                                  *IN99 = *OFFSYDT
     C*
     C*  END OF PGM
     C*
     C                   MOVE      '1'           *INLR
