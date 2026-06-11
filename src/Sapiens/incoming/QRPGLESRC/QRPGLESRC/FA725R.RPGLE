     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*
     H* API - FA725R - Validates Provider; Returns PDNAME & PDFEIN     DATA
     H*
     H*****************************************************************
     H*                                                               *
     H*    PROGRAM: FA725R                                            *
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
     FFMPDL     IF   E           K DISK

     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@FEIN            9 0
     C                   PARM                    @@MOD             3 0
     C                   PARM                    @@NAME           40
     C                   PARM                    @@ERR             1
     C*
     C* key list for provider demographics file
     C*
     C     PDKY          KLIST
     C                   KFLD                    @@FEIN
     C                   KFLD                    @@MOD
     C*
     C     PDKY1         KLIST
     C                   KFLD                    @@FEIN
     C*
     C     @@FEIN        IFNE      *ZEROS
     C     @@MOD         ANDNE     *ZEROS
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      '0'           @@ERR
     C                   MOVEL     *BLANKS       @@NAME
     C                   ELSE
     C                   MOVE      '1'           @@ERR
     C                   MOVEL     PDNAME        @@NAME
     C                   ENDIF                                                  *IN99 EQ *ON   S
     C                   ENDIF                                                  @@FEIN NE *ZEROS
     C*
     C     @@FEIN        IFNE      *ZEROS
     C     @@MOD         ANDEQ     *ZEROS
     C     PDKY1         CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVE      '0'           @@ERR
     C                   MOVEL     *BLANKS       @@NAME
     C                   ELSE
     C                   MOVE      '1'           @@ERR
     C                   MOVEL     PDNAME        @@NAME
     C                   ENDIF                                                  *IN99 EQ *ON   S
     C                   ENDIF                                                  @@FEIN NE *ZEROS
     C*
     C*  END OF PGM
     C*
     C                   MOVE      '1'           *INLR
