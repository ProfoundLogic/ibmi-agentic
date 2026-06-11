     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*TITLE SC712R - BUILD REMARKS PRINT WORKFILE SWCMP              *
     H* DESCRIPTION:                                                  *
     H*     Based on the Remark Code from the Remarks File, get the   *
     H*     corresponding summary code file and load into the         *
     H*     the remark's summary code field.                          *
     H*****************************************************************
     H*  CREATED: 01/26/94                                            *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   50    READ DARTS REMARK FILE                                *
     H*   60    GENERAL CHAIN                                         *
     H*****************************************************************
     FSDCML1    UF   E           K DISK
     FSTCTL1    IF   E           K DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  DARTS REMARK FILE
     C     CTKY          KLIST
     C                   KFLD                    CMRMCD
     C*
     C*** Read DARTS Remark file
     C                   READ      PCM01                                  50
     C     *IN50         DOWEQ     '0'
     C*
     C*** Based on Remark's Type Code
     C***             Chain to Remark Type Code Table File (STCTL1)
     C***             Get Summary Code (CTSMCD)
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       CMSMCD
     C                   ELSE
     C*
     C*** Write workfile record with retrieved summary code
     C                   MOVE      CTSMCD        CMSMCD
     C                   END
     C                   UPDATE    PCM01
     C*
     C*** Read next DARTS Remark Record
     C                   READ      PCM01                                  50
     C                   END
     C*
     C*** END PROGRAM
     C*
     C                   MOVE      '1'           *INLR
