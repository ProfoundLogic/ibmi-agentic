     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE CVTRMCD1 - CONVERT REMARK CODE IN WMDOP                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*****************************************************************
     F*
     FSTCTL     IF   E           K DISK
     FWMDOP     UF   E             DISK
     F*

     C*
     C*
     C* REMARK TYPE FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXTYPE            2
     C*
     C* DEPT/DOC ID MASTER FILE CONVERSION
     C*
     C                   READ      PDO01                                  60
     C     *IN60         DOWEQ     '0'
     C                   MOVEL     DORMTY        XXTYPE
     C     CTKY          CHAIN     PCT01                              50
     C                   MOVE      *BLANKS       DORMCD
     C     *IN50         IFEQ      '0'
     C     XXTYPE        ANDNE     *BLANKS
     C                   MOVEL     CTRMCD        DORMCD
     C                   ELSE
     C                   MOVEL     'XXXXX'       DORMCD
     C                   END
     C*
     C                   UPDATE    PDO01
     C*
     C                   READ      PDO01                                  60
     C*
     C                   END
     C*
     C*** END PROGRAM
     C                   MOVE      '1'           *INLR
