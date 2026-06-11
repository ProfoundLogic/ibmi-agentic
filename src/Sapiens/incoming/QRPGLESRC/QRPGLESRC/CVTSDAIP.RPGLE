     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE CVTSDAIP - UPDATE SDAIP                                 *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*       60      GENERAL USAGE (CHAIN)                           *
     H*****************************************************************
     F*
     FSDAIP     UF   E             DISK
     FSDCMLA    UF   E           K DISK
     FSTCTL1    IF   E           K DISK
     F*

     C*
     C     CTKY          KLIST
     C                   KFLD                    CMRMCD
     C*
     C     CMKY          KLIST
     C                   KFLD                    AIKEY
     C                   KFLD                    AISEQ#
     C*
     C* DEPT/DOC ID MASTER FILE CONVERSION
     C*
     C                   READ      PAI01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C     CTKY          CHAIN     PCT01                              99
     C     *IN99         IFEQ      *OFF
     C     CTDEPG        IFNE      *BLANKS
     C                   MOVEL(P)  CTDEPG        AIDPGM
     C                   MOVEL(P)  CMKEY         AIDKEY
     C                   MOVEL     'Y'           CMFL01
     C                   ENDIF
     C                   ENDIF
     C                   UPDATE    PCM01
     C                   ENDIF
     C*
     C                   UPDATE    PAI01
     C                   READ      PAI01                                  99
     C                   ENDDO
     C*
     C*** END PROGRAM
     C                   MOVE      '1'           *INLR
