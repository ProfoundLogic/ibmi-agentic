     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: CVTPASG
     H*****************************************************************
     H*  PROGRAMMER: C:32               PROJECT: MC131                *
     H*  CREATED: 02/27/96                                            *
     H*  THIS PROGRAM LOADS PCLPAS.                                   *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*  60    GENERAL CHAIN INDICATOR                                *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H**************************************************************************
     H*****************************************************************
     FFMDIL     IF   E           K DISK
     FUMPCP     UF   E           K DISK

     D                 DS
     D  XXLPAS                 1     10
     D  XXRGN                  1      1  0
     D  XXLGRP                 2     10

     C*
     C*  DIAGNOSIS CODE FILE
     C*
     C     DIKEY         KLIST
     C                   KFLD                    PCICD1
     C                   KFLD                    XXLOST            1
     C*
     C*** UPDATE UMPCP
     C                   READ      PPC01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C                   SUBST     PCPASG:1      XXRGNP            1
     C                   SUBST     PCICD1:1      XXWRK             1
     C     XXWRK         IFEQ      ' '
     C                   MOVE      'O'           XXLOST
     C                   ELSE
     C                   MOVE      'D'           XXLOST
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XXLPAS
     C*
     C     PCPASG        IFNE      *BLANKS
     C     PCPASG        ANDNE     '0000'
     C     DIKEY         CHAIN     PDI01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  DILGRP        XXLGRP                         INVALID DIAG CODE
     C                   MOVE      XXRGNP        XXRGN
     C                   END                                                    END *IN60
     C                   END
     C                   MOVEL(P)  XXLPAS        PCLPAS
     C*
     C                   UPDATE    PPC01
     C*
     C                   READ      PPC01                                  60
     C                   END                                                    END DOWEQ *IN60
     C*
     C*
     C                   MOVE      '1'           *INLR
