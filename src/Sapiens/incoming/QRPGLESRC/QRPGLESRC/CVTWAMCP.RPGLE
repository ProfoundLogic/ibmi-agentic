     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CVTWMPRP
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Load MCEMP# and MCDIV from claim master file (WMCMP)       *
     H*                                                               *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  08/10/98 C:87 PROJECT:WC591                                  *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/18/99  C:87  PROJECT: WC591                                *
     H*                 Only update records that do not have an emplr *
     H*                 name already assigned.                        *
     H*                                                               *
     H*****************************************************************
     F*
     FWAMCP     UF   E             DISK
     FWMCML1    IF   E           K DISK
     F*

     C     *LIKE         DEFINE    CMCO#         KKCO#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C*
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C*  Process all Multiple claim records
     C*
     C                   READ      PMC01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C***  BEGIN ADD  ***  11/18/99  **********************************
     C*  Only process if MCEMP#=0
     C     MCEMP#        IFEQ      0
     C***  END   ADD  ***  11/18/99  **********************************
     C*
     C                   Z-ADD     MCCO#         KKCO#
     C                   Z-ADD     MCFND         KKFND
     C                   Z-ADD     MCFYR         KKFYR
     C                   MOVE      MCCASE        KKCASE
     C*
     C     CMKY          CHAIN     PCM01                              61
     C     *IN61         IFEQ      *OFF
     C                   Z-ADD     CMEMP#        MCEMP#
     C                   Z-ADD     CMDIV         MCDIV
     C*
     C                   UPDATE    PMC01
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  11/18/99  **********************************
     C                   ENDIF                                                  MCEMP#=0
     C***  END   ADD  ***  11/18/99  **********************************
     C*
     C                   READ      PMC01                                  60
     C*
     C                   ENDDO
     C*
     C* END PROGRAM
     C*
     C                   SETON                                        LR
     C*
