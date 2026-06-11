     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE CVTWMBIP60
     H*****************************************************************
     H*                                                               *
     H*  PROGRAM DESCRIPTION                                          *
     H*                                                               *
     H*    Convert the co# in WTDCP, WTEMP and WTERP.                 *
     H*                                                               *
     H*                                                               *
     H*  CREATED:                                                     *
     H*                                                               *
     H*  04/16/98 C:15 PROJECT                                        *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*      60    READ/CHAIN Bank Info File WMBIL                    *
     H*      61    READ/CHAIN Fund Auxiliary WDF2L                    *
     H*      62    READ/CHAIN Signature Master WMSGL                  *
     H*      63    READ/CHAIN Fund Master WMFNL                       *
     H*                                                               *
     H*****************************************************************
      *
     FWTDCP     UF   E             DISK
     FWTEMP     UF   E             DISK
     FWTERP     UF   E             DISK
      *

     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
      *
     C                   MOVE      QQCO#         XXCO#             3 0
      *
      *  Convert files
      *
     C                   READ      PDC01                                  60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXCO#         DCCO#
     C                   UPDATE    PDC01
     C                   READ      PDC01                                  60
     C                   ENDDO
      *
     C                   READ      PEM01                                  60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXCO#         EMCO#
     C                   UPDATE    PEM01
     C                   READ      PEM01                                  60
     C                   ENDDO
      *
     C                   READ      PER01                                  60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXCO#         ERCO#
     C                   UPDATE    PER01
     C                   READ      PER01                                  60
     C                   ENDDO
      *
      * END PROGRAM
      *
     C                   SETON                                        LR
      *
