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
     H*    Load DQAMTR from DQAMT.                                    *
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
     FWADQL     UF   E           K DISK
      *
      *  Process all Net Deductible Recovery records
      *

     C                   READ      PDQ01                                  60
      *
     C     *IN60         DOWEQ     '0'
      *
     C                   Z-ADD     DQAMT         DQAMTR
     C                   UPDATE    PDQ01
      *
     C                   READ      PDQ01                                  60
      *
     C                   ENDDO
      *
      * END PROGRAM
      *
     C                   SETON                                        LR
      *
