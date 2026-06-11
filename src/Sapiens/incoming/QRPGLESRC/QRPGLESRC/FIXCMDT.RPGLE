     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE PROGRAM TO FIX CMDT
     H*****************************************************************
     H* Description:  This pgm was written to correct CMDT which was  *
     H*   loaded incorrectly by FA704R.                               *
     H*                                                               *
     H*****************************************************************
     FFMCML1    UF   E           K DISK

     C**************************************************************************
     C*
     C*  Start of main loop
     C*
     C                   READ      PCM01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL(P)  CMDT          XXCC              1
     C     CMDT          IFGT      1000300
     C     XXCC          ANDNE     '0'
     C     XXCC          ANDNE     '1'
     C*
     C                   MOVEL     '1'           CMDT
     C                   UPDATE    PCM01
     C*
     C                   END
     C*
     C                   READ      PCM01                                  60
     C                   END
     C*
     C                   SETON                                        LR
     C**************************************************************************
