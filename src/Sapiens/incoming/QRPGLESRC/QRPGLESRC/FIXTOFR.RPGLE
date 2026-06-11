     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  FIXTPFR - FIX EOFRDT AND EOTODT.              C:62 03/13/00  *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFAEOP     IF   E             DISK
     F                                     INFDS(DBFDS)
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D DBFDS           DS
     D  DBRRN                397    400B 0

     C*
     C                   READ      PEO01                                  99
     C*
     C     *IN99         DOWEQ     *OFF
     C*
     C     EOSYS         IFEQ      'M'
     C     EOSYS         OREQ      'U'
     C*
     C                   CALL      'FA724R'
     C                   PARM      DBRRN         ##RRN             9 0           1
     C                   PARM      *BLANKS       ##RTCD            1             2
     C*
     C                   ENDIF
     C*
     C                   READ      PEO01                                  99
     C*
     C                   ENDDO
     C*
     C                   SETON                                        LR
     C*
