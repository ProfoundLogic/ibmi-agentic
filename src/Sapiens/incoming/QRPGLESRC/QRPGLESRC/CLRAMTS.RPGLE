     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  CLRAMTS - CLR THE AMOUNT FIELDS               C:62 10/24/02  *
     H*                                                NWMNT          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FFMPRPUC   UF   E             DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*
     C                   READ      PPR01                                  99
     C     *IN99         DOWEQ     *OFF
     C*
     C                   Z-ADD     *ZEROS        PRMRA
     C                   Z-ADD     *ZEROS        PRANAM
     C                   Z-ADD     *ZEROS        PRPROF
     C                   Z-ADD     *ZEROS        PR92N1
     C                   Z-ADD     *ZEROS        PR92N2
     C                   Z-ADD     0010101       PREFDT
     C*
     C                   UPDATE    PPR01
     C*
     C                   READ      PPR01                                  99
     C                   ENDDO
     C*
     C                   SETON                                        LR
