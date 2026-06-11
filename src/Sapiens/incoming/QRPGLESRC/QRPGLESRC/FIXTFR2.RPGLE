     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FSMDLL     UF   E           K DISK
     FTFRXREFL  IF   E           K DISK
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     C     FX            KLIST
     C                   KFLD                    DLKEY
     C*
     C     DLK           KLIST
     C                   KFLD                    XRFKEY
     C*
     C*
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      *ALL'_'       LINE            198
     C                   READ      PDL01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     FX            CHAIN     FIX                                61
     C*
     C*** LOOP CMS
     C     *IN61         IFEQ      *OFF
     C                   MOVE      XRFKEY        SVKEY            40
     C                   MOVE      XRFSQ#        SVDSQ#            5 0
     C*
     C                   MOVE      XRTKEY        DLKEY
     C                   ADD       1             XXCNT             9 0
     C                   MOVEL     'FIXTFR2 '    DLPGM
     C*
     C                   UPDATE    PDL01
     C                   ENDIF
     C                   READ      PDL01                                  60
     C                   ENDDO
     C                   MOVE      *ON           *INLR
     C*
