     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIMIFL     UF   E           K DISK
     F*

     D                 DS
     D  IFLFM                  1     30
     D  XLIB                   1     10
     D  XFIL                  11     20
     D  XMBR                  21     30

     I*

     C     IFKY          KLIST
     C                   KFLD                    FRMBR            10
     F*
     C*
     C     *LOVAL        SETLL     PIF01
     C                   READ      PIF01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVEL     'QAFFRCVS'    FROMFL           10
     C                   MOVE      'AV'          FROMFL           10
     C*
     C                   MOVEL     FROMFL        XFIL
     C                   UPDATE    PIF01
     C*
     C                   READ      PIF01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *ON           *INLR
