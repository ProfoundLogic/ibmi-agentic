     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FIOCAERP   O    E             DISK

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     F*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     F*

     C     *ENTRY        PLIST
     C                   PARM                    FSTTXT
     C*
     C                   WRITE     IOCAER
     C                   RETURN
     C*
     C                   MOVE      *ON           *INLR
