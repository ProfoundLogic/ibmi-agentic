     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     FAOLOGP    O    E             DISK

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     **

     C     *ENTRY        PLIST
     C                   PARM                    WHEN              7
     C                   PARM                    QDATE             6
     C                   PARM                    QDAY              2
     C                   PARM                    QMONTH            2
     C                   PARM                    QYEAR             2
     C                   PARM                    QTIME            12
     C                   PARM      *BLANKS       RTN               2
     **
     C                   MOVE      *BLANKS       RTN
     C                   WRITE     RAOLOG
     C*
     C                   RETURN
     C                   MOVE      *ON           *INLR
