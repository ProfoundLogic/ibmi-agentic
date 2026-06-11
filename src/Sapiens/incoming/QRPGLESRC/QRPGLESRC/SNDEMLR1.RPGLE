     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     F*
     F*
     F*

     D                 DS
     D  TIMEID                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
      * * *  FILE INFORMATION DATA STRUCTURE
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      * * *  PROGRAM STATUS DATA STRUCTURE
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    XXSUBJ           55
     C                   PARM                    XXEML            40
     C                   PARM                    XXATCH           40
     C                   PARM                    XXTXT           255
     C                   PARM                    XXRTCD            1
     C*
     C     SNDEML        PLIST
     C                   PARM                    XXSUBJ           55
     C                   PARM                    XXEML            40
     C                   PARM                    XXATCH           40
     C                   PARM                    XXTXT           255
     C                   PARM                    XXRTCD            1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*
     C*
     C                   CALL      'SNDEMLC1'    SNDEML
     C*
      *
     C                   MOVE      *ON           *INLR
     C*
