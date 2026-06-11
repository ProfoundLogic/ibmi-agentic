     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
      *
      *** THIS PROGRAM GETS THE RECIPIENT FILE INFO AND PASSES BACK
      *** TO CALLING PROGRAM.  DOES NOT SET ON LR, INSTEAD USES RETRN
      *
      *****************************************************************
      *
      *
     FIMFRL2    IF   E           K DISK
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ****

     C     RCKY          KLIST
     C                   KFLD                    QQKEY            40
     C                   KFLD                    QQTYPE            2
     C*
     C*
      *
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQTYPE            2
     C                   PARM                    QQRCID           10
     C                   PARM                    QQRTCD            1
     C***
     C*
     C*
     C***
     C     RCKY          CHAIN     PFR01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     FRRCID        QQRCID           10
     C                   MOVEL     *BLANK        QQRTCD
     C*
     C                   ELSE
     C*
     C                   MOVEL     *BLANK        QQRCID           10
     C                   MOVEL     '1'           QQRTCD
     C*
     C*
     C                   ENDIF
     C*
     C     RETN          TAG
     C                   RETURN
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
     C*
