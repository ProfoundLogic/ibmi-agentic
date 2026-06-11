     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA008R - DISPLAY BATCH MESSAGE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A WINDOW WHICH WILL  *
     H*                LET THE USER KNOW THAT THE REQUESTED           *
     H*                OPERATION HAS BEEN SUBMITTED TO BATCH.         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 06/11/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FWA008F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@KL              1
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   EXFMT     WDW01
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE      *INKL         @@KL
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C                   MOVE      *ON           *INLR
     C*
