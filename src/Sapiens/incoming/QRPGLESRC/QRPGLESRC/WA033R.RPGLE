     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0033R - DISPLAY MESSAGE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A WINDOW WHICH WILL  *
     H*                LET THE USER KNOW THAT THE YEAR ENTERED IS NOT *
     H*                1 YEAR LESS THAN THE SYSTEM YEAR.              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/01/98  PROGRAMMER: C:33  PROJECT: WC871          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     FWA033F    CF   E             WORKSTN
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
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   EXFMT     WDW01
     C*
     C                   MOVE      *ON           *INLR
     C*
