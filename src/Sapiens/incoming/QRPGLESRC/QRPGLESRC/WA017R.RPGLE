     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      * ***************************************************************
      /TITLE W0017R - RECORD HAS BEEN UPDATED BY SOMEONE ELSE.
      * ***************************************************************
      *  DESCRIPTION:  THIS PROGRAM WILL DISPLAY AN WINDOW MESSAGE    *
      *                STATING THAT SOMEEON ELSE HAS ALREADY UPADTED  *
      *                THIS RECORD AND THAT THE USER SHOULD EXIT      *
      *                THE SCREEN AND RE-ENTER THEIR INFORMATION.     *
      *                                                               *
      *  CALLED FROM:                                                 *
      *                                                               *
      *  CALLS TO:                                                    *
      *    NONE                                                       *
      *                                                               *
      *  CREATED: 07/21/97  PROGRAMMER: C:47  PROJECT: 60ANC          *
      * ***************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      * ***************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      * ***************************************************************
     FWA017F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D XC01            C                   CONST('someone else')

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQUSER           10
     C                   PARM                    QQRTCD            1
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   MOVEL(P)  XC01          SCUSR
     C*
     C     QQUSER        IFNE      *BLANK
     C                   MOVEL(P)  QQUSER        SCUSR
     C                   END
     C*
     C                   EXFMT     WDW01
     C*
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C*
