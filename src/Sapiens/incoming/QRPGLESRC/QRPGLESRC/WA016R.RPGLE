     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0016R - GROUP HAS EXPIRED
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY AN WINDOW MESSAGE    *
     H*                STATING THAT UPDATE IS NOT AVAILABLE BECUASE   *
     H*                THE GROUP HAS EXPIRED.                         *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 07/18/97  PROGRAMMER: C:33  PROJECT: 60AND          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
     FWA016F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)

     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)

     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    XXFND             3 0
     C                   PARM                    XXNAME           60
     C                   PARM                    XXEXDT            7 0
     C*
     C*================================================================
     C*  Normal Programming Area
     C*================================================================
     C*
     C                   Z-ADD     XXFND         SCFND
     C                   MOVEL     XXNAME        SCNAME
     C*
     C                   Z-ADD     XXEXDT        SRDTI
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVEL     SRDTO         SCEXDT
     C*
     C                   EXFMT     WDW01
     C*
     C                   MOVE      *ON           *INLR
     C*
