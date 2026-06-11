     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SC019R - SEND A DARTS DOCUMENT                           *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H* SCR01:                                                        *
     H*****************************************************************
     FSC019F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     INFDS(FIDS01)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXRECF                 1     10
     D  XXSCR#                 2      5
     D                 DS
     D  XXFLDR                 1     80
     D  SCFLD1                 1     40
     D  SCFLD2                41     80
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D  CURSOR               370    371B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
      *------------------------*
      *   PARAMETER LIST FOR CALLED PROGRAM
      *------------------------*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFLDR           80
     C                   PARM                    QQCALL            1
     C*
     C                   MOVEL     QQDCID        SCDCID
     C                   MOVEL     QQFLDR        XXFLDR
     C*
     C*   LOAD THE RECORD FORMAT INFO FOR FIRST TIME
     C*
     C                   MOVEL     Q2FILE        SCFILE
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'WDW01'       Q2RECF
     C*
     C     Q2RECF        IFEQ      'WDW01'
     C                   EXFMT     WDW01
     C                   END                                                    END WDW01
     C*
      *===============================================================
      * TERMINATION OF PROGRAM
      *
     C     TERM          TAG
     C*
     C                   MOVE      '1'           *INLR
     C*
