     H* (C) Copyright Progressive Data Solutions, 2000
     H/TITLE LODFSDTAR                                                                      *
     H**************************************************************************************
     H*  Description:  Prompt to display window for selecting FS data                      *
     H*                                                                                    *
     H*  Created: 11/19/00   Programmer C:79    Project:FSMNT                              *
     H**************************************************************************************
     H*  Programmer notes:                                                                 *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FLODFSDTAF CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF

     D*
     D qqst            s              2                                         New FS Eff. Date
     D qqsel           s              1                                         New FS Eff. Date
     C     *entry        plist
     C                   parm                    qqst
     C                   parm                    qqsel

     C                   eval      scst=qqst

     C                   exfmt     scr01

     C                   eval      qqsel=scsel

     C                   eval      *inlr=*on
     C                   return
     C*
