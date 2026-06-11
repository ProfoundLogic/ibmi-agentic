     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE FIXBLOPT                                                                       *
     H**************************************************************************************
     H*  Description:  Convert existing data from TASB's billing option field to           *
     H*                populate policy type field.                                         *
     H*                                                                                    *
     H*  Created: 01/04/10   Programmer C:190   Project:25WRE                              *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H**************************************************************************************
     FWDELP     UF   E             DISK
    C*
     C*
     C*----------------------------------------------------------------------
     C*  Read through WDELP
     C*----------------------------------------------------------------------
     C                   read      pel01
     C                   dow       not %eof

     c                   if        eldedp <> *blanks
     C                   eval      elmbcd = eldedp
     c                   clear                   eldedp
     C                   update    pel01
     C                   endif

     C                   read      pel01
     C                   enddo                                                  not %eof

     C                   eval      *inlr = *on
