     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    H/title Print Error log
     H option(*nodebugio: *srcstmt)
     H*
     H* ============================================================= *
     H*BLDFSPGM  - Validate the file for copy                         *
     H*                                                               *
     H*  DESCRIPTION: -This program validates the file name to copy   *
     H*                for mode = 'F'.                                *
     H*               -In mode = 'A' program writes the WXCPP workfile*
     H*                to hold error description during the copy data.*
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    BLDFSDATC                                                  *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 08/24/10  PROGRAMMER: C:220  PROJECT: FS324         *
     H*                                                               *
     H* ============================================================= *
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* ============================================================= *
     f*
     ffncpp     if   e             disk
     ffxcpp     o    e             disk    rename(pcp01:pcp02)
     f*
      *  ============================================================= *
     c     *entry        plist
     c                   parm                    @@file           10
     c                   parm                    @@flg             1
     c                   parm                    @@mode            1
     c                   parm                    @@errtx         132
     c*
     c* Loop thru fncpp to detremine the Flag = 'Y'
     c*
     c                   if        @@mode = 'F'
     c                   eval      @@flg = 'Y'
     c                   read      pcp01                                  60
     c                   dow       *in60 = *off
     c*
     c                   if        @@file = cpname
     c*
     c                   eval      @@flg = 'N'
     c                   leave
     c                   endif
     c*
     c                   read      pcp01                                  60
     c                   enddo
     c                   endif
     c*
     c* Add erorr records in work file
     c*
     c                   if        @@mode = 'A'
     c*
     c                   eval      cpfile  = @@file
     c                   eval      cperrtx = @@errtx
     c*
     c                   write     pcp02
     c*
     c                   endif
     c*
     c                   eval      *inlr = *on
     c*
     c                   return
     c*
