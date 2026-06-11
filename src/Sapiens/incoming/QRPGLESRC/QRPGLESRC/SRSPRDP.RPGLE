     hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     hdftactgrp(*no) option(*nodebugio:*srcstmt)
     h*****************************************************************
     h/TITLE SRSPRDP - PEO Group: Discount Spread
     h*****************************************************************
     h*     Program description:                                      *
     h*        This program checks the discount spread for PEO groups *
     h*        and returns the flag to calling programs               *
     h*                                                               *
     h* CALLED FROM:                                                  *
     h*   W8002C0                                                     *
     h*                                                               *
     h* CALLS TO:                                                     *
     h*                                                               *
     h* PROGRAMMER: C:191                                             *
     h* CREATED:    09/12/11            PROJECT: WC1919               *
     h*                                                               *
     h*****************************************************************
     h* REVISIONS:                                                    *
     h*                                                               *
     h* XX/XX/XX C:XXX Project: XXXXXX                                *
     h*                                                               *
     h*                                                               *
     h*****************************************************************
     h*                                                               *
     fwmfnl     if   e           k disk
     f*                                                               *
     d xxco#           s                   like(fnco#) inz
     d xxfnd           s                   like(fnfnd) inz
     d*
     d qqco#           s              3a
     d qqfnd           s              3a
     d qqsprd          s              1a
     d*
     c* *entry list
     c     *entry        plist
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    qqsprd
     c*
     c* key list definition
     c     fnky          klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c*
     c* Main processing
     c                   eval      xxco# = %int(qqco#)
     c                   eval      xxfnd = %int(qqfnd)
     c                   eval      qqsprd = 'N'
     c*
     c     fnky          chain     pfn01
     c                   if        %found and
     c                             (fnpeof = 'Y' or
     c                             ((fnpeof='M' or fnpeof='G') and fnpeoa='Y'))
     c                   eval      qqsprd = 'Y'
     c                   endif
     c*
     c                   eval      *inlr = *on
