     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE WA083R - API to determine if Auto-Cash is setup for a Group
     H**********************************************************************
     H*  Description:  API to determine if Auto-Cash is setup on a         *
     H*                on a particular group                               *
     H*                                                                    *
     H*  Called from:                                                      *
     H*    W1300R - Upload Payment Info                                    *
     H*                                                                    *
     H*  CALLS TO:                                                         *
     H*    None                                                            *
     H*                                                                    *
     H**********************************************************************
     H*  Created on 07/18/2011 by c:190 for Project 71WLB                  *
     H**********************************************************************
     H* Revisions:                                                         *
     H*                                                                    *
     H*                                                                    *
     H**********************************************************************
     f*
     fwdf2l     if   e           k disk
     fwtc3L     if   e           k disk
     fwdcdl     if   e           k disk

     d prRtncd         pr             1a
     d inco#                          3  0
     d infnd                          3  0
     d inpgm                          1
     c*
     c     *entry        plist
     c                   parm                    @@co#             3
     c                   parm                    @@fnd             3
     c                   parm                    @@pgm             1
     c                   parm                    @@Rtcd            1
    c*
     c                   move      @@co#         xxco#             3 0
     c                   move      @@fnd         xxfnd             3 0
     c                   move      @@pgm         xxpgm             1

     c                   eval      @@Rtcd = '0'

     c                   eval      @@Rtcd = prRtncd(xxco#:xxfnd:xxpgm)

     c                   seton                                        lr
      *=============================================================
      * prRtncd  - Return Code
      *=============================================================
     p prRtncd         b
     d prRtncd         pi             1a
     d inco#                          3  0
     d infnd                          3  0
     d inpgm                          1

     d kkco#           s                   like(cdco#)
     d kkfnd           s                   like(cdfnd)
     d kkpgm           s              1
     d kkrtcd          s                   like(cdctyp)
     d kkhier          s                   like(cdhier)
     d xxflg           s              1a   inz
     c*
     c     f2ky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c*
     c     cdky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkhier
     c*
     c     cdky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c                   eval      kkco# = inco#
     c                   eval      kkfnd = infnd
     c                   eval      xxflg = '0'

     c     f2ky          chain     pf201                              60
     c                   if        %found

     c     f2ky          chain     pc301                              60
     c                   if        %found

     c                   eval      kkhier = 10000
     c     cdky          setll     pcd01
     c     cdky1         readpe    pcd01                                  60
     c                   if        not %eof

     c                   select
     c                   when      @@pgm  = 'L' and f2lcyf = 'Y'
     c                   eval      xxflg = '1'
     c                   when      @@pgm  = 'A' and f2acyf = 'Y'
     c                   eval      xxflg = '1'
     c                   when      @@pgm = 'C' and f2ccyf = 'Y'
     c                   eval      xxflg = '1'
     c                   when      @@pgm = 'E' and f2rcyf = 'Y'
     c                   eval      xxflg = '1'
     c                   other
     c                   eval      xxflg = '0'
     c                   endsl                                                  @@pgm = 'L'

     c                   endif                                                  pcd01
     c                   endif                                                  pc301
     c                   endif                                                  pf201

     c                   return    xxflg

     p prRtncd         e
