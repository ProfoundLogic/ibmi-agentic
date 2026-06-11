     *==================================================================
     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H/TITLE DA704R - API TO UPDATE DARTS REMARK
     H*==================================================================
     H* CALLED FROM:                                                    *
     H* CALLED TO:                                                      *
     H*                                                                 *
     H* Created 01/25/12  C:190  Project: 114IZS                        *
     *==================================================================
     * REVISIONS:
     *
     *==================================================================
      *  Indicators:                                                    *
      *                                                                 *

     *==================================================================
     * File Definitions
     *==================================================================
      * Remark Detail File
     fsdcmp     uf   e             disk
     *
     d/copy sPrDate
     d psds          esds                  extname(szq1p)
     dsysDt7           s                   like(cmdt)
    
    c*================================================================
    c*  PLIST definitions
    c*================================================================
     c     *entry        plist
     c                   parm                    @@rrn             9 0
     c                   parm                    @@comm          256
     c                   parm                    @@pgm            10
    C*================================================================
     C*
    C*================================================================
    C*  MAIN PROGRAM
    c*================================================================
     c     @@rrn         chain     pcm01
     c*
     c                   If        %found (sdcmp)
      *
      * Get today's date
      *
     c                   eval      sysDt7 = DateTo7(SystemDate)

     c                   eval      cmcomm = @@comm                              Comment/Remark
     c                   eval      cmdt   = sysDt7                              Maintaince date
     c                   eval      cmuser = q1user                              Maintaince user
     c                   eval      cmpgm  = @@pgm                               Maintaince Pgm
     c                   eval      cmtime = TimeTo6(SystemTime)                 Maintaince time

     c                   update    pcm01

     C                   endif                                                  %found (sdcmp)

     C                   eval      *inlr = *on
     C                   return
     C*
