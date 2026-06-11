     H* (C) Copyright Progressive Data Solutions, 2001
     H/TITLE FIXCMCLEX - Fix Claim Adjuster Staff Codes
     H**************************************************************************************
     H*  Created: 05/07/01     C:15                                                        *
     H**************************************************************************************
     Fwmcml1    if   e           k disk    Rename(pcm01 : pcm02) Prefix(c2:2)
     Fwmcml     uf   e           k disk
     Fwadtp     o    e             disk
     c*------------------------------------------------------------------------------
     C*  Key lists
     C
     C*   claim file
     C     cmky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
     C*
     C*====================================================================
     C*  mainloop
     C*====================================================================
     C*
     C*  Process all claims.
     C*
     C                   read      pcm01                                  99
     C                   dow       *in99 = *off
     C*
     C*    Retrieve the old claim record to get the correct adjuster.
     C*
     C     cmky          chain     pcm02                              98
     C                   if        *in98 = *off
     C                             and c2clex <> cmclex
     C*
     C*    Write record to diary transfer file.
     C*
     C                   move      cmco#         dtco#
     C                   move      cmfnd         dtfnd
     C                   move      cmfyr         dtfyr
     C                   move      cmcase        dtcase
     C                   move      cmclex        dtostf
     C                   move      c2clex        dtnstf
     C                   movel(p)  'PDS'         dtuser
     C                   movel(p)  'FIXCMSTAF'   dtpgm
     C                   move(p)   1020204       dtdt
     C                   move(p)   000000        dttime
     C
     C                   write     pdt01
     C
     C*    Update claim with correct adjuster.
     C
     C                   movel(p)  c2clex        cmclex
     C                   update    pcm01
     C
     C*    Call API to update WCMC claim with correct adjuster.
     C
     C                   move      cmco#         ##co#
     C                   move      cmfnd         ##fnd
     C                   move      cmfyr         ##fyr
     C                   move      cmcase        ##case
     C
     C                   call      'FA030R'
     C                   parm      *blank        ##call            1
     C                   parm                    ##co#             3
     C                   parm                    ##fnd             3
     C                   parm                    ##fyr             3
     C                   parm                    ##case            7
     C                   parm      cmnrse        ##cmnu            3
     C                   parm      *blanks       ##cmsu            3
     C                   parm      cmclex        ##clad            3
     C                   parm      cmsupr        ##clsu            3
     C                   parm      cmfead        ##clfa            3
     C                   parm      *blank        ##rtcd            1
     C
     C                   endif                                                  *in98 = *off
     C
     C                   read      pcm01                                  99
     C                   enddo                                                  *in99 = *off
     C
     C*   End of pgm
     C
     C                   eval      *inlr = *on
     C
