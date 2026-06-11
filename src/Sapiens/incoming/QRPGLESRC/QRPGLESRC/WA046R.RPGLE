     Fwmaal     if   e           k disk    rename(paa01:paa00)
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3  0
     D @@fein          s              9  0
     D @@mod           s              3  0
     D @@agt#          s              5  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------

     C     aaky          klist
     C                   kfld                    @@co#
     C                   kfld                    @@fein
     C                   kfld                    @@mod
     C*
     C     fnky          klist
     C                   kfld                    @@co#
     C                   kfld                    AAFND
     C*____________________________________________________________________
     C*
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fein
     C                   parm                    @@mod
     C                   parm                    @@agt#
     C*____________________________________________________________________
     C*
     C                   clear                   @@agt#
     C*
     C     aaky          setll     wmaal
     C*
     C                   dou       *in60
     C*
     C     aaky          reade     wmaal                                  60
     C*
     C                   if        not *in60 and AASTAT <> 'I'
     C*
     C                   clear                   FNTEST
     C     fnky          reade     wmfnl                                  61
     C*
     C                   if        not *in61 and FNTEST <> 'Y'
     C                   eval      @@agt# = AAAGT#
     C                   leave
     C                   endif
     C*
     C                   endif
     C*
     C                   enddo
     C*____________________________________________________________________
     C*
     C                   move      *on           *inlr
