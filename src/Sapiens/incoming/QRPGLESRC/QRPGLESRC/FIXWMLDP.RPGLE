      *========================================================================
      * FIXWMLDP - clears claims allowance and claims liability rate on policies
      *            that are not supposed to have them.
      *========================================================================
     fwmldl1    uf   e           k disk
     fwdell     if   e           k disk
     fwtgtl     if   e           k disk
     dkktbid           s                   like(gttbid)
     dkkkey            s                   like(gtkey)
     c     elky          klist
     c                   kfld                    ldco#
     c                   kfld                    ldfnd
     c                   kfld                    ldemp#
     c                   kfld                    lddiv
     c                   kfld                    ldfyr
     c     gtky          klist
     c                   kfld                    ldco#
     c                   kfld                    kktbid
     c                   kfld                    kkkey
     c                   read      pld01
     c                   dow       not(%eof)
     c     elky          chain     pel01
     c                   if        %found
     c                   eval      kktbid = 'PTYP'
     c                   eval      kkkey  = ELQTYP
     c     gtky          chain     pgt01
     c                   if        %found
     c                   select
     c                   when      gtfg01 = 'M'
     c                   when      gtfg01 = 'A'
     c                   if        ldclal <> 0
     c                   eval      ldclal =  0
     c                   move      'X'           ldpgm
     c                   update    pld01
     c                   endif
     c                   other
     c                   if        ldclal <> 0 or
     c                             ldlirt <> 0
     c                   eval      ldclal =  0
     c                   eval      ldlirt =  0
     c                   move      'X'           ldpgm
     c                   update    pld01
     c                   endif
     c                   endsl
     c                   else
     c                   if        ldclal <> 0 or
     c                             ldlirt <> 0
     c                   eval      ldclal =  0
     c                   eval      ldlirt =  0
     c                   move      'X'           ldpgm
     c                   update    pld01
     c                   endif
     c                   endif
     c                   endif
     c                   read      pld01
     c                   enddo
     c                   seton                                        lr
