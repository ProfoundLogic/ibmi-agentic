    h/TITLE FIXPAYUP - Fix effective dates in WMEPP and WASRP
     h option(*nodebugio:*srcstmt)
      *============================================================================================
     h*  Called from:  W8361C                                                                      *
     h*  Created: 06/08/06 by C:136 for Support: WA135 53013                                       *
      *============================================================================================
     fwdell     if   e           k disk
     fwmepl1    uf   e           k disk
     fwasrl2    uf   e           k disk
     dupdateflag       s               n   inz(*off)
     c     elky          klist
     c                   kfld                    svco#             3 0
     c                   kfld                    svfnd             3 0
     c                   kfld                    svemp#            9 0
     c                   kfld                    svdiv             5 0
     c                   kfld                    svfyr             3 0
     c                   eval      svco# = 777
     c                   read      pep01
     c                   dow       not(%eof)
     c                   if        epfnd <> svfnd    or
     c                             epemp# <> svemp#  or
     c                             epdiv <> svdiv    or
     c                             epfyr <> svfyr
     c                   eval      svfnd = epfnd
     c                   eval      svemp# = epemp#
     c                   eval      svdiv = epdiv
     c                   eval      svfyr = epfyr
     c     elky          chain     pel01
     c                   if        %found
     c                   eval      updateflag = *on
     c                   else
     c                   eval      updateflag = *off
     c                   endif
     c                   endif
     c                   if        epefdt < elpfdt and
     c                             updateflag = *on
     c                   eval      epefdt = elpfdt
     c                   update    pep01
     c                   endif
     c                   read      pep01
     c                   enddo
     c                   clear                   svfnd
     c                   clear                   svemp#
     c                   clear                   svdiv
     c                   clear                   svfyr
     c                   read      psr01
     c                   dow       not(%eof)
     c                   if        srfnd <> svfnd    or
     c                             sremp# <> svemp#  or
     c                             srdiv <> svdiv    or
     c                             srfyr <> svfyr
     c                   eval      svfnd = srfnd
     c                   eval      svemp# = sremp#
     c                   eval      svdiv = srdiv
     c                   eval      svfyr = srfyr
     c     elky          chain     pel01
     c                   if        %found
     c                   eval      updateflag = *on
     c                   else
     c                   eval      updateflag = *off
     c                   endif
     c                   endif
     c                   if        srefdt < elpfdt  and
     c                             updateflag = *on
     c                   eval      srefdt = elpfdt
     c                   update    psr01
     c                   endif
     c                   read      psr01
     c                   enddo
     c                   seton                                        lr
