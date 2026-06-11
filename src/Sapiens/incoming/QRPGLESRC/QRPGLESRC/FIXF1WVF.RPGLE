     H*  (C) Copyright Progressive Data Solutions, Inc., 2000
     H/TITLE CVTBLTY - copies fields from WDF1PX to WDF1P

     fwdf1px    if   e             disk    prefix(x)
     fwdf1l     uf   e           k disk

      *************************************************
      ** Program entry processing                    **
      *************************************************

     c     f1ky          klist
     c                   kfld                    xf1co#
     c                   kfld                    xf1fnd
     c                   kfld                    xf1fyr
      *************************************************
      ** Mainline processing                         **
      *************************************************

      ** Read next old record

     c                   read      wdf1px
     c                   dow       not %eof(wdf1px)

     c     f1ky          chain     pf101                              99
     c                   if        *in99 = *off
     c                   eval      f1ninv = xf1ninv
     c                   eval      f1wfam = xf1wfam
     c                   eval      f1wvar = xf1wvar
     c                   update    pf101
     c                   endif

     c                   read      wdf1px
     c                   enddo

     c                   seton                                        lr
