     H*  (C) Copyright Progressive Data Solutions, Inc., 2002
     H/TITLE FIX34918: V3 Change RPPNAM, RPADR1, etc. if Provider file changed.
     fwarpp     uf   e             disk
     fwmpdl     if   e           k disk

     D xxpnam          s                   like(rppnam)
     D xxadr1          s                   like(rpadr1)
     D xxadr2          s                   like(rpadr2)

     C*=========================================================================
     C     pdky          klist
     C                   kfld                    rpfein
     C                   kfld                    rpmod

      *************************************************
      ** Mainline processing                         **
      *************************************************

     c                   read      prp01                                  LR

      ** If not EOF, update new fields

     c                   dow       *inLR=*off

     c                   if        RPfein <> 0

     c     pdky          chain     ppd01                              99
     c                   if        *in99 = *off
     c*-----
     c                   if        pddba1 <> 'Y'
     c*
     c                   if        pdfsn <> *blanks
     c                   movel(p)  pdfsn         xxpnam
     c                   cat       pdname:1      xxpnam
     c                   else
     c                   movel(p)  pdname        xxpnam
     c                   endif
     c*
     c                   movel(p)  pdadr1        xxadr1
     c                   movel(p)  pdadr2        xxadr2
     c*
     c                   else
     c                   movel(p)  pdadr1        xxpnam
     c                   movel(p)  pdadr2        xxadr1
     c                   movel     *blanks       xxadr2
     c                   endif
     c*
     c                   if        rppnam <> xxpnam or rpadr1 <> xxadr1
     c                             or rpadr2 <> xxadr2 or rpcty <> pdcty
     c                             or rpst <> pdst or rpzip <> pdzip
     c                   eval      rppnam = xxpnam
     c                   eval      rpadr1 = xxadr1
     c                   eval      rpadr2 = xxadr2
     c                   eval      rpcty = pdcty
     c                   eval      rpst = pdst
     c                   eval      rpzip = pdzip
     c                   update    prp01
     c                   endif
     c*-----
     c                   endif
     c                   endif

     c                   read      prp01                                  LR
     c                   enddo
      **********************************************************************************************
