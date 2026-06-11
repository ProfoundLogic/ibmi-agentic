    h*  (C) Copyright Progressive Data Solutions, Inc., 2003
    h*********************************************************************
    h/TITLE FIXWDEAP - Load new initial date, pgm, and user fields
    h*********************************************************************
    h* Called from:                                                      *
    h*   Command line                                                    *
    h*                                                                   *
    h* Calls to:                                                         *
    h*   None                                                            *
    h*                                                                   *
    h* Created:  09/24/03     C:136    PROJECT: WC1116                   *
    h*********************************************************************
    h* Revisions:                                                        *
    h*                                                                   *
     h* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    h*********************************************************************

     fwdeal4    uf   e           k disk

     d  svbat#         s                   like(eabat#)
     d  loDate         s                   like(eaidt)
     d  loUser         s                   like(eauser)
     d  loPgm          s                   like(eapgm)
     d  loTime         s                   like(eatime)

     c     eaky          klist
     c                   kfld                    eaco#
     c                   kfld                    svbat#

     c                   read      pea01                                  60
     c                   dow       not *in60

     c                   eval      loTime = *hival
     c                   eval      loDate = *hival
     c                   if        eabat# <> svbat#
     c                   eval      svbat# =  eabat#
     c     eaky          chain     pea01                              60
     c                   dow       not *in60

     c                   if        eadt   < loDate

     c                   if        eatime < loTime
     c                   eval      loDate = eadt
     c                   eval      loPgm  = eapgm
     c                   eval      loUser = eauser
     c                   eval      loTime = eatime
     c                   endif                                                  eatime

     c                   endif                                                  eadt

     c     eaky          reade     pea01                                  60
     c                   enddo

     c     eaky          chain     pea01                              60
     c                   dow       not *in60

     c                   eval      eaidt  = loDate
     c                   eval      eaipgm = loPgm
     c                   eval      eaiusr = loUser
     c                   update    pea01

     c     eaky          reade     pea01                                  60
     c                   enddo

     c                   endif                                                  eabat# <> svbat#

     c     eaky          setgt     pea01
     c                   read      pea01                                  60
     c                   enddo
     c*
     c                   eval      *inlr = *on
