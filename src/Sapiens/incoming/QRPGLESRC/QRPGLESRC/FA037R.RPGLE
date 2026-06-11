     H* (C) Copyright Fiserv Insurance Solutions, 2006
     H**********************************************************************
     H*  Calculate BRS fee
     H**********************************************************************
     H*  Created on 08/06/2006 by c:62 Project 135WHU                      *
     H**********************************************************************
     H*  Revisions:                                                        *
     H*                                                                    *
     H*  09/05/07 c:62 Project MC135 59961                                 *
     H*                Drop to bill level if bamt is < 0 and rcn = 0       *
     H*                                                                    *
     H**********************************************************************
     Ffaeal1    if   e           k disk
     F*** BEGIN ADD *** 04/11/2007 *****************************************
     Ffddal     if   e           k disk
     F*** END   ADD *** 04/11/2007 *****************************************
     F
     D@@co#            s                   like(eaco#)
     D@@sub#           s                   like(easub#)
     D@@fnd            s                   like(eafnd)
     D@@fyr            s                   like(eafyr)
     D@@case           s                   like(eacase)
     D@@eob#           s                   like(eaeob#)
     D@@rcn            s                   like(earcn)
     D@@pct            s              5  2
     D@@bamt           s                   like(eabamt)
     D@@call           s              1
     D@@rtcd           s              1
     Dtotbamtp         s                   like(eabamt)
     Dtotpaidp         s                   like(eapaid)
     Dtotpaidn         s                   like(eapaid)
     Dtotsavep         s                   like(eamra)
     Dtotsaven         s                   like(eamra)
     D*** begin add *** 01/17/2008 *****************************************
     Dtotsavept        s                   like(eamra)
     Dtotsavent        s                   like(eamra)
     D*** end   add *** 01/17/2008 *****************************************
     D*** begin add *** 04/11/2007 *****************************************
     Dhdbamt           s                   like(eabamt)
     Dkkrcn            s                   like(earcn)
     D*** end   add *** 04/11/2007 *****************************************
     D
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@sub#
     C                   parm                    @@fnd
     C                   parm                    @@fyr
     C                   parm                    @@case
     C                   parm                    @@eob#
     C                   parm                    @@rcn
     C                   parm                    @@pct
     C                   parm                    @@bamt
     C                   parm                    @@call
     C                   parm                    @@rtcd
     C*
     C     eaky          klist
     c                   kfld                    @@co#
     c                   kfld                    @@sub#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@case
     c                   kfld                    @@eob#
     C*
     C     eaky2         klist
     c                   kfld                    @@co#
     c                   kfld                    @@sub#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@case
     c                   kfld                    @@eob#
     c                   kfld                    @@rcn
     c*** begin add *** 04/11/2007 *****************************************
     C*
     C     daky          klist
     c                   kfld                    @@co#
     c                   kfld                    @@sub#
     c                   kfld                    @@fnd
     c                   kfld                    @@fyr
     c                   kfld                    @@case
     c                   kfld                    @@eob#
     c                   kfld                    kkrcn
     c*** end   add *** 04/11/2007 *****************************************
     C*
     C                   eval      totbamtp = 0
     C                   eval      totpaidp = 0
     C                   eval      totpaidn = 0
     C                   eval      totsavep = 0
     C                   eval      totsaven = 0
     c*** begin add *** 01/17/2008 *****************************************
     C                   eval      totsavept = 0
     C                   eval      totsavent = 0
     c*** end   add *** 01/17/2008 *****************************************
     c                   eval      @@rtcd = ' '
     C*
     c                   if        @@rcn = 0
     C     eaky2         chain     pea01
     c                   if        eapaid = 0
     c                   eval      @@bamt = 0
     c                   else
     c*** begin chg *** 04/11/2007 *****************************************
     c**********         eval      @@bamt = (eamra + eaothr + eaoth2) *
     c**********                             @@pct
     c                   eval      hdbamt = 0
     c                   eval      kkrcn = earcn
     c     daky          chain     pda01
     c                   dow       not *in99
     c                   if        dapaid <> 0
     c                   eval      hdbamt = hdbamt + damra + daothr + daoth2
     c                   endif
     c     daky          reade     pda01                                  99
     c                   enddo
     c*
     c                   eval      @@bamt = hdbamt * @@pct
     c*** end   chg *** 04/11/2007 *****************************************
     c                   endif
     c                   else
     c     eaky          chain     pea01                              99
     c                   dow       not *in99
     c                   if        earcn < @@rcn
     c                   eval      totbamtp = totbamtp + eabamt
     c                   eval      totpaidp = totpaidp + eapaid
     c*** begin chg *** 04/11/2007 *****************************************
     c**********         eval      totsavep = totsavep + eamra + eaothr +
     c**********                              eaoth2
     c                   eval      kkrcn = earcn
     c     daky          chain     pda01
     c                   dow       not *in99
     c                   if        dapaid <> 0
     c                   eval      totsavep = totsavep + damra + daothr +
     c                                        daoth2
     c                   endif
     c*** begin add *** 01/17/2008 *****************************************
     c                   eval      totsavept = totsavept + damra + daothr +
     c                                        daoth2
     c*** end   add *** 01/17/2008 *****************************************
     c     daky          reade     pda01                                  99
     c                   enddo
     c*** end   chg *** 04/11/2007 *****************************************
     c                   else
     c                   if        earcn = @@rcn
     c                   eval      totpaidn = totpaidp + eapaid
     c*** begin chg *** 04/11/2007 *****************************************
     c**********         eval      totsaven = totsavep + eamra + eaothr +
     c**********                               eaoth2
     c                   eval      kkrcn = earcn
     c     daky          chain     pda01
     c                   dow       not *in99
     c                   if        dapaid <> 0
     c                   eval      totsaven = totsaven + damra + daothr +
     c                                        daoth2
     c                   endif
     c*** begin add *** 01/17/2008 *****************************************
     c                   eval      totsavent = totsavent + damra + daothr +
     c                                        daoth2
     c*** end   add *** 01/17/2008 *****************************************
     c     daky          reade     pda01                                  99
     c                   enddo
     c*
     c                   eval      totsaven = totsaven + totsavep
     c*** begin add *** 01/17/2008 *****************************************
     c                   eval      totsavent = totsavent + totsavept
     c*** end   add *** 01/17/2008 *****************************************
     c*** end   chg *** 04/11/2007 *****************************************
     c                   leave
     c                   endif
     c                   endif
     c     eaky          reade     pea01                                  99
     c                   enddo
     c*
     c                   if        totpaidp = 0
     c                   if        totpaidn = 0
     c                   eval      @@bamt = 0
     c                   else
     c*** begin add *** 01/17/2008 *****************************************
     c**********         eval      @@bamt = totsaven * @@pct
     c                   eval      @@bamt = totsavent * @@pct
     c*** end   add *** 01/17/2008 *****************************************
     c                   eval      @@rtcd = 'P'
     c                   endif
     c*
     c                   else
     c*
     c                   if        totpaidn = 0
     c                   if        @@call = 'P'
     c                   eval      @@bamt = totsaven * @@pct * -1
     c                   else
     c                   eval      @@bamt = totbamtp * -1
     c                   endif
     c                   else
     c     eaky2         chain     pea01
     c                   if        %found
     c*** begin chg *** 04/11/2007 *****************************************
     c**********         eval      @@bamt = (eamra + eaothr + eaoth2) *
     c**********                            @@pct
     c                   eval      kkrcn = earcn
     c                   eval      hdbamt = 0
     c     daky          chain     pda01
     c                   dow       not *in99
     c                   if        dapaid <> 0
     c                   eval      hdbamt = hdbamt + damra + daothr + daoth2
     c                   endif
     c     daky          reade     pda01                                  99
     c                   enddo
     c*
     c                   eval      @@bamt = hdbamt * @@pct
     c*** end   chg *** 04/11/2007 *****************************************
     c                   endif
     c                   endif
     c                   endif
     C                   endif
     c*** begin add *** 09/05/07 *******************************************
     c*
     c* If the recon is 0 and the fee is negative then figure the amount
     c* based upon the entire bill not by line item.
     c*
     c                   if        @@rcn = 0 and @@bamt < 0
     c*
     C                   eval      totbamtp = 0
     C                   eval      totpaidp = 0
     C                   eval      totpaidn = 0
     C                   eval      totsavep = 0
     C                   eval      totsaven = 0
     c                   eval      @@rtcd = ' '
     C*
     c                   if        @@rcn = 0
     C     eaky2         chain     pea01
     c                   if        eapaid = 0
     c                   eval      @@bamt = 0
     c                   else
     c                   eval      @@bamt = (eamra + eaothr + eaoth2) *
     c                                       @@pct
     c                   endif
     c                   else
     c     eaky          chain     pea01                              99
     c                   dow       not *in99
     c                   if        earcn < @@rcn
     c                   eval      totbamtp = totbamtp + eabamt
     c                   eval      totpaidp = totpaidp + eapaid
     c                   eval      totsavep = totsavep + eamra + eaothr +
     c                                        eaoth2
     c                   else
     c                   if        earcn = @@rcn
     c                   eval      totpaidn = totpaidp + eapaid
     c                   eval      totsaven = totsavep + eamra + eaothr +
     c                                         eaoth2
     c                   leave
     c                   endif
     c                   endif
     c     eaky          reade     pea01                                  99
     c                   enddo
     c*
     c                   if        totpaidp = 0
     c                   if        totpaidn = 0
     c                   eval      @@bamt = 0
     c                   else
     c                   eval      @@bamt = totsaven * @@pct
     c                   eval      @@rtcd = 'P'
     c                   endif
     c*
     c                   else
     c*
     c                   if        totpaidn = 0
     c                   if        @@call = 'P'
     c                   eval      @@bamt = totsaven * @@pct * -1
     c                   else
     c                   eval      @@bamt = totbamtp * -1
     c                   endif
     c                   else
     c     eaky2         chain     pea01
     c                   if        %found
     c                   eval      @@bamt = (eamra + eaothr + eaoth2) *
     c                                      @@pct
     c                   endif
     c                   endif
     c                   endif
     C                   endif
     c*
     c                   endif
     c*
     c* If the fee is still negative then zero out fee
     c*
     c                   if        @@rcn = 0 and @@bamt < 0
     c                   eval      @@rtcd = ' '
     c                   eval      @@bamt = 0
     c                   endif
     c*** end   add *** 09/05/07 *******************************************
     C*
     C
     C                   seton                                        lr
