     H* (C) Copyright Fiserv Insurance Solutions, 2006
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
     H**********************************************************************
     H*  Calculate Network fee
     H**********************************************************************
     H*  Created on 08/06/2006 by c:62 Project 135WHU                      *
     H**********************************************************************
     H*  Revisions:                                                        *
     H*                                                                    *
     H**********************************************************************
     Ffaeal1    if   e           k disk
     F
     D@@co#            s                   like(eaco#)
     D@@sub#           s                   like(easub#)
     D@@fnd            s                   like(eafnd)
     D@@fyr            s                   like(eafyr)
     D@@case           s                   like(eacase)
     D@@eob#           s                   like(eaeob#)
     D@@rcn            s                   like(earcn)
     D@@pct            s              5  2
     D@@namt           s                   like(eanamt)
     D@@call           s              1
     Dtotnamtp         s                   like(eanamt)
     Dtotpaidp         s                   like(eapaid)
     Dtotpaidn         s                   like(eapaid)
     Dtotsavep         s                   like(eamra)
     Dtotsaven         s                   like(eamra)
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
     C                   parm                    @@namt
     C                   parm                    @@call
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
     C*
     C                   eval      totnamtp = 0
     C                   eval      totpaidp = 0
     C                   eval      totpaidn = 0
     C                   eval      totsavep = 0
     C                   eval      totsaven = 0
     C*
     c                   if        @@rcn = 0
     C     eaky2         chain     pea01
     c                   if        eapaid = 0
     c                   eval      @@namt = 0
     c                   else
     c*** begin add *** 07/24/08 *******************************************
     c                   if        eandis>0
     c*** end   add *** 07/24/08 *******************************************
     c                   eval      @@namt = eandis * @@pct
     c*** begin add *** 07/24/08 *******************************************
     c                   else
     c                   eval      @@namt=0
     c                   endif
     c*** end   add *** 07/24/08 *******************************************
     c                   endif
     c                   else
     C     eaky          chain     pea01                              99
     c                   dow       not *in99
     c                   if        earcn < @@rcn
     c                   eval      totnamtp = totnamtp + eanamt
     C                   eval      totpaidp = totpaidp + eapaid
     c                   eval      totsavep = totsavep + eandis
     c                   else
     c                   if        earcn = @@rcn
     C                   eval      totpaidn = totpaidp + eapaid
     c                   eval      totsaven = totsavep + eandis
     c                   leave
     c                   endif
     c                   endif
     c     eaky          reade     pea01                                  99
     c                   enddo
     c*
     c                   if        totpaidp = 0
     c                   if        totpaidn = 0
     c                   eval      @@namt = 0
     c                   else
     c                   eval      @@namt = totsaven * @@pct
     c                   endif
     c*
     c                   else
     c*
     c                   if        totpaidn = 0
     c                   if        @@call = 'P'
     c                   eval      @@namt = totsaven * @@pct * -1
     c                   if        @@namt = 0 and totnamtp <> 0
     c                   eval      @@namt = totnamtp * -1
     c                   endif
     c                   else
     c                   eval      @@namt = totnamtp * -1
     c                   endif
     c                   else
     c     eaky2         chain     pea01
     c                   if        %found
     c                   eval      @@namt = eandis * @@pct
     c                   endif
     c                   endif
     c                   endif
     C                   endif
     C*
     C
     C                   seton                                        lr
