     H*copyright('Progressive Data Solutions, Inc., 1998')
     H*
     H****************************************************************
     H*  Calculates a date in the past by subtracting @@months from
     H*  @@date and returns value in @@date.  @@err is set on if there
     H*  is an error.  @@date must be passed in mmddccyy or mmddyy order.
     H*
     H*  10/06/98  C:87
     H****************************************************************
     D@@date           s              8  0
     D@@months         s              3  0
     D@@err            s              1
     D*
     Ddate1            s               d   datfmt(*iso)
     Ddate2            s               d   datfmt(*usa)
     D*
     C     *entry        plist
     C                   parm                    @@date
     C                   parm                    @@months
     C                   parm      *in99         @@err
     C*
     C                   if        @@date  < 1000000
     C     *mdy          test(d)                 @@date                 99
     C  N99*mdy          move      @@date        date1
     C                   else
     C     *usa          test(d)                 @@date                 99
     C  N99*usa          move      @@date        date1
     C                   endif
     C*
     C                   if        *in99 = *off
     C     date1         subdur    @@months:*m   date2                  99
     C                   if        *in99 = *off
     C                   move      date2         @@date
     C                   endif
     C                   endif
     C*
     C                   return
