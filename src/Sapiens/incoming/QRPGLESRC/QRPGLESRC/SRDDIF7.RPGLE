     H*Copyright('Progressive Data Solutions, Inc., 2003')
     H* 10/20/03  C:132  Input CYYMMDD fields stored in PDS Files
     H****************************************************************
     H*  Revisions:
     H*
     H****************************************************************
     D@@date1          s              7  0
     D@@date2          s              7  0
     D@@diff           s              5  0
     D@@err            s              1
     D*
     Ddate1            s               d   datfmt(*iso)
     Ddate2            s               d   datfmt(*iso)
     D*
     C     *entry        plist
     C                   parm                    @@date1
     C                   parm                    @@date2
     C                   parm                    @@diff
     C                   parm      *in99         @@err
     C*
     C                   move      *zeros        @@diff
     C*
     C     *CYMD         test(d)                 @@date1                99
     C  N99*CYMD         move      @@date1       date1
     C*
     C                   if        *in99 = *OFF
     C*
     C     *cymd         test(d)                 @@date2                99
     C  N99*cymd         move      @@date2       date2
     C*
     C                   if        *in99=*OFF
     C     date2         subdur    date1         @@diff:*days           99
     C                   endif
     C                   endif
     C                   return
