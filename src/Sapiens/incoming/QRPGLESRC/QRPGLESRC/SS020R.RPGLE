     H copyright('Progressive Data Solutions, Inc 2000')
     Fssypf     if   e             disk    usropn

     D@allow           s              1
     D
     C     *entry        plist
     C                   parm                    @allow
     C
     C                   movel     *blanks       @allow
     C                   open      ssypf
     C     1             chain     ssypf                              99
     C                   close     ssypf
     C                   if        *in99 = *off
     C                   movel     syfl07        @allow
     C                   endif

     C                   return
