     H*copyright('Progressive Data Solutions, Inc., 1996')
     D@@datein         s             10
     D@@tofmt          s              8
     D@@dateout        s              8
     D*
     Dzzdash           c                   '-'
     D*
     Dcendigit         s              1  0
     Dcendigits        s              2  0
     D*
     Dzziso            ds
     Dzzcc                            2
     Dzzyy                            2
     Dzzdash1                         1
     Dzzmm                            2
     Dzzdash2                         1
     Dzzdd                            2
     D*
     Dzzdateout        ds
     Dzzdat12                  1      2
     Dzzdat34                  3      4
     Dzzdat56                  5      6
     Dzzdat78                  7      8
     Dzzdat11                  1      1
     Dzzdat23                  2      3
     Dzzdat45                  4      5
     Dzzdat67                  6      7
     Dzzdat88                  8      8
     D*
     C     *entry        plist
     C                   parm                    @@datein
     C                   parm                    @@tofmt
     C                   parm                    @@dateout
     C*
     C                   movel     @@datein      zziso
     C                   exsr      cvtto
     C                   move      zzdateout     @@dateout
     C                   return
     C***
     C     cvtto         begsr
     C     @@tofmt       ifeq      '*MDYY'
     C                   move      zzmm          zzdat12
     C                   move      zzdd          zzdat34
     C                   move      zzcc          zzdat56
     C                   move      zzyy          zzdat78
     C                   endif
     C     @@tofmt       ifeq      '*CYMD'
     C                   move      zzcc          cendigits
     C     cendigits     sub       19            cendigit
     C                   move      cendigit      zzdat11
     C                   move      zzyy          zzdat23
     C                   move      zzmm          zzdat45
     C                   move      zzdd          zzdat67
     C                   move      *blanks       zzdat88
     C                   endif
     C                   endsr
