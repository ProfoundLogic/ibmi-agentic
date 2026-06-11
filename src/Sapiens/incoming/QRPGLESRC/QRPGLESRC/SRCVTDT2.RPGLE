     H*COPYRIGHT('Progressive Data Solutions, Inc., 1996')
     D@@datein         s              8  0
     D@@fromfmt        s              8
     D@@tofmt          s              8
     D@@dateout        s             10
     D@@err            s              1
     D*
     Dzzdash           c                   '-'
     D*
     Dcendigit         s              1  0
     Dcendigits        s              2  0
     D*
     D@@cidatout       s              8
     D*
     Dzziso            ds            10
     Dzzcc                            2  0
     Dzzyy                            2  0
     Dzzdash1                         1
     Dzzmm                            2  0
     Dzzdash2                         1
     Dzzdd                            2  0
     D*
     Dzzdatein         ds
     Dzzdat12                  1      2
     Dzzdat34                  3      4
     Dzzdat56                  5      6
     Dzzdat78                  7      8
     Dzzdat22                  2      2
     D*
     C     *entry        plist
     C                   parm                    @@datein
     C                   parm                    @@fromfmt
     C                   parm                    @@tofmt
     C                   parm                    @@dateout
     C     *in99         parm      *in99         @@err
     C*
     C     cvtiso        plist
     C                   parm                    zziso
     C                   parm                    @@tofmt
     C                   parm                    @@cidatout
     C*
     C                   move      zzdash        zzdash1
     C                   move      zzdash        zzdash2
     C                   move      @@datein      zzdatein
     C                   exsr      cvtfrom
B001 C     *in99         ifeq      *off
     C     *iso          test(d)                 zziso                  99
B001 C     *in99         ifeq      *off
B002 C     @@tofmt       ifne      '*ISO'
 002 C                   call      'SRCVTISO'    cvtiso
 002 C                   movel(p)  @@cidatout    @@dateout
X002 C                   else
 002 C                   movel(p)  zziso         @@dateout
E002 C                   endif
E001 C                   endif
E001 C                   endif
     C                   return
     C***
     C     cvtfrom       begsr
B001 C     @@fromfmt     ifeq      '*MDYY'
 001 C                   move      zzdat12       zzmm
 001 C                   move      zzdat34       zzdd
 001 C                   move      zzdat56       zzcc
 001 C                   move      zzdat78       zzyy
     C     zzcc          iflt      19
     C     zzcc          orgt      20
     C                   move      *on           *in99
     C                   endif
E001 C                   endif
B001 C     @@fromfmt     ifeq      '*CYMD'
 001 C                   move      zzdat22       cendigit
 001 C     cendigit      add       19            cendigits
 001 C                   move      cendigits     zzcc
 001 C                   move      zzdat34       zzyy
 001 C                   move      zzdat56       zzmm
 001 C                   move      zzdat78       zzdd
E001 C                   endif
     C                   endsr
