     *COPYRIGHT('Progressive Data Solutions, Inc., 1996')
     D@@datein         s              8  0
     D@@fromfmt        s              8
     D@@tofmt          s              8
     D@@dateout        s             10
     D@@err            s              1
     D##datein         s              8  0
     D##dateout        s             10
     *
     Dzzdash           c                   '-'
     *
     Dzdatein1         ds
     D zi1dat12                       2
     D  zi1dat22                      1    overlay(zi1dat12:2)
     D zi1dat34                       2
     D  zi1dat44                      1    overlay(zi1dat34:2)
     D zi1dat56                       2
     D zi1dat78                       2
     *
     Dzdatein2         ds             8
     D zi2dat12                       2
     D  zi2dat22                      1    overlay(zi2dat12:2)
     D zi2dat34                       2
     D zi2dat56                       2
     D zi2dat78                       2
     *
     Dzdateout1        ds
     D zo1dat18                       8
     D  zo1dat12                      2    overlay(zo1dat18:1)
     D  zo1dat15                      5    overlay(zo1dat18:1)
     D  zo1dat58                      4    overlay(zo1dat18:5)
     *
     Dzdateout2        ds             8
     D zo2dat18                       8
     D  zo2dat12                      2    overlay(zo2dat18:1)
     D  zo2dat15                      5    overlay(zo2dat18:1)
     D  zo2dat36                      4    overlay(zo2dat18:3)
     *
     C     *entry        plist
     C                   parm                    @@datein
     C                   parm                    @@fromfmt
     C                   parm                    @@tofmt
     C                   parm                    @@dateout
     C     *in99         parm                    @@err
     *
     C     cvtdat        plist
     C                   parm                    ##datein
     C                   parm                    @@fromfmt
     C                   parm                    @@tofmt
     C                   parm                    ##dateout
     C     *in99         parm                    @@err
     *
     C                   move      @@datein      zdatein1
     C                   move      *blanks       zdatein2
     C                   exsr      cvtfrom
     C                   move      zdatein2      ##datein
 002 C                   call      'SRCVTDAT'    cvtdat
     C                   if        NOT *in99
     C                   movel     ##dateout     zdateout1
     C                   exsr      cvtto
     C                   movel(p)  zdateout2     @@dateout
     C                   endif
     C                   return
     ***
     C     cvtfrom       begsr
B001 C                   if        @@fromfmt = '*MDYY'
 001 C                   move      zi1dat34      zi2dat12
     C                   move      '01'          zi2dat34
 001 C                   move      zi1dat56      zi2dat56
 001 C                   move      zi1dat78      zi2dat78
E001 C                   endif
B001 C                   if        @@fromfmt = '*MDY'
 001 C                   move      zi1dat56      zi2dat34
     C                   move      '01'          zi2dat56
 001 C                   move      zi1dat78      zi2dat78
E001 C                   endif
B001 C                   if        @@fromfmt = '*CYMD'
 001 C                   move      zi1dat44      zi2dat22
 001 C                   move      zi1dat56      zi2dat34
 001 C                   move      zi1dat78      zi2dat56
 001 C                   move      '01'          zi2dat78
E001 C                   endif
     C                   endsr
     ***
     C     cvtto         begsr
B001 C                   if        @@tofmt = '*MDYY'
 001 C                   move      zo1dat12      zo2dat12
 001 C                   move      zo1dat58      zo2dat36
E001 C                   endif
B001 C                   if        @@tofmt = '*CYMD'
 001 C                   move      zo1dat15      zo2dat15
E001 C                   endif
     C                   endsr
