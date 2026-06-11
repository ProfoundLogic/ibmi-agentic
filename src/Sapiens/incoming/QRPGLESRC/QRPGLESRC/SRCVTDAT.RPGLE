     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H dftactgrp(*no) actgrp(*caller)
     D*
     D@@datein         s              8  0
     D@@fromfmt        s              8
     D@@tofmt          s              8
     D@@dateout        s             10
     D@@err            s              1
     D*
     DdmyDate          s               d   datfmt(*dmy)
     DeurDate          s               d   datfmt(*eur)
     DisoDate          s               d   datfmt(*iso)
     DjulDate          s               d   datfmt(*jul)
     DmdyDate          s               d   datfmt(*mdy)
     DusaDate          s               d   datfmt(*usa)
     DymdDate          s               d   datfmt(*ymd)
     DnumDate5         s              5  0
     DnumDate6         s              6  0
     DnumDate7         s              7  0
     DnumDate8         s              8  0
     DcheckYear        s              4  0
     D*
     C     *entry        plist
     C                   parm                    @@datein
     C                   parm                    @@fromfmt
     C                   parm                    @@tofmt
     C                   parm                    @@dateout
     C                   parm      *in99         @@err
     C*
     C                   setoff                                       99
     C                   move      *blanks       @@dateout
     C*
     C                   if        @@datein = 0
     C                   seton                                        99
     C                   return
     C                   endif
     C**
     C*  Convert 8 digit number to a *USA type date.
     C**
     C                   select
     C*
     C                   when      @@fromfmt = *blanks
     C                   if        @@datein < 1000000
     C     *jobrun       test(d)                 @@datein               99
     C  n99*jobrun       move      @@datein      usaDate
     C                   else
     C     *usa          test(d)                 @@datein               99
     C  n99*usa          move      @@datein      usaDate
     C                   endif
     C*
     C                   when      @@fromfmt = '*CYMD'
     C     @@datein      add       19000000      numDate8
     C     *iso          test(d)                 numDate8               99
     C  n99*iso          move      numDate8      usaDate
     C*
     C                   when      @@fromfmt = '*YMD'
     C                   if        @@datein < 1000000
     C     *ymd          test(d)                 @@datein               99
     C  n99*ymd          move      @@datein      usaDate
     C                   else
     C     *iso          test(d)                 @@datein               99
     C  n99*iso          move      @@datein      usaDate
     C                   endif
     C*
     C                   when      @@fromfmt = '*YYMD' or
     C                             @@fromfmt = '*ISO'
     C     *iso          test(d)                 @@datein               99
     C  n99*iso          move      @@datein      usaDate
     C*
     C                   when      @@fromfmt = '*MDY'
     C                   if        @@datein < 1000000
     C     *mdy          test(d)                 @@datein               99
     C  n99*mdy          move      @@datein      usaDate
     C                   else
     C     *usa          test(d)                 @@datein               99
     C  n99*usa          move      @@datein      usaDate
     C                   endif
     C*
     C                   when      @@fromfmt = '*MDYY'
     C     *usa          test(d)                 @@datein               99
     C  n99*usa          move      @@datein      usaDate
     C*
     C                   when      @@fromfmt = '*DMY'
     C                   if        @@datein < 1000000
     C     *dmy          test(d)                 @@datein               99
     C  n99*dmy          move      @@datein      usaDate
     C                   else
     C     *eur          test(d)                 @@datein               99
     C  n99*eur          move      @@datein      usaDate
     C                   endif
     C*
     C                   when      @@fromfmt = '*DMYY'
     C     *eur          test(d)                 @@datein               99
     C  n99*eur          move      @@datein      usaDate
     C*
     C                   when      @@fromfmt = '*JUL'
     C     *jul          test(d)                 @@datein               99
     C  n99*jul          move      @@datein      usaDate
     C*
     C                   other
     C                   seton                                        99
     C*
     C                   endsl
     C**
     C*  If no errors, check year for out of bounds both low and high
     C**
     C                   if        *in99 = *off
     C                   extrct    usaDate:*Y    checkYear
     C*
     C                   if        checkYear < 1900
     C                             or checkYear > 2099
     C                   seton                                        99
     C                   endif
     C                   endif
     C*
     C                   if        *in99 = *off
     C                   if        @@tofmt = '*MDY'
     C                             or @@tofmt = '*YMD'
     C                             or @@tofmt = '*DMY'
     C                             or @@tofmt = '*JUL'
     C                   if        checkYear < 1940
     C                             or checkYear > 2039
     C                   seton                                        99
     C                   endif
     C                   endif
     C                   endif
     C**
     C*  If no errors, convert *USA date to the output field. The value
     C*  must be left justified in the output field.
     C**
     C                   if        *in99 = *off
     C                   select
     C*
     C                   when      @@tofmt = *blanks
     C                   move      usaDate       numDate8
     C                   movel(p)  numDate8      @@dateout
     C*
     C                   when      @@tofmt = '*CYMD'
     C                   move      usaDate       isoDate
     C                   move      isoDate       numDate8
     C     numDate8      sub       19000000      numDate7
     C                   movel(p)  numDate7      @@dateout
     C*
     C                   when      @@tofmt = '*YMD'
     C                   move      usaDate       ymdDate
     C                   move      ymdDate       numDate6
     C                   movel(p)  numDate6      @@dateout
     C*
     C                   when      @@tofmt = '*YYMD'
     C                   move      usaDate       isoDate
     C                   move      isoDate       numDate8
     C                   movel(p)  numDate8      @@dateout
     C*
     C                   when      @@tofmt = '*MDY'
     C                   move      usaDate       mdyDate
     C                   move      mdyDate       numDate6
     C                   movel(p)  numDate6      @@dateout
     C*
     C                   when      @@tofmt = '*MDYY'
     C                   move      usaDate       numDate8
     C                   movel(p)  numDate8      @@dateout
     C*
     C                   when      @@tofmt = '*DMY'
     C                   move      usaDate       dmyDate
     C                   move      dmyDate       numDate6
     C                   movel(p)  numDate6      @@dateout
     C*
     C                   when      @@tofmt = '*DMYY'
     C                   move      usaDate       eurDate
     C                   move      eurDate       numDate8
     C                   movel(p)  numDate8      @@dateout
     C*
     C                   when      @@tofmt = '*JUL'
     C                   move      usaDate       julDate
     C                   move      julDate       numDate5
     C                   movel(p)  numDate5      @@dateout
     C*
     C                   when      @@tofmt = '*ISO'
     C                   move      usaDate       isoDate
     C                   movel(p)  isoDate       @@dateout
     C*
     C                   other
     C                   seton                                        99
     C*
     C                   endsl
     C                   endif
     C**
     C                   return
