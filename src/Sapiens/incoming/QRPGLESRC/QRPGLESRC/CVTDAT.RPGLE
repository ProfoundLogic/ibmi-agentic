     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     * XX/XX/XXXX C:XXX PROJECT: XXXXX                                        *
     *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
     *                                                                        *
     * ---------------------------------------------------------------------- *
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.                    *
     * ---------------------------------------------------------------------- *
     *========================================================================*
     * INDICATOR USAGE:                                                       *
     *                                                                        *
     *  99 - General Error indicator.                                         *
     *                                                                        *
     *======================================================================= *
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     H NOMAIN

      ** Procedure Prototype
      /COPY CVTDATINC

     *========================================================================*
     * CvtDate                                                                *
     *========================================================================*
      * Description: Converts date formats.                                    *
      *                                                                        *
     * Parameters : 8.0 - date to convert                                     *
      *              8a - date from format                                     *
      *              8a - date to format                                       *
      *                                                                        *
      * Returns    : 10a - Converted date, or blanks for error                 *
     *========================================================================*
     P CvtDate         B                   EXPORT

      *** Procedure Interface ***
     D CvtDate         PI            10a
     D  @@datein                      8  0 Const
     D  @@fromfmt                     8a   Const
     D  @@tofmt                       8a   Const

      *** Locals ***
     D xxDateOut       s             10

     D dmyDate         s               d   datfmt(*dmy)
     D eurDate         s               d   datfmt(*eur)
     D isoDate         s               d   datfmt(*iso)
     D julDate         s               d   datfmt(*jul)
     D mdyDate         s               d   datfmt(*mdy)
     D usaDate         s               d   datfmt(*usa)
     D ymdDate         s               d   datfmt(*ymd)
     D numDate5        s              5  0
     D numDate6        s              6  0
     D numDate7        s              7  0
     D numDate8        s              8  0
     D checkYear       s              4  0

      *** Procedure Definition ***
     C                   Eval      *in99 = *off
     C                   Move      *blanks       xxDateOut

     C                   If        @@datein = 0
     C                   Return    *blanks
     C                   EndIf

      ** Convert 8 digit number to a *USA type date.
     C                   Select

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
     C                   Return    *blanks
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
     C                   Return    *blanks
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
     C                   movel(p)  numDate8      xxDateOut
     C*
     C                   when      @@tofmt = '*CYMD'
     C                   move      usaDate       isoDate
     C                   move      isoDate       numDate8
     C     numDate8      sub       19000000      numDate7
     C                   movel(p)  numDate7      xxDateOut
     C*
     C                   when      @@tofmt = '*YMD'
     C                   move      usaDate       ymdDate
     C                   move      ymdDate       numDate6
     C                   movel(p)  numDate6      xxDateOut
     C*
     C                   when      @@tofmt = '*YYMD'
     C                   move      usaDate       isoDate
     C                   move      isoDate       numDate8
     C                   movel(p)  numDate8      xxDateOut
     C*
     C                   when      @@tofmt = '*MDY'
     C                   move      usaDate       mdyDate
     C                   move      mdyDate       numDate6
     C                   movel(p)  numDate6      xxDateOut
     C*
     C                   when      @@tofmt = '*MDYY'
     C                   move      usaDate       numDate8
     C                   movel(p)  numDate8      xxDateOut
     C*
     C                   when      @@tofmt = '*DMY'
     C                   move      usaDate       dmyDate
     C                   move      dmyDate       numDate6
     C                   movel(p)  numDate6      xxDateOut
     C*
     C                   when      @@tofmt = '*DMYY'
     C                   move      usaDate       eurDate
     C                   move      eurDate       numDate8
     C                   movel(p)  numDate8      xxDateOut
     C*
     C                   when      @@tofmt = '*JUL'
     C                   move      usaDate       julDate
     C                   move      julDate       numDate5
     C                   movel(p)  numDate5      xxdateout
     C*
     C                   when      @@tofmt = '*ISO'
     C                   move      usaDate       isoDate
     C                   movel(p)  isoDate       xxdateout
     C*
     C                   other
     C                   Return    *blanks
     C*
     C                   endsl
     C                   endif

     C                   If        *in99 = *on
     C                   Eval      xxDateOut = *blanks
     C                   EndIf

     C                   Return    xxDateOut

     P CvtDate         E
