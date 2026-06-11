     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
      * DPIDATE - API's to handle date conversions and calculations.
      *
      * Created   : 04/19/00
      * Programmer: C:113
      * Project   : DARTS
      *
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================
     * REVISIONS:
     *
     * XX/XX/XXXX C:XXX PROJECT: XXXXX
     *            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     H nomain

     *========================================================================
      * Procedure Prototypes
     *========================================================================
      /copy DPRDATE


      *------------------------------------------------------------------------
      * CvtToDate - Converts a numeric date to the date data format
      *------------------------------------------------------------------------
     P CvtToDate       B                   Export

      *** Procedure Interface ***
     D CvtToDate       PI              d
     D  numDate8                      8  0 Const

      *** Locals ***
     D date1           s               d

      *** Procedure Definition ***
     C                   if        numDate8 < 1000000
     C     *mdy          test(d)                 numDate8               99
     C  n99*mdy          move      numDate8      date1
     C                   else
     C     *usa          test(d)                 numDate8               99
     C  n99*usa          move      numDate8      date1
     C                   endif
     C
     C                   return    date1

     P CvtToDate       E

      *------------------------------------------------------------------------
      * DateCheck - checks an numeric date in *MDYY format for validity
      *------------------------------------------------------------------------
     P DateCheck       B                   Export

      *** Procedure Interface ***
     D DateCheck       PI             1a
     D  numDate8                      8  0 Const

      *** Locals ***
     D num8            s              8  0
     D usaDate1        s               d   datfmt(*usa)
     D usaDate2        s               d   datfmt(*usa)

      *** Procedure Definition ***
     C                   eval      usaDate1 = cvtToDate(numDate8)
     C                   if        usaDate1 <> usaDate2
     C                   move      usaDate1      num8
     C                   endif

     C                   if        num8 = *zeros
     C                   return    *off
     C                   else
     C                   return    *on
     C                   endif

     P DateCheck       E


      *------------------------------------------------------------------------
      * Cvt2cymd - Converts a numeric date in *MDYY format to *CYMD format
      *------------------------------------------------------------------------
     P Cvt2cymd        B                   Export

      *** Procedure Interface ***
     D Cvt2cymd        PI             7  0
     D numDate8                       8  0 Const

      *** Locals ***
     D numDate7        s              7  0
     D num8            s              8  0
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)

      *** Procedure Definition ***
     C                   eval      isoDate1 = cvtToDate(numDate8)
     C                   if        isoDate1 <> isoDate2
     C                   move      isoDate1      num8
     C     num8          sub       19000000      numDate7
     C                   endif
     C
     C                   return    numDate7

     P Cvt2cymd        E


      *------------------------------------------------------------------------
      * Cvt2mdyy - Converts a numeric date in *CYMD format to *MDYY format
      *------------------------------------------------------------------------
     P Cvt2mdyy        B                   Export

      *** Procedure Interface ***
     D Cvt2mdyy        PI             8  0
     D  numDate7                      7  0 Const

      *** Locals ***
     D numDate8        s              8  0
     D usaDate         s               d   datfmt(*usa)

      *** Procedure Definition ***
     C     numDate7      add       19000000      numDate8
     C     *iso          test(d)                 numDate8               99
     C                   if        *in99 = *off
     C     *iso          move      numDate8      usaDate
     C                   move      usaDate       numDate8
     C                   else
     C                   move      *zeros        numDate8
     C                   endif
     C
     C                   return    numDate8

     P Cvt2mdyy        E


      *------------------------------------------------------------------------
      * DateDiff - Calculates the difference (in days) between two numeric
      *            dates.
      *------------------------------------------------------------------------
     P DateDiff        B                   Export

      *** Procedure Interface ***
     D DateDiff        PI             5  0
     D  numDate1                      8  0 Const
     D  numDate2                      8  0 Const

      *** Locals ***
     D diff            s              5  0
     D date1           s               d
     D date2           s               d
     D isoDate         s               d

      *** Procedure Definition ***
     C                   eval      date1 = cvtToDate(numDate1)
     C                   eval      date2 = cvtToDate(numDate2)

     C                   if        date1 <> isoDate  and
     C                             date2 <> isodate
     C     date2         subdur    date1         diff:*days             99
     C                   endif

     C                   return    diff

     P DateDiff        E


      *------------------------------------------------------------------------
      * dateFuture - Calculates a numeric date in the future after adding
      *              a numeric value.
      *------------------------------------------------------------------------
     P DateFuture      B                   Export

      *** Procedure Interface ***
     D DateFuture      PI             8  0
     D  numDate                       8  0 Const
     D  numOfDays                     5  0 Const

      *** Locals ***
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)
     D date8           s              8  0

      *** Procedure Definition ***
     C                   eval      isoDate1 = cvtToDate(numDate)

     C                   if        isoDate1 <> isoDate2
     C     isoDate1      adddur    numOfDays:*d  isoDate2               99
     C                   if        *in99 = *off
     C     *USA          move      isoDate2      date8
     C                   endif
     C                   endif
     C
     C                   return    date8

     P DateFuture      E


      *------------------------------------------------------------------------
      * datePast - Calculates a numeric date in the past after subtracting
      *            a numeric value.
      *------------------------------------------------------------------------
     P DatePast        B                   Export

      *** Procedure Interface ***
     D DatePast        PI             8  0
     D  numDate1                      8  0 Const
     D  numOfDays                     5  0 Const

      *** Locals ***
     D numOfDays2      s              5  0

      *** Procedure Definition ***
     C     numOfDays     ifgt      0
     C                   z-sub     numOfDays     numOfDays2
     C                   else
     C                   z-add     numOfDays     numOfDays2
     C                   endif
     C
     C                   return    DateFuture(numDate1 : numOfDays2)

     P DatePast        E


      *------------------------------------------------------------------------
      * DayLong - Returns the long day of week.
      *------------------------------------------------------------------------
     P DayLong         B                   Export

      *** Procedure Interface ***
     D DayLong         PI             9
     D  numDate                       8  0 Const

      *** Locals ***
     D dayOfWeek       s             10i 0

     D Week            DS
     D  allDays                      63    inz('Monday   Tuesday  Wednesday+
     D                                     Thursday Friday   Saturday Sunday  ')

     D  day                           9    dim(7) overlay(allDays)

      *** Procedure Definition ***
     C                   eval      dayOfWeek = DayNumber(numDate)
     C                   if        dayOfWeek = 0
     C                   return    'Invalid'
     C                   else
     C                   return    day(dayOfWeek)
     C                   endif

     P DayLong         E


      *------------------------------------------------------------------------
      * DayShort - Returns the short day of week.
      *------------------------------------------------------------------------
     P DayShort        B                   Export

      *** Procedure Interface ***
     D DayShort        PI             4
     D  numDate                       8  0 Const

      *** Locals ***
     D dayOfWeek       s             10i 0

     D Week            DS
     D  allDays                      28    inz('Mon.Tue.Wed.Thu.Fri.Sat.Sun.')

     D day                            4    dim(7) overlay(allDays)

      *** Procedure Definition ***
     C                   eval      dayOfWeek = DayNumber(numDate)
     C                   if        dayOfWeek = 0
     C                   return    'Inv.'
     C                   else
     C                   return    day(dayOfWeek)
     C                   endif

     P DayShort        E


      *------------------------------------------------------------------------
      * DayNumber - Returns numeric value for day of the week.
      *------------------------------------------------------------------------
     P DayNumber       B

      *** Procedure Interface ***
     D DayNumber       PI            10i 0
     D  numDate                       8  0 Const

      *** Locals ***
     D baseDate        s               d   static inz(d'1900-01-01')
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)
     D nDays           s              7  0
     D answer          s              7  0
     D remain          s              1  0

      *** Procedure Definition ***
     C                   eval      isoDate1 = cvtToDate(numDate)
     C                   if        isoDate1 <> isoDate2
     C                   if        isoDate1 > baseDate
     C     isoDate1      subdur    baseDate      nDays:*days
     C     nDays         div       7             answer
     C                   mvr                     remain
     C                   add       1             remain
     C                   endif
     C                   endif

     C                   return    remain

     P DayNumber       E


