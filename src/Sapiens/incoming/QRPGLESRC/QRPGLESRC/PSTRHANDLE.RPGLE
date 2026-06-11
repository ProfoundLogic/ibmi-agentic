     *========================================================================*
     * (C) Copyright Progressive Data Solutions, Inc., 2000                   *
     *========================================================================*
     * PSTRHANDLE - String and date handling procedures                       *
     *========================================================================*
     * REVISIONS:                                                             *
     *                                                                        *
     *======================================================================= *
     * NOTES:                                                                 *
     *                                                                        *
     *                                                                        *
     *========================================================================*
     H NOMAIN

     *========================================================================*
     * Procedure Prototypes                                                   *
     *========================================================================*
      /COPY PSTRINC

      *  System API to get operational descriptors
     DGetOpDesc        PR                  ExtProc('CEEDOD')
     D ParmPos                       10I 0 const
     D o_desctype                    10I 0
     D o_datatype                    10I 0
     D o_descinf1                    10I 0
     D o_descinf2                    10I 0
     D o_fieldlen                    10I 0
     D o_errors                      12a   options(*omit)


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

      *  Convert 8 digit number to a *USA type date.
     C                   Select

     C                   when      @@fromfmt = *blanks
     C                   if        @@datein < 1000000
     C     *jobrun       test(d)                 @@datein               99
     C  n99*jobrun       move      @@datein      usaDate
     C                   else
     C     *usa          test(d)                 @@datein               99
     C  n99*usa          move      @@datein      usaDate
     C                   endif

     C                   when      @@fromfmt = '*CYMD'
     C     @@datein      add       19000000      numDate8
     C     *iso          test(d)                 numDate8               99
     C  n99*iso          move      numDate8      usaDate

     C                   when      @@fromfmt = '*YMD'
     C                   if        @@datein < 1000000
     C     *ymd          test(d)                 @@datein               99
     C  n99*ymd          move      @@datein      usaDate
     C                   else
     C     *iso          test(d)                 @@datein               99
     C  n99*iso          move      @@datein      usaDate
     C                   endif

     C                   when      @@fromfmt = '*YYMD' or
     C                             @@fromfmt = '*ISO'
     C     *iso          test(d)                 @@datein               99
     C  n99*iso          move      @@datein      usaDate

     C                   when      @@fromfmt = '*MDY'
     C                   if        @@datein < 1000000
     C     *mdy          test(d)                 @@datein               99
     C  n99*mdy          move      @@datein      usaDate
     C                   else
     C     *usa          test(d)                 @@datein               99
     C  n99*usa          move      @@datein      usaDate
     C                   endif

     C                   when      @@fromfmt = '*MDYY'
     C     *usa          test(d)                 @@datein               99
     C  n99*usa          move      @@datein      usaDate

     C                   when      @@fromfmt = '*DMY'
     C                   if        @@datein < 1000000
     C     *dmy          test(d)                 @@datein               99
     C  n99*dmy          move      @@datein      usaDate
     C                   else
     C     *eur          test(d)                 @@datein               99
     C  n99*eur          move      @@datein      usaDate
     C                   endif

     C                   when      @@fromfmt = '*DMYY'
     C     *eur          test(d)                 @@datein               99
     C  n99*eur          move      @@datein      usaDate

     C                   when      @@fromfmt = '*JUL'
     C     *jul          test(d)                 @@datein               99
     C  n99*jul          move      @@datein      usaDate

     C                   other
     C                   seton                                        99

     C                   endsl

      *  If no errors, check year for out of bounds both low and high
     C                   if        *in99 = *off
     C                   extrct    usaDate:*Y    checkYear

     C                   if        checkYear < 1900
     C                             or checkYear > 2099
     C                   Return    *blanks
     C                   endif
     C                   endif

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

      *  If no errors, convert *USA date to the output field. The value
      *  must be left justified in the output field.
     C                   if        *in99 = *off
     C                   select

     C                   when      @@tofmt = *blanks
     C                   move      usaDate       numDate8
     C                   movel(p)  numDate8      xxDateOut

     C                   when      @@tofmt = '*CYMD'
     C                   move      usaDate       isoDate
     C                   move      isoDate       numDate8
     C     numDate8      sub       19000000      numDate7
     C                   movel(p)  numDate7      xxDateOut

     C                   when      @@tofmt = '*YMD'
     C                   move      usaDate       ymdDate
     C                   move      ymdDate       numDate6
     C                   movel(p)  numDate6      xxDateOut

     C                   when      @@tofmt = '*YYMD'
     C                   move      usaDate       isoDate
     C                   move      isoDate       numDate8
     C                   movel(p)  numDate8      xxDateOut

     C                   when      @@tofmt = '*MDY'
     C                   move      usaDate       mdyDate
     C                   move      mdyDate       numDate6
     C                   movel(p)  numDate6      xxDateOut

     C                   when      @@tofmt = '*MDYY'
     C                   move      usaDate       numDate8
     C                   movel(p)  numDate8      xxDateOut

     C                   when      @@tofmt = '*DMY'
     C                   move      usaDate       dmyDate
     C                   move      dmyDate       numDate6
     C                   movel(p)  numDate6      xxDateOut

     C                   when      @@tofmt = '*DMYY'
     C                   move      usaDate       eurDate
     C                   move      eurDate       numDate8
     C                   movel(p)  numDate8      xxDateOut

     C                   when      @@tofmt = '*JUL'
     C                   move      usaDate       julDate
     C                   move      julDate       numDate5
     C                   movel(p)  numDate5      xxdateout

     C                   when      @@tofmt = '*ISO'
     C                   move      usaDate       isoDate
     C                   movel(p)  isoDate       xxdateout

     C                   other
     C                   Return    *blanks

     C                   endsl
     C                   endif

     C                   If        *in99 = *on
     C                   Eval      xxDateOut = *blanks
     C                   EndIf

     C                   Return    xxDateOut

     P CvtDate         E

     *========================================================================*
     * Center                                                                 *
     *========================================================================*
      * Description: Centers a character field                                 *
      *                                                                        *
     * Parameters : 1024a - field to be centered                              *
      *                                                                        *
      * Returns    : 1024a - centered field                                    *
     *========================================================================*
     P Center          B                   EXPORT

      *** Procedure Interface ***
     D Center          PI          1024a   opdesc
     D  @@string                   1024a   CONST

      *** Locals ***
     D new_string      s                   like(@@string)
     D tempString      s                   like(@@string)
     D x               s                   like(nlength)

      * API Parms
     D nDescType       s             10I 0
     D nDataType       s             10I 0
     D nDescInf1       s             10I 0
     D nDescInf2       s             10I 0
     D nlength         s             10I 0

     C                   Clear                   new_string
     C                   Clear                   tempString

     C                   If        @@string <> *blanks

     C                   Eval      tempString = @@string

     C                   CALLP     GetOpDesc( 1 : nDescType : nDataType :
     C                                            nDescInf1 : nDescInf2 :
     C                                            nlength   : *omit)

     C     ' '           Check     tempString    x
     C                   Eval      tempString = %subst(tempString : 1 : nlength)
     C     ' '           CheckR    tempString    x
     C                   Eval      x = nlength - x

     C                   If        x < *zero
     C                   Eval      x = x - x
     C                   EndIf

     C                   Eval      x = x / 2

     C                   Cat(p)    tempString:x  new_string

     C                   EndIf

     C                   Return    new_string

     P Center          E
