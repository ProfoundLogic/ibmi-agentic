     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
     * is970RPI - Procedure interfaces for is970R/R02
     *========================================================================
     * REVISIONS:
     *
     *
     * ----------------------------------------------------------------------
     *  NOTE: PLEASE ADD COMMENTS TO THE TOP OF REVISIONS.
     * ----------------------------------------------------------------------
     *========================================================================
     * NOTES:
     *
     *
     *========================================================================
      *========================================================================
      * Procedure Definitions
      *========================================================================

      *------------------------------------------------------------------------*
      * Cvt2CYMD - Convert date from MDYY to CYMD
      *------------------------------------------------------------------------*
      * Description: Converts date formats.                                    *
      *                                                                        *
     * Parameters : 8.0 - date to convert                                     *
      *                                                                        *
      * Returns    : 7.0 - Converted date, or zeros for error                  *
      *------------------------------------------------------------------------*
     P Cvt2CYMD        B

      *** Procedure Interface ***
     D Cvt2CYMD        PI             7  0
     D  dateIn                        8s 0

      *** Locals ***
     D isoDate         s               d   datfmt(*iso)
     D usaDate         s               d   datfmt(*usa)
     D ymdDate         s               d   datfmt(*ymd)
     D numDate7        s              7  0
     D numDate8        s              8  0

     C     *usa          test(d)                 dateIn                 99
     C  n99*usa          move      dateIn        usaDate
     C                   if        *in99 = *off
     C                   move      usaDate       isoDate
     C                   move      isoDate       numDate8
     C     numDate8      sub       19000000      numDate7
     C                   return    numDate7
     C                   else
     C                   return    *zeros
     C                   endif

     P Cvt2CYMD        E

