     *========================================================================
     * (C) Copyright Progressive Data Solutions, Inc., 2000
     *========================================================================
      * DPRDATE - Date procedure prototypes
      *
      * Created   : 03/31/00
      * Programmer: C:113
      * Project   : DARTS
      *
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
     * NOTES:
     *
     *
     *========================================================================

      *=====================================================================
      * CvtToDate
      *=====================================================================
      * Description: Converts number to date data type.
      *
      * Parameter1:  8.0 - date to convert
      *
      * Returns: date data type (date will be 01/01/0001 if invalid)
      *=====================================================================
     D CvtToDate       PR              d
     D  date                          8  0 Const
     D


      *=====================================================================
      * DateCheck
      *=====================================================================
      * Description: Checks a numeric date in *MDYY format for validity
      *
      * Parameter 1:  8.0 - date to validate
      *
      * Returns:      1a - *on if valid, *off if invalid
      *=====================================================================
     D DateCheck       PR             1a
     D  date                          8  0 Const


      *=====================================================================
      * Cvt2cymd
      *=====================================================================
      * Description: Converts a numeric date in *MDYY format to *CYMD format
      *
      * Parameter 1:  8.0 - date to convert
      *
      * Returns:      7.0 - converted date (zero if invalid)
      *=====================================================================
     D Cvt2cymd        PR             7  0
     D  date8                         8  0 Const


      *=====================================================================
      * Cvt2mdyy
      *=====================================================================
      * Description: Converts a numeric date in *CYMD format to *MDYY format
      *
      * Parameter 1:   7.0 - date to convert
      *
      * Returns:       8.0 - converted date (zero if invalid)
      *=====================================================================
     D Cvt2mdyy        PR             8  0
     D  date7                         7  0 Const


      *=====================================================================
      * DateDiff -
      *=====================================================================
      * Description: Calculates the difference (in days) between two numeric
      *              dates
      *
      * Parameter 1:  8.0 - date 1
      * Parameter 2:  8.0 - date 2
      *
      * Returns:      5.0 - difference in days (0 if invalid dates entered)
      *=====================================================================
     D DateDiff        PR             5  0
     D  date1                         8  0 Const
     D  date2                         8  0 Const


      *=====================================================================
      * DateFuture
      *=====================================================================
      * Description: Calculates a numeric date in the future after adding
      *              a numeric value
      *
      * Parameter 1:  8.0 - start date
      * Parameter 2:  5.0 - days to add
      *
      * Returns:      8.0 - new date (zero if invalid)
      *=====================================================================
     D DateFuture      PR             8  0
     D  date1                         8  0 Const
     D  numOfDays                     5  0 Const


      *=====================================================================
      * DatePast
      *=====================================================================
      * Description: Calculates a numeric date in the past after subtracting
      *              a numeric value
      *
      * Parameter 1:  8.0 - start date
      * Parameter 2:  5.0 - days to subtract
      *
      * Returns:      8.0 - new date (zero if invalid)
      *=====================================================================
     D DatePast        PR             8  0
     D  date1                         8  0 Const
     D  numOfDays                     5  0 Const


      *=====================================================================
      * DayLong -
      *=====================================================================
      * Description: Returns the long day of week
      *
      * Parameter 1:  8.0 - date
      *
      * Returns:      9a - day of the week (blank if invalid date entered)
      *=====================================================================
     D DayLong         PR             9a
     D  date1                         8  0 Const


      *=====================================================================
      * DayShort - returns the short day of week
      *=====================================================================
      * Description: Returns the short day of the week.
      *
      * Parameter 1:  8.0 - date
      *
      * Returns:      4a - day of the week (blank if invalid date entered)
      *=====================================================================
     D DayShort        PR             4a
     D  date1                         8  0 Const


      *=====================================================================
      * DayNumber
      *=====================================================================
      * Description: Returns the number for the day of the week.
      *
      * Parameter 1:  8.0 - date
      *
      * Returns:     10.0i - 1=Sunday ... 7=Saturday
      *=====================================================================
     D DayNumber       PR            10i 0
     D  date                          8  0 Const


