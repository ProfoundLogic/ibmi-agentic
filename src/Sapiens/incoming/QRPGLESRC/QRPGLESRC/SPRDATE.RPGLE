     D*=====================================================================
     D* Date handling procedure prototypes
     D*=====================================================================
     P*=====================================================================
     P* AgingDays - Returns 30, 60, 90...
     P*  Parameter 1 - comparison date
     P*  Parameter 2 - max days (increments of 30) optional {default 90}
     P*=====================================================================
     DAgingDays        pr            10    varying
     D inDate                          d   const
     D inMaxDays                     10i 0 const options(*nopass)

     P*=====================================================================
     P* AlphaMonth - Returns the name of the month
     P*  Parameter 1 - 2 numeric month
     P*  Parameter 2 - 10 alpha (*SHORT, *LONG) optional
     P*  Returns - 10 character (blank if invalid)
     P*=====================================================================
     DAlphaMonth       pR            10    varying
     D inNumMonth                     2  0 const
     D inRtnType                     10    const options(*nopass)

     P*=====================================================================
     P* alphaUSA - convert date to alpha date in XX/XX/XXXX format
     P*  Parameter 1 - date field
     P*  Returns - alpha date
     P*=====================================================================
     DalphaUSA         PR            10
     D date                            d   datfmt(*usa) const
     D
     P*=====================================================================
     P* alpha6USA - convert MMYYYY date to alpha date in XX/XXXX format
     P*  Parameter 1 - numeric date
     P*  Returns - alpha date
     P*=====================================================================
     Dalpha6USA        PR             7
     D date                           6  0  const
     D
     P*=====================================================================
     P* alphaMDY - convert date to alpha date in XX/XX/XX format
     P*  Parameter 1 - date field
     P*  Returns - alpha date
     P*=====================================================================
     DalphaMDY         PR             8
     D date                            d   const
     D
     P*=====================================================================
     P* alpha6MDY - convert MMYYYY date to alpha date in XX/XX format
     P*  Parameter 1 - numeric date
     P*  Returns - alpha date
     P*=====================================================================
     Dalpha6MDY        PR             5
     D date                           4  0  const
     D
     P*=====================================================================
     P* cvtToDate - convert number to date data type
     P*  Parameter 1 - 8 numeric date
     P*  Returns - date data type (date will be 01/01/0001 if invalid)
     P*=====================================================================
     DCvtToDate        PR              d
     D numDate                        8  0 Const
     D
     P*=====================================================================
     P* DateTo7 - Converts a date field to *CYMD numeric
     P*  Parameter 1 - date
     P*  Returns - 7 numeric date
     P*=====================================================================
     DDateTo7          PR             7  0
     D date                            d   Const

     P*=====================================================================
     P* DateTo8 - Converts a date field to *MDYY numeric
     P*  Parameter 1 - date
     P*  Returns - 8 numeric date
     P*=====================================================================
     DDateTo8          PR             8  0
     D date                            d   datfmt(*USA) Const
     P*=====================================================================
     P* DateToYYMD - Converts a date field to *YYMD numeric
     P*  Parameter 1 - date
     P*  Returns - 8 numeric date
     P*=====================================================================
     DDateToYYMD       PR             8  0
     D date                            d   datfmt(*USA) Const

     P*=====================================================================
     P* dtCheck - checks an numeric date in *MDYY format for validity
     P*  Parameter 1 - 8 numeric date
     P*  Parameter 2 - 8 alpha dateType
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     DdtCheck          PR             8  0
     D numDate8                       8  0 Const
     D dateType                       8    const options(*nopass)
     P*=====================================================================
     P* dt6Check - checks an numeric date in MMYYYY format for validity
     P*  Parameter 1 - 6 numeric date
     P*  Returns - 6 numeric date (zero if invalid)
     P*=====================================================================
     Ddt6Check         PR             6  0
     D numDate6                       6  0 Const
     D
     D***  Begin Add  ***  06/12/19  **************************************
     P*=====================================================================
     P* Dt6to8 - converts a numeric date in MMYYYY format to MM01YYYY
     P*  Parameter 1 - 6 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     DDt6to8           pr             8  0
     D numDate6                       6  0 Const

     P*=====================================================================
     P* Dt8to6 - converts a numeric date in MM01YYYY format to MMYYYY
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 6 numeric date MMYYYY (zero if invalid)
     P*=====================================================================
     DDt8to6           pr             6  0
     D numDate8                       8  0 Const
     D***  End   Add  ***  06/12/19  **************************************

      *=====================================================================
      * RangeHas0229 - Returns *On if date range has a leap date
      *=====================================================================
     d RangeHas0229    pr              n
     d inFromDate                      d
     d inToDate                        d

     P*=====================================================================
     P* dtCYMD - converts a numeric date in *MDYY format to *CYMD format
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 7 numeric date (zero if invalid)
     P*=====================================================================
     DdtCYMD           PR             7  0
     D numDate8                       8  0 Const
     D
     P*=====================================================================
     P* dt5CYMD - converts a numeric date in MMYYYY format to CYYMM format
     P*  Parameter 1 - 6 numeric date
     P*  Returns - 5 numeric date (zero if invalid)
     P*=====================================================================
     Ddt5CYMD          PR             5  0
     D numDate6                       6  0 Const
     D
     P*=====================================================================
     P* dtMDYY - converts a numeric date in *CYMD format to *MDYY format
     P*  Parameter 1 - 7 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     DdtMDYY           PR             8  0
     D numDate7                       7  0 Const
     D
     P*=====================================================================
     P* dt6MDYY - converts a numeric date in CYYMM format to MMYYYY format
     P*  Parameter 1 - 5 numeric date
     P*  Returns - 6 numeric date (zero if invalid)
     P*=====================================================================
     Ddt6MDYY          PR             6  0
     D numDate5                       5  0 Const
     D
     P*=====================================================================
     P* dtYYMD - converts a numeric date in *MDYY format to *YYMD format
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     DdtYYMD           PR             8  0
     D numDate8                       8  0 Const
     D
     P*=====================================================================
     P* dtMDYY2 - converts a numeric date in *YYMD format to *MDYY format
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     DdtMDYY2          PR             8  0
     D numDate8                       8  0 Const
     D
     P*=====================================================================
     P* dtDiff - calculates the difference (in days) between two numeric
     P*          dates
     P*  Parameter 1 - 8 numeric date
     P*  Parameter 2 - 8 numeric date
     P*  Returns - 5 numeric (zero if invalid)
     P*=====================================================================
     DdtDiff           PR             5  0
     D numDate1                       8  0 Const
     D numDate2                       8  0 Const
     D
     P*=====================================================================
     P* dtFuture - calculates a numeric date in the future after adding
     P*          a numeric value
     P*  Parameter 1 - 8 numeric date
     P*  Parameter 2 - 5 numeric
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     DdtFuture         PR             8  0
     D numDate1                       8  0 Const
     D numOfDays                      5  0 Const
     D charCalFlg                     1    Const options(*nopass)
     D
     P*=====================================================================
     P* dtPast - calculates a numeric date in the past after subtracting
     P*          a numeric value
     P*  Parameter 1 - 8 numeric date
     P*  Parameter 2 - 5 numeric
     P*  Returns - 8 numeric (zero if invalid)
     P*=====================================================================
     DdtPast           PR             8  0
     D numDate1                       8  0 Const
     D numOfDays                      5  0 Const
     D
     P*=====================================================================
     P* dowLong - returns the long day of week
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 9 character (blank if invalid)
     P*=====================================================================
     DdowLong          PR             9
     D numDate1                       8  0 Const
     D

     P***  Begin Add  ***  06/12/19  ************************************
     P*=====================================================================
     P* DOWNum - returns the numeric value of the day of week from name
     P*           the first two characters of the name are used
     P*  Parameter 1 - name of day (abbreviations allowed)
     P*  Returns - 1 numeric  (1=Monday ... 7=Sunday)
     P*=====================================================================
     DDOWNum           pr            10i 0
     D inDay                         10    const varying
     P***  End   Add  ***  06/12/19  ************************************

     P*=====================================================================
     P* dowShort - returns the short day of week
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 4 character (blank if invalid)
     P*=====================================================================
     DdowShort         PR             4
     D numDate1                       8  0 Const
     D
     P*=====================================================================
     P* dow - returns numeric value for day of the week
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 1 numeric  (1=Sunday ... 7=Saturday)
     P*=====================================================================
     Ddow              PR            10i 0
     D numDate                        8  0 Const
     D
     P*=====================================================================
     P* GetFirstDay - Get the first day of the of the month
     P*  Parameter 1 - date field
     P*  Returns - date of the the first day of the month
     P*=====================================================================
     DGetFirstDay      PR              d
     D dateIn                          d   const
     D
     P*=====================================================================
     P* getLastDay - Get the last day of the of the month
     P*  Parameter 1 - date field
     P*  Returns - date of the the last day of the month
     P*=====================================================================
     DgetLastDay       PR              d
     D dateIn                          d   const
     D
      *============================================================
      * Returns *On if the wild card date was incremented
      *============================================================
     d GetNextDate     pr              n
     d  inDate                         d
     d  inWildCard                   10    Const
     d  inWildCardFmt                 6    Const
     P*=====================================================================
     P* JobEntSys - Job Entered System
     P*   Returns Timestamp process entered system.
     P*   (Only accurate down to seconds)
     P*=====================================================================
     D JobEntSys       PR              z
     D
      *============================================================
      * Returns *On if the inDate matches inCompDate. InCompDate
      *  can have wild card values...
      *============================================================
     d MatchesDate     pr              n
     d  inDate                         d   Const
     d  inCompDate                   10    Const
     d  inCompDateFmt                 6    Const
     d  inCompOper                   12    Const
     P*=====================================================================
     P* numDate - convert alpha date to numeric date
     P*  Parameter 1 - 8 alpha date
     P*  Returns - numeric date
     P*=====================================================================
     DnumDate          PR             8  0
     D alphaDate8                     8    Const
     D
     P*=====================================================================
     P* numDate6 - convert alpha MMYYYY date to numeric date
     P*  Parameter 1 - 8 alpha date
     P*  Returns - numeric date
     P*=====================================================================
     DnumDate6         PR             6  0
     D alphaDate6                     6    Const
     D

      *=====================================================================
      * RangeOverlap - Returns *On if date ranges overlap
      *  parm1 = Existing from date (eg. from file)
      *  parm2 = Existing to date   (eg. from file)
      *  parm3 = New from date      (eg. from screen)
      *  parm4 = New to date        (eg. from screen)
      *=====================================================================
     dRangeOverlap     pr              n
     d inDateFrom1                    8  0 const
     d inDateTo1                      8  0 const
     d inDateFrom2                    8  0 const
     d inDateTo2                      8  0 const

     P*=====================================================================
     P* systemDate - get the system date as a date field
     P*  Returns - system date
     P*=====================================================================
     DsystemDate       PR              d

     P*=====================================================================
     P* systemTime - get the system time as a time field
     P*  Returns - system time
     P*=====================================================================
     DsystemTime       PR              t

     P*=====================================================================
     P* TimestampAddDur - Timestamp add duration (mostly used by CL)
     P*  Parameter 1 - timestamp (20a)
     P*  Parameter 2 - durration value (5.0)
     P*  Parameter 3 - durration code (10a)
     P*   *YEARS    - *Y
     P*   *MONTHS   - *M
     P*   *DAYS     - *D
     P*   *HOURS    - *H
     P*   *MINUTES  - *MN
     P*   *SECONDS  - *S
     P*   *MSECONDS - *MS
     P*  Returns - 6 numeric date
     P*=====================================================================
     DTimestampAddDur  pr            20
     D inTimestamp                   20
     D inDurValue                     5  0 const
     D inDurCode                     10    const

     P*=====================================================================
     P* TimeTo6 - Converts a time field to *HMS numeric
     P*  Parameter 1 - time
     P*  Returns - 6 numeric date
     P*=====================================================================
     DTimeTo6          Pr             6  0
     D time                            t   Const
