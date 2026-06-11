     Hcopyright('(c) 2019 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H nomain
    H*********************************************************************
    H*  REVSIONS:                                                        *
    H*                                                                   *
    H*  12/19/19 C:33  Project: SY409                                    *
    H*                -Add AgingDays                                     *
    H*                                                                   *
    H*  06/12/19 C:33  Project: SY407                                    *
    H*                -Add DOWnum procedure                              *
    H*                -Export Dt8To6 and Dt6To8                          *
    H*                                                                   *
    H*  12/12/17 C:33  Project: 150WAQ                                   *
    H*                 Add RangeOverlap procedure                        *
    H*                                                                   *
    H*  06/26/17 C:136 Support: 46320                                    *
    H*                 Add RangeHas0229 procedure                        *
    H*                                                                   *
    H*  04/10/17 C:136 Project: 129WUL                                   *
    H*                 Add MatchesDate subprocedure                      *
    H*                                                                   *
    H*  01/31/17  C:33  Project: WC2405                                  *
    H*                  -TimeStamp arithmetic routines                   *
    H*                                                                   *
    H*  04/15/16  C:33  Project: SY285                                   *
    H*                  -Add AlphaMonth                                  *
    H*                                                                   *
    H*  08/26/14  C:283 Support: WC133 85974                             *
    H*                  -Add the calculation for adding months to a date *
    H*                                                                   *
    H*  05/19/12  C:62  Project: Transform (WC2115)                      *
    H*                  -Convert YYMD dates to MDYY                      *
    H*                  -Convert MDYY to YYMD                            *
    H*                                                                   *
    H*  03/12/12  C:65  Project: Transform (WC2116)                      *
    H*                  Add check for null before attempting to convert  *
    H*                  a date field, and return 0 instead.              *
    H*                                                                   *
    H*  10/19/11  C:61  Project: 139IQJ                                  *
    H*                 -Add JobEntSys procedure.                         *
    H*                                                                   *
    H*  11/10/11 C:136 Support: WA132 77464                              *
    H*                 Add type to dtCheck so that it can validate       *
    H*                 a date for a specific type (*MDY, *YMD, etc)      *
    H*                                                                   *
    H*  11/19/09  C:33  Project: 25WQI                                   *
    H*                 -Export DOW procedure                             *
    H*                 -Add GetFirstDay procedure                        *
    H*                                                                   *
    H*  12/02/07  C:33  Support                                          *
    H*                  Dt6To8 does not work with 99xx                   *
    H*                                                                   *
    H*  10/03/05  C:33  Seq#: WA091/49201                                *
    H*                  Allow for conversion of dates greater than 2039. *
    H*                                                                   *
    H*  10/10/03  C:33  Project: WC1017                                  *
    H*                  Add DateTo7, DateTo8 and TimeTo6                 *
    H*********************************************************************
     D*
     D/copy sprdate
     D***  Begin Add  ***  06/12/19  ***************************
     D/copy sPrString
     D***  End   Add  ***  06/12/19  ***************************
     D
     D***  Begin Del  ***  06/12/19  ***************************
     D**********dt6to8           PR             8  0
     D********** numDate6                       6  0 Const
     D**********
     D**********dt8to6           PR             6  0
     D********** numDate8                       8  0 Const
     D***  End   Del  ***  06/12/19  ***************************
     D
     P***  Begin Add  ***  12/19/19  **************************
     P*=====================================================================
     P* AgingDays - Returns 30, 60, 90...
     P*  Parameter 1 - comparison date
     P*  Parameter 2 - max days (increments of 30) optional {default 90}
     P*                (negative for future dates)
     P*=====================================================================
     PAgingDays        b                   export
     DAgingDays        pi            10    varying
     D inDate                          d   const
     D inMaxDays                     10i 0 const options(*nopass)

     DdecDays          s             10i 0 inz
     DdecMaxDays       s             10i 0 inz(90)
     DdecMult          s             10i 0 inz(1)
     DdtInz            s               d   inz
     Di1               s             10i 0 inz
     DstrAging         s             10    inz varying
      /Free
       If %parms >= 2;
          decMaxDays = inMaxDays;
       Endif;
       If inDate = dtInz;
          Return *blanks;
       Endif;
       decDays = %diff(inDate:%date():*d);
       Select;
       When decMaxDays < 0;
           Exsr AgingFuture;
       Other;
          Exsr AgingPast;
       Endsl;
       Return strAging;
       //=========================
       Begsr AgingFuture;
        Select;
        When decDays >= 0;
           Leavesr;
        When %abs(decDays) > %abs(decMaxDays);
           strAging = %char(%abs(decMaxDays)) + '+';
           Leavesr;
        Endsl;
        For i1 = decMaxDays-1 to 30 by 30;
            If decDays <= i1;
               Leave;
            Endif;
        Endfor;
        //i1 += 1;
        strAging = %char(%abs(i1)) + '-' + %char(%abs(i1+1-30));
       Endsr;
       //=========================
       Begsr AgingPast;
        Select;
        When decDays <= 0;
           Leavesr;
        When %abs(decDays) > %abs(decMaxDays);
           Return %char(%abs(decMaxDays)) + '+';
           Leavesr;
        Endsl;
        For i1 = decMaxDays+1 downto 30 by 30;
            If decDays >= i1;
               Leave;
            Endif;
        Endfor;
        //i1 -= 1;
        strAging = %char(%abs(i1)) + '-' + %char(%abs(i1-1+30));
       Endsr;
      /End-free
     PAgingDays        e
     P***  End   Add  ***  12/19/19  **************************

     P*=====================================================================
     P* AlphaMonth - Returns the name of the month
     P*  Parameter 1 - 2 numeric month
     P*  Parameter 2 - 10 alpha (*SHORT, *LONG) optional
     P*  Returns - 10 character (blank if invalid)
     P*=====================================================================
     PAlphaMonth       b                   export
     DAlphaMonth       pi            10    varying
     D inNumMonth                     2  0 const
     D inRtnType                     10    const options(*nopass)
     D
     D strRtnType      s             10    inz('*SHORT')
     D
     D                 DS
     D dsLong                       120
     D  longJan                1     10    inz('January')
     D  longFeb               11     20    inz('February')
     D  longMar               21     30    inz('March')
     D  longApr               31     40    inz('April')
     D  longMay               41     50    inz('May')
     D  longJun               51     60    inz('June')
     D  longJul               61     70    inz('July')
     D  longAug               71     80    inz('August')
     D  longSep               81     90    inz('September')
     D  longOct               91    100    inz('October')
     D  longNov              101    110    inz('November')
     D  longDec              111    120    inz('December')
     D longMonth                     10    dim(12) overlay(dsLong)
     D                 DS
     D dsShort                       36
     D  shortJan               1      3    inz('Jan')
     D  shortFeb               4      6    inz('Feb')
     D  shortMar               7      9    inz('Mar')
     D  shortApr              10     12    inz('Apr')
     D  shortMay              13     15    inz('May')
     D  shortJun              16     18    inz('Jun')
     D  shortJul              19     21    inz('Jul')
     D  shortAug              22     24    inz('Aug')
     D  shortSep              25     27    inz('Sep')
     D  shortOct              28     30    inz('Oct')
     D  shortNov              31     33    inz('Nov')
     D  shortDec              34     36    inz('Dec')
     D shortMonth                     3    dim(12) overlay(dsShort)
     D
      /Free
       If %parms >= 2 and inRtnType <> *blank;
          strRtnType = inRtnType;
       Endif;

       Select;
       When inNumMonth < 1 or inNumMonth > 12;
          Return *blanks;
       When strRtnType = '*LONG';
          Return longMonth(inNumMonth);
       Other;
          Return shortMonth(inNumMonth);
       Endsl;

      /End-free
     PAlphaMonth       e

     P*=====================================================================
     P* alphaUSA - convert date to alpha date in XX/XX/XXXX format
     P*  Parameter 1 - date field
     P*  Returns - alpha date
     P*=====================================================================
     PalphaUSA         B                   Export
     DalphaUSA         PI            10
     D date                            d   datfmt(*usa) const
     D
     D alphaDate       s             10    inz
     D nullDate        s               d   inz
     C*
     C                   if        date <> nullDate
     C                   move      date          alphaDate
     C                   endif
     C
     C                   return    alphaDate
     PalphaUSA         E

     P*=====================================================================
     P* alpha6USA - convert MMYYYY date to alpha date in XX/XXXX format
     P*  Parameter 1 - numeric date
     P*  Returns - alpha date
     P*=====================================================================
     Palpha6USA        B                   Export
     Dalpha6USA        PI             7
     D date                           6  0  const
     D
     D alphaDate       s              7
     D numDate         s              6
     C*
     C                   move      date          numDate
     C                   eval      alphaDate = %subst(numDate:1:2) + '/' +
     C                                         %subst(numDate:3:4)
     C
     C                   return    alphaDate
     Palpha6USA        E

     P*=====================================================================
     P* alphaMDY - convert date to alpha date in XX/XX/XX format
     P*  Parameter 1 - date field
     P*  Returns - alpha date
     P*=====================================================================
     PalphaMDY         B                   Export
     DalphaMDY         PI             8
     D date                            d   const
     D
     D alphaDate       s              8    inz
     D nullDate        s               d   inz
     D outDate         s               d   inz
     D chgOfCent       s              4  0 inz(2039)
     D year            s              4  0
     C*
     C                   eval      outDate = date
     C                   extrct    date:*y       year
     C                   if        year > chgOfCent
     C                   subdur    100:*y        outDate
     C                   endif
     C*
     C                   if        date <> nullDate
     C     *mdy          move      outDate       alphaDate
     C                   endif
     C
     C                   return    alphaDate
     PalphaMDY         E

     P*=====================================================================
     P* alpha6MDY - convert MMYY date to alpha date in XX/XX format
     P*  Parameter 1 - numeric date
     P*  Returns - alpha date
     P*=====================================================================
     Palpha6MDY        B                   Export
     Dalpha6MDY        PI             5
     D date                           4  0  const
     D
     D alphaDate       s              5
     D numDate         s              4
     C*
     C                   move      date          numDate
     C                   eval      alphaDate = %subst(numDate:1:2) + '/' +
     C                                         %subst(numDate:3:2)
     C
     C                   return    alphaDate
     Palpha6MDY        E

     P*=====================================================================
     P* cvtToDate - convert number to date data type
     P*  Parameter 1 - 8 numeric date
     P*  Returns - date data type (date will be 01/01/0001 if invalid)
     P*=====================================================================
     PcvtToDate        B                   Export
     DcvtToDate        PI              d
     D numDate8                       8  0 Const
     D
     D date1           s               d
     D date2           s               d   datfmt(*ISO)
     D year            s              4  0
     C*
     C                   if        numDate8 < 1000000
     C     *mdy          test(d)                 numDate8               99
     C  n99*mdy          move      numDate8      date1
     C                   else
     C     *usa          test(d)                 numDate8               99
     C  n99*usa          move      numDate8      date1
     C                   If        Not *in99
     C     *usa          move      numDate8      date1
     C                   Else
     C     *iso          test(d)                 numDate8               99
     C  n99*iso          move      numDate8      date2
     C                   move      date2         date1
     C                   EndIf
     C                   endif
     C
     C  n99              extrct    date1:*y      year
     C
     C                   if        *in99 or year < 1900
     C     *cymd         test(d)                 numDate8               99
     C  n99*cymd         move      numDate8      date1
     C                   endif
     C
     C                   return    date1
     PCvtToDate        E

     P*=====================================================================
     P* DateTo7 - Converts a date field to *CYMD numeric
     P*  Parameter 1 - date
     P*  Returns - 7 numeric date
     P*=====================================================================
     PDateTo7          B                   Export
     DDateTo7          PI             7  0
     D date                            d   Const
     D
     D num7            s              7  0
     d nullDate        s               d
     C
     C                   if        date <> nullDate
     C
     C     *cymd         move      date          num7
     C
     c                   else
     c                   z-add     0             num7
     c                   endif
     C                   return    num7
     PDateTo7          E

     P*=====================================================================
     P* DateTo8 - Converts a date field to *MDYY numeric
     P*  Parameter 1 - date
     P*  Returns - 8 numeric date
     P*=====================================================================
     PDateTo8          B                   Export
     DDateTo8          PI             8  0
     D date                            d   datfmt(*USA) Const
     D
     D num8            s              8  0
     d nullDate        s               d
     C
     C                   if        date <> nullDate
     C
     C                   move      date          num8
     C
     c                   else
     c                   z-add     0             num8
     c                   endif
     C                   return    num8
     PDateTo8          E
     P*=====================================================================
     P* DateToYYMD - Converts a date field to *YYMD numeric
     P*  Parameter 1 - date
     P*  Returns - 8 numeric date
     P*=====================================================================
     PDateToYYMD       B                   Export
     DDateToYYMD       PI             8  0
     D date                            d   datfmt(*USA) Const
     D
     D num8            s              8  0
     d nullDate        s               d
      /Free
       If date<>nullDate;
          num8=%Dec(date:*ISO);
       Else;
          num8=0;
       EndIf;

       Return num8;
      /End-Free
     PDateToYYMD       E

     P*=====================================================================
     P* dtCheck - checks an numeric date in *MDYY format for validity
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     PdtCheck          B                   Export
     DdtCheck          PI             8  0
     D numDate8                       8  0 Const
     D dateType                       8    const options(*nopass)
     D
     D num8            s              8  0
     D usaDate1        s               d   datfmt(*usa)
     D usaDate2        s               d   datfmt(*usa)
     d ymdDate         s               d   datfmt(*ymd)
     d dmyDate         s               d   datfmt(*dmy)
     d mdyDate         s               d   datfmt(*mdy)
     d julDate         s               d   datfmt(*jul)
     d type            s              8
     d lo              c                   'abcdefghijklmnopqrstuvwxyz'
     d up              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     C
     C                   eval      usaDate1 = cvtToDate(numDate8)
     C                   if        usaDate1 <> usaDate2
     C                   move      usaDate1      num8
     C                   endif

     c                   if        %parms >= 2 and
     c                             dateType <> *blanks and
     c                             num8 <> 0
     c                   eval      type = %xlate(lo : up : dateType)
     c                   monitor
     c                   select
     c                   when      type = '*YMD'
     c                   move      usaDate1      ymdDate
     c                   when      type = '*DMY'
     c                   move      usaDate1      dmyDate
     c                   when      type = '*MDY'
     c                   move      usaDate1      mdyDate
     c                   when      type = '*JUL'
     c                   move      usaDate1      julDate
     c                   endsl
     c                   on-error
     c                   eval      num8 = 0
     c                   endmon
     c                   endif

     C                   return    num8
     PdtCheck          E

     P*=====================================================================
     P* dt6Check - checks an numeric date in MMYYYY format for validity
     P*  Parameter 1 - 6 numeric date
     P*  Returns - 6 numeric date (zero if invalid)
     P*=====================================================================
     Pdt6Check         B                   Export
     Ddt6Check         PI             6  0
     D numDate6                       6  0 Const
     D
     D num6            s              6  0
     D num8            s              8  0
     C
     C                   eval      num8 = dt6to8(numDate6)
     C                   eval      num8 = dtCheck(num8)
     C                   eval      num6 = dt8to6(num8)
     C*
     C                   return    num6
     C
     Pdt6Check         E

     P*=====================================================================
     P* dtCYMD - converts a numeric date in *MDYY format to *CYMD format
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 7 numeric date (zero if invalid)
     P*=====================================================================
     PdtCYMD           B                   Export
     DdtCYMD           PI             7  0
     D numDate8                       8  0 Const
     D
     D numDate7        s              7  0
     D num8            s              8  0
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)
     C
     C                   eval      isoDate1 = cvtToDate(numDate8)
     C                   if        isoDate1 <> isoDate2
     C                   move      isoDate1      num8
     C     num8          sub       19000000      numDate7
     C                   endif
     C
     C                   return    numDate7
     PdtCYMD           E

     P*=====================================================================
     P* dt5CYMD - converts a numeric date in MMYYYY format to CYYMM format
     P*  Parameter 1 - 6 numeric date
     P*  Returns - 5 numeric date (zero if invalid)
     P*=====================================================================
     Pdt5CYMD          B                   Export
     Ddt5CYMD          PI             5  0
     D numDate6                       6  0 Const
     D
     D dtCYYMM         s              5  0
     D num8            s              8  0
     D num7            s              7  0
     C
     C                   eval      num8    = dt6to8(numDate6)
     C                   eval      num7    = dtCYMD(num8)
     C                   movel     num7          dtCYYMM
     C
     C                   return    dtCYYMM
     C
     Pdt5CYMD          E

     P*=====================================================================
     P* dtMDYY - converts a numeric date in *CYMD format to *MDYY format
     P*  Parameter 1 - 7 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     PdtMDYY           B                   Export
     DdtMDYY           PI             8  0
     D numDate7                       7  0 Const
     D
     D numDate8        s              8  0
     D usaDate         s               d   datfmt(*usa)
     C
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
     PdtMDYY           E

     P*=====================================================================
     P* dt6MDYY - converts a numeric date in CYYMM format to MMYYYY format
     P*  Parameter 1 - 5 numeric date
     P*  Returns - 6 numeric date (zero if invalid)
     P*=====================================================================
     Pdt6MDYY          B                   Export
     Ddt6MDYY          PI             6  0
     D numDate5                       5  0 Const
     D
     D dtMMYYYY        s              6  0
     D num8            s              8  0
     D num7            s              7  0
     C
     C                   movel     numDate5      num7
     C                   eval      num7     = num7 + 01
     C                   eval      num8     = dtMDYY(num7)
     C                   eval      dtMMYYYY = dt8to6(num8)
     C
     C                   return    dtMMYYYY
     C
     Pdt6MDYY          E
     P*=====================================================================
     P* dtYYMD - converts a numeric date in *MDYY format to *YYMD format
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     PdtYYMD           B                   Export
     DdtYYMD           PI             8  0
     D numDate8                       8  0 Const
     D
     D numDateYYMD     s              8  0 inz(*Zeros)
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)
      /Free
       isoDate1=cvtToDate(numDate8);
       If isoDate1 <> isoDate2;
          numDateYYMD=%Dec(isoDate1:*ISO);
       EndIf;

       Return numDateYYMD;
      /End-Free
     PdtYYMD           E
     P*=====================================================================
     P* dtMDYY2 - converts a numeric date in *YYMD format to *MDYY format
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     PdtMDYY2          B                   Export
     DdtMDYY2          PI             8  0
     D numDate8                       8  0 Const
     D
     D usaDate         s               d   datfmt(*usa)
     D isoDate         s               d   datfmt(*iso)
     D numDateMDYY     s              8  0 inz(*zeros)
      /Free
       test(de) *ISO numdate8;
       If Not %Error;
          usaDate=cvtToDate(numDate8);
          numDateMDYY=%Dec(usaDate:*USA);
       Else;
          numDateMDYY=0;
       EndIf;

       Return numDateMDYY;
      /End-Free
     PdtMDYY2          E

     P*=====================================================================
     P* dtDiff - calculates the difference (in days) between two numeric
     P*          dates
     P*  Parameter 1 - 8 numeric date
     P*  Parameter 2 - 8 numeric date
     P*  Returns - 5 numeric (zero if invalid)
     P*=====================================================================
     PdtDiff           B                   Export
     DdtDiff           PI             5  0
     D numDate1                       8  0 Const
     D numDate2                       8  0 Const
     D
     D diff            s              5  0
     D date1           s               d
     D date2           s               d
     D isoDate         s               d
     C
     C                   eval      date1 = cvtToDate(numDate1)
     C                   eval      date2 = cvtToDate(numDate2)
     C                   if        date1 <> isoDate  and
     C                             date2 <> isodate
     C     date2         subdur    date1         diff:*days             99
     C                   endif
     C                   return    diff
     PdtDiff           E
     P*=====================================================================
     P* dtFuture - calculates a numeric date in the future after adding
     P*          a numeric value
     P*  Parameter 1 - 8 numeric date
     P*  Parameter 2 - 5 numeric
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     PdtFuture         B                   Export
     DdtFuture         PI             8  0
     D numDate                        8  0 Const
     D numOfDays                      5  0 Const
     D charCalFlg                     1    Const options(*nopass)
     D
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)
     D date8           s              8  0
     D charFlg         s              1    inz(' ')
     C
     C                   eval      isoDate1 = cvtToDate(numDate)
     C                   if        isoDate1 <> isoDate2
      /Free
       If %Parms > 2;
          charFlg = charCalFlg;
       Else;
          charFlg = ' ';
       Endif;

       Monitor;
          Select;
          When charFlg = 'D' Or charFlg = ' ';
             isoDate2 = isoDate1 + %Days(numOfDays);

          When charCalFlg = 'M';
             isoDate2 = isoDate1 + %Months(numOfDays);

          When charCalFlg = 'Y';
             isoDate2 = isoDate1 + %Years(numOfDays);

          Other;
             *In99 = *On;
          Endsl;
       On-Error;
          *In99 = *On;
       EndMon;
      /End-Free
     C                   if        *in99 = *off
     C     *USA          move      isoDate2      date8
     C                   endif
     C                   endif
     C
     C                   return    date8
     PdtFuture         E
     P*=====================================================================
     P* dtPast - calculates a numeric date in the past after subtracting
     P*          a numeric value
     P*  Parameter 1 - 8 numeric date
     P*  Parameter 2 - 5 numeric
     P*  Returns - 8 numeric (zero if invalid)
     P*=====================================================================
     PdtPast           B                   Export
     DdtPast           PI             8  0
     D numDate1                       8  0 Const
     D numOfDays                      5  0 Const
     D
     D numOfDays2      s              5  0
     C
     C     numOfDays     ifgt      0
     C                   z-sub     numOfDays     numOfDays2
     C                   else
     C                   z-add     numOfDays     numOfDays2
     C                   endif
     C
     C                   return    dtFuture(numDate1 : numOfDays2)
     PdtPast           E

     P*=====================================================================
     P* dt6to8 - converts a numeric date in MMYYYY format to MM01YYYY
     P*  Parameter 1 - 6 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     P***  Begin Chg  ***  06/12/19  ***************************
     P**********dt6to8           B
     PDt6to8           B                   export
     P***  End   Chg  ***  06/12/19  ***************************
     Ddt6to8           PI             8  0
     D numDate6                       6  0 Const
     D
     D num8            s              8  0
     D alpha6          s              6
     D alpha8          s              8
     C
     C                   move      numDate6      alpha6
     C                   select
     C                   when      numDate6 > 10000
     C                   eval      alpha8   = %subst(alpha6:1:2) + '01' +
     C                                        %subst(alpha6:3:4)
     C                   when      %subst(alpha6:3:2) > '12'
     C                   eval      alpha8   = '00' + %subst(alpha6:5:2) +
     C                                        '01' + %subst(alpha6:3:2)
     C                   other
     C                   eval      alpha8   = '00' + %subst(alpha6:3:2) +
     C                                        '01' + %subst(alpha6:5:2)
     C                   endsl
     C                   move      alpha8        num8
     C*
     C                   eval      num8 = dtCheck(num8)
     C*
     C                   return    num8
     C
     Pdt6to8           E

     P*=====================================================================
     P* dt8to6 - converts a numeric date in MM01YYYY format to MMYYYY
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 6 numeric date MMYYYY (zero if invalid)
     P*=====================================================================
     P***  Begin Chg  ***  06/12/19  ***************************
     P**********dt8to6           B
     PDt8to6           B                   export
     P***  End   Chg  ***  06/12/19  ***************************
     Ddt8to6           PI             6  0
     D numDate8                       8  0 Const
     D
     D num6            s              8  0
     D alpha6          s              6
     D alpha8          s              8
     C
     C                   move      numDate8      alpha8
     C                   eval      alpha6   = %subst(alpha8:1:2) +
     C                                        %subst(alpha8:5:4)
     C                   move      alpha6        num6
     C*
     C                   return    num6
     C
     Pdt8to6           E

     P*=====================================================================
     P* dowLong - returns the long day of week
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 9 character (blank if invalid)
     P*=====================================================================
     PdowLong          B                   Export
     DdowLong          PI             9
     D numDate                        8  0 Const
     D
     D dayOfWeek       s             10i 0
     D
     DWeek             DS
     D allDays                       63    inz('Monday   Tuesday  Wednesday+
     D                                     Thursday Friday   Saturday Sunday  ')
     D day                            9    dim(7) overlay(allDays)
     D
     C                   eval      dayOfWeek = dow(numDate)
     C                   if        dayOfWeek = 0
     C                   return    'Invalid'
     C                   else
     C                   return    day(dayOfWeek)
     C                   endif
     PdowLong          E

     P***  Begin Add  ***  06/12/19  ************************************
     P*=====================================================================
     P* DOWNum - returns the numeric value of the day of week from name
     P*           the first two characters of the name are used
     P*  Parameter 1 - name of day (abbreviations allowed)
     P*  Returns - 1 numeric  (1=Monday ... 7=Sunday)
     P*=====================================================================
     PDOWNum           b                   export
     DDOWNum           pi            10i 0
     D inDay                         10    const varying

     DdsWeek           ds
     D allDays                       28    inz('MOTUWETHFRSASU')
     D day                            2    dim(7) overlay(allDays)
      /Free
       Return %lookup(Upper(%subst(inDay:1:2)):day:1);
      /End-free
     PDOWNum           e
     P***  End   Add  ***  06/12/19  ************************************

     P*=====================================================================
     P* dowShort - returns the short day of week
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 4 character (blank if invalid)
     P*=====================================================================
     PdowShort         B                   Export
     DdowShort         PI             4
     D numDate                        8  0 Const
     D
     D dayOfWeek       s             10i 0
     D
     DWeek             DS
     D allDays                       28    inz('Mon.Tue.Wed.Thu.Fri.Sat.Sun.')
     D day                            4    dim(7) overlay(allDays)
     D
     C                   eval      dayOfWeek = dow(numDate)
     C                   if        dayOfWeek = 0
     C                   return    'Inv.'
     C                   else
     C                   return    day(dayOfWeek)
     C                   endif
     PdowShort         E
     P*=====================================================================
     P* dow - returns numeric value for day of the week
     P*  Parameter 1 - 8 numeric date
     P*  Returns - 1 numeric  (1=Sunday ... 7=Saturday)
     P*=====================================================================
     Pdow              B                   export
     Ddow              PI            10i 0
     D numDate                        8  0 Const
     D
     D baseDate        s               d   static inz(d'1900-01-01')
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)
     D nDays           s              7  0
     D answer          s              7  0
     D remain          s              1  0
     C
     C                   eval      isoDate1 = cvtToDate(numDate)
     C                   if        isoDate1 <> isoDate2
     C                   if        isoDate1 > baseDate
     C     isoDate1      subdur    baseDate      nDays:*days
     C     nDays         div       7             answer
     C                   mvr                     remain
     C                   add       1             remain
     C                   endif
     C                   endif
     C*
     C                   return    remain
     Pdow              E

     P*=====================================================================
     P* GetFirstDay - Get the first day of the of the month
     P*  Parameter 1 - date field
     P*  Returns - date of the the first day of the month
     P*=====================================================================
     PGetFirstDay      B                   Export
     DGetFirstDay      PI              d
     D dateIn                          d   const
     D
     D date            s               d
     D day             s              3  0
     C*
     C                   eval      date = dateIn
     C                   eval      day = %subdt(date:*d) - 1
     C                   subdur    day:*d        date
     C
     C                   return    date
     PGetFirstDay      E

     P*=====================================================================
     P* getLastDay - Get the last day of the of the month
     P*  Parameter 1 - date field
     P*  Returns - date of the the last day of the month
     P*=====================================================================
     PgetLastDay       B                   Export
     DgetLastDay       PI              d
     D dateIn                          d   const
     D
     D date            s               d
     D day             s              3  0
     C*
     C     dateIn        adddur    1:*m          date
     C                   extrct    date:*d       day
     C                   subdur    day:*d        date
     C
     C                   return    date
     PgetLastDay       E

      *============================================================
      * Returns *On if the wild card date was incremented
      *============================================================
     p GetNextDate     b
     d GetNextDate     pi              n
     d  inDate                         d
     d  inWildCard                   10    Const
     d  inWildCardFmt                 6    Const

     d dsWildCardUSA   ds            10
     d  mmUSA                         2    overlay(dsWildCardUSA:1)
     d  ddUSA                         2    overlay(dsWildCardUSA:4)
     d  ccyyUSA                       4    overlay(dsWildCardUSA:7)

     d  mm             s              2
     d  dd             s              2
     d  ccyy           s              4
     d maxDate         s               d   datfmt(*usa/)
     d saveDate        s               d   datfmt(*usa/)

      /Free
       Select;
       When inWildCardFmt = '*USA/';
          dsWildCardUSA = inWildCard;
          mm   = mmUSA;
          dd   = ddUSA;
          ccyy = ccyyUSA;
       Endsl;
       saveDate = inDate;
       maxDate  = %Date() + %Years(10);
       If dd = '**';
          inDate = inDate + %Days(1);
          Select;
          When inDate > maxDate;
             Return *Off;
          When %Subdt(inDate:*D) > %Subdt(saveDate:*D);
             Return *On;
          When mm = '**' and %Subdt(inDate:*M) > %Subdt(saveDate:*M);
             Return *On;
          When ccyy = '****' and %Subdt(inDate:*Y) > %Subdt(saveDate:*Y);
             Return *On;
          Endsl;
          If mm <> '**' and %Subdt(inDate:*M) > %Int(mm);
             inDate = inDate - %Months(1);
          Endif;
          If ccyy <> '****' and %Subdt(inDate:*Y) > %Int(ccyy);
             inDate = inDate - %Years(1);
          Endif;
       Endif;
       If mm = '**';
          Eval inDate = inDate + %Months(1);
          Select;
          When inDate > maxDate;
             Return *Off;
          When %Subdt(inDate:*M) > %Subdt(saveDate:*M);
             Return *On;
          When ccyy = '****' and  %Subdt(inDate:*Y) > %Subdt(saveDate:*Y);
             Return *On;
          Endsl;
          If ccyy <> '****' and %Subdt(inDate:*Y) > %Int(ccyy);
             inDate = inDate - %Years(1);
          Endif;
       Endif;
       If ccyy = '****';
          inDate = inDate + %Years(1);
          Select;
          When inDate > maxDate;
             Return *Off;
          Other;
             Return *On;
          Endsl;
       Endif;
       Return *Off;
      /End-Free
     p GetNextDate     e

      *=====================================================================
      * JobEntSys - Job Entered System
      *   Returns Timestamp process entered system.
      *   (Only accurate down to seconds)
      *=====================================================================
     P JobEntSys       B                   Export
     D JobEntSys       PI              z

     d @@timestamp     s               z
     d xxtime          s             20a
     d xxdate          s             20a

     D ERRC0100        Ds                  Qualified
     D  BytPrv                       10i 0 Inz( %Size( ERRC0100 ))
     D  BytAvl                       10i 0
     D  MsgId                         7a
     D                                1a
     D  MsgDta                      128a

     D JOBI0400        Ds                  Qualified
     D  BytRtn                       10i 0
     D  BytAvl                       10i 0
     D  JobNam                       10a
     D  UsrNam                       10a
     D  JobNbr                        6a
     D  Date_time                    13a   Overlay( JOBI0400: 63 )

     D RtvJobInf       Pr                  ExtPgm( 'QUSRJOBI' )
     D  RiRcvVar                  32767a          Options( *VarSize )
     D  RiRcvVarLen                  10i 0 Const
     D  RiFmtNam                      8a   Const
     D  RiJobNamQ                    26a   Const
     D  RiJobIntId                   16a   Const
     D  RiError                   32767a   Options( *NoPass: *VarSize )
     D  RiRstStc                      1a          Options( *NoPass )

      /Free

       RtvJobInf( JOBI0400
                 : %Size( JOBI0400 )
                 : 'JOBI0400'
                 : '*'
                 : *Blank
                 : ERRC0100
              );

       If  ERRC0100.BytAvl > *Zero;
          @@timestamp = %timestamp;
       Else;
          xxdate =
            %char(%date(%subst(JOBI0400.Date_Time :1:7):*cymd0):*iso0);
          xxtime =
            %char(%time(%subst(JOBI0400.Date_Time :8):*hms0):*iso0) +
            '000000';
          @@timestamp = %timestamp( %trim(xxdate) + %trim(xxtime) : *iso0);
       Endif;

       return @@timestamp;

      /End-free

     P JobEntSys       E

      *============================================================
      * Returns *On if the inDate matches the comparison date value
      *  which may use '*' for wild card matching
      *============================================================
     p MatchesDate     b                   Export
     d MatchesDate     pi              n
     d  inDate                         d   Const
     d  inCompDate                   10    Const
     d  inCompDateFmt                 6    Const
     d  inCompOper                   12    Const

     d dsWildCardUSA   ds            10
     d  mmUSA                         2    overlay(dsWildCardUSA:1)
     d  ddUSA                         2    overlay(dsWildCardUSA:4)
     d  ccUSA                         2    overlay(dsWildCardUSA:7)
     d  yyUSA                         2    overlay(dsWildCardUSA:9)

     d cc              s              2
     d dd              s              2
     d mm              s              2
     d yy              s              2
     d chrCompDate     s             10
     d compDate        s               d
     d nextDate        s               d

      /Free
       Select;
       When inCompDateFmt = '*USA/';
          dsWildCardUSA = inCompDate;
          mm   = mmUSA;
          dd   = ddUSA;
          cc   = ccUSA;
          yy   = yyUSA;
       Endsl;
       If %Scan('*':inCompDate) <> 0;
         // Get the earliest date the wild card date allows and
         //  return *on if the inDate matches it
          If dd = '**';
             dd = '01';
          Endif;
          If mm = '**';
             mm = '01';
          Endif;
          If cc = '**';
             cc = '19';
          Endif;
          // Pass '****' in ccyy position of chrCompDate so that
          //  GetNextDate works properly if yy is a wildcard
          chrCompDate = inCompDate;
          If yy = '**';
             %Subst(chrCompDate:7:2) = '**';
          Endif;
          Select;
          When yy <> '**';
          When cc = '19';
             yy = '70';
          When cc = '20';
             yy = '00';
          Endsl;
          nextDate = %date(cc + yy + '-' + mm  + '-' + dd:*iso-);
          If inDate = nextDate;
             Return *On;
          Endif;
         // Get next available date the wild card allows and return *on
         //  if the inDate matches it
          Dow GetNextDate(nextDate:chrCompDate:inCompDateFmt) = *On;
             If inDate = nextDate;
                Return *On;
             Endif;
          Enddo;
       Else;
          Select;
          When inCompDateFmt = '*USA/';
             compDate = %Date(inCompDate:*USA/);
          Endsl;
          Select;
          When inCompOper = 'EQUAL TO'
           and inDate = compDate;
             Return *On;
          When inCompOper = 'LESS THAN'
           and inDate < compDate;
             Return *On;
          When inCompOper = 'GREATER THAN'
           and inDate > compDate;
             Return *On;
          Endsl;
       Endif;
       Return *Off;
      /End-Free

     p MatchesDate     e

     P*=====================================================================
     P* numDate - convert alpha date to numeric date
     P*  Parameter 1 - 8 alpha date
     P*  Returns - numeric date
     P*=====================================================================
     PnumDate          B                   Export
     DnumDate          PI             8  0
     D alphaDate8                     8    Const
     D
     D date8           s              8  0
     C*
     C                   move      alphaDate8    date8
     C
     C                   return    date8
     PnumDate          E

     P*=====================================================================
     P* numDate6 - convert alpha MMYYYY date to numeric date
     P*  Parameter 1 - 8 alpha date
     P*  Returns - numeric date
     P*=====================================================================
     PnumDate6         B                   Export
     DnumDate6         PI             6  0
     D alphaDate6                     6    Const
     D
     D date6           s              6  0
     C*
     C                   move      alphaDate6    date6
     C
     C                   return    date6
     PnumDate6         E

      *=====================================================================
      * RangeHas0229 - Returns *On if date range has a leap date
      *=====================================================================
     p RangeHas0229    b                   export
     d RangeHas0229    pi              n
     d inFromDate                      d
     d inToDate                        d
     d invalidDate     s               n
     d fromYear        s              4  0
     d leapDate        s               d
     d leapMthDay      s              6    inz('-02-29')
     d toYear          s              4  0
     d #OfDays         s              3  0
      /Free
       fromYear = %Subdt(inFromDate:*y);
       invalidDate = *Off;
       Monitor;
          leapDate = %Date(%Editc(fromYear:'X') + leapMthDay:*Iso);
       On-Error;
          invalidDate = *On;
       Endmon;
       Select;
       When invalidDate = *On;
       When leapDate >= inFromDate and leapDate <= inToDate;
          Return *On;
       Endsl;
       toYear = %Subdt(inToDate:*y);
       If fromYear = toYear;
          Return *Off;
       Endif;
       invalidDate = *Off;
       Monitor;
          leapDate = %Date(%Editc(toYear:'X') + leapMthDay:*Iso);
       On-Error;
          invalidDate = *On;
       Endmon;
       Select;
       When invalidDate = *On;
       When leapDate >= inFromDate and leapDate <= inToDate;
          Return *On;
       Endsl;
       Return *Off;
      /End-Free
     p RangeHas0229    e

      *=====================================================================
      * RangeOverlap - Returns *On if date ranges overlap
      *  parm1 = Existing from date (eg. from file)
      *  parm2 = Existing to date   (eg. from file)
      *  parm3 = New from date      (eg. from screen)
      *  parm4 = New to date        (eg. from screen)
      *=====================================================================
     pRangeOverlap     b                   export
     dRangeOverlap     pi              n
     d inDateFrom1                    8  0 const
     d inDateTo1                      8  0 const
     d inDateFrom2                    8  0 const
     d inDateTo2                      8  0 const

     d dtFrom1         s               d
     d dtFrom2         s               d
     d dtTo1           s               d
     d dtTo2           s               d
     d dtNull          s               d   inz
      /Free
       dtFrom1 = CvtToDate(inDateFrom1);
       dtFrom2 = CvtToDate(inDateFrom2);
       dtTo1 = CvtToDate(inDateTo1);
       dtTo2 = CvtToDate(inDateTo2);
       Select;
       When dtFrom1 = dtTo1;
          Return *on;
       When dtFrom2 = dtTo2;
          Return *on;
       When dtTo1 = dtNull and dtTo2 = dtNull;
          Return *on;
       When dtTo1 = dtNull and dtFrom2 >= dtFrom1;
          Return *on;
       When dtTo1 = dtNull and dtTo2 > dtFrom1;
          Return *on;
       When dtTo2 = dtNull and dtFrom2 < dtTo1;
          Return *on;
       When dtTo2 > dtFrom1 and dtFrom2 < dtTo1;
          Return *on;
       Endsl;
       Return *off;
      /End-Free
     pRangeOverlap     e

     P*=====================================================================
     P* systemDate - get the system date as a date field
     P*  Returns - system date
     P*=====================================================================
     PsystemDate       B                   Export
     DsystemDate       PI              d
     D
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D
     D date            s               d
     C*
     C                   time                    dsdttm
     C     *mdy          move      dsdate        date
     C
     C                   return    date
     C*
     PsystemDate       E

     P*=====================================================================
     P* systemTime - get the system time as a time field
     P*  Returns - system time
     P*=====================================================================
     PsystemTime       B                   Export
     DsystemTime       PI              t
     D
     D                 ds
     D dsdttm                        12  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        6  0 overlay(dsdttm:7)
     D
     D time            s               t
     C*
     C                   time                    dsdttm
     C                   move      dstime        time
     C
     C                   return    time
     C*
     PsystemTime       E

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
     PTimestampAddDur  b                   export
     DTimestampAddDur  pi            20
     D inTimestamp                   20
     D inDurValue                     5  0 const
     D inDurCode                     10    const
     D
     D outTimestamp    s               z
      /Free
       outTimestamp = %timestamp(inTimestamp:*iso0);
       Select;
       When inDurCode = '*YEARS'    or inDurCode = '*Y';
          outTimestamp = outTimestamp + %years(inDurValue);
       When inDurCode = '*MONTHS'   or inDurCode = '*M';
          outTimestamp = outTimestamp + %months(inDurValue);
       When inDurCode = '*DAYS'     or inDurCode = '*D';
          outTimestamp = outTimestamp + %days(inDurValue);
       When inDurCode = '*HOURS'    or inDurCode = '*H';
          outTimestamp = outTimestamp + %hours(inDurValue);
       When inDurCode = '*MINUTES'  or inDurCode = '*MN';
          outTimestamp = outTimestamp + %minutes(inDurValue);
       When inDurCode = '*SECONDS'  or inDurCode = '*S';
          outTimestamp = outTimestamp + %seconds(inDurValue);
       When inDurCode = '*MSECONDS' or inDurCode = '*MS';
          outTimestamp = outTimestamp + %mseconds(inDurValue);
       Endsl;
       Return %char(outTimestamp:*iso0);
      /End-free
     PTimestampAddDur  E

     P*=====================================================================
     P* TimeTo6 - Converts a time field to *HMS numeric
     P*  Parameter 1 - time
     P*  Returns - 6 numeric date
     P*=====================================================================
     PTimeTo6          B                   Export
     DTimeTo6          PI             6  0
     D time                            t   Const
     D
     D num6            s              7  0
     C
     C     *hms          move      time          num6
     C
     C                   return    num6
     PTimeTo6          E
