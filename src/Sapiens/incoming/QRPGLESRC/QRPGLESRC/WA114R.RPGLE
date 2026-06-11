    Hcopyright ('(c) Copyright Fiserv, Inc., 2000 - 2005')
    H*****************************************************************
    H/TITLE WA114R - S/R EXCLUSION API
    H*****************************************************************
    H*                                                               *
    H*  DESCRIPTION:                                                 *
    H*    Check the billing schedule code assigned employer to       *
    H*    determine if it is a "Special" S/R processing code.        *
    H*    If it is,                                                  *
    H*      - Determine if the reporting month/year falls within the *
    H*        "# of months to be reported" in the billing schedule   *
    H*        code definition.                                       *
    H*      - Determine if the reporting month/year falls on         *
    H*        the month designated as the "Month deposit is to be    *
    H*        transferred" in the billing schedule code definition.  *
    H*                                                               *
    H*  CALLED FROM:                                                 *
    H*    W7221R - Monthly Interim Payroll Report                    *
    H*    W0161R - Mass Cancellations workfile build                 *
    H*    W7571R - S/R Late Notice                                   *
    H*    W7581R - Agent S/R Late Notice                             *
    H*                                                               *
    H*  CALLS TO:                                                    *
    H*                                                               *
    H*                                                               *
    H*  CREATED: 03/02/00  C:61  PROJECT: 109AM                      *
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
    H*  06/24/05  C:33  Project: 109WDZ                              *
    H*                  Converted to RPGIV and moved to base.        *
    H*****************************************************************
    H*                                                               *
    H*  INDICATOR USAGE:                                             *
    H*                                                               *
    H*                                                               *
    H*****************************************************************
     *
     *
     *  Billing Schedule Code
     Fwtabl     if   e           k disk
     *
     *
     D rm              s              5  0 dim(13)
     *
     *
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
     *
     *
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ###mth          s              3  0
     D ##frdt          s              8  0
     D #7Frdt          s              7  0
     D ##pfdt          s              8  0
     D #7Pfdt          s              7  0
     D ##tmth          s              3  0
     D ##yymm          s              5  0
     D #mnths          s              3  0
     D @@bscd          s              3
     D @@co#           s              3
     D @@exfl          s              1
     D @@pfdt          s              7
     D @@tdfl          s              1
     D @@yymm          s              5
     D day             s              2  0
     D factor          s              1  0
     D srdti           s              8  0
     D srdto           s             10
     D srffmt          s              8
     D srtfmt          s              8
     D x               s              3  0
     D xxbscd          s                   like(bskey)
     D xxco#           s                   like(bsco#)
     D xxpfdt          s                   like(bsdt)
     D xxyymm          s              5  0
     D pfDt            s               d
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D*
     D/copy sPrDate

     DDtFutureMth      PR             8  0
     D numDate1                       8  0 Const
     D numOfDays                      5  0 Const
     *
     *================================================================
     *  PLIST definitions
     *================================================================
     *
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@pfdt
     C                   parm                    @@bscd
     C                   parm                    @@yymm
     C                   parm                    @@exfl
     C                   parm                    @@tdfl
     *
     *
     *================================================================
     *  KLIST definitions
     *================================================================
     *
     *   KEY LIST FOR THE BILLING SCHEDULE CODE FILE
     *
     C     bsky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxbscd
     *
     *
     *================================================================
     *  Normal Programming Area
     *================================================================
     *
     C                   time                    dsdttm
     *
     C                   move      @@co#         xxco#
     C                   move      @@pfdt        xxpfdt
     C                   movel(p)  @@bscd        xxbscd
     C                   move      @@yymm        xxyymm
     C                   move      'N'           @@exfl
     C                   move      'N'           @@tdfl
     C*
     C                   eval      pfDt = CvtToDate(xxPfDt)
     *
     C     bsky          chain     pbs01                              60
     *
     C                   if        *in60 = *off
     C                   exsr      Process
     C                   endif
     *
     C                   eval      *inlr = *on
     C*
     C*====================================================================
     C* Execute the process
     C*====================================================================
     C     Process       begsr
     *
     * If the mo/yr passed in for employer is not one of the months
     * that should be reported based on the billing schedule code
     * definition, then set flag to exclude from reporting EXFL = Y
     *
     C                   eval      ##PfDt = dtMDYY(xxPfDt)
     *
     C                   eval      rm = 0
     * Load the array, RM, with the values for the months to be reported
     * If S/R Frequency is 'M' then report every month up to BS#MTH,
     *                  is 'Q' then rprt every 3rd month, BS#MTH times
     *                  is 'S' then rprt on the 6th month if BS#MTH=1
     *
     *     Assign multiplier based on S/R Frequency
     C                   select
     C                   when      bssrfq = 'M'
     C                             or bssrfq = ' '
     C                   eval      factor = 1
     C                   eval      #mnths = 13
     C                   when      bssrfq = 'Q'
     C                   eval      factor = 3
     C                   when      bssrfq = 'S'
     C                   eval      factor = 6
     C                   endsl
    C*
     C                   if        bsmsfl = 'Y'
     C                   eval      #mnths = bs#mth
     C                   endif
    C*
     C                   extrct    pfDt:*days    day
     *
     *     Calc months to be reported and load them into the array
     C                   do        #mnths        x
     C                   eval      ##frdt = ##pfdt
     C                   eval      ###mth = x * factor
     C                   eval      ###mth = ###mth - 1
    C*
    C*         If policy starts after 15th of month, add 1 month for
    C*         'Quarterly' and 'Semi-annual' processing
     C                   if        day > 15
     C                             and bssrfq = 'Q'
     C                             or day > 15
     C                             and bssrfq = 'S'
     C                   eval      ###mth = ###mth + 1
     C                   endif
     C*
     C                   eval      ##FrDt = DtFutureMth(##FrDt:###mth)
     C                   eval      #7FrDt = DtCYMD(##FrDt)
     C                   movel     #7FrDt        ##yymm
     C                   eval      rm(x) = ##yymm
     C*
     C                   enddo
     *
     * If the month is not in the array then set exclude flag to 'Y'
     C     xxyymm        lookup    rm                                     77
     C                   if        *in77 = *off
     C                   move      'Y'           @@exfl
     C                   endif
     *
     * If the mo/yr passed in for employer equals "month deposit to be
     * transferred" on the billing schedule code definition,
     * then set flag to transfer deposit. TDFL = Y
     *
     C                   eval      ##PfDt = DtMDYY(xxPfDt)
     *
     C                   eval      ##tmth = bstmth - 1
     *
     C                   eval      ##PfDt = DtFutureMth(##PfDt:##tmth)
     C                   eval      #7PfDt = DtCYMD(##PfDt)
     C                   movel     #7PfDt        ##yymm
     *
     C                   if        xxyymm = ##yymm
     C                   move      'Y'           @@tdfl
     C                   endif
     *
     C                   endsr
     *





     P*=====================================================================
     P* DtFutureMth - calculates a numeric date in the future after adding
     P*               a numeric value of months
     P*  Parameter 1 - 8 numeric date
     P*  Parameter 2 - 5 numeric
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     PDtFutureMth      B                   Export
     DDtFutureMth      PI             8  0
     D numDate                        8  0 Const
     D numOfMths                      5  0 Const
     D
     D isoDate1        s               d   datfmt(*iso)
     D isoDate2        s               d   datfmt(*iso)
     D date8           s              8  0
     C
     C                   eval      isoDate1 = CvtToDate(numDate)
     C                   if        isoDate1 <> isoDate2
     C     isoDate1      adddur    numOfMths:*m  isoDate2               99
     C                   if        *in99 = *off
     C                   eval      date8 = DateTo8(isoDate2)
     C                   endif
     C                   endif
     C
     C                   return    date8
     PDtFutureMth      E
