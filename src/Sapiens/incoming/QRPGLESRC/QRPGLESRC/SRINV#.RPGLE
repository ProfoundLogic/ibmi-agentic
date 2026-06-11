     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
    H*********************************************************************
    H/title SRINV# - Retrieve Invoice # - API                            *
    H*********************************************************************
    H*  Created: 11/19/09   C:33   Project: 25WQI                        *
    H*********************************************************************
    H*  Revisions:                                                       *
    H*                                                                   *
     H* 07/04/12  C:282  Support: WA025 80248                             *
     H*                  Replacing W1210R02 instead of W8020R added under *
     H*                  support 78731                                    *
    H*                                                                   *
     H* 01/03/12  C:62   Project: WA025 78731                             *
     H*                  If called from invoice process, calculate like   *
    H*                  it used too prior to 11/07 change.               *
    H*                                                                   *
     H* 11/07/11  C:62   Project: WA025 78298                             *
     H*                  Check to see if there is an invoice # on bill    *
    H*                  type.                                            *
    H*                                                                   *
     H* 02/26/10  C:33   Project: 25WRC  Priss #14                        *
     H*                  If frequency not specified, assume monthly.      *
    H*                                                                   *
     H* 01/04/10  C:190  PROJECT: 25WRE                                   *
     H*                  Changed the param condition to >= 8 .            *
    H*********************************************************************
     Fwdell     if   e           k disk    rename(pel01:pel00)
     Fwmeal6    if   e           k disk    rename(pea01:pea06)
     Fwmeal25   if   e           k disk    rename(pea01:pea25)
     F***  Begin Add  ***  03/15/10  **************************************
     Fwdeal2    if   e           k disk    rename(pea01:dea02)
     Fwdeal25   if   e           k disk    rename(pea01:dea25)
     F***  End   Add  ***  03/15/10  **************************************
     Fwmfnl     uf   e           k disk    rename(pfn01:pfn00)
     Fwtabl     if   e           k disk    include(pbs01)

     D*entry
     DSrInv#           pr
     D inCo#                               like(EACO#)  const
     D inFnd                               like(EAFND)  const
     D inEmp#                              like(EAEMP#) const
     D inDiv                               like(EADIV)  const
     D inFyr                               like(EAFYR)  const
     D inDate                              like(EADT)   const
     D invoice#                            like(EANINV)
     D inCreateNew                    1n
     DSrInv#           pi
     D inCo#                               like(EACO#)  const
     D inFnd                               like(EAFND)  const
     D inEmp#                              like(EAEMP#) const
     D inDiv                               like(EADIV)  const
     D inFyr                               like(EAFYR)  const
     D inDate                              like(EADT)   const
     D invoice#                            like(EANINV)
     D inCreateNew                    1n

     DcreateNew        s              1n   inz('1')
     D*** Begin Add *** 11/07/11 *******************************************
     D holdInvoice#    s                   like(EANINV) inz(0)
     D*** End   Add *** 11/07/11 *******************************************
     D***  Begin Add  ***  01/03/12  **************************************
     D invoices        s               n
     D***  End   Add  ***  01/03/12  **************************************
     Dpsds           esds                  extname(szq1p)

     D/copy sPrDate
     D/copy wPrAR

     D/copy srinv#pr

     C***  Begin Add  ***  01/03/12  **************************************
     C*
     C* Check to see if this was called from invoices.
     C*
     C***  Begin Chg  ***  07/04/12  **************************************
     C*****              eval      invoices=*off
     C****               call(E)   'SRCHKPGM'
     C****               parm      'W8020R'      ##PGM            10
     C****               parm      *BLANKS       ##ERR             1
     C****               if        ##ERR=*on
     C****               eval      invoices=*on
      *
     C                   eval      invoices=*on
     C                   call(E)   'SRCHKPGM'
     C                   parm      'W1210R02'    ##PGM            10
     C                   parm      *BLANKS       ##ERR             1
     C                   if        ##ERR=*on
     C                   eval      invoices=*off
     C***  End   Add  ***  07/04/12  **************************************
     C                   endif
     C***  End   Add  ***  01/03/12  **************************************
      *** BEG CHG **** 01/04/10 **********************************************
     C                   if        %parms >= 8
     C************       if        %parms >  8
      *** END CHG **** 01/04/10 **********************************************
     C                   eval      createNew = inCreateNew
     C                   endif
     C*** Begin Add *** 11/07/11 *******************************************
     C***  Begin Add  ***  01/03/12  **************************************
     C                   if        not invoices
     C***  End   Add  ***  01/03/12  **************************************
     C                   eval      holdInvoice#=invoice#
     C                   clear                   invoice#
     C***  Begin Add  ***  01/03/12  **************************************
     C                   endif
     C***  End   Add  ***  01/03/12  **************************************
     C*** End   Add *** 11/07/11 *******************************************
     C*
     C                   eval      invoice# = RetrieveInv#(inCo#
     C                                                    :inFnd
     C                                                    :inEmp#
     C                                                    :inDiv
     C                                                    :inFyr
     C                                                    :CvtToDate(inDate)
     C                                                    :createNew
     C                                                    )

     C*                  eval      *inlr = *on
     C                   return
      /eject
     P*=====================================================================
     PRetrieveInv#     b                   export
     DRetrieveInv#     pi                  like(EANINV)
     D inCo#                               like(EACO#)  const
     D inFnd                               like(EAFND)  const
     D inEmp#                              like(EAEMP#) const
     D inDiv                               like(EADIV)  const
     D inFyr                               like(EAFYR)  const
     D inDate                          d                const
     D inCreateNew                    1n                const options(*nopass)
     D
     Dinvoice#         s                   like(EANINV) inz
     DcreateNew        s              1n                inz('1')
     DxxFromDate       s               d
     DxxToDate         s               d
     Dx7FromDate       s              7  0
     Dx7ToDate         s              7  0
     Dday              s              2  0
     DbillDay          s              2  0
     DxxKey            s                   like(BSKEY)
     D***  Begin Add  ***  03/15/10  **************************************
     DttRRN            s              9  0
     D***  End   Add  ***  03/15/10  **************************************

     C     keyWMFNL      klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd

     C     keyWDELL      klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr

     C     keyWTABL      klist
     C                   kfld                    inCo#
     C                   kfld                    xxKey

     C     keyWMEAL6     klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr
     C                   kfld                    x7ToDate
     C*
     C     key1WMEAL6    klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr

     C     keyWMEAL25    klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    invoice#

     C***  Begin Add  ***  03/15/10  **************************************
     C     keyWDEAL2     klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inFyr
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    x7ToDate
     C*
     C     key1WDEAL2    klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    inFyr
     C                   kfld                    inEmp#
     C                   kfld                    inDiv

     C     keyWDEAL25    klist
     C                   kfld                    inCo#
     C                   kfld                    inFnd
     C                   kfld                    invoice#
     C***  End   Add  ***  03/15/10  **************************************

      *** BEG CHG **** 01/04/10 **********************************************
     C                   if        %parms >= 7
     C*************      if        %parms >  7
      *** END CHG **** 01/04/10 **********************************************
     C                   eval      createNew = inCreateNew
     C                   endif
     C*
     C     keyWDELL      chain     wdell
     C*
     C*** Begin Add *** 11/07/11 *******************************************
     C***  Begin Chg  ***  01/03/12  **************************************
     C**********         if        holdInvoice#=0
     C                   if        holdInvoice#=0 or invoices
     C***  End   Chg  ***  01/03/12  **************************************
     C*** End   Add *** 11/07/11 *******************************************
     C                   exsr      ChkPriorRcds
     C*** Begin Add *** 11/07/11 *******************************************
     C                   else
     C                   exsr      ChkBilledInv
     C                   endif
     C*** End   Add *** 11/07/11 *******************************************
     C*
     C                   if        invoice# <> *zeros
     C                   return    invoice#
     C                   endif
     C*
     C                   if        not createNew
     C                   return    invoice#
     C                   endif
     C*
     C* Retrieve/update next invoice #
     C     keyWMFNL      chain     WMFNL
     C                   if        %found
     C                   if        FNNINV = *zero
     C                   eval      FNNINV = 1
     C                   endif
     C***  Begin Chg  ***  03/15/10  **************************************
     C**********         dou       not %found(wmeal25)
     C                   dou       not %found(wmeal25)
     C                              and not %found(wdeal25)
     C***  End   Chg  ***  03/15/10  **************************************
     C                   eval      invoice# = FNNINV
     C                   eval      FNNINV = FNNINV + 1
     C     keyWMEAL25    chain     wmeal25
     C***  Begin Add  ***  03/15/10  **************************************
     C     keyWDEAL25    chain     wdeal25
     C***  End   Add  ***  03/15/10  **************************************
     C                   enddo
     C                   exsr      UpdateWMFNL
     C                   endif
     C
     C                   return    invoice#
     C*
     C*====================================================================
     C* Check for records during billing period to use same invoice #
     C*====================================================================
     C     ChkPriorRcds  begsr
     C*
     C* Get the billing schedule code information
     C                   eval      xxKey = %editc(ELBSCD:'X')
     C     keyWTABL      chain     pbs01
     C*
     C* Determine range of dates to be used based on type of billing
     C                   select
     C*  S/R - Use current month
     C                   when      BSSEFL = 'S'
     C                   exsr      FullMonth
     C*  Monthly - Use current month
     C                   when      BSFREQ = 'M'
     C                   exsr      FullMonth
     C*  Twice monthly - Use current month (days 1-15 or 16-31)
     C                   when      BSFREQ = 'T'
     C                   exsr      HalfMonth
     C*  Weekly - Use current week
     C                   when      BSFREQ = 'W'
     C                   exsr      WeekDates
     C***  Begin Add  ***  02/26/10  **************************************
     C*  Everything else - assume current month
     C                   other
     C                   exsr      FullMonth
     C***  End   Add  ***  02/26/10  **************************************
     C                   endsl
     C*
     C                   eval      x7FromDate = DateTo7(xxFromDate)
     C                   eval      x7ToDate   = DateTo7(xxToDate)
     C*
     C     keyWMEAL6     setgt     WMEAL6
     C                   dou       %eof
     C                              or EAPODT < x7FromDate
     C                              or (BillType(EAARTY)
     C                                   and EANINV <> *zero)
     C     key1WMEAL6    readpe    WMEAL6
     C                   enddo
     C*
     C***  Begin Add  ***  03/15/10  **************************************
     C* Negative logic over whether a record was found.  Used to determine
     C*  if A/R batch should be checked.
     C                   if        not (not %eof
     C                                  and (EAPODT >= x7FromDate and
     C                                       EAPODT <= x7ToDate)
     C                                  and EANINV <> *zero)
     C                   clear                   EAARTY
     C                   clear                   EANINV
     C     keyWDEAL2     setgt     WDEAL2
     C                   dou       %eof
     C                              or EAPODT < x7FromDate
     C                              or (BillType(EAARTY)
     C                                   and EANINV <> *zero)
     C     key1WDEAL2    readpe    WDEAL2
     C                   eval      EANINV = EAINV#
     C                   eval      EAARTY = GetARType(EAARCD:ttRRN:EACO#)
     C                   enddo
     C                   endif
     C***  End   Add  ***  03/15/10  **************************************
     C*
     C                   if        not %eof
     C                              and (EAPODT >= x7FromDate and
     C                                   EAPODT <= x7ToDate)
     C                              and EANINV <> *zero
     C                   eval      invoice# = EANINV
     C                   endif
     C*
     C                   endsr
     C*
     C*** Begin Add *** 11/07/11 *******************************************
     C*====================================================================
     C* Check for passed in invoice with existing billing record.
     C*====================================================================
     C     ChkBilledInv  begsr
     C*
     C     key1WMEAL6    setgt     WMEAL6
     C                   dou       %eof or (BillType(EAARTY)
     C                                       and EANINV = holdInvoice#)
     C     key1WMEAL6    readpe    WMEAL6
     C                   enddo
     C*
     C                   if        not %eof and EANINV <> *zero
     C                   eval      invoice# = EANINV
     C                   endif
     C*
     C                   endsr
     C*** End   Add *** 11/07/11 *******************************************
     C*====================================================================
     C* Get full month dates
     C*====================================================================
     C     FullMonth     begsr
     C*
     C                   eval      xxFromDate = GetFirstDay(inDate)
     C                   eval      xxToDate   = GetLastDay(inDate)
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Get half month dates
     C*====================================================================
     C     HalfMonth     begsr
     C*
     C                   eval      day = %subdt(xxFromDate:*d)
     C                   select
     C                   when      day <= 15
     C                   eval      xxFromDate = GetFirstDay(inDate)
     C     xxFromDate    adddur    14:*d         xxToDate
     C                   when      day > 15
     C                   eval      xxFromDate = GetFirstDay(inDate)
     C                   adddur    15:*d         xxFromDate
     C                   eval      xxToDate = GetLastDay(inDate)
     C                   endsl
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Update WMFNL
     C*====================================================================
     C     UpdateWMFNL   begsr
     C*
     C                   eval      FNDT   = DateTo7(SystemDate)
     C                   eval      FNTIME = TimeTo6(SystemTime)
     C                   eval      FNUSER = q1user
     C                   eval      FNPGM  = q1pgm
     C*
     C                   update    pfn00
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Get weekly dates
     C*====================================================================
     C     WeekDates     begsr
     C*
     C* 1=Monday...7=Sunday
     C                   select
     C                   when      BSDOWK = 'MO'
     C                   eval      billDay = 1
     C                   when      BSDOWK = 'TU'
     C                   eval      billDay = 2
     C                   when      BSDOWK = 'WE'
     C                   eval      billDay = 3
     C                   when      BSDOWK = 'TH'
     C                   eval      billDay = 4
     C                   when      BSDOWK = 'FR'
     C                   eval      billDay = 5
     C                   endsl
     C*
     C                   eval      day = dow(DateTo8(inDate))
     C*
     C                   if        billDay < day
     C                   eval      billDay = billDay + 7
     C                   endif
     C*
     C                   eval      day = billDay - day - 1
     C*
     C     inDate        adddur    day:*d        xxToDate
     C     xxToDate      subdur    6:*d          xxFromDate
     C*
     C                   endsr
     C*
     PRetrieveInv#     e

