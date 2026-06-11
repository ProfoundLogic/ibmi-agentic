     hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h/title W7761R  : Written vs Collected premium
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
     h*=================================================================================
     h*  DESCRIPTION:
     h*  Produces a report/workfile of written and collected premium numbers. Written premium
     h*  is pulled from the CEU.  Collected premium, fees billed, and fees paid are pulled from A/R.
     h*
     h*  CALLED FROM: W7761C
     h*  CALLS TO:
    h*  CREATED: 06/16/05      C:136     PROJECT: 114WJB
     h*=================================================================================
     h* REVISIONS:
     h*
     h* 08/26/14 C:307 Project: SY354
     h*                -Implement Routing Page Changes.
     h*
    h*  01/28/10  C:249 Project: SY215                                   *
    h*            C:189 Retrieve brand Name                              *
     h*
     h* 02/20/09 C:136 Support: WA114 66995
     h*                Fixed year beginning date
     h*
     h* 01/24/06 C:136  Support: WA114 50221
     h*                 Fixed I/O error.  Except to QSYSPRT when it is closed.
     h*=================================================================================
      * CEU File (co/fnd//fyr/emp#/div)
     fwfeul1    if   e           k disk
      * Employer Master (co/fnd/emp#/div)
     fwmeml     if   e           k disk
      * Employer Detail (co/fnd/emp#/div/fyr)
     fwdell     if   e           k disk
      * A/R File (co/fnd/fyr/emp/div/arty)
     fwmeal     if   e           k disk
      * Routing file
     fsmorl     if   e           k disk
      * Written Prem vs Collected Prem
     fwfwcp     o    e             disk
      * Print file
     fqsysprt   o    f  198        printer oflind(*inof) usropn
     *---------------------------------------------------------------------
     * Stand Alone Fields
     *---------------------------------------------------------------------
     d xxco#           s              3  0
     d xxfgrp          s              3  0
     d xxtgrp          s              3  0
     d xxGrpRng        s             10    inz(*blanks)
     d xxque           s             10
     d xxfil#          s                   like(ORFIL#)
     d xxtype          s             15
     d xxCat           s              1
     d x8asod          s              8  0
     d x7asod          s              7  0
     d charJan1        s              7
     d x7Jan1          s              7  0
     d x7date          s              7  0
     d svfyr           s                   like(EUFYR)
     d svfnd           s                   like(EUFND)
     d svemp#          s                   like(EUEMP#)
     d svdiv           s                   like(EUDIV)
     d companyNm       s             30
     d groupName       s             25
     d upusnm          s             50
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d xxtitle         s             40
     d xxPrtVb         s              3
     d xxFilVb         s              3
     d line1           s            198
     d line2           s            130
     d polCurrPrmWr    s             11  2
     d polCurrFBill    s             11  2
     d polCurrFPaid    s             11  2
     d polCurrPrmPd    s             11  2
     d grpPrYrPrmWr    s             15  2
     d grpCurrPrmWr    s             15  2
     d grpPrYrFBill    s             15  2
     d grpCurrFBill    s             15  2
     d grpPrYrFPaid    s             15  2
     d grpCurrFPaid    s             15  2
     d grpPrYrPrmPd    s             15  2
     d grpCurrPrmPd    s             15  2
     d grpPrYrBalan    s             15  2
     d grpCurrBalan    s             15  2
     d gTotPremWrtn    s             15  2
     d gTotPremPaid    s             15  2
     d gTotFeesBill    s             15  2
     d gTotFeesPaid    s             15  2
     d gTotBalance     s             15  2
     d totPrYrPrmWr    s             18  2
     d totCurrPrmWr    s             18  2
     d totPrYrFBill    s             18  2
     d totCurrFBill    s             18  2
     d totPrYrFPaid    s             18  2
     d totCurrFPaid    s             18  2
     d totPrYrPrmPd    s             18  2
     d totCurrPrmPd    s             18  2
     d totPrYrBalan    s             18  2
     d totCurrBalan    s             18  2
     d tTotPremWrtn    s             18  2
     d tTotPremPaid    s             18  2
     d tTotFeesBill    s             18  2
     d tTotFeesPaid    s             18  2
     d tTotBalance     s             18  2
     *---------------------------------------------------------------------
     * Arrays and Data Structure
     *---------------------------------------------------------------------
     d feeCat          s              1    dim(5) perrcd(5) ctdata
     d psds          esds                  extname(szq1p)
     *---------------------------------------------------------------------
     * Constants
     *---------------------------------------------------------------------
     d title           c                   'Written Premium vs Collected -
     d                                     Premium'
     *---------------------------------------------------------------------
     * Procedure Interfaces
     *---------------------------------------------------------------------
     d*entry
     d W7761R          pr
     d  @@co#                         3
     d  @@fgrp                        3
     d  @@tgrp                        3
     d  @@asod                        8
     d  @@output                      1
     d W7761R          pi
     d  @@co#                         3
     d  @@fgrp                        3
     d  @@tgrp                        3
     d  @@asod                        8
     d  @@output                      1
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************

     d/copy sPrString
     d/copy sPrDate
     d/copy sPrCompany
     d/copy wPrGroup
     d/copy wPrUsrPrms
     d/copy wPrAR
      *=========================================================================
      * klist
      *=========================================================================
     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#

     c     emky          klist
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv

     c     elky          klist
     c                   kfld                    xxco#
     c                   kfld                    svfnd
     c                   kfld                    svemp#
     c                   kfld                    svdiv
     c                   kfld                    svfyr
      *=========================================================================
      * Main
      *=========================================================================
     c                   read      peu01
     c                   eval      svfnd  = eufnd
     c                   eval      svfyr  = eufyr
     c                   eval      svemp# = euemp#
     c                   eval      svdiv  = eudiv
     c                   eval      groupName = GetGrpName(svfnd)
     c                   dow       not(%eof)

     c                   if        *inof = *on and
     c                             @@output <> 'F'
     c                   eval      *inof = *off
     c                   except    polHeader
     c                   endif

     c                   if        svfnd  <> eufnd  or
     c                             svemp# <> euemp# or
     c                             svdiv  <> eudiv  or
     c                             svfyr  <> eufyr
     c                   exsr      srPolDetail
     c                   eval      svfyr  = eufyr
     c                   eval      svemp# = euemp#
     c                   eval      svdiv  = eudiv
     c                   endif

     c                   if        svfnd <> eufnd and
     c                             @@output <> 'F'
     c                   exsr      srGrpTotal
     c                   endif

      *** beg chg ***** 02/20/09 ************************************************
     c*****************  add       EUYDWR        polCurrPrmWr
     c                   eval      polCurrPrmWr =  EUYDWR + EUPYWR
      *** end chg ***** 02/20/09 ************************************************
     c                   add       EUPYWR        grpPrYrPrmWr
     c                   add       EUYDWR        grpCurrPrmWr
     c                   add       EUPYWR        totPrYrPrmWr
     c                   add       EUYDWR        totCurrPrmWr

     c                   read      peu01
     c                   enddo

     c                   if        *inof = *on and
     c                             @@output <> 'F'
     c                   eval      *inof = *off
     c                   except    polHeader
     c                   endif

     c                   exsr      srPolDetail
      *** beg add ***** 01/24/06 *******************************************************
     c                   if        @@output <> 'F'
     c                   exsr      srGrpTotal
     c                   exsr      srTotal
     c                   endif
      *** end add ***** 01/24/06 *******************************************************

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       if %open(qsysprt);
       xxfil# = '0';
       Chain orky POR01;
       If %found;
          Except Route;
       Endif;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   seton                                        lr
      *=========================================================================
      *    *inzsr
      *=========================================================================
     c     *inzsr        begsr

     c                   move      @@asod        x8asod
     c                   eval      x7asod = dtCYMD(x8asod)
     c                   move      x7asod        charJan1
      *** beg chg ***** 02/20/09 *******************************************************
     c*****************  move      '01'          charJan1
     c                   move      '0101'        charJan1
      *** end chg ***** 02/20/09 *******************************************************
     c                   move      charJan1      x7Jan1

     c                   if        @@output <> 'F'
     c                   open      qsysprt
     c                   move      *all'_'       line1
     c                   move      *all'_'       line2
     c                   eval      *inof = *on
     c                   exsr      routing
     c                   endif

     c                   endsr
      *=========================================================================
      * Routing
      *=========================================================================
     c     routing       begsr

     c                   eval      *in08 = *off
     c                   eval      *in09 = *on
     c                   eval      sys_Date  = SystemDate
     c                   eval      sys_Date  = SystemDate
     c                   eval      sys_Time  = SystemTime
     c                   eval      xxco#     = UserCo#(q1user)
     c                   eval      upusnm    = GetUsrName(q1user)
     c                   eval      companyNm = GetCompNam(xxco#)
     c                   eval      xxque     = GetUsrOutq(q1user)
     c                   move      @@fgrp        xxfgrp
     c                   move      @@tgrp        xxtgrp

     c                   if        xxfgrp = 0 and
     c                             xxtgrp = 0
     c                   movel     '*all'        xxGrpRng
     c                   else
     c                   eval      xxGrpRng = @@fgrp + ' - ' + @@tgrp
     c                   endif

     c                   if        xxtgrp = 0
     c                   eval      xxtgrp = *hival
     c                   endif

     c                   select
     c                   when      @@output = 'P'
     c                   eval      xxPrtVb = 'Yes'
     c                   eval      xxFilVb = 'No'
     c                   when      @@output = 'F'
     c                   eval      xxPrtVb = 'No'
     c                   eval      xxFilVb = 'Yes'
     c                   when      @@output = 'B'
     c                   eval      xxPrtVb = 'Yes'
     c                   eval      xxFilVb = 'Yes'
     c                   endsl
     *
     * Get the Distribution Information
     *
     c                   clear                   xxtitle
     c                   eval      xxfil#   = '0'
     c     orky          chain     por01                              60

     c                   if        ORTITL = *blank
     c                   eval      xxtitle = title
     c                   else
     c                   eval      xxtitle = ORTITL
     c                   endif
      *
     * Print the Distribution Information
     *
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** End Add ***** 01/28/10 **************************************
     c                   if        not *in60
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   endif

     c                   eval      xxtitle = centerFld(xxtitle)

     c                   endsr
      *=========================================================================
      *  srGetAR
      *=========================================================================
     c     srGetAR       begsr

     c     elky          setll     pea01
     c     elky          reade     pea01
     c                   dow       not(%eof)
      *** beg add ***** 02/20/09 *******************************************************
     c                   eval      xxCat = getARCat(EAARCD)
      *** end add ***** 02/20/09 *******************************************************

      *** beg chg ***** 02/20/09 *******************************************************
     c*****************  if        EAARTY = 'F'
     c                   if        EAARTY = 'F' and EATRDT <= x7asod
      *** end chg ***** 02/20/09 *******************************************************
      *** beg del ***** 02/20/09 *******************************************************
     c***********        eval      xxCat = getARCat(EAARCD)
      *** end del ***** 02/20/09 *******************************************************
     c     xxCat         lookup    feeCat                                 60
     c                   if        %equal
     c                   eval      x7date = getARDate(xxco#:svfnd:svfyr:svemp#:
     c                                               svdiv:EAARTY:EAPODT:EATRDT)
      *** beg add ***** 02/20/09 *******************************************************
     c                   add       EAAMT         polCurrFBill
      *** end add ***** 02/20/09 *******************************************************
     c                   if        x7date >= X7Jan1
      *** beg del ***** 02/20/09 *******************************************************
     c***********        add       EAAMT         polCurrFBill
      *** end del ***** 02/20/09 *******************************************************
     c                   add       EAAMT         grpCurrFBill
     c                   add       EAAMT         totCurrFBill
     c                   else
     c                   add       EAAMT         grpPrYrFBill
     c                   add       EAAMT         totPrYrFBill
     c                   endif
     c                   endif
     c                   endif

      *** beg chg ***** 02/20/09 *******************************************************
     c*****************  if        EAARTY = 'Q'
     c                   if        EAARTY = 'Q' and EATRDT <= x7asod
      *** end chg ***** 02/20/09 *******************************************************
      *** beg del ***** 02/20/09 *******************************************************
     c***********        eval      xxCat = getARCat(EAARCD)
      *** end del ***** 02/20/09 *******************************************************
     c     xxCat         lookup    feeCat                                 60
     c                   if        %equal
     c                   eval      x7date = getARDate(xxco#:svfnd:svfyr:svemp#:
     c                                               svdiv:EAARTY:EAPODT:EATRDT)
      *** beg add ***** 02/20/09 *******************************************************
     c                   add       EAAMT         polCurrFPaid
      *** end add ***** 02/20/09 *******************************************************
     c                   if        x7date >= X7Jan1
      *** beg del ***** 02/20/09 *******************************************************
     c***********        add       EAAMT         polCurrFPaid
      *** end del ***** 02/20/09 *******************************************************
     c                   add       EAAMT         grpCurrFPaid
     c                   add       EAAMT         totCurrFPaid
     c                   else
     c                   add       EAAMT         grpPrYrFPaid
     c                   add       EAAMT         totPrYrFPaid
     c                   endif
     c                   endif
     c                   endif

      *** beg chg ***** 02/20/09 *******************************************************
     c*****************  if        EAARTY = 'P' or
     c*****************            EAARTY = 'T'
     c                   if        (EAARTY = 'P' or
     c                             (EAARTY = 'T' and xxCat = 'P')) and
     c                             EATRDT <= x7asod
      *** end chg ***** 02/20/09 *******************************************************
     c                   eval      x7date = getARDate(xxco#:svfnd:svfyr:svemp#:
     c                                               svdiv:EAARTY:EAPODT:EATRDT)
      *** beg add ***** 02/20/09 *******************************************************
     c                   add       EAAMT         polCurrPrmPd
      *** end add ***** 02/20/09 *******************************************************
     c                   if        x7date >= X7Jan1
      *** beg del ***** 02/20/09 *******************************************************
     c***********        add       EAAMT         polCurrPrmPd
      *** end del ***** 02/20/09 *******************************************************
     c                   add       EAAMT         grpCurrPrmPd
     c                   add       EAAMT         totCurrPrmPd
     c                   else
     c                   add       EAAMT         grpPrYrPrmPd
     c                   add       EAAMT         totPrYrPrmPd
     c                   endif
     c                   endif

      *** beg chg ***** 02/20/09 *******************************************************
     c*****************  if        EAARTY = 'R' or
     c*****************            EAARTY = 'N' or
     c*****************            EAARTY = 'V'
     c                   if        ((EAARTY = 'R' and xxCat = 'P') or
     c                              EAARTY = 'N' or
     c                             (EAARTY = 'V' and xxCat = 'P')) and
     c                             EATRDT <= x7asod
      *** end chg ***** 02/20/09 *******************************************************
     c                   eval      x7date = getARDate(xxco#:svfnd:svfyr:svemp#:
     c                                               svdiv:EAARTY:EAPODT:EATRDT)
      *** beg add ***** 02/20/09 *******************************************************
     c                   sub       EAAMT         polCurrPrmPd
      *** end add ***** 02/20/09 *******************************************************
     c                   if        x7date >= X7Jan1
      *** beg del ***** 02/20/09 *******************************************************
     c***********        sub       EAAMT         polCurrPrmPd
      *** end del ***** 02/20/09 *******************************************************
     c                   sub       EAAMT         grpCurrPrmPd
     c                   sub       EAAMT         totCurrPrmPd
     c                   else
     c                   sub       EAAMT         grpPrYrPrmPd
     c                   sub       EAAMT         totPrYrPrmPd
     c                   endif
     c                   endif

     c     elky          reade     pea01
     c                   enddo

     c                   endsr
      *=========================================================================
      *  srPolDetail
      *=========================================================================
     c     srPolDetail   begsr

     c                   if        *inof = *on and
     c                             @@output <> 'F'
     c                   eval      *inof = *off
     c                   except    polHeader
     c                   endif

     c                   clear                   pwc01
     c     emky          chain     pem01
     c                   if        %found(WMEML)
     c     elky          chain     pel01
     c                   endif

     c                   if        %found(WMEML) and
     c                             %found(WDELL)
     c                   eval      WCCO#  = xxco#
     c                   eval      WCCONM = companyNm
     c                   eval      WCFND  = svfnd
     c                   eval      WCFNNM = groupName
     c                   eval      WCEMP# = svemp#
     c                   eval      WCDIV  = svdiv
     c                   eval      WCENAM = EMNAME
     c                   eval      WCINDT = dtMDYY(EMEFD1)
     c                   eval      WCCXDT = dtMDYY(EMCAD1)
     c                   eval      WCFYR  = svfyr
     c                   eval      WCPFDT = dtMDYY(ELPFDT)
     c                   eval      WCPTDT = dtMDYY(ELPTDT)
     c                   exsr      srGetAR
     c                   eval      WCWRPR = polCurrPrmWr
     c                   eval      WCCLPR = polCurrPrmPd
     c                   eval      WCSURB = polCurrFBill
     c                   eval      WCSURP = polCurrFPaid
     c                   eval      WCBAL  = (polCurrPrmWr + polCurrFBill) -
     c                                      (polCurrPrmPd + polCurrFPaid)
     c                   eval      WCPGM  = Q1PGM
     c                   eval      WCUSER = Q1USER
     c                   eval      WCDT   = dateTo7(SystemDate)
     c                   eval      WCTIME = timeTo6(SystemTime)

     c                   if        @@output <> 'P'
     c                   write     pwc01
     c                   endif

     c                   if        @@output <> 'F'
     c                   if        WCWRPR <> 0 or
     c                             WCCLPR <> 0 or
     c                             WCSURB <> 0 or
     c                             WCSURP <> 0
     c                   except    polDetail
     c                   endif
     c                   endif

     c                   endif

     c                   clear                   polCurrPrmWr
     c                   clear                   polCurrPrmPd
     c                   clear                   polCurrFBill
     c                   clear                   polCurrFPaid

     c                   endsr
      *=========================================================================
      *  2rGrpTotal
      *=========================================================================
     c     srGrpTotal    begsr

     c                   if        *inof = *off
     c                   eval      *inof = *on
     c                   endif

     c                   eval      grpPrYrBalan =
     c                             (grpPrYrPrmWr + grpPrYrFBill) -
     c                             (grpPrYrPrmPd + grpPrYrFPaid)

     c                   eval      grpCurrBalan =
     c                             (grpCurrPrmWr + grpCurrFBill) -
     c                             (grpCurrPrmPd + grpCurrFPaid)

     c                   eval      gTotPremWrtn =
     c                             (grpPrYrPrmWr + grpCurrPrmWr)

     c                   eval      gTotPremPaid =
     c                             (grpPrYrPrmPd + grpCurrPrmPd)

     c                   eval      gTotFeesBill =
     c                             (grpPrYrFBill + grpCurrFBill)

     c                   eval      gTotFeesPaid =
     c                             (grpPrYrFPaid + grpCurrFPaid)

     c                   eval      gTotBalance  =
     c                             (gTotPremWrtn + gTotFeesBill) -
     c                             (gTotPremPaid + gTotFeesPaid)

     c                   if        *inof = *on
     c                   except    grpHeader
     c                   except    grpDetail
     c                   endif

     c                   clear                   grpPrYrPrmWr
     c                   clear                   grpCurrPrmWr
     c                   clear                   grpPrYrPrmPd
     c                   clear                   grpCurrPrmPd
     c                   clear                   grpPrYrFBill
     c                   clear                   grpCurrFBill
     c                   clear                   grpPrYrFPaid
     c                   clear                   grpCurrFPaid
     c                   clear                   grpPrYrBalan
     c                   clear                   grpCurrBalan
     c                   clear                   gTotPremWrtn
     c                   clear                   gTotPremPaid
     c                   clear                   gTotFeesBill
     c                   clear                   gTotFeesPaid
     c                   clear                   gTotBalance

     c                   eval      svfnd = eufnd
     c                   eval      groupName = GetGrpName(svfnd)
     c                   eval      *inof = *on

     c                   endsr
      *=========================================================================
      *  srTotal
      *=========================================================================
     c     srTotal       begsr

     c                   if        *inof = *off
     c                   eval      *inof = *on
     c                   endif

     c                   eval      totPrYrBalan =
     c                             (totPrYrPrmWr + totPrYrFBill) -
     c                             (totPrYrPrmPd + totPrYrFPaid)

     c                   eval      totCurrBalan =
     c                             (totCurrPrmWr + totCurrFBill) -
     c                             (totCurrPrmPd + totCurrFPaid)

     c                   eval      tTotPremWrtn =
     c                             (totPrYrPrmWr + totCurrPrmWr)

     c                   eval      tTotPremPaid =
     c                             (totPrYrPrmPd + totCurrPrmPd)

     c                   eval      tTotFeesBill =
     c                             (totPrYrFBill + totCurrFBill)

     c                   eval      tTotFeesPaid =
     c                             (totPrYrFPaid + totCurrFPaid)

     c                   eval      tTotBalance  =
     c                             (tTotPremWrtn + tTotFeesBill) -
     c                             (tTotPremPaid + tTotFeesPaid)

     c                   if        *inof = *on
     c                   except    totHeader
     c                   except    totDetail
     c                   endif

     c                   endsr
      *=========================================================================
     o/copy scopyrout
     oqsysprt   e            route          2
     o                                           20 'Company  . . :'
     o                       xxco#               25
     o          e            route          2
     o                                           20 'Group Range  :'
     o                       xxGrpRng          +002
     o          e            route          2
     o                                           20 'As of date   :'
     o                       x8asod            +002 '  /  /    '
     o          e            route          2
     o                                           20 'Print  . . . :'
     o                       xxPrtVb           +002
     o          e            route          2
     o                                           20 'File  (WFWCP):'
     o                       xxFilVb           +002
     o*** Beg Chg *****  01/28/10  ************************************
     o***       e            route          1 60
     o***                                           '(C) Copyright Progressive'
     o***                                     +   1 'Data Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     o*** END CHG *****  01/28/10  ************************************
     o          e            polHeader      1 06
     o                       sys_time
     o                       companyNm          109
     o                       sys_date           198
     o          e            polHeader      2
     o                       q1pgm
     o                       xxtitle            114
     o                                          192 'PAGE:'
     o                       page1         z    198
     o          e            polHeader      2
     o                                              'Group/Name:'
     o                       svfnd             +002
     o                       groupName         +002
     O          E            polHeader      1
     O                                            8 'Policy'
     O                                           26 'Employer/Div'
     O                                           46 'Employer Name'
     O                                           93 'Policy Period'
     O                                          109 'Cx Date'
     O                                          127 'Written'
     O                                          143 'Collected'
     O                                          161 'Surch'
     O                                          179 'Surch'
     O                                          197 'Balance'
     o          e            polHeader      0
     o                       line1
     O          e            polHeader      2
     O                                            8 'Year'
     O                                          127 'Premium'
     O                                          143 'Premium'
     O                                          161 'Billed'
     o                                          179 'Paid'
     o          e            polDetail      1
     o                       WCFYR         4      8
     o                       WCEMP#        4     20
     o                                           21 '/'
     o                       WCDIV               26
     o                       WCENAM              73
     o                       WCPFDT              85 '  /  /    '
     o                                           88 ' - '
     o                       WCPTDT              98 '  /  /    '
     o                       WCCXDT             110 '  /  /    '
      *
     o                       polCurrPrmWr  j    128 '$'
     o                       polCurrPrmPd  j    144 '$'
     o                       polCurrFBill  j    162 '$'
     o                       polCurrFPaid  j    180 '$'
     o                       WCBAL         j    198 '$'
     o          e            grpHeader      1 06
     o                       sys_time
     o                       companyNm          109
     o                       sys_date           198
     o          e            grpHeader      4
     o                       q1pgm
     o                       xxtitle            114
     o                                          192 'PAGE:'
     o                       page1         z    198
     o          e            grpHeader      2
     o                                           57 'Group/Name . . . . . :'
     o                       svfnd             +002
     o                       groupName         +002
     O          E            grpHeader      1
     O                                           84 'Written'
     O                                          112 'Collected'
     O                                          140 'Surch'
     O                                          168 'Surch'
     O                                          197 'Balance'
     o          e            grpHeader      0
     o                       line2              198
     O          e            grpHeader      2
     O                                           84 'Premium'
     O                                          112 'Premium'
     O                                          140 'Billed'
     o                                          168 'Paid'
     o          e            grpDetail      1
     o                                           57 'Prior year balances  :'
     o                       grpPrYrPrmWr  j     85 '$'
     o                       grpPrYrPrmPd  j    113 '$'
     o                       grpPrYrFBill  j    141 '$'
     o                       grpPrYrFPaid  j    169 '$'
     o                       grpPrYrBalan  j    198 '$'
     o          e            grpDetail      0
     o                       line2              198
     o          e            grpDetail      1
     o                                           57 'Current year balances:'
     o                       grpCurrPrmWr  j     85 '$'
     o                       grpCurrPrmPd  j    113 '$'
     o                       grpCurrFBill  j    141 '$'
     o                       grpCurrFPaid  j    169 '$'
     o                       grpCurrBalan  j    198 '$'
     o          e            grpDetail      1
     o                                           57 'Totals . . . . . . . :'
     o                       gTotPremWrtn  j     85 '$'
     o                       gTotPremPaid  j    113 '$'
     o                       gTotFeesBill  j    141 '$'
     o                       gTotFeesPaid  j    169 '$'
     o                       gTotBalance   j    198 '$'
     o          e            totHeader      1 06
     o                       sys_time
     o                       companyNm          109
     o                       sys_date           198
     o          e            totHeader      2
     o                       q1pgm
     o                       xxtitle            114
     o                                          192 'PAGE:'
     o                       page1         z    198
     o          e            totHeader      4
     o                                              'Group range  . . . . :'
     o                       xxGrpRng          +002
     O          E            totHeader      1
     O                                           84 'Written'
     O                                          112 'Collected'
     O                                          140 'Surch'
     O                                          168 'Surch'
     O                                          197 'Balance'
     o          e            totHeader      0
     o                       line2              198
     O          e            totHeader      2
     O                                           84 'Premium'
     O                                          112 'Premium'
     O                                          140 'Billed'
     o                                          168 'Paid'
     o          e            totDetail      1
     o                                           57 'Prior year balances  :'
     o                       totPrYrPrmWr  j     85 '$'
     o                       totPrYrPrmPd  j    113 '$'
     o                       totPrYrFBill  j    141 '$'
     o                       totPrYrFPaid  j    169 '$'
     o                       totPrYrBalan  j    198 '$'
     o          e            totDetail      0
     o                       line2              198
     o          e            totDetail      1
     o                                           57 'Current year balances:'
     o                       totCurrPrmWr  j     85 '$'
     o                       totCurrPrmPd  j    113 '$'
     o                       totCurrFBill  j    141 '$'
     o                       totCurrFPaid  j    169 '$'
     o                       totCurrBalan  j    198 '$'
     o          e            totDetail      1
     o                                           57 'Totals . . . . . . . :'
     o                       tTotPremWrtn  j     85 '$'
     o                       tTotPremPaid  j    113 '$'
     o                       tTotFeesBill  j    141 '$'
     o                       tTotFeesPaid  j    169 '$'
     o                       tTotBalance   j    198 '$'
**ctdata feeCat
12345
