     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h*********************************************************************
    h/TITLE - Internal Account Rep Commission Report
     H option(*nodebugio:*srcstmt)
    h*********************************************************************
    h*                                                                   *
    h*  Reads through WXRCP and calculates commission for Internal       *
    h*  Account Reps from the sliding scale defined in WMICP.            *
    h*                                                                   *
    h*  CALLED FROM:                                                     *
    h*    W3002C                                                         *
    h*                                                                   *
    h*  CALLS TO:                                                        *
    h*                                                                   *
    h*  CREATED: 01/15/03      C:136     PROJECT: 114WEY                 *
    h*********************************************************************
    h*  REVISIONS:                                                       *
    h*                                                                   *
     h* 08/26/14 C:307 Project: SY354                                     *
     h*                -Implement Routing Page Changes.                   *
     h*                                                                   *
    h* 01/16/15 C:136 PhaseWare: 32635                                   *
    h*                CSI requested chg for determing new/renew          *
    h*                status. If cross reference exists, it is           *
    h*                renewal.                                           *
    h*                                                                   *
    h*  01/28/10  C:249 Project: SY215                                   *
    h*            C:189 Retrieve brand Name                              *
    h*                                                                   *
    h* 10/28/09 C:191 Support: 69113 WA114                               *
    h*                Correct new/renew logic to use API                 *
    h*                                                                   *
    h*  03/17/04  C:132 Project: 114WGQ                                  *
    h*               - Add New workfile for break on Business type, and  *
    h*                 load when write detail records. Change report to  *
    h*                 User controlled, so can turn off printing.        *
    h*               - Remove all the 03/05/04 Changes which will be in  *
    h*                 new program W3004R.                               *
    h*                                                                   *
    h*  03/05/04  C:132 Project: 114WGQ                                  *
    h*               - Add subtotals for New business, Renewal business  *
    h*                 and Expring business for each Sales Executive.    *
    h*               - Read new logical for WXRCP and break on Businesss *
    h*                 Type sort, then print totals for the Type.        *
    h*                                                                   *
    h*  08/05/03 C:136 Support: 114WFR                                  *
    h*                 Negative sign not printing on "new" negative      *
    h*                 commission amounts.                               *
    h*                                                                   *
    h*  06/20/03 C:136 Project: 114WFR                                   *
    h*                 Add column to dislay commission rates.            *
    h*                                                                   *
    h*********************************************************************
    f*
     fWXRCP     if   e           k disk
     fWXR2P     uf a e           k disk
     fWMICL     if   e           k disk
      *** beg add ***** 01/16/15 ***************************************
     fWZEXL     if   e           k disk    rename(pex01:pex0l)
      *** end add ***** 01/16/15 ***************************************
     f*** BEGIN DEL ****** 10/28/09 ************************************
     f************WDELL     if   e           k disk
     f************WZEXL     if   e           k disk
     f*** END   DEL ****** 10/28/09 ************************************
     fSMORL     if   e           k disk
      *
      *
     fQSYSPRT   o    f  198        printer oflind(*inof) USROPN
    f*
     d/copy sprdate
     d/copy wprusrprms
     d/copy sprstring
     D/copy sprcompany
    d*
     d psds          esds                  extname(szq1p)
    d*---------------------------------------------------------------------
    d* Stand Alone Fields - TOP
    d*---------------------------------------------------------------------
     d xxfil#          s                   like(ORFIL#)
     d xxque           s                   like(@@que)
     d title           c                   'Internal Commissions'
     d title_currMth   c                   'Current Month'
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d usa_rpt_dt      s               d   datfmt(*usa)
     d userName        s             50
     d routeCo#        s              3  0
     d UPUSNM          s                   like(userName)
     d @@co#           s              3
     d @@from_date     s              8
     d @@to_date       s              8
     d @@mth_end_date  s              8
     d @@que           s             10
     d companyNm       s             60
     d kkco#           s              3  0
     d xxfrom_dt       s                   like(rcpodt)
     d xxto_dt         s                   like(rcpodt)
     d xxmth_end_dt    s                   like(rcpodt)
     d prem_arep_tot   s             15  2 inz(*zero)
     d prem_grp_tot    s             13  2 inz(*zero)
     d prem_emp_tot    s             11  2 inz(*zero)
     d prem_yr_tot     s             11  2 inz(*zero)
     d new_total       s             11  2 inz(*zero)
     d firstRead       s               n   inz(*off)
     d new_flg         s               n   inz(*off)
     d renew_flg       s               n   inz(*off)
     d arepBrkFlag     s               n   inz(*off)
     d grpBrkFlag      s               n   inz(*off)
     d empBrkFlag      s               n   inz(*off)
     d newCommRate     s               n   inz(*off)
     d noRepRecFlag    s               n   inz(*on)
     d noGrpRecFlag    s               n   inz(*on)
     d noEmpRecFlag    s               n   inz(*on)
     d noYrRecFlag     s               n   inz(*on)
     d noRecFlag       s               n   inz(*on)
     d OpenPRTF        s               n   inz(*off)
     d x               s              2  0
     d y               s              1  0
     d renew_rate      s                   like(ICRATE)
     d kkefdt          s                   like(ICEFDT)
     d comm_amt        s              9  2 inz(*zero)
     d comm_accum      s              9  2 inz(*zero)
     d comm_dtl_amt    s              9  2 inz(*zero)
     d comm_yr_tot     s              9  2 inz(*zero)
     d comm_emp_tot    s              9  2 inz(*zero)
     d comm_grp_tot    s              9  2 inz(*zero)
     d comm_arep_tot   s              9  2 inz(*zero)
      * Expand for receiver Error in testing
     d amt_left        s             13  2 inz(*zero)
     d wrk_total       s             11  2 inz(*zero)
     d lvl_diff        s             13  2 inz(*zero)
      *
     d saveYr          s                   like(RCFYR)
     d saveDiv         s                   like(RCDIV)
     d saveGrp         s                   like(RCGRP)
     d saveEmp#        s                   like(RCEMP#)
     d saveArep        s                   like(RCAREP)
     d line            s            198
     d arep_name       s             25
     d pol_number      s             18
     d emp_name        s             17
     d rc_post_dt      s              8  0
     d pol_from_dt     s              8  0
     d pol_to_dt       s              8  0
     d pol_canx_dt     s              8  0
     d new_renew       s              5
     d accumPrem_lvl   s             13  2
     d rpt_mth         s              2  0
     d rpt_year        s              4
     d reportMthYr     s             20
     d centerMthYr     s             20
     d rptFromDt       s               d   datfmt(*usa)
     d rptToDt         s               d   datfmt(*usa)
    d*====================================================================
    d*  Data Structures & Arrays
    d*====================================================================
     d months          s              8    dim(12) ctdata
      *
     dreport_date      ds
     d  xxmth_rpt_dt           1      7  0
     d  xxmth_rpt_yr           1      3  0
     d  xxmth_rpt_mth          4      5  0
     d  xxmth_rpt_day          6      7  0
      *
     dcomm_scale       ds                  occurs(10)
     d  prem_lvl                           like(ICPLVL)
     d  prem_rate                          like(ICRATE)
      *
     dcommDet          ds                  occurs(10)
     d  commAmt                            like(comm_dtl_amt)
     d  comRate                       5  2
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    c*====================================================================
    c*  PLIST definitions
    c*====================================================================
     c     *entry        plist
     c                   parm                    @@co#             3
     c                   parm                    @@from_date       8
     c                   parm                    @@to_date         8
     c                   parm                    @@mth_end_date    8
     c                   parm                    @@que            10
    c*====================================================================
    c*  KLIST definitions
    c*====================================================================
     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#
      *
     c*** BEGIN DEL ****** 10/28/09 ************************************
     c* Not needed anymore
     c*****elky          klist
     c***********        kfld                    kkco#
     c***********        kfld                    RCGRP
     c***********        kfld                    RCEMP#
     c***********        kfld                    RCDIV
      ***********
     c*****elky1         klist
     c***********        kfld                    kkco#
     c***********        kfld                    RCGRP
     c***********        kfld                    RCEMP#
     c***********        kfld                    RCDIV
     c***********        kfld                    RCFYR
     c*** END   DEL ****** 10/28/09 ************************************
      *
     c     icky          klist
     c                   kfld                    kkco#
     c                   kfld                    RCGRP
      *
     c     icky1         klist
     c                   kfld                    kkco#
     c                   kfld                    RCGRP
     c                   kfld                    kkefdt
      *
    c*================================================================
    c*  Main
    c*================================================================
     c                   exsr      initialize
      *
     c                   read      prc01                                  60
     c                   dow       not *in60
      *
     c*
     c                   if        saveArep <> RCAREP
     c                   eval      arepBrkFlag = *on
     c                   exsr      arepBrk
     c                   eval      noRepRecflag = *on
     c                   eval      arepBrkFlag = *off
     c                   endif
      *
     c                   if        saveGrp <> RCGRP
     c                   eval      grpBrkFlag = *on
     c                   exsr      grpBrk
     c                   eval      noGrpRecflag = *on
     c                   eval      grpBrkFlag = *off
     c                   endif
      *
     C                   if        saveEmp# <> RCEMP# or saveDiv <> RCDIV
     c                   eval      empBrkFlag = *on
     c                   exsr      empBrk
     c                   eval      noEmpRecflag = *on
     c                   eval      empBrkFlag = *off
     c                   endif
    c*
     c                   if        saveYr <> RCFYR
     c                   exsr      yrBrk
     c                   eval      noYrRecflag = *on
     c                   endif
     c*
     c                   exsr      processRec
     c                   eval      firstRead = *on
     c                   read      prc01                                  60
     c                   enddo
      *
     c                   eval      arepBrkFlag = *on
     c                   exsr      arepBrk
     *
     c                   IF        OpenPRTF = *on
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfil# = '0';
       Chain orky POR01;
       If %found and OpenPRTF = *on;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   close     qsysprt
     c                   endIF
      *
     c                   eval      *inlr = *on
    c*====================================================================
    c* arepBrk - subroutine for internal account representative
    c*====================================================================
     c     arepBrk       begsr
      *
     c                   if        firstRead = *on
      *
     c                   if        *inof = *on and openPRTF = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
      *
     c                   if        noRepRecFlag = *on and OpenPRTF = *ON
     c                   except    noRecord
      *
      *
     c                   endif
      *
     c                   if        noRepRecFlag = *off
     c                   exsr      yrBrk
     c                   exsr      empBrk
     c                   exsr      grpBrk
     c                   endif
      *
     c                   IF        OpenPRTF = *on
     c                   except    arepTot
     c                   eval      *inof = *on
     c                   endif
      ***** END ADD ***** 03/17/04 **********************************
     c                   endif
      *
     c                   eval      prem_arep_tot = 0
     c                   eval      comm_arep_tot = 0
     c                   eval      saveArep = RCAREP
     c                   eval      arep_name = RCAREP
      *
     c                   endsr
    c*====================================================================
    c* empBrk - subroutine for breaking employer/div
    c*====================================================================
     c     empBrk        begsr
      *
     c                   if        firstRead = *on
      *
     c                   if        *inof = *on AND OpenPRTF = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
      *
     c                   if        arepBrkFlag <> *on
     c                   if        grpBrkFlag <> *on
     c                   exsr      yrBrk
     c                   endif
     c                   endif
      *
     c                   if        noRepRecflag = *off and
     c                             noGrpRecflag = *off and
     c                             noEmpRecflag = *off and OpenPRTF = *on
     c                   except    empTot
     c                   endif
      *
     c                   endif
     c*
     c                   eval      prem_emp_tot = 0
     c                   eval      comm_emp_tot = 0
     c                   eval      saveEmp# = RCEMP#
     c                   eval      saveDiv = RCDIV
      *
     c                   endsr
    c*====================================================================
    c* grpBrk - subroutine for breaking groups
    c*====================================================================
     c     grpBrk        begsr
      *
     c                   if        firstRead = *on
      *
     c                   if        *inof = *on AND OpenPRTF = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
      *
     c                   if        arepBrkFlag <> *on
     c                   exsr      yrBrk
     c                   exsr      empBrk
     c                   endif
      *
     c                   if        noRepRecflag = *off and
     c                             noGrpRecflag = *off AND OpenPRTF = *on
     c                   except    grpTot
     c                   endif
      *
     c                   endif
      *
     c                   eval      prem_grp_tot = 0
     c                   eval      comm_grp_tot = 0
     c                   eval      saveGrp = RCGRP
      *
     c                   endsr
    c*====================================================================
    c* initialize - initialize fields
    c*====================================================================
     c     initialize    begsr
    c*
     c     *hms          move      q1time        sys_time
     c     *usa          move      *date         sys_date
     c                   eval      xxfrom_dt = dtCYMD(numDate(@@from_date))
     c                   eval      xxto_dt   = dtCYMD(numDate(@@to_date))
     c                   eval      rptFromDt = cvtToDate(numDate(@@from_date))
     c                   eval      rptToDt   = cvtToDate(numDate(@@to_date))
     c                   eval      xxmth_end_dt=dtCYMD(numDate(@@mth_end_date))
     c                   eval      xxmth_rpt_dt = xxmth_end_dt
     c                   eval      xxmth_rpt_day = 1
      *
     c                   eval      usa_rpt_dt = cvtToDate(xxmth_rpt_dt)
     c                   extrct    usa_rpt_dt:*M rpt_mth
     c                   extrct    usa_rpt_dt:*Y rpt_year
     c                   eval      reportMthYr = %trimr(months(rpt_mth))
     c                             + ' ' + rpt_year
     c                   eval      centerMthYr = centerFld(reportMthYr)
      *
     c                   move      @@co#         kkco#
     c                   eval      companyNm = GetCompNam(kkco#)
     c                   eval      companyNm = centerFld(companyNm)
     c                   eval      line = *all'_'
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
     *
     * SET the Open Flag for printer file here
     c                   eval      OpenPRTF = *OFF
     *
     c                   IF        OpenPRTF = *on
     c                   open      qsysprt
     c                   exsr      routing
     c                   except    head
     c                   endIF
      *
     c                   endsr
    c*====================================================================
    c* processRec - subroutine for processing record
    c*====================================================================
     c     processRec    begsr
      *
     c                   eval      y = 1
      *
     c                   dow       y <= 5
     c     y             occur     commDet
     c                   clear                   commDet
     c                   add       1             y
     c                   enddo
      *
     c                   eval      y = 1
     c     y             occur     commDet
      *** end add ***** 06/20/03 ******************************************
      *
     c                   add       RCAMT         prem_yr_tot
     c                   add       RCAMT         prem_emp_tot
     c                   add       RCAMT         prem_grp_tot
     c                   eval      comm_amt = 0
     c                   eval      comm_accum = 0
     c                   eval      comm_dtl_amt = 0
     c                   eval      accumPrem_lvl = 0
      *
     c                   if        new_flg = *on and
     c                             RCPODT < xxmth_rpt_dt
     c                   add       RCAMT         new_total
     c                   endif
      *
     c                   if        RCPODT >= xxmth_rpt_dt and
     c                             RCPODT <= xxmth_end_dt
      *
     c                   if        new_flg = *on and
     c                             newCommRate = *on
      *
     c                   if        RCAMT > 0
      *
     c                   eval      x = 1
     c     x             occur     comm_scale
     c                   eval      accumPrem_lvl = prem_lvl
     c                   dow       accumPrem_lvl < new_total
     c                   add       1             x
     c     x             occur     comm_scale
     c                   eval      accumPrem_lvl = prem_lvl + accumPrem_lvl
     c                   enddo
      *
     c                   eval      amt_left = RCAMT
     c                   eval      lvl_diff = accumPrem_lvl - new_total
      *
     c                   dow       amt_left > 0
      *
     c                   if        amt_left > lvl_diff
      *
     c                   if        lvl_diff > 0
     c                   eval(h)   comm_amt = lvl_diff * prem_rate
     c                   eval(h)   commAmt  = comm_amt  * .01
     c                   eval      comRate  = prem_rate
     c                   add       1             y
     c     y             occur     commDet
     c                   else
     c                   eval(h)   comm_amt = prem_lvl * prem_rate
     c                   eval(h)   commAmt  = comm_amt  * .01
     c                   eval      comRate  = prem_rate
     c                   add       1             y
     c     y             occur     commDet
     c                   endif
      *
     c                   else
     c                   eval(h)   comm_amt = amt_left * prem_rate
     c                   eval(h)   commAmt  = comm_amt  * .01
     c                   eval      comRate  = prem_rate
     c                   add       1             y
     c     y             occur     commDet
      *** end add ***** 06/20/03 ***********************************************
     c                   endif
      *
     c                   if        lvl_diff >= 0
     c                   eval      amt_left = amt_left - lvl_diff
     c                   else
     c                   eval      amt_left = amt_left - prem_lvl
     c                   endif
      *
     c                   add       1             x
     c     x             occur     comm_scale
     c                   eval      lvl_diff = prem_lvl - amt_left
      *
     c                   eval      comm_accum = comm_accum + comm_amt
     c                   enddo
      *
     c                   endif
      *
     c                   if        RCAMT < 0
      *
     c                   eval      amt_left = -RCAMT
     c                   eval      wrk_total = new_total - amt_left
      *
     c                   if        wrk_total < 0
     c                   eval      wrk_total = 0
     c                   endif
      *
     c                   eval      x = 1
     c     x             occur     comm_scale
     c                   eval      accumPrem_lvl = prem_lvl
     c                   dow       accumPrem_lvl < wrk_total
     c                   add       1             x
     c     x             occur     comm_scale
     c                   eval      accumPrem_lvl = prem_lvl + accumPrem_lvl
     c                   enddo
      *
     c                   eval      lvl_diff = accumPrem_lvl - wrk_total
      *
     c                   dow       amt_left > 0
      *
     c                   if        amt_left > lvl_diff
      *
     c                   if        lvl_diff > 0
     c                   eval(h)   comm_amt = lvl_diff * prem_rate
     c                   eval(h)   commAmt  = comm_amt  * .01
     c                   mult      -1            commAmt
     c                   eval      comRate  = prem_rate
     c                   add       1             y
     c     y             occur     commDet
     c                   else
     c                   eval(h)   comm_amt = prem_lvl * prem_rate
     c                   eval(h)   commAmt  = comm_amt  * .01
     c                   mult      -1            commAmt
     c                   eval      comRate  = prem_rate
     c                   add       1             y
     c     y             occur     commDet
     c                   endif
      *
     c                   else
     c                   eval(h)   comm_amt = amt_left * prem_rate
     c                   eval(h)   commAmt  = comm_amt  * .01
     c                   mult      -1            commAmt
     c                   eval      comRate  = prem_rate
     c                   add       1             y
     c     y             occur     commDet
     c                   endif
      *
     c                   if        lvl_diff >= 0
     c                   eval      amt_left = amt_left - lvl_diff
     c                   else
     c                   eval      amt_left = amt_left - prem_lvl
     c                   endif
      *
     c                   add       1             x
     c     x             occur     comm_scale
     c                   eval      lvl_diff = prem_lvl - amt_left
      *
     c                   eval      comm_accum = comm_accum + comm_amt
     c                   enddo
      *
     c                   mult      -1            comm_accum
     c                   endif
      *
     c                   eval      new_total = RCAMT + new_total
     c                   eval(h)   comm_dtl_amt = comm_accum * .01
      *
     c                   endif
      *
     c                   if        renew_flg = *on
     c                   eval(h)   comm_dtl_amt = RCAMT * renew_rate *.01
     c                   eval(h)   commAmt  = comm_dtl_amt
     c                   eval      comRate  = renew_rate
     c                   endif
      *
     c                   add       comm_dtl_amt  comm_yr_tot
     c                   add       comm_dtl_amt  comm_emp_tot
     c                   add       comm_dtl_amt  comm_grp_tot
     c                   add       comm_dtl_amt  comm_arep_tot
     c                   eval      rc_post_dt  = dtMDYY(RCPODT)
     c                   eval      pol_from_dt = dtMDYY(RCPFDT)
     c                   eval      pol_to_dt   = dtMDYY(RCPTDT)
     c                   eval      pol_canx_dt = dtMDYY(RCCNDT)
     c                   eval      pol_number = RCCAP#
     c                   eval      emp_name = RCENAM
      *
     c                   if        *inof and OpenPRTF = *ON
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
      *
     c                   if        comm_dtl_amt <> 0
     c                   eval      noRepRecflag = *off
     c                   eval      noRecflag    = *off
     c                   eval      y = 1
     c     y             occur     commDet
     c                   dow       commAmt <> 0
      *
      * LOAD AND OUTPUT SECOND WORKFILE RECORD
     c                   clear                   pr201
      *
     c                   movel(p)  arep_name     R2arep
     c                   eval      r2co#   = rcco#
     c                   eval      r2grp   = rcgrp
     c                   eval      r2bst2  = rcbst2
     c                   eval      r2bstp  = rcbstp
     c                   eval      r2emp#  = rcemp#
     c                   eval      r2div   = rcdiv
     c                   eval      r2fyr   = rcfyr
     c                   eval      r2cap#  = rccap#
     c                   eval      r2enam  = rcenam
     c                   eval      r2arcd  = rcarcd
     c                   eval      r2amt   = rcamt
      *
     c                   eval      r2podt  = rc_post_dt
     c                   eval      r2pfdt  = pol_from_dt
     c                   eval      r2ptdt  = pol_to_dt
     c                   eval      r2cndt  = pol_canx_dt
     c                   eval      r2nrnw  = new_renew
     c                   eval      r2crat  = comRate
     c                   eval      r2camt  = commAmt
      *
     c                   write     pr201
      *
     c                   add       RCAMT         prem_arep_tot
      *
     c                   if        OpenPRTF = *on
     c                   except    detail
     C                   ENDIF
     c                   add       1             y
     c     y             occur     commDet
     c                   enddo
     c                   endif
      *
     c                   endif
      *
     c                   endsr
    c*====================================================================
    c* routing - set-up for routing pages
    c*====================================================================
     c     routing       begsr
     c                   move      @@co#         routeCo#
    c*
    c* Get the Distribution Information
    c*
     c                   clear                   ORTITL
     c*
     c                   eval      xxfil#   = '0'
     c     orky          chain     por01                              60
     c*
     c                   if        ORTITL = *blank
     c                   eval      ORTITL = title
     c                   endif
     c*
     c                   eval      ORTITL = leftJstify(ORTITL)
    c*
    c* Print the Distribution Information
    c*
     c                   if        not *in60 AND OpenPRTF = *on
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   endif
     c*
     c                   eval      ORTITL = centerFld(ORTITL)
    c*
     c     @@que         comp      *blank                             090908
    c*
     c                   eval      xxque    = @@que
     c                   eval      userName = getUsrName(q1user)
     c                   eval      UPUSNM   = userName
    c*
     c                   endsr
    c*====================================================================
    c* yrBrk - subroutine for breaking year
    c*====================================================================
     c     yrBrk         begsr
     *
     c                   if        firstRead = *on
     *
     c                   if        *inof = *on and OpenPRTF = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
     *
     c                   if        noYrRecFlag  = *off and
     c                             noEmpRecFlag = *off and OpenPRTF = *on
     c                   except    yrTot
     c                   endif
     *
     c                   endif
      *
     c                   eval      prem_yr_tot = 0
     c                   eval      comm_yr_tot = 0
     c                   eval      new_total = 0
     c                   eval      saveYr = RCFYR
     c*** BEGIN CHG ****** 10/28/09 ************************************
     c* Change new/renew logic to use API
     c*
     c*****elky1******   chain     pel01
     c**** elky*******   readpe    pel01                                  60
     c***60elky*******   chain     pex01                              60
     *
     c* call API to determine new/renew status
     c                   call      'WA069R'
     c                   parm      rcco#         ##co#             3 0
     c                   parm      rcgrp         ##fnd             3 0
     c                   parm      rcemp#        ##emp#            9 0
     c                   parm      rcfyr         ##fyr             3 0
     c                   parm      *blanks       ##nwrw            1
      *** beg add ***** 01/16/15 **************************************
      /free
       Chain (RCco#:RCgrp:RCemp#:RCdiv) pex0l;
       If %found;
          ##nwrw = 'R';
       Endif;
      /end-free
      *** end add ***** 01/16/15 **************************************
     c*
     c**************     if        not *in60
     c                   if        ##nwrw = 'R'
     c*** END   CHG ****** 10/28/09 ************************************
     c                   eval      new_flg = *off
     c                   eval      renew_flg = *on
     c                   eval      new_renew = 'Renew'
     c                   else
     c                   eval      new_flg = *on
     c                   eval      renew_flg = *off
     c                   eval      new_renew = 'New'
     c                   endif
     *
      *
      *  Get renewal rate
      *
     c                   clear                   renew_rate
     c                   eval      kkefdt = RCPFDT
     c     icky1         setgt     pic01
     c     icky          readpe    pic01                                  60
     c                   dow       not *in60

     c                   if        ICSTAT = 'A' and
     c                             ICNWRW = 'R'
     c                   leave
     c                   endif

     c     icky          readpe    pic01                                  60
     c                   enddo
      *
     c                   if        ICNWRW = 'R' and
     c                             ICSTAT = 'A' and
     c                             *in60 = *off
     c                   eval      renew_rate = ICRATE
     c                   endif
      *
     c                   eval      x = 1
     c                   eval      kkefdt = RCPFDT
     c     icky1         setgt     pic01
     c     icky          readpe    pic01                                  60
     c                   dow       not *in60

     c                   if        ICSTAT = 'A' and
     c                             ICNWRW = 'N'
     c                   leave
     c                   endif

     c     icky          readpe    pic01                                  60
     c                   enddo
      *
     c                   if        ICNWRW = 'N' and
     c                             ICSTAT = 'A' and
     c                             *in60 = *off
     c                   eval      kkefdt = ICEFDT
     c                   endif
      *
      *  Get new rate/s
      *
     c                   eval      x = 1
      *
     c                   dow       x <= 10
     c     x             occur     comm_scale
     c                   clear                   comm_scale
     c                   add       1             x
     c                   enddo
      *
     c                   eval      x = 1
     c                   eval      newCommRate = *off
     c     icky1         chain     pic01                              60
     c                   dow       not *in60
      *
     c                   if        ICNWRW = 'N' and
     c                             ICSTAT = 'A'
     c     x             occur     comm_scale
     c                   eval      prem_lvl = ICPLVL
     c                   eval      prem_rate = ICRATE
     c                   eval      newCommRate = *on
     c                   add       1             x
     c                   endif
      *
     c                   if        x > 10
     c                   leave
     c                   endif
      *
     c     icky1         reade     pic01                                  60
     c                   enddo
      *
     c                   endsr
     o/copy scopyrout
     oqsysprt   e            route          2
     o                                           20 'COMPANY  . . :'
     o                       routeCo#      z     25
     o          e            route          2
     o                                           20 'DATE RANGE   :'
     o                       rptFromDt           32
     o                                           34 '-'
     o                       rptToDt             45
     o          e            route          2
     o                                           20 'REPORT MTH/YR:'
     o                       reportMthYr         42
     O*** BEG CHG *****  01/28/10  ************************************
     o****      e            route          1 60
     o****                                          '(C) Copyright Progressive'
     o****                                    +   1 'Data Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** END CHG *****  01/28/10  ************************************
    o*
    o* Report Heading
    o*
     o          e            head           1 06
     o                       sys_time
     o                       companyNm          129
     o                       sys_date           198
     o          e            head           1
     o                       q1pgm
     o                       ORTITL             129
     o                                          192 'PAGE:'
     o                       page          z    198
     o          e            head           2
     o                       centerMthYr        109
     o          e            head           1
     o                       rptFromDt           97
     o                                           99 '-'
     o                       rptToDt            110
     o          e            head           1
     o                                          174 'Policy'
     o                                          186 'Policy'
     o                                          198 'Policy'
     o          e            head           1
     o                                            7 'Account'
     o                                           32 'Group'
     o                                           44 'Employer'
     o                                           54 'Division'
     o                                           62 'Policy'
     o                                           70 'Policy'
     o                                           92 'Employer'
     o                                          105 'A/R'
     o                                          118 'A/R'
     o                                          128 'New/'
     o                                          135 'Comm'
     o                                          149 'Commission'
     o                                          162 'Posting'
     o                                          173 'From'
     o                                          184 'To'
     o                                          198 'Cancel'
     o          e            head           0
     o                                            9 'Executive'
     o                                           33 'Number'
     o                                           42 'Number'
     o                                           53 'Number'
     o                                           61 'Year'
     o                                           70 'Number'
     o                                           90 'Name'
     o                                          106 'Code'
     o                                          121 'Amount'
     o                                          129 'Renew'
     o                                          135 'Rate'
     o                                          160 'Date'
     o                                          173 'Date'
     o                                          185 'Date'
     o                                          197 'Date'
     o          e            head           2
     o                       line
     o          e            detail         1
     o                       arep_name           25
     o                       RCGRP         z     31
     o                       RCEMP#        z     44
     o                       RCDIV         z     53
     o                       RCFYR         z     61
     o                       pol_number          82
     o                       emp_name           101
     o                       RCARCD             105
     o                       RCAMT         j    122
     o                       new_renew          129
     o                       comRate       4    135
     o                                          136 '%'
     o                       commAmt       j    150
     o                       rc_post_dt    y    162
     o                       pol_from_dt   y    174
     o                       pol_to_dt     y    186
     o                       pol_canx_dt        198 '  /  /   0'
     o          e            yrTot          1
     o                       line
     o          e            yrTot          2
     o                                           23 'Total for Policy Year :'
     o                       saveGrp       z     31
     o                       saveEmp#      z     44
     o                       saveDiv       z     53
     o                       saveYr        z     61
     o                       prem_yr_tot   j    127
     o                       comm_yr_tot   j    150
     o          e            empTot         2
     o                                           23 'Total for Employer/Div:'
     o                       saveGrp       z     31
     o                       saveEmp#      z     44
     o                       saveDiv       z     53
     o                       prem_emp_tot  j    127
     o                       comm_emp_tot  j    150
     o          e            grpTot         2
     o                                           23 'Total for Group . . . :'
     o                       saveGrp       z     31
     o                       prem_grp_tot  j    127
     o                       comm_grp_tot  j    150
      *
     o          e            arepTot        2
     o                                           23 'Total for Account Rep :'
     o                       arep_name           50
     o                       prem_arep_tot j    122
     o                       comm_arep_tot j    150
     o          e            noRecord       1
     o                                          124 '*** No commissions to repo-
     o                                              rt for current month ***'
** CTDATA MONTHS
JANUARY
FEBRUARY
MARCH
APRIL
MAY
JUNE
JULY
AUGUST
SEPTEMBER
OCTOBER
NOVEMBER
DECEMBER
