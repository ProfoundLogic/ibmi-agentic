     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h*********************************************************************
    h/TITLE - Internal Account Rep Commission Report
    h*********************************************************************
    h*                                                                   *
    h*  Reads through WXR2P and Prints the Internal Account Reps Report  *
    h*                                                                   *
    h*  CALLED FROM:                                                     *
    h*    W3002C                                                         *
    h*                                                                   *
    h*  CALLS TO:                                                        *
    h*                                                                   *
    h*  CREATED: 03/17/04      C:132     PROJECT: 114WGQ                 *
    h*********************************************************************
    h*  REVISIONS:                                                       *
    h*                                                                   *
     H* 08/05/15 C:307 Project: SY354                                     *
     H*                -Adding 'Route To:'                                *
     H*                                                                   *
     h* 08/26/14 C:307 Project: SY354                                     *
     h*                -Implement Routing Page Changes.                   *
     h*                                                                   *
    h*  01/28/10  C:249 Project: SY215                                   *
    h*            C:189 Retrieve brand Name                              *
    h*********************************************************************
    f*
     fWXR2P     if   e           k disk
      *
     fSMORL     if   e           k disk
      *
      *
     fQSYSPRT   o    f  198        printer oflind(*inof)
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
     d xxfrom_dt       s                   like(r2podt)
     d xxto_dt         s                   like(r2podt)
     d xxmth_end_dt    s                   like(r2podt)
     d prem_arep_tot   s             15  2 inz(*zero)
     d new_total       s             11  2 inz(*zero)
     d firstRead       s               n   inz(*off)
     d arepBrkFlag     s               n   inz(*off)
     d noRepRecFlag    s               n   inz(*on)
     d noRecFlag       s               n   inz(*on)
      *
     d comm_arep_tot   s              9  2 inz(*zero)
      *
      * Expand for receiver Error in testing
     d amt_left        s             13  2 inz(*zero)
     d wrk_total       s             11  2 inz(*zero)
      *
     d arep_exp_prm    s              9  2 inz(*zero)
     d arep_exp_com    s              9  2 inz(*zero)
     d arep_new_prm    s              9  2 inz(*zero)
     d arep_new_com    s              9  2 inz(*zero)
     d arep_ren_prm    s              9  2 inz(*zero)
     d arep_ren_com    s              9  2 inz(*zero)
      *
     d saveBtyp        s                   like(R2BST2)
      *
     d saveDiv         s                   like(R2DIV)
     d saveEmp#        s                   like(R2EMP#)
     d saveArep        s                   like(R2AREP)
     d line            s            198
     d arep_name       s             25
     d pol_number      s             18
     d emp_name        s             17
      *
     d rc_post_dt      s              8  0
     d pol_from_dt     s              8  0
     d pol_to_dt       s              8  0
     d pol_canx_dt     s              8  0
     d new_renew       s              5
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
     d commAmt         s              9  2
     d comRate         s              5  2
      *
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
    c*================================================================
    c*  Main
    c*================================================================
     c                   exsr      initialize
      *
     c                   read      pr201                                  60
     c                   dow       not *in60
     c*
     c                   if        saveArep <> R2AREP
     c                   eval      arepBrkFlag = *on
     c                   exsr      arepBrk
     c                   eval      noRepRecflag = *on
     c                   eval      arepBrkFlag = *off
     c                   endif
     c*
     c                   exsr      processRec
     c                   eval      firstRead = *on
     c*
     c                   read      pr201                                  60
     c                   enddo
      *
     c                   eval      arepBrkFlag = *on
     c                   exsr      arepBrk
      *
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfil# = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   eval      *inlr = *on
    c*====================================================================
    c* arepBrk - subroutine for internal account representative
    c*====================================================================
     c     arepBrk       begsr
      *
     c                   if        firstRead = *on
      *
     c                   if        *inof = *on
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
      *
     c                   if        noRepRecFlag = *on
     c                   except    noRecord
      *
     c                   else
      *
     c                   if        arep_new_prm <> *zeros or                    NEW
     c                             arep_new_com <> *zeros
     c                   except    arepTotN
     c                   eval      savebtyp =  R2BST2
     c                   eval      arep_new_prm  = 0
     c                   eval      arep_new_com  = 0
     c                   endif
      *
     c                   if        arep_exp_prm <> *zeros or                    Expiring
     c                             arep_exp_com <> *zeros
     c                   except    arepTotE
     c                   eval      saveBtyp =  R2BST2
     c                   eval      arep_exp_prm  = 0
     c                   eval      arep_exp_com  = 0
     c                   endif
      *
     c                   if        arep_ren_prm <> *zeros or                    Expiring
     c                             arep_ren_com <> *zeros
     c                   except    arepTotR
     c                   eval      saveBtyp =  R2BST2
     c                   eval      arep_ren_prm  = 0
     c                   eval      arep_ren_com  = 0
     c                   endif
      *
     c                   endif
      *
     c                   except    arepTot
     c                   eval      *inof = *on
     c                   endif
      *
     c                   eval      prem_arep_tot = 0
     c                   eval      comm_arep_tot = 0
     c                   eval      saveArep = R2AREP
     c                   movel(p)  R2AREP        arep_name
      *
     c                   eval      saveBtyp = R2BST2
     c                   eval      saveEmp# = R2EMP#
     c                   eval      saveDiv = R2DIV
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
    c*
     c                   exsr      routing
     c                   except    head
      *
     c                   endsr
    c*====================================================================
    c* processRec - subroutine for processing record
    c*====================================================================
     c     processRec    begsr
      *
     c                   if        saveBtyp <> R2BST2 AND
     c                             saveArep =  R2AREP
     c                   select
     c                   when      saveBtyp = '1' AND arep_new_prm <> *zeros    NEW
     c                   except    arepTotN
     c                   eval      savebtyp =  R2BST2
     c                   eval      arep_new_prm  = 0
     c                   eval      arep_new_com  = 0
     c                   when      savebtyp = '2' AND arep_exp_prm <> *zeros    EXPIRING
     c                   except    arepTotE
     c                   eval      saveBtyp =  R2BST2
     c                   eval      arep_exp_prm  = 0
     c                   eval      arep_exp_com  = 0
      *
     c                   when      savebtyp = '3' AND arep_ren_prm <> *zeros    RENEW
     c                   except    arepTotR
     c                   eval      saveBtyp =  R2BST2
     c                   eval      arep_ren_prm  = 0
     c                   eval      arep_ren_com  = 0
      *
     c                   other
     c                   eval      saveBtyp =  R2BST2
      *
     c                   endsl
      *
     c                   endif                                                  saveBtyp <> RCBST2
      *
     c                   if        R2AMT > 0
      *
     c                   eval      new_total = R2AMT + new_total
      *
     c                   eval      commAmt  = R2CAMT
     c                   eval      comRate  = R2CRAT
      *
     c                   endif
      *
     c                   eval      rc_post_dt  = R2PODT
     c                   eval      pol_from_dt = R2PFDT
     c                   eval      pol_to_dt   = R2PTDT
     c                   eval      pol_canx_dt = R2CNDT
     c                   eval      pol_number = R2CAP#
     c                   eval      emp_name  = R2ENAM
     c                   eval      new_renew = R2NRNW
     c                   eval      arep_name = R2arep
      *
     c                   if        *inof
     c                   except    head
     c                   eval      *inof = *off
     c                   endif
      *
     c                   if        R2CAMT <> 0
     c                   eval      noRepRecflag = *off
     c                   eval      noRecflag    = *off
     c                   add       R2AMT         prem_arep_tot
     c                   add       R2CAMT        comm_arep_tot
      *
     c                   if        saveBtyp <> R2BST2 AND
     c                             saveArep =  R2AREP
     c                   IF        saveBtyp = '1' AND arep_new_prm <> *zeros    NEW
     c                   except    arepTotN
     c                   eval      savebtyp =  R2BST2
     c                   eval      arep_new_prm  = 0
     c                   eval      arep_new_com  = 0
     c                   endIF
      *
     c                   IF        savebtyp = '2' AND arep_exp_prm <> *zeros    EXPIRING
     c                   except    arepTotE
     c                   eval      saveBtyp =  R2BST2
     c                   eval      arep_exp_prm  = 0
     c                   eval      arep_exp_com  = 0
     c                   endIF
      *
     c                   IF        savebtyp = '3' AND arep_ren_prm <> *zeros    RENEW
     c                   except    arepTotR
     c                   eval      saveBtyp =  R2BST2
     c                   eval      arep_ren_prm  = 0
     c                   eval      arep_ren_com  = 0
     c                   endIF
      *
     c                   endif                                                  saveBtyp <> RCBST2
      *
     c                   except    detail
      *
     c                   select
      *
     c                   when      R2BSTP  = 'N'                                New Business
     c                   eval      arep_new_prm = arep_new_prm + R2AMT
     c                   eval      arep_new_com = arep_new_com + R2CAMT
      *
     c                   when      R2BSTP  = 'E'                                Expiring Business
     c                   eval      arep_exp_prm = arep_exp_prm + R2AMT
     c                   eval      arep_exp_com = arep_exp_com + R2CAMT
      *
     c                   when      R2BSTP  = 'R'                                Renewal Business
     c                   eval      arep_ren_prm = arep_ren_prm + R2AMT
     c                   eval      arep_ren_com = arep_ren_com + R2CAMT
     c                   endsl
      *
     c                   endif                                                  comm_dtl_amt <> 0
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
     c                   if        not *in60
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
     o***       e            route          1 60
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
     o                       R2GRP         z     31
     o                       R2EMP#        z     44
     o                       R2DIV         z     53
      *  FOR TESTING ONLY - PRINT TYPE
     o                       R2BSTP              56
     o                       R2FYR         z     61
     o                       pol_number          82
     o                       emp_name           101
     o                       R2ARCD             105
     o                       R2AMT         j    122
     o                       R2NRNW             129
     o                       R2CRAT        4    135
     o                                          136 '%'
     o                       R2CAMT        j    150
      *
     o                       R2PODT        y    162
     o                       R2PFDT        y    174
     o                       R2PTDT        y    186
     o                       R2CNDT             198 '  /  /   0'
      *
     o          e            arepTotN       2
     o                                           23 'New Business:     '
     o                       arep_new_prm  j    122
     o                       arep_new_com  j    150
     o          e            arepTotE       2
     o                                           23 'Expiring Business:'
     o                       arep_exp_prm  j    122
     o                       arep_exp_com  j    150
     o          e            arepTotR       2
     o                                           23 'Renewal Business: '
     o                       arep_ren_prm  j    122
     o                       arep_ren_com  j    150
      *
     o          e            arepTot        2
     o                                           23 'Total for Account Rep :'
     o                       arep_name           50
     o                       prem_arep_tot j    122
     o                       comm_arep_tot j    150
     o          e            noRecord       1
     o                                          124 '*** No commissions to repo-
     o                                              rt for current month ***'
** CTDATA MONTHS                                                                               |
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
