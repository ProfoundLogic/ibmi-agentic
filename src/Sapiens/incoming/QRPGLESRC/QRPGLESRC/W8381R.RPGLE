     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h**********************************************************************************************
    h/TITLE W8381R - Upload premium - report
     h option(*nodebugio:*srcstmt)
    h**********************************************************************************************
    h*  DESCRIPTION:  This program reads the flat file WXUPP, edits the records, and outputs an   *
    h*                error report or a successful upload verication report. This program can     *
    h*                process one header and one trailer record in each flat file only.           *
    h*                                                                                            *
     h*  Called from:  W8381C                                                                      *
     h*                                                                                            *
     h*  Created: 02/18/03 by C:136 for project: WC1115                                            *
    h**********************************************************************************************
    h* Revisions:                                                                                 *
     H*
     H* 08/26/14 C:307 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     h*  01/28/10  C:249 Project: SY215
     h*            C:189 Retrieve brand Name
     h*                                                               *
     H* 11/19/09 C:33  Project: 25WQI
     H*               -For 025, load the invoice # when writing billing rcds
     h*                                                                                            *
     h* 03/07/07 C:136 Support: WA135 52336                                                        *
     h*                Add negative sign to process the posted amount as a negative number.        *
     h*                                                                                            *
/*  0h* 09/01/06 C:136 SUPPORT: WA135 52009                                                        *
/*   h*                PUT IN CHANGES TO COMBINE A/R SO THAT CEU DOESN'T                           *
/*   h*                ERR OUT.                                                                    *
/*   h*                                                                                            *
     h* 06/20/06 C:136 Project: WC1324                                                             *
     h*                Corrections for WC1324.                                                     *
     H*                                                                                            *
     H* 10/20/05 C:053 PROJECT: WC1324                                                             *
     H*                Enhancements to payroll/premium import.                                     *
     h*                                                                                            *
     h* 07/28/05 C:136 Support: WA135 48010                                                        *
     h*                Add edit to ensure that posting date of A/R record falls within policy      *
     h*                date range.                                                                 *
     h*                                                                                            *
     h* 04/08/05 C:136 Project: WC1288 (Summarize Self-Reporting)                                  *
     h*                Populate EADIV with the state reporting division and EARDIV with the        *
     h*                division actually generating payroll premium.                               *
     h*                                                                                            *
     h* 02/16/05 C:136 Support: WA129 45825                                                        *
     h*                Add error for invalid vendor number.                                        *
     h*                                                                                            *
     h* 01/09/04 C:136 Project: WC1115                                                             *
     h*                Check for existence of vendor number in vendor history instead of           *
     h*                employer detail.                                                            *
     h*                                                                                            *
     h* 11/04/03 C:136  Project: WC1161                                                            *
     h*                 Add code to load new category field in A/R detail.                         *
     h*                                                                                            *
     h* 09/11/03 C:136  Support: 38984                                                             *
     h*                 Correct routing information.  Correct error check on vendor A/R codes.     *
     h*                                                                                            *
     h* 06/24/03 C:136  Support: WC1115                                                            *
     h*                 Change selection on agent plan types to be either 'A' or not 'A' in case   *
     h*                 modified code has different plan types.                                    *
     h*                                                                                            *
    h**********************************************************************************************
      * Payroll flat file
     fwxupp     if   e             disk
      * Routing file
     fsmorl     if   e           k disk
      * Carrier/state reporting file  (co#/cap#)
     fwmrpl3    if   e           k disk
      * Employer detail file  (co#/fnd/emp/div/fyr)
     fwdell     if   e           k disk
      * Payroll vendor file  (co#/vdr#)
     fwmvpl     if   e           k disk
      *** beg add ***** 01/09/04 ******************************************************************
      * Vendor history file  (co#/fnd/emp/div/yr/efdt)
     fwdvhl1    if   e           k disk
      *** end add ***** 01/09/04 ******************************************************************
      *** beg add ***** 04/08/05 ******************************************************************
      * Employer Master file (co#/fnd/emp/div)
     fwmeml     if   e           k disk
      *** end add ***** 04/08/05 ******************************************************************
      * Table file (co#/key)
     fwtabl     if   e           k disk
      * A/R file
      *** beg chg *** 10/20/05 *******************************************  ***********************
     f*wmeap *** o ** e             disk
      *** beg chg *** 09/01/06 *******************************************  ***********************
     f*********** wdeal4    uf a e           k disk
     fwdeal4    uf   e           k disk    rename(pea01:dea0l4)
     fwdeap     if a e           k disk    rename(pea01:dea0p)
      *** end chg *** 09/01/06 *******************************************  ***********************
      *** end chg *** 10/20/05 *******************************************  ***********************
      * Print files
     fqsysprt   o    f  210        printer oflind(*inoa)                        error report
     fqsysprt1  o    f  105        printer oflind(*inob)                        upload report
      *=============================================================================================
      * Stand alone fields
      *=============================================================================================
     d @@que           s             10
     d @@upld          s              1
     d @@rpdt          s              7
     D companyNm       s             60
     d recType         s              1
     d recCount        s             13  0 inz(*zero)
     d accumPostAmt    s             17  2 inz(*zero)
     d accumCommAmt    s             11  2 inz(*zero)
     d accumNetAmt     s             17  2 inz(*zero)
     d kkkey           s             10
     d kkvdr#          s              5  0
     d userName        s             50
     d UPUSNM          s                   like(userName)
     d xxque           s                   like(@@que)
     d group#          s                   like(ELFND)
     d company         s                   like(ELCO#)
     d fnrrn           s              9  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d errCount        s              6  0 inz(*zero)
     d firstError      s              1n   inz(*on)
     d firstRead       s              1n   inz(*on)
     d badDATA         s              1n   inz(*off)
     d errorFlag       s              1n   inz(*off)
     d headerFirst     s              1n   inz(*off)
     d headerChk       s              1n   inz(*off)
     d trailerLast     s              1n   inz(*off)
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d xxfil#          s                   like(ORFIL#)
     d kkcap#          s                   like(RPCAP#)
     d kkco#           s                   like(ELCO#)
     d kkfnd           s                   like(ELFND)
     d kkemp           s                   like(ELEMP#)
     d kkdiv           s                   like(ELDIV)
     d kkfyr           s                   like(ELFYR)
     d runDate         s                   like(ELPFDT)
     d xxfnd           s                   like(ELFND)
     d xxemp           s                   like(ELEMP#)
     d xxdiv           s                   like(ELDIV)
     d xxfyr           s                   like(ELFYR)
     d commPlan        s              1
     d isoPostDate     s               d   datfmt(*iso)
     d isotrDate       s               d   datfmt(*iso)
     d x7PostDate      s              7  0
     d x7transDate     s              7  0
     d transMonth      s              2  0
     d sysMonth        s              2  0
     D ORTITL1         s                   like(ORTITL)
     d line            s            199
     d line1           s            105
     d digitCheck      s              2  0
      *** beg add ***** 06/20/06 *******************************************************************
     d xxsycd          s                   like(EASYCD)
      *** end add ***** 06/20/06 *******************************************************************
      *=============================================================================================
      * Constants
      *=============================================================================================
     d digits          c                   '0123456789'
     d title           c                   'Premium Upload Error Report'
     d title1          c                   'Premium Upload Report'
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
      *=============================================================================================
      * Arrays
      *=============================================================================================
      *** beg chg ***** 07/28/05 ***********************************************
     d********** err             s             60    dim(21) ctdata perrcd(1)        error msg array
     d err             s             60    dim(30) ctdata perrcd(1)             error msg array
      *** end chg ***** 07/28/05 ***********************************************
      *** beg add *** 10/20/05 *******************************************  ***********************
     d b#a             s              9  0 dim(4)                               batch # assignment
     d bamt            s             11  2 dim(4)                               batch # assignment
      *** end add *** 10/20/05 *******************************************  ***********************
     d***  Begin Add  ***  11/19/09  **************************************
     d cltID           s              3
     d***  End   Add  ***  11/19/09  **************************************
      *=============================================================================================
      * Data structures
      *=============================================================================================
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d checkHeader     ds
     d  chrec_type                    1
     d  chnum_range                   8
     d  chtransDate                   8    overlay(chnum_range)
     d  chtransYear                   4    overlay(chnum_range)
     d  chtransMth                    2    overlay(chnum_range:5)
     d  chtransDay                    2    overlay(chnum_range:7)

     d checkDetail     ds
     d  cdrec_type                    1
     d  cdpolicy#                    25
     d  cdnum_range                  59
     d  cdfnd                         3    overlay(cdnum_range)
     d  cdemp#                        9    overlay(cdnum_range:4)
     d  cddiv                         5    overlay(cdnum_range:13)
     d  cdfyr                         3    overlay(cdnum_range:18)
     d  cdpost_date                   8    overlay(cdnum_range:21)
     d  cdpost_year                   4    overlay(cdnum_range:21)
     d  cdpost_mth                    2    overlay(cdnum_range:25)
     d  cdpost_day                    2    overlay(cdnum_range:27)
     d  cdpost_amt                   11    overlay(cdnum_range:29)
     d  cdcomm_amt                    9    overlay(cdnum_range:40)
     d  cdnet_amt                    11    overlay(cdnum_range:49)

     d checkTrailer    ds
     d  ctrec_type                    1
     d  ctnum_range                  47
     d  ctrec_count                   2    overlay(ctnum_range)
     d  ctpost_amt_tt                17    overlay(ctnum_range:3)
     d  ctcomm_amt_tt                11    overlay(ctnum_range:20)
     d  ctnet_amt_tot                17    overlay(ctnum_range:31)

     d header          ds
     d  hrec_type                     1
     d  htransDate                    8  0
     d  htransYear                    4    overlay(htransDate)
     d  htransMth                     2    overlay(htransDate:5)
     d  htransDay                     2    overlay(htransDate:7)

     d detail          ds
     d  drec_type                     1
     d  dpolicy#                     25
     d  dfnd                          3  0
     d  demp#                         9  0
     d  ddiv                          5  0
     d  dfyr                          3  0
     d  dpost_date                    8  0
     d  dpost_year                    4    overlay(dpost_date)
     d  dpost_mth                     2    overlay(dpost_date:5)
     d  dpost_day                     2    overlay(dpost_date:7)
     d  dpost_amt                    11  2
     d  dcomm_amt                     9  2
     d  dnet_amt                     11  2
      *** beg add ***** 03/07/07 ***********************************************
     d  dneg_amt_flg                  1
      *** end add ***** 03/07/07 ***********************************************

     d trailer         ds
     d  trec_type                     1
     d  trec_cnt                     13  0
     d  tpost_amt_tt                 17  2
     d  tcomm_amt_tt                 11  2
     d  tnet_amt_tot                 17  2

     d errorFields     ds
     d  rec_type                      7
     d  policy#                      25
     d  grp#                          3
     d  emp#                          9
     d  div                           5
     d  fyr                           3
     d  tr_post_date                 10
     d  post_amt                     17
     d  comm_amt                     11
     d  net_amt                      17
     d  errReason                    60

     d psds          esds                  extname(szq1p)

     d/copy sprdate
     d/copy sprcompany
     d/copy sprstring
     d/copy wprgroup
     d/copy wprusrprms
     d/copy wpragent
     d***  Begin Add  ***  11/19/09  **************************************
     d/copy wprar
     d/copy srinv#pr
     d*
     d GetInv#         pr                  like(EAINV#)
     d***  End   Add  ***  11/19/09  **************************************
      *=============================================================================================
      * KLISTS
      *=============================================================================================
     c     elky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#

     c     rpky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkcap#

      *** beg add ***** 01/09/04 ******************************************************************
     c     vhky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
     c                   kfld                    x7PostDate
      *** end add ***** 01/09/04 ******************************************************************

     c     vpky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkvdr#

     c     ttky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey

      *** beg add ***** 04/08/05 **********************************************
     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp
      *** end add ***** 04/08/05 **********************************************
      *** beg add ***** 10/20/05 **********************************************
     C     eaky1         KLIST
     C                   KFLD                    kkco#
     C                   KFLD                    EABAT#
      *** end add ***** 10/20/05 **********************************************
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    @@que
     c                   parm                    @@upld
     c                   parm                    @@rpdt

     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *=============================================================================================
      * Initialize
      *=============================================================================================
      *
      * Get report info.
      *
     c     *hms          move      q1time        sys_time
     c     *usa          move      *date         sys_date
     c                   eval      kkco# = UserCo#(q1user)
     c                   eval      companyNm = GetCompNam(kkco#)
     c                   eval      companyNm = centerFld(companyNm)
     c***  Begin Add  ***  11/19/09  **************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltID
     c***  End   Add  ***  11/19/09  **************************************
      *=============================================================================================
      * Edit flat file
      *=============================================================================================
      *
      * Run through file the first time checking for errors. If error is found, except the line to
      * the error report and continue checking the file for other errors. Prevent the batch from
      * further processing.
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
      *
     c                   read      pup01                                  60

     c                   dow       *in60 = *off

     c                   clear                   errorFields
     c     1             subst     DATA          recType

     c                   exsr      checkDATA

     c                   if        badDATA = *off

     c                   select

     c                   when      recType = 'H'
     c                   movel     DATA          header
     c                   eval      recCount = recCount + 1
     c                   exsr      headerEdit
     c                   eval      headerFirst = *on

     c                   when      recType = 'D'
     c                   clear                   detail
     c                   movel     DATA          detail
     c                   eval      recCount = recCount + 1
     c                   exsr      detailEdit

      *** beg add ***** 03/07/07 *******************************************************
     c                   if        dneg_amt_flg = '-'
     c                   eval      dpost_amt = dpost_amt * -1
     c                   endif
      *** end add ***** 03/07/07 *******************************************************
     c                   eval      accumPostAmt = accumPostAmt + dpost_amt
     c                   eval      accumCommAmt = accumCommAmt + dcomm_amt
     c                   eval      accumNetAmt  = accumNetAmt  + dnet_amt

     c                   when      recType = 'T'
     c                   movel     DATA          trailer
     c                   eval      recCount = recCount + 1
     c                   exsr      trailerEdit
     c                   eval      trailerLast = *on

     c                   other
     c                   eval      recCount = recCount + 1
     c                   exsr      otherEdit

     c                   endsl

     c                   endif                                                  badData = *off

     c                   read      pup01                                  60
     c                   enddo

     c                   if        DATA = *blanks

     c                   seton                                        21
     c                   eval      errCount = errCount + 1
     c                   exsr      printError                                   error report

     c                   endif

     c  n21              if            headerFirst = *off
     c                             and badData     = *off
     c                             or  trailerLast = *off
     c                             and badData     = *off

     c                   seton                                        22
     c                   eval      errCount = errCount + 1
     c                   exsr      printError                                   error report

     c                   endif

     c                   if        errCount = *zero
     c                   exsr      printError                                   error report
     c                   exsr      printUpload                                  upload report
      *** beg add *** 10/20/05 *******************************************  ***********************
     c                   exsr      getBatchNum
      *** end add *** 10/20/05 *******************************************  ***********************
     c                   exsr      processWXUPP
     c                   eval      @@upld = 'Y'
     c                   move(p)   x7transDate   @@rpdt
      *** beg add *** 10/20/05 *******************************************  ***********************
     c                   exsr      finalAR
      *** end add *** 10/20/05 *******************************************  ***********************
     c                   else
     c                   eval      @@upld = 'N'
     c                   endif

      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxfil# = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       xxfil# = '1';
       Chain ORKY POR01;
       If %found;
          Except Route1;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c                   eval      *inlr = *on
      *** beg add *** 10/20/05 *******************************************  ***********************
      *============================================================================================
      *  Assign Batch Numbers depending on Auto Transfer flag in WMVPP
      *============================================================================================
     c     asignBatchNum begsr

      * The deal on b#a -batch assignment #'s:
      *    autotran_N = *on  b#a(1)(2) non transfer billing/payroll
      *    autotran_Y = *on  b#a(3)(4) transfer billing/payroll
      *
      *    autotran_N = *off b#a( )( ) non transfer billing/payroll
      *    autotran_Y = *on  b#a(1)(2) transfer billing/payroll
      *
      *    autotran_N = *on  b#a(1)(2) non transfer billing/payroll
      *    autotran_Y = *off b#a( )( ) transfer billing/payroll

     c                   select
     c     autotran_N    wheneq    *on
     c     autotran_Y    andeq     *on

     c     vpautr        ifeq      'N'
     c                   eval      EAACHF = 'S'
     c                   eval      EATFAP = 'N'
     c                   eval      EATFFL = 'N'
     c                   eval      EASTAT = ' '
     c                   eval      EAAUTR = ' '

     c                   select
     c     rcdtype       wheneq    '1'
     c                   eval      eabat# = b#a(1)
     c                   add       eaamt         bamt(1)

     c     rcdtype       wheneq    '2'
     c                   eval      eabat# = b#a(2)
     c                   add       eaamt         bamt(2)
     c                   endsl
     c                   endif

     c     vpautr        ifeq      'Y'
     c     vpautr        oreq      ' '
     c                   eval      EAACHF = 'S'
     c                   eval      EATFAP = 'Y'
     c                   eval      EATFFL = 'Y'
     c                   eval      EASTAT = ' '
     c                   eval      EAAUTR = 'Y'

     c                   select
     c     rcdtype       wheneq    '1'
     c                   eval      eabat# = b#a(3)
     c                   add       eaamt         bamt(3)

     c     rcdtype       wheneq    '2'
     c                   eval      eabat# = b#a(4)
     c                   add       eaamt         bamt(4)
     c                   endsl
     c                   endif

     c     autotran_N    wheneq    *on
     c     autotran_Y    andeq     *off
     c     autotran_N    oreq      *off
     c     autotran_Y    andeq     *on

     c     vpautr        ifeq      'N'
     c                   eval      EAACHF = 'S'
     c                   eval      EATFAP = 'N'
     c                   eval      EATFFL = 'N'
     c                   eval      EASTAT = ' '
     c                   eval      EAAUTR = ' '

     c                   select
     c     rcdtype       wheneq    '1'
     c                   eval      eabat# = b#a(1)
     c                   add       eaamt         bamt(1)

     c     rcdtype       wheneq    '2'
     c                   eval      eabat# = b#a(2)
     c                   add       eaamt         bamt(2)
     c                   endsl
     c                   endif

     c     vpautr        ifeq      'Y'
     c     vpautr        oreq      ' '
     c                   eval      EAACHF = 'S'
     c                   eval      EATFAP = 'Y'
     c                   eval      EATFFL = 'Y'
     c                   eval      EASTAT = ' '
     c                   eval      EAAUTR = 'Y'

     c                   select
     c     rcdtype       wheneq    '1'
     c                   eval      eabat# = b#a(1)
     c                   add       eaamt         bamt(1)

     c     rcdtype       wheneq    '2'
     c                   eval      eabat# = b#a(2)
     c                   add       eaamt         bamt(2)
     c                   endsl
     c                   endif

     c                   endsl
     c                   endsr                                                  checkDATA
      *** end add *** 10/20/05 *******************************************  ***********************
      *=============================================================================================
      * checkDATA subroutine - looks for character data where numerical data should be.
      *                        turns on badData flag and prevents decimal data error.
      *=============================================================================================
     c     checkDATA     begsr

     c                   clear                   digitCheck

     c                   select

     c                   when      recType = 'H'
     c                   clear                   checkHeader
     c                   movel     DATA          checkHeader
     c     digits        check     chnum_range   digitCheck

     c                   if        digitCheck    <> 0
     c                   eval      badDATA = *on
     c                   eval      errorFlag = *on
     c                   move(p)   err(15)       errReason
     c                   exsr      printError
     c                   endif

     c                   when      recType = 'D'
     c                   clear                   checkDetail
     c                   movel     DATA          checkDetail
     c     digits        check     cdnum_range   digitCheck

     c                   if        digitCheck <> 0
     c                   eval      badDATA = *on
     c                   eval      errorFlag = *on
     c                   move(p)   err(15)       errReason
     c                   exsr      printError
     c                   endif

     c                   when      recType = 'T'
     c                   clear                   checkTrailer
     c                   movel     DATA          checkTrailer
     c     digits        check     ctnum_range   digitCheck

     c                   if        digitCheck    <> 0
     c                   eval      badDATA = *on
     c                   eval      errorFlag = *on
     c                   move(p)   err(15)       errReason
     c                   exsr      printError
     c                   endif

     c                   endsl
      *
      * Ensure the first record is an 'H' type. The routine that turns on *in22 will print the error
      *
     c                   if        headerChk = *off and
     c                             recType <> 'H'
     c                   eval      badDATA = *on
     c                   endif

     c                   eval      headerChk = *on

     c                   endsr                                                  checkDATA
      *=============================================================================================
      *  detailEdit subroutine - edit detail record
      *=============================================================================================
     c     detailEdit    begsr
      *
      * Ensure that there are no detail records read before the header record.
      *
     c                   if        headerFirst = *off
     c                   eval      errorFlag = *on
     c                   move(p)   err(7)        errReason
     c                   exsr      printError
     c                   endif
      *
      * Ensure that there are no detail records after the trailer record.
      *
     c                   if        trailerLast = *on
     c                   eval      errorFlag = *on
     c                   move(p)   err(9)        errReason
     c                   exsr      printError
     c                   endif
      *
      * Validate post date
      *
     c     *iso          test(de)                dpost_date

     c                   if        %error
     c                   eval      errorFlag = *on
     c                   move(p)   err(2)        errReason
     c                   exsr      printError
     c                   else
     c     *iso          move      dpost_date    isoPostDate
     c     *cymd         move      isoPostDate   x7PostDate
     c                   endif                                                  %error
      *
      * Ensure that required information is not missing.
      *
     c                   if            dpolicy# = *blanks

     c                   if            dfnd     = *zeros
     c                             or  demp#    = *zeros
     c                             or  dfnd     = *zeros
     c                   eval      errorFlag = *on
     c                   move(p)   err(6)        errReason
     c                   exsr      printError
     c                   endif                                                  dfnd=*zeros

     c                   endif                                                  dpolicy#=*blanks

     c                   if            dfnd     = *zeros
     c                             or  demp#    = *zeros
     c                             or  dfnd     = *zeros

     c                   if            dpolicy# = *blanks
     c                   eval      errorFlag = *on
     c                   move(p)   err(6)        errReason
     c                   exsr      printError
     c                   endif                                                  dpolicy#=*blanks

     c                   endif                                                  dfnd = *zeros
      *
      * Validate carrier policy # if provided, otherwise validate fnd/emp/div/fyr. Ensure that
      * A/R codes are in WMVPP (Vendor file)
      *
     c                   if        dpolicy# <> *blanks
     c                   eval      kkcap# = dpolicy#
     c     rpky          chain     prp01                              60

     c                   if        *in60 = *off
     c                   eval      kkfnd = RPFND
     c                   eval      kkemp = RPEMP#
     c                   eval      kkdiv = ddiv
     c                   eval      kkfyr = RPFYR
     c                   else
     c                   eval      errorFlag = *on
     c                   move(p)   err(3)        errReason                      invalid policy#
     c                   exsr      printError
     c                   endif                                                  *in60 = *off

     c                   else
     c                   eval      kkfnd = dfnd
     c                   eval      kkemp = demp#
     c                   eval      kkdiv = ddiv
     c                   eval      kkfyr = dfyr

     c                   endif                                                  dpolicy# <> *blanks

     c     elky          chain     pel01
     c                   if        not %found
     c                   eval      errorFlag = *on
     c                   move(p)   err(17)       errReason                      invalid policy
     c                   exsr      printError

     c                   else
      *
      * If commission plan is direct billed, validate dpost_amt. If plan is agency billed, validate
      * dcomm_amt and dnet_amt.
      *
      *** beg chg ***** 01/09/04 *******************************************************************
     c*****************  eval      kkvdr# = ELVDR#
     c     vhky1         setgt     pvh01
     c     elky          readpe    pvh01
     c                   if        not %eof

     c                   select
     c                   when      vhexdt = 0
     c                   eval      kkvdr# = VHVDR#
     c                   when      vhexdt <> 0 and vhexdt > x7PostDate
     c                   eval      kkvdr# = VHVDR#
     c                   other
     c                   eval      kkvdr# = 0
     c                   endsl

     c                   else
     c                   eval      kkvdr# = 0
     c                   endif
      *** end chg ***** 01/09/04 *******************************************************************
      *
     c     vpky          chain     pvp01
      *** beg add ***** 02/16/05 ******************************************************************
     c                   if        %found
      *** end add ***** 02/16/05 ******************************************************************
      *** beg add *** 10/20/05 *******************************************  ***********************
     c                   if        vpautr = 'Y'
      *** beg add ***** 06/20/06 ********************************************************
     c                             or VPAUTR = ' '
      *** end add ***** 06/20/06 ********************************************************
     c                   eval      autotran_Y = *on
     c                   endif                                                  dpost_amt<=0
      *
      *** beg chg ***** 06/20/06 ********************************************************
     c*****************  if        vpautr = ' '
     c                   if        vpautr = 'N'
      *** end chg ***** 06/20/06 ********************************************************
     c                   eval      autotran_N = *on
     c                   endif                                                  dpost_amt<=0
      *** end add *** 10/20/05 *******************************************  ***********************

     c                   eval      commPlan = getEmpPlan(kkco#:kkfnd:kkemp:
     c                                        kkdiv:kkfyr:x7PostDate)

      *** beg chg **** 06/24/02 *******************************************************************
     c****************** if        commPlan = *blank
     c                   if        commPlan <> 'A'
      *** end chg **** 06/24/02 *******************************************************************

     c                   if        dpost_amt <= 0
     c                   eval      errorFlag = *on
     c                   move(p)   err(5)        errReason                      invalid post amount
     c                   exsr      printError
     c                   endif                                                  dpost_amt<=0

      *** beg chg **** 09/11/03 *******************************************************************
     c****************   if        VPCPCD = *blank or
     c                   if        VPPBCD = *blank or
      *** end chg **** 09/11/03 *******************************************************************
     c                             VPPPCD = *blank
     c                   eval      errorFlag = *on
     c                   move(p)   err(18)       errReason                      no A/R codes
     c                   exsr      printError
     c                   endif                                                  VPCPCD = *blank

     c                   endif                                                  commPlan <> 'A'

     c                   if        commPlan  = 'A'

     c                   if        dcomm_amt <= 0
     c                   eval      errorFlag = *on
     c                   move(p)   err(8)        errReason                      invalid comm amt
     c                   exsr      printError
     c                   endif                                                  dpost_amt<=0

     c                   if        dnet_amt  <= 0
     c                   eval      errorFlag = *on
     c                   move(p)   err(12)       errReason                      invalid net amt
     c                   exsr      printError
     c                   endif                                                  dpost_amt<=0

     c                   if        VPCPCD = *blank or
     c                             VPPBCD = *blank
     c                   eval      errorFlag = *on
     c                   move(p)   err(19)       errReason                      no A/R codes
     c                   exsr      printError
     c                   endif                                                  VPCPCD = *blank

     c                   endif                                                  commPlan = 'A'

      *** beg add ***** 02/16/05 ******************************************************************
     c                   else
     c                   eval      errorFlag = *on
     c                   move(p)   err(21)       errReason                      no A/R codes
     c                   exsr      printError
     c                   endif                                                  vpky is %found
      *** end add ***** 02/16/05 ******************************************************************
      *** beg add ***** 07/28/05 ******************************************************************
      *
      *  Ensure posting date is in policy date range
      *
     c                   if        x7PostDate <  ELPFDT or
     c                             x7PostDate >= ELPTDT
     c                   eval      errorFlag = *on
     c                   move(p)   err(22)       errReason                      post date out of rng
     c                   exsr      printError
     c                   endif
     c
      *** end add ***** 07/28/05 ******************************************************************

     c                   endif                                                  not %found

     c                   endsr                                                  detailEdit
      *** beg add *** 10/20/05 *******************************************  ***********************
      *============================================================================================
      *  Finalize A/R Batch records - delete place holders and update batch totals
      *============================================================================================
     c     finalAR       begsr

     c                   do        4             z
     c                   eval      eabat# = b#a(z)
     c     eaky1         chain     wdeal4                             99

     c                   dow       *in99 = *off
     c                   if        eachk# = 9999999 and
     c                             eackam = 9999999.99
      *** beg chg ***** 09/01/06 *********************************************
     c***********        delete    pea01
     c                   delete    dea0l4
      *** end chg ***** 09/01/06 *********************************************

     c                   else
     c                   eval      eabata = bamt(z)
      *** beg chg ***** 09/01/06 *********************************************
     c***********        update    pea01
     c                   update    dea0l4
      *** end chg ***** 09/01/06 *********************************************
     c                   endif

     c     eaky1         reade     wdeal4                                 99
     c                   enddo
     c                   enddo

     c                   endsr                                                  headerEdit
      *** end add *** 10/20/05 *******************************************  ***********************
      *=============================================================================================
      * headerEdit subroutine - edit header record
      *=============================================================================================
     c     headerEdit    begsr
      *
      * Ensure only one header record exists in file.
      *
     c                   if        headerFirst = *on
     c                   eval      errorFlag = *on
     c                   move(p)   err(16)       errReason
     c                   exsr      printError
     c                   endif
      *
      * Validate transaction date.
      *
     c     *iso          test(de)                htransDate
     c                   if        %error
     c                   eval      errorFlag = *on
     c                   move(p)   err(2)        errReason
     c                   exsr      printError

     c                   else
     c     *iso          move      htransDate    isotrDate
     c     *cymd         move      isoTrDate     x7transDate
     c                   extrct    isotrDate:*m  transMonth
     c                   extrct    sys_Date:*m   sysMonth

     c                   if        transMonth <> sysMonth
     c                   eval      errorFlag = *on
     c                   move(p)   err(20)       errReason
     c                   exsr      printError
     c                   endif                                                  transMonth

     c                   endif                                                  %error

     c                   endsr                                                  headerEdit
      *** beg add *** 10/20/05 *******************************************  ***********************
      *============================================================================================
      * Gets Batch Numbers for later assignment
      *============================================================================================
     c     getBatchNum   begsr
      *
      *  Determine last used batch number
      *
     c     *like         define    EABAT#        nxbat#
     c     *like         define    *in99         autotran_N
     c     *like         define    *in99         autotran_Y
      *** beg add ***** 06/20/06 ***********************************************
     c     kkco#         reade     psy01
     c                   dow       not(%eof)
     c                   if        SYTYPE = 'Y' and
     c                             SYSTAT = 'A'
     c                   eval      xxsycd = SYKEY
     c                   leave
     c                   endif
     c     kkco#         reade     psy01
     c                   enddo
      *** end add ***** 06/20/06 ***********************************************

      ******             sub       nxbat#        nxbat#
     c                   move      *hival        EABAT#
     c     eaky1         setgt     wdeal4
     c     kkco#         readpe(n) wdeal4                                 99
     c     *in99         ifeq      *off
     c                   z-add     EABAT#        nxbat#
     c                   endif
      *
      *  Determine how many batch numbers we need (array explained below)
      *
     c                   z-add     *zeros        y                 2 0
     c     autotran_N    ifeq      *on
     c                   add       2             y
     c                   endif
      *
     c     autotran_Y    ifeq      *on
     c                   add       2             y
     c                   endif
      *
     c                   do        y             z                 2 0
      *
      *** beg chg ***** 09/01/06 *************************************
     c***********        clear                   pea01
     c                   clear                   dea0p
      *** end chg ***** 09/01/06 *************************************
     c     nxbat#        add       z             EABAT#
     c                   move      kkco#         EACO#
     c                   move      *HIVAL        EACHK#
     c                   move      *HIVAL        EACKAM
     c                   move      'N'           EATFAP
     c                   move      'N'           EATFFL
     c                   move      'S'           EAACHF
     c                   eval      EASYCD = xxsycd
      *
     c                   move      q1pgm         EAPGM
     c                   move      q1user        EAUSER
     c                   time                    dsdttm
     c                   eval      EADT   = dtCYMD(dsdate)
     c                   moveL     dstime        EATIME
     c                   move      q1pgm         EAIPGM
     c                   move      q1user        EAIUSR
     c                   time                    DSDTTM
     c                   eval      EAIDT  = dtCYMD(dsdate)
      *
      * The deal on b#a -batch assignment #'s:
      *    autotran_N = *on  b#a(1)(2) non transfer billing/payroll
      *    autotran_Y = *on  b#a(3)(4) transfer billing/payroll
      *
      *    autotran_N = *off b#a( )( ) non transfer billing/payroll
      *    autotran_Y = *on  b#a(1)(2) transfer billing/payroll
      *
      *    autotran_N = *on  b#a(1)(2) non transfer billing/payroll
      *    autotran_Y = *off b#a( )( ) transfer billing/payroll
      *
     c                   z-add     EABAT#        b#a(z)
     c***  Begin Add  ***  11/19/09  **************************************
     C                   eval      EAINV# = GetInv#
     c***  End   Add  ***  11/19/09  **************************************
      *
      *** beg chg ***** 09/01/06 *************************************
     c***********        write     pea01
     c                   write     dea0p
      *** end chg ***** 09/01/06 *************************************
      *
     c                   enddo                                                  otherEdit
      *
     c                   endsr                                                  otherEdit
      *** end add *** 10/20/05 *******************************************  ***********************
      *=============================================================================================
      * otherEdit subroutine - prints bad record type to error report
      *=============================================================================================
     c     otherEdit     begsr

     c                   eval      errorFlag = *on
     c                   move(p)   err(1)        errReason
     c                   exsr      printError

     c                   endsr                                                  otherEdit
      *=============================================================================================
      * trailerEdit subroutine - prints bad record type to error report
      *=============================================================================================
     c     trailerEdit   begsr
      *
      * Ensure only one trailer record exists in file.
      *
     c                   if        trailerLast = *on
     c                   eval      errorFlag = *on
     c                   move(p)   err(16)       errReason
     c                   exsr      printError
     c                   endif
      *
     c                   if        trailerLast = *off
      *
      * Ensure that trailer totals equal the accumulated totals.
      *
     c                   if        accumPostAmt <> tpost_amt_tt
     c                   eval      errorFlag = *on
     c                   move(p)   err(10)       errReason
     c                   exsr      printError
     c                   endif
      *
     c                   if        accumCommAmt <> tcomm_amt_tt
     c                   eval      errorFlag = *on
     c                   move(p)   err(10)       errReason
     c                   exsr      printError
     c                   endif
      *
     c                   if        accumNetAmt  <> tnet_amt_tot
     c                   eval      errorFlag = *on
     c                   move(p)   err(10)       errReason
     c                   exsr      printError
     c                   endif
      *
      * Ensure that trailer record count is correct.
      *
     c                   if        trec_cnt <> recCount
     c                   eval      errorFlag = *on
     c                   move(p)   err(11)       errReason
     c                   exsr      printError
     c                   endif

     c                   endif                                                  trailerLast = *off

     c                   endsr                                                  trailerEdit
      *=============================================================================================
      * printError subroutine - prints record to error report - also used to print "No Error Found"
      *=============================================================================================
     c     printError    begsr

     c                   if        firstError = *on

     c                   eval      firstError = *off
     c                   eval      *inoa = *on
     *
     * Get user name if applicable.
     *
     c     @@que         comp      *blank                             090908
    c*
     C                   eval      xxque    = @@que
     c                   eval      userName = getUsrName(q1user)
     c                   eval      UPUSNM   = userName
     c                   move      *all'_'       line
     *
     * Get the distribution information for the error report.
     *
     c                   clear                   ORTITL

     c                   eval      xxfil#   = '0'
     c     orky          chain     por01                              60

     c                   if        ORTITL = *blank
     c                   eval      ORTITL = title
     c                   endif

     c                   eval      ORTITL = leftJstify(ORTITL)

     c                   if        not *in60
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   endif

     c                   eval      ORTITL = centerFld(ORTITL)

     c                   endif                                                  firstError = *on

     c                   if        errorFlag = *on and badDATA = *off

     c                   if        *inoa = *on
     c                   eval      *inoa = *off
     c                   except    head
     c                   endif

     c                   select

     c                   when      recType = 'H'
     c                   eval      rec_type = 'Header'
     c                   eval      tr_post_date = htransMth + '/' + htransDay +
     c                             '/' + htransYear

     c                   when      recType = 'D'

     c                   eval      rec_type = 'Detail'
     c                   eval      policy# = dpolicy#
     c                   move      dfnd          grp#
     c                   move      demp#         emp#
     c                   move      ddiv          div
     c                   move      dfyr          fyr
     c                   eval      tr_post_date = dpost_mth + '/' + dpost_day +
     c                             '/' + dpost_year
     c                   move      dpost_amt     post_amt
     c                   move      dcomm_amt     comm_amt
     c                   move      dnet_amt      net_amt

     c                   when      recType = 'T'

     c                   eval      rec_type = 'Trailer'
     c                   move      tpost_amt_tt  post_amt
     c                   move      tcomm_amt_tt  comm_amt
     c                   move      tnet_amt_tot  net_amt

     c                   other

     c                   eval      rec_type = 'Invalid'

     c                   endsl

     c                   eval      errorFlag = *off
     c                   eval      errCount = errCount + 1
     c                   except    det

     c                   endif                                                  errorFlag = *on

     c                   if        errorFlag = *on and badDATA = *on

     c                   if        *inoa = *on
     c                   eval      *inoa = *off
     c                   except    head
     c                   endif

     c                   select

     c                   when      recType = 'H'
     c                   eval      rec_type = 'Header'
     c                   eval      tr_post_date = chtransMth + '/' + chtransDay
     c                             + '/' + chtransYear


     c                   when      recType = 'D'

     c                   eval      rec_type = 'Detail'
     c                   eval      policy# = cdpolicy#
     c                   eval      grp# = cdfnd
     c                   eval      emp# = cdemp#
     c                   eval      div  = cddiv
     c                   eval      fyr  = cdfyr
     c                   eval      tr_post_date = cdpost_mth + '/' + cdpost_day
     c                             + '/' + cdpost_year
     c                   move      cdpost_amt    post_amt
     c                   move      cdcomm_amt    comm_amt
     c                   move      cdnet_amt     net_amt

     c                   when      recType = 'T'

     c                   eval      rec_type = 'Trailer'
     c                   eval      post_amt = ctpost_amt_tt
     c                   eval      comm_amt = ctcomm_amt_tt
     c                   eval      net_amt  = ctnet_amt_tot

     c                   other

     c                   eval      rec_type = 'Invalid'

     c                   endsl

     c                   eval      errorFlag = *off
     c                   eval      errCount = errCount + 1
     c                   except    det

     c                   endif                                                  errorFlag = *on

     c                   if        errCount = *zero
     c                   eval      *in20 = *on                                  print "no error"
     c                   except    head
     c                   except    det
     c                   endif

     c                   if        *in21 = *on                                  DATA = *blanks
     c                   except    head
     c                   except    det
     c                   endif

     c                   if        *in22 = *on                                  Trailer or header

     c                   if        *inoa = *on
     c                   eval      *inoa = *off
     c                   except    head
     c                   endif

     c                   except    det
     c                   endif

     c                   endsr                                                  printError
      *=============================================================================================
      * printUpload - prints successful upload report
      *=============================================================================================
     c     printUpload   begsr
     *
     * Get user name if applicable.
     *
     c     @@que         comp      *blank                             090908
    c*
     C                   eval      xxque    = @@que
     c                   eval      userName = getUsrName(q1user)
     c                   eval      UPUSNM   = userName
     c                   move      *all'_'       line1
     *
     * Get the distribution information for the upload report.
     *
     c                   clear                   ORTITL

      *** beg chg **** 09/11/03 *******************************************************************
     c****************** eval      xxfil#   = '0'
     c                   eval      xxfil#   = '1'
      *** end chg **** 09/11/03 *******************************************************************
     c     orky          chain     por01                              61

     c                   if        ORTITL = *blank
     c                   eval      ORTITL1 = title1
     c                   else
     c                   eval      ORTITL1 = ORTITL
     c                   endif

     C                   eval      ORTITL1 = leftJstify(ORTITL1)

     c                   if        not *in61
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route1
      ***** End Del ***** 08/26/14 ************************************
     c                   endif

     c                   eval      ORTITL1 = centerFld(ORTITL1)
     c                   except    head1
     c                   except    det1

     c                   endsr                                                  printUpload
      *=============================================================================================
      * processWXUPP - processes the flat file.
      *=============================================================================================
     c     processWXUPP  begsr

     c     1             setll     pup01
     c                   read      pup01                                  60

     c                   dow       *in60 = *off

     c     1             subst     DATA          recType

     c                   if        recType = 'D'

     c                   clear                   detail
     c                   movel     DATA          detail
      *
      * Write premium amounts to A/R
      *
     c                   if        dpolicy# <> *blanks
     c                   eval      kkcap# = dpolicy#
     c     rpky          chain     prp01
     c                   eval      kkfnd  = RPFND
     c                   eval      kkemp  = RPEMP#
     c                   eval      kkdiv  = ddiv
     c                   eval      kkfyr  = RPFYR
     c     *iso          move      dpost_date    isoPostDate
     c     *cymd         move      isoPostDate   x7PostDate
     c                   else
     c                   eval      kkfnd  = dfnd
     c                   eval      kkemp  = demp#
     c                   eval      kkdiv  = ddiv
     c                   eval      kkfyr  = dfyr
     c     *iso          move      dpost_date    isoPostDate
     c     *cymd         move      isoPostDate   x7PostDate
     c                   endif

     c                   eval      commPlan = getEmpPlan(kkco#:kkfnd:kkemp:
     c                                        kkdiv:kkfyr:x7PostDate)

     c     elky          chain     pel01
      *** beg chg ***** 01/09/04 *******************************************************************
     c*****************  eval      kkvdr# = ELVDR#
     c     vhky1         setgt     pvh01
     c     elky          readpe    pvh01
     c                   if        not %eof

     c                   select
     c                   when      vhexdt = 0
     c                   eval      kkvdr# = VHVDR#
     c                   when      vhexdt <> 0 and vhexdt > x7PostDate
     c                   eval      kkvdr# = VHVDR#
     c                   other
     c                   eval      kkvdr# = 0
     c                   endsl

     c                   else
     c                   eval      kkvdr# = 0
     c                   endif
      *** end chg ***** 01/09/04 *******************************************************************

     c     vpky          chain     pvp01

     c                   exsr      writeAR

     c                   endif                                                  recType = D

     c                   read      pup01                                  60
     c                   enddo

     c                   endsr                                                  processWXUPP
      *=============================================================================================
      * writeAR - writes WMEAP
      *=============================================================================================
     c     writeAR       begsr                                                  writeAR
      *** beg chg ***** 09/01/06 *************************************
     c***********        clear                   pea01
     c                   clear                   dea0p
      *** end chg ***** 09/01/06 *************************************
     c                   eval      EACO#  = kkco#                               EACO#
     c                   eval      EAFND  = kkfnd                               EAFND
     c                   eval      EAFYR  = kkfyr                               EAFYR
     c                   eval      EAEMP# = kkemp                               EAEMP#
     c                   eval      EADIV  = kkdiv                               EADIV
      *** beg add ***** 04/08/05 *******************************************************************
     c                   eval      EARDIV = kkdiv                               EADIV
      *** beg add ***** 06/20/06 *******************************************************************
     c                   eval      EASYCD = xxsycd
      *** end add ***** 06/20/06 *******************************************************************
      *** beg add ***** 03/07/07 *******************************************************************
     c                   if        dneg_amt_flg = '-'
     c                   eval      dpost_amt = dpost_amt * -1
     c                   endif
      *** end add ***** 03/07/07 *******************************************************************

     c     elky          chain     pel01
     c                   if        ELARSM = 'Y'

     c     emky          setll     pem01
     c     emky          reade     pem01
     c                   dow       not %eof
     c                   if        EMSTRP = 'Y'
     c                   eval      EADIV  = EMDIV
     c                   leave
     c                   endif
     c     emky          reade     pem01
     c                   enddo

     c                   endif                                                  ELARSM = 'Y'
      *** end add ***** 04/08/05 *******************************************************************

     c                   eval      EASTAT = 'A'                                 EASTAT
     c                   eval      EATRDT = x7transDate                         EATRDT
     c                   eval      EAPODT = dtCYMD(x7postDate)                  EAPODT
     c                   eval      EAUSER = Q1USER
     c                   eval      EAPGM  = Q1PGM
     c                   time                    dsdttm
     c                   eval      EADT   = dtCYMD(dsdate)
      *** add del *** 10/20/05 *******************************************
     c                   move      q1pgm         EAIPGM
     c                   move      q1user        EAIUSR
     c                   time                    DSDTTM
     c                   eval      EAIDT  = dtCYMD(dsdate)
      *** add del *** 10/20/05 *******************************************
     c
      *** beg chg **** 06/24/02 *******************************************************************
     c****************** if        commPlan = *blank
     c                   if        commPlan <> 'A'
      *** end chg **** 06/24/02 *******************************************************************
     c                   eval      EAARCD = VPPPCD
     c                   eval      kkkey  = VPPPCD
     c     ttky          chain     ptt01
      *** beg del *** 10/20/05 *******************************************
     c*******            eval      EAARTY = TTTYPE
      *** end del *** 10/20/05 *******************************************
     c                   eval      EADESC = TTDESC
      *** beg add ***** 11/04/03 ***********************************************
     c                   eval      EACTYP = TTCTYP
      *** end add ***** 11/04/03 ***********************************************
     c                   move(p)   dpost_amt     EAAMT                          EAAMT
     c                   eval      dstime = dstime + 1
     c                   eval      EATIME = dstime

      *** beg add *** 10/20/05 *******************************************  ***********************
     c                   eval      EACKAM = EAAMT
     c                   move      '1'           rcdtype           1
     c                   exsr      asignBatchNum
      *** end add *** 10/20/05 *******************************************  ***********************
     c***  Begin Add  ***  11/19/09  **************************************
     C                   eval      EAINV# = GetInv#
     c***  End   Add  ***  11/19/09  **************************************
      *** beg chg ***** 09/01/06 *************************************
     c***********        write     pea01
     c                   write     dea0p
      *** end chg ***** 09/01/06 *************************************

     c                   eval      EAARCD = VPPBCD
     c                   eval      kkkey  = VPPBCD
     c     ttky          chain     ptt01
      *** beg del *** 10/20/05 *******************************************
     c*******            eval      EAARTY = TTTYPE
      *** end del *** 10/20/05 *******************************************
     c                   eval      EADESC = TTDESC                              EADESC
      *** beg add ***** 11/04/03 ***********************************************
     c                   eval      EACTYP = TTCTYP
      *** end add ***** 11/04/03 ***********************************************
     c                   move(p)   dpost_amt     EAAMT                          EAAMT
     c                   eval      dstime = dstime + 1
     c                   eval      EATIME = dstime
      *** beg add *** 10/20/05 *******************************************  ***********************
     c                   eval      EACKAM = EAAMT
     c                   move      '2'           rcdtype           1
     c                   exsr      asignBatchNum
      *** end add *** 10/20/05 *******************************************  ***********************
     c***  Begin Add  ***  11/19/09  **************************************
     C                   eval      EAINV# = GetInv#
     c***  End   Add  ***  11/19/09  **************************************
      *** beg chg ***** 09/01/06 *************************************
     c***********        write     pea01
     c                   write     dea0p
      *** end chg ***** 09/01/06 *************************************
     c                   endif
     c
     c                   if        commPlan = 'A'                               EAARCD
     c                   eval      EAARCD = VPCPCD
     c                   eval      kkkey  = VPCPCD
     c     ttky          chain     ptt01
      *** beg del *** 10/20/05 *******************************************
     c*******            eval      EAARTY = TTTYPE
      *** end del *** 10/20/05 *******************************************
     c                   eval      EADESC = TTDESC                              EADESC
      *** beg add ***** 11/04/03 ***********************************************
     c                   eval      EACTYP = TTCTYP
      *** end add ***** 11/04/03 ***********************************************
     c                   eval      EAAMT  = dcomm_amt                           EAAMT
     c                   eval      dstime = dstime + 1
     c                   eval      EATIME = dstime
      *** beg add *** 10/20/05 *******************************************  ***********************
     c                   eval      EACKAM = EAAMT
     c                   move      '1'           rcdtype           1
     c                   exsr      asignBatchNum
      *** end add *** 10/20/05 *******************************************  ***********************
     c***  Begin Add  ***  11/19/09  **************************************
     C                   eval      EAINV# = GetInv#
     c***  End   Add  ***  11/19/09  **************************************
      *** beg chg ***** 09/01/06 *************************************
     c***********        write     pea01
     c                   write     dea0p
      *** end chg ***** 09/01/06 *************************************
     c                   eval      EAARCD = VPPPCD
     c                   eval      kkkey  = VPPPCD
     c     ttky          chain     ptt01
      *** beg del *** 10/20/05 *******************************************
     c*******            eval      EAARTY = TTTYPE
      *** end del *** 10/20/05 *******************************************
     c                   eval      EADESC = TTDESC                              EADESC
      *** beg add ***** 11/04/03 ***********************************************
     c                   eval      EACTYP = TTCTYP
      *** end add ***** 11/04/03 ***********************************************
     c                   move(p)   dnet_amt      EAAMT                          EAAMT
     c                   eval      dstime = dstime + 1
     c                   eval      EATIME = dstime
      *** beg add *** 10/20/05 *******************************************  ***********************
     c                   eval      EACKAM = EAAMT
     c                   move      '2'           rcdtype           1
     c                   exsr      asignBatchNum
      *** end add *** 10/20/05 *******************************************  ***********************
     c***  Begin Add  ***  11/19/09  **************************************
     C                   eval      EAINV# = GetInv#
     c***  End   Add  ***  11/19/09  **************************************
      *** beg chg ***** 09/01/06 *************************************
     c***********        write     pea01
     c                   write     dea0p
      *** end chg ***** 09/01/06 *************************************
     c                   endif

     c                   endsr                                                  writeAR
      *=============================================================================================
      * Output files
      *=============================================================================================
     o/copy scopyrout

     oqsysprt   e            route          1                                   error report

     o*** Beg Chg *****  01/28/10  ************************************
     o***       e            route          1 60
     o***                                           '(C) Copyright Progressive'
     o***                                     +   1 'Data Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     o*** End Chg *****  01/28/10  ************************************
    o*
    o* Report Heading
    o*
     o          e            head           1 06
     o                       sys_time
     o                       companyNm          135
     o                       sys_date           201
     o          e            head           2
     o                       q1pgm
     o                       ORTITL             135
     o                                          195 'PAGE:'
     o                       page          z    201
     o          e            head           0
     o                                            8 'Rec type'
     o                                           15 'Policy'
     o                                           38 'Grp'
     o                                           48 'Emp#'
     o                                           54 'Div'
     o                                           61 'Fyr'
     o                                           73 'Tr/Post Dt'
     o                                           97 'Post Amount'
     o                                          113 'Commission'
     o                                          137 'Net Amount'
     o                                          151 'Error reason'
     o          e            head           2
     o                       line               199
     o          e            det            1
     o              n20n21n22rec_type            07
     o              n20n21n22policy#             34
     o              n20n21n22grp#                38
     o              n20n21n22emp#                48
     o              n20n21n22div                 54
     o              n20n21n22fyr                 61
     o              n20n21n22tr_post_date        73
     o              n20n21n22post_amt            97
     o              n20n21n22comm_amt           113
     o              n20n21n22net_amt            137
     o              n20n21n22errReason          199
     o               20                         113 '*** No errors to report. *-
     o                                              **'
     o               21                         115 '*** No records exist in WX-
     o                                              UPP ***'
     o               22                         116 '*** Header or trailer is m-
     o                                              issing ***'
     o/copy scopyrout1

     oqsysprt1  e            route1         1                                   successful upload

     o*** BEg Chg *****  01/28/10  ************************************
     o          e            route1         1 60
     o***                                           '(C) Copyright Progressive'
     o***                                     +   1 'Data Solutions, Inc.,'
     O          e            route1         1 59
     O                       #brnd1
     O          e            route1         1
     O                       #brnd2
     o*** End Chg *****  01/28/10  ************************************
    o*
    o* Report Heading
    o*
     o          e            head1          1 06
     o                       sys_time
     o                       companyNm           88
     o                       sys_date           105
     o          e            head1          2
     o                       q1pgm
     o                       ORTITL1             88
     o                                          100 'PAGE:'
     o                       page1         z    105
     o          e            head1          0
     o                                           20 'Record Count'
     o                                           50 'Post Amount'
     o                                           70 'Commission'
     o                                          100 'Net Amount'
     o          e            head1          2
     o                       line1              105
     o          e            det1           1
     o                       trec_cnt      4     20
     o                       tpost_amt_tt  4     50
     o                       tcomm_amt_tt  4     70
     o                       tnet_amt_tot  4    100
     c***  Begin Add  ***  11/19/09  **************************************
     *=============================================================================================
      * Get the invoice # for billing records being created
     *=============================================================================================
     p GetInv#         b
     d GetInv#         pi                  like(EAINV#)

     d invoice#        s                   like(EAINV#) inz

     C                   select
     C                   when      cltID = '025'
     C                   if        BillType(GetARType(EAARCD))
     C                   eval      invoice# = RetrieveInv#(EACO#
     C                                                    :EAFND
     C                                                    :EAEMP#
     C                                                    :EADIV
     C                                                    :EAFYR
     C                                                    :CvtToDate(EAPODT)
     C                                                    )
     C                   endif
     C                   endsl

     C                   return    invoice#

     p GetInv#         e
     c***  End   Add  ***  11/19/09  **************************************

** Error messages
Record type is invalid. It must be a 'H','D', or 'T'.
Date is invalid.
Carrier policy number is not valid.
Report date doesn't match report date in header.
Direct billed policy has invalid posting (gross) amount.
Required information is missing.
Detail record read before the header record.
Agency billed policy has invalid commission amount.
Detail record read after trailer record.
Trailer total amount is incorrect.
Trailer record count doesn't match number of records read.
Agency billed policy has invalid net amount.
Policy # isn't assigned to this policy.
Trailer information doesn't match header information.
Numeric fields have character information (bad data).
Duplicate header or trailer records not allowed in file.
Policy year doesn't exist in employer detail.
A/R codes don't exist for direct bill type policy.
A/R codes don't exist for agency bill type policy.
Transaction date doesn't belong to current month.
Invalid vendor number.
Posting date not in policy date range.                                                    07/28/05
NOT USED
NOT USED
NOT USED
NOT USED
NOT USED
NOT USED
NOT USED
NOT USED
