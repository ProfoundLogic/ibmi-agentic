     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
    H*********************************************************************
    H/TITLE W7383R - A/R DETAIL REPORT
    H*********************************************************************
    H*  DESCRIPTION:  This program sorts and totals all A/R Transactions *
    H*                based on the selection criteria entered on the     *
    H*                prompt screen.                                     *
    H*                                                                   *
    H*  CALLED FROM:                                                     *
    H*    W7381C                                                         *
    H*  CALLS TO:                                                        *
    H*    NONE                                                           *
    H*                                                                   *
    H*  REWRITE: 08/01/00    C:33    PROJECT: WC804                      *
    H*********************************************************************
    H* REVISIONS:
     H*
     H* 11/06/23 C:291 Project: WC2934
     H*                Populate Batch Type and Batch Type Description fields
     H*                to Report output file.
     H*
     H* 12/17/21 C:079 Support: 140/99805
     H*                Fix condition for group totals
     H*
     H* 01/08/21 C:298 Support: 91132
     H*                Totals should be totals for group/year
     H*                only
     H*
     H* 06/21/17 C:136 Support: 47921
     H*                Run report totals if year range is passed
     H*                in as zeros.
     H*
     H* 06/24/16 C:136 Project: SY371
     H*                Add Scriptura Phase 2 processing
     H*
     H* 08/05/15 C:307 Project: SY354
     H*                -Adding 'Route To:'
     H*
     H* 07/03/15 C:307 Support: 35698
     H*                -A/R Detail Report Routing Sheet                   *
     H*                                                                   *
     H* 08/26/14 C:298 Project: SY354                                     *
     H*                -Implement Routing Page Changes.                   *
     H*                                                                   *
     H* 05/28/14  C:279  Project: 129WSV                                  *
     H*                  Modified to use Group Select process.            *
     H*
     H* 09/09/10  C:257  Support: 73822                                   *
     H*                  Fix for Employee restriction: Not to print all   *
     H*                  employee report when user is restricted to       *
     H*                  one employee.                                    *
     H*
     H* 01/28/10  C:249  Project: SY215                                   *
     H*           C:189  Retrieve brand Name                              *
     H*                                                                   *
    H* 11/19/09 C:33  Project: 25WQI                                     *
     H*               -Print 'Statement #' along with the invoice #       *
      *
    H* 01/04/10 C:190 Project: 25WRE                                     *
     H*                Add district# label                                *
    H*                                                                   *
    H* 06/23/09 C:190 Support: 68713                                     *
    H*                Corrected A/R detail report to use Alternate       *
    H*                Company Name in place of company name if entered   *
    H*                                                                   *
      * 07/07/08  C:61  Project: 132WKR
      *                 Write new fields to output file WFARP.
      *                   ARBSCD - Billing schedule code
      *                   ARBDSC - Bill Sched Code Description
      *                   ARSEFL - Billing Sched Code Type
      *                   ARABFL - Agency Bill Flag
    H*                                                                   *
    H* 02/17/05 C:136 Support:                                           *
    H*                Added code to recognize 3rd, 4th, and 5th          *
    H*                assessments as a surcharge in WFARP.               *
    H*                Added code to add category 'T' amounts to          *
    H*                WFARP.                                             *
    H*                                                                   *
    H* 12/07/04 C:136 Project: 129WHU                                    *
    H*                Print new billing type parm to routing page.       *
    H*                                                                   *
    H* 02/19/04 C:136 Project: WC1033 (Return Premium)                   *
    H*                Add processing for EAARTY = 'T' (true void)        *
    H*                transaction.                                       *
    H*                                                                   *
    H* 07/16/02 C:136 Support: WA048 33364                               *
    H*                Bucket totals in Group Summary are incorrect.  The *
    H*                program is picking up the first employer's AR      *
    H*                totals from the next Group before printing the     *
    H*                Group Summary during a group break or year break.  *
    H*                Also corrected errors in the routing information   *
    H*                and the constant Heading1.                         *
    H*                                                                   *
    H* 06/18/02 C:136 Support: WA048 Seq: 33005                          *
    H*                Routing will not display the division.             *
    H*                                                                   *
    H* 04/15/02  C:35   PROJECT: WC970                                   *
    H*                  Add Category Type to the output file.            *
    H*                                                                   *
    H*  02/19/02 C:90   Support# 31285                                   *
    H*           Accumulate premium across all divisions with "Summarize *
    H*           A/R" flag set to "Y".                                   *
    H*                                                                   *
    H*  01/15/02 C:136  Support# 30550                                   *
    H*           Headings do not print on first page of report when      *
    H*           routing isn't set up                                    *
    H*                                                                   *
    H*********************************************************************
     Fsmorl     if   e           k disk
     Fwxeal1    if   e           k disk
     Fwxeal     if   e           k disk    rename(pea01:pea00) prefix(l0_)
     Fwdell1    if   e           k disk
     Fwdf1l     if   e           k disk    rename(pf101:pf100)
      **** begin add **** 02/19/02 *****************************************
     Fwmeml     if   e           k disk
      **** end   add **** 02/19/02 *****************************************
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwdelp     if   e             disk    rename(pel01:pel0p)
     Fwmahp     if   e             disk    rename(pah01:pah0p)
     Fwfarp     o    e             disk    rename(par01:par0p)
      ******* beg add ****** 07/07/08 *********************************
     Fwtbsp     if   e             disk
      ******* end add ****** 07/07/08 *********************************
      ******* beg add ****** 09/09/10 *********************************
     Fwmupl     if   e           k disk
      ******* end add ****** 09/09/10 *********************************
      *** beg add ***** 06/22/16 ******************
     fwxarp1    o    e             disk    usropn
     fwxarp2    o    e             disk    usropn
     fwxarp3    o    e             disk    usropn
     fwxarp3d   o    e             disk    usropn rename(par01:pxarp3d)
     fwxarpac   o    e             disk    usropn
      *** end add ***** 06/22/16 ******************
     Fqsysprt   o    f  180        printer oflind(*inof) usropn
     Fqsysprt1  o    f  180        printer oflind(*inoa) usropn

    D*
    D* Define Sort Selection Compile time Array
    D*
     D sort            s              1    dim(3) ctdata perrcd(1)
     D sortDesc        s             25    dim(3) alt(sort)
     D plan            s              1    dim(3) ctdata perrcd(1)
     D planDesc        s             25    dim(3) alt(plan)
     D rpt             s              1    dim(3) ctdata perrcd(1)
     D rptDesc         s             25    dim(3) alt(rpt)
     D ARCode          s              1    dim(4) ctdata perrcd(1)
     D ARCodeDesc      s             50    dim(4) alt(ARCode)
    D*
    D* Define A/R Type Summary Arrays
    D*
     D numOfElmts      c                   999
     D codes           s              2    dim(numOfElmts)
     D amounts         s             15  2 dim(numOfElmts)
     D codesTtl        s              2    dim(numOfElmts)
     D amountsTtl      s             15  2 dim(numOfElmts)
     D codes_amts      s             17    dim(numOfElmts) ascend
    D*
    D* Combination Array
     D code_amt        ds
     D  xCode                         2
     D  xAmount                      15  2
    D*
    D* Define arrays for A/R selection criteria
     D #ofCodes        c                   30
     D #ofCats         c                   10
     D code            s              2    dim(#ofCodes) ascend
     D codeDesc        s             35    dim(#ofCodes)
     D category        s              1    dim(#ofCats) ascend
     D ctgryDesc       s             25    dim(#ofCats)
    D*
     D psds          esds                  extname(szq1p)

     D/copy sprcompany
     D/copy sprdate
     D/copy sprstring

     D/copy wprusrprms
     D/copy wprgroup
     D/copy wpremplr
     D/copy wpragent
     D/copy wprproduce
     D/copy wprAR
     D/copy wprrateset
     D/copy wprregion
     D/copy wprchapter
     D/copy wproffice
     D/copy wprpremfin

     D chkEmpAct       PR             1

     DrmvZeros         PR         32766    opdesc
     D fieldToRmv                 32766    options(*varsize) const

    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D @@co#           s              3
     D @@fnd_fr        s              3
     D @@fnd_to        s              3
     D @@fyr_fr        s              3
     D @@fyr_to        s              3
     D @@ARdt_fr       s              8
     D @@ARdt_to       s              8
     D @@EmpNoAct      s              1
     D @@sort          s              1
     D @@agt#          s              5
     D @@emp#          s              9
     D @@div           s              5
     D @@CommPlan      s              1
     D @@ctyp_all      s             10
     D @@ARCode        s              1
     D @@arcd_all      s             60
     D @@report        s              1
     D @@output        s              1
     D @@que           s             10
     D companyNm       s             60
     D groupName       s             60
     D emplrName       s             40
      *** beg add **** 01/04/10 *******************************************************************
     D prtEmpNa25      s             27
     D prtEmpName      s             40
     D*********** prtEmpName      s             40
      *** end add **** 01/04/10 *******************************************************************
     D prtEmpNam1      s             18
     D agentName       s             50
     D prtAgtName      s             40
     D colEmp          s             11  2
     D billEmp         s             11  2
     D othrEmp         s             11  2
     D feesDue         s             11  2
     D depositDue      s             11  2
     D premiumDue      s             11  2
     D totalDue        s             13  2
     D ttlColEmp       s             13  2
     D ttlOthrEmp      s             13  2
     D ttlBillEmp      s             13  2
     D currentBal      s             13  2
     D fndANPR         s             13  2
     D fndTotal        s             13  2
     D prvBal          s             13  2
     D prvAdjGrp       s             13  2
     D prvBillGrp      s             13  2
     D prvAdtGrp       s             13  2
     D prvNSFGrp       s             13  2
     D prvPmntGrp      s             13  2
     D prvVoidGrp      s             13  2
     D prvRfndGrp      s             13  2
     D prvDepBGrp      s             13  2
     D prvDepPGrp      s             13  2
     D prvFeeBGrp      s             13  2
     D prvFeePGrp      s             13  2
     D prvBalGrp       s             13  2
     D adjGrp          s             13  2
     D billGrp         s             13  2
     D depGrp          s             13  2
     D NSFGrp          s             13  2
     D pmntGrp         s             13  2
     D voidGrp         s             13  2
     D rfndGrp         s             13  2
     D depBGrp         s             13  2
     D depPGrp         s             13  2
     D feeBGrp         s             13  2
     D feePGrp         s             13  2
     D ttlColGrp       s             13  2
     D ttlOthrGrp      s             13  2
     D ttlBillGrp      s             13  2
     D ttlANPR         s             15  2 inz(*zero)
     D total           s             15  2 inz(*zero)
     D prvAdjTtl       s             15  2 inz(*zero)
     D prvBillTtl      s             15  2 inz(*zero)
     D prvAdtTtl       s             15  2 inz(*zero)
     D prvNSFTtl       s             15  2 inz(*zero)
     D prvPmntTtl      s             15  2 inz(*zero)
     D prvVoidTtl      s             15  2 inz(*zero)
     D prvRfndTtl      s             15  2 inz(*zero)
     D prvDepBTtl      s             15  2 inz(*zero)
     D prvDepPTtl      s             15  2 inz(*zero)
     D prvFeeBTtl      s             15  2 inz(*zero)
     D prvFeePTtl      s             15  2 inz(*zero)
     D prvBalTtl       s             15  2 inz(*zero)
     D adjTtl          s             15  2 inz(*zero)
     D billTtl         s             15  2 inz(*zero)
     D depTtl          s             15  2 inz(*zero)
     D NSFTtl          s             15  2 inz(*zero)
     D pmntTtl         s             15  2 inz(*zero)
     D voidTtl         s             15  2 inz(*zero)
     D rfndTtl         s             15  2 inz(*zero)
     D depBTtl         s             15  2 inz(*zero)
     D depPTtl         s             15  2 inz(*zero)
     D feeBTtl         s             15  2 inz(*zero)
     D feePTtl         s             15  2 inz(*zero)
      *** beg add ***** 02/19/04 ***********************************************
     D prvTrVdGrp      s             13  2 inz(*zero)
     D prvTrVdTtl      s             15  2 inz(*zero)
     D trVdGrp         s             13  2 inz(*zero)
     D trVdTtl         s             15  2 inz(*zero)
      *** end add ***** 02/19/04 ***********************************************
     D userName        s             50
     D routeCo#        s              3
      ******* beg del ******* 05/28/14 ****************************
     D*routeGrp******  s             10
      ******* end del ******* 05/28/14 ****************************
     D routeYr         s             10
     D routeNoAct      s              3
     D routeSort       s             25
     D routeAgt#       s              5
     D routeEmpDv      s             15
     D routePlan       s             25
     D routeRpt        s             25
     D routeAR         s             50
     D summaryTtl      s             13  2
     D empAnnPrm       s             11  2
     D ELPFDT_mdy      s               d   datfmt(*mdy/)
     D ELPTDT_mdy      s               d   datfmt(*mdy/)
     D cancelDt        s               d   datfmt(*mdy/)
     D cxDate          s                   like(EMCAD1)
     D xCodeDesc       s             30
     D xCodeType       s              1
     D xxamt           s             11  2
     D xxco#           s                   like(EACO#)
     D x7ARdt_fr       s                   like(EAxxDT)
     D x7ARdt_to       s                   like(EAxxDT)
     D EAPODT_usa      s               d   datfmt(*usa)
     D EATRDT_usa      s               d   datfmt(*usa)
     D frdt_usa        s               d   datfmt(*usa)
     D todt_usa        s               d   datfmt(*usa)
     D sys_date        s               d   datfmt(*usa)
     D sys_time        s               t   timfmt(*hms:)
     D saveGrp         s                   like(EAFND)  inz(*zero)
     D saveYr          s                   like(EAFYR)  inz(*zero)
     D saveAgt#        s                   like(EAAGT#) inz(*zero)
     D saveEmp#        s                   like(EAEMP#) inz(*zero)
     D saveDiv         s                   like(EADIV)  inz(*zero)
      *** beg add **** 01/04/10 *******************************************************************
     D savdst#         s                   like(EMSEMP)  inz(*blanks)
      *** end add **** 01/04/10 *******************************************************************
     D summaryRpt      s                   like(*in60)
     D detailRpt       s                   like(*in60)
     D agentSort       s                   like(*in60)
     D printRpt        s                   like(*in60)
     D loadFile        s                   like(*in60)
     D empAct          s                   like(*in60)
     D prtEmpTtls      s                   like(*in60)
     D prtEmpInfo      s              1    inz(*off)
     D prtPrvBal       s              1    inz(*off)
     D firstRcrd       s              1    inz(*on)
     D oneRcrd         s              1    inz(*off)
      *** beg del **** 07/16/02 *******************************************************************
     D************ oneGrpRcrd      s              1    inz(*off)
      *** end del **** 07/16/02 *******************************************************************
     D groupRun        s              1    inz(*on)
     D groupBreak      s              1    inz(*off)
      *** beg add **** 01/15/02 ********************************************************************
     D firstRead       s              1    inz(*on)
     D firstRead1      s              1    inz(*on)
      *** end add **** 01/15/02 ********************************************************************
     D xxfil#          s                   like(ORFIL#)
     D UPUSNM          s                   like(userName)
     D xxque           s                   like(@@que)
     D EACO#_a         s              3
     D EAFND_a         s              3
     D EAEMP#_a        s              9
     D EADIV_a         s              5
     D EAFYR_a         s              3
     D EATRDT_a        s              7
     D EATIME_a        s              6
     D ARCatType       s              1
     D agtMod          s              3  0
     D FNrrn           s              9  0
     D AArrn           s              9  0
     D AGrrn           s              9  0
     D AHrrn           s              9  0
     D EMrrn           s              9  0
     D ELrrn           s              9  0
      ******* beg add ****** 07/07/08 *********************************
     D BSrrn           s              9  0
     D xx50C           s             50
      ******* end add ****** 07/07/08 *********************************
     D TTrrn           s              9  0
     D rrn             s              9  0
     D x1              s              5  0
     D v               s              5  0
     D x               s              1  0
     D y               s              3  0
     D z               s              3  0
     D nullDate        s               d   inz
     D ORTITL1         s                   like(ORTITL)
      *** beg add **** 06/18/02 ********************************************************
     D xxemp           s              9
     D xxdiv           s              5
      *** end add **** 06/18/02 ********************************************************
      *** beg add **** 07/16/02 ********************************************************
      ***** beg del ****** 05/28/14 ****************************************************
     D*xxfnd_fr****    s              3
     D*xxfnd_to****    s              3
      ***** end del ****** 05/28/14 ****************************************************
     D xxfyr_fr        s              3
     D xxfyr_to        s              3
      *** end add **** 07/16/02 ********************************************************
      *** beg add **** 12/07/04 ********************************************************
     d billingtype     s             11a
      *** end add **** 12/07/04 ********************************************************
     D***  Begin Add  ***  11/19/09  **************************************
     d cltID           s              3a
     D***  End   Add  ***  11/19/09  **************************************
      *** beg add ***** 06/24/16 ******************************************
     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d outCo#          s              3  0 inz
     d outFnd          s              3  0 inz
     d outFldrPrefix   s              2    inz
     d outPreString    s             10    inz
     d outReturn       s            250    inz
     d outXMLFile      s             61    inz
     d processType     s            100    inz
     d prtGrpYrSumm    s               n
     d prtRptSummary   s               n
     d strCommand      s            255
     d xmlFile         s             60    inz

     d                 ds
     d xard                    1   1050    dim(#ofCodes)
     d  ARardsc1               1     35
     d  ARardsc2              36     70
     d  ARardsc3              71    105
     d  ARarcsc4             106    140
     d  ARardsc5             141    175
     d  ARardsc6             176    210
     d  ARardsc7             211    245
     d  ARardsc8             246    280
     d  ARardsc9             281    315
     d  ARardsc10            316    350
     d  ARardsc11            351    385
     d  ARardsc12            386    420
     d  ARardsc13            421    455
     d  ARardsc14            456    490
     d  ARardsc15            491    525
     d  ARardsc16            526    560
     d  ARardsc17            561    595
     d  ARardsc18            596    630
     d  ARardsc19            631    665
     d  ARardsc20            666    700
     d  ARardsc21            701    735
     d  ARardsc22            736    770
     d  ARardsc23            771    805
     d  ARardsc24            806    840
     d  ARardsc25            841    875
     d  ARardsc26            876    910
     d  ARardsc27            911    945
     d  ARardsc28            946    980
     d  ARardsc29            981   1015
     d  ARardsc30           1016   1050

     d                 ds
     d xct                     1     10    dim(#ofCats)
     d  ARctcde1               1      1
     d  ARctcde2               2      2
     d  ARctcde3               3      3
     d  ARctcde4               4      4
     d  ARctcde5               5      5
     d  ARctcde6               6      6
     d  ARctcde7               7      7
     d  ARctcde8               8      8
     d  ARctcde9               9      9
     d  ARctcde10             10     10

     d                 ds
     d xctd                    1    250    dim(#ofCats)
     d  ARctdsc1               1     25
     d  ARctdsc2              26     50
     d  ARctdsc3              51     75
     d  ARctcsc4              76    100
     d  ARctdsc5             101    125
     d  ARctdsc6             126    150
     d  ARctdsc7             151    175
     d  ARctdsc8             176    200
     d  ARctdsc9             201    225
     d  ARctdsc10            226    250

     d/copy sPrModule
     d/copy sPrXML
     d QCmdExc         pr                  extpgm('QCMDEXC')
     d  command                            const like(strCommand)
     d  length                       15  5 const
     d S4401C          pr                  extpgm('S4401C')
     d  xmlFile                      61
     d WrtWXARP1       pr                                                       XML Rpt Totals
     d WrtWXARP2       pr                                                       XML Grp Totals
     d WrtWXARP3       pr                                                       XML Emp Totals
     d WrtWXARPAC      pr                                                       XML A/R Summary
      *** end add ***** 06/24/16 ******************************************
      ****** beg add ****** 05/28/14 **************************************
     d isMultiGrp      s              1a   inz(' ')
      ****** end add ****** 05/28/14 **************************************
     D title           c                   'Employer A/R Detail Report'
     D title0          c                   '(Detail)'
     D title1          c                   '(Summary)'
     D Heading         c                   ' EMPLOYER   DIV NAME               -
     D                                                             -
     D                                     POLICY DATE RANGE      -
     D                                     ANNUAL PREM    AGENT AGENT NAME'
      **** begin add **** 01/04/10 *****************************************
     D Heading25       c                   ' EMPLOYER   DIV NAME               -
     D                                              DISTRICT#         -
     D                                     POLICY DATE RANGE      -
     D                                     ANNUAL PREM    AGENT AGENT NAME'
      **** end   chg **** 01/04/10 *****************************************
     D Heading1        c                   ' EMPLOYER   DIV NAME               -
     D                                      POLICY DATE RANGE  -
     D                                        ANNUAL PREM   -
     D                                      CX DATE -
     D                                      PREVIOUS BALANCE   -
     D                                           COLLECTIONS   -
     D                                           OTHER TRANS   -
     D                                              BILLINGS   -
     D                                               BALANCE'
     D solidLine       c                   '____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     __________'
     D***  Begin Add  ***  11/19/09  **************************************
     D solidLine025    c                   '____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     __________'
     D***  End   Add  ***  11/19/09  **************************************
     D solidLine1      c                   '____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________-
     D                                     ____________________'
     D cvtDate         c                   19000000
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************
    C*
     C     orky          klist
     C                   kfld                    q1pgm
     C                   kfld                    xxfil#
    C*
     C     eaky          klist
     C                   kfld                    EACO#
     C                   kfld                    EAFND
     C                   kfld                    EAFYR
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
    C*
     C     eaky_1        klist
     C                   kfld                    EACO#
     C                   kfld                    EAFND
     C                   kfld                    EAFYR
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
     C                   kfld                    x7ARdt_fr
    C*
      **** begin add **** 02/19/02 *****************************************
     C     elky          klist
     C                   kfld                    EACO#
     C                   kfld                    EAFND
     C                   kfld                    EAFYR
     C                   kfld                    EAEMP#
      *
     C     emky          klist
     C                   kfld                    EACO#
     C                   kfld                    EAFND
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
      **** end   add **** 02/19/02 *****************************************
    C*
     C     elky1         klist
     C                   kfld                    EACO#
     C                   kfld                    EAFND
     C                   kfld                    EAFYR
     C                   kfld                    EAEMP#
     C                   kfld                    EADIV
    C*
     C     f1ky          klist
     C                   kfld                    EACO#
     C                   kfld                    EAFND
     C                   kfld                    EAFYR
    
     C******* beg add ****** 09/09/10 *********************************
     C     upky          klist
     C                   kfld                    q1user
    C*
     C******* end add ****** 09/09/10 *********************************
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fnd_fr
     C                   parm                    @@fnd_to
     C                   parm                    @@fyr_fr
     C                   parm                    @@fyr_to
     C                   parm                    @@ARdt_fr
     C                   parm                    @@ARdt_to
     C                   parm                    @@EmpNoAct
     C                   parm                    @@sort
     C                   parm                    @@agt#
     C                   parm                    @@emp#
     C                   parm                    @@div
     C                   parm                    @@CommPlan
     C                   parm                    @@ctyp_all
     C                   parm                    @@ARCode
     C                   parm                    @@arcd_all
     C                   parm                    @@report
     C                   parm                    @@output
     C                   parm                    @@que
      *** beg add **** 12/07/04 ************************************************
     C                   parm                    @@btyp            1
      *** end add **** 12/07/04 ************************************************
      *** beg add **** 01/04/10 ************************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltid             3

     C     cltid         ifeq      '025'
     C                   move      *on           *in97
     C                   endif
      *** end add **** 01/04/10 ************************************************

     C                   exsr      Initialize
    C*
    C*  Begin Reading Work File
    C*
     C                   read      pea01                                  54
    C*
     C                   dow       *in54 = *off
    C*
     C                   eval      prtEmpTtls = *on
     C*
     C                   if        saveGrp <> EAFND
      /free
       //******* beg add ******* 05/28/14 ***********************************
       If saveGrp <> *zeros;
          isMultiGrp = 'Y';
       Endif;
       //******* end add ******* 05/28/14 ***********************************
      /end-free
     C                   exsr      groupBrk
     C                   endif
     C*
     C                   if        saveYr <> EAFYR
     C                   exsr      yrBrk
     C                   endif
     C*
     C                   if        saveAgt# <> EAAGT#
     C                   exsr      agentBrk
     C                   endif
     C*
     C                   if        saveEmp# <> EAEMP# or saveDiv <> EADIV
     C                   exsr      empBrk
     C                   endif
    C*
     C                   if        prvBal <> *zero or prtEmpInfo = *on
     C*
     C                   eval      oneRcrd    = *on
      *** beg del **** 07/16/02 *******************************************************************
     C****************** eval      oneGrpRcrd = *on
      *** end del **** 07/16/02 *******************************************************************
    C*
     C                   exsr      srCurrBal
    C*
     C                   endif
    C*
     C                   read      pea01                                  54
    C*
     C                   enddo
     C*
     C                   exsr      final
    C*
      /Free
       //***** Beg Del ***** 07/03/15 *************************************
       //***** Beg Add ***** 08/26/14 *************************************
       //If not %open(Qsysprt) and detailRpt = *on;
       //   Open qsysprt;
       //Endif;
       //xxFil# = '0';
       //Chain orKy POR01;
       //If %found;
       //   Except Route;
       //Endif;
       //If not %open(Qsysprt1) and summaryRpt= *on;
       //   Open qsysprt1;
       //Endif;
       //xxFil# = '1';
       //Chain orKy POR01;
       //If %found;
       //   Except Route1;
       //Endif;
       //***** End Add ***** 08/26/14 *************************************
       //***** End Del ***** 07/03/15 *************************************
       //*** beg add ***** 06/24/16 ***************
       If isUseScriptura;
          outXmlFile = xmlFile + '@';
          Callp S4401C(outXmlFile);
       Endif;
       If not(isUseSpoolOut);
          *Inlr = *off;
          Return;
       Endif;
       //*** end add ***** 06/24/16 ***************
       //***** Beg Add ***** 07/03/15 *************************************
       If detailRpt = *on;
          If not %open(Qsysprt);
             Open qsysprt;
          Endif;
          xxFil# = '0';
          Chain orKy POR01;
          If %found;
             Except Route;
          Endif;
       Endif;
       If summaryRpt = *on;
          If not %open(Qsysprt1);
             Open qsysprt1;
          Endif;
          xxFil# = '1';
          Chain orKy POR01;
          If %found;
             Except Route1;
          Endif;
       Endif;
       //***** End Add ***** 07/03/15 *************************************
      /End-free
     C                   eval      *inlr = *on
    C*____________________________________________________________________
    C*
    C*====================================================================
    C* agentBrk - Set up for new agent
    C*====================================================================
    C*
     C     agentBrk      begsr
     C*
     C                   if        saveGrp <> *zero
     C                   exsr      empTotals
     C                   endif
    C*
     C                   eval      prtAgtName = getAgtName(EAAGT#:AGrrn:AArrn
     C                                                    :EAFND:EACO#)
    C*
     C                   eval      saveAgt# = EAAGT#
     C*
     C                   if        agentSort = *on and saveGrp = EAFND
     C                   eval      *inof  = *on
     C                   eval      *inoa  = *on
     C                   endif
     C*
     C                   exsr      empBrk
    C*
     C                   endsr
    C*
    C*====================================================================
    C* empBrk - Set up for new employer/division
    C*====================================================================
    C*
     C     empBrk        begsr
     C*
     C                   eval      prtPrvBal = *on
     C*
     C                   if        prtEmpTtls = *on
     C                   exsr      empTotals
     C                   endif
    C*
    C*  Get new employer information
    C*
     C                   eval      emplrName = getEmpName(EAEMP#:EADIV:EMrrn
     C                                                   :EAFND:EACO#)
      *** beg chg **** 01/04/10 *******************************************************************
     C   97              eval      prtEmpNa25 = emplrName
     C  n97              eval      prtEmpName = emplrName
     C************       eval      prtEmpName = emplrName
      *** end chg **** 01/04/10 *******************************************************************
     C                   eval      prtEmpNam1 = emplrName
    C*
     C                   eval      empAct    = chkEmpAct
     C                   eval      empAnnPrm = *zero
    C*
     C                   clear                   ELPFDT_mdy
     C                   clear                   ELPTDT_mdy
     C                   clear                   cancelDt
     C                   clear                   cxDate
    C*
     C     elky1         chain     wdell1                             60
    C*
     C                   if        *in60 = *off
    C*
      **** begin add **** 02/19/02 *****************************************
     C     emky          chain     wmeml                              61
      **** begin add **** 01/04/10 *****************************************
     C   97              eval      savdst# = emsemp
      **** end   add **** 01/04/10 *****************************************
      *
     C                   if        EMSTRP = 'Y'
     C                             and ELARSM = 'Y'
      *
     C     elky          setll     wdell1
     C     elky          reade     wdell1                                 61
     C     *in61         doweq     *off
     C                   if        ELARSM = 'Y'
     C                   if        ELFAF = 'A'
     C                   add       ELAAPR        empAnnPrm
     C                   else
     C                   add       ELANPR        empAnnPrm
     C                   endif
     C                   endif
     C     elky          reade     wdell1                                 61
     C                   enddo
      *
     C                   else
      *
      **** end   add **** 02/19/02 *****************************************
     C                   if        ELFAF = 'A'
     C                   eval      empAnnPrm = ELAAPR
     C                   else
     C                   eval      empAnnPrm = ELANPR
     C                   endif
      *
      **** begin add **** 02/19/02 *****************************************
     C                   endif
      **** end   add **** 02/19/02 *****************************************
     C*
     C                   eval      ELPFDT_mdy = cvtToDate(ELPFDT)
     C                   eval      ELPTDT_mdy = cvtToDate(ELPTDT)
     C*
     C                   eval      cxDate = getCxDate(EACO#:EAFND:EAEMP#:
     C                                                EADIV)
    C*
     C                   endif
     C*
     C                   eval      *in41 = *off
     C*
     C                   if        cxDate < ELPFDT or
     C                             cxDate > ELPTDT
     C                   clear                   cxDate
     C                   endif
     C*
     C                   if        cxDate <> *zero
     C                   eval      cancelDt = cvtToDate(cxDate)
     C                   eval      *in41 = *on
     C                   endif
     C*
     C                   eval      prtEmpInfo = empAct     = *on or
     C                                          @@EmpNoAct = 'Y'
    C*
     C                   eval      saveEmp# = EAEMP#
     C                   eval      saveDiv  = EADIV
    C*
      *** beg add **** 07/16/02 ********************************************************************
      *** Beg Chg **** 12/17/21 ********************************************************************
     C*****************  if        saveGrp <> *zero and saveGrp =  EAFND
     C*****************            or saveYr <> *zero and saveYr =  EAFYR
     C                   if        saveGrp <> *zero and saveGrp <> EAFND
     C                             or saveYr <> *zero and saveYr <> EAFYR
      *** End Chg **** 12/17/21 ********************************************************************
     C                   exsr      grpTotals
    C                   eval      *inoa = *on
    C                   eval      *inof = *on
     C                   endif
      *** end add **** 07/16/02 ********************************************************************
    C*
     C                   exsr      srPrevBal
     C*
     C                   if        prtEmpInfo = *on and printRpt = *on
     C                              and saveGrp = EAFND and saveYr = EAFYR
      *** beg add ***** 06/24/16 *****************************************
     c                              and isUseSpoolOut
      *** end add ***** 06/24/16 *****************************************
      *** beg chg **** 01/15/02 ********************************************************************
     C****************** if        detailRpt = *on
     C                   if        detailRpt = *on and firstRead = *off
      *** end chg **** 01/15/02 ********************************************************************
     C*
      *** beg chg ***** 06/24/16 ****************
     C                   if        *inof = *on
     C                   exsr      headings
     C                   else
     C***  Begin Add  ***  11/19/09  **************************************
     C                   select
     C                   when      cltID = '025'
     C                   except    line025
     C                   other
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    line
     C***  Begin Add  ***  11/19/09  **************************************
     C                   endsl
     C***  End   Add  ***  11/19/09  **************************************
     C                   endif
     C*
      *** beg add **** 01/04/10 ************************************************
     C                   if        *in97 = *on
     C                   movel     HEADING25     @@hding         150
     C                   else
     C                   movel     HEADING       @@hding
     C                   endif
      *** end add **** 01/04/10 ************************************************
     C                   except    empHead
     C***  Begin Add  ***  11/19/09  **************************************
     C                   select
     C                   when      cltID = '025'
     C                   except    empDtl025
     C                   endsl
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    empDetail
     C                   except    prvBalDetl
     C*
     C                   eval      prtPrvBal = *off
     C*
     C                   endif
     C*
     C                   endif
    C*
     C                   eval      ttlColEmp  = *zero
     C                   eval      ttlOthrEmp = *zero
     C                   eval      ttlBillEmp = *zero
     C                   eval      totalDue   = *zero
     C                   eval      currentBal = prvBal
    C*
     C                   eval      prtEmpTtls = *on
    C*
     C                   endsr
     C*
    C*====================================================================
    C* empTotals - Set up for new group
    C*====================================================================
    C*
     C     empTotals     begsr
     C*
     C                   eval      prtEmpTtls = *off
    C*
     C                   eval      totalDue  = depositDue + feesDue +
     C                                         premiumDue
    C*
    C*  Print Employer Total Line
    C*
     C                   if        prtEmpInfo = *on and printRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C*
     C                   if        detailRpt = *on
      *** beg add ***** 06/24/16 *****************************************
     c                             and isUseSpoolOut
      *** end add ***** 06/24/16 *****************************************
     C*
     C                   if        *inof = *on
     C                   exsr      headings
      *** beg add **** 01/04/10 ************************************************
     C                   if        *in97 = *on
     C                   movel     HEADING25     @@hding
     C                   else
     C                   movel     HEADING       @@hding
     C                   endif
      *** end add **** 01/04/10 ************************************************
     C                   except    empHead
     C                   except    empDetail
     C                   endif
     C*
     C                   except    empTotal
    C*
     C                   endif
     C*
     C                   if        summaryRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C*
      *** beg chg **** 01/15/02 ********************************************************************
     C*****************  if        *inoa = *on
     C                   if        *inoa = *on or firstRead1 = *on
     C                   eval      firstRead1 = *off
      *** beg chg **** 01/15/02 ********************************************************************
     C                   exsr      headings1
      *** beg add **** 01/04/10 ************************************************
     C                   if        *in97 = *on
     C                   movel     HEADING25     @@hding
     C                   else
     C                   movel     HEADING       @@hding
     C                   endif
      *** end add **** 01/04/10 ************************************************
     C                   except    empHead1
     C                   except    line1
     C                   endif
    C*
     C                   except    empDetail1
    C*
     C                   endif
    C*
     C                   endif

      *** beg add ***** 06/24/16 *****************
      /free
       If isUseXML;
          WrtWXARP3();
       Endif;
      /end-free
      *** end add ***** 06/24/16 *****************

    C*
    C*  Add to the Totals
    C*
       //***** Beg Del ***** 01/08/21 **********************************
       //***** Beg Add ***** 01/08/21 **********************************
      /Free
       //*****If (saveGrp =  EAfnd and saveYr  <> EAfyr) or (saveGrp <> EAfnd);
          //*****ttlColGrp  = 0;
          //*****ttlOthrGrp = 0;
          //*****ttlBillGrp = 0;
          //*****prvBalGrp  = 0;
          //*****fndANPR    = 0;
          //fndTotal = 0;
       //*****Endif;
      /End-Free
       //***** End Add ***** 01/08/21 **********************************
       //***** End Del ***** 01/08/21 **********************************
     C                   eval      ttlColGrp  = ttlColGrp  + ttlColEmp
     C                   eval      ttlOthrGrp = ttlOthrGrp + ttlOthrEmp
     C                   eval      ttlBillGrp = ttlBillGrp + ttlBillEmp
     C                   eval      prvBalGrp  = prvBalGrp  + prvBal
     C                   eval      fndANPR    = fndANPR    + empAnnPrm
     C                   eval      fndTotal   = fndTotal   + currentBal
    C*
     C                   endsr
     C*
    C*====================================================================
    C* final - Print final pages before program end
    C*====================================================================
    C*
     C     final         begsr
     C*
     C                   exsr      empTotals
     C                   exsr      grpTotals
     C*
     C                   if        printRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C*
     C                   if        oneRcrd  = *off
     C*
    C*  SELECTION CRITERIA NOT FOUND
    C*
     C                   if        detailRpt = *on
     C***  Begin Add  ***  11/19/09  **************************************
     C                   select
     C                   when      cltID = '025'
     C                   except    head025
     C                   other
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    head
     C***  Begin Add  ***  11/19/09  **************************************
     C                   endsl
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    noRec
     C                   endif
     C*
     C                   if        summaryRpt = *on
     C                   except    head1
     C                   except    noRec1
     C                   endif
     C*
     C                   else
     C*
      ******* beg chg ********* 05/28/14 ************************************
     C*******            if        @@fnd_fr <> @@fnd_to or
     C*******                      @@fyr_fr <> @@fyr_to
      *** beg chg ***** 06/21/17 ***************************
      *** /free
      ***  If isMultiGrp = 'Y' or @@fyr_fr <> @@fyr_to;
      *** /end-free
      /free
       If isMultiGrp = 'Y'
        or @@fyr_fr <> @@fyr_to
        or @@fyr_to = *zero;
      /end-free
      *** end chg ***** 06/21/17 ***************************
      ******* end chg ********* 05/28/14 ************************************
     C                   exsr      totals
     C                   endif
     C*
     C                   endif
    C*
     C                   endif

      *** beg add ***** 06/24/16 ****************
      /free
       If isUseXML;
          WrtWXARP1();
       Endif;
      /end-free
      *** end add ***** 06/24/16 ****************
    C*
     C                   endsr
     C*
    C*====================================================================
    C* groupBrk - Set up for new group
    C*====================================================================
    C*
     C     groupBrk      begsr
     C*
     C                   if        saveGrp = *zero
     C                   exsr      loadGroup
     C                   endif
     C*
     C*
     C                   exsr      yrBrk
    C*
     C                   exsr      loadGroup
    C*
     C                   endsr
     C*
    C*====================================================================
    C* grpTotals - Print group totals
    C*====================================================================
    C*
     C     grpTotals     begsr
     C*
     C                   eval      groupBreak = *on
    C*
     C                   if        oneRcrd = *on and detailRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                             and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   exsr      headings
     C                   except    groupSumm
     C                   endif
    C*
     C                   if        oneRcrd = *on and summaryRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   exsr      headings1
     C                   except    grpTotal1
     C                   exsr      headings1
     C                   except    groupSumm1
     C                   endif

      *** beg add ***** 06/02416 ****************
      /free
       If isUseXML;
          WrtWXARP2();
       Endif;
      /end-free
      *** end add ***** 06/02416 ****************

     C*
     C                   eval      groupBreak = *on
    C*
    C*  Print A/R Summary
    C*
     *** beg chg ***** 06/24/16 *****************
     C**********         exsr      srARDetail
      /free
       prtGrpYrSumm  = *on;
       Exsr srArDetail;
       prtGrpYrSumm  = *off;
      /end-free
     *** end chg ***** 06/24/16 *****************
     C*
    C*  Clear totals
    C*
     C                   exsr      newGroup
    C*
     C                   endsr
     C*
    C*====================================================================
    C* headings - Print page headings (QSYSPRT)
    C*====================================================================
    C*
     C     headings      begsr
     C*
     C***  Begin Add  ***  11/19/09  **************************************
     C                   select
     C                   when      cltID = '025'
     C                   except    head025
     C                   other
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    head
     C***  Begin Add  ***  11/19/09  **************************************
     C                   endsl
     C***  End   Add  ***  11/19/09  **************************************
     C*
     C                   if        groupRun = *on
     C                   except    groupHead
     C                   if        groupBreak = *on
     C                   except    subHead
     C                   endif
     C                   else
     C                   except    totalHead
     C                   endif
    C*
     C                   endsr
     C*
    C*====================================================================
    C* headings1 - Print page headings (QSYSPRT1)
    C*====================================================================
    C*
     C     headings1     begsr
     C*
     C                   except    head1
     C*
     C                   if        groupRun = *on
     C                   except    groupHead1
     C                   if        groupBreak = *on
     C                   except    subHead1
     C                   endif
     C                   else
     C                   except    totalHead1
     C                   endif
     C*
     C                   if        agentSort  = *on and groupRun = *on and
     C                             groupBreak = *off
     C                   except    agentHead1
     C                   endif
    C*
     C                   endsr
     C*
    C*====================================================================
    C* Initialize - Initialize fields
    C*====================================================================
    C*
     C     Initialize    begsr
    C*
     C     *hms          move      q1time        sys_time
     C     *usa          move      *date         sys_date
     C******* beg add ****** 09/09/10 *********************************
     C     upky          chain     pup01                              60
     C                   if        UPXEMP  = 'Y'
     C                   move      upemp#        @@emp#
     C                   endif
     C******* end add ****** 09/09/10 *********************************
    C*
     C                   move      @@co#         xxco#
     C                   eval      frdt_usa = CvtToDate(numDate(@@ARdt_fr))
     C                   eval      todt_usa = CvtToDate(numDate(@@ARdt_to))
    C*
     C                   eval      x7ARdt_fr = dtCYMD(numDate(@@ARdt_fr))
     C                   eval      x7ARdt_to = dtCYMD(numDate(@@ARdt_to))
     C*
     C                   eval      loadFile = @@output = 'F' or
     C                                        @@output = 'B'
     C                   eval      printRpt = @@output = 'P' or
     C                                        @@output = 'B'
     C*
     C                   eval      summaryRpt = *off
     C                   eval      detailRpt  = *off
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     C*** END ADD ***** 01/28/10 **************************************
    C*
     C                   if        printRpt = *on
     C                   exsr      routing
     C                   endif
    C*
     C***  Begin Add  ***  11/19/09  **************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltID
     C***  End   Add  ***  11/19/09  **************************************
    C*
     C                   endsr
     C*
    C*====================================================================
    C* loadGroup - Load group header fields
    C*====================================================================
    C*
     C     loadGroup     begsr
    C*
     C                   eval      groupName = GetGrpName(EAFND:FNrrn:EACO#)
      **** begin add **** 06/23/09 *****************************************
     C**************     eval      companyNm = GetAltCoNm(EAFND:EACO#)
     C                   eval      companyNm = GetAltCoNm(EACO#:EAFND)
      **** end   add **** 06/23/09 *****************************************
    C*
     C                   if        companyNm = *blank
     C                   eval      companyNm = GetCompNam(EACO#)
     C                   endif
    C*
     C                   eval      companyNm = centerFld(companyNm)
    C*
     C                   endsr
     C*
    C*====================================================================
    C* newGroup - Reset values for new group
    C*====================================================================
    C*
     C     newGroup      begsr
    C*
    C*  Accumulate totals
    C*
     C                   eval      ttlANPR    = ttlANPR    + fndANPR
     C                   eval      Total      = Total      + fndTotal
     C                   eval      prvAdjTtl  = prvAdjTtl  + prvAdjGrp
     C                   eval      prvBillTtl = prvBillTtl + prvBillGrp
     C                   eval      prvAdtTtl  = prvAdtTtl  + prvAdtGrp
     C                   eval      prvNSFTtl  = prvNSFTtl  + prvNSFGrp
     C                   eval      prvPmntTtl = prvPmntTtl + prvPmntGrp
     C                   eval      prvVoidTtl = prvVoidTtl + prvVoidGrp
     C                   eval      prvRfndTtl = prvRfndTtl + prvRfndGrp
      *** beg add ***** 02/19/04 **********************************************
     C                   eval      prvTrVdTtl = prvTrVdTtl + prvTrVdGrp
      *** end add ***** 02/19/04 **********************************************
     C                   eval      prvDepBTtl = prvDepBTtl + prvDepBGrp
     C                   eval      prvDepPTtl = prvDepPTtl + prvDepPGrp
     C                   eval      prvFeeBTtl = prvFeeBTtl + prvFeeBGrp
     C                   eval      prvFeePTtl = prvFeePTtl + prvFeePGrp
     C                   eval      prvBalTtl  = prvBalTtl  + prvBalGrp
     C                   eval      adjTtl     = adjTtl     + adjGrp
     C                   eval      billTtl    = billTtl    + billGrp
     C                   eval      depTtl     = depTtl     + depGrp
     C                   eval      NSFTtl     = NSFTtl     + NSFGrp
     C                   eval      pmntTtl    = pmntTtl    + pmntGrp
     C                   eval      voidTtl    = voidTtl    + voidGrp
     C                   eval      rfndTtl    = rfndTtl    + rfndGrp
      *** beg add ***** 02/19/04 **********************************************
     C                   eval      trVdTtl    = trVdTtl    + trVdGrp
      *** end add ***** 02/19/04 **********************************************
     C                   eval      depBTtl    = depBTtl    + depBGrp
     C                   eval      depPTtl    = depPTtl    + depPGrp
     C                   eval      feeBTtl    = feeBTtl    + feeBGrp
     C                   eval      feePTtl    = feePTtl    + feePGrp
    C*
    C*  Initialize Group Totals
    C*
     C                   clear                   ttlColEmp
     C                   clear                   ttlOthrEmp
     C                   clear                   ttlBillEmp
     C                   clear                   prvBalGrp
     C                   clear                   fndANPR
     C                   clear                   fndTotal
     C                   clear                   prvAdjGrp
     C                   clear                   prvBillGrp
     C                   clear                   prvAdtGrp
     C                   clear                   prvNSFGrp
     C                   clear                   prvPmntGrp
     C                   clear                   prvVoidGrp
     C                   clear                   prvRfndGrp
      *** beg add ***** 02/19/04 ***********************************************
     C                   clear                   prvTrVdGrp
      *** end add ***** 02/19/04 ***********************************************
     C                   clear                   prvDepBGrp
     C                   clear                   prvDepPGrp
     C                   clear                   prvFeeBGrp
     C                   clear                   prvFeePGrp
     C                   clear                   adjGrp
     C                   clear                   billGrp
     C                   clear                   depGrp
     C                   clear                   NSFGrp
     C                   clear                   pmntGrp
     C                   clear                   voidGrp
     C                   clear                   rfndGrp
      *** beg add ***** 02/19/04 ***********************************************
     C                   clear                   trVdGrp
      *** end add ***** 02/19/04 ***********************************************
     C                   clear                   depBGrp
     C                   clear                   depPGrp
     C                   clear                   feeBGrp
     C                   clear                   feePGrp
    C*
    C*  Initialize A/R Summary Fields
    C*
     C                   clear                   amounts
     C                   clear                   codes
     C                   clear                   codes_amts
    C*
     C                   endsr
     C*
    C*====================================================================
    C* Routing - Set-up for routing pages
    C*====================================================================
    C*
     C     Routing       begsr
     C*
     C                   eval      summaryRpt = @@report = 'S' or
     C                                          @@report = 'B'
     C                   eval      detailRpt  = @@report = 'D' or
     C                                          @@report = 'B'

     C*** beg add ***** 06/24/16 ******************************************
      /Free
       If printRpt;
          processType = 'A_R Detail Report';
          outReturn   = processType;
          outFldrPrefix  = *blanks;
          isUseSpoolOut  = UseSpoolOut(processType);
          isUseScriptura = UseScriptura(processType);
          isUse3rdParty  = Use3rdXml(processType);
          isUseXML = isUseScriptura or isUse3rdParty;
          If isUseXML;
             Open WXARP1;
             Open WXARP2;
             Open WXARP3;
             Open WXARPAC;
             If detailRpt;
                Open WXARP3D;
             Endif;
             outPreString = 'ARDtlRpt';
             xmlFile = GetXMLFileName(' ':*zeros:outCo#:outFnd:
              outFldrPrefix:outPreString:outReturn);
          Endif;
       Endif;
      /End-free
     C*** end add ***** 06/24/16 ******************************************

     C                   eval      agentSort  = @@sort   = 'A'
    C*
     C                   eval      routeCo# = rmvZeros(@@co#)
     C*
      ***** beg del ****** 05/28/14 ****************************************************
     C*****              if        @@fnd_to = *zeros
     C*****              eval      routeGrp = '*ALL'
     C*****              else
      *** beg chg **** 07/16/02 ********************************************************
     C****************** eval      routeGrp = %trimr(rmvZeros(@@fnd_fr))
     C******************                      + ' - ' +
     C******************                      %trimr(rmvZeros(@@fnd_to))
     C*****              eval      xxfnd_fr = rmvZeros(@@fnd_fr)
     C*****              eval      xxfnd_to = rmvZeros(@@fnd_to)
     C*****              eval      routeGrp = xxfnd_fr + ' - ' + xxfnd_to
      *** end chg **** 07/16/02 ********************************************************
     C*****              endif
      ***** end del ****** 05/28/14 ****************************************************
     C*
     C                   if        @@fyr_to = *zeros
     C                   eval      routeYr  = '*ALL'
     C                   else
      *** beg chg **** 07/16/02 ********************************************************
     C****************** eval      routeYr  = %trimr(rmvZeros(@@fyr_fr))
     C******************                      + ' - ' +
     C******************                      %trimr(rmvZeros(@@fyr_to))
     C                   eval      xxfyr_fr = rmvZeros(@@fyr_fr)
     C                   eval      xxfyr_to = rmvZeros(@@fyr_to)
     C                   eval      routeYr = xxfyr_fr + ' - ' + xxfyr_to
      *** end chg **** 07/16/02 ********************************************************
     C                   endif
     C*
     C                   if        @@agt# <> *zeros
     C                   eval      routeAgt# = rmvZeros(@@agt#)
     C                   else
     C                   eval      routeAgt# = '*ALL'
     C                   endif
     C*
     C                   select
     C                   when      @@emp# <> *zeros and @@div <> *blank
      *** beg chg **** 06/18/02 ********************************************************
     C*****************  eval      routeEmpDv = %trimr(rmvZeros(@@emp#))
     C*****************                         + '/' +
     C*****************                         %trimr(rmvZeros(@@div))
     C                   eval      xxemp = rmvZeros(@@emp#)
     C                   eval      xxdiv = rmvZeros(@@div)
     C                   eval      routeEmpDv = xxemp + '/' + xxdiv
      *** end chg **** 06/18/02 ********************************************************
     C                   when      @@emp# <> *zeros
      *** beg chg **** 07/16/02 ********************************************************
     C*****************  eval      routeEmpDv = %trimr(rmvZeros(@@emp#))
     C*****************                         + '/*ALL'
     C                   eval      xxemp = rmvZeros(@@emp#)
     C                   eval      routeEmpDv = xxemp + '/' + '*ALL'
      *** end chg **** 07/16/02 ********************************************************
     C                   other
     C                   eval      routeEmpDv = '*ALL'
     C                   endsl
    C*
     C                   eval      x = 1
     C     @@sort        lookup    sort(x)                                50
     C   50              eval      routeSort = sortDesc(x)
    C*
     C                   eval      x = 1
     C     @@CommPlan    lookup    plan(x)                                50
     C   50              eval      routePlan = planDesc(x)
    C*
     C                   eval      x = 1
     C     @@report      lookup    rpt(x)                                 50
     C   50              eval      routeRpt = rptDesc(x)
    C*
     C                   eval      x = 1
     C     @@ARCode      lookup    ARCode(x)                              50
     C   50              eval      routeAR = ARCodeDesc(x)
    C*
     C                   if        @@EmpNoAct = 'Y'
     C                   eval      routeNoAct = 'YES'
     C                   else
     C                   eval      routeNoAct = 'NO'
     C                   endif
    C*
    C* Sort the Category Array
    C*
     C                   clear                   ctgryDesc
    C*
     C                   if        @@ctyp_all <> *blanks
    C*
     C                   movea     @@ctyp_all    category
     C                   sorta     category
    C*
     C                   eval      *in20 = *on
     C                   eval      z     = 1
    C*
     C                   do        #ofCats       y
    C*
     C                   if        category(y) <> *blanks
     C                   eval      ctgryDesc(z) = category(y) + ' ' +
     C                               GetCatDesc(xxco#:category(y))
     C                   eval      z = z + 1
     C                   endif
    C*
     C                   enddo
    C*
     C                   else
    C*
     C                   eval      *in20        = *off
     C                   eval      ctgryDesc(1) = '*ALL'
    C*
     C                   endif
    C*
    C* Sort the Code Array
    C*
     C                   clear                   codeDesc
    C*
     C                   if        @@arcd_all <> *blanks
    C*
     C                   movea     @@arcd_all    code
     C                   sorta     code
    C*
     C                   eval      *in21 = *on
     C                   eval      z     = 1
    C*
     C                   do        #ofCodes      y
    C*
     C                   if        code(y) <> *blanks
     C                   eval      codeDesc(z) = code(y) + ' ' +
     C                               GetARDesc(code(y):TTrrn:xxco#)
     C                   eval      z = z + 1
     C                   endif
    C*
     C                   enddo
    C*
     C                   else
    C*
     C                   eval      *in21       = *off
     C*
     C                   if        @@ARCode = *blank
     C                   eval      codeDesc(1) = '*ALL'
     C                   endif
    C*
     C                   endif
    C*
     C     @@que         comp      *blank                             090908
    C*
     C                   eval      xxque    = @@que
     C                   eval      userName = getUsrName(q1user)
     C                   eval      UPUSNM   = userName

      *** beg chg **** 12/07/04 ********************************************************
     c                   select
     c                   when      @@btyp = 'A'
     c                   eval      billingtype = 'ACH'
     c                   when      @@btyp = 'D'
     c                   eval      billingtype = 'Direct Bill'
     c                   when      @@btyp = 'P'
     c                   eval      billingtype = 'Payroll'
     c                   when      @@btyp = *blank
     c                   eval      billingtype = '*All'
     c                   endsl
      *** end chg **** 12/07/04 ********************************************************
     C*
     C*  Summary Report
     C*
     C                   if        summaryRpt = *on
     C*
      *** beg chg ***** 06/24/16 ****************
     C***********       open      qsysprt1
      /free
       If isUseSpoolOut;
          Open Qsysprt1;
       Endif;
      /end-free
      *** end chg ***** 06/24/16 ****************
    C*
    C* Get the Distribution Information
    C*
     C                   clear                   ORTITL
     C*
     c                   eval      xxfil#   = '1'
     C     orky          chain     por01                              60
     C*
     C                   if        ORTITL = *blank
     C                   eval      ORTITL = title + ' ' + title1
     C                   endif
     C*
     C                   eval      ORTITL1 = leftJstify(ORTITL)
    C*
    C* Print the Distribution Information
    C*
      ***** Beg Del ***** 08/26/14 ***********************************
     C********           if        not *in60
     C********           except    route1
     C********           endif
      ***** End Del ***** 08/26/14 ***********************************
     C*
     C                   eval      ORTITL1 = centerFld(ORTITL1)
     C*
     C                   endif
     C*
     C*  Detail Report
     C*
     C                   if        detailRpt = *on
     C*
      *** beg chg ***** 06/24/16 ****************
     C***********       open      qsysprt
      /free
       If isUseSpoolOut;
          Open Qsysprt;
       Endif;
      /end-free
      *** end chg ***** 06/24/16 ****************
    C*
    C* Get the Distribution Information
    C*
     C                   clear                   ORTITL
     C*
     c                   eval      xxfil#   = '0'
     C     orky          chain     por01                              60
     C*
     C                   if        ORTITL = *blank
     C                   eval      ORTITL = title + ' ' + title0
     C                   endif
     C*
     C                   eval      ORTITL = leftJstify(ORTITL)
    C*
    C* Print the Distribution Information
    C*
      ***** Beg Del ***** 08/26/14 ***********************************
     C**********         if        not *in60
     C**********         except    route
     C**********         endif
      ***** End Del ***** 08/26/14 ***********************************
     C*
     C                   eval      ORTITL = centerFld(ORTITL)
     C*
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* srARDetail - PRINT A/R CODE SUMMARY
    C*====================================================================
    C*
     C     srARDetail    begsr
    C*
    C*  Move Code and Total Arrays to the Combination Array
    C*
     C                   eval      v = 1
     C                   eval      summaryTtl = *zero
    C*
     C                   dow       v < numOfElmts and codes(v) <> *blanks
     C                   eval      xCode         = codes(v)
     C                   eval      xAmount       = amounts(v)
     C                   eval      codes_amts(v) = code_amt
     C                   eval      v             = v + 1
     C                   enddo
    C*
    C*  If There are Records in the Array then Continue
    C*
     C                   if        v <> 1
    C*
    C*  Print A/R Summary Heading
    C*
     C                   if        detailRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   exsr      headings
     C                   except    ARHead
     C                   endif
    C*
     C                   if        summaryRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   exsr      headings1
     C                   except    ARHead1
     C                   endif
     C*
     C                   eval      *inof = *off
     C                   eval      *inoa = *off
    C*
    C*  Sort the Array by Code
    C*
     C                   sorta     codes_amts
    C*
    C*  Find the First record in the Array
    C*
     C                   eval      v = 1
     C     *blanks       lookup    codes_amts(v)                      60
    C*
     C     v             do        numOfElmts    x1
    C*
     C                   eval      code_amt = codes_amts(x1)
    C*
    C*  Retrieve A/R Code Description
    C*
     C                   eval      xCodeDesc = getARDesc(xCode:rrn:xxco#)
     C                   eval      xCodeType = getARType(xCode:rrn:xxco#)
    C*
    C*  Print the Array Detail Line
    C*
     C                   if        *inof = *on and detailRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                               and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   exsr      headings
     C                   except    ARHead
     C                   endif
    C*
     C                   if        *inoa = *on and summaryRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                               and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   exsr      headings1
     C                   except    ARHead1
     C                   endif
    C*
    C* ADD AMOUNT TO SUMMARY TOTAL FIELD
    C*
     C                   eval      summaryTtl = summaryTtl + xAmount
     C*
     C                   if        detailRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   except    ARDetail
     C                   endif
     C*
     C                   if        summaryRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   except    ARDetail1
     C                   endif

      /free
       If isUseXML;
          WrtWXARPAC();
       Endif;
      /end-free
    C*
     C                   enddo
    C*
    C* PRINT SUMMARY TOTAL LINE
    C*
     C                   if        detailRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   except    ARTotal
     C                   endif
    C*
     C                   if        summaryRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   except    ARTotal1
     C                   endif
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* srARSumm - Load A/R types into arrays
    C*====================================================================
    C*
     C     srARSumm      begsr
     C*
     C*  Accumulate group codes
    C*
    C* Check to See if Code Exists in the Array
    C*
     C                   eval      v = 1
     C     EAARCD        lookup    codes(v)                               33
    C*
     C                   if        *in33 = *off
    C*
    C*  Find Next Available Position
    C*
     C                   eval      v = 1
     C     '  '          lookup    codes(v)                               33
    C*
     C                   eval      amounts(v) = EAAMT
     C                   eval      codes(v)   = EAARCD
    C*
     C                   else
    C*
    C*  If Code is in Array Add to Existing Amount
    C*
     C                   eval      amounts(v) = amounts(v) + EAAMT
    C*
     C                   endif
     C*
     C*  Accumulate total codes
    C*
    C* Check to See if Code Exists in the Array
    C*
     C                   eval      v = 1
     C     EAARCD        lookup    codesTtl(v)                            33
    C*
     C                   if        *in33 = *off
    C*
    C*  Find Next Available Position
    C*
     C                   eval      v = 1
     C     '  '          lookup    codesTtl(v)                            33
    C*
     C                   eval      amountsTtl(v) =  EAAMT
     C                   eval      codesTtl(v)   = EAARCD
    C*
     C                   else
    C*
    C*  If Code is in Array Add to Existing Amount
    C*
     C                   eval      amountsTtl(v) = amountsTtl(v) + EAAMT
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* srCurrBal - Calculate current balance
    C*====================================================================
    C*
     C     srCurrBal     begsr
    C*
     C                   if        EAxxDT <= x7ARdt_to and
     C                             EAxxDT >= x7ARdt_fr
    C*
     C                   eval      EATRDT_usa = cvtToDate(EATRDT)
     C                   eval      EAPODT_usa = cvtToDate(EAPODT)
    C*
    C*  Initialize detail line fields
    C*
     C                   eval      colEmp  = *zero
     C                   eval      othrEmp = *zero
     C                   eval      billEmp = *zero
    C*
    C*  Reverse Amounts for Payments, Initial Deposits and Fee Payments
    C*
     C                   eval      EAAMT = ARValue(EAAMT:EAARTY)
     C*
     C                   select
    C*____________________________________________________________________
    C*
    C*  1. Accumulate Adjustments
    C*
     C                   when      EAARTY = 'A'
     C                   eval      adjGrp     = adjGrp + EAAMT
     C                   eval      billEmp    = EAAMT
     C                   eval      ttlBillEmp = ttlBillEmp + EAAMT
     C                   eval      premiumDue = premiumDue + EAAMT
    C*
    C*  2. Accumulate Bills
    C*
     C                   when      EAARTY = 'B'
     C                   eval      billGrp    = billGrp  + EAAMT
     C                   eval      billEmp    = EAAMT
     C                   eval      ttlBillEmp = ttlBillEmp + EAAMT
     C                   eval      premiumDue = premiumDue + EAAMT
    C*
    C*  3. Accumulate Audits
    C*
     C                   when      EAARTY = 'D'
     C                   eval      depGrp     = depGrp + EAAMT
     C                   eval      billEmp    = EAAMT
     C                   eval      ttlBillEmp = ttlBillEmp + EAAMT
     C                   eval      premiumDue = premiumDue + EAAMT
    C*
    C*  4. Accumulate Voids
    C*
     C                   when      EAARTY = 'V'
     C                   eval      voidGrp    = voidGrp + EAAMT
     C                   eval      othrEmp    = EAAMT
     C                   eval      ttlOthrEmp = ttlOthrEmp + EAAMT
     C                   eval      premiumDue = premiumDue + EAAMT
    C*
    C*  5. Accumulate Payments
    C*
     C                   when      EAARTY = 'P'
     C                   eval      pmntGrp    = pmntGrp + EAAMT
     C                   eval      colEmp     = EAAMT
     C                   eval      ttlColEmp  = ttlColEmp  + EAAMT
     C                   eval      premiumDue = premiumDue + EAAMT
    C*
    C*  6. Accumulate NSF's
    C*
     C                   when      EAARTY = 'N'
     C                   eval      NSFGrp     = NSFGrp + EAAMT
     C                   eval      othrEmp    = EAAMT
     C                   eval      ttlOthrEmp = ttlOthrEmp + EAAMT
     C                   eval      premiumDue = premiumDue + EAAMT
    C*
    C*  7. Accumulate Deposits Paid
    C*
     C                   when      EAARTY = 'I'
     C                   eval      depPGrp    = depPGrp + EAAMT
     C                   eval      colEmp     = EAAMT
     C                   eval      ttlColEmp  = ttlColEmp  + EAAMT
     C                   eval      depositDue = depositDue + EAAMT
    C*
    C*  8. Accumulate Deposits Billed
    C*
     C                   when      EAARTY = 'H'
     C                   eval      depBGrp    = depBGrp + EAAMT
     C                   eval      billEmp    = EAAMT
     C                   eval      ttlBillEmp = ttlBillEmp + EAAMT
     C                   eval      depositDue = depositDue + EAAMT
    C*
    C*  9. Accumulate Refunds
    C*
     C                   when      EAARTY = 'R'
     C                   eval      rfndGrp    = rfndGrp + EAAMT
     C                   eval      othrEmp    = EAAMT
     C                   eval      ttlOthrEmp = ttlOthrEmp + EAAMT
     C                   eval      premiumDue = premiumDue + EAAMT
      *** beg add ***** 02/19/04 **********************************************
    C*
    C*  9a. Accumulate True Voids
    C*
     C                   when      EAARTY = 'T'
     C                   eval      trVdGrp    = trVdGrp + EAAMT
     C                   eval      othrEmp    = EAAMT
     C                   eval      ttlOthrEmp = ttlOthrEmp + EAAMT
     C                   eval      premiumDue = premiumDue + EAAMT
      *** end add ***** 02/19/04 **********************************************
    C*
    C*  10. Accumulate Fees Billed
    C*
     C                   when      EAARTY = 'F'
     C                   eval      feeBGrp    = feeBGrp + EAAMT
     C                   eval      billEmp    = EAAMT
     C                   eval      ttlBillEmp = ttlBillEmp + EAAMT
     C                   eval      feesDue    = feesDue + EAAMT
    C*
    C*  11. Accumulate Fees Paid
    C*
     C                   when      EAARTY = 'Q'
     C                   eval      feePGrp   = feePGrp + EAAMT
     C                   eval      colEmp    = EAAMT
     C                   eval      ttlColEmp = ttlColEmp + EAAMT
     C                   eval      feesDue   = feesDue   + EAAMT
    C*____________________________________________________________________
    C*
     C                   endsl
    C*
     C                   eval      currentBal = currentBal + EAAMT
    C*
     C                   if        prtEmpInfo = *on and printRpt = *on
     C*
     C                   if        detailRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
    C*
      *** beg chg **** 01/15/02 *******************************************************************
     C****************** if        *inof = *on
     C                   if        *inof = *on or firstRead = *on
     C                   eval      firstRead = *off
      *** end chg **** 01/15/02 *******************************************************************
     C*
     C                   exsr      headings
      *** beg add **** 01/04/10 ************************************************
     C                   if        *in97 = *on
     C                   movel     HEADING25     @@hding
     C                   else
     C                   movel     HEADING       @@hding
     C                   endif
      *** end add **** 01/04/10 ************************************************
     C                   except    empHead
     C***  Begin Add  ***  11/19/09  **************************************
     C                   select
     C                   when      cltID = '025'
     C                   except    empDtl025
     C                   endsl
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    empDetail
     C*
     C                   if        prtPrvBal = *on
     C                   except    prvBalDetl
     C                   eval      prtPrvBal = *off
     C                   endif
    C*
     C                   endif
    C*
     C***  Begin Add  ***  11/19/09  **************************************
     C                   select
     C                   when      cltID = '025'
     C                   except    detail025
     C                   endsl
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    detail
     C*
     C                   endif
     C*
     C                   endif
     C*
      *** beg chg ***** 06/24/16 ****************
     C***********        if        loadFile = *on
     C***********        exsr      writeWFARP
     C***********        endif
      /free
       If isUseXML or loadFile = *on;
          Exsr WriteWFARP;
       Endif;
      /end-free
      *** end chg ***** 06/24/16 ****************
    C*
     C                   exsr      srARSumm
    C*
     C                   endif
    C*
     C                   endsr
    C*
    C*====================================================================
    C* srPrevBal - Calculate previous balances
    C*====================================================================
    C*
     C     srPrevBal     begsr
    C*
    C*  INITIALIZE TOTAL DUE FEILDS
    C*
     C                   eval      depositDue = *zero
     C                   eval      feesDue    = *zero
     C                   eval      premiumDue = *zero
     C                   eval      totalDue   = *zero
     C                   eval      prvBal     = *zero
    C*
     C     eaky          chain     pea00                              58
    C*
    C*  Get the Previous Balance for the Specified Employer
    C*
     C                   dow       not *in58 and
     C                             l0_EAxxDT < x7ARdt_fr
     C*
     C                   eval      xxamt = ARValue(l0_EAAMT:l0_EAARTY)
    C*
    C*  Accumulate Previous Balance
    C*
     C                   eval      prvBal = prvBal + xxamt
     C*
     C                   select
     C*____________________________________________________________________
    C*
    C*  1. Accumulate Previous Adjustments
    C*
     C                   when      l0_EAARTY = 'A'
     C                   eval      prvAdjGrp = prvAdjGrp + xxamt
    C*
    C*  2. Accumulate Previous Bills
    C*
     C                   when      l0_EAARTY = 'B'
     C                   eval      prvBillGrp = prvBillGrp + xxamt
    C*
    C*  3. Accumulate Previous Audits
    C*
     C                   when      l0_EAARTY = 'D'
     C                   eval      prvAdtGrp = prvAdtGrp + xxamt
    C*
    C*  4. Accumulate Previous Voids
    C*
     C                   when      l0_EAARTY = 'V'
     C                   eval      prvVoidGrp = prvVoidGrp + xxamt
    C*
    C*  5. Accumulate Previous Payments
    C*
     C                   when      l0_EAARTY = 'P'
     C                   eval      prvPmntGrp = prvPmntGrp + xxamt
    C*
    C*  6. Accumulate Previous NSF's
    C*
     C                   when      l0_EAARTY = 'N'
     C                   eval      prvNSFGrp = prvNSFGrp + xxamt
    C*
    C*  7. Accumulate Previous Deposits Paid
    C*
     C                   when      l0_EAARTY = 'I'
     C                   eval      prvDepPGrp = prvDepPGrp + xxamt
    C*
    C*  8. Accumulate Previous Deposits Billed
    C*
     C                   when      l0_EAARTY = 'H'
     C                   eval      prvDepBGrp = prvDepBGrp + xxamt
    C*
    C*  9. Accumulate Previous Refunds
    C*
     C                   when      l0_EAARTY = 'R'
     C                   eval      prvRfndGrp = prvRfndGrp + xxamt
      *** beg add ***** 02/19/04 ***********************************************
    C*
    C*  9a. Accumulate Previous True Voids
    C*
     C                   when      l0_EAARTY = 'T'
     C                   eval      prvTrVdGrp = prvTrVdGrp + xxamt
      *** end add ***** 02/19/04 ***********************************************
    C*
    C*  10. Accumulate Previous Fees Billed
    C*
     C                   when      l0_EAARTY = 'F'
     C                   eval      prvFeeBGrp = prvFeeBGrp + xxamt
    C*
    C*  11. Accumulate Previous Fees Paid
    C*
     C                   when      l0_EAARTY = 'Q'
     C                   eval      prvFeePGrp = prvFeePGrp + xxamt
    C*____________________________________________________________________
    C*
     C                   endsl
    C*
     C                   select
     C                   when      l0_EAARTY = 'I' or l0_EAARTY = 'H'
     C                   eval      depositDue = depositDue + xxamt
     C                   when      l0_EAARTY = 'F' or l0_EAARTY = 'Q'
     C                   eval      feesDue = feesDue + xxamt
     C                   other
     C                   eval      premiumDue = premiumDue + xxamt
     C                   endsl
    C*
     C     eaky          reade     pea00                                  58
    C*
     C                   enddo
    C*
     C                   endsr
     C*
    C*====================================================================
    C* totals - Print total pages
    C*====================================================================
    C*
     C     totals        begsr
     C*
     C                   if        detailRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C***  Begin Add  ***  11/19/09  **************************************
     C                   select
     C                   when      cltID = '025'
     C                   except    head025
     C                   other
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    head
     C***  Begin Add  ***  11/19/09  **************************************
     C                   endsl
     C***  End   Add  ***  11/19/09  **************************************
     C                   except    totalHead
     C                   except    totalSumm
     C                   endif
     C*
     C                   if        summaryRpt = *on
      *** beg add **** 06/24/16 **************************************
     c                              and isUseSpoolOut
      *** end add **** 06/24/16 **************************************
     C                   except    head1
     C                   except    totalHead1
     C                   except    totalSumm1
     C                   endif
    C*
     C                   eval      codes    = codesTtl
     C                   eval      amounts  = amountsTtl
     C                   eval      groupRun = *off
     C*
      *** beg chg ***** 06/24/16 ****************
     C***********        exsr      srARDetail
      /free
       prtRptSummary = *on;
       Exsr srARDetail;
       prtRptSummary = *off;
      /end-free
      *** end chg ***** 06/24/16 ****************
    C*
     C                   endsr
     C*
    C*====================================================================
    C* writeWFARP - Write records to WFARP
    C*====================================================================
    C*
     C     writeWFARP    begsr
     C*
     C                   eval      ELrrn = GetEmpYrRR(EAFYR:EAEMP#:EADIV:
     C                                                EAFND:EACO#)
     C*
     C     EMrrn         chain     pem0p                              60
     C     ELrrn         chain     pel0p                              60
    C*
     C                   clear                   par0p
      ******* beg add ****** 07/07/08 *********************************

      *  ARBSCD - Billing schedule code

     c                   move      elbscd        arbscd

      *  ARBDSC - Bill Sched Code Description
      *  ARSEFL - Billing Sched Code Type

     c                   eval      xx50C = GetBillSch(ELrrn:BSrrn)
     c     BSrrn         chain     wtbsp

     c                   if        %found
     c                   eval      arbdsc = bsdesc
     c                   eval      arsefl = bssefl
     c                   endif
      * zzz
      *  ARABFL - Agency Bill Flag
     c                   eval      arabfl = GetEmpPlan(ELco#:ELfnd:ELemp#:
     c                                      ELdiv:ELfyr:ELpfdt)

      ******* end add ****** 07/07/08 *********************************
     C*
     C** **              eval      ARKEY  = %char(EACO#)  + %char(EAFND)  +
     C** **                                 %char(EAEMP#) + %char(EADIV)  +
     C** **                                 %char(EAFYR)  + %char(EATRDT) +
     C** **                                 %char(EATIME)
     C                   move      EACO#         EACO#_a
     C                   move      EAFND         EAFND_a
     C                   move      EAEMP#        EAEMP#_a
     C                   move      EADIV         EADIV_a
     C                   move      EAFYR         EAFYR_a
     C                   move      EATRDT        EATRDT_a
     C                   move      EATIME        EATIME_a
    C*
     C                   eval      ARKEY  = EACO#_a + EAFND_a + EAEMP#_a +
     C                                      EADIV_a + EAFYR_a + EATRDT_a +
     C                                      EATIME_a
    C*
     C                   eval      ARCO#  = EACO#
     C                   eval      ARCNAM = leftJstify(companyNm)
     C                   eval      ARFND  = EAFND
     C                   eval      ARGNAM = groupName
     C                   eval      ARFYR  = EAFYR
     C                   eval      AREMP# = EAEMP#
     C                   eval      ARDIV  = EADIV
     C                   eval      ARPOL# = getEmpPol#(ELrrn)
     C                   eval      ARENAM = emplrName
     C                   eval      ARARCD = EAARCD
     C                   eval      ARARTY = EAARTY
     C                   eval      ARDESC = getARDesc(EAARCD:rrn:EACO#)
     C                   eval      ARAMT  = EAAMT
     C*
     C                   select
     C                   when      EAARTY = 'P' or EAARTY = 'I'
     C                              or EAARTY = 'Q'
     C                   eval      ARCOLL = EAAMT
     C                   when      EAARTY = 'B' or EAARTY = 'A'
     C                              or EAARTY = 'D' or EAARTY = 'H'
     C                              or EAARTY = 'F'
     C                   eval      ARBILL = EAAMT
     C                   when      EAARTY = 'V' or EAARTY = 'N'
     C                              or EAARTY = 'R'
      *** beg add ***** 02/17/05 ***********************************************
     C                              or EAARTY = 'T'
      *** end add ***** 02/17/05 ***********************************************
     C                   eval      AROTHR = EAAMT
     C                   endsl
     C*
     C                   eval      ARTRDT = EATRDT + cvtDate
     C                   eval      ARPODT = EAPODT + cvtDate
     C                   eval      ARCHK# = EACHK#
     C                   eval      ARNINV = EANINV
     C                   eval      ARLATE = EALATE
     C                   eval      ARFAF  = ELFAF
     C                   eval      ARRST  = RatingSt(ELCO#:ELRTCD)
     C*
     C                   eval      ARRGN# = EMRGN#
     C                   eval      ARRGDS = getRgnDesc(EMRGN#:rrn:EMFND:EMCO#)
     C                   eval      ARCHAP = EMCHAP
     C                   eval      ARCDES = getChapDes(EMCHAP:rrn:EMFND:EMCO#)
     C                   eval      AROFF# = EMOFF#
     C                   eval      AROFNM = getOffName(EMOFF#:rrn:EMCO#)
     C                   eval      ARPFIN = ELPFIN
     C                   eval      ARFNAM = getPrmFNm(ELPFIN:ELPFDT:rrn:
     C                                                ELFND:ELCO#)
     C                   eval      ARPFDT = ELPFDT + cvtDate
     C                   eval      ARPTDT = ELPTDT + cvtDate
     C                   eval      ARCXDT = getCxDate(EMCO#:EMFND:EMEMP#:
     C                                                EMDIV) + cvtDate
     C*
     C                   if        ARCXDT = cvtDate
     C                   clear                   ARCXDT
     C                   endif
    C*
    C*  Set surcharge flag on if a/r type = 1 (1st assessment), 2 (2nd
    C*  assessment), or T (tax)
    C*
     C                   eval      ARCatType = getARCat(EAARCD:rrn:EACO#)
     C                   if        ARCatType = '1' or ARCatType = '2' or
     C                             ARCatType = 'T'
      *** beg add ***** 02/17/05 ***********************************************
     C                             or ARCatType = '3' or ARCatType = '4' or
     C                             ARCatType = '5'
      *** end add ***** 02/17/05 ***********************************************
     C                   eval      ARSUR = 'Y'
     C                   endif
     C*
     C                   eval      ARAGT# = getEmpAgt(EACO#:EAFND:EAEMP#:
     C                                      EADIV:EAFYR:0:agentName:AHrrn)
     C                   eval      ARAGNM = agentName
     C     AHrrn         chain     pah0p                              60
     C                   eval      AR2AGT = AHSAGT
     C                   eval      AR2ANM = getAgtName(AHSAGT:AGrrn:AArrn:
     C                                                 AHFND:AHCO#)
     C*
     C                   eval      ARCAG# = getEmpAgt(EACO#:EAFND:EAEMP#:
     C                                      EADIV:EAFYR:EATRDT:agentName:
     C                                      AHrrn)
     C                   eval      ARCAGN = agentName
     C     AHrrn         chain     pah0p                              60
     C                   eval      ARFEIN = getAgtFein(AHCO#:AHFND:AHAGT#:
     C                                                 agtMod)
     C                   eval      ARMOD  = agtMod
     C                   eval      ARPROD = AHPROD
     C                   eval      ARPRNM = getProdNm(ARFEIN:ARMOD:ARPROD)
     C*
     C                   if        PayComm(EACO#:EAARCD) = *on
     C                   eval      ARCRAT = EmpCommRt(AHrrn:EAARCD:EATRDT)
     C                   endif
     C                   eval      ARCOMM = EAAMT * (ARCRAT/100)
     C*
     C                   eval      AR2CAG = AHSAGT
     C                   eval      AR2CAN = getAgtName(AHSAGT:AGrrn:AArrn:
     C                                                 AHFND:AHCO#)
     C                   eval      AR2FIN = getAgtFein(AHCO#:AHFND:AHSAGT:
     C                                                 agtMod)
     C                   eval      AR2MOD = agtMod
     C                   eval      AR2PRD = AHSPRD
     C                   eval      AR2PNM = getProdNm(AR2FIN:AR2MOD:AR2PRD)
     C*
     C                   if        PayComm(EACO#:EAARCD) = *on
     C                   eval      AR2CRT = EmpCommRt(AHrrn:EAARCD:EATRDT
     C                                               :'2')
     C                   endif
     C                   eval      AR2COM = EAAMT * (AR2CRT/100)
    C*
     C                   eval      ARFRDT = x7ARdt_fr + cvtDate
     C                   eval      ARTODT = x7ARdt_to + cvtDate
    C*
     C     *hms          move      q1time        sys_time
     C     *cymd         move      sys_date      ARDT
     C                   move      sys_time      ARTIME
     C                   eval      ARDT   = ARDT + cvtDate
     C                   eval      ARPGM  = q1pgm
     C                   eval      ARUSER = q1user
      **** BEGIN ADD **** 04/15/02 *****************************************************************
     C                   eval      ARCTYP = EACTYP
      **** END   ADD **** 04/15/02 *****************************************************************
      //*** Beg Add ***** 11/06/23 *********************************************
       ARBtyp = EABtyp;
       ARBDesc = EABDesc;
       ARRDiv = EARDiv;
       ARInvTypId = EAInvTypId;
       ARRef# = EARef#;
       ARBat# = EABat#;
      //*** End Add ***** 11/06/23 *********************************************
    C*
      *** beg chg ***** 06/24/16 *****************
     C***********        write     par0p
      /free
       If loadFile = *on;
          ARxmlFile = *blank;
          Write par0p;
       Endif;
       If isUseXML and detailRpt;
          ARxmlFile = xmlFile;
          Write pxarp3d;
       Endif;
      /end-free
    C*
     C                   endsr
     C*
    C*====================================================================
    C* yrBrk - Set up for new year
    C*====================================================================
    C*
     C     yrBrk         begsr
    C*
     C                   exsr      agentBrk
     C*** beg del **** 07/16/02 *******************************************
    C*****************  if        saveGrp <> *zero and oneGrpRcrd = *on
    C*****************  exsr      grpTotals
    C*****************  eval      *inoa = *on
    C*****************  eval      *inof = *on
    C*****************  endif
     C*** end del **** 07/16/02 *******************************************
    C*
     C     f1ky          chain     pf100                              60
     C*
     C                   clear                   ttlColEmp
     C                   clear                   ttlOthrEmp
     C                   clear                   ttlBillEmp
     C                   clear                   fndTotal
     C                   clear                   prvBalGrp
     C                   clear                   fndANPR
     C*
       //***** Beg Add ***** 01/08/21 **********************************
      /Free
       ttlColGrp  = 0;
       ttlOthrGrp = 0;
       ttlBillGrp = 0;
       prvBalGrp  = 0;
       fndANPR    = 0;
       fndTotal = 0;
      /End-Free
       //***** End Add ***** 01/08/21 **********************************
     C                   eval      saveGrp    = EAFND
     C                   eval      saveYr     = EAFYR
     C*** beg del **** 07/16/02 *******************************************
     C****************** eval      oneGrpRcrd = *off
     C*** end del **** 07/16/02 *******************************************
    C*
     C                   endsr
    C*____________________________________________________________________
    C*
     O/copy scopyrout
    O*
    O*  SELECTION CRITERIA
    O*
     Oqsysprt   e            route          1
     O                                           20 'COMPANY  . . :'
     O                       routeCo#            25
    O*
      ******* beg del ******* 05/28/14 *****************************************
     O*******   e            route          1
     O*******                                    20 'GROUP RANGE  :'
     O*******                routeGrp          +  2
      ******* end del ******* 05/28/14 *****************************************
    O*
     O          e            route          1
     O                                           20 'YEAR RANGE   :'
     O                       routeYr           +  2
    O*
     O          e            route          1
     O                                           20 'DATE RANGE   :'
     O                       frdt_usa          +  2
     O                                         +  1 '-'
     O                       todt_usa          +  1
    O*
     O          e            route          1
     O                                           20 'PRINT EMPS W/ '
     O          e            route          1
     O                                           20 '  NO ACTIVITY:'
     O                       routeNoAct        +  2
    O*
     O          e            route          1
     O                                           20 'SORT BY  . . :'
     O                       routeSort         +  2
    O*
     O          e            route          1
     O                                           20 'AGENT  . . . :'
     O                       routeAgt#         +  2
    O*
     O          e            route          1
     O                                           20 'EMPLOYER/DIV :'
     O                       routeEmpDv        +  2
    O*
     O          e            route          1
     O                                           20 'POLICY PLAN  :'
     O                       routePlan         +  2
    O*
     O          e            route          1
     O                                           20 'REPORT TYPE  :'
     O                       routeRpt          +  2
    O*
     O          e            route       2  1
     O                                         +  8 'A/R CATEGORIES INCLUDED'
     O                                         +  1 'IN CALCULATION'
     O          e            route          1
     O                       ctgryDesc(1)      +  9
     O                       ctgryDesc(2)      +  1
     O                       ctgryDesc(3)      +  1
     O          e       20   route          1
     O                       ctgryDesc(4)      +  9
     O                       ctgryDesc(5)      +  1
     O                       ctgryDesc(6)      +  1
     O          e       20   route          1
     O                       ctgryDesc(7)      +  9
     O                       ctgryDesc(8)      +  1
     O                       ctgryDesc(9)      +  1
     O          e       20   route          1
     O                       ctgryDesc(10)     +  9
    O*
     O          e            route       2  1
     O                       routeAR           +  8
     O          e            route          1
     O                       codeDesc(1)       +  9
     O                       codeDesc(2)       +  1
     O                       codeDesc(3)       +  1
     O          e       21   route          1
     O                       codeDesc(4)       +  9
     O                       codeDesc(5)       +  1
     O                       codeDesc(6)       +  1
     O          e       21   route          1
     O                       codeDesc(7)       +  9
     O                       codeDesc(8)       +  1
     O                       codeDesc(9)       +  1
     O          e       21   route          1
     O                       codeDesc(10)      +  9
     O                       codeDesc(11)      +  1
     O                       codeDesc(12)      +  1
     O          e       21   route          1
     O                       codeDesc(13)      +  9
     O                       codeDesc(14)      +  1
     O                       codeDesc(15)      +  1
     O          e       21   route          1
     O                       codeDesc(16)      +  9
     O                       codeDesc(17)      +  1
     O                       codeDesc(18)      +  1
     O          e       21   route          1
     O                       codeDesc(19)      +  9
     O                       codeDesc(20)      +  1
     O                       codeDesc(21)      +  1
     O          e       21   route          1
     O                       codeDesc(22)      +  9
     O                       codeDesc(23)      +  1
     O                       codeDesc(24)      +  1
     O          e       21   route          1
     O                       codeDesc(25)      +  9
     O                       codeDesc(26)      +  1
     O                       codeDesc(27)      +  1
     O          e       21   route          1
     O                       codeDesc(28)      +  9
     O                       codeDesc(29)      +  1
     O                       codeDesc(30)      +  1
     O*** BEGIN CHG ***  01/28/10  ************************************
     O*****     E            ROUTE          1 59
     O*******                                  +001 '(c)'
     O*******                *YEAR             +001
     O******                                   +001 'StoneRiver, Inc.'
     O******                                   +001 'All rights reserved.'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     O*** END  CHG ***  01/28/10  ************************************
     O*** BEGIN DEL ***  01/28/10  ************************************
     O****      E            ROUTE          1
     O***                                      +001 'StoneRiver is a'
     O***                                      +001 'trademark of'
     O***                                      +001 'StoneRiver, Inc.'
     O*** END DEL *****  01/28/10  ************************************
    O*
    O* Report Heading
    O*
     O          e            head           1 06
     O                       sys_time
     O                       companyNm          105
     O                       sys_date           150
     O          e            head           1
     O                       q1pgm
     O                       ORTITL             105
     O                                          144 'PAGE:'
     O                       page          z    150
     O          e            head           1
     O                       frdt_usa            73
     O                                         +  1 '-'
     O                       todt_usa          +  1
     O***  Begin Add  ***  11/19/09  **************************************
     O          e            head025        1 06
     O                       sys_time
     O                       companyNm          115
     O                       sys_date           170
     O          e            head025        1
     O                       q1pgm
     O                       ORTITL             115
     O                                          164 'PAGE:'
     O                       page          z    170
     O          e            head025        1
     O                       frdt_usa            83
     O                                         +  1 '-'
     O                       todt_usa          +  1
     O***  End   Add  ***  11/19/09  **************************************
    O*
    O* Group Heading
    O*
     O          e            groupHead      2
     O                                              'GROUP:'
     O                       saveGrp       z   +  1
     O                       groupName         +  1
     O                                          106 'POLICY YEAR:'
     O                       saveYr        z   +  1
     O                                          138 'PRINT W/NO ACTIVITY:'
     O                       routeNoAct        +  1
    O*
     O          e            subHead        2
     O                                         + 57 'AGENT:'
     O                       routeAgt#         +  1
     O                                         +  5 'EMPLOYER:'
     O                       routeEmpDv        +  1
    O*
    O* Employer Heading
    O*  This line is printed twice for pinfed printers
    O*
     O          e            empHead        0
      **** begin chg **** 01/04/10 *****************************************
     O                       @@hding
     O************           Heading
      **** end   chg **** 01/04/10 *****************************************
     O               41                         150 'CX DATE'
     O          e            empHead        1
      **** begin chg **** 01/04/10 *****************************************
     O                       @@hding
     O************           Heading
      **** end   chg **** 01/04/10 *****************************************
     O               41                         150 'CX DATE'
    O*
     O          e            line           2
     O                       solidLine
     O***  Begin Add  ***  11/19/09  **************************************
     O          e            line025        2
     O                       solidLine
     O***  End   Add  ***  11/19/09  **************************************
    O*
      *** beg add **** 01/04/10 ************************************************
     O          e    97      empDetail      2
     O                       saveEmp#      z
     O                       saveDiv       z   +  1
     O                       prtEmpNa25        +  1
     O                       savdst#           +  1
     O                       ELPFDT_mdy        +  2
     O                                         +  1 '-'
     O                       ELPTDT_mdy        +  1
     O                       empAnnPrm     a   +  2
     O                       saveAgt#      z   +  2
     O                       prtAgtName        +  1
     O               41                         140 '  '
     O               41      cancelDt           150
    O*
      *** end add **** 01/04/10 ************************************************
      **** begin chg **** 01/04/10 *****************************************
     O          e   N97      empDetail      2
     O********  e            empDetail      2
      *** end chg **** 01/04/10 ************************************************
     O                       saveEmp#      z
     O                       saveDiv       z   +  1
     O                       prtEmpName        +  1
     O                       ELPFDT_mdy        +  2
     O                                         +  1 '-'
     O                       ELPTDT_mdy        +  1
     O                       empAnnPrm     a   +  2
     O                       saveAgt#      z   +  2
     O                       prtAgtName        +  1
     O               41                         140 '  '
     O               41      cancelDt           150
     O          e            empDetail      1
     O                                              'TRANS DATE'
     O                                         +  2 ' POST DATE'
     O                                         +  3 'CD TP'
     O                                         +  2 'DESCRIPTION'
     O                                           67 'CHECK#'
     O                                           88 'COLLECTION'
     O                                          108 'OTHER TRANS'
     O                                          128 'BILLING'
     O                                          148 'BALANCE'
     O***  Begin Add  ***  11/19/09  **************************************
     O          e            empDtl025      0
     O                                          170 'STATEMENT#'
     O***  End   Add  ***  11/19/09  **************************************
    O*
    O* Previous Balance Line
    O*
     O          e            prvBalDetl     1
     O                                           49 'PREVIOUS BALANCE:'
     O                       prvBal        a    150
    O*
    O* Detail Line
    O*
     O          e            detail         1
     O                       EATRDT_usa
     O                       EAPODT_usa       +   2
     O                       EAARCD           +   3
     O                       EAARTY           +   2
     O                       EADESC           +   2
     O                       EACHK#        z     67
     O                       colEmp        a     90
     O                       othrEmp       a    110
     O                       billEmp       a    130
     O                       currentBal    a    150
     O***  Begin Add  ***  11/19/09  **************************************
     O          e            detail025      0
     O                       EANINV        z    170
     O***  End   Add  ***  11/19/09  **************************************
    O*
    O* Total Line for Employer
    O*
     O          e            empTotal    1  1
     O                                           70 'TOTAL:'
     O                       ttlColEmp     a     90
     O                       ttlOthrEmp    a    110
     O                       ttlBillEmp    a    130
     O                       currentBal    a    150
     O          e            empTotal       1
     O                                           43 'DEPOSIT DUE'
     O                       depositDue    a   +  5
     O          e            empTotal       1
     O                                           43 'FEES    DUE'
     O                       feesdue       a   +  5
     O          e            empTotal       1
     O                                           43 'PREMIUM DUE'
     O                       premiumDue    a   +  5
     O          e            empTotal       2
     O                                           43 'TOTAL   DUE'
     O                       totalDue      a   +  2
    O*
    O* Group Total Page
     O*
     O          e            groupSumm      2
     O                                              'TOTAL ANNUAL PREMIUM:'
     O                       fndANPR       a   +  2
     O                                          123 'GROUP TOTAL:'
     O                       fndTotal      a   +  2
     O          e            groupSumm      2
     O                                         +  5 '** GROUP SUMMARY **'
     O          e            groupSumm      1
     O                                         +  5 'ADJUSTMENTS       '
     O                       prvAdjGrp     a   +  2
     O          e            groupSumm      1
     O                                         +  5 'BILLS             '
     O                       prvBillGrp    a   +  2
     O          e            groupSumm      1
     O                                         +  5 'AUDITS            '
     O                       prvAdtGrp     a   +  2
     O          e            groupSumm      1
     O                                         +  5 'NSFS              '
     O                       prvNSFGrp     a   +  2
     O          e            groupSumm      1
     O                                         +  5 'PAYMENTS          '
     O                       prvPmntGrp    a   +  2
     O          e            groupSumm      1
     O                                         +  5 'VOIDS             '
     O                       prvVoidGrp    a   +  2
     O          e            groupSumm      1
     O                                         +  5 'REFUNDS           '
     O                       prvRfndGrp    a   +  2
     O          e            groupSumm      1
      *** beg add ***** 02/19/04 ***********************************************
     O                                         +  5 'TRUE VOIDS        '
     O                       prvTrVdGrp    a   +  2
     O          e            groupSumm      1
      *** end add ***** 02/19/04 ***********************************************
     O                                         +  5 'DEPOSITS REQUESTED'
     O                       prvDepBGrp    a   +  2
     O          e            groupSumm      1
     O                                         +  5 'DEPOSITS RECEIVED '
     O                       prvDepPGrp    a   +  2
     O          e            groupSumm      1
     O                                         +  5 'FEES              '
     O                       prvFeeBGrp    a   +  2
     O          e            groupSumm      2
     O                                         +  5 'FEE PAYMENTS      '
     O                       prvFeePGrp    a   +  2
     O          e            groupSumm      2
     O                                         +  5 'PREVIOUS BALANCE  '
     O                       prvBalGrp     a   +  2
     O          e            groupSumm      1
     O                                         +  5 'ADJUSTMENTS       '
     O                       adjGrp        a   +  2
     O          e            groupSumm      1
     O                                         +  5 'BILLS             '
     O                       billGrp       a   +  2
     O          e            groupSumm      1
     O                                         +  5 'AUDITS            '
     O                       depGrp        a   +  2
     O          e            groupSumm      1
     O                                         +  5 'NSFS              '
     O                       NSFGrp        a   +  2
     O          e            groupSumm      1
     O                                         +  5 'PAYMENTS          '
     O                       pmntGrp       a   +  2
     O          e            groupSumm      1
     O                                         +  5 'VOIDS             '
     O                       voidGrp       a   +  2
     O          e            groupSumm      1
     O                                         +  5 'REFUNDS           '
     O                       rfndGrp       a   +  2
     O          e            groupSumm      1
      *** beg add ***** 02/19/04 ***********************************************
     O                                         +  5 'TRUE VOIDS        '
     O                       trVdGrp       a   +  2
     O          e            groupSumm      1
      *** end add ***** 02/19/04 ***********************************************
     O                                         +  5 'DEPOSITS REQUESTED'
     O                       depBGrp       a   +  2
     O          e            groupSumm      1
     O                                         +  5 'DEPOSITS RECEIVED '
     O                       depPGrp       a   +  2
     O          e            groupSumm      1
     O                                         +  5 'FEES              '
     O                       feeBGrp       a   +  2
     O          e            groupSumm      2
     O                                         +  5 'FEE PAYMENTS      '
     O                       feePGrp       a   +  2
     O          e            groupSumm      1
     O                                         +  5 'GROUP TOTAL       '
     O                       fndTotal      a   +  2
    O*
    O* Final Total Page
    O*
     O          e            totalHead      2
      ******* beg del ******* 05/28/14 *****************************************
     O*******                                  + 30 'GROUP RANGE:'
     O*******                routeGrp          +  1
      ******* end del ******* 05/28/14 *****************************************
     O                                         +  5 'YEAR RANGE:'
     O                       routeYr           +  1
     O                                         +  5 'AGENT:'
     O                       routeAgt#         +  1
     O                                         +  5 'EMPLOYER:'
     O                       routeEmpDv        +  1
    O*
     O          e            totalSumm      2
     O                                              'TOTAL ANNUAL PREMIUM:'
     O                       ttlANPR       a   +  1
     O                                          127 'TOTAL:'
     O                       total         a   +  1
     O          e            totalSumm      2
     O                                           24 '** TOTAL SUMMARY **'
     O          e            totalSumm      1
     O                                         +  5 'ADJUSTMENTS       '
     O                       prvAdjTtl     a   +  2
     O          e            totalSumm      1
     O                                         +  5 'BILLS             '
     O                       prvBillTtl    a   +  2
     O          e            totalSumm      1
     O                                         +  5 'AUDITS            '
     O                       prvAdtTtl     a   +  2
     O          e            totalSumm      1
     O                                         +  5 'NSFS              '
     O                       prvNSFTtl     a   +  2
     O          e            totalSumm      1
     O                                         +  5 'PAYMENTS          '
     O                       prvPmntTtl    a   +  2
     O          e            totalSumm      1
     O                                         +  5 'VOIDS             '
     O                       prvVoidTtl    a   +  2
     O          e            totalSumm      1
     O                                         +  5 'REFUNDS           '
     O                       prvRfndTtl    a   +  2
      *** beg add ***** 02/19/04 ***********************************************
     O          e            totalSumm      1
     O                                         +  5 'TRUE VOIDS        '
     O                       prvTrVdTtl    a   +  2
      *** end add ***** 02/19/04 ***********************************************
     O          e            totalSumm      1
     O                                         +  5 'DEPOSITS REQUESTED'
     O                       prvDepBTtl    a   +  2
     O          e            totalSumm      1
     O                                         +  5 'DEPOSITS RECEIVED '
     O                       prvDepPTtl    a   +  2
     O          e            totalSumm      1
     O                                         +  5 'FEES              '
     O                       prvFeeBTtl    a   +  2
     O          e            totalSumm      2
     O                                         +  5 'FEE PAYMENTS      '
     O                       prvFeePTtl    a   +  2
     O          e            totalSumm      2
     O                                         +  5 'PREVIOUS BALANCE  '
     O                       prvBalTtl     a   +  2
     O          e            totalSumm      1
     O                                         +  5 'ADJUSTMENTS       '
     O                       adjTtl        a   +  2
     O          e            totalSumm      1
     O                                         +  5 'BILLS             '
     O                       billTtl       a   +  2
     O          e            totalSumm      1
     O                                         +  5 'AUDITS            '
     O                       depTtl        a   +  2
     O          e            totalSumm      1
     O                                         +  5 'NSFS              '
     O                       NSFTtl        a   +  2
     O          e            totalSumm      1
     O                                         +  5 'PAYMENTS          '
     O                       pmntTtl       a   +  2
     O          e            totalSumm      1
     O                                         +  5 'VOIDS             '
     O                       voidTtl       a   +  2
     O          e            totalSumm      1
     O                                         +  5 'REFUNDS           '
     O                       rfndTtl       a   +  2
      *** beg add ***** 02/19/04 ***********************************************
     O          e            totalSumm      1
     O                                         +  5 'TRUE VOIDS        '
     O                       trVdTtl       a   +  2
      *** end add ***** 02/19/04 ***********************************************
     O          e            totalSumm      1
     O                                         +  5 'DEPOSITS REQUESTED'
     O                       depBTtl       a   +  2
     O          e            totalSumm      1
     O                                         +  5 'DEPOSITS RECEIVED '
     O                       depPTtl       a   +  2
     O          e            totalSumm      1
     O                                         +  5 'FEES              '
     O                       feeBTtl       a   +  2
     O          e            totalSumm      2
     O                                         +  5 'FEE PAYMENTS      '
     O                       feePTtl       a   +  2
     O          e            totalSumm      1
     O                                         +  5 'TOTAL             '
     O                       total         a   +  2
    O*
    O* A/R Summary Heading
    O*
     O          e            ARHead      2  1
     O                                           29 '* * *   A / R   T Y P E'
     O                                           53 'S U M M A R Y   * * * '
     O          e            ARHead         2
     O                                           17 'CODE    TYPE'
     O                                           31 'DESCRIPTION'
     O                                           67 'TOTAL'
    O*
    O* A/R Summary Detail Line
    O*
     O          e            ARDetail       1
     O                       xCode                8
     O                       xCodeType           15
     O                       xCodeDesc           50
     O                       xAmount       a     69
    O*
    O* A/R TOTAL LINE
    O*
     O          e            ARTotal     1  1
     O                                           48 'TOTAL:'
     O                       summaryTtl    a   +  2
    O*
    O*  SELECTION CRITERIA NOT FOUND
    O*
     O          e            noRec       1
     O                                              '*** NO RECORDS MEET'
     O                                          + 1 'SELECTION CRITERIA ***'
    O*____________________________________________________________________
    O*
     O/copy scopyrout1
    O*
    O*  SELECTION CRITERIA
    O*
     Oqsysprt1  e            route1         1
     O                                           20 'COMPANY  . . :'
     O                       routeCo#            25
    O*
      ******* beg del ******* 05/28/14 *****************************************
     O*******   e            route1         1
     O*******                                    20 'GROUP RANGE  :'
     O*******                routeGrp          +  2
      ******* end del ******* 05/28/14 *****************************************
    O*
     O          e            route1         1
     O                                           20 'YEAR RANGE   :'
     O                       routeYr           +  2
    O*
     O          e            route1         1
     O                                           20 'DATE RANGE   :'
     O                       frdt_usa          +  2
     O                                         +  1 '-'
     O                       todt_usa          +  1
    O*
     O          e            route1         1
     O                                           20 'PRINT EMPS W/ '
     O          e            route1         1
     O                                           20 '  NO ACTIVITY:'
     O                       routeNoAct        +  2
    O*
     O          e            route1         1
     O                                           20 'SORT BY  . . :'
     O                       routeSort         +  2
    O*
     O          e            route1         1
     O                                           20 'AGENT  . . . :'
     O                       routeAgt#         +  2
    O*
     O          e            route1         1
     O                                           20 'EMPLOYER/DIV :'
     O                       routeEmpDv        +  2
    O*
     O          e            route1         1
     O                                           20 'POLICY PLAN  :'
     O                       routePlan         +  2
    O*
     O          e            route1         1
     O                                           20 'REPORT TYPE  :'
     O                       routeRpt          +  2
    O*
      *** beg add ***** 12/07/04 ***********************************************
     O          e            route1         1
     O                                           20 'BILLING TYPE :'
     O                       billingtype       +  2
      *** end add ***** 12/07/04 ***********************************************
    O*
     O          e            route1      2  1
     O                                         +  8 'A/R CATEGORIES INCLUDED'
     O                                         +  1 'IN CALCULATION'
     O          e            route1         1
     O                       ctgryDesc(1)      +  9
     O                       ctgryDesc(2)      +  1
     O                       ctgryDesc(3)      +  1
     O          e       20   route1         1
     O                       ctgryDesc(4)      +  9
     O                       ctgryDesc(5)      +  1
     O                       ctgryDesc(6)      +  1
     O          e       20   route1         1
     O                       ctgryDesc(7)      +  9
     O                       ctgryDesc(8)      +  1
     O                       ctgryDesc(9)      +  1
     O          e       20   route1         1
     O                       ctgryDesc(10)     +  9
    O*
     O          e            route1      2  1
     O                       routeAR           +  8
     O          e            route1         1
     O                       codeDesc(1)       +  9
     O                       codeDesc(2)       +  1
     O                       codeDesc(3)       +  1
     O          e       21   route1         1
     O                       codeDesc(4)       +  9
     O                       codeDesc(5)       +  1
     O                       codeDesc(6)       +  1
     O          e       21   route1         1
     O                       codeDesc(7)       +  9
     O                       codeDesc(8)       +  1
     O                       codeDesc(9)       +  1
     O          e       21   route1         1
     O                       codeDesc(10)      +  9
     O                       codeDesc(11)      +  1
     O                       codeDesc(12)      +  1
     O          e       21   route1         1
     O                       codeDesc(13)      +  9
     O                       codeDesc(14)      +  1
     O                       codeDesc(15)      +  1
     O          e       21   route1         1
     O                       codeDesc(16)      +  9
     O                       codeDesc(17)      +  1
     O                       codeDesc(18)      +  1
     O          e       21   route1         1
     O                       codeDesc(19)      +  9
     O                       codeDesc(20)      +  1
     O                       codeDesc(21)      +  1
     O          e       21   route1         1
     O                       codeDesc(22)      +  9
     O                       codeDesc(23)      +  1
     O                       codeDesc(24)      +  1
     O          e       21   route1         1
     O                       codeDesc(25)      +  9
     O                       codeDesc(26)      +  1
     O                       codeDesc(27)      +  1
     O          e       21   route1         1
     O                       codeDesc(28)      +  9
     O                       codeDesc(29)      +  1
     O                       codeDesc(30)      +  1
     O          E            ROUTE1         1 59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE1         1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
    O*
    O* Report Heading
    O*
     O          e            head1          1 06
     O                       sys_time
     O                       companyNm          120
     O                       sys_date           180
     O          e            head1          1
     O                       q1pgm
     O                       ORTITL1            120
     O                                          174 'PAGE:'
     O                       page1         z    180
     O          e            head1          1
     O                       frdt_usa            88
     O                                         +  1 '-'
     O                       todt_usa          +  1
    O*
    O* Group Heading
    O*
     O          e            groupHead1     2
     O                                              'GROUP:'
     O                       saveGrp       z   +  1
     O                       groupName         +  1
     O                                          135 'POLICY YEAR:'
     O                       saveYr        z   +  1
     O                                          175 'PRINT W/NO ACTIVITY:'
     O                       routeNoAct        +  1
    O*
     O          e            subHead1       2
     O                                         + 72 'AGENT:'
     O                       routeAgt#         +  1
     O                                         +  5 'EMPLOYER:'
     O                       routeEmpDv        +  1
    O*
    O* Agent Heading
    O*
     O          e            agentHead1     2
     O                                              'AGENT:'
     O                       saveAgt#      z   +  1
     O                       prtAgtName        +  1
    O*
    O* Employer Heading
    O*  This line is printed twice for pinfed printers
    O*
     O          e            empHead1       0
     O                       Heading1
      *** beg chg **** 07/16/02 ********************************************************
     O************** 41                          77 'CX DATE'
     O               41                          80 'CX DATE'
      *** end chg **** 07/16/02 ********************************************************
     O          e            empHead1       1
     O                       Heading1
      *** beg chg **** 07/16/02 ********************************************************
     O************** 41                          77 'CX DATE'
     O               41                          80 'CX DATE'
      *** end chg **** 07/16/02 ********************************************************
    O*
     O          e            line1          2
     O                       solidLine1
    O*
     O          e            empDetail1     1
     O                       saveEmp#      z
     O                       saveDiv       z   +  1
     O                       prtEmpNam1        +  1
     O                       ELPFDT_mdy        +  1
     O                                         +  1 '-'
     O                       ELPTDT_mdy        +  1
     O                       empAnnPrm     a   +  1
     O               41      cancelDt          +  1
     O                       prvBal        a   +  1
     O                       ttlColEmp     a   +  1
     O                       ttlOthrEmp    a   +  1
     O                       ttlBillEmp    a   +  1
     O                       currentBal    a   +  1
    O*
    O* Group Total Page
    O*
     O          e            grpTotal1      0
     O                                         + 50 '      ANNUAL PREM  '
     O                                         + 12 ' PREVIOUS BALANCE  '
     O                                         +  1 '      COLLECTIONS  '
     O                                         +  1 '      OTHER TRANS  '
     O                                         +  1 '         BILLINGS  '
     O                                         +  1 '          BALANCE  '
     O          e            grpTotal1      1
     O                                         + 50 '      ANNUAL PREM  '
     O                                         + 12 ' PREVIOUS BALANCE  '
     O                                         +  1 '      COLLECTIONS  '
     O                                         +  1 '      OTHER TRANS  '
     O                                         +  1 '         BILLINGS  '
     O                                         +  1 '          BALANCE  '
     O          e            grpTotal1      2
     O                       solidLine1
     O          e            grpTotal1      0
     O                                              'TOTALS:'
     O          e            grpTotal1      1
     O                       fndANPR       a   + 50
     O                       prvBalGrp     a   + 12
     O                       ttlColGrp     a   +  1
     O                       ttlOthrGrp    a   +  1
     O                       ttlBillGrp    a   +  1
     O                       fndTotal      a   +  1
    O*
     O          e            totalHead1     2
      ******* beg del ******* 05/28/14 *****************************************
     O*******                                  + 40 'GROUP RANGE:'
     O*******                routeGrp          +  1
      ******* end del ******* 05/28/14 *****************************************
     O                                         +  5 'YEAR RANGE:'
     O                       routeYr           +  1
     O                                         +  5 'AGENT:'
     O                       routeAgt#         +  1
     O                                         +  5 'EMPLOYER:'
     O                       routeEmpDv        +  1
     O*
     O          e            groupSumm1     2
     O                                              'TOTAL ANNUAL PREMIUM:'
     O                       fndANPR       a   +  2
     O                                          150 'GROUP TOTAL:'
     O                       fndTotal      a   +  2
     O          e            groupSumm1     2
     O                                         +  5 '** GROUP SUMMARY **'
     O          e            groupSumm1     1
     O                                         +  5 'ADJUSTMENTS       '
     O                       prvAdjGrp     a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'BILLS             '
     O                       prvBillGrp    a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'AUDITS            '
     O                       prvAdtGrp     a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'NSFS              '
     O                       prvNSFGrp     a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'PAYMENTS          '
     O                       prvPmntGrp    a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'VOIDS             '
     O                       prvVoidGrp    a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'REFUNDS           '
     O                       prvRfndGrp    a   +  2
     O          e            groupSumm1     1
      *** beg add ***** 02/19/04 ***********************************************
     O                                         +  5 'TRUE VOIDS        '
     O                       prvTrVdGrp    a   +  2
     O          e            groupSumm1     1
      *** beg add ***** 02/19/04 ***********************************************
     O                                         +  5 'DEPOSITS REQUESTED'
     O                       prvDepBGrp    a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'DEPOSITS RECEIVED '
     O                       prvDepPGrp    a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'FEES              '
     O                       prvFeeBGrp    a   +  2
     O          e            groupSumm1     2
     O                                         +  5 'FEE PAYMENTS      '
     O                       prvFeePGrp    a   +  2
     O          e            groupSumm1     2
     O                                         +  5 'PREVIOUS BALANCE  '
     O                       prvBalGrp     a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'ADJUSTMENTS       '
     O                       adjGrp        a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'BILLS             '
     O                       billGrp       a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'AUDITS            '
     O                       depGrp        a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'NSFS              '
     O                       NSFGrp        a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'PAYMENTS          '
     O                       pmntGrp       a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'VOIDS             '
     O                       voidGrp       a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'REFUNDS           '
     O                       rfndGrp       a   +  2
      *** beg add **** 02/19/04 ************************************************
     O          e            groupSumm1     1
     O                                         +  5 'TRUE VOIDS        '
     O                       trVdGrp       a   +  2
      *** end add **** 02/19/04 ************************************************
     O          e            groupSumm1     1
     O                                         +  5 'DEPOSITS REQUESTED'
     O                       depBGrp       a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'DEPOSITS RECEIVED '
     O                       depPGrp       a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'FEES              '
     O                       feeBGrp       a   +  2
     O          e            groupSumm1     2
     O                                         +  5 'FEE PAYMENTS      '
     O                       feePGrp       a   +  2
     O          e            groupSumm1     1
     O                                         +  5 'GROUP TOTAL       '
     O                       fndTotal      a   +  2
    O*
    O* Final Total Page
    O*
     O          e            totalSumm1     2
     O                                              'TOTAL ANNUAL PREMIUM:'
     O                       ttlANPR       a   +  1
     O                                          157 'TOTAL:'
     O                       total         a   +  1
     O          e            totalSumm1     2
     O                                           24 '** TOTAL SUMMARY **'
     O          e            totalSumm1     1
     O                                         +  5 'ADJUSTMENTS       '
     O                       prvAdjTtl     a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'BILLS             '
     O                       prvBillTtl    a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'AUDITS            '
     O                       prvAdtTtl     a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'NSFS              '
     O                       prvNSFTtl     a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'PAYMENTS          '
     O                       prvPmntTtl    a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'VOIDS             '
     O                       prvVoidTtl    a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'REFUNDS           '
     O                       prvRfndTtl    a   +  2
      *** beg add ***** 02/19/04 ***********************************************
     O          e            totalSumm1     1
     O                                         +  5 'TRUE VOIDS        '
     O                       prvTrVdTtl    a   +  2
      *** end add ***** 02/19/04 ***********************************************
     O          e            totalSumm1     1
     O                                         +  5 'DEPOSITS REQUESTED'
     O                       prvDepBTtl    a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'DEPOSITS RECEIVED '
     O                       prvDepPTtl    a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'FEES              '
     O                       prvFeeBTtl    a   +  2
     O          e            totalSumm1     2
     O                                         +  5 'FEE PAYMENTS      '
     O                       prvFeePTtl    a   +  2
     O          e            totalSumm1     2
     O                                         +  5 'PREVIOUS BALANCE  '
     O                       prvBalTtl     a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'ADJUSTMENTS       '
     O                       adjTtl        a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'BILLS             '
     O                       billTtl       a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'AUDITS            '
     O                       depTtl        a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'NSFS              '
     O                       NSFTtl        a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'PAYMENTS          '
     O                       pmntTtl       a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'VOIDS             '
     O                       voidTtl       a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'REFUNDS           '
     O                       rfndTtl       a   +  2
      *** beg add ***** 02/19/04 ***********************************************
     O          e            totalSumm1     1
     O                                         +  5 'TRUE VOIDS        '
     O                       trVdTtl       a   +  2
      *** end add ***** 02/19/04 ***********************************************
     O          e            totalSumm1     1
     O                                         +  5 'DEPOSITS REQUESTED'
     O                       depBTtl       a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'DEPOSITS RECEIVED '
     O                       depPTtl       a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'FEES              '
     O                       feeBTtl       a   +  2
     O          e            totalSumm1     2
     O                                         +  5 'FEE PAYMENTS      '
     O                       feePTtl       a   +  2
     O          e            totalSumm1     1
     O                                         +  5 'TOTAL             '
     O                       total         a   +  2
    O*
    O* A/R Summary Heading
    O*
     O          e            ARHead1     2  1
     O                                           29 '* * *   A / R   T Y P E'
     O                                           53 'S U M M A R Y   * * * '
     O          e            ARHead1        2
     O                                           17 'CODE    TYPE'
     O                                           31 'DESCRIPTION'
     O                                           67 'TOTAL'
    O*
    O* A/R Summary Detail Line
    O*
     O          e            ARDetail1      1
     O                       xCode                8
     O                       xCodeType           15
     O                       xCodeDesc           50
     O                       xAmount       a     69
    O*
    O* A/R TOTAL LINE
    O*
     O          e            ARTotal1    1  1
     O                                           48 'TOTAL:'
     O                       summaryTtl    a   +  2
    O*
    O*  SELECTION CRITERIA NOT FOUND
    O*
     O          e            noRec1      1
     O                                              '*** NO RECORDS MEET'
     O                                          + 1 'SELECTION CRITERIA ***'
    O*





     P*====================================================================
     P* chkEmpAct  - Check if there is any activity within the date range
     P*====================================================================
     P*
     P chkEmpAct       b
     P*
     D chkEmpAct       PI             1

     D empAct          s              1
    C*
    C* CHECK TO SEE IF THERE ARE ANY TRANSACTIONS WITHIN THE DATE
    C*   RANGE.  ONLY PROCESS AN EMPLOYER IF THERE ARE TRANSACTIONS.
    C*
     C     eaky_1        setll     pea00
     C     eaky          reade     pea00                                  50
    C*
     C                   eval      empAct = not *in50 and
     C                                      l0_EAxxDT <= x7ARdt_to
     C*
     C                   return    empAct

     P chkEmpAct       e





     P*=====================================================================
     P* rmvZeros - remove leading zeros
     P*=====================================================================
     P*
     PrmvZeros         B
     P*
     DrmvZeros         PI         32766    opdesc
     D fieldToRmv                 32766    options(*varsize) const
     D
     D startPoint      s              5  0
     D returnFld       s                   like(fieldToRmv)

     C                   eval      returnFld = *blanks
     C*
 001 C     '0'           check     fieldToRmv:1  startPoint
     C*
 002 C                   eval      %subst(returnFld:startPoint) =
     C                             %subst(fieldToRmv:startPoint)
     C*
     C                   return    returnFld

     PrmvZeros         E

      *** beg add ***** 06/24/16 ****************
     pWrtWXARP1        b
      /free
       ARXMLFILE   = xmlFile;
       ARCO#       = xxCo#;
       ARCNAM      = leftJstify(companyNm);
       ARPNAM      = Q1pgm;
       ARTITL      = ORtitl;
       ARRNDT      = DateTo8(sys_Date);
       ARRNTM      = TimeTo6(sys_Time);
       ARRSFRDT    = DateTo8(frdt_usa);
       ARRSTODT    = DateTo8(todt_usa);
       ARAGTSLCT   = routeAgt#;
       AREMPSLCT   = routeEmpDv;
       ARYEARSLCT  = routeYr;
       ARNOEMSLCT  = routeNoAct;
       ARSORTSLCT  = routeSort;
       ARPLANSLCT  = routePlan;
       ARRTYPSLCT  = routeRpt;
       ARTTLANPR   = ttlAnPr;
       ARRPTTTL    = total;
       ARPRADJTTL  = prvAdjTtl;
       ARPRBILTTL  = prvBillTtl;
       ARPRADTTTL  = prvAdtTtl;
       ARPRNSFTTL  = prvNSFTtl;
       ARPRPAYTTL  = prvPmntTtl;
       ARPRVODTTL  = prvVoidTtl;
       ARPRREFTTL  = prvRfndTtl;
       ARPRTVDTTL  = prvTrVdTtl;
       ARPRDPBTTL  = prvDepBTtl;
       ARPRDPPTTL  = prvDepPTtl;
       ARPRFEBTTL  = prvFeeBTtl;
       ARPRFEPTTL  = prvFeePTtl;
       ARPRBALTTL  = prvBalTtl;
       ARCRADJTTL  = adjTtl;
       ARCRBILTTL  = billTtl;
       ARCRADTTTL  = depTtl;
       ARCRNSFTTL  = NSFTtl;
       ARCRPAYTTL  = pmntTtl;
       ARCRVODTTL  = voidTtl;
       ARCRREFTTL  = rfndTtl;
       ARCRTVDTTL  = trVdTtl;
       ARCRDPBTTL  = depBTtl;
       ARCRDPPTTL  = depPTtl;
       ARCRFEBTTL  = feeBTtl;
       ARCRFEPTTL  = feePTtl;
       ARCRBALTTL  = total;
       ARARCDMSG   = routeAR;
       xard        = codeDesc;
       xct         = category;
       xctd        = ctgryDesc;
       Write pxarp1;
      /end-free
     pWrtWXARP1        e
     pWrtWXARP2        b
      /free
       ARXMLFILE  = xmlFile;
       ARCO#      = xxCo#;
       ARFND      = saveGrp;
       ARGRPNAME  = groupName;
       ARFYR      = saveYr;
       ARPRADJGRP = prvAdjGrp;
       ARPRBILGRP = prvBillGrp;
       ARPRADTGRP = prvAdtGrp;
       ARPRNSFGRP = prvNsfGrp;
       ARPRPAYGRP = prvPmntGrp;
       ARPRVODGRP = prvVoidGrp;
       ARPRREFGRP = prvRfndGrp;
       ARPRTVDGRP = prvTrVdGrp;
       ARPRDPBGRP = prvDepBGrp;
       ARPRDPPGRP = prvDepPGrp;
       ARPRFEBGRP = prvFeeBGrp;
       ARPRFEPGRP = prvFeePGrp;
       ARPRBALGRP = prvBalGrp;
       ARCRADJGRP = adjGrp;
       ARCRBILGRP = billGrp;
       ARCRADTGRP = depGrp;
       ARCRNSFGRP = nsfGrp;
       ARCRPAYGRP = pmntGrp;
       ARCRVODGRP = voidGrp;
       ARCRREFGRP = rfndGrp;
       ARCRTVDGRP = trVdGrp;
       ARCRDPBGRP = depBGrp;
       ARCRDPPGRP = depPGrp;
       ARCRFEBGRP = feeBGrp;
       ARCRFEPGRP = feePGrp;
       ARCRBALGRP = fndTotal;
       Write pxarp2;
      /end-free
     pWrtWXARP2        e
     pWrtWXARP3        b
      /free
       ARXMLFILE  = xmlFile;
       ARCO#      = xxCo#;
       ARFND      = saveGrp;
       ARFYR      = saveYr;
       AREMP#     = saveEmp#;
       ARDIV      = saveDiv;
       ARENAM     = emplrName;
       ARPFDT     = dateTo8(ELPFDT_mdy);
       ARPTDT     = dateTo8(ELPTDT_mdy);
       ARANPREM   = empAnnPrm;
       ARAGT#     = saveAgt#;
       ARAGTNAM   = prtAgtName;
       ARCXDT     = dateTo8(cancelDt);
       ARPRVBAL   = prvBal;
       AREMPCOLTT = ttlColEmp;
       AREMPOTHTT = ttlOthrEmp;
       AREMPBILTT = ttlBillEmp;
       AREMPCURTT = currentBal;
       AREMPDEPDU = depositDue;
       AREMPFEEDU = feesDue;
       AREMPPRMDU = premiumDue;
       AREMPTOTDU = totalDue;
       Write pxarp3;
      /end-free
     pWrtWXARP3        e
     pWrtWXARPAC       b
      /free
       ARXMLFILE  = xmlFile;
       ARCO#      = xxCo#;
       ARARCD     = xCode;
       ARARTYPE   = xCodeType;
       ARARCDDES  = xCodeDesc;
       ARARCDAMT  = xAmount;
       Select;
       When prtGrpYrSumm;
          ARFND      = saveGrp;
          ARFYR      = saveYr;
          ARLVLFLG   = 'GrpYr';
       When prtRptSummary;
          ARFND      = *zero;
          ARFYR      = *zero;
          ARLVLFLG   = 'Rpt';
       Endsl;
       Write pxarpac;
      /end-free
     pWrtWXARPAC       e
      *** end add ***** 06/24/16 ****************

**  Sort/Desc
AAGENT SEQUENCE
EEMPLOYER NUMBER SEQUENCE
NEMPLOYER NAME SEQUENCE
**  Plan/Desc
AALL POLICIES
EEXCLUDE ACCT CURRENT
CACCT CURRENT ONLY
**  Plan/Desc
DDETAIL
SSUMMARY
BDETAIL/SUMMARY
**  ARCode/Desc
 A/R CODES INCLUDED IN CALCULATION
ISELECTED CODES ARE INCLUDED IN CALCULATION
ESELECTED CODES ARE EXCLUDED FROM CALCULATION
CCOMMISSION CODES ONLY ARE INCULDED IN CALCULATION
