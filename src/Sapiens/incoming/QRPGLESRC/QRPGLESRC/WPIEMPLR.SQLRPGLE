     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H nomain
     H option(*nodebugio:*srcstmt)
      *********************************************************************
      * REVISIONS:
      *
      * 11/10/25 C:062 Support: 119674(099)
      *                If name not found in prod, check in quote.
      *
      * 02/27/25 C:300 Support: 118580 (AmWins)
      *                Copy ARAP Charge (ELARP1/2/3) only to MA Divisions
      *
      * 11/06/24 C:300 Support: 117931 (Prescient)
      *                Interim Audit Sample Not Balancing
      *                Some corrections in OvrRenewalCx procedure
      *
      * 04/29/24 C:333 Ticket: 115436 (150)
      *                Safety Fee defaulting on states not configured
      *
      * 09/14/23 C:62  Ticket: 113477(129)
      *                Add GetEmpEnv to tell where employer exists.
      *
      * 08/30/23 C:65  Project: 150WFY (Phw#113607)
      *                Reduce a negative discount to zero so it does not
      *                cause any reporting issues - amending 8/17/23 change.
      *
      * 08/17/23 C:65  Project: 150WFY (Phw#113607)
      *                Cap a negative percentage so it does not exceed
      *                -100% in the RtvDeductible procedure.
      *
      * 07/07/23 C:300 Project: WC2810
      *                  Add GetEmpBalToMin
      *
      * 12/13/22 C:333 Ticket: 105535(129)
      *                Processing cancellation issue.
      *                - For Employer no. in 7 or more digits.
      *
      * 05/20/22 C:062 Ticket: 102001(150)
      *                Use maintenance fields instead of init fields.
      *
      * 04/01/22 C:291 Support: 101527
      *                Retrieve Large deductible values from Group Setup
      *                except amouts. Retrive amounts from Master PEO or
      *                State Reporting divisin for non-Peo.
    H*
    H* 03/22/22 C:136 Project: CN028
    H*                Add join to office info in SqlWithPolicyInfo
    H*
    H*  02/22/22  C:62  Project: 150WFN
    H*                  Add GetNameAddress
    H*
    H*  01/19/22  C:33  Project: WC2837
    H*                  Add IsUseNetRate
    H*
    H*  12/09/21  C:33  Support: 99529
    H*                  Increase throughput
      *
      * 11/17/21 C:136 Support: 99564
      *                WC2812 changed WDELP to Update. Need to change
      *                all chain/read op codes to non-locking.
      *
      * 11/15/21 C:136 Project: 150WFM
      *                Add logic to import PEO policy into quote
      *                environment.
      *
      * 10/19/21 C:33  Support: 98816
      *                Ensure ChkCxStatus returns a value
      *
      * 08/24/21 C:291 Project: WC2812
      *                Add DefaultRatingFactors
      *
      * 07/01/21 C:33  Support: 96462
      *                Add DeleteEmployer
      *
      * 03/04/21 C:33  Support: 94441
      *                Update CHKFUTDTL to ignore quote for claims only
      *
      * 12/14/20 C:33  Project: SY406
      *                Add CxJSONData
      *
      * 11/18/20 C:180 Project WC2757
      *                Recompiled only.
      *
      * 10/06/20 C:33  Project: SY406 {No Comments}
      *                Update PolicyJSONData to break out division info
      *
      * 07/08/20 C:33  Project: SY409 {No Comments}
      *                Update PolicyJSONData to include "billingPlan"
      *
      * 06/30/20 C:33  Project: SY409 {No Comments}
      *                Update PolicyJSONData to include 'Expired'
      *
      * 06/22/20 C:33  Project: SY406
      *               -Add SetFQLockSts
      *               -Add UpdFQLockSts
      *
      * 03/16/20 C:136 Project: WC2700
      *                Add check to override non-renewal cancellations
      *                with OvrRenewalCx.
      *
      * 01/15/20 C:295 Support: 80383
      *                To fix GetStRptPDays fail for non-statereporting and
      *                flat cancelled div while runninf Audit bill on same.
      *                                                                    *
      * 01/22/20 C:33  Project: SY406
      *                Add SqlWithPolicyInfo
      *
      * 12/19/19 C:33  Project: SY409 {No Comments}
      *                Add to PolicyJSONData            .
      *                - "multipleDivisions"
      *                - "aging"
      *
      * 10/15/19 C:136 SY406 (DE546)
      *                Correct GetRateSet so that it can populate co#
      *                if passed in and user doesn't have a default co#
      *
      * 05/29/19 C:65  Project: SY407
      *                Add PolicyJSONData for generating json of policy data.
      *                                                                    *
      *  04/22/19  C:33  Project: WC2610
      *                  Add GetScheduleRatingFactor
      *                                                                    *
      *  03/04/19  C:33  Project SY390
      *                  Soft-code 'Portal'
      *
      * 02/11/19 C:267 Project: SY390_1221
      *                Check for file open prior to executing file override
      *                for WMEMLQ in 'GetFQLockSts'.
      *
      * 01/14/19 C:33  Project: SY407
      *                Add 'QuoteStsChgHst'
      *
      * 12/18/18 C:33  Project: SY406
      *                Allow for 'Portal' lockout
      *
      * 06/26/18 C:136 Project: 150WAQ
      *                Change to SQLRPGLE and added GetColManPrmAdj procedure
      *
      * 06/21/18 C:33  Support: 71743
      *                Update Last Division to use 5.0
      *
      * 06/11/18 C:65  Project: 150WAQ
      *                Add procedure for calculating collateral and
      *                writing the amounts to A/R Batch.
      *
      * 05/30/18 C:65  Project: WC2607
      *                Add procedure for deductible information.
      *
      * 05/15/18 C:291 Project: 150WCR
      *                Modify EdtPEOPolType procedure to allow manual
      *                override of Master vs. MCP state setup.
      *
      * 08/01/17 C:062 Project: 150WAP
      *                Add routine to edit for the PEO policy type.
      *
      * 06/19/17 C:136 PhaseWare: 46320
      *                Make GetStPRptDays bulletproof (changes not commented)
      *        !!!!!    W8010R and W7720R must be included if this is EBF'd  !!!!!
      *                 Get default info from PEO Master if applicable
      *                 Return days from division being processed if
      *                  state reporting division does not exist.
      *                Change GetStRptDv
      *                 Correct return division to a 5,0
      *
      * 04/03/17 C:136 PhaseWare: 46414
      *               -Adjust ChkFutDtl to check for future year in live and
      *                quote to pass back a message if called from the policy
      *                toolbar
      *               -Adjust ChkQteEmp to pass back message if
      *                called from policy tool bar.
      *
      * 01/24/17 C:65  PhaseWare: 45365
      *                Change GetStRptPDays to use policy expiration date
      *                if cx date is in a future policy year.
      *
      * 01/10/17 C:136 PhaseWare: 45005
      *                Change ChkFutDtl to check future policies by
      *                policy from date. Changes not commented.
      *
      * 01/09/17 C:136 PhaseWare: 44983
      *                Correct ChkRenewal's logic. Changes not commented.
      *
      * 05/04/16 C:300 Phaseware: 40913
      *                Policy renewal displays empty message
      *                Fix ChkFutDtl to return *off in case of no error
      *
      * 04/06/16 C:136 Phaseware: 40646
      *                Add subprocedure to get policy days for
      *                state reporting division
      *
      * 03/24/16 C:136 Phaseware: 40447
      *                Fix ChkPolDates so that it checks only the
      *                state reporting divisions dates
      *
      * 02/10/16 C:242 PW37116                                        *
      *                DBA/Fed Names and Links for Quote not working  *
      *                correctly.                                     *
      *                                                               *
      * 01/28/16 C:65  Project: Phw# 39607 (CSI - 114WKA)
      *                Perform a file open/close on submission history
      *                to allow temporary overrides to file to be deleted.
      *
      * 12/14/15 C:65  Project: Phw#38862 - LUBA (Ref WC2254)
      *                Correct the future year READE in ChkFutDtl.
      *
      * 09/17/15 C:302 Project: WC2254
      *                Procedurwe to check if it is a renewal
      *
     * 06/23/15 C:136 Project: WC2182                                *
     *                Add various edit routines for Single Employer  *
     *                Renewal                                        *
     *                                                               *
      * 05/25/15 C:295 Project: WC2332
      *                Recompilation only
      *
      * 05/22/15 C:279 Project: 114WKA
      *                Add procedure PolPageProcessed to determine if a
      *                policy page is genearted.
      *
      * 04/02/15 C:279 Project: WC2356
      *                Add procedure MultiState to determine if a policy
      *                is a multi-state policy.
      *
      * 03/16/15 C:062 Support: 33798 (089)
      *                Add procedure to retrieve last division used.

      * 12/29/14 C:283 Support: RedMine 4809
      *                Add procedure to check the FQ lock on an Employer.
      *
      * 07/23/14 C:320 Support 85709
      *                Do not return "Deleted Division" message unless
      *                endorsement flag is set to 'Y and no re-instate
      *                date present.  Otherwise it means the division
      *                has been re-intated.    Program clean-up.
      *                Free form the whole GetCxRsn procedure.

      * 02/27/14 C:283 Support: WA099 84925
      *                Added procedure GetQteMode; to fetch if the employer
      *                is in Quote or Live environment.
      *                Added RtvEnviLib which returns the library in which
      *                the job is running presently.
      *                Corrected GetEmpName to fetch the correct name as per
      *                the environment ie. Quote Or Live.
      *                Corrected GetEmpYrRr to fetch the correct RRN number
      *                based on the environment ie. Quote Or Live.

      * 02/10/14 C:267 Project: Red3768
      *                Set Quote status to correct value based on value in
      *                QSUSED in quote status table WTQSL. Do this only
      *                when policy status is 'T' for transferred.

      * 08/01/13 C:136 Project: C147WAI
      *                Return total premium accumulated from all divisions

      * 06/14/13 C:283 Support WA146 82469
      *                - Correctly populate the fields with the Status Code
      *                  so that correct status appears on the search screen.

      * 05/31/13 C:63  Redmine #2644
      *                - CoverageLapse - Determine if there is a lapse in
      *                                   coverage

      * 04/03/13 C:62  Redmine: 2530
      *                Correct the open and close of WDELLQ/1               is

      * 02/15/13 C:65  Project: WC2116
      *                - GetCxRsn - if reason is blank and endorsement flag is
      *                  set to 'Y' then return 'Deleted Division' for reason.

      * 01/11/13 C:62  Project: WC2116
      *                - getEmpYrRR - Add Quote processing

      * 11/16/12 C:65  Project: WC2116
      *                - GetEmpPlan - Get Large Deductible, Retrospective
      *                  Rating, or Scheduled Rating status for div or policy.

      * 10/16/12 C:190 Project: WC2116
      *                - GetRnwlQte - Get Renewal Quote Flag
      *                - GetRnwlQteDiv - Get Renew to Quote Division Flag

      * 10/07/12 C:062 Project: WC2115
      *                - GetExclMed - Get exclude medical flag

      * 10/04/12 C:062 Project: WC2116
      *                - GetEmpYrRr - Correct RRN retrieval for Quote

      * 09/28/12 C:190 Project: WC2115
      *                - GetIncLimit - Add Additional parameters for Description.

      * 07/09/12 C:62  Project: WC2115
      *                Add RtvPolYr, RtvPolFrDt, RtvPolToDt, RtvMedExcl

      * 05/16/12 C:33  Project: WC2116
      *                Add RtvQuoteLib

      * 05/15/12 C:65  Project: WC2116
      *                Add parm to EmpYrRR procedure to allow for year selection.

      * 05/14/12 C:189 Project: WC2115
      *                Add procedures:
      *                - GetIncLimit

      * 04/12/12 C:33  Project: WC2116
      *                Add procedures:
      *                - GetRatingState
      *                - GetQuoteSts
      *                - GetQuoteStsDesc

      * 03/11/12 C:180 V11 install support
      *          C:33  Chg Getcxdtate so it does not return a cx date
      *                when an employer is not found.

      * 03/06/12  C:189 Project: WC2115
      *                 Added a new procedure to fetch Employer Phone# and
      *                 area code

      * 03/07/12  C:190 Project: WC2116
      *                 Add new procedures to fetch full name of employer

      * 08/03/11 C:136 Support: WA025 77556
      *                Chg GetCxDate so that it doesn't return a cx date
      *                if the latest inception date doesn't have an
      *                associated cancellation date.

      * 11/10/10  C:33  Project: 139WPL
      *                 Add check for HCN Program.

      * 01/07/09  C:33  Seq#: WA099/66660
      *                 Account for months prior to October (month 10)

      * 12/12/08 C: 220 SUPPORT: 66397
      *                - Change dsDate format to be *ISO compatable

      * 03/08/06 C: 132 PROJECT: WC071 - 51160
      *                - Change *DATE to use The Current Date from DSDTTM
      *                  Data Structure DSDATE.

      * 12/11/02  C:61  SUPPORT WA081 35144
      *                 Get correct CX reason code record.

      *********************************************************************

     Fwmeml     if   e           k disk    rename(pem01:pem00) infds(emfds)
     Fwmemlq    if   e           k disk    rename(pem01:pem0q) usropn
     F*** Begin Add *** 11/10/25 *******************************************
     F                                     infds(emfdsq)
     F*** End   Add *** 11/10/25 *******************************************
     Fwmeml3    if   e           k disk    rename(pem01:pem03) usropn
     F                                     infds(emfds3) Prefix(L3_)
     Fwdell1    if   e           k disk                        infds(el1fds)
     Fwdell1q   if   e           k disk    rename(pel01:pel0q1) usropn
     F                                     infds(el1fdsQ)
     Fwdell2    if   e           k disk    rename(pel01:pel02)
     Fwdell2q   if   e           k disk    rename(pel01:pel0q2) usropn
     Fwzexl     if   e           k disk    rename(pex01:pex00)
     Fwzexl1    if   e           k disk    rename(pex01:pex01)
     Fwmpfl     if   e           k disk    rename(ppf01:ppf00) infds(pffds)
     Fwdcxl1    if   e           k disk
     Fwmarl     if   e           k disk    rename(par01:par00)
     Fwtcrl     if   e           k disk    rename(pcr01:pcr00)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
      *** Beg Chg ***** 08/24/21 **************************
     F**wdelp     if   e             disk    rename(pel01:pel0p)
     Fwdelp     uf   e             disk    rename(pel01:pel0p)
      *** End Chg ***** 08/24/21 **************************
     Fwmahp     if   e             disk    rename(pah01:pah0p)
     Fwdhcl     if   e           k disk    rename(phc01:phc00) infds(HCfds)
     Fwdell     if   e           k disk    rename(pel01:pel00) infds(el2fds)
     Fwdellq    if   e           k disk    rename(pel01:pel0q) usropn
     F                                     infds(el2fdsQ)
     Fwaqtl7    if   e           k disk    rename(pqt01:pqt07)
     Fwaqtp     uf a e             disk    rename(pqt01:pqt0p) usropn
     Fwtqsl1    if   e           k disk
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     Fwdrrl     if   e           k disk    rename(prr01:prr00)
     FWTLML1    IF   E           K DISK    rename(PLM01:PLM01)
     Fwdcxl4    if   e           k disk    rename(pcx01:pcx04)
     FWTABL     if   e           k disk
     fwdell8    if   e           k disk    rename(pel01:pel08)
      *** Beg Add ***** 08/24/21 **************************
     fwdell8q   if   e           k disk    rename(pel01:pel08q) usropn
      *** End Add ***** 08/24/21 **************************
     Fwdf1l1    if   e           k disk
     fwmshl1    if   e           k disk    usropn
     Fwtddl2    if   e           k disk    rename(pdd01:pdd02)
      *** Beg Chg ***** 08/24/21 **************************
     F***wmldl1    if   e           k disk
     Fwmldl1    uf a e           k disk
     Fwmldl3    if   e           k disk    rename(pld01:pld03)
     Fwmldl1q   if   e           k disk    rename(pld01:pld01q) usropn
      *** End Chg ***** 08/24/21 **************************
     Fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     Fwdf1l     if   e           k disk    rename(pf101:pf100)
     Fwmcol1    if   e           k disk    rename(pco01:pco01)
     Fwdeal     if   e           k disk
     F                                     RENAME(PEA01:PEA01B)
     Fwdeal4    uf a e           k disk
     F                                     RENAME(PEA01:PEA04B)
      **** beg add ***** 03/16/20 **************************
     fwdxhl15   if   e           k disk    Rename(pxh01:pxh15)
     fwdehl11   if   e           k disk    Rename(peh01:peh11)
     fwdehl12   if   e           k disk    Rename(peh01:peh12)
      **** end add ***** 03/16/20 **************************
      *** Beg Add *****  07/07/23  *******************************************
      * Admin/Dart Control Definition File
     Fwmexl     if   e           k disk    rename(pex01:pex0m)
      * Emlpoyer Premium Details File
     Fwdmpl     if   e           k disk
      * Emlpoyer Payroll Work File
     Fwmepl1    if   e           k disk
      *** End Add *****  07/07/23  *******************************************

      ***  Begin Add  ***  07/01/21  ***********************
     di0               s             10i 0 inz
     dkey              s             50a   varying
     dnode0            s                   Like(yajl_val)
     dstrErrorMsg      s            500a   Varying
     dval              s                   Like(yajl_val)
      ***  End   Add  ***  07/01/21  ***********************

     D emfds           ds
     D em_rrn                397    400b 0
     D*** Begin Add *** 11/10/25 *******************************************
     D emfdsq          ds
     D em_rrnq               397    400b 0
     D*** End   Add *** 11/10/25 *******************************************
     D emfds3          ds
     D em_rrn3               397    400b 0
     D el1fds          ds
     D el1_rrn               397    400b 0
     D el1fdsq         ds
     D el1q_rrn              397    400b 0
     D el2fds          ds
     D el2_rrn               397    400b 0
     D el2fdsQ         ds
     D el2Q_rrn              397    400b 0
     D pffds           ds
     D pf_rrn                397    400b 0
     D tbfds           ds
     D tb_rrn                397    400b 0
     D HCfds           ds
     D HC_rrn                397    400b 0
      *
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D                 DS
     D  SDC                    1     20
     D                                     DIM(10)
     D  DDDC01                 1      2
     D  DDDC02                 3      4
     D  DDDC03                 5      6
     D  DDDC04                 7      8
     D  DDDC05                 9     10
     D  DDDC06                11     12
     D  DDDC07                13     14
     D  DDDC08                15     16
     D  DDDC09                17     18
     D  DDDC10                19     20
     D                 DS
     D  LDC                    1     20
     D                                     DIM(10)
     D  LDR101                 1      2
     D  LDR102                 3      4
     D  LDR103                 5      6
     D  LDR104                 7      8
     D  LDR105                 9     10
     D  LDR106                11     12
     D  LDR107                13     14
     D  LDR108                15     16
     D  LDR109                17     18
     D  LDR110                19     20
     D*
     D*** Begin Add ***  08/24/21  ***********************************
     DdsWDELL8         ds                  likerec(pel08:*input) inz
     DdsWDELL8Q        ds                  likerec(pel08Q:*input) inz
     DdsWDELLQ         ds                  likerec(pel0Q:*input) inz
     DdsWDELL          ds                  likerec(pel00:*input) inz
     DdsWDRRLc         ds                  likerec(PRR00:*input) inz
     DdsWDRRLs         ds                  likerec(PRR00:*input) inz
     DdsWMLDL1         ds                  likerec(pld01:*input) inz
     DclientID         s              3    inz('000')
     D*** End   Add ***  08/24/21  ***********************************
     D*** Begin Add ***  04/01/22  ***********************************
     DdsWMLDL1Q        ds                  likerec(pld01q:*input) inz
     D*** End   Add ***  04/01/22  ***********************************
     D*** Begin Add *** 02/22/22 *******************************************
     d SRaddr          pr                  extpgm('SRADDR')
01   D  inCompany#Chr                 3    Const
02   D  inGroup#Chr                   3    Const
03   D  inEmployer#Chr...
03   D                                9    Const
04   D  inDivisionChr                 5    Const
38   D  inOrder                       1
39   D  inNameOrder                   1
40   D  inAddressOrder...
40   D                                1
05   D  outEmployerName1...
     D                               50
06   D  outEmployerName2...
     D                               50
07   D  outEmployerName3...
     D                               50
08   D  outEmployerName4...
     D                               50
09   D  outEmployerName5...
     D                               50
10   D  outEmployerName6...
     D                               50
11   D  outEmployerName7...
     D                               50
12   D  outAddressLocation...
     D                                3  0
13   D  outNameLocation...
     D                                3  0
14   D  outFederalNameLocation...
     D                                3  0
15   D  outPEOName1                  57
16   D  outPEOName2                  57
41   D  inPEOGroupFlag...
41   D                                1
17   D  outContactLocation...
     D                                3  0
18   D  outAMName1                   60
19   D  outAMName2                   60
20   D  outAMName3                   60
21   D  outAMName4                   60
22   D  outAMName5                   60
23   D  outAMName6                   60
24   D  outAMName7                   60
25   D  outAMName8                   60
26   D  outAMName9                   60
27   D  outPEOLocation...
27   D                                3  0
42   D  inPrintDBALabel...
     D                                1
28   D  outAddressListCount...
     D                                3  0
29   D  outNameListCount...
     D                                3  0
30   D  outFedNameListCount...
     D                                3  0
31   D  outPEONameListCount...
     D                                3  0
32   D  outContactListCount...
     D                                3  0
43   D  inPEOExtension...
     D                               10
33   D  outScripturaName1...
     D                              120
33   D  outScripturaName2...
33   D                              120
33   D  outScripturaName3...
33   D                              120
33   D  outScripturaName4...
33   D                              120
37   D  outInsuredFein...
37   D                                9  0
     D*** End   Add *** 02/22/22 *******************************************
     d*
     d* SReffd prototype
     d SReffd          pr                  extpgm('SREFFD')
     d   ##CO#                        3
     d   ##FND                        3
     d   ##FYR                        3
     d   ##EMP#                       9
     d   ##DIV                        5
     d   ##Date                       7

     DdtNull           s               d
     DstrCommand       s            255

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     D***  Begin Add  ***  12/09/21  ****************
     DQp0zLprintf      pr            10I 0 extProc('Qp0zLprintf')
     D szOutputStg                     *   value options(*string)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D                                 *   value options(*string:*nopass)
     D***  End   Add  ***  12/09/21  ****************

     D*** Begin Add *** 08/24/21 ***********************************
     D* Get client ID
     D GetClientID     pr                  extpgm('RTVCLTID')
     D   outCltID                     3
     D*** End   Add *** 08/24/21 ***********************************

     D/copy sprdate
     D/copy sPrFiles
     D***  Begin Add  ***  06/22/20  ************************
     D/copy sPrJobInfo
     D***  End   Add  ***  06/22/20  ************************
      ***  Begin Add  ***  07/01/21  ***********************
     D/copy sPrLock
     D/copy sPrMessage
      ***  End   Add  ***  07/01/21  ***********************
     D/copy sPrModule
     D/copy sPrRunCmd
       //***  Begin Add  ***  03/04/21  ***********************
     D/copy sPrTable
       //***  End   Add  ***  03/04/21  ***********************
     D/copy wprusrprms
     D/copy wprgroup
     D/copy wpremplr
     D/copy wPrAgent
     D/copy wprcarrier
     D/copy wprrateset
     D/copy wprtable
     d/copy sPrGenPop
     d/copy sPrSecure
     d/copy sPrAtrium
     d/copy sPrJson
     d/copy sPrSql
      /Include yajl_h

     D psds          esds                  extname(szq1p)

     DconCantRenew     c                   'Cannot Renew -'
     DconNotRenewed    c                   'Not Renewed -'
      /Free
       Exec Sql Set Option Commit = *None;
      /End-free

     P*=================================================================
     P* CoverageLapse - Determine if there is a lapse in coverage
     P*
     P*  CoverageLapse - returns *on if lapse, otherwise *off
     P*   inCo#      - accepts the current company          (required)
     P*   inGrp      - accepts the current group            (required)
     P*   inEmp#     - accepts the current employer         (required)
     P*   inDiv      - accepts the current division         (required)
     P*   inFyr      - accepts the current year             (required)
     P*   inQte      - accepts the environment              (required)
     P*   inEffDate  - accepts the current effective date   (optional)
     P*   inXRefGrp  - accepts the x-ref employer           (optional)
     P*   inXRefEmp# - accepts the x-ref division           (optional)
     P*   inXRefDiv  - accepts the x-ref year               (optional)
     P*====================================================================
     PCoverageLapse    b                   export
     DCoverageLapse    pi             1n
     D inCo#                          3  0 const
     D inGrp                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inFyr                          3  0 const
     D inQte                          1    const
     D inEffDate                       d   const options(*nopass)
     D inXRefGrp                      3  0 const options(*nopass)
     D inXRefEmp#                     9  0 const options(*nopass)
     D inXRefDiv                      5  0 const options(*nopass)

     D intXRefGrp      s              3  0 inz
     D intXRefEmp#     s              9  0 inz
     D intXRefDiv      s              5  0 inz
     D intYr           s              3  0 inz
     D dtEffDate       s               d   inz

     C     keyWDELL      klist
     C                   kfld                    inCo#
     C                   kfld                    inGrp
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    intYr

     C     key1WDELL     klist
     C                   kfld                    inCo#
     C                   kfld                    intXRefGrp
     C                   kfld                    intXRefEmp#
     C                   kfld                    intXRefDiv
     C                   kfld                    intYr

     C     keyWZEXL      klist
     C                   kfld                    inCo#
     C                   kfld                    inGrp
     C                   kfld                    inEmp#
     C                   kfld                    inDiv

      /Free

       If not %open(WDELLQ);
          strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
           %trim(RtvQuoteLib('*LIV')) + '/WDELLQ)';
          Callp QCMDEXC(%trim(strCommand):%len(strCommand));
          Open wdellq;
       Endif;

       // If no Effective Date is sent in, get from the file
       Select;
       When %parms >= 7 and inEffDate <> dtNull;
          dtEffDate = inEffDate;
       Other;
          intYr = inFyr;
          Chain keyWDELL wdell;
          dtEffDate = CvtToDate(ELpfDt);
       Endsl;

       // If no Previous Employer is sent in, check cross refference
       Select;
       When %parms > 7 and inXRefGrp <> 0;
          intXRefGrp  = inXRefGrp;
          intXRefEmp# = inXRefEmp#;
          intXRefDiv  = inXRefDiv;
       Other;
          Clear EXoFnd;
          Clear EXoEmp;
          Clear EXoDiv;
          Chain keyWZEXL wzexl;
          intXRefGrp  = EXoFnd;
          intXRefEmp# = EXoEmp;
          intXRefDiv  = EXoDiv;
       Endsl;

       // Get prior year information
       Clear ELptDt;
       // Read prior year for current
       intYr = inFyr - 1;
       Chain keyWDELL wdell;
       // If not exist Read prior year for current in Live
       If not %found and inQte = 'Q';
          Chain keyWDELL wdellq;
       Endif;
       // If not exist Read prior year for xRef
       If not %found;
          Chain key1WDELL wdell;
       Endif;
       // If not exist Read prior year for xRef in Live
       If not %found and inQte = 'Q';
          Chain key1WDELL wdellq;
       Endif;

       If %open(WDELLQ);
          Close wdellq;
       Endif;

       // If inEffDate is more than 1 day from ELptDt, return *on
       Select;
       When ELptDt = *zero;
          Return *off;
       When %diff(dtEffDate:CvtToDate(ELptDt):*days) > 1;
          Return *on;
       Other;
          Return *off;
       Endsl;

      /End-Free

     PCoverageLapse    e

     P***  Begin Add  ***  08/24/21  ***********************************
     P*====================================================================
     P* DefaultRatingFactors - Default Rating Factors
     P*
     P*    accepts--
     P*     rrn:       - rrn of WDELP   (required)
     P*     Entry Type - Entry type, Curent Rate code and
     P*                  Current Billing plan  (required)
     P*====================================================================

     PDefaultRatingFactors...
     P                 b                   export

     DDefaultRatingFactors...
     D                 pi
     D inRrn                          9  0 const
     D inEntryType                   30    const

     D flgFirstOccrSt  s              1    inz('N')
     D flgLargeDeduct  s              1n   inz(*off)
     D flgAddPolicy    s              1n   inz(*off)
     D flgAddDivision  s              1n   inz(*off)
     D flgStRptDivDft  s              1    inz('N')
      *** Beg add ***** 04/01/22 ***********************
     D isUseDsDefault  s              1n
      *** End add ***** 04/01/22 ***********************
     D intStRptDiv     s              5  0
     D intCxDate       s              7  0
     D statePeoType    s              1
     D strKey          s             10
     D strCurState     s              2
     D strDftState     s              2
     D strMode         s             10
       //*** beg add ***** 11/15/21 ***************
       Dcl-S flgPeoType Char(1);
       Dcl-S isQteImport Ind;
       //*** end add ***** 11/15/21 ***************

      /Free
       Dcl-Ds dsEntry Qualified Inz;
        entryType Char(20);
        rateCode Char(3);
        billPlan Packed(3:0);
       End-Ds;

       dsEntry = inEntryType;
       flgFirstOccrSt = 'N';
       flgStRptDivDft = 'N';
       flgLargeDeduct = *Off;
       flgAddPolicy = *Off;
       flgAddDivision = *Off;

       Select;
       When %subst(dsEntry.entryType:1:3) = 'QTE';
          strMode = 'Quote';
       When %subst(dsEntry.entryType:1:3) = 'IMP';
          strMode = 'Import';
       When %subst(dsEntry.entryType:1:3) = 'LIV';
          strMode = 'Live';
       Endsl;

       GetClientID(clientID);

       Chain(n) inRrn pel0p;
       Chain (ELco#:ELfnd) pfn00;

       strKey = %editc(dsEntry.billPlan : 'X');
       Chain(n) (ELco#:strKey) pbs01;
       flgLargeDeduct = *off;
       If BSlpgm = 'L';
          flgLargeDeduct = *on;
       Endif;

        //Get state reporting division
        If FNpeof = *Blank;
           intStRptDiv = GetStRptDv(ELco#:Elfnd:Elemp#);
        Else;
           intStRptDiv = 0;
        Endif;

        //Get Current state
        Chain (ELCO#:dsEntry.rateCode) prs00;
        strCurState = RSjrst;
        //Get Rating and Statistics for current(new) state
        Reset dsWDRRLc;
        Setgt (ELco#:ELfnd:strCurState:ELpfdt) PRR00;
        Readpe (ELco#:Elfnd:strCurState) PRR00 dsWDRRLc;

       // Get the type of PEO if Combined PEO
       statePeoType = *Blank;
       If FNpeoF = 'C';
          statePeoType = GetStatePeoFlag(ELfnd:dsEntry.rateCode:
           ELpfDt:ELco#);
       EndIf;

       //*** beg add ***** 11/15/21 ***********************
       // Simplify use of PEO flag...
       Select;
       When FNpeof = 'C';
          flgPeoType = statePeoType;
       Other;
          flgPeoType = FNpeof;
       EndSl;
       //*** end add ***** 11/15/21 ***********************

       //---------------------------------------------------------------
       //Entry type:
       // QTE Add Division  - Add division: Quote entry, Quote Review
       // LIV Add Division  - Add Division: Policy toolbar
       // IMP Add Division  - Add Division: From PEO Import
       // QTE Add Policy    - Add Quote to PEO or Add Quote to non-Peo
       //                     Quote Entry for Master PEO or Combined Peo with
       //                     State Peo Type 'S' will be treated as Adding a Division
       // LIV Add Policy    - Add Policy
       // IMP Add Policy    - Add Policy through PEO Policy Import
       //                     Add Policy through PEO Import for Combined Peo with
       //                     State Peo Type 'S' will be treated as Adding a Division
       //---------------------------------------------------------------
       //*** beg chg ***** 11/15/21 ***********************
       //Select;
       //When dsEntry.entryType = 'QTE Add Division' or            //Add div: Quote entry, Quote rev
       // dsEntry.entryType = 'LIV Add Division' or                  //Add Div: Policy toolbar
       // dsEntry.entryType = 'IMP Add Division' or               //Add Div:Pol toolbar,Mid-term End
       // dsEntry.entryType = 'QTE Add Policy' and FNpeof = 'S' or       //Add Div to Master PEO
       // dsEntry.entryType = 'QTE Add Policy' and statePeoType = 'S'or //Add Div to PEO type Master
       // dsEntry.entryType = 'IMP Add Policy' and statePeoType = 'S';  //Add Div: Import, PEO typ S
       //   flgAddDivision = *On;
       //   Exsr AddDivDefaults;
       //When dsEntry.entryType = 'QTE Add Policy' or                    //Add Quote
       // dsEntry.entryType = 'LIV Add Policy' or                        //Add Policy
       // dsEntry.entryType = 'IMP Add Policy';                          //Add Policy through Import
       //   flgAddPolicy = *On;
       //   Exsr AddPolDefaults;
       //Endsl;

       Select;
       When dsEntry.entryType = 'IMP Add Qte Policy' And flgPeoType = 'M';
          isQteImport = *On;
          flgAddPolicy = *On;
          Exsr AddPolDefaults;
       When dsEntry.entryType = 'IMP Add Qte Policy' And flgPeoType = 'S';
          isQteImport = *On;
          flgAddDivision = *On;
          Exsr AddDivDefaults;
       When dsEntry.entryType = 'IMP Add Qte Div';
          isQteImport = *On;
          flgAddDivision = *On;
          Exsr AddDivDefaults;
       When dsEntry.entryType = 'QTE Add Division' or // Quote entry, Quote review
        dsEntry.entryType = 'LIV Add Division' or     // Policy toolbar, M/T End
        dsEntry.entryType = 'IMP Add Division' or     // Live PEO Import
        dsEntry.entryType = 'QTE Add Policy' and flgPeoType = 'S'; // Add Div to Master PEO
          flgAddDivision = *On;
          Exsr AddDivDefaults;
       When dsEntry.entryType = 'QTE Add Policy' or   // Add Quote
        dsEntry.entryType = 'LIV Add Policy' or       // Add Policy
        dsEntry.entryType = 'IMP Add Policy';         // Live PEO Import
          flgAddPolicy = *On;
          Exsr AddPolDefaults;
       Endsl;
       //*** end chg ***** 11/15/21 ***********************

       //---------------------------------------------------------------
       Begsr AddDivDefaults;
       //---------------------------------------------------------------
        //Get Current state or State reporting division WDELP record to default
        If FNpeof = 'S' or statePeoType = 'S';
           Exsr GetPEOEmpDtlToDft;
        Else;
           Exsr GetEmpDtlToDft;
        Endif;

        //Default Factors for Division Add from first occurrence State or State Reporting Division
        Chain inRrn pel0p;
        If %found(wdelp);
           //Manual Premium%
           ELmanP = dsWDELL8.ELmanP;
           //Client Use Flag
           ELfFl2 = dsWDELL8.ELfFl2;
           //Comm Mod Multiplier
           ELcMul = dsWDELL8.ELcMul;
           //Deductible discount
           ELcxBl = dsWDELL8.ELcxBl;
           ELdCde = dsWDELL8.ELdCde;
           ELdCd2 = dsWDELL8.ELdCd2;
           ELdCEf = dsWDELL8.ELdCEf;
           ELdCEx = dsWDELL8.ELdCEx;
           EL2DEf = dsWDELL8.EL2DEf;
           EL2DEx = dsWDELL8.EL2DEx;
           If ELdCde <> *Blanks;
              ELdCEf = ELpfdt;
           Endif;
           If ELdCd2 <> *Blanks;
              EL2DEf = ELpfdt;
           Endif;
           Exsr ChkDeductibleCode;
           //Contractors Adj Credit
           ELasp1 = dsWDELL8.ELasp1;
           ELcrEf = dsWDELL8.ELcrEf;
           ELcrEx = dsWDELL8.ELcrEx;
           If ELasp1 <> *zeros;
              ELcref = ELpfdt;
           Endif;
           //Stabilization Factor
           ELstFc = dsWDELL8.ELstFc;
           //Subject premium credit
           ELaspM = dsWDELL8.ELaspM;
           ELmcEf = dsWDELL8.ELmcEf;
           ELmcEx = dsWDELL8.ELmcEx;
           ELaspS = dsWDELL8.ELaspS;
           ELscEf = dsWDELL8.ELscEf;
           ELscEx = dsWDELL8.ELscEx;
           If ELaspM <> *zeros;
              ELmcef = ELpfdt;
           Endif;
           If ELaspS <> *zeros;
              ELscEf = ELpfdt;
           Endif;
           //Experience Modifier
           ELexM2 = dsWDELL8.ELexM2;
           ELeMD2 = dsWDELL8.ELeMD2;
           ELexM3 = dsWDELL8.ELexM3;
           ELeMD3 = dsWDELL8.ELeMD3;
           //Arap #
           //*** Beg Add ***** 02/27/25 ******************************************
           If strCurState = 'MA';
           //*** End Add ***** 02/27/25 ******************************************
           ELaRp1 = dsWDELL8.ELaRp1;
           ELaRp2 = dsWDELL8.ELaRp2;
           ELaRp3 = dsWDELL8.ELaRp3;
           //*** Beg Add ***** 02/27/25 ******************************************
           Endif;
           //*** End Add ***** 02/27/25 ******************************************
           //Exp Mod  1 Not Final Flag
           ELexMd1Nf = dsWDELL8.ELexMd1Nf;
           ELexMd2Nf = dsWDELL8.ELexMd2Nf;
           ELexMd3Nf = dsWDELL8.ELexMd3Nf;
           //Stock Discount code
           ELasp3 = dsWDELL8.ELasp3;
           ELdisc = dsWDELL8.ELdisc;
           ELdisP = dsWDELL8.ELdisP;
           If ELasp3 = *Zeros;
              Clear Eldisc;
              If dsWDRRlc.RRdisc <> *Blank;
                 ELdisc = dsWDRRlc.RRdisc;
              Else;
                 ELasp3 = dsWDRRlc.RRdspc;
              Endif;
           Endif;
           //Membership Fee
           ELmFee = dsWDELL8.ELmFee;
           //Facultative Reinsurance
           ELarCp = dsWDELL8.ELarCp;
           ELarCa = dsWDELL8.ELarCa;       //Flat Amount
           ELarEf = dsWDELL8.ELarEf;
           ELarEx = dsWDELL8.ELarEx;
           If ELarCp <> *zeros;
              ELarEf = ELpfdt;
           Endif;
           //Vol Labor Charge Amt
           ELvLCa = dsWDELL8.ELvLCa;
           If FNpeof <> *Blank;
              //Mco Type Code/Mco Group Code
              ELmcCd = dsWDELL8.ELmcCd;
              ELmcGp = dsWDELL8.ELmcGp;
              //Internal Promulgation
              ELprom = dsWDELL8.ELprom;
           Else;
              Clear ELmcCd;
              Clear ELmcGp;
              Clear ELprom;
           Endif;
           //Waiver Type/Waiver %/Waiver Flat Charge
           ELwvTp = dsWDELL8.ELwvTp;
           ELwvPc = dsWDELL8.ELwvPc;
           ELfChg = dsWDELL8.ELfChg;
           ELwMin = dsWDELL8.ELwMin;    //Minimum Waiver Surcharge
           ELwMax = dsWDELL8.ELwMax;    //Waiver Max Chrg
           ELwvFl = dsWDELL8.ELwvFl;    //Waiver Surchrg Prem Lvl
           //Safety Fee Percent/Min Amount
           //*** Beg Chg ***** 04/29/24 ***************************
           //***ELsFMn = dsWDELL8.ELsFMn;
           //***ELsFPc = dsWDELL8.ELsFPc;
           //Safety Fee Percent/Min Amount
           //***If dsWDRRLc.RRsfpc <> *zeros;
           //***   ELSfpc = dsWDRRLc.RRSfpc;
           //***   ELSfmn = dsWDRRLc.RRSfam;
           //***Endif;
           ELSfpc = dsWDRRLc.RRSfpc;
           ELSfmn = dsWDRRLc.RRSfam;
           //*** End Chg ***** 04/29/24 ***************************
           //Default Large Deductible information from State Reporting Division
           If flgLargeDeduct = *on;
              Exsr LargeDeductible;
           Endif;
            //Default Sunz Manual premium percentage from Group setup
           If clientID = '150';
              Chain(n) (ELCo#:ELFnd:ELFyr) pf100;
              If %found(wdf1l);
               ELManP = F1ManP;
              Endif;
           Endif;

           //Do not carry over factors that are listed on Import spreadsheet from PEO
           //When this procedure is called from Import spreadsheet, following factors will not be
           // carried over from PEO to the new PEO client
           If strMode <> 'Import';
              //Increased Limits code
              ELiLCd = dsWDELL8.ELiLCd;
              ELinL2 = dsWDELL8.ELinL2;
              ELiLEf = dsWDELL8.ELiLEf;
              ELiLEx = dsWDELL8.ELiLEx;
              ELi2Ef = dsWDELL8.ELi2Ef;
              ELi2Ex = dsWDELL8.ELi2Ex;
              If ELiLCd <> ' ';
                 ELiLEf = ELpfdt;
              Endif;
              If ELinL2 <> ' ';
                 ELi2Ef = ELpfdt;
              Endif;
              Exsr ChkIncrLimitCode;
              //Schedule Db/cr Factor
              ELdbCr = dsWDELL8.ELdbCr;
              ELsdEf = dsWDELL8.ELsdEf;
              ELsdEx = dsWDELL8.ELsdEx;
              If ELdbcr <> *zeros;
                 ELsdEf = ELpfdt;
              Endif;
              //Experience Modifier
              ELexM1 = dsWDELL8.ELexM1;
              ELeMD1 = dsWDELL8.ELeMD1;
              //Deviation
              ELasp2 = dsWDELL8.ELasp2;
              If Elasp2 <> 0;
                 ELdvEf = ELpfdt;
                 ELdvEx = 0;
              Endif;
              ELdev2 = dsWDELL8.ELdev2;
              If Eldev2 <> 0;
                 ELd2ef = ELpfdt;
                 ELd2ex = 0;
              Endif;
           Endif;                    //strMode <> 'Import'

           //If Rating Factors default from State reporting division,
           // check if factors are valid for new state
           If flgStRptDivDft = 'Y';
              Exsr ChkValidFactors;
           Endif;

           //*** Begin Chg *** 05/20/22 ***********************************
           //*** ELIPgm = Q1PGM;
           //*** ELIUsr = Q1USER;
           //*** ELITm  = Timeto6(SystemTime());
           //*** ELIDt  = Dateto7(SystemDate());
           ELpgm  = Q1PGM;
           ELuser = Q1USER;
           ELtime = Timeto6(SystemTime());
           ELdt   = Dateto7(SystemDate());
           //*** End   Chg *** 05/20/22 ***********************************
           Update pel0p;
        Endif;
       Endsr;

       //---------------------------------------------------------------
       Begsr AddPolDefaults;
       //---------------------------------------------------------------
        //Quote/Policy entry for MCP PEO/Non-peo
        Chain inRrn pel0p;
        If %found(wdelp);
           If clientID = '150';
              Chain(n) (ELCo#:ELFnd:ELFyr) pf100;
              If %found(wdf1l);
                 ELManP = F1ManP;
              Endif;
           Endif;
           //Default Large Deductible information from PEO
           If flgLargeDeduct = *on;
              Exsr LargeDeductible;
           Endif;
           //Waiver
           If dsWDRRLc.RRwdtp <> *Blank;
              ELwvtp = dsWDRRLc.RRwdtp;
              ELwvpc = dsWDRRLc.RRwdpc;
              ELfchg = dsWDRRLc.RRwdfc;
              ELwmin = dsWDRRLc.RRwdmn;
              ELwvfl = dsWDRRLc.RRwvfl;
           Endif;
           //Safety Fee Percent/Min Amount
           ELSfpc = dsWDRRLc.RRSfpc;
           ELSfmn = dsWDRRLc.RRSfam;
           //Stock Discount
           Clear Eldisc;
           Clear ELasp3;
           If dsWDRRlc.RRdisc <> *Blank;
              ELdisc = dsWDRRlc.RRdisc;
           Else;
              ELasp3 = dsWDRRlc.RRdspc;
           Endif;
           //Deviation 1 and Deviation 2 defaults
           If strMode <> 'Import';
              If dsWDRRlc.RRdv1dft <> 0 and dsWDRRlc.RRdv1c <> 0;
                 ELasp2 = dsWDRRLc.RRDV1DFT;
                 ELdvEf = ELPFDT;
              Endif;
              If dsWDRRlc.RRdv2dft <> 0 and dsWDRRlc.RRdv2c <> 0;
                 ELdev2 = dsWDRRLc.RRDV2DFT;
                 ELd2ef = ELPFDT;
              Endif;
           Endif;

           //*** End   Chg *** 05/20/22 ***********************************
           //*** ELIPgm = Q1PGM;
           //*** ELIUsr = Q1USER;
           //*** ELITm  = Timeto6(SystemTime());
           //*** ELIDt  = Dateto7(SystemDate());
           ELpgm  = Q1PGM;
           ELuser = Q1USER;
           ELtime = Timeto6(SystemTime());
           ELdt   = Dateto7(SystemDate());
           //*** End   Chg *** 05/20/22 ***********************************
           Update pel0p;
        Endif;

       Endsr;

       //---------------------------------------------------------------
       Begsr ChkValidFactors;
       //---------------------------------------------------------------
        If strMode <> 'Import';
           //Deviation 1
           If dsWDRRLs.RRDV1C <> dsWDRRLc.RRDV1C;
              Clear ELasp2;
              Clear ELdvef;
              Clear ELdvex;
              Clear ELd1s2;
              Clear ELd1d2;
              Clear ELd1x2;
              //Default
              ELasp2 = dsWDRRLc.RRDV1DFT;
              If Elasp2 <> 0;
                 ELdvEf = ELpfdt;
                 ELdvEx = 0;
              Endif;
           Endif;
           //Deviation 2
           If dsWDRRLs.RRDV2C <> dsWDRRLc.RRDV2C;
              Clear ELdev2;
              Clear ELd2ef;
              Clear ELd2ex;
              Clear ELd2s2;
              Clear ELd2d2;
              Clear ELd2x2;
              Clear ELdev2F;
              Clear ELdev2BP;
              Clear ELdev2AP;
              //Default
              ELdev2 = dsWDRRLc.RRDV2DFT;
              If Eldev2 <> 0;
                 ELd2ef = ELpfdt;
                 ELd2ex = 0;
              Endif;
           Endif;
          //Schedule Credit/Debit
           If dsWDRRLs.RRCRCD <> dsWDRRLc.RRCRCD or
            dsWDRRLs.RRDBCD <> dsWDRRLc.RRDBCD;
              Clear ELdbCr;
              Clear ELsdEf;
              Clear ELsdEx;
           Endif;
           If dsWDELL8.ELdbCr > 1.0;
              If dsWDELL8.ELdbCr > dsWDRRLc.RRdMax And
               dsWDRRLc.RRdMax <> 0;
                 Clear ELdbCr;
                 Clear ELsdEf;
                 Clear ELsdEx;
              Endif;
           Endif;
           If dsWDELL8.ELdbCr < 1.0;
              If dsWDELL8.ELdbCr < dsWDRRLc.RRcMax And
               dsWDRRLc.RRcMax <> 0;
                 Clear ELdbCr;
                 Clear ELsdEf;
                 Clear ELsdEx;
              Endif;
           Endif;
           If dsWDELL8.ELsdEf <> 0 and dsWDELL8.ELsdEf < dsWDELL8.ELpfdt;
              ELsdef = dsWDELL8.ELpfdt;
           Endif;
           If dsWDELL8.ELsdEx <> 0 and dsWDELL8.ELsdEx > dsWDELL8.ELptdt;
              ELsdEx = dsWDELL8.ELptdt;
           Else;
              If dsWDELL8.ELsdEf < dsWDELL8.ELpfdt and
               dsWDELL8.ELsdEx < dsWDELL8.ELpfdt;
                 Clear ELdbCr;
                 Clear ELsdEf;
                 Clear ELsdEx;
              Endif;
           Endif;
        Endif;                      //strMode <> 'Import'
        //Contractors Credit
        If dsWDRRLs.RRCCCD <> dsWDRRLc.RRCCCD;
           Clear ELasp1;
           Clear ELcrEf;
           Clear ELcrEx;
        Endif;
        //Subj prem credit 1A
        If dsWDRRLs.RR1ACD <> dsWDRRLc.RR1ACD;
           Clear ELaspM;
           Clear ELmcEf;
           Clear ELmcEx;
        Endif;
        //Subj prem credit 1B
        If dsWDRRLs.RR1BCD <> dsWDRRLc.RR1BCD;
           Clear ELaspS;
           Clear ELscEf;
           Clear ELscEx;
        Endif;
        //Facultative Reinsurance
        If dsWDRRLc.RRarcc <> dsWDRRLs.RRarcc;
           Clear ELarCp;
           Clear ELarCa;
           Clear ELarEf;
           Clear ELarEx;
        Endif;
        // Waiver
        If dsWDRRLc.RRwvac <> dsWDRRLs.RRwvac;
           Clear ELwvtp;
           Clear ELwvpc;
           Clear ELfchg;
           Clear ELwmin;
           Clear ELwmax;
           Clear ELwvfl;
           If dsWDRRLc.RRwdtp <> *Blank;
              ELwvtp = dsWDRRLc.RRwdtp;
              ELwvpc = dsWDRRLc.RRwdpc;
              ELfchg = dsWDRRLc.RRwdfc;
              ELwmin = dsWDRRLc.RRwdmn;
              ELwvfl = dsWDRRLc.RRwvfl;
           Endif;
        Endif;
       Endsr;

       //---------------------------------------------------------------
       Begsr GetEmpDtlToDft;
       //---------------------------------------------------------------
        //Find first occurence of the state
        //Default factors from the first occurence if the state already exist on the policy
        Reset dsWDELL8;
        flgFirstOccrSt = 'N';
        Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
        Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08 dsWDELL8;
        Dow not %eof(wdell8);
           chain (dsWDELL8.ELCO#:dsWDELL8.ELRTCD) prs00;
           If RSjrst = strCurState and dsWDELL8.ELdiv <> ELdiv;
              intCxDate = GetCxDate(dsWDELL8.ELCO#:dsWDELL8.ELFnd:
               dsWDELL8.ELEmp#:dsWDELL8.ELDiv);
              If intCxDate = 0 or (intCxDate <> 0 and intCxDate <> ELpfdt);
                 flgFirstOccrSt = 'Y';
                 Leave;
              Endif;
           Endif;
           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08 dsWDELL8;
        Enddo;

        //If State does not exist on the policy, Get state Reporting division WDELP
        //Default factors from the State Reporting Division if the state doesn't exist on the policy
        If flgFirstOccrSt = 'N';
           Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08 dsWDELL8;
           Dow not %eof(wdell8);
              If dsWDELL8.ELdiv = intStRptDiv;
                 flgStRptDivDft = 'Y';
                 Leave;
              Endif;
              Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08 dsWDELL8;
           Enddo;
        Endif;

        //Get Rating and Statistics for State reporting division
        chain (dsWDELL8.ELCO#:dsWDELL8.ELRTCD) prs00;
        strDftState = RSjrst;
        Reset dsWDRRLs;
        Setgt (ELco#:ELfnd:strDftState:ELpfdt) PRR00;
        Readpe (ELco#:Elfnd:strDftState) PRR00 dsWDRRLs;
       Endsr;

       //---------------------------------------------------------------
       Begsr GetPEOEmpDtlToDft;
       //---------------------------------------------------------------
        //Find first occurence of the state
        //Default factors from the first occurence of the state
        // if the state already exist on the policy
        Reset dsWDELL8;
        flgFirstOccrSt = 'N';
        Setll (ELco#:ELfnd:ELemp#:ELfyr) pel08;
        Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08 dsWDELL8;
        Dow not %eof(wdell8);
           chain (dsWDELL8.ELCO#:dsWDELL8.ELRTCD) prs00;
           If RSjrst = strCurState and dsWDELL8.ELdiv <> ELdiv;
              intCxDate = GetCxDate(dsWDELL8.ELCO#:dsWDELL8.ELFnd:
               dsWDELL8.ELEmp#:dsWDELL8.ELDiv);
              If intCxDate = 0 or (intCxDate <> 0 and intCxDate <> ELpfdt);
                 flgFirstOccrSt = 'Y';
                 Leave;
              Endif;
           Endif;
           Reade (ELco#:ELfnd:ELemp#:ELfyr) pel08 dsWDELL8;
        Enddo;
        If flgFirstOccrSt = 'N';
           //Check if State exist in Live when adding a quote that will be
           //transferred as a division to Master PEO
           //*** beg chg ***** 11/15/21 *******************
           // If strMode = 'Quote';
            If strMode = 'Quote' Or isQteImport;
           //*** end chg ***** 11/15/21 *******************
               If %Open(WDELL8Q);
                  Close WDELL8Q;
               Endif;
               strCommand = 'OVRDBF FILE(WDELL8Q) TOFILE(' +
               %Trim(RtvQuoteLib('*LIV')) + '/WDELL8Q)';
               Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));

               Open WDELL8Q;
               Setll (ELco#:ELfnd:1:ELfyr) pel08Q;
               Reade (ELco#:ELfnd:1:ELfyr) pel08Q dsWDELL8Q;
               Dow not %eof(wdell8Q);
                  chain (dsWDELL8Q.ELCO#:dsWDELL8Q.ELRTCD) prs00;
                  If RSjrst = strCurState;
                     intCxDate = GetCxDate(dsWDELL8Q.ELCO#:dsWDELL8Q.ELFnd:
                      dsWDELL8Q.ELEmp#:dsWDELL8Q.ELDiv);
                     If intCxDate = 0 or
                      (intCxDate <> 0 and intCxDate <> ELpfdt);
                        flgFirstOccrSt = 'Y';
                        Leave;
                     Endif;
                  Endif;
                  Reade (ELco#:ELfnd:1:ELfyr) pel08Q dsWDELL8Q;
               Enddo;
               dsWDELL8 = dsWDELL8Q;
               Close WDELL8Q;
               strCommand = 'DLTOVR FILE(WDELL8Q)';
               Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
            Endif;
        Endif;

        //If State does not exist on the policy, Get state Reporting division WDELP
        //Default factors from the State Reporting Division if the state doesn't exist on the policy
        If flgFirstOccrSt = 'N';
           Chain (ELco#:ELfnd:1:0:ELfyr) pel00 dsWDELL;
           If %found(wdell);
              flgStRptDivDft = 'Y';
              dsWDELL8 = dsWDELL;
           Endif;
           //If Quote is being entered and PEO exist in Live, get SRD from Live
           //*** beg chg ***** 11/15/21 *******************
           // If flgStRptDivDft = 'N' and strMode = 'Quote';
           If flgStRptDivDft = 'N'
            And (strMode = 'Quote' Or isQteImport);
           //*** end chg ***** 11/15/21 *******************
              If %Open(WDELLQ);
                 Close WDELLQ;
              Endif;
              strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
              %Trim(RtvQuoteLib('*LIV')) + '/WDELLQ)';
              Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
              Open WDELLQ;
              Chain (ELco#:ELfnd:1:0:ELfyr) pel0q dsWDELLq;
              If %found(wdellq);
                 flgStRptDivDft = 'Y';
                 dsWDELL8 = dsWDELLQ;
              Endif;
              Close WDELLQ;
              strCommand = 'DLTOVR FILE(WDELLQ)';
              Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
           Endif;
        Endif;

        //Get Rating and Statistics for First Occurrence/State reporting division
        chain (dsWDELL8.ELCO#:dsWDELL8.ELRTCD) prs00;
        strDftState = RSjrst;
        Reset dsWDRRLs;
        Setgt (ELco#:ELfnd:strDftState:ELpfdt) PRR00;
        Readpe (ELco#:Elfnd:strDftState) PRR00 dsWDRRLs;
       Endsr;

       //---------------------------------------------------------------
       Begsr LargeDeductible;
       //---------------------------------------------------------------
       //*** Beg Chg ***** 04/01/22 *****************************************
       // Select;
       // When FNpeoF = *Blank;
       //Adding a Policy
       //    Select;
       //    When flgAddPolicy = *On;
       //       Setgt  (ELco#:ELfnd:0:0:0:strCurState:ELpfdt) pld03;
       //       Readpe (ELco#:ELfnd:0:0:0:strCurState) pld03;
       //       If %Eof(wmldl3);
       //          If strMode = 'Quote';
       //             If %Open(WMLDL3);
       //                Close WMLDL3;
       //             Endif;
       //             strCommand = 'OVRDBF FILE(WMLDL3) TOFILE(' +
       //             %Trim(RtvQuoteLib('*LIV')) + '/WMLDL3)';
       //             Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //
       //             Open WMLDL3;
       //             Setgt  (ELco#:ELfnd:0:0:0:strCurState:ELpfdt) pld03;
       //             Readpe (ELco#:ELfnd:0:0:0:strCurState) pld03;
       //
       //             Close WMLDL3;
       //             strCommand = 'DLTOVR FILE(WMLDL3)';
       //             Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //             Open WMLDL3;
       //          Endif;
       //       Endif;
       //    //Adding a Division to non-Peo policy in Quote or Live
       //    When flgAddDivision = *On;
       //       Chain(n) (ELco#:ELfnd:ELemp#:intStRptDiv:ELfyr) pld01;
       //       If not %Found(wmldl1);
       //          If strMode = 'Quote';
       //             If %Open(WMLDL1Q);
       //                Close WMLDL1Q;
       //             Endif;
       //             strCommand = 'OVRDBF FILE(WMLDL1Q) TOFILE(' +
       //             %Trim(RtvQuoteLib('*LIV')) + '/WMLDL1Q)';
       //             Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //
       //             Open WMLDL1Q;
       //             Chain (ELco#:ELfnd:ELemp#:intStRptDiv:ELfyr) pld01q;
       //
       //             Close WMLDL1Q;
       //             strCommand = 'DLTOVR FILE(WMLDL1Q)';
       //             Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //          Endif;
       //       Endif;
       //    Endsl;
       // When FNpeoF <> *Blank;
       //    //Get Large deductible information from PEO for Master PEO DiV Add or MCP Quote/Policy
       //    Chain(n) (ELco#:ELfnd:1:0:ELfyr) pld01;
       //    If not %Found(wmldl1);
       //    //*** beg chg ***** 11/15/21 *******************
       //      // If strMode = 'Quote';
       //       If strMode = 'Quote' Or isQteImport;
       //    //*** end chg ***** 11/15/21 *******************
       //          If %Open(WMLDL1Q);
       //             Close WMLDL1Q;
       //          Endif;
       //          strCommand = 'OVRDBF FILE(WMLDL1Q) TOFILE(' +
       //          %Trim(RtvQuoteLib('*LIV')) + '/WMLDL1Q)';
       //          Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));

       //          Open WMLDL1Q;
       //          Chain (ELco#:ELfnd:1:0:ELfyr) pld01q;
       //
       //          Close WMLDL1Q;
       //          strCommand = 'DLTOVR FILE(WMLDL1Q)';
       //          Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       //       Endif;
       //    Endif;
       // Endsl;
       //
       // LDco#  = ELco#;
       // LDfnd  = ELfnd;
       // LDemp# = ELemp#;
       // LDdiv  = ELdiv;
       // LDfyr  = ELfyr;
       // LDefd1 = ELpfdt;
       //
       // LDipgm = Q1PGM;
       // LDiusr = Q1USER;
       // LDitim = Timeto6(SystemTime());
       // LDidt  = Dateto7(SystemDate());
       //
       // Write pld01;

        Reset dsWMLDL1;
        Reset dsWMLDL1Q;
        Select;
        When FNpeoF = *Blank;
           //Adding a Division to non-Peo policy in Quote or Live
           Chain(n) (ELco#:ELfnd:ELemp#:intStRptDiv:ELfyr) pld01 dsWMLDL1;
           isUseDsDefault = %Found(wmldl1);
           If not %Found(wmldl1);
              If strMode = 'Quote';
                 If %Open(WMLDL1Q);
                    Close WMLDL1Q;
                 Endif;
                 strCommand = 'OVRDBF FILE(WMLDL1Q) TOFILE(' +
                 %Trim(RtvQuoteLib('*LIV')) + '/WMLDL1Q)';
                 Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));

                 Open WMLDL1Q;
                 Chain (ELco#:ELfnd:ELemp#:intStRptDiv:ELfyr) pld01q dsWMLDL1Q;
                 isUseDsDefault = %Found(wmldl1q);
                 dsWMLDL1 = dsWMLDL1Q;

                 Close WMLDL1Q;
                 strCommand = 'DLTOVR FILE(WMLDL1Q)';
                 Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
              Endif;
           Endif;
        When FNpeoF <> *Blank;
           //Get Large deductible information from PEO for Master PEO DiV Add or MCP Quote/Policy
           Chain(n) (ELco#:ELfnd:1:0:ELfyr) pld01 dsWMLDL1;
           isUseDsDefault = %Found(wmldl1);
           If not %Found(wmldl1);
              If strMode = 'Quote' Or isQteImport;
                 If %Open(WMLDL1Q);
                    Close WMLDL1Q;
                 Endif;
                 strCommand = 'OVRDBF FILE(WMLDL1Q) TOFILE(' +
                 %Trim(RtvQuoteLib('*LIV')) + '/WMLDL1Q)';
                 Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));

                 Open WMLDL1Q;
                 Chain (ELco#:ELfnd:1:0:ELfyr) pld01q dsWMLDL1Q;
                 isUseDsDefault = %Found(wmldl1q);
                 dsWMLDL1 = dsWMLDL1Q;

                 Close WMLDL1Q;
                 strCommand = 'DLTOVR FILE(WMLDL1Q)';
                 Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
              Endif;
           Endif;
        Endsl;
        //Get Large Deductible defaults from Group Configuration
        Setgt  (ELco#:ELfnd:0:0:0:strCurState:ELpfdt) pld03;
        Readpe (ELco#:ELfnd:0:0:0:strCurState) pld03;
        If %Eof(wmldl3);
           If strMode = 'Quote';
              If %Open(WMLDL3);
                 Close WMLDL3;
              Endif;
              strCommand = 'OVRDBF FILE(WMLDL3) TOFILE(' +
              %Trim(RtvQuoteLib('*LIV')) + '/WMLDL3)';
              Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));

              Open WMLDL3;
              Setgt  (ELco#:ELfnd:0:0:0:strCurState:ELpfdt) pld03;
              Readpe (ELco#:ELfnd:0:0:0:strCurState) pld03;

              Close WMLDL3;
              strCommand = 'DLTOVR FILE(WMLDL3)';
              Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
              Open WMLDL3;
           Endif;
        Endif;

        If not %Eof(WMLDL3) and LDdes1 <> ' ';
           If LDdes2  <>  ' ';
              LDefd2 = ELpfDt;
           Endif;
           LDexp1 = *zero;
           LDexp2 = *zero;
           LDco#  = ELco#;
           LDfnd  = ELfnd;
           LDemp# = ELemp#;
           LDdiv  = ELdiv;
           LDfyr  = ELfyr;
           LDefd1 = ELpfdt;
           //Get LD amounts from master PEO or SRD for non-PEO
           If isUseDsDefault;
              LDamP1  = dsWMLDL1.LDamP1;
              LDamP2  = dsWMLDL1.LDamP2;
              LDamG1  = dsWMLDL1.LDamG1;
              LDamG2  = dsWMLDL1.LDamG2;
              LDvara1 = dsWMLDL1.LDvara1;
              LDvara2 = dsWMLDL1.LDvara2;
              LDadP1  = dsWMLDL1.LDadP1;
              LDadP2  = dsWMLDL1.LDadP2;
              LDdes1  = dsWMLDL1.LDdes1;
              LDdes2  = dsWMLDL1.LDdes2;
           Endif;

           LDipgm = Q1PGM;
           LDiusr = Q1USER;
           LDitim = Timeto6(SystemTime());
           LDidt  = Dateto7(SystemDate());
           Write pld01;
        Endif;
       //*** End Chg ***** 04/01/22 *****************************************
       Endsr;
       //---------------------------------------------------------------
       Begsr ChkDeductibleCode;
       //---------------------------------------------------------------
        //Verify ded code exists for new division
        If dsWDELL8.ELdcde <> *Blanks;
           SetGt (Elco#:ELfnd:strCurState:dsWDELL8.ELdcde:ELpfdt) pdd02;
           Dou %Eof(wtddl2);
              Readpe (Elco#:ELfnd:strCurState:dsWDELL8.ELdcde) pdd02;
              If %Eof(wtddl2);
                 Leave;
              Endif;
              If DDstat = 'I';
                 Iter;
              EndIf;
              If DDstat = 'A';
                 Leave;
              Endif;
           Enddo;
           If %Eof(wtddl2);
              ELdcde = *Blanks;
              ELdcef = *Zeros;
              ELdcex = *Zeros;
           Endif;
        Endif;
        If dsWDELL8.ELdcd2 <> *Blanks;
           SetGt (Elco#:ELfnd:strCurState:dsWDELL8.ELdcd2:ELpfdt) pdd02;
           Dou %Eof(wtddl2);
              Readpe (Elco#:ELfnd:strCurState:dsWDELL8.ELdcd2) pdd02;
              If %Eof(wtddl2);
                 Leave;
              Endif;
              If DDstat = 'I';
                 Iter;
              EndIf;
              If DDstat = 'A';
                 Leave;
              Endif;
           Enddo;
           If %Eof(wtddl2);
              ELdcd2 = *Blanks;
              EL2def = *Zeros;
              EL2dex = *Zeros;
           Endif;
        Endif;
       Endsr;

       //---------------------------------------------------------------
       Begsr ChkIncrLimitCode;
       //---------------------------------------------------------------
        //Verify Inc Limits exists for new division
        If dsWDELL8.ELiLCd <> *Blank;
           Setgt (ELco#:ELfnd:strCurState:dsWDELL8.ELilcd:ELpfdt) plm01;
           Dou %Eof(WTLML1);
              Readpe (ELco#:ELfnd:strCurState:dsWDELL8.ELilcd) plm01;
              If %Eof(WTLML1);
                 Leave;
              Endif;
              If LMstat = 'I';
                 Iter;
              EndIf;
              If LMstat = 'A';
                 Leave;
              Endif;
           Enddo;
           If %Eof(WTLML1);
              ELilcd = *Blanks;
              ELilef = *Zeros;
              ELilex = *Zeros;
           Endif;
        Endif;
        If dsWDELL8.ELinl2 <> *Blank;
           Setgt (ELco#:ELfnd:strCurState:dsWDELL8.ELinl2:ELpfdt) plm01;
           Dou %Eof(WTLML1);
              Readpe (ELco#:ELfnd:strCurState:dsWDELL8.ELinl2) plm01;
              If %Eof(WTLML1);
                 Leave;
              Endif;
              If LMstat = 'I';
                 Iter;
              EndIf;
              If LMstat = 'A';
                 Leave;
              Endif;
           Enddo;
           If %Eof(WTLML1);
              ELinl2 = *Blanks;
              ELi2ef = *Zero;
              ELi2ex = *Zero;
           Endif;
        Endif;

       Endsr;

      /End-Free
     PDefaultRatingFactors...
     P                 e

     P***  End   Add  ***  08/24/21  ***********************************

     P***  Begin Add  ***  07/01/21  ***********************************
     P*====================================================================
     P* DeleteEmployer - Delete employer (one or all divisions)
     P*
     P* DeleteEmployer - returns *on if successful
     P*   json      - accepts a json string of parms       (required)
     P*    accepts--
     P*     rrn:     - rrn of WMEMP             (required)
     P*     quote:   - 'Q' if from quote
     P*     div:     - '*ALL' for all divisions
     P*     edit:    - '*ONLY' for no deletions
     P*     dispaly: - '*YES' for displaying messages within
     P*    returns-- this will be blank if successful deletion
     P*     ind:     - indicators (this was from W0050R)
     P*     msgId:   - id from WMESGG
     P*     msg:     - message text
     P*====================================================================
     PDeleteEmployer   b                   export
     DDeleteEmployer   pi             1n
     D ioJson                          a   len(1000) varying
     D
     DW0050R01         pr                  extPgm('W0050R01')
     D outCo#                         3  0 const
     D outFnd                         3  0 const
     D outEmp#                        9  0 const
     D outDiv                         5  0 const
     D outQuote                       1    const
     dW0050R02         pr                  extpgm('W0050R02')
     d outKC                          1    const
     d outKL                          1    const
     d outMode                        1    const
     d outRRN                         9  0 const
     d outQte                         1    const
     DW0050R04         pr                  extPgm('W0050R04')
     D outRrn                         9  0 const
     D outQuote                       1    const
     D outInd                         2p 0
     D outAllDiv                      1n   const options(*nopass)

     DdsWMEMP        e ds                  extname(WMEMP) inz

     DdsIn             ds                  inz qualified
     D rrn                            9  0
     D quote                          1
     D div                            5
     D edit                           5
     D display                        5
     DdsOut            ds                  inz qualified
     D ind                            2p 0
     D msgId                          7
     D msg                          132    varying
     DaryOut           ds                  likeds(dsOut) dim(99) inz
     Dm1               s              3  0
     DdecDiv           s              5  0 inz
     DdecRRN           s              9  0 inz
     DisAllDiv         s              1n   inz
     DisDisplayMsg     s              1n   inz
     DisEditOnly       s              1n   inz
     DisEofError       s              1n   inz
     DisEofWMEMP       s              1n   inz
     DisError          s              1n   inz
     DstrDivList       s            151    inz varying
     dstrJobInfo       s             28
     dstrProcess       s             10
     dstrSqlError      s            500    inz varying
     dstrSqlWMEMP      s            500    inz varying
     dstrUser          s             50
      /Free
       Exsr LoadMsgArray;
       Exsr GetParms;
       Exsr BuildEmpSql;
       Exsr CheckErrors;
       Select;
       When dsOut.ind <> 0;
          If dsOut.ind > 0;
             dsOut = aryOut(dsOut.ind);
          Endif;
          If isDisplayMsg;
             PopOneBtn(dsOut.msg:'Employer Deletion');
          Endif;
          isError = *on;
       When dsOut.ind = 0 and isEditOnly;
          dsOut.msg = 'All edits passed.';
       Other;
          Exsr ProcessDeletion;
       Endsl;
       Exsr BuildJson;
       Return not isError;
       //=====================================
       Begsr BuildCursorError;
        Exec sql close cursorError;
        Exec sql prepare sqlError from :strSqlError;
        Exec sql declare cursorError cursor for sqlError;
        Exec sql open cursorError;
        Reset isEofError;
        Reset strDivList;
       Endsr;
       //=====================================
       Begsr BuildCursorWMEMP;
        Exec sql close cursorWMEMP;
        Exec sql prepare sqlWMEMP from :strSqlWMEMP;
        Exec sql declare cursorWMEMP cursor for sqlWMEMP;
        Exec sql open cursorWMEMP;
        Reset isEofWMEMP;
       Endsr;
       //=====================================
       Begsr BuildEmpSql;
        strSqlWMEMP = 'select rrn(WMEMP), WMEMP.* from WMEMP' +
         ' where EMco# = ' + %char(EMco#) +
         '  and EMfnd = ' + %char(EMfnd) +
         '  and EMemp# = ' + %char(EMemp#);
        If not isAllDiv;
           strSqlWMEMP += ' and EMdiv = ' + %char(EMdiv);
        Endif;
       Endsr;
       //=====================================
       Begsr BuildJson;
        If dsOut.ind > 0;
           dsOut = aryOut(dsOut.ind);
        Endif;
        Select;
        When dsOut.msg <> *blanks;
        When dsOut.msgId <> *blanks;
        Other;
           Leavesr;
        Endsl;
        ioJson += '{';
        ioJson += BuildJsonNode('ind':%char(dsOut.ind)) + ',';
        ioJson += BuildJsonNode('msgId':dsOut.msgId) + ',';
        ioJson += BuildJsonNode('msg':dsOut.msg) + ',';
        ioJson = %scanrpl(',':'}':ioJson:%len(ioJson)-1);
       Endsr;
       //=====================================
       Begsr CheckErrors;
        If GetPolicyLockStatus(EMco#:EMfnd:EMemp#:strJobInfo:
         strProcess:strUser) = *On;
           If strProcess = 'REVBILLING';
              dsOut.ind = -1;
              dsOut.msg = 'This operation is available when ' +
               'Revised Billing Job(' + %Trim(strJobInfo) +
               ') has completed for ' + %Trim(strUser) + '.';
              Leavesr;
           Endif;
        Endif;
        // If no errors are found, dsOut.ind=0
        W0050R04(dsIn.rrn:dsIn.quote:dsOut.ind:isAllDiv);
        Select;
        When isDisplayMsg and dsOut.ind <> 0;
           // PopOneBtn(dsOut.msg:'Deletion Error');
        When isDisplayMsg;
           // These messages allow for override
           Exsr CheckErrorsDARTS;
           Exsr CheckErrorsLocations;
           Exsr CheckErrorsPayrolls;
        Endsl;
       Endsr;
       //=====================================
       Begsr CheckErrorsDARTS;
        If dsOut.ind <> 0;
           Leavesr;
        Endif;
        strSqlError = 'select distinct EMdiv from WMEMP' +
         ' join SDCMP on CMkey like' +
         '  (digits(EMco#)||digits(EMfnd)||' +
         '   digits(EMemp#)||digits(EMdiv)||''%'')' +
         ' where EMco# = ' + %char(EMco#) +
         '  and EMfnd = ' + %char(EMfnd) +
         '  and EMemp# = ' + %char(EMemp#);
        If not isAllDiv;
           strSqlError += ' and EMdiv = ' + %char(EMdiv);
        Endif;
        strSqlError += ' and substring(CMkey,40,1) = ''' + dsIn.quote + '''' +
         '  and CMstat <> ''I''';
        // 'An active DARTS Remark exists'
        dsOut.ind = 75;
        Exsr CheckOverridable;
       Endsr;
       //=====================================
       Begsr CheckErrorsLocations;
        If dsOut.ind <> 0;
           Leavesr;
        Endif;
        strSqlError = 'select distinct ALDIV from WMALP' +
         ' where ALco# = ' + %char(EMco#) +
         '  and ALfnd = ' + %char(EMfnd) +
         '  and ALemp# = ' + %char(EMemp#);
        If not isAllDiv;
           strSqlError += ' and ALdiv = ' + %char(EMdiv);
        Endif;
        dsOut.msg = 'Entities/Locations exist.';
        dsOut.ind = -2;
        Exsr CheckOverridable;
       Endsr;
       //=====================================
       Begsr CheckErrorsPayrolls;
        If dsOut.ind <> 0;
           Leavesr;
        Endif;
        strSqlError = 'select distinct SRDIV from WASRP' +
         ' where SRco# = ' + %char(EMco#) +
         '  and SRfnd = ' + %char(EMfnd) +
         '  and SRemp# = ' + %char(EMemp#);
        If not isAllDiv;
           strSqlError += ' and SRdiv = ' + %char(EMdiv);
        Endif;
        // 'Self-reporting payrolls have been entered'
        dsOut.ind = 86;
        Exsr CheckOverridable;
       Endsr;
       //=====================================
       Begsr CheckOverridable;
        Exsr BuildCursorError;
        Dou isEofError;
           Exec sql fetch next from cursorError into :decDiv;
           Select;
           When not IsValidFetch(sqlstt);
              isEofError = *on;
              Iter;
           Endsl;
           strDivList += %editc(decDiv:'X') + ',';
        Enddo;
        Select;
        When strDivList <> *blank;
           Exsr DisplayOverride;
        Other;
           Reset dsOut;
        Endsl;
       Endsr;
       //=====================================
       Begsr DisplayOverride;
        Select;
        When %len(strDivList) = 0;
        When %len(strDivList) = %size(strDivList);
           %subst(strDivList:%size(strDivList)-6) = '...';
        Other;
           strDivList = %scanrpl(',':'':strDivList:%len(strDivList)-1);
        Endsl;
        If dsOut.msg = *blanks;
           dsOut.msg = %trim(aryOut(dsOut.ind).msg);
        Endif;
        dsOut.msg += ' Employer: ' + %editc(EMemp#:'X') +
         ' Division(s): ' + strDivList +
         ' Confirm deletion?';
        If PopTwoBtn(dsOut.msg:'Confirm Deletion':'Confirm');
           Reset dsOut;
        Endif;
       Endsr;
       //=====================================
       Begsr GetParms;
        node0 = yajl_string_load_tree(ioJson:strErrorMsg);
        Reset i0;
        Dow YAJL_object_loop(node0:i0:key:val);
           Select;
           When key = 'rrn';
              dsIn.rrn = %int(YAJL_get_string(val));
           When key = 'quote';
              dsIn.quote = YAJL_get_string(val);
           When key = 'div';
              dsIn.div = YAJL_get_string(val);
           When key = 'edit';
              dsIn.edit = YAJL_get_string(val);
           When key = 'display';
              dsIn.display = YAJL_get_string(val);
           Endsl;
        Enddo;
        yajl_tree_free(node0);
        Clear ioJson;
        isAllDiv = dsIn.div = '*ALL';
        isEditOnly = dsIn.edit = '*ONLY';
        Select;
        When PgmInStack('WTPOLTVAL');
           isDisplayMsg = *off;
        Other;
           isDisplayMsg = dsIn.display = '*YES';
        Endsl;
        Reset dsWMEMP;
        Exec sql
         select * into :dsWMEMP
          from WMEMP
          where rrn(WMEMP) = :dsIn.rrn;
       Endsr;
       //=====================================
       Begsr LoadMsgArray;
        aryOut(61).msg = 'S/R Billings for this div exist in the St Rpt Div';
        aryOut(69).msgId = 'WC30335'; // Billings for state reporting
        aryOut(70).msgId = 'WC31338'; // MC Claims exists
        aryOut(72).msgId = 'WCR1097'; // Payments exist
        aryOut(73).msgId = 'WCR0623'; // Claims exist
        aryOut(74).msgId = 'WCR1095'; // A/R in prior period
        aryOut(75).msgId = 'WC30011'; // DARTS exists
        aryOut(77).msgId = 'WC30020'; // Not authorized
        aryOut(81).msgId = 'WC30100'; // Reporting submitted
        aryOut(82).msgId = 'WC30101'; // A/R batch records exist
        aryOut(86).msg =
         'Employer not eligible for deletion; S/R payroll has been reported.';
        For m1 = 1 to 99;
           aryOut(m1).ind = m1;
           Select;
           When aryOut(m1).msgId = *blank;
              Iter;
           Endsl;
           aryOut(m1).msg = RtvMsgD('WMESGG':aryOut(m1).msgId);
        Endfor;
       Endsr;
       //=====================================
       Begsr ProcessDeletion;
        Exsr BuildCursorWMEMP;
        // Process deletion(s)
        Dou isEofWMEMP;
           Exec sql fetch next from cursorWMEMP into :decRRN, :dsWMEMP;
           Select;
           When not IsValidFetch(sqlstt);
              isEofWMEMP = *on;
              Iter;
           Endsl;
         //W0050R01(EMco#:EMfnd:EMemp#:EMdiv:dsIn.quote);
           W0050R02('0':'0':'E':decRRN:dsIn.quote);
        Enddo;
        dsOut.msg = 'Employer deleted.';
       Endsr;
      /End-free
     PDeleteEmployer   e
     P***  End   Add  ***  07/01/21  ***********************************

     P*====================================================================
     P* DivCount - Count the # of divisions for an employer
     P*
     P*  DivCount - returns # of divsions for an employer
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*====================================================================

     PDivCount         b                   export

     DDivCount         PI             5  0
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const

     D count           s              5  0

     C     emky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#

     C                   clear                   count

     C     emky          chain     pem00                              60

     C                   dow       not *in60
     C                   eval      count = count + 1
     C     emky          reade     pem00                                  60
     C                   enddo

     C                   return    count

     PDivCount         e


     P*====================================================================
     P* EmpAuth - Determine if user is authorized to employer
     P*
     P*  EmpAuth - returns *on if the user is authorized, otherwise *off
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   userEntry - accepts the user to search over (defaults to Q1USER)
     P*   accessLvl - returns access level (ie. 'I'nquiry or 'A'll)
     P*====================================================================

     PEmpAuth          b                   export

     DEmpAuth          PI             1
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const
     D userEntry                     10    const options(*nopass)
     D accessLvl                      1          options(*nopass)

     DSA008R           PR                  extpgm('SA008R')
     D ##co#                          3  0 const
     D ##user                        10    const
     D ##appl                         1    const
     D ##rtcd                         1
     D ##auth                         1          options(*nopass)
     D ##grp                          3  0 const options(*nopass)
     D ##emp#                         9  0 const options(*nopass)

     D user            s                   like(q1user)
     D records         s              1
     D authority       s              1
     D access          s              1
     D authorized      s              1

     C                   eval      user = q1user

     C                   if        %parms >= 4 and userEntry <> *blanks
     C                   eval      user = userEntry
     C                   endif

     C                   callp     SA008R(co#:user:'W':records)
     C                   callp     SA008R(co#:user:'W':authority:access:
     C                                    group:emp#)

     C                   eval      *in60      = records   = *on and
     C                                          authority = *on
     C                   eval      authorized = not *in60

     C                   if        %parms >= 5
     C                   eval      accessLvl = access
     C                   endif

     C                   return    authorized

     PEmpAuth          e


     P*====================================================================
     P* EmpCommRt - Load field with commission rate (renewal vs. renewal)
     P*
     P*  EmpCommRt - returns commission rate
     P*   AHrrn     - passed in for validation                  (required)
     P*   ARCode    - passed in for validation                  (required)
     P*   transDt   - passed in for validation                  (required)
     P*   agent     - passed in for primary or secondary agent  (optional)
     P*====================================================================

     PEmpCommRt        b                   export

     DEmpCommRt        PI             5  2
     D AHrrn                          9  0 const
     D ARCode                         2    const
     D transDt                        7  0 const
     D agent                          1    const options(*nopass)

     D agent#          s              1    inz('1')
     D rate            s                   like(ARRNRT)
     D xxrt#           s                   like(AHRT#)
     D firstOfMth      s                   like(AREFDT)
     D renewDate       s               d   datfmt(*usa)
     D dateUSA         s               d   datfmt(*usa)

     C     emky          klist
     C                   kfld                    AHCO#
     C                   kfld                    AHFND
     C                   kfld                    AHEMP#
     C                   kfld                    AHDIV

     C     elky1         klist
     C                   kfld                    AHCO#
     C                   kfld                    AHFND
     C                   kfld                    AHFYR
     C                   kfld                    AHEMP#
     C                   kfld                    AHDIV

     C     arky          klist
     C                   kfld                    AHCO#
     C                   kfld                    AHFND
     C                   kfld                    xxrt#

     C     arky_1        klist
     C                   kfld                    AHCO#
     C                   kfld                    AHFND
     C                   kfld                    xxrt#
     C                   kfld                    firstOfMth

     C                   if        %parms >= 4
     C                   eval      agent# = agent
     C                   endif

     C     AHrrn         chain     pah0p                              60
     C     emky          chain     pem00                              60
     C     elky1         chain     pel01                              60

     C                   eval      dateUSA = getLastDay(cvtToDate(transDt))
     C                   adddur    1:*d          dateUSA
     C                   subdur    1:*m          dateUSA
     C     *cymd         move      dateUSA       firstOfMth

     C                   select
     C                   when      agent# = '1'
     C                   eval      xxrt# = AHRT#
     C                   when      agent# = '2'
     C                   eval      xxrt# = AHSRT#
     C                   endsl

     C     arky_1        setgt     par00
     C     arky          readpe    par00                                  60

     C                   select
     C                   when      EMEFD5 <> *zero
     C     *cymd         move      EMEFD5        renewDate
     C                   when      EMEFD4 <> *zero
     C     *cymd         move      EMEFD4        renewDate
     C                   when      EMEFD3 <> *zero
     C     *cymd         move      EMEFD3        renewDate
     C                   when      EMEFD2 <> *zero
     C     *cymd         move      EMEFD2        renewDate
     C                   when      EMEFD1 <> *zero
     C     *cymd         move      EMEFD1        renewDate
     C                   endsl

     C                   adddur    1:*y          renewDate

     C                   select
     C                   when      renewDate >= cvtToDate(ELPTDT)
     C                   eval      rate = ARNWRT
     C                   when      renewDate <= cvtToDate(ELPFDT)
     C                   eval      rate = ARRNRT
     C                   when      cvtToDate(transDt) <  renewDate
     C                   eval      rate = ARNWRT
     C                   other
     C                   eval      rate = ARRNRT
     C                   endsl

     C                   return    rate

     PEmpCommRt        e


     P*====================================================================
     P* EmpDivName - Load field with compressed Emp#/Div/Name
     P*
     P*  EmpDivName - returns compressed Emp#/Div/Name
     P*   emp#      - passed in for validation  (required)
     P*   div       - passed in for validation  (required)
     P*   emrrn     - returns the RRN of WMEMP
     P*   group     - passed in for validation
     P*   co#       - accepts the company # to search over
     P*====================================================================
     P*
     PEmpDivName       b                   export
     P*
     DEmpDivName       PI            80
     D emp#                                like(EMEMP#)                  const
     D div                                 like(EMDIV)                   const
     D emrrn                          9  0              options(*nopass)
     D group                               like(EMFND)  options(*nopass) const
     D co#                                 like(EMCO#)  options(*nopass) const

     D group#          s                   like(EMFND)
     D company         s                   like(EMCO#)
     D rrn             s              9  0
     D edn             s             80
     D number          s             10
     D numOfZeros      s              3  0


     C                   eval      company = UserCo#(q1user)
     C                   eval      group#  = UserGrp(q1user)

     C                   if        %parms >= 4
     C                   eval      group# = group
     C                   endif

     C                   if        %parms >= 5
     C                   eval      company = co#
     C                   endif

     C                   movel(p)  emp#          number
     C     '0'           check     number        numOfZeros

     C                   eval      edn = %subst(number:numOfZeros)

     C                   movel(p)  div           number
     C     '0'           check     number        numOfZeros

     C                   if        div <> *zero
     C                   eval      edn = %trim(edn) + ' / ' +
     C                                   %trimr(%subst(number:numOfZeros)) +
     C                                   ' - ' +
     C                                   GetEmpName(emp#:div:rrn:group#:company)
     C                   else
     C                   eval      edn = %trim(edn) + ' / 0 - ' +
     C                                   GetEmpName(emp#:div:rrn:group#:company)
     C                   endif

     C                   if        %parms >= 3
     C                   eval      emrrn = rrn
     C                   endif

     C                   return    edn

     PEmpDivName       e


     P*====================================================================
     P* EmpSrch - Search employers
     P*
     P*  EmpSrch - returns RRN of WMEMP
     P*   inkc   - returns *on if F3 was pressed from search  (required)
     P*   inkl   - returns *on if F12 was pressed from search (required)
     P*   error  - returns a '1' if group passed in is invalid
     P*   group  - accepts the group # to search over
     P*   co#    - accepts the company # to search over
     P*====================================================================
    
     PEmpSrch          b                   export

     DEmpSrch          PI             9  0
     D inkc                           1
     D inkl                           1
     D error                          1    options(*nopass)
     D group                               options(*nopass) const like(EMFND)
     D co#                                 options(*nopass) const like(EMCO#)

     D err             s              1
     D group#          s                   like(EMFND)
     D company         s                   like(EMCO#)
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@qte           s              1
     D fnrrn           s              9  0

     C                   eval      err     = *blank
     C                   eval      group#  = UserGrp(q1user)
     C                   eval      company = UserCo#(q1user)

     C                   if        %parms >= 4
     C                   eval      group# = group
     C                   endif

     C                   if        %parms >= 5
     C                   eval      company = co#
     C                   endif

     C                   if        ValidGrp(group#:fnrrn:company) = *off
     C                   eval      err = '1'
     C                   endif

     C                   if        %parms >= 3
     C                   eval      error = err
     C                   endif

     C                   if        err = *blank

     C                   call      'W0050R'
     C     inkc          parm                    @@kc
     C     inkl          parm                    @@kl
     C                   parm      '2'           @@call
     C                   parm                    @@rrn
     C                   parm      fnrrn         @@rrn2
     C                   parm                    @@qte

     C                   endif

     C                   return    @@rrn

     PEmpSrch          e


     P*====================================================================
     P* EmpYrSrch - Search employer policy years
     P*
     P*  EmpYrSrch - returns RRN of WDELP
     P*   inkc     - returns *on if F3 was pressed from search  (required)
     P*   inkl     - returns *on if F12 was pressed from search (required)
     P*   error    - returns a '1' if group passed in is invalid
     P*              returns a '2' if employer passed in is invalid
     P*   emp#     - accepts the employer # to search over
     P*   div      - accepts the divsion to search over
     P*   group    - accepts the group # to search over
     P*   co#    - accepts the company # to search over
     P*====================================================================

     PEmpYrSrch        b                    export

     DEmpYrSrch        PI             9  0
     D inkc                           1
     D inkl                           1
     D error                          1    options(*nopass)
     D emp#                                options(*nopass) const like(ELEMP#)
     D div                                 options(*nopass) const like(ELDIV)
     D group                               options(*nopass) const like(ELFND)
     D co#                                 options(*nopass) const like(EMCO#)

     D employer        s                   like(ELEMP#)
     D division        s                   like(ELDIV)
     D err             s              1
     D group#          s                   like(ELFND)
     D company         s                   like(ELCO#)
     D rrn             s              9  0
     D @@kc            s              1
     D @@kl            s              1
     D @@call          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D @@qte           s              1
     D fnrrn           s              9  0
     D emrrn           s              9  0

     C     emky          klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    employer
     C                   kfld                    division

     C                   eval      err     = *blank
     C                   eval      @@rrn   = *zeros
     C                   eval      company = UserCo#(q1user)
     C                   eval      group#  = UserGrp(q1user)

     C                   if        %parms >= 3
     C                   eval      employer = emp#
     C                   eval      division = div
     C                   endif

     C                   if        %parms >= 6
     C                   eval      group# = group
     C                   endif

     C                   if        %parms >= 7
     C                   eval      company = co#
     C                   endif

     C                   if        ValidGrp(group#:fnrrn:company) = *off
     C                   eval      err = '1'
     C                   endif

     C                   if        %parms >= 5
     C                   eval      error = err
     C                   endif

     C                   if        employer = *zero and err = *blank

     C                   eval      rrn = EmpSrch(inkc:inkl:err:group#:company)

     C                   if        rrn <> *zero and inkl = *off
     C     rrn           chain     pem0p                              60
     C                   eval      employer = EMEMP#
     C                   eval      division = EMDIV
     C                   eval      emrrn    = rrn
     C                   else
     C                   return    @@rrn
     C                   endif

     C                   else

     C     emky          chain     pem00                              60

     C                   if        not *in60
     C                   eval      emrrn = em_rrn
     C                   else
     C                   eval      err = '2'
     C                   endif

     C                   endif

     C                   if        err = *blank

     C                   call      'W0051R'
     C     inkc          parm                    @@kc
     C     inkl          parm                    @@kl
     C                   parm      '2'           @@call
     C                   parm                    @@rrn
     C                   parm      emrrn         @@rrn2
     C                   parm                    @@qte

     C                   endif

     C                   return    @@rrn

     PEmpYrSrch        e


     P*====================================================================
     P* GetBillSch - Get employer billing schedule
     P*
     P*  GetBillSch - returns billing schedule (code/desc)
     P*   ELrrn     - accepts the RRN of WDELP             (required)
     P*   BSrrn     - returns the RRN of WTBSP
     P*   code      - returns the billing schedule code
     P*====================================================================

     PGetBillSch       b                   export

     DGetBillSch       PI            50
     D ELrrn                          9  0 const
     D BSrrn                          9  0       options(*nopass)
     D code                          10          options(*nopass)

     D billSchd        s             50
     D billSchdCd      s             10
     D rrn             s              9  0

     C                   clear                   billSchd
     C                   clear                   ELBSCD

      *** beg chg ***** 11/17/21 *************************
     C***  ELrrn         chain     WDELP                              60
     C     ELrrn         Chain(n)  WDELP                              60
      *** end chg ***** 11/17/21 *************************

     C                   movel(p)  ELBSCD        billSchdCd

     C                   eval      billSchd = %trim(billSchdCd) + ' ' +
     C                                        BillSchdDs(ELCO#:billSchdCd:rrn)

     C  n60              if        %parms >= 2
     C                   eval      BSrrn = rrn
     C                   endif

     C  n60              if        %parms >= 3
     C                   movel(p)  ELBSCD        code
     C                   endif

     C                   return    billSchd

     PGetBillSch       e


     P*====================================================================
     P* GetCxDate - Get cancellation date
     P*
     P*  GetCxDate  - returns the cancellation date
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the employer to search over  (required)
     P*   cx_reason - returns the cx code/description
     P*   env   - accepts the environment Quote/live
     P*====================================================================

     PGetCxDate        b                   export

     DGetCxDate        PI             7  0
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D cx_reason                     65    options(*nopass)
     D env                            1    options(*nopass)

     D cx_date         s              7  0
     D strCommand      s            255

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     C     emky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div

      /Free
       If %parms >= 6 and env = 'Q';
          strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMEMLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WMEMLQ;
      /End-Free
     C     emky          chain     pem0q                              99
     C                   else
     C     emky          chain     pem00                              99
     C                   endif

     C                   clear                   cx_date

     C                   select
     C                   when      not %found
     c                   eval      cx_date = *zeros
     C                   when      EMEFD5 <> *zero
     C                   eval      cx_date = EMCAD5
     C                   when      EMEFD4 <> *zero
     C                   eval      cx_date = EMCAD4
     C                   when      EMEFD3 <> *zero
     C                   eval      cx_date = EMCAD3
     C                   when      EMEFD2 <> *zero
     C                   eval      cx_date = EMCAD2
     C                   when      EMEFD1 <> *zero
     C                   eval      cx_date = EMCAD1
     C                   other
     c                   eval      cx_date = *zeros
     C                   endsl

     C                   if        %parms >= 5
     C                   eval      cx_reason = GetCxRsn(co#:group:emp#:div:
     C                                                  cx_date)
     C                   endif

     C                   if        %parms >= 6 and env = 'Q'
     C                   close     WMEMLQ
     C                   endif
     C                   return    cx_date

     PGetCxDate        e


     P*====================================================================
     P* GetCxRsn - Get cancellation reason
     P*
     P*  GetCxRsn - returns the cancellation reason code/desc
     P*   co#     - accepts the company # to search over (required)
     P*   group   - accepts the group to search over     (required)
     P*   emp#    - accepts the employer to search over  (required)
     P*   div     - accepts the employer to search over  (required)
     P*   cxDate  - accepts the cx date to search over   (required)
     P*   cxCode  - returns the cx code
     P*====================================================================

     PGetCxRsn         b                   export

     DGetCxRsn         PI            65
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D cxDate                              like(EMCAD1) const
     D cxCode                         2    options(*nopass)
      *** beg add ***** 03/16/20 **************************
     D outNcciCode                    2    options(*nopass)
     D outOvrCode                     1    options(*nopass)
     D inTransDate                    7s 0 options(*nopass)
      *** end add ***** 03/16/20 **************************

     D cxReason        s             65
     D kkkey           s                   like(CRKEY)
     D kkst            s                   like(CRST)
     D kkefdt          s                   like(CREFDT)

      /Free

       // Clean up
       Clear CXRSN;
       Clear CRDESC;
       Clear cxReason;
    
       // Get last cancellatiopn record.
       Setgt (co#:group:emp#:div:cxDate) pcx01;
       Readpe (co#:group:emp#:div:cxDate) pcx01;

       If Not %EOF;

          // Get rate set Code to get the rating state
          Chain (co#:group:CXFYR:EMP#:div) pel01;
    
          //  Get rating state
          kkst = RatingSt(ELCO#:ELRTCD);
          kkkey  = CXRSN;
         //*** beg chg ***** 03/16/20 *********
          //*** kkefdt = DateTo7(SystemDate);
          If %Parms() >= 9;
             kkefdt = inTransDate;
          Endif;
          If kkefdt = 0;
             kkefdt = DateTo7(SystemDate);
          Endif;
         //*** end chg ***** 03/16/20 *********
    
          //  Get last active reason code description for that state.
          //  as of the transaction date defined above in case the
          //  table was updated.
          Setgt (co#:kkst:kkkey:kkefdt) pcr00;
          Readpe (co#:kkst:kkkey) pcr00;

          Dow not %EOF And CRstat = 'I';
             Readpe (co#:kkst:kkkey) pcr00;
          Enddo;

          cxReason = CXRSN + ' ' + CRDESC;

          //  If cancellation is due to an endorsement to delete the division
          //  and the division has not since been re-intated,
          //  then, no reason code is found.  That is why we need to hardcoded.
          If cxReason = ' ' and CXEnfl = 'Y' and CXredt = *Zeros;
             cxReason = 'Deleted Division';
          Endif;

          If %parms >= 6;
             cxCode = CXRSN;
          Endif;

          //*** beg add ***** 03/16/20 *************
          If %parms >= 7;
             outNcciCode = CRncci;
          Endif;

          If %parms >= 8;
             outOvrCode = CRidov;
          Endif;
          //*** end add ***** 03/16/20 *************

       Endif;

       Return cxReason;

      /End-Free
     PGetCxRsn         e


     P*====================================================================
     P* GetEmpName - Get employer name
     P*
     P*  GetEmpName - returns employer name
     P*   emp#      - passed in for validation  (required)
     P*   div       - passed in for validation  (required)
     P*   emrrn     - returns the RRN of WMEMP
     P*   group     - passed in for validation
     P*   co#       - accepts the company # to search over
     P*====================================================================

     PGetEmpName       b                   export

     DGetEmpName       PI            40
     D emp#                                like(EMEMP#)                  const
     D div                                 like(EMDIV)                   const
     D emrrn                          9  0              options(*nopass)
     D group                               like(EMFND)  options(*nopass) const
     D co#                                 like(EMCO#)  options(*nopass) const
     D mode                           1A                options(*nopass) const

     D group#          s                   like(EMFND)
     D company         s                   like(EMCO#)
     D name            s                   like(EMNAME)
     D qteMode         s              1A
     D strLibrary      s             10    inz(' ')

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     C     emky          klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    emp#
     C                   kfld                    div
     C     emky3         klist
     C                   kfld                    company
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    group#

     C                   eval      company = UserCo#(q1user)
     C                   eval      group#  = UserGrp(q1user)

     C                   if        %parms >= 4
     C                   eval      group# = group
     C                   endif

     C                   if        %parms >= 5
     C                   eval      company = co#
     C                   endif

      /Free
         qteMode = *Blanks;
         If %parms >= 6;
            qteMode = mode;
            RtvEnviLib ('*CURRENT':strLibrary:*Blanks);
            If (strLibrary = RtvQuoteLib('*QTE') And qteMode = ' ') Or
                (strLibrary = RtvQuoteLib('*LIV') And qteMode = 'Q');
                If qteMode = 'Q';
                   strCommand = 'OVRDBF FILE(WMEML3) TOFILE(' +
                    %trim(RtvQuoteLib('*QTE')) + '/WMEML3)';
                Else;
                   strCommand = 'OVRDBF FILE(WMEML3) TOFILE(' +
                    %trim(RtvQuoteLib('*LIV')) + '/WMEML3)';
                Endif;
                Callp QCMDEXC(strCommand:%len(strCommand));

                Open WMEML3;
                Chain(n) emky3 WMEML3;
                If %Found(WMEML3);
                   name = L3_EMNAME;
                   emrrn = em_rrn3;
                Endif;
                Close WMEML3;
                strCommand = 'DLTOVR FILE(WMEML3)';
                Callp QCMDEXC(strCommand:%len(strCommand));
                Return name;
            Endif;

         Endif;
      /End-Free

     C                   clear                   name
     C     emky          chain     pem00                              60
     C                   if        not *in60
     C                   eval      name = EMNAME
                         //*** Begin Add *** 11/10/25 **********************
                         Else;
                            strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
                               %trim(RtvQuoteLib('*QTE')) + '/WMEMLQ)';
                            Callp QCMDEXC(%Trim(strCommand):
                                          %len(%Trim(strCommand)));
                            Open WMEMLQ;
                            Chain (company:group#:emp#:div) pem0q;
                            If %Found(wmemlq);
                               name   = EMname;
                               em_rrn = em_rrnq;
                            EndIf;
                            Close WMEMLQ;
                         //*** End   Add *** 11/10/25 **********************
     C                   endif

     C                   if        %parms >= 3
     C                   eval      emrrn = em_rrn
     C                   endif

     C                   return    name

     PGetEmpName       e


     P*====================================================================
     P* GetEmpFullName - Get employer name
     P*
     P*  GetEmpFullName - returns employer name
     P*   emp#      - passed in for validation  (required)
     P*   div       - passed in for validation  (required)
     P*   emrrn     - returns the RRN of WMEMP
     P*   group     - passed in for validation
     P*   co#       - accepts the company # to search over
     P*   nametype  - passed in for name: DBA or federal (default to DBA)
     P*   mode      - Live or Quote
     P*====================================================================

     PGetEmpFullName   b                   export

     DGetEmpFullName   PI           100
     D emp#                                like(EMemp#)                  const
     D div                                 like(EMdiv)                   const
     D emrrn                          9  0              options(*nopass)
     D group                               like(EMfnd)  options(*nopass) const
     D co#                                 like(EMco#)  options(*nopass) const
     D nameType                            like(EMpsrv) options(*nopass) const
     D mode                           1A                options(*nopass) const

     D group#          s                   like(EMfnd)
     D company         s                   like(EMco#)
     D nameFull        s            100
     D nameDbaFed      s              3
     D qteMode         s              1A
     D strLibrary      s             10    inz(' ')

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     C     emky          klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    emp#
     C                   kfld                    div
     C     emky3         klist
     C                   kfld                    company
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    group#

      /Free
                         company = UserCo#(q1user);
                         group#  = UserGrp(q1user);

                         if        %parms >= 4;
                         group# = group;
                         endif;

                         if        %parms >= 5;
                         eval      company = co#;
                         endif;

         qteMode = *Blanks;
         If %parms >= 7;
            qteMode = mode;
            RtvEnviLib ('*CURRENT':strLibrary:*Blanks);
            If (strLibrary = RtvQuoteLib('*QTE') And qteMode = ' ') Or
                (strLibrary = RtvQuoteLib('*LIV') And qteMode = 'Q');
                If qteMode = 'Q';
                   strCommand = 'OVRDBF FILE(WMEML3) TOFILE(' +
                    %trim(RtvQuoteLib('*QTE')) + '/WMEML3)';
                Else;
                   strCommand = 'OVRDBF FILE(WMEML3) TOFILE(' +
                    %trim(RtvQuoteLib('*LIV')) + '/WMEML3)';
                Endif;
                Callp QCMDEXC(strCommand:%len(strCommand));

                Open WMEML3;
                Chain(n) emky3 WMEML3;
                If %Found(WMEML3);

                   nameDbaFed = nametype;
                   select;
                    when      nameDbafed  = 'DBA';
                    nameFull = L3_EMname + L3_EMnam2;

                    when      nameDbaFed  = 'FED';
                    nameFull = L3_EMfdnm + L3_Emfdn2;
                   endsl;
                   emrrn = em_rrn3;
                Endif;
                Close WMEML3;
                strCommand = 'DLTOVR FILE(WMEML3)';
                Callp QCMDEXC(strCommand:%len(strCommand));
                Return namefull;
            Endif;
         Endif;
                         clear     nameFull;
                         chain  emky   pem00;

                         if        %parms >= 3;
                         eval      emrrn = em_rrn;
                         endif;

                       if        %parms >= 6;

                         nameDbaFed = nametype;

                          select;
                           when      nameDbafed  = 'DBA';
                           nameFull = EMname + EMnam2;

                           when      nameDbaFed  = 'FED';
                           nameFull = EMfdnm + Emfdn2;
                          endsl;

                       endif;
      /End-Free
     C                   return    nameFull

     PGetEmpFullName   e


     P*====================================================================
     P* GetEmpPol# - Get employer policy #
     P*
     P*  GetEmpPol# - returns employer policy #
     P*   elrrn     - recives the RRN for WDELP (required)
     P*   type      - if passed as blank or not passed the policy # will
     P*                be returned as WMRPP/RPCAP# if found, otherwise
     P*                GRP/EMP#/YR
     P*               if passed as 'C' the policy # will be returned as
     P*                the carrier policy # (*blank if not found)
     P*               if passed as 'E' the policy # will be returned as
     P*                GRP/EMP#/YR
     P*====================================================================

     PGetEmpPol#       b                   export

     DGetEmpPol#       PI            25
     D elrrn                          9  0 const
     D type                           1    const options(*nopass)

     D policy#         s             25
     D pol#Type        s                   like(type)
     D grp             s              3
     D emp#            s              9
     D yr              s              3

     C                   clear                   pol#Type

     C                   if        %parms >= 2
     C                   eval      pol#Type = type
     C                   endif

      *** Beg Chg ***** 08/24/21 **************************
     C**** elrrn         chain     pel0p                              60
     C     elrrn         chain(n)  pel0p                              60
      *** End Chg ***** 08/24/21 **************************

     C                   move      ELFND         grp
     C                   move      ELEMP#        emp#
     C                   move      ELFYR         yr

     C                   eval      policy# = GetCarrPol(ELCO#:ELFND:ELEMP#:
     C                                                  ELDIV:ELFYR)

     C                   if        pol#Type = 'E' or
     C                             (pol#Type = *blank and policy# = *blank)
     C                   eval      policy# = grp + emp# + yr
     C                   endif

     C                   return    policy#

     PGetEmpPol#       e


     P*====================================================================
     P* GetEmpRRN - Get employer name
     P*
     P*  GetEmpRRN - returns employer RRN
     P*   emp#     - passed in for validation  (required)
     P*   div      - passed in for validation  (required)
     P*   group    - passed in for validation
     P*   co#      - passed in for validation
     P*====================================================================

     PGetEmpRRN        b                   export

     DGetEmpRRN        PI             9  0
     D emp#                                like(EMEMP#)                  const
     D div                                 like(EMDIV)                   const
     D group                               like(EMFND)  options(*nopass) const
     D co#                                 like(EMCO#)  options(*nopass) const

     D company         s                   like(EMCO#)
     D group#          s                   like(EMFND)

     C     emky          klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    emp#
     C                   kfld                    div

     C                   eval      company = UserCo#(q1user)
     C                   eval      group#  = UserGrp(q1user)

     C                   if        %parms >= 3
     C                   eval      group# = group
     C                   endif

     C                   if        %parms >= 4
     C                   eval      company = co#
     C                   endif

     C     emky          chain     pem00                              60

     C                   return    em_rrn

     PGetEmpRRN        e


     P*====================================================================
     P* GetEmpYrRR - Get employer policy year RRN
     P*
     P*  GetEmpYrRR - return employer policy year RRN
     P*   yr        - passed in for validation  (required)
     P*   emp#      - passed in for validation  (required)
     P*   div       - passed in for validation  (required)
     P*   group     - passed in for validation
     P*   co#       - passed in for validation
     P*   yearFlg   - passed in for validation (F = First, L = Latest, blank = Current)
     P*   env       - accepts the environment Quote/live
     P*====================================================================

     PGetEmpYrRR       b                   export

     DGetEmpYrRR       PI             9  0
     D yr                                  like(ELFYR)                   const
     D emp#                                like(ELEMP#)                  const
     D div                                 like(ELDIV)                   const
     D group                               like(ELFND)  options(*nopass) const
     D co#                                 like(ELCO#)  options(*nopass) const
     D yearFlg                        1                 options(*nopass) const
     D env                            1                 options(*nopass) const

     D sysDt7          s                   like(ELPFDT)

     D company         s                   like(ELCO#)
     D group#          s                   like(ELFND)
     D quote           s               n   inz(*Off)
     D strCommand      s            255
     D empYrRrn        s              9  0 inz(0)
     D strLibrary      s             10    inz(' ')

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     C     elky1         klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    yr
     C                   kfld                    emp#
     C                   kfld                    div
     C     elky2         klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    emp#
     C                   kfld                    div

     C                   eval      company = UserCo#(q1user)
     C                   eval      group#  = UserGrp(q1user)

     C                   if        %parms >= 4
     C                   eval      group# = group
     C                   endif

     C                   if        %parms >= 5
     C                   eval      company = co#
     C                   endif

      /Free

       quote=*Off;
       If %parms >= 7;
        RtvEnviLib ('*CURRENT':strLibrary:*Blanks);
        If (strLibrary = RtvQuoteLib('*QTE') And env = ' ') Or
            (strLibrary = RtvQuoteLib('*LIV') And env = 'Q');
           If env = 'Q';
              strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
               %trim(RtvQuoteLib('*QTE')) + '/WDELLQ)';
              Callp QCMDEXC(strCommand:%len(strCommand));
              strCommand = 'OVRDBF FILE(WDELL1Q) TOFILE(' +
               %trim(RtvQuoteLib('*QTE')) + '/WDELL1Q)';
              Callp QCMDEXC(strCommand:%len(strCommand));
           Else;
              strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
               %trim(RtvQuoteLib('*LIV')) + '/WDELLQ)';
              Callp QCMDEXC(strCommand:%len(strCommand));
              strCommand = 'OVRDBF FILE(WDELL1Q) TOFILE(' +
               %trim(RtvQuoteLib('*LIV')) + '/WDELL1Q)';
              Callp QCMDEXC(strCommand:%len(strCommand));
           Endif;
           quote=*On;
           Open WDELLQ;
           Open WDELL1Q;
        EndIf;

       EndIf;

      /End-Free

     C                   if        %parms >= 6 and yr = 0
     c
     C                   select
     c
     C                   when      yearFlg = 'F'
     C     Quote         ifeq      *off
     C     elky2         setll     pel00
     C     elky2         reade     pel00
     C                   else
     C     elky2         setll     pel0q
     C     elky2         reade     pel0q
     C                   endIf
     c
     C                   when      yearFlg = 'L'
     C     Quote         ifeq      *off
     C     elky2         setgt     pel00
     C     elky2         readpe    pel00
     C                   else
     C     elky2         setgt     pel0q
     C     elky2         readpe    pel0q
     C                   Endif
     c
     C                   other
     C                   eval      sysDt7 = DateTo7(SystemDate)
     c
     C     Quote         ifeq      *off
     C     elky2         setgt     pel00
     C     elky2         readpe    pel00                                  99
     C                   else
     C     elky2         setgt     pel0q
     C     elky2         readpe    pel0q                                  99
     C                   endif
     C                   dow       elpfdt > sysDt7 and *in99=*Off
     C     Quote         ifeq      *off
     C     elky2         readpe    pel00                                  99
     C                   else
     C     elky2         readpe    pel0q                                  99
     C                   endif
     c                   enddo
     c
     c                   endsl
     c
     c                   if        *in99=*On
     C     Quote         ifeq      *off
     C     elky2         setgt     pel00
     C     elky2         readpe    pel00
     C                   else
     C     elky2         setgt     pel0q
     C     elky2         readpe    pel0q
     C                   endif
     c                   endif
     c
     C     Quote         ifeq      *off
     C                   eval      empYrRrn=el2_rrn
     C                   else
     C                   eval      empYrRrn=el2Q_rrn
     C                   endif
     c
     c                   else

     C     Quote         ifeq      *off
     C     elky1         chain     pel01                              60
     C                   else
     C     elky1         chain     pel0q1                             60
     C                   endif

     C     Quote         ifeq      *off
     C                   eval      empYrRrn=el1_rrn
     C                   else
     C                   eval      empYrRrn=el1Q_rrn
     C                   endif
     C                   endif

      /Free

       If %parms >= 7;
          If quote = *On;
             Close WDELLQ;
             Close WDELL1Q;
          Endif;
       EndIf;

      /End-Free

     C                   return    empYrRrn

     PGetEmpYrRR       e

     P*** Begin Add *** 02/22/22 *******************************************
     P*====================================================================
     P* GetNameAddress - Retrieve Employer Name/Address Information.
     P*
     P* inCompany#             - Company # (Required)
     P* inGroup#               - Group # (Required)
     P* inEmployer#            - Employer# (Required)
     P* inDivision             - Division (Required)
     P* inYear                 - Policy Year (Required)
     P* outEmployerName1       - Employer Name 1
     P* outEmployerName2       - Employer Name 2
     P* outEmployerName3       - Employer Name 3
     P* outEmployerName4       - Employer Name 4
     P* outEmployerName5       - Employer Name 5
     P* outEmployerName6       - Employer Name 6
     P* outEmployerName7       - Employer Name 7
     P* outAddressLocation     - Address Location Position
     P* outNameLocation        - Name Location Position
     P* outFederalNameLocation - Feneral Name Location Position
     P* outPEOName1            - PEO Name Line 1
     P* outPEOName2            - PEO Name Line 2
     P* outContactLocation     - Constact Location Position
     P* outAMName1             - Fully Formatted Name 1
     P* outAMName2             - Fully Formatted Name 2
     P* outAMName3             - Fully Formatted Name 3
     P* outAMName4             - Fully Formatted Name 4
     P* outAMName5             - Fully Formatted Name 5
     P* outAMName6             - Fully Formatted Name 6
     P* outAMName7             - Fully Formatted Name 7
     P* outAMName8             - Fully Formatted Name 8
     P* outAMName9             - Fully Formatted Name 9
     P* outPEOLocation         - PEO Location Position
     P* outAddressListCount    - Address List Count
     P* outNameListCount       - Name List Count
     P* outFedNameListCount    - Federal Name List Count
     P* outPEONameListCount    - PEO Name List Count
     P* outContactListCount    - Contact List Count
     P* outScripturaName1      - Name Field 1 for Scriptura
     P* outScripturaName2      - Name Field 2 for Scriptura
     P* outScripturaName3      - Name Field 3 for Scriptura
     P* outScripturaName4      - Name Field 4 for Scriptura
     P* outInsuredFein         - Insured FEIN
     P* inOrder                - Fill From Bottom if value is W
     P*                          (Optional)
     P* inNameOrder            - Name Order, employer name prints
     P*                          first or federal name prints first
     P*                          (value F) (Optional)
     P* inAddressOrder         - Determines the address print order
     P*                          (Optional)
     P* inPEOGroupFlag         - PEO Group Flag (Optional)
     P* inPrintDBALabel        - Return DBA instead of federal name
     P*                          (Optional)
     P* inPEOExtension         - Flexible field for PEO acronym to be
     P*                          used populated in place of L/C/F.
     P*                          (Optional)
     P*
     P*====================================================================

     PGetNameAddress   b                   export
     DGetNameAddress   pi
01   D inCompany#                     3  0 const
02   D inGroup#                       3  0 const
03   D inEmployer#                    9  0 const
04   D inDivision                     5  0 const
05   D inYear                         3  0 const
06   D outEmployerName1...
     D                               50
07   D outEmployerName2...
     D                               50
08   D outEmployerName3...
     D                               50
09   D outEmployerName4...
     D                               50
10   D outEmployerName5...
     D                               50
11   D outEmployerName6...
     D                               50
12   D outEmployerName7...
     D                               50
13   D outAddressLocation...
     D                                3  0
14   D outNameLocation...
     D                                3  0
15   D outFederalNameLocation...
     D                                3  0
16   D outPEOName1                   57
17   D outPEOName2                   57
18   D outContactLocation...
     D                                3  0
19   D outAMName1                    60
20   D outAMName2                    60
21   D outAMName3                    60
22   D outAMName4                    60
23   D outAMName5                    60
24   D outAMName6                    60
25   D outAMName7                    60
26   D outAMName8                    60
27   D outAMName9                    60
28   D outPEOLocation                 3  0
29   D outAddressListCount...
     D                                3  0
30   D outNameListCount...
     D                                3  0
31   D outFedNameListCount...
     D                                3  0
32   D outPEONameListCount...
     D                                3  0
33   D outContactListCount...
     D                                3  0
34   D outScripturaName1...
     D                              120
35   D outScripturaName2...
     D                              120
36   D outScripturaName3...
     D                              120
37   D outScripturaName4...
     D                              120
38   D outInsuredFein                 9  0
39   D inOrder                        1    options(*nopass)
40   D inNameOrder                    1    options(*nopass)
41   D inAddressOrder                 1    options(*nopass)
42   D inPEOGroupFlag...
     D                                1    options(*nopass)
43   D inPrintDBALabel...
     D                                1    options(*nopass)
44   D inPEOExtension...
     D                               10    options(*nopass)

     D chrOrder        s              1    Inz
     D chrNameOrder    s              1    Inz
     D chrAddressOrder...
     D                 s              1    Inz
     D chrPEOGroupflag...
     D                 s              1    Inz
     D chrPrintDBALabel...
     D                 s              1    Inz
     D chrPEOExtension...
     D                 s             10    Inz
      /free

       chrOrder = ' ';
       If %Parms() >= 39;
          chrOrder = inOrder;
       EndIf;

       chrNameOrder = 'F';
       If %Parms() >= 40;
          chrNameOrder = inNameOrder;
       EndIf;

       chrAddressOrder = 'M';
       If %Parms() >= 41;
          chrAddressOrder = inAddressOrder;
       EndIf;

       chrPEOGroupflag = ' ';
       If %Parms() >= 42;
          chrPEOGroupflag = inPEOGroupFlag;
       Else;
          Chain(n) (inCompany#:inGroup#) pfn00;
          If FNpeoF <> ' ';
             chrPEOGroupflag = ' ';
             Chain (inCompany#:inGroup#:inEmployer#:inDivision) pem00;
             Chain (inCompany#:inGroup#:inEmployer#:inDivision:inYear) pel00;
             Chain (inCompany#:ELrtCd) prs00;
             Setgt (inCompany#:inGroup#:RSjrSt:ELpfDt) prr00;
             ReadPE (inCompany#:inGroup#:RSjrSt) prr00;
             Select;
             When EMrPEO <> ' ';
                chrPEOGroupFlag = '2';
             Other;
                If RRpgFl = ' ';
                   chrPEOGroupFlag = 'B';
                Else;
                   chrPEOGroupFlag = RRpgFl;
                Endif;
             EndSl;
             If EMstRp = 'Y' And RSjrSt = 'CA';
                Select;
                When chrPEOGroupFlag = 'B';
                   chrPEOGroupFlag = 'C';
                When chrPEOGroupFlag = '3';
                   chrPEOGroupFlag = 'E';
                EndSl;
             Endif;
             If (inEmployer# = 1 and (FNpeoF = 'M' Or FNpeoF = 'C')) Or
                (FNpeoF <> 'M' And FNpeoF <> 'C');
                chrPEOGroupFlag = ' ';
             EndIf;
          EndIf;
       EndIf;

       chrPrintDBALabel = ' ';
       If %Parms() >= 43;
          chrPrintDBALabel = inPrintDBALabel;
       Else;
          // Is this a NJ policy?
          //***** Beg Chg *** 12/13/22 ******************************
          //***Chain (inCompany#:inGroup#:inYear:inEmployer#) pel02;
          //***If %Found(wdell2);
             //***DoU %Eof(wdell2);
          Chain (inCompany#:inGroup#:inYear:inEmployer#) pel01;
          If %Found(wdell1);
             DoU %Eof(wdell1);
          //***** End Chg *** 12/13/22 ******************************
                Chain (ELco#:ELrtCd) prs00;
                If %Found(wmrsl) And RSjrSt = 'NJ';
                   chrPrintDBALabel = 'Y';
                   Leave;
                Endif;
          //***** Beg Chg *** 12/13/22 ******************************
                //***ReadE (inCompany#:inGroup#:inYear:inEmployer#) pel02;
                ReadE (inCompany#:inGroup#:inYear:inEmployer#) pel01;
          //***** End Chg *** 12/13/22 ******************************
             Enddo;
          Endif;
       EndIf;

       chrPEOExtension = ' ';
       If %Parms() >= 44;
          chrPEOExtension = inPEOExtension;
       Else;
          Chain (inCompany#:inGroup#:inEmployer#:inDivision) pem00;
          Chain (inCompany#:inGroup#:inEmployer#:inDivision:inYear) pel00   ;
          Chain (inCompany#:ELrtCd) prs00;
          Setgt (inCompany#:inGroup#:RSjrSt:ELpfDt) prr00;
          ReadPE (inCompany#:inGroup#:RSjrSt) prr00;
          If FNpeoF <> ' ' And EMrPEO <> 'Y';
             chrPEOExtension = RRpeoA;
          EndIf;
       EndIf;

       SRaddr(%EditC(inCompany#:'X'):%EditC(inGroup#:'X'):
              %EditC(inEmployer#:'X'):%EditC(inDivision:'X'):
              chrOrder:chrNameOrder:chrAddressOrder:
              outEmployerName1:outEmployerName2:outEmployerName3:
              outEmployerName4:outEmployerName5:outEmployerName6:
              outEmployerName7:outAddressLocation:outNameLocation:
              outFederalNameLocation:outPEOName1:outPEOName2:
              chrPEOGroupFlag:outContactLocation:outAMName1:
              outAMName2:outAMName3:outAMName4:outAMName5:outAMName6:
              outAMName7:outAMName8:outAMName9:outPEOLocation:
              chrPrintDBALabel:outAddressListCount:outNameListCount:
              outFedNameListCount:outPEONameListCount:outContactListCount:
              chrPEOExtension:outScripturaName1:outScripturaName2:
              outScripturaName3:outScripturaName4:outInsuredFein);

      /end-free

     PGetNameAddress   e
     P*** End   Add *** 02/22/22 *******************************************

     P*====================================================================
     P* GetPremFin - Get employer premium finance co
     P*
     P*  GetPremFin - returns premimium finance co (code/name)
     P*   ELrrn     - accepts the RRN of WDELP             (required)
     P*   date      - premium finance co as of date
     P*   PFrrn     - returns the RRN of WMPFP
     P*   code      - returns the premium finance co code
     P*====================================================================

     PGetPremFin       b                   export

     DGetPremFin       PI            50
     D ELrrn                          9  0 const
     D date                            d   const options(*nopass)
     D PFrrn                          9  0       options(*nopass)
     D code                           5          options(*nopass)

     D premFinCo       s             50
     D x7efdt          s                   like(PFEFDT)
     D null_date       s               d   inz

     C     pfky          klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELPFIN

     C     pfky_1        klist
     C                   kfld                    ELCO#
     C                   kfld                    ELFND
     C                   kfld                    ELPFIN
     C                   kfld                    x7efdt

     C                   clear                   premFinCo
     C                   clear                   ELPFDT
     C                   clear                   ELPFIN
     C                   clear                   PFNAME

      *** beg chg ***** 11/17/21 *************************
     C***  ELrrn         chain     WDELP                              60
     C     ELrrn         Chain(n)  WDELP                              60
      *** end chg ***** 11/17/21 *************************

     C                   if        %parms >= 2 and date <> null_date
     C     *cymd         move      date          x7efdt
     C                   else
     C                   eval      x7efdt = ELPFDT
     C                   endif

     C     pfky_1        setgt     ppf00
     C     pfky          readpe    ppf00                                  60

     C                   eval      premFinCo = ELPFIN + ' ' + PFNAME

     C  n60              if        %parms >= 3
     C                   eval      pfrrn = pf_rrn
     C                   endif

     C  n60              if        %parms >= 4
     C                   eval      code = ELPFIN
     C                   endif

     C                   return    premFinCo

     PGetPremFin       e


     P*====================================================================
     P* GetPrvEmp# - Get previous employer #
     P*
     P*  GetPrvEmp# - returns previous employer # (Group/Emp#/Div)
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the employer to search over  (required)
     P*   p_group   - returns the previous group #
     P*   p_emp#    - returns the previous employer #
     P*   p_div     - returns the previous division
     P*====================================================================

     PGetPrvEmp#       b                   export

     DGetPrvEmp#       PI            20
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D p_group                             like(EMFND)  options(*nopass)
     D p_emp#                              like(EMEMP#) options(*nopass)
     D p_div                               like(EMDIV)  options(*nopass)

     D alpha_grp       s              3
     D alpha_emp#      s              9
     D alpha_div       s              5
     D prvEmp#         s             20
     D numOfZeros      s              3  0

     C     exky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div

     C                   clear                   prvEmp#
     C                   clear                   EXOFND
     C                   clear                   EXOEMP
     C                   clear                   EXODIV

     C     exky          chain     pex00                              60

     C                   if        not *in60

     C                   if        %parms >= 5
     C                   eval      p_group = EXOFND
     C                   eval      p_emp#  = EXOEMP
     C                   eval      p_div   = EXODIV
     C                   endif

     C                   movel(p)  EXOFND        alpha_grp
     C     '0'           check     alpha_grp     numOfZeros
     C                   eval      %subst(alpha_grp:1:numOfZeros - 1)
     C                              = '   '

     C                   movel(p)  EXOEMP        alpha_emp#
     C     '0'           check     alpha_emp#    numOfZeros
     C                   eval      %subst(alpha_emp#:1:numOfZeros - 1)
     C                              = '         '

     C                   movel(p)  EXODIV        alpha_div
     C     '0'           check     alpha_div     numOfZeros
     C                   if        numOfZeros = 0
     C                   eval      numOfZeros = 6
     C                   endif
     C                   eval      %subst(alpha_div:1:numOfZeros - 1)
     C                              = '     '
    
     C                   eval      prvEmp# = alpha_grp + ' ' + alpha_emp# +
     C                                 ' ' + alpha_div

     c                   endif

     C                   return    prvEmp#

     PGetPrvEmp#       e


     PGetQuoteSts      b                   export
     P*====================================================================
     P* Get the quote status for employer
     P*
     P*  GetRatingState - returns rating state for a policy
     P*   co#   - accepts the company # of policy (required)
     P*   group - accepts the group of policy     (required)
     P*   emp#  - accepts the employer of policy  (required)
     P*   div   - accepts the divis on of policy  (required)
     P*   yr    - accepts the yrvision of policy  (optional, default *HIVAL)
     P*====================================================================

     DGetQuoteSts      pi             1

     D inCo#                               like(EMco#)  const
     D inGroup                             like(EMfnd)  const
     D inEmp#                              like(EMemp#) const
     D inDiv                               like(EMdiv)  const
     D inYr                                like(ELfYr)  const options(*nopass)
     D inStat                         1                 const options(*nopass)

     D xxYr            s                   like(ELfYr) inz(*zero)
     D xxAgt#          s              5  0
     D xxStat          s                   like(QTstat) inz('W')
     D xxDate          s              7  0
     D xxANam          s             40
     D xxAHRrn         s              9  0
     D xxAARrn         s              9  0
     D xxAGRrn         s              9  0

      /Free

       If %parms >= 5;
          xxYr = inYr;
       Endif;
       If %parms >= 6;
          xxStat = inStat;
       Endif;
       If xxStat <> 'Q';
          xxStat = 'W';
       Endif;
       clear xxDate;
       clear xxANam;
       clear xxAHRrn;
       clear xxAARrn;
       clear xxAGRrn;
       xxAgt# = GetEmpAgt(inCo#:inGroup:inEmp#:inDiv:xxYr:xxDate:xxANam:xxAHRrn:
        xxAARrn:xxAGRrn:xxStat);
       Clear QTtype;
       Setgt (inCo#:inGroup:inEmp#:inDiv:xxStat:xxAgt#) pqt07;
       Readpe (inCo#:inGroup:inEmp#:inDiv:xxStat:xxAgt#) pqt07;
       If QTtype = 'T' and QTxFer <> *blanks;
          QTtype = QTxFer;
       //** if transfer type is BLANK, chain out to quote status table
       //** and retrieve the value of QSused instead.  If QSused is also
       //** blank, value of QTType is defaulted to incoming value.
       Else;
       If QTtype = 'T' and QTxFer = *blanks;
       Chain (inCo#:QTtype) pqs01;
       If QSused <> *blank;
          QTtype = QSused;
       Endif;
       Endif;
       Endif;
       Select;
       When QTtype = 'F';
          QTtype = 'p';
       When QTtype = 'G';
          QTtype = 'q';
       Endsl;
       Return QTtype;

      /End-Free

     PGetQuoteSts      e


     PGetQuoteStsDesc  b                   export
     P*====================================================================
     P* Get the quote status description for employer
     P*
     P*  GetRatingState - returns rating state for a policy
     P*   co#   - accepts the company # of policy (required)
     P*   group - accepts the group of policy     (required)
     P*   emp#  - accepts the employer of policy  (required)
     P*   div   - accepts the divis on of policy  (required)
     P*   yr    - accepts the yrvision of policy  (optional, default *HIVAL)
     P*====================================================================

     DGetQuoteStsDesc  pi            30

     D inCo#                               like(EMco#)  const
     D inGroup                             like(EMfnd)  const
     D inEmp#                              like(EMemp#) const
     D inDiv                               like(EMdiv)  const
     D inYr                                like(ELfYr)  const options(*nopass)
     D inStat                         1                 const options(*nopass)

     D xxYr            s                   like(ELfYr) inz(*zero)
     D xxAgt#          s              5  0
     D xxType          s                   like(QTtype)
     D xxStat          s                   like(QTstat) inz('W')

      /Free

       If %parms >= 5;
          xxYr = inYr;
       Endif;
       If %parms >= 6;
          xxStat = inStat;
       Endif;
       If xxStat <> 'Q';
          xxStat = 'W';
       Endif;
       xxType = GetQuoteSts(inCo#:inGroup:inEmp#:inDiv:xxYr:xxStat);
       Clear QSused;
       Clear QSdesc;
       Chain (inCo#:xxType) pqs01;
       If QSused <> *blank;
          Chain (inCo#:QSused) pqs01;
       Endif;
       Return QSdesc;

      /End-Free

     PGetQuoteStsDesc  e


     PGetRatingState   b                   export
     P*====================================================================
     P* Get the rating state for employer
     P*
     P*  GetRatingState - returns rating state for a policy
     P*   co#   - accepts the company # of policy (required)
     P*   group - accepts the group of policy     (required)
     P*   emp#  - accepts the employer of policy  (required)
     P*   div   - accepts the divis on of policy  (required)
     P*   yr    - accepts the yrvision of policy  (optional, default *HIVAL)
     P*   env   - accepts the environment Quote/live
     P*====================================================================

     DGetRatingState   pi             2

     D inCo#                               like(EMco#)  const
     D inGroup                             like(EMfnd)  const
     D inEmp#                              like(EMemp#) const
     D inDiv                               like(EMdiv)  const
     D inYr                                like(ELfYr)  const options(*nopass)
     D env                            1                       options(*nopass)

     D xxYr            s                   like(ELfYr) inz(*hival)
     D strCommand      s            255

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

      /Free

       If %parms >= 5 and inYr <> *zeros;
          xxYr = inYr;
       Endif;
       // Get policy year information
       Clear ELfYr;
       Clear ELrtCd;
       If %parms >= 6 and env = 'Q';
          strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WDELLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WDELLQ;
          Setgt (inCo#:inGroup:inEmp#:inDiv:xxYr) pel0q;
          Readpe (inCo#:inGroup:inEmp#:inDiv) pel0q;
       Else;
          Setgt (inCo#:inGroup:inEmp#:inDiv:xxYr) pel00;
          Readpe (inCo#:inGroup:inEmp#:inDiv) pel00;
       Endif;
       If xxYr <> *hival and ELfYr <> xxYr;
          Return *blanks;
       Endif;
       Clear RSJRST;
       Setll (ELCO#:ELRTCD) prs00;
       Reade (ELCO#:ELRTCD) prs00;
       If %parms >=6;
          Close WDELLQ;
       Endif;
       Return RSJRST;

      /End-Free

     PGetRatingState   e


     P*====================================================================
     P* GetRateSet - Get rate set code
     P*
     P*  GetRateSet - returns rate set code
     P*   ELrrn     - accepts the RRN of WDELP             (required)
     P*====================================================================

     PGetRateSet       b                   export

     DGetRateSet       PI                  like(ELRTCD)
     D ELrrn                          9  0 const

     D billSchdCd      s             10

     C                   clear                   ELRTCD

      *** beg chg ***** 11/17/21 *************************
     C***  ELrrn         chain     WDELP                              60
     C     ELrrn         Chain(n)  WDELP                              60
      *** end chg ***** 11/17/21 *************************

     C                   return    ELRTCD

     PGetRateSet       e



     P*====================================================================
     P* GetScheduleRatingFactor - Calculate/return the schedule rating factor
     P*  based on the answers to the questions
     P*
     P*  GetScheduleRatingFactor - returns schedule rating factor
     P*   co#       - accepts the company # to search over      (required)
     P*   app#      - accepts the application #                 (required)
     P*   year      - accepts the employer year to search over  (required)
     P*   group     - accepts the group to search over          (required)
     P*   emp#      - accepts the employer to search over       (required)
     P*   division  - accepts the division to search over       (required)
     P*   asOfdt    - returns the date of the latest question answered
     P*====================================================================

     PGetScheduleRatingFactor...
     P                 b                   export
     DGetScheduleRatingFactor...
     D                 pi             5  4
     D inCo#                               like(EMCO#)  const
     D inApp#                         9  0              const
     D inYear                              like(ELFYR)  const
     D inGroup                             like(EMFND)  const
     D inEmp#                              like(EMEMP#) const
     D inDiv                               like(EMDIV)  const
     D outAsOfDt                       d   options(*nopass)

     D decApp#         s              9  0 inz
     D decFactor       s              5  4 inz
     D dt7AsOf         s              7  0 inz
      /Free
       // If app# not passed in, try to find it
       Select;
       When inApp# <> *zero;
          decApp# = inApp#;
       Other;
          Exec SQL
           select EMapp# into :decApp#
            from WIEMP
            where EMco# = :inCo#
             and EMfnd = :inGroup
             and EMemp# = :inEmp#
             and EMfyr = :inYear;
       Endsl;

       // Accumulate factors for Schedule Rating questions answered affirmatively
       Exec SQL
        select sum(s.QUfactor) into :decFactor
         from WIAPPANS
          join WIQUP q on (APco#=q.QUco# and APid#=q.QUid#)
          join WIQUPSCD s on (APco#=s.QUco# and APid#=s.QUid#)
         where APco# = :inCo#
          and APapp# = :decApp#
          and APfnd = :inGroup
          and APemp# = :inEmp#
          and APfyr = :inYear
          and q.QUtype = 'Schedule Rating'
          and upper(APresp) = 'YES';

       If %parms >= 7;
          // Get the date when the last answered question was update
          Exec SQL
           select max(APdt) into :dt7AsOf
            from WIAPPANS
             join WIQUP q on (APco#=q.QUco# and APid#=q.QUid#)
             join WIQUPSCD s on (APco#=s.QUco# and APid#=s.QUid#)
            where APco# = :inCo#
             and APapp# = :decApp#
             and APfnd = :inGroup
             and APemp# = :inEmp#
             and APfyr = :inYear
             and q.QUtype = 'Schedule Rating';
          outAsOfDt = CvtToDate(dt7AsOf);
       Endif;

       Return decFactor;
      /End-free
     PGetScheduleRatingFactor...
     P                 e


     P*====================================================================
     P* GetStRptDv - Get employer state reporting division
     P*
     P*  GetStRptDv - returns employer state reporting division
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   emrrn     - returns the RRN of WMEMP
     P*====================================================================

     PGetStRptDv       b                   export

     DGetStRptDv       PI             5  0
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D emrrn                          9  0 options(*nopass)
     D strpflg                        1    options(*nopass)
     D div             s                   like(EMDIV)

     C     emky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#

     C                   clear                   div

     C     emky          setll     pem00

     C                   dou       *in60 or EMSTRP = 'Y'
     C     emky          reade     pem00                                  60
     C                   enddo

     C  n60              eval      div = EMDIV

     C  n60              if        %parms >= 4
     C                   eval      emrrn = em_rrn
     C                   endif

      /Free

                         if        %parms >= 5 and *in60 = *off;
                         strpflg = EMstrp;
                         endif;

      /End-Free

     C                   return    div

     PGetStRptDv       e


     P*====================================================================
     P* HCNPgmExists - Check for HCN Program on the policy
     P*
     P*  HCNPgmExists - return *on if exists
     P*   co#     - passed in for validation  (required)
     P*   group   - passed in for validation  (required)
     P*   emp#    - passed in for validation  (required)
     P*   div     - passed in for validation  (required)
     P*   fyr     - passed in for validation  (required)
     P*   date    - passed in for validation  (optional)
     P*   rrn     - returns rrn of WDCHP      (optional)
     P*====================================================================

     PHCNPgmExists     b                   export

     DHCNPgmExists     pi             1n
     D co#                                 like(ELCO#)  const
     D group                               like(ELFND)  const
     D emp#                                like(ELEMP#) const
     D div                                 like(ELDIV)  const
     D fyr                                 like(ELFYR)  const
     D date                           7  0              const options(*nopass)
     D rrn                            9  0                    options(*nopass)
     D*
     DinDate           s              7  0 inz(*loval)
     Ddt7              s              7  0 inz(*hival)
     Dexists           s              1n   inz(*off)
     DoutRRN           s              9  0 inz(*loval)

     C     keyWMHCL      klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    fyr

     C     key1WMHCL     klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div
     C                   kfld                    fyr
     C                   kfld                    dt7

     C                   if        %parms >= 6
     C                              and date > *zero
     C                   eval      dt7    = date
     C                   eval      inDate = date
     C                   endif
     C

     C     key1WMHCL     setll     phc00
     C                   dou       %eof
     C     keyWMHCL      reade     phc00
     C                   if        not %eof
     C                              and HCIN <> HCOUT
     C                              and (HCOUT = *zero
     C                                   or HCOUT > inDate)
     C                   eval      exists = *on
     C                   eval      outRRN = HC_rrn
     C                   leave
     C                   endif
     C                   enddo

     C                   if        %parms >= 7
     C                   eval      rrn = outRRN
     C                   endif

     C                   return    exists

     PHCNPgmExists     e


     P*====================================================================
     P* RtvQuoteLib - Retrieve the library for employer
     P*
     P*  RtvQuoteLib - returns library where WMEMP should exist
     P*   env     - passed in for envrionment requesting (required)
     P*             *QTE/*LIV
     P*====================================================================

     PRtvQuoteLib      b                   export

     DRtvQuoteLib      pi            10
     D inEnv                          4    const

     DRtvQteLib        pr                  ExtPgm('RTVQTELIB')
     D @@env                          4    const
     D @@lib                         10    const

     DoutLibrary       s             10

      /Free

       Callp RtvQteLib(inEnv:outLibrary);
       Return outLibrary;

      /End-Free
     PRtvQuoteLib      e


     P*====================================================================
     P* ValidEmp - Valid employer/division
     P*
     P*  ValidEmp - return *on if, employer (division) valid
     P*   emp#    - passed in for validation  (required)
     P*   div     - passed in for validation
     P*   emrrn   - returns the RRN of WMEMP
     P*   group   - passed in for validation
     P*   co#     - passed in for validation
     P*====================================================================

     PValidEmp         b                   export

     DValidEmp         PI             1
     D emp#                                like(EMEMP#)                  const
     D div                                 like(EMDIV)  options(*nopass) const
     D emrrn                          9  0              options(*nopass)
     D group                               like(EMFND)  options(*nopass) const
     D co#                                 like(EMCO#)  options(*nopass) const

     D division        s                   like(EMDIV)
     D group#          s                   like(EMFND)
     D company         s                   like(EMCO#)

     C     emky          klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    emp#

     C     emky_1        klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    emp#
     C                   kfld                    division

     C                   eval      company = UserCo#(q1user)
     C                   eval      group#  = UserGrp(q1user)

     C                   if        %parms >= 4
     C                   eval      group# = group
     C                   endif

     C                   if        %parms >= 5
     C                   eval      company = co#
     C                   endif

     C                   if        %parms >= 2
     C                   eval      division = div
     C     emky_1        chain     pem00                              60
     C                   else
     C     emky          chain     pem00                              60
     C                   endif

     C                   if        %parms >= 3
     C                   eval      emrrn = em_rrn
     C                   endif

     C                   return    not *in60

     PValidEmp         e


     P*====================================================================
     P* ValidEmpYr - Valid employer policy year
     P*
     P*  ValidEmpYr - return *on if, year is valid for employer
     P*   yr        - passed in for validation  (required)
     P*   emp#      - passed in for validation  (required)
     P*   div       - passed in for validation
     P*   elrrn     - returns the RRN of WDELP
     P*   group     - passed in for validation
     P*   co#       - passed in for validation
     P*====================================================================

     PValidEmpYr       b                   export

     DValidEmpYr       PI             1
     D yr                                  like(ELFYR)                   const
     D emp#                                like(ELEMP#)                  const
     D div                                 like(ELDIV)  options(*nopass) const
     D elrrn                          9  0              options(*nopass)
     D group                               like(ELFND)  options(*nopass) const
     D co#                                 like(ELCO#)  options(*nopass) const

     D division        s                   like(ELDIV)
     D company         s                   like(ELCO#)
     D group#          s                   like(ELFND)

     C     elky1         klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    yr
     C                   kfld                    emp#

     C     elky1_1       klist
     C                   kfld                    company
     C                   kfld                    group#
     C                   kfld                    yr
     C                   kfld                    emp#
     C                   kfld                    division

     C                   eval      company = UserCo#(q1user)
     C                   eval      group#  = UserGrp(q1user)

     C                   if        %parms >= 5
     C                   eval      group# = group
     C                   endif

     C                   if        %parms >= 6
     C                   eval      company = co#
     C                   endif

     C                   if        %parms >= 3
     C                   eval      division = div
     C     elky1_1       chain     pel01                              60
     C                   else
     C     elky1         chain     pel01                              60
     C                   endif

     C                   if        %parms >= 4
     C                   eval      elrrn = el1_rrn
     C                   endif

     C                   return    not *in60

     PValidEmpYr       e


     P*====================================================================
     P* GetCxDate1- Get cancellation date
     P*
     P*  GetCxDate1 - returns the cancellation date
     P*   inco#       - accepts the company # to search over (required)
     P*   ingroup     - accepts the group to search over     (required)
     P*   inemp#      - accepts the employer to search over  (required)
     P*   indiv       - accepts the employer to search over  (required)
     P*   infur       - accepts the employer to search over  (required)
     P*   outcfdt     - returns cancellation confirmed date.
     P*   outredt     - returns reinstatement date.
     P*====================================================================

     PGetCxDate1       b                   export

     DGetCxDate1       PI             7  0
     D inCo#                               like(EMco#)  const
     D inGroup                             like(EMfnd)  const
     D inEmp#                              like(EMemp#) const
     D inDiv                               like(EMdiv)  const
     D inFyr                               like(ELfyr)  const
     D outCfDt                             like(EMefd1)
     D outReDt                             like(EMefd1)

     D cx_date1        s              7  0

     c* Cancellation Detail File Key List

     c     keyWDCXL4     Klist
     c                   Kfld                    inCo#
     c                   Kfld                    inGroup
     c                   Kfld                    inEmp#
     c                   Kfld                    inDiv
     c                   Kfld                    inFyr

      /Free

              setll  keyWDCXL4  pcx04;
              reade  keyWDCXL4  pcx04;
                         If   not %eof;
                         outCfDt = CXcfdt;
                         outReDt = CXredt;
                         cx_date1 = CXcxdt;
                         endif;

                         return    cx_date1;

      /End-Free
     PGetCxDate1       e


     P*====================================================================
     P* GetEmpPhn#- Get Employer Phone#
     P*
     P*  GetEMpPhn# - returns the Emplyer Phone#
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the employer to search over  (required)
     P*   Area      - Returns Area Code
     P*====================================================================
     P*
     PGetEMpPhn#       b                   export
     P*
     DGetEMpPhn#       PI             7  0
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D Area                                like(EMAREA)


     c* Cancellation Detail File Key List

     C     keyWMEML      Klist
     C                   Kfld                    co#
     C                   Kfld                    group
     C                   Kfld                    emp#
     C                   Kfld                    div


      /Free

                   chain keyWMEML pem00;
                         If  %found;
                         Area = EMArea;
                         return    EMPhn#;
                         endif;

      /End-Free
     PGetEmpPhn#       e


     P*====================================================================
     P* GetBillSts  Get Bill Status.
     P*
     P*  GetBillSts - returns the Bill Status.
     P*   ELrrn     - passed in for validation                  (required)
     P*====================================================================

     PGetBillSts       b                   export

     DGetBillSts       PI            15
     D elRRN                          9  0 const

     D BillSts         s             15

      /Free

                 // *** Beg Chg ***** 08/24/21 **********************
                 //chain elRRn   pel0p;
                   chain(n) elRRn   pel0p;
                 // *** End Chg ***** 08/24/21 **********************
                       Select;
                         When      ELfaf = 'A';
                         eval      BillSts = 'Audited';
                         When      ELfaf = 'R';
                         eval      BillSts = 'Revised';
                         When      ELfaf = 'E';
                         eval      BillSts = 'Estimated';
                         When      ELfaf = ' ';
                         eval      BillSts = 'Not Billed';
                       Endsl;

                       return    BillSts;

      /End-Free

     PGetBillSts       e


     P*====================================================================
     P* GetEmIncDt  Get Employer Inception date.
     P*
     P*  GetEmIncDt - returns the Employer Inception date
     P*   co#     - passed in for validation    (required)
     P*   group   - passed in for validation    (required)
     P*   emp#      - passed in for validation  (required)
     P*   div       - passed in for validation  (required)
     P*====================================================================

     PGetEmIncDt       b                   export

     DGetEmIncDt       PI             7  0
     D inCo#                               like(EMco#)                  const
     D inGroup                             like(EMfnd)                  const
     D inEmp#                              like(EMemp#)                 const
     D inDiv                               like(EMdiv)                  const
     D federalId                      9  0       options(*nopass)

     D outIncDt        s              7  0

     C     emky1         klist
     C                   kfld                    inCO#
     C                   kfld                    inGroup
     C                   kfld                    inemp#
     C                   kfld                    indiv

      /Free
                 chain   emky1  pem00;
                         if        %found ;
                         outincDt = EMefd1;
                         endif;

                         if        %found and %parms >= 5;
                         federalId = EMfein;
                         endif;

                         return    outincDt;


      /End-Free


     PGetEmIncDt       e
     P*====================================================================
     P* GetIncLimit - Get Employer Increased/Liability Limits
     P*
     P*   co#     - passed in for validation  (required)
     P*   group   - passed in for validation  (required)
     P*   emp#    - passed in for validation  (required)
     P*   div     - passed in for validation  (required)
     P*   fyr     - passed in for validation  (required)
     P*   OutincLt1   Returns Increased Limit 1
     P*   OutincLt2   Returns Increased Limit 2
     P*   OutincDt1   Returns Increased Limit 1 Eff Date
     P*   OutincDt2   Returns Increased Limit 2 Eff Date
     P*   OutincDes   Returns Standard Liability limit description
     P*   Outeffdt    Returns policy year eff date
     P*   OutincLt1D  Returns Inc Limit 1 description
     P*   OutincLt2D  Returns Inc Limit 2 description
     P*====================================================================

     PGetIncLimit      b                   export

     DGetIncLimit      PI
     D inCo#                               like(EMco#)                  const
     D inGroup                             like(EMfnd)                  const
     D inEmp#                              like(EMemp#)                 const
     D inDiv                               like(EMdiv)                  const
     D inFyr                               like(ELfyr)                  const
     D inST                                like(ELilcd)
     D OutincLt1                           like(ELilcd)
     D OutincLt2                           like(ELilcd)
     D OutincDt1                           like(ELi2ef)
     D OutincDt2                           like(ELi2ef)
     D OutincDes                           like(RRemlb)
     D Outeffdt                            like(Elpfdt)
     D OutincLt1D                          like(RRemlb) options(*nopass)
     D OutincLt2D                          like(RRemlb) options(*nopass)
     D xxKey           s             10
     D xxDate          s                   like(ELPFDT)


     C     keyWDELL      klist
     C                   kfld                    inCo#
     C                   kfld                    inGroup
     C                   kfld                    inemp#
     C                   kfld                    indiv
     C                   kfld                    infyr

     C     keyWDRRL      klist
     C                   kfld                    inCo#
     C                   kfld                    inGroup
     C                   kfld                    inST

     C     keyWTLML1     Klist
     C                   Kfld                    inCO#
     C                   Kfld                    inGroup
     C                   Kfld                    inSt
     C                   Kfld                    xxKey
     C                   Kfld                    xxDate

     C     key1WTLML1    Klist
     C                   Kfld                    inCO#
     C                   Kfld                    inGroup
     C                   Kfld                    inSt
     C                   Kfld                    xxKey

      /Free

       Chain   keyWDELL pel00;
       If %found and ELilcd <> *Blanks;
          OutincLt1 = ELilcd;
          OutincLt2 = ELinl2;
          OutincDt1 = ELilef;
          OutincDt2 = ELi2ef;

          xxDate =  ELpfdt;
          Clear xxKey;
          xxKey = ELilcd;
          SetGt keyWTLML1 PLM01;
          Readpe key1WTLML1 PLM01;
          OutincLt1D = LMdesc;

          Clear xxKey;
          xxKey = ELinl2;
          SetGt keyWTLML1 PLM01;
          Readpe key1WTLML1 PLM01;
          OutincLt2D = LMdesc;
       Else;
          Chain   keyWDRRL prr00;
          If %found;
             OutincDes = RRemlb;
             Outeffdt  = Elpfdt;
          endif;
       Endif;

      /End-Free


     PGetIncLimit      e
     P*====================================================================
     P* GetPolYr - Get Policy Year
     P*
     P*   co#     - passed in to find policy year (Required)
     P*   group   - passed in to find policy year (Required)
     P*   emp#    - passed in to find policy year (Required)
     P*   div     - passed in to find policy year (Required)
     P*   date    - passed in to find policy year (Required)
     P*   Returns Policy Year if found
     P*====================================================================

     PRtvPolYr         b                   export

     DRtvPolYr         PI             3  0
     D inCo#                               like(EMco#)                  const
     D inGroup                             like(EMfnd)                  const
     D inEmp#                              like(EMemp#)                 const
     D inDiv                               like(EMdiv)                  const
     D inDate                              like(ELpFdt)                 const

     D kkPfDt          s                   Like(ELpFdt)

     C     key1WDELL2    klist
     C                   kfld                    inCo#
     C                   kfld                    inGroup
     C                   kfld                    inEmp#
     C                   kfld                    inDiv

     C     key2WDELL2    klist
     C                   kfld                    inCo#
     C                   kfld                    inGroup
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    kkPfDt

      /Free

       kkPfDt=inDate;
       SetGt key2WDELL2 pel02;
       ReadPE key1WDELL2 pel02;
       If %Eof(WDELL2);
          Return 0;
       Else;
          If inDate<=ELptDt;
             Return ELfYr;
          Else;
             Return 0;
          EndIf;
       EndIf;

      /End-Free


     PRtvPolYr         e
     P*====================================================================
     P* GetPolFrDt - Get Policy From Date
     P*
     P*   co#     - passed in to find policy from date (Required)
     P*   group   - passed in to find policy from date (Required)
     P*   emp#    - passed in to find policy from date (Required)
     P*   div     - passed in to find policy from date (Required)
     P*   fyr     - passed in to find policy from date (Required)
     P*   Returns Policy From Date
     P*====================================================================

     PRtvPolFrDt       b                   export

     DRtvPolFrDt       PI             7  0
     D inCo#                               like(EMco#)                  const
     D inGroup                             like(EMfnd)                  const
     D inEmp#                              like(EMemp#)                 const
     D inDiv                               like(EMdiv)                  const
     D inFyr                               like(ELfYr)                  const

     C     keyWDELL      klist
     C                   kfld                    inCo#
     C                   kfld                    inGroup
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr

      /Free

       Chain keyWDELL pel00;
       If %Found(WDELL);
          Return ELpFdt;
       Else;
          Return 0;
       endif;

      /End-Free


     PRtvPolFrDt       e
     P*====================================================================
     P* GetPolToDt - Get Policy To Date
     P*
     P*   co#     - passed in to find policy to date (Required)
     P*   group   - passed in to find policy to date (Required)
     P*   emp#    - passed in to find policy to date (Required)
     P*   div     - passed in to find policy to date (Required)
     P*   fyr     - passed in to find policy to date (Required)
     P*   Returns Policy To Date
     P*====================================================================

     PRtvPolToDt       b                   export

     DRtvPolToDt       PI             7  0
     D inCo#                               like(EMco#)                  const
     D inGroup                             like(EMfnd)                  const
     D inEmp#                              like(EMemp#)                 const
     D inDiv                               like(EMdiv)                  const
     D inFyr                               like(ELfYr)                  const

     C     keyWDELL      klist
     C                   kfld                    inCo#
     C                   kfld                    inGroup
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr

      /Free

       Chain keyWDELL pel00;
       If %Found(WDELL);
          Return ELpTdt;
       Else;
          Return 0;
       endif;

      /End-Free


     PRtvPolToDt       e
     P*====================================================================
     P* GetMedExcl - Get Medical Exclusion Flag
     P*
     P*   co#     - passed in to find policy to date (Required)
     P*   group   - passed in to find policy to date (Required)
     P*   emp#    - passed in to find policy to date (Required)
     P*   div     - passed in to find policy to date (Required)
     P*   fyr     - passed in to find policy to date (Required)
     P*   Returns Medical Exclusion Flag
     P*====================================================================

     PRtvMedExcl       b                   export

     DRtvMedExcl       PI             1
     D inCo#                               like(EMco#)                  const
     D inGroup                             like(EMfnd)                  const
     D inEmp#                              like(EMemp#)                 const
     D inDiv                               like(EMdiv)                  const
     D inFyr                               like(ELfYr)                  const

     C     keyWDELL      klist
     C                   kfld                    inCo#
     C                   kfld                    inGroup
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr

      /Free

       Chain keyWDELL pel00;
       If %Found(WDELL);
          Return ELexMf;
       Else;
          Return *Blanks;
       endif;

      /End-Free


     PRtvMedExcl       e
     P*====================================================================
     P* GetLatXEmp#- Get future employer #
     P*
     P*  GetLatXEmp# - returns future employer # (Group/Emp#/Div)
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the employer to search over  (required)
     P*   f_co#     - returns the future company
     P*   f_group   - returns the future group #
     P*   f_emp#    - returns the future employer #
     P*   f_div     - returns the future division
     P*   f_fyr     - returns the future year
     P*====================================================================

     PGetLatXEmp#      b                   export

     DGetLatXEmp#      PI            20
     D co#                                 like(EMco#)  const
     D group                               like(EMfnd)  const
     D emp#                                like(EMemp#) const
     D div                                 like(EMdiv)  const
     D f_co#                               like(EMco#)  options(*nopass)
     D f_group                             like(EMfnd)  options(*nopass)
     D f_emp#                              like(EMemp#) options(*nopass)
     D f_div                               like(EMdiv)  options(*nopass)
     D f_fyr                               like(ELfyr)  options(*nopass)

     D alpha_grp       s              3
     D alpha_emp#      s              9
     D alpha_div       s              5
     D futEmp#         s             20
     D numOfZeros      s              3  0
     D kkCO#           s              3  0
     D kkFND           s              3  0
     D kkEMP#          s              9  0
     D kkDIV           s              5  0
     D polToDt         s              7  0
     D sysDt7          s              7  0
     D kkFYR           s              3  0
     D polYear         s              3  0

     c     exky1         klist
     c                   kfld                    kkCO#
     c                   kfld                    kkFND
     c                   kfld                    kkEMP#
     c                   kfld                    kkDIV

     c     elky          klist
     c                   kfld                    EXco#
     c                   kfld                    EXfnd
     c                   kfld                    EXemp#
     c                   kfld                    EXdiv
     c                   kfld                    kkFYR

     c     elky_1        klist
     c                   kfld                    EXco#
     c                   kfld                    EXfnd
     c                   kfld                    EXemp#
     c                   kfld                    EXdiv

     c                   clear                   futEmp#
     c                   clear                   EXco#
     c                   clear                   EXfnd
     c                   clear                   EXemp#
     c                   clear                   EXdiv

     c                   eval      kkCO# = co#
     c                   eval      kkFND = group
     c                   eval      kkEMP#= emp#
     c                   eval      kkdiv = div

     c                   eval      sysDt7 = DateTo7(SystemDate)

     c     exky1         setll     pex01
     c     exky1         reade     pex01                                  60
     c                   Dou       *in60
     c                   If        %parms >= 5
     c                   eval      kkfyr = *Hival
     c     elky          setgt     pel00
     c     elky_1        readpe    pel00
     c                   If        not %Eof
     c                   eval      polYear = Elfyr
     c                   Eval      polToDt = RtvPolToDt(EXco#:EXFnd:
     c                              EXemp#:EXdiv:polYear)
     c                   Endif

     c                   If        (getCxDate(EXco#:EXfnd:EXemp#:EXdiv) = 0)
     c                             and (polToDt > sysDt7)
     c                   eval      f_co#   = EXco#
     c                   eval      f_group = EXfnd
     c                   eval      f_emp#  = EXemp#
     c                   eval      f_div   = EXdiv
     c                   eval      f_fyr   = polYear
     c                   Endif
     c                   Endif
     c                   eval      kkco# = EXco#
     c                   eval      kkFND = EXfnd
     c                   eval      kkEMP#= EXemp#
     c                   eval      kkDIV = EXdiv
     c     exky1         reade     pex01                                  60
     c                   enddo

     c                   eval      kkfyr = *Hival
     c     elky          setgt     pel00
     c     elky_1        readpe    pel00
     c                   If        %Found
     c                   eval      polYear = Elfyr
     c                   Eval      polToDt = RtvPolToDt(EXco#:EXFnd:
     c                              EXemp#:EXdiv:polYear)
     c                   Endif
     c                   If        (getCxDate(EXco#:EXfnd:EXemp#:EXdiv) = 0)
     c                             and (polToDt > sysDt7)
     C                   movel(p)  EXfnd         alpha_grp
     C     '0'           check     alpha_grp     numOfZeros
     C                   eval      %subst(alpha_grp:1:numOfZeros - 1)
     C                              = '   '

     C                   movel(p)  EXemp#        alpha_emp#
     C     '0'           check     alpha_emp#    numOfZeros
     C                   eval      %subst(alpha_emp#:1:numOfZeros - 1)
     C                              = '         '

     C                   movel(p)  EXdiv         alpha_div
     C     '0'           check     alpha_div     numOfZeros
     C                   if        numOfZeros = 0
     C                   eval      numOfZeros = 6
     C                   endif
     C                   eval      %subst(alpha_div:1:numOfZeros - 1)
     C                              = '     '
    
     C                   eval      futEmp# = alpha_grp + ' ' + alpha_emp# +
     C                                 ' ' + alpha_div

     c                   Endif

     C                   return    futEmp#

     PGetLatXEmp#      e


     P*====================================================================
     P* GetFutEmp# - Get Future employer #
     P*
     P*  GetFutEmp# - returns Future employer # (Group/Emp#/Div)
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the employer to search over  (required)
     P*   f_group   - returns the future group #
     P*   f_emp#    - returns the future employer #
     P*   f_div     - returns the future division
     P*====================================================================

     PGetFutEmp#       b                   export

     DGetFutEmp#       PI            20
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D f_group                             like(EMFND)  options(*nopass)
     D f_emp#                              like(EMEMP#) options(*nopass)
     D f_div                               like(EMDIV)  options(*nopass)

     D alpha_grp       s              3
     D alpha_emp#      s              9
     D alpha_div       s              5
     D futEmp#         s             20
     D numOfZeros      s              3  0

     c     exky1         klist
     c                   kfld                    co#
     c                   kfld                    group
     c                   kfld                    emp#
     c                   kfld                    div

     c                   clear                   futEmp#
     c                   clear                   EXfnd
     c                   clear                   EXemp#
     c                   clear                   EXdiv

     C     exky1         chain     pex01                              60

     C                   if        not *in60

     C                   if        %parms >= 5
     C                   eval      f_group = EXFND
     C                   eval      f_emp#  = EXEMP#
     C                   eval      f_div   = EXDIV
     C                   endif

     C                   movel(p)  EXfnd         alpha_grp
     C     '0'           check     alpha_grp     numOfZeros
     C                   eval      %subst(alpha_grp:1:numOfZeros - 1)
     C                              = '   '

     C                   movel(p)  EXemp#        alpha_emp#
     C     '0'           check     alpha_emp#    numOfZeros
     C                   eval      %subst(alpha_emp#:1:numOfZeros - 1)
     C                              = '         '

     C                   movel(p)  EXdiv         alpha_div
     C     '0'           check     alpha_div     numOfZeros
     C                   if        numOfZeros = 0
     C                   eval      numOfZeros = 6
     C                   endif
     C                   eval      %subst(alpha_div:1:numOfZeros - 1)
     C                              = '     '
    
     C                   eval      futEmp# = alpha_grp + ' ' + alpha_emp# +
     C                                 ' ' + alpha_div

     c                   endif

     C                   return    futEmp#

     PGetFutEmp#       e


     P*====================================================================
     P* GetExclMed - Get Exclude Medical Flag
     P*
     P*  GetExclMed - returns Exclude Medical Flag
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the employer to search over  (required)
     P*   year      - accepts the policy year              (required)
     P*====================================================================
     PGetExclMed       b                   export
     DGetExclMed       PI             1
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const
     D year                                like(ELFYR)  const

     c     keyWDELL      klist
     c                   kfld                    co#
     c                   kfld                    group
     c                   kfld                    emp#
     c                   kfld                    div
     c                   kfld                    year

      /Free

       Chain keyWDELL pel00;
       If %Found(WDELL);
          Return ELexMf;
       Else;
          Return ' ';
       Endif;

      /End-Free

     PGetExclMed       e
     P*====================================================================
     P* GetRnwlQte - Get Renewal Quote Flag
     P*
     P*  GetRnwlQte - returns Exclude Medical Flag
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the employer to search over  (required)
     P*====================================================================
     PGetRnwlQte       b                   export
     DGetRnwlQte       PI             1
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     D strCommand      s            255

     C     emky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div

      /Free

       strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/WMEMLQ)';
       Callp QCMDEXC(strCommand:%len(strCommand));

       Close(e) WMEMLQ;
       Open WMEMLQ;

       Chain emky pem0q;
       If %Found(WMEMLQ);
          Return EMrqfl;
       Else;
          Return ' ';
       Endif;

       Close WMEMLQ;

      /End-Free

     PGetRnwlQte       e


     P*====================================================================
     P* GetRnwlQteDiv - Get Renew to Quote Division Flag
     P*
     P*  GetRnwlQteDiv - returns Renew to Quote Division Flag
     P*   co#       - accepts the company # to search over (required)
     P*   group     - accepts the group to search over     (required)
     P*   emp#      - accepts the employer to search over  (required)
     P*   div       - accepts the employer to search over  (required)
     P*====================================================================
     PGetRnwlQteDiv    b                   export
     DGetRnwlQteDiv    PI             1
     D co#                                 like(EMCO#)  const
     D group                               like(EMFND)  const
     D emp#                                like(EMEMP#) const
     D div                                 like(EMDIV)  const

     C     emky          klist
     C                   kfld                    co#
     C                   kfld                    group
     C                   kfld                    emp#
     C                   kfld                    div

      /Free

       Chain emky pem00;
       If %Found(WMEML);
          Return EMrdfl;
       Else;
          Return ' ';
       Endif;

      /End-Free


     PGetRnwlQteDiv    e
     P*====================================================================
     P* GetEmpRating - Get Large Deduc, Retro Rating, or Schedule Rating info
     P*
     P* GetEmpRating - returns Large Deductible or Retro Rating flag
     P*   co#       - accepts the company # to search over  (required)
     P*   group     - accepts the group to search over      (required)
     P*   emp#      - accepts the employer to search over   (required)
     P*   div       - accepts the employer to search over   (required)
     P*   fyr       - accepts the emplr year to search over (required)
     P*   all       - accepts flag if entire policy is used (optional)
     P*   sched     - returns the scheduled rating for div/policy (optional)
     P*   large     - returns the large deductible for policy (optional)
     P*   retro     - returns the retro rating for policy     (optional)
     P*====================================================================

     PGetEmpRating     b                   export

     DGetEmpRating     PI             1
     D co#                            3  0 const
     D fnd                            3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D fyr                            3  0 const
     D all                            1    const options(*nopass)
     D sched                          1    options(*nopass)
     D large                          1    options(*nopass)
     D retro                          1    options(*nopass)

     D xxKey           s             10    inz(*blanks)
     D xxPlan          s              1    inz(' ')
     D xxLarge         s              1    inz(' ')
     D xxRetro         s              1    inz(' ')
     D xxSched         s              1    inz(' ')

     C     keyWDELL1     klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    fyr
     C                   kfld                    emp#
     C                   kfld                    div

     C     key1WDELL1    klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    fyr
     C                   kfld                    emp#

     C     keyWMRSL      klist
     C                   kfld                    co#
     C                   kfld                    ELRtcd

     C     keyWDRRL      klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    RSJrst
     C                   kfld                    ELPfdt

     C     key1WDRRL     klist
     C                   kfld                    co#
     C                   kfld                    fnd
     C                   kfld                    RSJrst

     C     keyWTABL      klist
     C                   kfld                    co#
     C                   kfld                    xxKey

      /Free

       If all = 'Y';
          Setll key1WDELL1 pel01;
          Reade(n) key1WDELL1 pel01;
       Else;
          Setll keyWDELL1 pel01;
          Reade(n) keyWDELL1 pel01;
       Endif;

       Dow not %eof(WDELL1);

          Chain(n)  keyWMRSL  prs00;
          Setgt     keyWDRRL  prr00;
          Readpe(n) key1WDRRL prr00;

          If not %eof(WDRRL) and RRDBCR = 'Y' and ELDBCR <> 0;
             xxSched = 'Y';
          Endif;

          xxKey = %editc(elbscd : 'X');
          Chain(n)  keyWTABL  pbs01;

          If %found;
             If ELDiv = div;
                xxPlan = BSLPGM;
             Endif;
             If BSLPGM = 'L';
                xxLarge = 'Y';
             Endif;
             If BSLPGM = 'R' or BSLPGM = 'P' or BSLPGM = 'I';
                xxRetro = 'Y';
             Endif;
          Endif;

          If all = 'Y';
             Reade(n) key1WDELL1 pel01;
          Else;
             Leave;
          Endif;

       Enddo;

       sched = xxSched;
       large = xxLarge;
       retro = xxRetro;

       Return xxPlan;

      /End-Free


     PGetEmpRating     e
     P*====================================================================
     P* GetTotPrem  - returns total premium of all divisions
     P*  co#        - accepts the company # to search over
     P*  group      - accepts the group to search over
     P*  fyr        - accepts the emplr year to search over
     P*  emp#       - accepts the employer to search over
     P*  reqFlg     - 'C' pulls premium for current status
     P*               'E' pulls estimated premium
     P*               'A' pulls audited premium
     P*  lvlPrmFlg  - 'M' for Manual
     P*               'J' for Subject
     P*               'E' for Exp Modified
     P*               'S' for Standard
     P*               'N' for Normal
     P*               'A' for Annual
     P*               'Z' for Annual - Exp Const
     P*====================================================================
     pGetTotPrem       b                   export
     dGetTotPrem       pi            13  2
     d co#                            3  0 const
     d fnd                            3  0 const
     d fyr                            3  0 const
     d emp#                           9  0 const
     d reqFlg                         1    const
     d lvlPremFLg                     1    const

     d retPremAmt      s             13  2

      /Free

       Setll (co#:fnd:fyr:emp#) pel01;
       Reade (co#:fnd:fyr:emp#) pel01;
       Dow not(%eof);
          Select;
          When lvlPremFlg = 'M';
             retPremAmt = retPremAmt + SelPremAmt(ELfaf:reqFlg:ELMAPR:ELAMPR);
          When lvlPremFlg = 'J';
             retPremAmt = retPremAmt + SelPremAmt(ELfaf:reqFlg:ELESBJ:ELASBJ);
          When lvlPremFlg = 'E';
             retPremAmt = retPremAmt + SelPremAmt(ELfaf:reqFlg:ELEMPM:ELAMPM);
          When lvlPremFlg = 'S';
             retPremAmt = retPremAmt + SelPremAmt(ELfaf:reqFlg:ELSTPR:ELASPR);
          When lvlPremFlg = 'N';
             retPremAmt = retPremAmt + SelPremAmt(ELfaf:reqFlg:ELENMP:ELANMP);
          When lvlPremFlg = 'A';
             retPremAmt = retPremAmt + SelPremAmt(ELfaf:reqFlg:ELANPR:ELAAPR);
          When lvlPremFlg = 'Z';
             retPremAmt = retPremAmt + SelPremAmt(ELfaf:reqFlg:
              (ELANPR - ELPRTD):(ELAAPR - ELPRTD));
          Endsl;
       Reade (co#:fnd:fyr:emp#) pel01;
       Enddo;
       Return retPremAmt;

      /End-Free


     pGetTotPrem       e
     P*====================================================================
     P* SelPremAmt - returns estimated or audited premium amount
     P*              depending on what the request is for and what
     P*              the status of the policy is in
     P*  polStat   - indicates audit status of policy
     P*  reqFlg    - indicates whether Audited, Current, or Estimated
     P*              Premium is requested.
     P*  estPrem   - Estimated premium from WDELP
     P*  audPrem   - Audited premium from WDELP
     P*====================================================================
     pSelPremAmt       b
     dSelPremAmt       pi            +2    like(ELANPR)
     d polStat                        1    const
     d reqFlg                         1    const
     d estPrem                             like(ELANPR) const
     d audPrem                             like(ELANPR) const

      /Free

       Select;
       When reqFlg = 'A';
          Return audPrem;
       When reqFlg = 'C';
          If polStat = 'A';
             Return audPrem;
          Else;
             Return estPrem;
          Endif;
       When reqFlg = 'E';
          Return estPrem;
       Endsl;

      /End-Free


     pSelPremAmt       e

      //***  Begin Add  ***  06/22/20  *********************************
     P*====================================================================
     P* Set the FQ lock status field (EMFQLK).  This value will be determined
     P*  by the quote status.
     P*
     P* SetFQLockSts - returns ' ' if the policy should be available to underwriter
     P*                returns 'P' if the policy should be locked to all but Portal
     P*                returns 'Y' if the policy should be locked to all but FQ
     P*                returns the current value if unable to determine locking
     P*   co#        - accepts the company #                 (required)
     P*   status     - accepts the quote status value        (required) {QTtype}
     P*   entryPoint - accepts the entry point of the policy (required) {EMiUsr}
     P*   current    - accepts the current value of EMFQLK   (optional)
     P*====================================================================
     pSetFQLockSts     b                   export
     dSetFQLockSts     pi             1
     d inCo#                          3  0 const
     d inQteSts                       1    const
     d inEntryPoint                  10    const
     d inCurrent                      1    const options(*nopass)

     dstrCurrent       s              1    inz
      /Free
       If %parms >= 4;
          strCurrent = inCurrent;
       Endif;
       Clear QSqte;
       Clear QSlocking;
       Chain (inCo#:inQteSts) pqs01;
       Select;
       When QSqte <> 'Q';
          Return strCurrent;
       When QSlocking = 'U';
          Return ' ';
       When QSlocking = 'L';
          Select;
          When inEntryPoint = 'CSPORTAL';
             Return 'P';
        //When inEntryPoint = 'WCEXPRESS';
        //   Return 'Y';
          Endsl;
       Other;
       Endsl;
       Return strCurrent;
      /End-free
     pSetFQLockSts     e
      //***  End   Add  ***  06/22/20  *********************************

     P*====================================================================
     P* GetQteMode - Get Quote Mode
     P*
     P* GetQteMode - returns 'Q' if the Employer is in Quote.
     P*              returns 'L' if the Employer is in Live.
     P*              returns '2' if the Employer is in both Live and Quote.
     P*   co#       - accepts the company # to search over  (required)
     P*   group     - accepts the group to search over      (required)
     P*   emp#      - accepts the employer to search over   (required)
     P*   div       - accepts the employer to search over   (required)
     P*   QteMode   - returns the Quote Mode for policy     (optional)
     P*====================================================================
     PGetQteMode       b                   export
     DGetQteMode       PI             1
     D co#                            3  0 const
     D fund                           3  0 const
     D emp#                           9  0 const
     D div                            5  0 const

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     D strLibrary      s             10    inz(' ')
     D QteMode         s              1    inz(' ')

      /Free

         RtvEnviLib ('*CURRENT':strLibrary:*Blanks);

         Open WDELLQ;
         Chain (co# :fund: emp#: div) WDELLQ;
         If %Found(WDELLQ);
            If strLibrary = RtvQuoteLib('*LIV');
               QteMode = 'L';
            Else;
               QteMode = 'Q';
            EndIf;
         EndIf;
         Close WDELLQ;

         If strLibrary = RtvQuoteLib('*LIV');
            strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
             %trim(RtvQuoteLib('*QTE')) + '/WDELLQ)';
         Else;
            strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
             %trim(RtvQuoteLib('*LIV')) + '/WDELLQ)';
         EndIf;

         Callp QCMDEXC(strCommand:%len(strCommand));
         Open WDELLQ;
         Chain (co# :fund: emp#: div) WDELLQ;
         If %found(WDELLQ);
            If QteMode = *BLanks;
               If strLibrary = RtvQuoteLib('*LIV');
                  QteMode = 'Q';
               Else;
                  QteMode = 'L';
               EndIf;
            Else;
               QteMode = '2';
            EndIf;

         EndIf;
         Close WDELLQ;

         strCommand = 'DLTOVR FILE(WDELLQ)';
         Callp QCMDEXC(strCommand:%len(strCommand));

         Return QteMode;

      /End-Free


     pGetQteMode       e

     P*====================================================================
     P* RtvEnviLib - Retrieve the library for the Environment
     P*
     P*  RtvEnviLib - returns environment in which the job is running
     P*   env     - *CURRENT/*OTHER is passed to fetch the library
     P*====================================================================
     PRtvEnviLib       b                   export
     DRtvEnviLib       pi
     D inEnv                         10    const
     D outLib                        10    const
     D outMLib                       10    const

     DRtvEnvLib        pr                  ExtPgm('RTVENVLIB')
     D @@env                         10    const
     D ##lib                         10    const
     D ##Mlib                        10    const

      /Free

       Callp RtvEnvLib(inEnv:outLib:outMLib);
       return;

      /End-Free

     PRtvEnviLib       e
      *
     P*====================================================================
     P* GetFQLockSts - Get FastQuote Employer lock status
     P*
     P* GetFQLockSts - returns '0' if the Employer is not locked by FQ.
     P*                returns '1' if the Employer is locked by FQ.
     P*
     P*   co#       - accepts the company # to search over  (required)
     P*   group     - accepts the group to search over      (required)
     P*   emp#      - accepts the employer to search over   (required)
     P*   div       - accepts the employer to search over   (required)
     P*   QteMode   - returns the Quote Mode for policy     (optional)
     P*   message   - accepts whether the message should be
     P*                displayed                            (optional)
     P*   app       - returns the locking app               (optional)
     P*====================================================================
     PGetFQLockSts     b                   export
     DGetFQLockSts     PI             1
     D co#                            3  0 const
     D fund                           3  0 const
     D emp#                           9  0 const
     D div                            5  0 const
     D qteMode                        1    const
     D inDspMsg                       1n   const options(*nopass)
     D outApp                        25          options(*nopass)

     DisDspMsg         s              1n   inz
     D IsFQLock        s              1A
     DstrApp           s             25    inz
     DstrMsg           s            256    inz
     DstrTitle         s             50    inz

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

      /Free
       If %open(WMEMLQ);
          close WMEMLQ;
       EndIf;
       isFQLock = '0';
       If qteMode = 'Q';
          strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMEMLQ)';
       Else;
          strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
           %trim(RtvQuoteLib('*LIV')) + '/WMEMLQ)';
       Endif;

       Callp QCMDEXC(strCommand:%len(strCommand));
       Open wmemlq;

       If %parms >= 6;
          isDspMsg = inDspMsg;
       Endif;

       Clear EMfqLk;
       Chain (co# :fund: emp#: div) wmemlq;
       If %Found(wmemlq);
          If EMfqlk <> *blank;
             isFQLock = '1';
          Endif;
       Endif;

       Select;
       When EMfqLk = 'Y';
          strApp = 'FastQuote';
       When EMfqLk = 'P';
          strApp = GetModuleProperty('Portal Module':'Name');
       Endsl;

       If %parms >= 7;
          outApp = strApp;
       Endif;

       If isFQLock = '1' and isDspMsg;
          strMsg = 'This quote is being processed within ' +
           %trim(strApp) + ' and will open for your review in inquiry.';
          strTitle = %trim(strApp) + ' Message';
          PopOneBtn(strMsg:strTitle);
       Endif;

       strCommand = 'DLTOVR FILE(WMEMLQ)';
       Callp QCMDEXC(strCommand:%len(strCommand));

       Close wmemlq;
       Return isFQLock;

      /End-Free

     pGetFQLockSts     e

     P*====================================================================
     P* GetLastDiv - Get Last Division Used
     P*
     P* GetLastDiv - Returns Last Division Used.
     P*
     P*   co#       - accepts the company # to search over  (required)
     P*   group     - accepts the group to search over      (required)
     P*   emp#      - accepts the employer to search over   (required)
     P*====================================================================
     PGetLastDivision  b                   export
     DGetLastDivision  PI             5  0
     D co#                            3  0 const
     D group                          3  0 const
     D emp#                           9  0 const

     D lastDivision    s              5  0 Inz(0)

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

      /Free

       // Make sure WMEMLQ is closed
       If %Open(WMEMLQ);
          Close wmemlq;
       EndIf;

       // Let's Look in Quote First
       strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
                     %trim(RtvQuoteLib('*QTE')) + '/WMEMLQ)';

       Callp QCMDEXC(strCommand:%len(strCommand));
       Open wmemlq;

       SetGT  (co#:group:emp#) pem0q;
       ReadPE (co#:group:emp#) pem0q;
       If Not %Eof(WMEMLQ);
          lastDivision = EMdiv;
       Endif;

       strCommand = 'DLTOVR FILE(WMEMLQ)';
       Callp QCMDEXC(strCommand:%len(strCommand));

       Close wmemlq;

       // Let's Look in Live Next
       strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
                     %trim(RtvQuoteLib('*LIV')) + '/WMEMLQ)';

       Callp QCMDEXC(strCommand:%len(strCommand));
       Open wmemlq;

       SetGT  (co#:group:emp#) pem0q;
       ReadPE (co#:group:emp#) pem0q;
       If Not %Eof(WMEMLQ);
          If EMdiv > lastDivision;
             lastDivision = EMdiv;
          EndIf;
       Endif;

       strCommand = 'DLTOVR FILE(WMEMLQ)';
       Callp QCMDEXC(strCommand:%len(strCommand));

       Close wmemlq;

       Return lastDivision;

      /End-Free

     pGetLastDivision  e

      //***  Begin Add  ***  01/19/22  *************************
      *====================================================================
      * IsUseNetRate - Determine if the policy should be using net rate
      *
      * IsUseNetRate - returns *on if using net rate, otherwise *off
      *
      *   co#       - accepts the company # to search over  (required)
      *   group     - accepts the group to search over      (required)
      *   emp#      - accepts the employer to search over   (required)
      *   div       - accepts the division to search over   (required)
      *   fyr       - accepts the year to search over       (required)
      *====================================================================
     pIsUseNetRate     b                   export
     dIsUseNetRate     pi             1n
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const
      /Free
       Clear F1netD;
       Clear F1netR;
       Clear F1netSr;
       Clear ELpfDt;
       Clear ELbsCD;
       Clear BSseFl;
       Chain(n) (inCo#:inFnd:inFyr) pf100;
       Chain(n) (inCo#:inFnd:inEmp#:inDiv:inFyr) pel00;
       Chain(n) (inCo#:%editc(ELbsCd:'X')) pbs01;
       Select;
       When F1netR = 'Y' and ELpfDt >= F1NetD;
          Return *on;
       When F1netSr = 'Y' and BSseFl = 'S';
          Return *on;
       Endsl;
       Return *off;
      /End-free
     pIsUseNetRate     e
      //***  End   Add  ***  01/19/22  *************************

      *====================================================================
      * MultiState - Determine if a policy is a multi-state policy
      *
      * MultiState - returns '0' if not a multi-state policy
      *                returns '1' if a multi-state policy
      *
      *   co#       - accepts the company # to search over  (required)
      *   group     - accepts the group to search over      (required)
      *   emp#      - accepts the employer to search over   (required)
      *   fyr       - accepts the year to search over       (optional)
      *====================================================================
     pMultiState       b                   export
     dMultiState       pi             1n
     d inCo#                          3  0 const
     d inFund                         3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const options(*nopass)

     d decYear         s              3  0 inz
     d strHldSt        s              2    inz

      /free

       If %parms >= 4;
          decYear = inFyr;
       Endif;

       SetGt (inCo#:inFund:inEmp#:*hival) pel08;
       Readpe (inCo#:inFund:inEmp#) pel08;
       If not %eof(wdell8) and decYear = 0;
          decYear = ELfyr;
       Endif;

       strHldSt = GetRatingState(inCo#:inFund:inEmp#:ELdiv:decYear);

       Setll (inCo#:inFund:inEmp#:decYear) pel08;
       Reade (inCo#:inFund:inEmp#:decYear) pel08;
       Dow not %eof(wdell8);
          If not (ELpfDt = ELptDt);
             If strHldSt <> GetRatingState(inCo#:inFund:inEmp#:ELdiv:decYear);
                Return *on;
             Endif;
          Endif;
          Reade (inCo#:inFund:inEmp#:decYear) pel08;
       Enddo;

       Return *off;

      /end-free

     pMultiState       e

     *====================================================================
      * ChkRnwStatus - Checks Renewal Flag from State Reporting division
      *              - Returns *on if policy is flagged to renew
     *  co#         - accepts the company # to search over
     *  group       - accepts the group to search over
     *  emp#        - accepts the employer to search over
     *  fyr         - accepts the emplr year to search over
      *  errorMsg    - returns errorMsg (optional)
     *====================================================================
     pChkRnwStatus     b                   export
     dChkRnwStatus     pi              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d fyr                            3  0 const
     d errorMsg                     200    options(*nopass)
     d stRptDiv        s              5  0
     d strMsgPrefix    s             50    inz

      /free
       Select;
       When %parms > 5 and errorMsg = 'Renewal';
          strMsgPrefix = conNotRenewed;
       When %parms > 5 and errorMsg = 'PolTBar';
          strMsgPrefix = conCantRenew;
       Endsl;

       stRptDiv = GetStRptDv(co#:group:emp#);
       Chain (co#:group:emp#:stRptDiv:fyr) pel00;
       If %found and ELrlfl = 'Y';
          Select;
          When %parms > 5;
             errorMsg = %trim(strMsgPrefix) + ' Do not renew flag set in ' +
              'state reporting division.';
             Return *off;
          Endsl;
       Endif;
       Chain (co#:group:emp#:div:fyr) pel00;
       If %found and ELrlfl = 'Y';
          Select;
          When %parms > 5;
             errorMsg = %trim(strMsgPrefix) + ' Do not renew flag set in ' +
              'employer detail.';
             Return *off;
          Endsl;
       Endif;
       Select;
       When %parms > 5 and (errorMsg = 'Renewal' or errorMsg = 'PolTBar');
          errorMsg = *blanks;
          Return *on;
       Endsl;
      /end-free
     pChkRnwStatus     e
     *====================================================================
      * ChkCxStatus - Retrieves cancellation status of division
      *             - Returns *on if division has cancellation date
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  div        - accepts the division to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     pChkCxStatus      b                   export
     dChkCxStatus      pi              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d errorMsg                     200    options(*nopass)
     d cxDate          s              7  0
     d sysDate         s              7  0
     d charDate        s             10
     d stRptDiv        s              5  0
     d strMsgPrefix    s             50    inz

      /free
       Select;
       When %parms > 4 and errorMsg = 'Renewal';
          strMsgPrefix = conNotRenewed;
       When %parms > 4 and errorMsg = 'PolTBar';
          strMsgPrefix = conCantRenew;
       Endsl;

       // Check division passed in for cx date
       cxDate   = GetCxDate(co#:group:emp#:div);
       sysDate  = DateTo7(SystemDate);
       charDate = AlphaUSA(CvtToDate(cxDate));
       if cxDate <> 0;
          Select;
          When %parms > 4 and cxDate > sysDate;
             errorMsg = %trim(strMsgPrefix) + ' Pending cancellation ' +
              'effective ' + charDate + '.';
             Return *on;
          When %parms > 4 and cxDate <= sysDate;
             errorMsg = %trim(strMsgPrefix) + ' Employer cancelled ' +
              'effective ' + charDate + '.';
             Return *on;
          Endsl;
        //***  Begin Add  ***  10/19/21  *************
          Return *on;
        //***  End   Add  ***  10/19/21  *************
       Endif;
       // Check if state reporting div is cancelled.
       stRptDiv = GetStRptDv(co#:group:emp#);
       cxDate   = GetCxDate(co#:group:emp#:stRptDiv);
       charDate = AlphaUSA(CvtToDate(cxDate));
       if cxDate <> 0;
          Select;
          When %parms > 4 and cxDate > sysDate;
             errorMsg = %trim(strMsgPrefix) + ' Pending cancellation ' +
              'effective ' + charDate + '.';
             Return *on;
          When %parms > 4 and cxDate <= sysDate;
             errorMsg = %trim(strMsgPrefix) + ' Employer cancelled ' +
              'effective ' + charDate + '.';
             Return *on;
          Endsl;
        //***  Begin Add  ***  10/19/21  *************
          Return *on;
        //***  End   Add  ***  10/19/21  *************
       Endif;
       Select;
       When %parms > 4 and (errorMsg = 'Renewal' or errorMsg = 'PolTBar');
          errorMsg = *blanks;
          Return *off;
       Endsl;
       //***  Begin Add  ***  10/19/21  *************
       Return *off;
       //***  End   Add  ***  10/19/21  *************
      /end-free
     pChkCxStatus      e
     *====================================================================
      * ChkFutDtl   - Checks if policy year has already been renewed to
      *                Live or Quote
      *             - Returns *on if so
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  div        - accepts the division to search over
     *  fyr        - accepts the emplr year to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     pChkFutDtl        b                   export
     dChkFutDtl        pi              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d fyr                            3  0 const
     d errorMsg                     200    options(*nopass)
     d strCommand      s            255

     dQCMDEXC          pr                  ExtPgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     d strMsgPrefix    s             50    inz

      /free
       Select;
       When %parms > 5 and errorMsg = 'Renewal';
          strMsgPrefix = conNotRenewed;
       When %parms > 5 and errorMsg = 'PolTBar';
          strMsgPrefix = conCantRenew;
       Endsl;

       Chain (co#:group:emp#:div:fyr) pel00;
       Setgt (co#:group:emp#:div:ELpfdt) pel02;
       Reade (co#:group:emp#:div) pel02;
       Select;
       When %eof;
       When %parms > 5;
          errorMsg = %trim(strMsgPrefix) +  ' Policy year for the new ' +
           'employer policy year exists in Live.';
          Return *on;
       Other;
          Return *on;
       Endsl;
       //***  Begin Add  ***  03/04/21  ***********************
       If ClaimsOnly();
          Return *off;
       Endif;
       //***  End   Add  ***  03/04/21  ***********************
       strCommand = 'OVRDBF FILE(WDELL2Q) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/WDELL2Q)';
       Callp QCMDEXC(strCommand:%len(strCommand));
       Open WDELL2Q;
       Setgt (co#:group:emp#:div:ELpfdt) pel0q2;
       Reade (co#:group:emp#:div) pel0q2;
       Close WDELL2Q;
       strCommand = 'DLTOVR FILE(WDELL2Q)';
       Callp QCMDEXC(strCommand:%len(strCommand));
       Select;
       When %eof;
          If %parms > 5;
             errorMsg = *blanks;
          Endif;
          Return *off;
       When %parms > 5;
          errorMsg = %trim(strMsgPrefix) + ' Policy year for the new ' +
           'employer policy year exists in Quote.';
          Return *on;
       Other;
          Return *on;
       Endsl;
      /end-free

     pChkFutDtl        e
     *====================================================================
      * ChkQteEmp   - Checks if employer exists in quote
      *             - Returns *on if so
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  div        - accepts the division to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     pChkQteEmp        b                   export
     dChkQteEmp        pi              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d errorMsg                     200    options(*nopass)

     dQCMDEXC          pr                  ExtPgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const

     d strMsgPrefix    s             50    inz

      /Free
       Select;
       When %Parms > 4 and errorMsg = 'Renewal';
          strMsgPrefix = conNotRenewed;
       When %Parms > 4 and errorMsg = 'PolTBar';
          strMsgPrefix = conCantRenew;
       Endsl;

       strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
        %trim(RtvQuoteLib('*QTE')) + '/WMEMLQ)';
       Callp QCMDEXC(strCommand:%len(strCommand));
       Open WMEMLQ;
       Chain (co#:group:emp#:div) pem0q;
       Close WMEMLQ;
       strCommand = 'DLTOVR FILE(WMEMLQ)';
       Callp QCMDEXC(strCommand:%len(strCommand));
       Select;
       When Not(%Found);
         If %Parms > 4;
            errorMsg = *Blanks;
         Endif;
         Return *Off;
       Other;
          If %Parms > 4;
             errorMsg = %trim(strMsgPrefix) + ' This employer already ' +
              'exists in the quote envirionment.';
          Endif;
          Return *On;
       Endsl;
      /End-free

     pChkQteEmp        e
      *====================================================================
      * ChkPolDates - Checks if future group policy year exists to renew to
      *               and if new policy from date lands in the future group
      *               policy year date range
      *               Validates that the expiration date is valid to
      *               to renew. It can't exist in the current fund year.
      *             - Returns *on if so
     *  co#        - accepts the company # to search over
     *  group      - accepts the group to search over
     *  emp#       - accepts the employer to search over
     *  div        - accepts the division to search over
     *  fyr        - accepts the emplr year to search over
      *  errorMsg   - returns errorMsg (optional)
     *====================================================================
     pChkPolDates      b                   export
     dChkPolDates      pi              n
     d co#                            3  0 const
     d group                          3  0 const
     d emp#                           9  0 const
     d div                            5  0 const
     d fyr                            3  0 const
     d errorMsg                     200    options(*nopass)
     d newPolFrDate    s              7  0
     d stRptDiv        s              5  0
     d strMsgPrefix    s             50    inz

      /free
       Select;
       When %parms > 5 and errorMsg = 'Renewal';
          strMsgPrefix = conNotRenewed;
       When %parms > 5 and errorMsg = 'PolTBar';
          strMsgPrefix = conCantRenew;
       Endsl;

       // Don't renew division if state reporting division has bad dates.
       stRptDiv = GetStRptDv(co#:group:emp#);
       Chain (co#:group:emp#:stRptdiv:fyr) pel00;
       newPolFrDate = ELptdt;
       Setgt (co#:group:ELpfdt) pf101;
       Reade (co#:group) pf101;
       Select;
       When %eof;
          errorMsg = %trim(strMsgPrefix) + ' No group detail for ' +
           'the new employer policy year.';
          Return *off;
       When newPolFrDate >= F1enpo;
          errorMsg = %trim(strMsgPrefix) + ' New policy effective date ' +
           'is not within group policy date range.';
          Return *off;
       When newPolFrDate < F1stpo;
          Select;
          When %parms > 5 and errorMsg = 'Renewal';
             errorMsg = 'Policy from and to date within same year ' +
              'preventing creation of renewal year - rewrite of ' +
              'policy required to renew.';
             Return *off;
          Endsl;
       Endsl;
       Select;
       When %parms > 5 and (errorMsg = 'Renewal' or errorMsg = 'PolTBar');
          errorMsg = *blanks;
          Return *on;
       Endsl;
      /end-free
     pChkPolDates      e
       //===================================================================
       // PolPageProcessed - checks if policy page generated
       //             - Returns *on if so
       // co#         - accepts the company # to search over
       // group       - accepts the group to search over
       // emp#        - accepts the employer to search over
       // fyr         - accepts the year to search over
       // inState     - (optional)
       //==================================================================
     pPolPagProcessed  b                   export
     dPolPagProcessed  pi              n
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d inState                        2    options(*nopass)

      /free

       If SubHistStatus(inCo#:inGroup:inEmp#:inFyr) > '2';
          Return *on;
       Endif;

       Return *off;

      /end-free

     pPolPagProcessed  e

       //***  Begin Add  ***  01/22/20  *************************
       //===================================================================
       // Create 'with' conditions to build policy information including data from:
       //  WDELP, WMEMP, WMAHP, WMAAP
       //             - Returns a partial SQL string to be included in 'with'
       // as of Date  - agent history as of date (current date if not passed)
       //==================================================================
     pSqlWithPolicyInfo...
     p                 b                   export
     dSqlWithPolicyInfo...
     d                 pi          5000    varying
     d inAsOfDate                      d   options(*nopass)
     d
     ddtAsOf           s               d   inz(*sys)
     ddtAsOf7          s              7  0
     ddtNull           s               d   inz
     dstrWith          s           5000    varying
      /Free
       If %parms >= 1 and inAsOfDate <> dtNull;
          dtAsOf = inAsOfDate;
       Endif;
       dtAsOf7 = DateTo7(dtAsOf);
       strWith = ' ' +
        'max_agt as ' +
        '(select AHco#, AHfnd, AHemp#, AHdiv, AHfyr, max(AHefDt) max_date ' +
        '  from WMAHP ' +
        '  where AHstat = ''A'' and AHefDt <= ' + %editc(dtAsOf7:'X') +
        '  group by AHco#, AHfnd, AHemp#, AHdiv, AHfyr), ' +
        'agent_history as ' +
        '(select a.* ' +
        '  from WMAHP a ' +
        '  join max_agt m ' +
        '   on a.AHco# = m.AHco# and a.AHfnd = m.AHfnd ' +
        '    and a.AHemp# = m.AHemp# and a.AHdiv = m.AHdiv ' +
        '    and a.AHfyr = m.AHfyr and a.AHefDt = m.max_date ' +
        '  where AHstat = ''A''), ' +
        'agent_info as ' +
        '(select * '+
        '  from agent_history ' +
        '  left join WMAAP ' +
        '   on AAco# = AHco# and AAfnd = AHfnd and AAagt# = AHagt# ' +
        '  left join WMAGP ' +
        '   on AGco# = AAco# and AGfein = AAfein and AGmod = AAmod), ' +
        'policy_number as ' +
        '(select ELco# as RPco#, ELfnd as RPfnd, ELemp# as RPemp#, ' +
        '  ELdiv as RPdiv, ELfyr as RPfyr, RPCAP#, ' +
        '  case when (RPcap# is null or RPcap# = '''') ' +
        '   then digits(ELfnd)||''-''||digits(ELemp#)||''-''||digits(ELfyr) ' +
        '  else RPcap# end as policy# ' +
        '  from WDELP ' +
        '  left join WMRPP ' +
        '   on ELco# = RPco# and ELfnd = RPfnd ' +
        '    and ELemp# = RPemp# and ELdiv = RPdiv ' +
        '    and ELfyr = RPfyr), ' +
       //*** beg chg ***** 03/22/22 *****************
       // 'policy_info as ' +
       // '(select * ' +
       // '  from WDELP ' +
       // '  left join WMEMP ' +
       // '   on ELco# = EMco# and ELfnd = EMfnd ' +
       // '    and ELemp# = EMemp# and ELdiv = EMdiv ' +
       // '  left join agent_info ' +
       // '   on ELco# = AHco# and ELfnd = AHfnd ' +
       // '    and ELemp# = AHemp# and ELdiv = AHdiv ' +
       // '    and ELfyr = AHfyr ' +
       // '  left join WMRSP ' +
       // '   on ELco# = RSco# and ELrtCd = RSrtCd ' +
       // '  left join policy_number ' +
       // '   on ELco# = RPco# and ELfnd = RPfnd ' +
       // '    and ELemp# = RPemp# and ELdiv = RPdiv ' +
       // '    and ELfyr = RPfyr)';
        'policy_info as ' +
        '(select * ' +
         'from WDELP ' +
         'join WMEMP ' +
         ' on ELco# = EMco# and ELfnd = EMfnd ' +
         ' and ELemp# = EMemp# and ELdiv = EMdiv ' +
         'left join agent_info ' +
         ' on ELco# = AHco# and ELfnd = AHfnd ' +
         ' and ELemp# = AHemp# and ELdiv = AHdiv ' +
         ' and ELfyr = AHfyr ' +
         'join WMRSP ' +
         ' on ELco# = RSco# and ELrtCd = RSrtCd ' +
         'join policy_number ' +
         ' on ELco# = RPco# and ELfnd = RPfnd ' +
         ' and ELemp# = RPemp# and ELdiv = RPdiv ' +
         ' and ELfyr = RPfyr ' +
         'join WMOFP ' +
         ' on EMco# = OFco# ' +
         ' and EMoff# = OFofc# ' +
        ')';
       //*** end chg ***** 03/22/22 *****************
       Return strWith;
      /End-free
     pSqlWithPolicyInfo...
     p                 e
       //***  End   Add  ***  01/22/20  *************************
       //==================================================================
       // SubHistStatus - checks if policy page generated
       //               - Returns *on if so
       // co#           - accepts the company # to search over
       // group         - accepts the group to search over
       // emp#          - accepts the employer to search over
       // fyr           - accepts the year to search over
       // inState       - (optional)
       //====================================================================
     pSubHistStatus    b                   export
     dSubHistStatus    pi             1
     d inCo#                          3  0 const
     d inGroup                        3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d inState                        2    options(*nopass)

      /free
       Open wmshl1;

       Chain (inCo#:inGroup:inEmp#:inFyr:'P') psh01;
       If %found(WMSHL1);
          Close wmshl1;
          Return SHstat;
       Endif;

       Close wmshl1;
       Return ' ';

      /end-free

     pSubHistStatus    e

      //***  Begin Add  ***  06/22/20  *********************************
     P*====================================================================
     P* Update the FQ lock status field (EMFQLK).  This value will be determined
     P*  by the quote status.
     P*
     P* UpdFQLockSts
     P*   co#        - accepts the company #                 (required)
     P*   fnd        - accepts the group                     (required)
     P*   emp#       - accepts the employer #                (required)
     P*   div        - accepts the division                  (required)
     P*   status     - accepts the quote status value        (required) {QTtype}
     P*====================================================================
     pUpdFQLockSts     b                   export
     dUpdFQLockSts     pi
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inQteSts                       1    const

     ddsWMEMP        e ds                  extname(WMEMP) inz qualified
     ddsWTQSP        e ds                  extname(WTQSP) inz qualified
     dstrFqLk          s              1
     dstrPgm           s             10
     dsysDt7           s              7  0
     dsysTime6         s              6  0
      /Free
       Exec sql
        select *
         into :dsWMEMP
         from WMEMP
         where EMco# = :inCo# and EMfnd = :inFnd
          and EMemp# = :inEmp# and EMdiv = :inDiv;
       If not IsValidFetch(sqlstt);
          Return;
       Endif;
       strFqLk = SetFQLockSts(inCo#:inQteSts:dsWMEMP.EMiUsr:dsWMEMP.EMfqLk);
       If dsWMEMP.EMfqLk <> strFqLk;
          strPgm = GetWhatCalled(q1Pgm);
          sysDt7 = DateTo7(SystemDate());
          sysTime6 = TimeTo6(SystemTime());
          Exec sql
           update WMEMP
            set EMfqLk = :strFqLk,
             EMpgm = :strPgm, EMuser = :q1User,
             EMdt = :sysDt7, EMtime = :sysTime6
            where EMco# = :inCo# and EMfnd = :inFnd
             and EMemp# = :inEmp# and EMdiv = :inDiv;
       Endif;
      /End-free
     pUpdFQLockSts     e
      //***  End   Add  ***  06/22/20  *********************************

     P*=================================================================
     P* ChkRenewal - Determine if it is a renewal
     P*
     P*  ChkRenewal - returns *on if renewal, otherwise *off
     P*   inCo#      - accepts the current company          (required)
     P*   inGrp      - accepts the current group            (required)
     P*   inEmp#     - accepts the current employer         (required)
     P*   inFyr      - accepts the current year             (required)
     P*   inQte      - accepts the environment              (required)
     P*====================================================================
     PChkRenewal       b                   export
     DChkrenewal       pi             1n
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inEmp#                         9  0 const
     D inFyr                          3  0 const
     D inQte                          1    const

     D dtCurrPfDt      s               d   inz
     D dtPrYrPtDt      s               d   inz
     D svPfDt          s              7  0 inz
     D intStRptDiv     s              5  0 inz

      /Free
       If not %open(wdell2q) and inQte = 'Q';
          strCommand = 'OVRDBF FILE(WDELL2Q) TOFILE(' +
          %trim(RtvQuoteLib('*LIV')) + '/WDELL2Q)';
          QCMDEXC(%trim(strCommand):%len(strCommand));
          Open wdell2q;
       Endif;

       // get current dates from state rpt division
       intStRptDiv = GetStRptDv(inCo#:inGroup:inEmp#);
       Setgt  (inCo#:inGroup:inEmp#:intStRptDiv:inFyr) Wdell;
       Readpe (inCo#:inGroup:inEmp#:intStRptDiv:inFyr) Wdell;
       dtCurrPfDt = cvtToDate(ELpfdt);
       svPfDt = ELpfdt;
       Clear ELptdt;

       // Read prior year in current environment
       Readpe (inCo#:inGroup:inEmp#:intStRptDiv) Wdell;

       // Look in Live if in Quote
       If %Eof and inQte = 'Q';
          Setll  (inCo#:inGroup:inEmp#:intstRptDiv:svPfDt) Wdell2q;
          Readpe (inCo#:inGroup:inEmp#:intStRptDiv) Wdell2q;
       Endif;

       // Check cross reference if no hit so far
       If %Eof;

          Chain (inCo#:inGroup:inEmp#:intStRptDiv) wzexl;
          If %found;

            //Look in current environment
             Setll  (EXco#:EXofnd:EXoemp:EXodiv:svPfDt) Wdell2;
             Readpe (EXco#:EXofnd:EXoemp:EXodiv) Wdell2;

            // Look in live if in quote
             If %Eof and inQte = 'Q';
                Setll  (EXco#:EXofnd:EXoemp:EXodiv:svPfDt) Wdell2q;
                Readpe (EXco#:EXofnd:EXoemp:EXodiv) Wdell2q;
             Endif;

          Endif;

       Endif;

       If ELptdt <> 0;
          dtPrYrPtDt = CvtToDate(ELptdt);
       Endif;

       If %Open(wdell2q);
          Close wdell2q;
       Endif;

       Select;
        // It's new business if prior year doesn't exist
       When ELptDt = *zero;
          Return *off;
        // It's new business if cross reference group has a
        //  different carrier
       When ELfnd <> inGroup and not SameCarrier(ELco#:inGroup:ELfnd);
          Return *off;
        // It's new business if coverage lapsed
       When %diff(dtCurrPfDt:dtPrYrPtDt:*days) > 1;
          Return *off;
        // It's renewal business if none of the above
        //  conditions are met.
       Other;
          Return *on;
       Endsl;

      /End-free
     pChkRenewal       e
      *====================================================================
      * GetStRptPDays - returns the number of policy days (Subtracts
      *  for leap day)
      *   inCo#  - accepts the current company   (required)
      *   inGrp  - accepts the current group     (required)
      *   inEmp# - accepts the current employer  (required)
      *   inFyr  - accepts the current year      (required)
      *   inDiv  - accepts the division          (required)
      *====================================================================
     pGetStRptPDays    b                   export
     dGetStRptPDays    pi             3  0
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d inDiv                          5  0 const

     d chrPeoFlg       s              1
     d errleapDate     s               n
     d fromDate        s               d
     d intCxDate       s              7  0
     d intDiv          s              5  0
     d intDiv#         s              5  0
     d outRrn          s              9  0
     d outStRptFlg     s              1
     d strCommand      s            255
     d strLibrary      s             10    inz(' ')
     d stRptDiv        s              5  0
     d toDate          s               d
     d #OfDays         s              3  0

     d****** Beg Add ***** 01/15/20 ******************************
     d EffWorkDate     s                   like(CREFDT)
     d ExpWorkDate     s                   like(CREFDT)
     d****** End Add ***** 01/15/20 ******************************

      /Free
       intDiv# = inDiv;
       stRptDiv = GetStRptDv(inCo#:inFnd:inEmp#:outRrn:outStRptFlg);
       chrPeoFlg = GetPeoFlag(inFnd);

       Select;
       When outStRptFlg = 'Y';
          intDiv = stRptDiv;
          Exsr Get#OfDays;
       When outStRptFlg = *blank and chrPeoFlg <> 'S';
          intDiv = intDiv#;
          Exsr Get#OfDays;
       When outStRptFlg = *blank and chrPeoFlg =  'S';
          RtvEnviLib ('*CURRENT':strLibrary:*Blanks);
          If (strLibrary = RtvQuoteLib('*QTE'));
             Exsr GetPeo#OfDays;
          Else;
             intDiv = intDiv#;
             Exsr Get#OfDays;
          Endif;
       Endsl;

       Return #OfDays;

       //====================================
       Begsr Get#OfDays;
       //====================================
       intCxDate = GetCxDate(inCo#:inFnd:inEmp#:intDiv);
       //*** Beg Chg ***** 01/15/20 ******************************
       Chain (inCo#:inFnd:inEmp#:intDiv:inFyr) pel00;
       If not %found;
          Exec Sql
          Select Min(ELpfdt), Max(ELptdt)
          Into :EffWorkDate, :ExpWorkDate
          From WDELP
          Where ELco# = :inCo#
          And ELfnd = :inFnd
          And ELemp# = :inEmp#
          And ELfyr = :inFyr;
          If SqlStt = '00000';
             ELpfdt = EffWorkDate;
             ELptdt = ExpWorkDate;
             Else;
             Return *Zero;
           Endif;
       Endif;
       //*** End Chg ***** 01/15/20 ******************************
       Exsr Calc#OfDays;
       Endsr;

       //====================================
       Begsr GetPeo#OfDays;
       //====================================
       If not %open(WMEMLQ);
          strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
           %trim(RtvQuoteLib('*LIV')) + '/WMEMLQ)';
          Callp QCMDEXC(%trim(strCommand):%len(strCommand));
          Open WMEMLQ;
       Endif;

       If not %open(WDELLQ);
          strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
           %trim(RtvQuoteLib('*LIV')) + '/WDELLQ)';
          Callp QCMDEXC(%trim(strCommand):%len(strCommand));
          Open wdellq;
       Endif;

       Chain (inCo#:inFnd:1:0) pem0q;
       Select;
       When EMefd5 <> *zero;
          intCxDate = EMcad5;
       When EMefd4 <> *zero;
          intCxDate = EMcad4;
       When EMefd3 <> *zero;
          intCxDate = EMcad3;
       When EMefd2 <> *zero;
          intCxDate = EMcad2;
       When EMefd1 <> *zero;
          intCxDate = EMcad1;
       Other;
          intCxDate = *Zero;
       Endsl;

       Chain (inCo#:inFnd:1:0:inFyr) pel0q;
       Exsr Calc#OfDays;

       If %Open(WMEMLQ);
          Close WMEMLQ;
       EndIf;
       If %Open(WDELLQ);
          Close WDELLQ;
       EndIf;

       Endsr;

       //==================================
       Begsr Calc#OfDays;
       //==================================
       fromDate = %Date(ELpfdt:*cymd);

       Select;
       When intCxDate <> 0 and intCxDate <= ELPtdt
        and intCxDate >= ELPfdt;
          toDate = %Date(intCxDate:*cymd);
       Other;
          toDate = %Date(ELptdt:*cymd);
       Endsl;

       #OfDays  = %Diff(toDate:fromDate:*days);
       If RangeHas0229(fromDate:toDate) and #OfDays <> 0;
          #OfDays -= 1;
       Endif;

       Endsr;
      /End-Free

     pGetStRptPDays    e
      *====================================================================
      * EdtPeoPolType - Returns Error Message
      *   inCo#     - accepts the current company     (required)
      *   inGrp     - accepts the current group       (required)
      *   inEmp#    - accepts the current employer    (required)
      *   inDiv     - accepts the division            (required)
      *   inJrSt    - accepts the jurisdiction state  (required)
      *   inEfDt    - accepts the effective date      (required)
      *   inCall    - accepts Where it is called from (Optional)
      *====================================================================
     pEdtPeoPolType    b                   export
     dEdtPeoPolType    pi           100
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inJrSt                         2    const
     d inEfDt                         7  0 const
     d inCall                         1    const options(*nopass)

     d errorMessage    s            100    Inz(' ')
     d peoFlag         s              1    Inz(' ')
     d peoPolicyType   s              1    Inz(' ')
     d strAppId        s             10    Inz('MCPMastOvr')
     d strMsg          s            200    Inz(' ')
     d strInCall       s              1    Inz(' ')

      /Free

       // Retrieve State PEO Policy Type
       SetGT  (inCo#:inFnd:inJrSt:inEfDt) prr00;
       ReadPE (inCo#:inFnd:inJrSt)        prr00;
       If Not %Eof(wdrrl);
          peoPolicyType = RRpeoF;
       EndIf;

       // Retrieve Group PEO Policy Type
       peoFlag = GetPeoFlag(inFnd);

       If %parms >= 7;
          strInCall = inCall;
       Else;
          strInCall = *blanks;
       Endif;

       Select;
       When strInCall = 'L';
          strMsg = 'The rating state for this policy/quote/division ' +
           'indicates this policy/quote/division should not be ' +
           'entered as the state is set as a Master/MCP policy ' +
           'state. Press Override to proceed or Cancel.';
       When strInCall = 'P';
          strMsg = 'The rating state for this policy/quote/division ' +
           'indicates this rate set code should not be changed ' +
           'as the state is set as a Master/MCP policy ' +
           'state. Press Override to proceed or Cancel.';
       Other;
          Clear strMsg;
       Endsl;

       // Group needs to be a Combined PEO group or the edits below don't matter
       If peoFlag = 'C';
          // State PEO Policy Type must be set
          If peoPolicyType  = ' ';
             errorMessage = 'State PEO policy type is not setup. ' +
                inJrSt;
          Else;
             // Master PEO
             If inEmp# = 1;
                If peoPolicyType = 'M' And inDiv <> 0;
                   If strInCall = 'P' or strInCall = 'L'; //'P'olicy Rating tab or 'L'ocation master
                      //Check Override authority. If true,display popup otherwise errormessage below
                      If CheckSecLevel(StrAppId);
                         //Display Override Window
                         If PopTwoBtn(strmsg:'PEO Master/MCP Policy Override':
                             'Override':'Cancel');
                            errorMessage = 'Override Master/MCP';
                         Else;
                            errorMessage = 'Master Policy cannot be entered ' +
                             'on a state set as a Multi-Coordinated Policy.';
                         Endif;
                      Else;
                         errorMessage = 'Master Policy cannot be entered ' +
                          'on a state set as a Multi-Coordinated Policy.';
                      Endif;
                   Else;
                      errorMessage = 'Master Policy cannot be entered ' +
                       'on a state set as a Multi-Coordinated Policy.';
                   Endif;
                EndIf;
             // MCP PEO
             Else;
                If peoPolicyType = 'S';
                   If strInCall = 'P' or strInCall = 'L'; //'P'olicy Rating tab or 'L'ocation master
                      //Check Override authority. If true,display popup otherwise errormessage below
                      If CheckSecLevel(StrAppId);
                         //Display Override Window
                         If PopTwoBtn(strmsg:'PEO Master/MCP Policy Override':
                             'Override':'Cancel');
                            errorMessage = 'Override Master/MCP';
                         Else;
                            errorMessage = 'Multi-Coordinated Policy cannot ' +
                             'be entered on a state set as a Master Policy.';
                         Endif;
                      Else;
                         errorMessage = 'Multi-Coordinated Policy cannot ' +
                          'be entered on a state set as a Master Policy.';
                      Endif;
                   Else;
                      errorMessage = 'Multi-Coordinated Policy cannot be ' +
                       'entered on a state set as a Master Policy.';
                   Endif;
                EndIf;
             EndIf;
          EndIf;
       EndIf;

       Return errorMessage;

      /End-Free

     pEdtPeoPolType    e
      *====================================================================
      * QuoteStsChgHst - Updates quote status change history
      *====================================================================
     pQuoteStsChgHst...
     p                 b                   export
     dQuoteStsChgHst...
     d                 pi             1n
     d inWAQTPnew                          const likeds(dsWAQTPpr)
     d inWAQTPold                          const likeds(dsWAQTPpr)

     ddsWAQTPnew       ds                  likerec(pqt0p:*all) inz
     ddsWAQTPold       ds                  likerec(pqt0p:*all) inz
     dstrCmd           s            500    varying
      /Free
       Exsr CreateAlias;
       dsWAQTPold = inWAQTPold;
       dsWAQTPnew = inWAQTPnew;
       If dsWAQTPnew <> dsWAQTPold;
          Exsr AllChange;
       Endif;
       If dsWAQTPnew.QTtype <> dsWAQTPold.QTtype;
          Exsr StatusChange;
       Endif;
       Return *on;
       //=======================================================
       Begsr CreateAlias;
        CreateAliasSQL('WAQTP_HST':'WAQTP':'HISTORY':*on);
        CreateAliasSQL('WAQTP_STS':'WAQTP':'STATUS':*on);
       Endsr;
       //=======================================================
       Begsr AllChange;
        If %open(WAQTP);
           Close WAQTP;
        Endif;
        strCmd = 'OVRDBF FILE(WAQTP) TOFILE(WAQTP) MBR(HISTORY) ' +
         'OVRSCOPE(*ACTGRPDFN)';
        RunCmd(strCmd:%len(strCmd));
        Open WAQTP;
        Write pqt0p dsWAQTPnew;
        Close WAQTP;
        strCmd = 'DLTOVR FILE(WAQTP) LVL(*ACTGRPDFN)';
        RunCmd(strCmd:%len(strCmd));
       Endsr;
       //=======================================================
       Begsr StatusChange;
        If %open(WAQTP);
           Close WAQTP;
        Endif;
        strCmd = 'OVRDBF FILE(WAQTP) TOFILE(WAQTP) MBR(STATUS) ' +
         'OVRSCOPE(*ACTGRPDFN)';
        RunCmd(strCmd:%len(strCmd));
        Open WAQTP;
        Write pqt0p dsWAQTPnew;
        Close WAQTP;
        strCmd = 'DLTOVR FILE(WAQTP) LVL(*ACTGRPDFN)';
        RunCmd(strCmd:%len(strCmd));
       Endsr;
      /End-free
     pQuoteStsChgHst...
     p                 e
      *====================================================================
      * RtvDeductible - returns deductible plan (blank, small, large)
      *                 as well as all deductible information.
      *   inCo#  - accepts the current company   (required)
      *   inGrp  - accepts the current group     (required)
      *   inEmp# - accepts the current employer  (required)
      *   inDiv  - accepts the current division  (required)
      *   inFyr  - accepts the current year      (required)
     *   premPct - returns the premium disc pct (optional)
     *   dedAmt  - returns the deduct amt/occur (optional)
     *   dedAgg  - returns the deduct aggregate (optional)
     *   lossSub - returns the loss subject cde (optional)
     *   basisDed- returns the basis deduct cde (optional)
     *   dedNet  - returns the deduct net/gross (optional)
     *   dedVerb - returns the deduct verbiage  (optional)
     *   dedClas - returns the deduct class cde (optional)
     *   dedPct  - returns the deduct percentag (optional) (not used on our system)
      *====================================================================
     pRtvDeductible    b                   export
     dRtvDeductible    pi             1
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const
     D premPct                       12  9 options(*nopass)
     D dedAmt                         9  0 options(*nopass)
     D dedAgg                         9  0 options(*nopass)
     D lossSub                        2  0 options(*nopass)
     D basisDed                       2  0 options(*nopass)
     D dedNet                         1    options(*nopass)
     D dedVerb                        1    options(*nopass)
     D dedClas                        5  0 options(*nopass)
     D dedPct                         2  0 options(*nopass)

     d xxPremPct       s             12  9
     d xxDedAmt        s              9  0
     d xxDedAgg        s              9  0
     d xxLossSub       s              2  0
     d xxBasisDed      s              2  0
     d xxDedPct        s              2  0
     d xxDedClas       s              5  0
     d xxDedNet        s              1
     d xxDedVerb       s              1

     d dedPlanType     s              1    Inz(' ')
     d xxComp          s              1    Inz(' ')
     d xxMed           s              1    Inz(' ')
     d il              s              2  0
     d q2co#           s              3
     d q2fnd           s              3
     d q2emp#          s              9
     d q2div           s              5
     d q2fyr           s              3
     d q2date          s              7
     d xxDate          s              7  0
     d xxKey           s                   like(BSKEY)

     C     elky          klist
     C                   kfld                    inCO#
     C                   kfld                    inFnd
     C                   kfld                    inEmp#
     C                   kfld                    inDiv
     C                   kfld                    inFyr

     C     elky          Chain     pel00

     c                   move      inCO#         q2co#
     c                   move      inFND         q2fnd
     c                   move      inEMP#        q2emp#
     c                   move      inDIV         q2div
     c                   move      inFYR         q2fyr
     c                   move      ELDCEF        q2date
     c                   movel(p)  ELBscd        xxKey
      /Free

       Chain(n) (inCO#:ELRTCD) prs00;

       Chain(n) (inCo#:xxKey) pbs01;

       If BSLPGM = 'L';
          dedPlanType = 'L';
       Else;
          If ELDCDE <> ' ';
             dedPlanType = 'S';
          Endif;
       Endif;

       Clear xxPremPct;
       Clear xxDedAmt;
       Clear xxDedAgg;
       Clear xxDedClas;
       Clear xxDedPct;
       Clear xxDedVerb;
       Clear xxDedNet;
       Clear xxLossSub;
       Clear xxBasisDed;

       //* Small Deductible

       If dedPlanType = 'S';
          Sreffd(q2co#:q2fnd:q2fyr:q2emp#:q2div:q2date);
          xxDate = %int(q2date);
          Setgt (inCo#:inFnd:RSJrst:ELDcde:xxDate) pdd02;
          Readpe (inCo#:inFnd:RSJrst:ELDcde) pdd02;
          Dow not %eof and DDStat <> 'A';
             Readpe (inCo#:inFnd:RSJrst:ELDcde) pdd02;
          Enddo;

          If not %eof;
             xxPremPct = DDPct;
             xxDedAmt  = DDAmpo;
             xxDedAgg  = DDAmag;
             xxDedClas = DDClas;
             xxDedPct  = 0;
             xxDedVerb = DDVerb;
             xxDedNet  = DDType;

             Clear xxComp;
             Clear xxMed;
             il = 1;
             Dow il <= 10;

                If sdc(il) <> ' ';

                   xxKey = sdc(il);
                   Chain(n) (inCo#:xxKey) prt01;
                   If %found and RTType = 'C';
                      xxComp = 'Y';
                   Endif;
                   If %found and RTType = 'M';
                      xxMed = 'Y';
                   Endif;

                Endif;
                il = il + 1;

             Enddo;

             Select;
             When xxComp = 'Y' and xxMed = 'Y';
                xxLossSub = 03;
             When xxComp = 'Y';
                xxLossSub = 02;
             When xxMed = 'Y';
                xxLossSub = 01;
             Other;
                xxLossSub = 00;
             Endsl;

             Select;
             When DDVerb = 'C' and DDAmag <> 0 and DDAmpo <> 0;
                 xxBasisDed = 10;
             When DDVerb <> 'C' and DDAmag <> 0 and DDAmpo <> 0;
                xxBasisDed = 09;
             When DDAmag <> 0 and DDAmpo = 0;
                xxBasisDed = 03;
             When DDVerb <> 'C' and DDAmag = 0 and DDAmpo <> 0;
                xxBasisDed = 02;
             When DDVerb = 'C' and DDAmag = 0 and DDAmpo <> 0;
                xxBasisDed = 01;
             Other;
                xxBasisDed = 00;
             Endsl;

          Endif;

       Endif;

       //* Large Deductible

       If dedPlanType = 'L';
          Chain(n) (inCo#:inFnd:inEmp#:inDiv:inFyr) pld01;

          If %found;
             xxPremPct = LDPct1;
             xxDedAmt  = LDAmp1;
             xxDedAgg  = LDAmg1;
             xxDedClas = LDCls1;
             xxDedPct  = 0;
             xxDedVerb = LDVrb1;
             xxDedNet  = LDTyp1;

             Clear xxComp;
             Clear xxMed;
             il = 1;
             Dow il <= 10;

                If ldc(il) <> ' ';

                   xxKey = ldc(il);
                   Chain(n) (inCo#:xxKey) prt01;
                   If %found and RTType = 'C';
                      xxComp = 'Y';
                   Endif;
                   If %found and RTType = 'M';
                      xxMed = 'Y';
                   Endif;

                Endif;
                il = il + 1;

             Enddo;

             Select;
             When xxComp = 'Y' and xxMed = 'Y';
                xxLossSub = 03;
             When xxComp = 'Y';
                xxLossSub = 02;
             When xxMed = 'Y';
                xxLossSub = 01;
             Other;
                xxLossSub = 00;
             Endsl;

             Select;
             When LDVrb1 = 'C' and LDAmg1 <> 0 and LDAmp1 <> 0;
                 xxBasisDed = 10;
             When LDVrb1 <> 'C' and LDAmg1 <> 0 and LDAmp1 <> 0;
                xxBasisDed = 09;
             When LDAmg1 <> 0 and LDAmp1 = 0;
                xxBasisDed = 03;
             When LDVrb1 <> 'C' and LDAmg1 = 0 and LDAmp1 <> 0;
                xxBasisDed = 02;
             When LDVrb1 = 'C' and LDAmg1 = 0 and LDAmp1 <> 0;
                xxBasisDed = 01;
             Other;
                xxBasisDed = 00;
             Endsl;

          Endif;

       Endif;

       If xxPremPct >= 100;
          xxPremPct = 99.999;
       Endif;
       //*** begin add ***  08/17/23  **************************************
       //*** begin chg ***  08/30/23  **************************************
       //*****.If xxPremPct <= -100;
       //*****   xxPremPct = -99.999;
       //*****Endif;
       If xxPremPct < 0;
          xxPremPct = 0;
       Endif;
       //*** end chg *****  08/30/23  **************************************
       //*** end add *****  08/17/23  **************************************

       If %parms >= 6;
          premPct = xxPremPct;
       Endif;

       If %parms >= 7;
          dedAmt = xxDedAmt;
       Endif;

       If %parms >= 8;
          dedAgg = xxDedAgg;
       Endif;

       If %parms >= 9;
          lossSub = xxLossSub;
       Endif;

       If %parms >= 10;
          basisDed = xxBasisDed;
       Endif;

       If %parms >= 11;
          dedNet = xxDedNet;
       Endif;

       If %parms >= 12;
          dedVerb = xxDedVerb;
       Endif;

       If %parms >= 13;
          dedClas = xxDedClas;
       Endif;

       If %parms >= 14;
          dedPct = xxDedPct;
       Endif;

       Return dedPlanType;

      /End-Free

     pRtvDeductible    e
      *====================================================================
      * CollateralSR   - returns amount of collateral based on amount passed
      *                  into program.
      *   inCo#  - accepts the current company      (required)
      *   inGrp  - accepts the current group        (required)
      *   inEmp# - accepts the current employer     (required)
      *   inFyr  - accepts the current policy year  (required)
     *   inMode - accepts the mode/type of call    (required)
      *          - 'E' S/R Payroll Entry
      *          - 'I' S/R Payroll Import
      *          - 'J' S/R Payroll Import w/Auto Transfer
     *   inAmt  - accepts the S/R billing amt      (required)
     *   inDate - accepts the date for setup.      (optional)
     *   inBBatch - accepts/returns billing batch  (optional)
     *   inPBatch - accepts/returns payment batch  (optional)
     *   inBBAR - accepts/returns billing A/R code (optional)
     *   inPBAR - accepts/returns payment A/R code (optional)
      *====================================================================
     pCollateralSR     b                   export
     dCollateralSR     pi            11  2
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d inMode                         1    const
     d inAmt                         11  2 const
     d inDate                         7  0 const options(*nopass)
     d inBBatch                       9  0 options(*nopass)
     d inPBatch                       9  0 options(*nopass)
     d inBAR                          2    options(*nopass)
     d inPAR                          2    options(*nopass)

     d intColPrmAdj    s             11  0
     d xxColtAmt       s             11  2 inz(0)
     d xxBTotal        s             11  2 inz(0)
     d xxPTotal        s             11  2 inz(0)
     d xxKey           s             10
     d xxEmp#          s              9  0
     d xxDiv           s              5  0
     d xxRound         s              9  0 inz(0)
     d xxDate          s              7  0 inz(0)
     d xxCurDate       s              7  0 inz(0)
     d xxBAR           s              2    inz('  ')
     d xxPAR           s              2    inz('  ')
     d xxBsycd         s              2    inz('  ')
     d xxPsycd         s              2    inz('  ')
     d xxType          s              1    inz(' ')
     d xxBBatch        s              9  0 inz(0)
     d xxPBatch        s              9  0 inz(0)
     d xxVal1          s              9  0 inz(0)
     d xxVal2          s              9  0 inz(0)

      /Free
       xxBAR = '  ';
       xxBTotal = 0;
       xxBsycd = '  ';
       xxPAR = '  ';
       xxPTotal = 0;
       xxPsycd = '  ';
       xxColtAmt = 0;

       //* Use current system date unless passed in.
       If %parms >= 7;
          xxDate = inDate;
       Else;
          xxDate = DateTo7(SystemDate);
       Endif;

       xxCurDate = DateTo7(SystemDate);

       Chain(n) (inCO#:inFnd:inFyr) pf100;

       //* Use billing A/R code if passed in.
       If %parms >= 10;
          xxBAR = inBAR;
       Endif;

       If xxBar = '  ';
          xxBar = F1ColB;
          If %parms >= 10;
             inBAR = xxBAR;
          Endif;
       Endif;

       //* Use payment A/R code if passed in.
       If %parms >= 11;
          xxPAR = inPAR;
       Endif;

       If xxPar = '  ';
          xxPar = F1ColP;
          If %parms >= 11;
             inPAR = xxPAR;
          Endif;
       Endif;

       //* Exit if no billing and payment A/R codes.
       If xxBar = '  ' or xxPar = '  ';
          Return 0;
       Endif;

       //* Use PEO itself, or state reporting division if not a PEO group.
       Chain(n) (inCO#:inFnd) pfn00;
       If FNPEOF <> ' ';
          xxEmp# = 1;
          xxDiv = 0;
       Else;
          xxEmp# = inEmp#;
          xxDiv = GetStrptDv(inCo#:inFnd:inEmp#);
       Endif;

       //* Exit if no setup, or setup is expired, or no percentage.
       Setll (inCo#:inFnd:xxEmp#:xxDate) pco01;
       Reade(n) (inCo#:inFnd:xxEmp#) pco01;
       If %eof or (not %eof and COExdt <> 0 and COExdt <= xxDate)
        or (not %eof and COPerc = 0);
          Return 0;
       Endif;

       Select;
       When inAmt <> 0;
          intColPrmAdj = inAmt;
       Other;
          intColPrmAdj = GetColManPrmAdj(inCo#:inFnd:xxEmp#:inFyr:xxDate);
       EndSl;

       Eval(h) xxRound = intColPrmAdj * COPerc / 100;

       //* Exit if calculated collateral is zero.
       If xxRound = 0;
          Return 0;
       Endif;

       //* Store incoming billing batch if passed in.
       If %parms >= 9;
          xxBBatch = inBBatch;
       Else;
          xxBBatch = 0;
       Endif;

       //* Store incoming payment batch if passed in.
       If %parms >= 9;
          xxPBatch = inPBatch;
       Else;
          xxPBatch = 0;
       Endif;

       //* Find ongoing s/r payroll entry batch when none passed in.
       If xxBBatch = 0 and inMode = 'E';
          xxType = 'B';
          Exsr FindBatch;
       Endif;

       //* Find ongoing s/r payroll entry batch when none passed in.
       If xxPBatch = 0 and inMode = 'E';
          xxType = 'P';
          Exsr FindBatch;
       Endif;

       //* Set the billing batch
       If xxBBatch = 0;
          xxType = 'B';
          Exsr ReserveBatch;
       Endif;

       //* Set the payment batch
       If xxPBatch = 0;
          xxType = 'P';
          Exsr ReserveBatch;
       Endif;

       Exsr BatchTotals;

       //* Store collateral amount and write to batch
       xxColtAmt = xxRound;

       Exsr WriteBatches;

       Exsr CleanBatches;

       //* Return batch #s
       If %parms >= 9;
          inBBatch = xxBBatch;
          inPBatch = xxPBatch;
       Endif;

       //* Return amount of collateral billed
       Return xxColtAmt;

      *****************************************************************
      *
      *  FIND ONLINE BATCH # AND STORE TOTAL BATCH AMOUNT.
      *
      *****************************************************************
       Begsr FindBatch;

          SetGt (inCo#:' ') pea01b;
          Readpe(n) (inCo#:' ') pea01b;
          Dow not %eof;

             If EACollFlg <> 'Y' or EAIdt <> xxCurDate or ((EAarcd <> xxBar
              and xxType = 'B') or (EAArcd <> xxPar and xxType = 'P'));
                Readpe(n) (inCo#:' ') pea01b;
             Else;
                Leave;
             Endif;
          Enddo;

         //* Collateral batch has been found.
          If not %eof and EAtfap <> 'Y' and EAtffl <> 'Y';
             If xxType = 'B';
                xxBBatch = EABat#;
                xxBsycd = EAsycd;
             Else;
                xxPBatch = EABat#;
                xxPsycd = EAsycd;
             Endif;

          Endif;

       Endsr;
      *****************************************************************
      *
      *  Batch Totals
      *
      *****************************************************************
       Begsr BatchTotals;

         //* Billing total
          Setll (inCo#:xxBBatch) pea04b;
          Reade(n) (inCo#:xxBBatch) pea04b;
          Dow not %eof;
             xxBTotal = xxBTotal + EAAmt;
             Reade(n) (inCo#:xxBBatch) pea04b;
          Enddo;

         //* Payment total
          Setll (inCo#:xxPBatch) pea04b;
          Reade(n) (inCo#:xxPBatch) pea04b;
          Dow not %eof;
             xxPTotal = xxPTotal + EAAmt;
             Reade(n) (inCo#:xxPBatch) pea04b;
          Enddo;

       Endsr;
      *****************************************************************
      *
      *  RESERVE NEXT AVAILABLE BATCH NUMBER FOR A/R BATCH.
      *
      *****************************************************************
       Begsr ReserveBatch;

         //* FIND A VALID SYSTEM TABLE CODE

          Setll inCo# psy01;
          Dou not %eof;
             Reade inCo# psy01;
             If SYType = 'Y' and SYStat <> 'I';
                If xxType = 'B';
                   xxBSycd = SYKey;
                Else;
                   xxPSycd = SYKey;
                Endif;
                Leave;
             Endif;
          Enddo;

         //* Find next available batch #
          xxVal1 = 0;
          xxVal2 = 0;

          Setgt (inCo#:' ') pea01b;
          Readp(n) pea01b;
          If not %eof and EACo# = inCo#;
             xxVal1 = EABat# + 1;
          Endif;

          Setgt (inCo#:'T') pea01b;
          Readp(n) pea01b;
          If not %eof and EACo# = inCo#;
             xxVal2 = EABat# + 1;
          Endif;

          If xxVal1 > xxVal2;
             If xxType = 'B';
                xxBBatch = xxVal1;
             Else;
                xxPBatch = xxVal1;
             Endif;
          Else;
             If xxType = 'B';
                xxBBatch = xxVal2;
             Else;
                xxPBatch = xxVal2;
             Endif;
          Endif;

          If xxType = 'B' and xxBBatch = 0;
             xxBBatch = 1;
          Endif;

          If xxType = 'P' and xxPBatch = 0;
             xxPBatch = 1;
          Endif;

         //* Write an A/R batch dummy record, to hold the bat#.

         EACo# = inCo#;
         If xxType = 'B';
            EABat# = xxBBatch;
         Else;
            EABat# = xxPBatch;
         Endif;
         EABata = 0;
         EAFnd = *hival;
         EAFyr = *hival;
         EAEmp# = *hival;
         EADiv = *hival;
         EAArcd = '  ';
         EACtyp = ' ';
         EADesc = ' ';
         EAPodt = 0;
         EAChk# = 0;
         EACkam = 0;
         EAAmt = 0;
         EASycd = '  ';
         EATfap = 'N';
         EATffl = 'N';
         EAAutr = 'N';
         EATfdt = 0;
         EAStat = ' ';
         EAUser = Q1User;
         EAPgm = Q1Pgm;
         EATime = 1;
         EADt = xxCurDate;
         EAIDt = xxCurDate;
         EAIUsr = Q1User;
         EAIPgm = Q1Pgm;
         EAInv# = 0;
         EATrdt = 0;
         EALate = ' ';
         Write pea04b;

       Endsr;
      *****************************************************************
      *
      * WRITE BATCH RECORD.
      *
      *****************************************************************
       Begsr WriteBatches;

          Clear pea04b;

         //* Write Billing batch record
          EACo# = inCo#;
          EAFnd = inFnd;
          EAFyr = inFyr;
          EAEmp# = xxEmp#;
          EADiv = xxDiv;
          EARDiv = xxDiv;
          EABat# = xxBBatch;
          EAChk# = 0;
          EABata = 0;
          EASycd = xxBsycd;
          EATfap = 'N';
          EATffl = 'N';
          EAAutr = 'N';
          //* Flag batches as auto-transfer if incoming mode requests
          If inMode = 'J';
             EATfap = 'Y';
             EATffl = 'Y';
             EAAutr = 'Y';
          Endif;
          EATfdt = 0;
          EATfam = 0;
          EACollFlg = 'Y';
          EAColEfdt = COEfdt;
          EAColExdt = COExdt;
          EAColPerc = COPerc;
          EAArcd = xxBar;
          xxKey = xxBar;
          Chain(n) (inCo#:xxKey) ptt01;
          EADesc = TTDesc;
          EACtyp = TTCtyp;
          EAAmt = xxColtAmt;
          EASrManPr = intColPrmAdj;
          EASrRptDt = xxDate;
          EAachf = 'L';
          EACkam = xxColtAmt;
          EAPodt = xxCurDate;
          EAInv# = 0;
          EALate = ' ';
          EAStat = ' ';
          EAUser = Q1User;
          EAPgm = Q1Pgm;
          EATime = Timeto6(SystemTime());
          EADt = xxCurDate;
          EAIdt = xxCurDate;
          EAIUsr = Q1User;
          EAIPgm = Q1Pgm;
          Write pea04b;

          xxBTotal = xxBTotal + EAAmt;

         //* Write Payment batch record
          EABat# = xxPBatch;
          EASycd = xxPsycd;
          EAArcd = xxPar;
          xxKey = xxPar;
          Chain(n) (inCo#:xxKey) ptt01;
          EADesc = TTDesc;
          EACtyp = TTCtyp;
          EATime = EATime + 1;
          Write pea04b;

          xxPTotal = xxPTotal + EAAmt;

       Endsr;
      *****************************************************************
      *
      * UPDATE BATCH TOTAL AND REMOVE DUMMY PLACEHOLDER BATCH.
      *
      *****************************************************************
       Begsr CleanBatches;

         //* Update all batch records with new total.
          Setll (inCo#:xxBBatch) pea04b;
          Reade (inCo#:xxBBatch) pea04b;
          Dow not %eof;
             EABata = xxBTotal;
             Update pea04b;
             Reade (inCo#:xxBBatch) pea04b;
          Enddo;

          Setll (inCo#:xxPBatch) pea04b;
          Reade (inCo#:xxPBatch) pea04b;
          Dow not %eof;
             EABata = xxPTotal;
             Update pea04b;
             Reade (inCo#:xxPBatch) pea04b;
          Enddo;

         //* Delete the Billing A/R batch dummy record.
         Chain (inCo#:xxBBatch:*hival:*hival:*hival:*hival) pea04b;
         If %found;
            Delete pea04b;
         Endif;

         //* Delete the Payment A/R batch dummy record.
         Chain (inCo#:xxPBatch:*hival:*hival:*hival:*hival) pea04b;
         If %found;
            Delete pea04b;
         Endif;

       Endsr;
      *****************************************************************
      /End-Free

     pCollateralSR     e

      //***  Begin Add  ***  12/14/20  *******************
      *====================================================================
      * CxJSONData returns JSON string of cx data
      *  inCo#  - accepts the current company   (required)
      *  inFnd  - accepts the current group     (required)
      *  inEmp# - accepts the current employer  (required)
      *  inDiv  - accepts the current division  (required)
      *====================================================================
     pCxJSONData       b                   Export
     dCxJSONData       pi              a   Len(1000) Varying
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const

     ddt7Cx            s              7  0 inz
     dstrCxReason      s             65a   inz
     djson             s               a   len(1000) varying inz
      /Free
       dt7Cx = GetCxDate(inCo#:inFnd:inEmp#:inDiv:strCxReason);
       json += '"cancellationInfo":{';
        json += BuildJsonNode('date':%char(dt7Cx):'date') + ',';
        json += BuildJsonNode('reason':%trim(strCxReason)) + ',';
        json = %scanrpl(',':'':json:%len(json)-1);
       json += '}';
       Return json;
      /End-Free
     pCxJSONData       e
      //***  End   Add  ***  12/14/20  *******************


      *====================================================================
      * GetColManPrmAdj returns the difference between manual premium in WASRP and
      *  the amount of manual premium stored in A/R for which collateral has been written.
      *  inCo#  - accepts the current company   (required)
      *  inGrp  - accepts the current group     (required)
      *  inEmp# - accepts the current employer  (required)
      *  inEmp# - accepts the current division  (required)
      *  inFyr  - accepts the current pol year  (required)
      *  inDate - accepts the report date       (required)
      *====================================================================
     pGetColManPrmAdj  b                   Export
     dGetColManPrmAdj  pi            11  0
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inFyr                          3  0 const
     d inDate                         7  0 const

     d                 ds
     d inRptDate                      7  0
     d inYyyMm                        5  0 overlay(inRptDate)

     d prvManPrm1      s             11  0
     d prvManPrm2      s             11  0
     d curManPrm       s             11  0

      /Free
       inRptDate = inDate;
       Chain (inCo#:inFnd:inFyr) pf100;

       Exec SQL Select Sum(SRManPrm) Into :curManPrm from WASRP
        Where SRco# = :inCo# and SRfnd = :inFnd and  SRemp# = :inEmp#
          and SRfyr = :inFyr and (SRtodt = :inDate or SRrpmy = :inYyyMm);

       Exec SQL Select Sum(EASrManPr) Into :prvManPrm1 from WMEAP
        Where EAco# = :inCo# and EAfnd = :inFnd and  EAemp# = :inEmp#
          and EAfyr = :inFyr and EAarcd = :F1colb and EASrRptDt = :inDate;

       Exec SQL Select Sum(EASrManPr) Into :prvManPrm2 from WDEAP
        Where EAco# = :inCo# and EAfnd = :inFnd and  EAemp# = :inEmp#
          and EAfyr = :inFyr and EAarcd = :F1colb and EASrRptDt = :inDate
          and EAstat = ' ';

       Return curManPrm - (prvManPrm1 + prvManPrm2);
      /End-Free

     pGetColManPrmAdj  e

     d*** begin add ***  03/16/20  *******************************************
      *====================================================================
      * OvrRenewalCx returns *On if Cancellation can be overriden to allow
      *  certain processes to process
      *====================================================================
     pOvrRenewalCx     b                   Export
     DOvrRenewalCx     pi              n
     d inMode                         3
     d inCo#                               Like(EMco#)
     d inFnd                               Like(EMfnd)
     d inEmp#                              Like(EMemp#)
     d inDiv                               Like(EMdiv)
     d inFyr                               Like(ELfyr)
     d inCxDate                            Like(EMdt)
     d inTrDate                            Like(EMdt) Options(*NoPass)
     d inTrTime                            Like(EMtime) Options(*NoPass)

     d chrCxCode       s              2a   Inz
     d chrNcciCode     s              2a   Inz
     d chrOvrId        s              1a   Inz
     d intCxDate       s                   Like(EMcad1)
     d intStRptDiv     s                   Like(EMdiv)
     d intTrDate       s              7s 0
     d intTrTime       s              6s 0
     d intStRptDivExpDate...
     d                 s              7s 0 Inz

      /Free
        If %Parms() >= 9;
           intTrDate = inTrDate;
           intTrTime = inTrTime;
        Else;
           intTrDate = %Dec(%Date():*cymd);
           intTrTime = %Dec(%Time():*hms);
        Endif;
       // Check Cx History to see if this was a midterm
       // deletion of divison. If so, return *off.
       // The CEU has already accounted for this.
        If inMode <> 'CEU';
           Setgt (inCo#:inFnd:inEmp#:inDiv:inFyr:
            intTrDate:intTrTime) pxh15;
           Readpe (inCo#:inFnd:inEmp#:inDiv:inFyr) pxh15;
           If XHpgm = 'WTPOLDLTDV';
              Return *Off;
           Endif;
        Endif;
       // Get latest policy expiration date from Premium
       // History for state reporting division written
       // by estimated/revised billing.
        intStRptDiv = GetStRptDv(inCo#:inFnd:inEmp#);
        //*** Beg Chg ***** 11/06/24 ******************************************
        //*** Setgt (inCo#:inFnd:inEmp#:inDiv:inFyr:
        //***  intTrDate:intTrTime) peh12;
        //*** Dou %Eof;
        //***    Readpe (inCo#:inFnd:inEmp#:inDiv:inFyr) peh12;
        Setgt (inCo#:inFnd:inEmp#:intStRptDiv:inFyr:
         intTrDate:intTrTime) peh12;
        Dou %Eof;
           Readpe (inCo#:inFnd:inEmp#:intStRptDiv:inFyr) peh12;
        //*** End Chg ***** 11/06/24 ******************************************
           If %Eof;
              Leave;
           Endif;
           If EHfaf = 'A';
              Iter;
           Endif;
           intStRptDivExpDate = EHptdt;
           Leave;
        Enddo;
       // In case there's no premium history records for the
       // state reporting division for the year, look in
       // Policy Detail for an expiration date.
        If intStRptDivExpDate = 0;
           Chain (inCo#:inFnd:inFyr:inEmp#:intStRptDiv) pel01;
           If %Found;
              intStRptDivExpDate = ELptdt;
           Endif;
        Endif;
       // The current state reporting division might not exist
       // in a prior year.
       // Look for oldest Premium History not flagged as
       // rewritten by a policy date change for the year to
       // get a division and expiration date.
       // It's most likely the original state reporting division.
        If intStRptDivExpDate = 0;
           Setll (inCo#:inFnd:inFyr:inEmp#) peh11;
           Dou %Eof;
              Reade (inCo#:inFnd:inFyr:inEmp#) peh11;
              If %Eof;
                 Leave;
              EndIf;
              If EHfaf = 'A';
                 Iter;
              Endif;
              If EHrwfl = 'Y';
                 Iter;
              Endif;
              intStRptDiv = EHdiv;
              intStRptDivExpDate = EHptdt;
              Leave;
           Enddo;
        Endif;
       // If still no expiration date, look in Policy Detail
       // for oldest record.
        If intStRptDivExpDate = 0;
           Chain (inCo#:inFnd:inFyr:inEmp#) pel01;
           If Not %Found;
              Return *Off;
           Endif;
           intStRptDiv = ELdiv;
           intStRptDivExpDate = ELptdt;
        Endif;
       // Get the latest Cancellation History record for
       // the state reporting division as the transaction
       // date and time.
        Setgt (inCo#:inFnd:inEmp#:intStRptDiv:inFyr:
         intTrDate:intTrTime) pxh15;
        Readpe (inCo#:inFnd:inEmp#:intStRptDiv:inFyr) pxh15;
        If %Eof;
           Return *Off; // Can't find enough information to override.
        EndIf;
       // Return *Off if inCxDate <> state reporting division's exp date
        If inCxDate <> intStRptDivExpDate;
           Return *Off;
        Endif;
       // Return *Off if inCxDate doesn't match the Cx Date from the
       // state reporting division. It should match because midterm
       // deletion of division was checked above, but in case it
       // doesn't...
        If inCxDate <> XHcxdt;
           Return *Off;
        EndIf;
       // Finally, get the codes and return *On if override is valid or
       // *Off if not.
        GetCxRsn(inCo#:inFnd:inEmp#:inDiv:inCxDate:chrCxCode:
         chrNcciCode:chrOvrID:intTrDate);
       // If Renewal Cx Code or Renewal Override...
        If chrNcciCode = '00' Or chrOvrID = '3';
           Return *On;
        Else;
           Return *Off;
        Endif;
      /End-Free

     pOvrRenewalCx     e
      *** End  add ***  03/16/20  *******************************************

      *====================================================================
      * PolicyJSONData returns JSON string of general policy data.
      *  inCo#  - accepts the current company   (required)
      *  inFnd  - accepts the current group     (required)
      *  inEmp# - accepts the current employer  (required)
      *  inDiv  - accepts the current division  (required)
      *  inFyr  - accepts the current pol year  (required)
      *====================================================================
     pPolicyJSONData   b                   Export
     dPolicyJSONData   pi              a   Len(5000) Varying
     d inCo#                          3  0 const
     d inFnd                          3  0 const
     d inEmp#                         9  0 const
     d inDiv                          5  0 const
     d inFyr                          3  0 const

     dsysDt7           s              7  0
     ddecAgingMax      s             10i 0 inz(180)
     ddecCount         s             10i 0 inz
     ddtCancelled      s               d
     ddt8Cancelled     s              8  0 inz
     dstrAudited       s              5    inz('false')
     dstrMultiDiv      s              5    inz('false')
     D***  Begin Chg  ***  12/09/21  ****************
     d*xxFullName      s            100
     d*xxPolicy#       s             25
     d*xxStatus        s             25
     d xxFullName      s            100    varying
     d xxPolicy#       s             25    varying
     d xxStatus        s             25    varying
     D***  End   Chg  ***  12/09/21  ****************
     d json            s               a   len(5000) varying inz
      /Free
         sysDt7 = DateTo7(SystemDate());

         dt8Cancelled = DtMDYY(GetCxDate(inCo#:inFnd:inEmp#:inDiv));
         dtCancelled = CvtToDate(dt8Cancelled);

         Chain (inCo#:inFnd:inEmp#:inDiv:inFyr) pel00;

         If ELfaf = 'A';
            strAudited = 'true';
         Endif;

         xxFullName = ' ';
         Setll (inCo#:inFnd:inEmp#) pem00;
         Reade (inCo#:inFnd:inEmp#) pem00;
         Dow not %eof;
            If EMStrp = 'Y';
               If EMFdnm <> ' ';
                  xxFullName = EMFdnm + EMFdn2;
               Else;
                  xxFullName = EMName + EMNam2;
               Endif;
               Leave;
            Endif;
            Reade (inCo#:inFnd:inEmp#) pem00;
         Enddo;


         Chain (inCo#:inFnd:inEmp#:inDiv) pem00;
         If %found;
            If xxFullName = ' ';
               If EMFdnm <> ' ';
                  xxFullName = EMFdnm + EMFdn2;
               Else;
                  xxFullName = EMName + EMNam2;
               Endif;
            Endif;

            xxPolicy# = ' ';
          //***  Begin Chg  ***  12/09/21  ****************
          //xxPolicy# = GetCarrPol(inCO#:inFND:inEMP#:inDiv:inFyr);
            xxPolicy# = %trim(GetCarrPol(inCO#:inFND:inEMP#:inDiv:inFyr));
          //***  End   Chg  ***  12/09/21  ****************
            If xxPolicy# = ' ';
               xxPolicy# = %editc(inFnd:'X') + '-' + %editc(inEmp#:'X') +
                '-' + %editc(inFyr:'X');
            Endif;

            Select;
            When EMCad1 <> 0;
               xxStatus = 'Cancelled';
            When ELptDt <= sysDt7;
               xxStatus = 'Expired';
            Other;
               xxStatus = 'Active';
            Endsl;

         Endif;
       //***  Begin Add  ***  12/09/21  ****************
         xxFullName = %trim(xxFullName);
       //***  End   Add  ***  12/09/21  ****************

         Exec sql
          select count(*) into :decCount
           from WDELP
           where ELco# = :inCo# and ELfnd = :inFnd
            and ELemp# = :inEmp# and ELfyr = :inFyr;
         If decCount > 1;
            strMultiDiv = 'true';
         Endif;

         json += '"policyInfo":{' +
          BuildJsonNode('company':%editc(EMco#:'X')) + ',' +
          BuildJsonNode('insuredName':xxFullName) + ',' +
          BuildJsonNode('policyNumber':xxPolicy#) + ',' +
          BuildJsonNode('year':%editc(inFyr:'X')) + ',' +
          BuildJsonNode('group':%editc(inFnd:'X')) + ',' +
          BuildJsonNode('employer':%editc(inEmp#:'X')) + ',' +
          '"divisionInfo":{' +
           BuildJsonNode('number':%editc(inDiv:'X')) + ',' +
         //***  Begin Chg  ***  12/09/21  **************** +
         //BuildJsonNode('name':EMName + EMNam2) + ',' +
           BuildJsonNode('name':%trim(EMName + EMNam2)) + ',' +
         //***  End   Chg  ***  12/09/21  **************** +
           BuildJsonNode('display':%editc(inDiv:'X')+' - '+
         //***  Begin Chg  ***  12/09/21  **************** +
         // EMName + EMNam2) + '},' +
            %trim(EMName + EMNam2)) + '},' +
         //***  End   Chg  ***  12/09/21  **************** +
          '"address":{' +
           BuildJsonAddress(EMpAd1:EMpAd2:EMpCty:EMpSt:%char(EMpZip)
            :'physical') + ',' +
           BuildJsonAddress(EMmAd1:EMmAd2:EMmCty:EMmSt:%char(EMmZip)
            :'mailing') + '},' +
          BuildJsonNode('policyPeriod':
           AlphaUsa(CvtToDate(ELPFDT))+' - '+AlphaUsa(CvtToDate(ELPTDT))) +
           ',' +
          BuildJsonNode('multipleDivisions':strMultiDiv:'boolean') + ',' +
          '"dates":{' +
           BuildJsonNode('effective':%char(ELpfDt):'date') + ',' +
           BuildJsonNode('expiration':%char(ELptDt):'date') + ',' +
           BuildJsonNode('cancel':%char(dt8Cancelled):'date') + '},' +
          BuildJsonCode(%editc(ELbsCd:'X'):BillSchdDs(ELco#:%editc(ELbsCd:'X'))
           :'billingPlan') + ', ' +
          BuildJsonNode('policyStatus':xxStatus) + ',' +
          BuildJsonNode('audited':strAudited:'boolean') + ',' +
          '"aging":{' +
           BuildJsonNode('cancelled'
            :AgingDays(dtCancelled:-decAgingMax)) + ',' +
           BuildJsonNode('cancelling'
            :AgingDays(dtCancelled:decAgingMax)) + ',' +
           BuildJsonNode('expired'
            :AgingDays(CvtToDate(ELptDt):-decAgingMax)) + ','+
           BuildJsonNode('expiring'
            :AgingDays(CvtToDate(ELptDt):decAgingMax)) +
           '}' +
         ' } ';

         Return json;

      /End-Free
     pPolicyJSONData   e

      *** Beg Add *****  07/07/23  *******************************************
     P*====================================================================
      * GetEmpBalToMin - Calculate and Return the Balance to Minimum Premium
      *  co#           - accepts the company # to search over
      *  group         - accepts the group to search over
      *  emp#          - accepts the employer to search over
      *  fyr           - accepts the emplr year to search over
      *  audFlg        - 'C' pulls premium for current status
      *                  'E' pulls estimated premium
      *                  'A' pulls audited premium
     P*====================================================================
     pGetEmpBalToMin   b                   export
     dGetEmpBalToMin   pi             9  2
     d co#                            3  0 const
     d fnd                            3  0 const
     d emp#                           9  0 const
     d fyr                            3  0 const
     d audFlg                         1    const

     d retBalToMin     s              9  2 inz
     D xxminf          s              1    inz
     D xxminp          s             11  0 inz
     D xxwk10          s             11  0 inz
     D xxtmpr          s             11  2 inz
     D xxempr          s             11  2 inz

     D tas03           s             11  2 inz
     D tas11           s             11  2 inz
     D tas20           s             11  2 inz
     D tms02           s             11  2 inz
     D tms06           s             11  2 inz
     D tms07           s             11  2 inz
     D tms09           s             11  2 inz
     D tms14           s             11  2 inz
     D tms15           s             11  2 inz
     D tms16           s             11  2 inz
     D tms18           s             11  2 inz
     D tms19           s             11  2 inz                                  50
     D tms44           s             11  2 inz                                  HCN credit pre-min
     D xsexpa          s             11  2 inz
     D tasvlc          s             11  0 inz

      /Free

       retBalToMin = 0;
       xxempr = 0;
       xxminf = ' ';

       Setll (co#:fnd:emp#:fyr) pel08;
       Reade (co#:fnd:emp#:fyr) pel08;
       Dow not(%eof);
          If audFlg = 'A' and ELamnp = 'Y';
             xxminf = 'Y';
             tas11 = tas11 + ELanmp;               // Audited Normal Premium
          Endif;
          If audFlg <> 'A' and ELemnp = 'Y';
             xxminf = 'Y';
             tas11 = tas11 + ELenmp;               // Estimated Normal Premium
          Endif;

          If audFlg = 'A';
             tas03 = tas03 + ELain1;               // Audited Incr Limits
             tas20 = tas20 + ELain2;               // Audited 2nd Incr Limits
          Else;
             tas03 = tas03 + ELein1;               // Estimated Incr Limits
             tas20 = tas20 + ELein2;               // Estimated 2nd Incr Limits
          Endif;

          tasvlc = tasvlc + ELvlca;                // Voluntary Labor Charge

          Chain (co#:fnd:fyr:emp#:ELdiv) pmp01;
          If %found(wdmpl);
             If audFlg = 'A';
                tms14 = tms14 + mpadct;
                xsexpa = xsexpa + mpaxpa;
                tms15  = tms15 + mpadcm;
                tms16  = tms16 + mpadcs;
                tms02  = tms02 + mpadc1;
                tms06  = tms06 + mpadc2;
                tms07  = tms07 + mpadc3;
                tms18  = tms18 + mpadv2;
                tms19  = tms19 + mpadc;
                tms44  = tms44 + mpahcn;
                If mpprtd <> 0;
                   tms09 = mpprtd;
                Endif;
             Else;
                tms14 = tms14 + mpedct;
                xsexpa = xsexpa + mpexpa;
                tms15  = tms15 + mpedcm;
                tms16  = tms16 + mpedcs;
                tms02  = tms02 + mpedc1;
                tms06  = tms06 + mpedc2;
                tms07  = tms07 + mpedc3;
                tms18  = tms18 + mpedv2;
                tms19  = tms19 + mpedc;
                tms44  = tms44 + mpehcn;
                If mpprtd <> 0;
                   tms09 = mpprtd;
                Endif;
             Endif;
          Endif;


          Reade (co#:fnd:emp#:fyr) pel08;
       Enddo;


       Setll (co#:fnd:fyr:emp#) pep01;
       Reade (co#:fnd:fyr:emp#) pep01;
       Dow not(%eof);
          If audFlg = 'A';
             If ELpror <> 1 and ELpror <> 0;
                eval(h)   xxwk10 = ELpror * EPampr;
                xxminp = xxminp + xxwk10;
             Endif;
             xxtmpr = xxtmpr + EPampr;
          Else;
             If ELpror <> 1 and ELpror <> 0;
                eval(h)   xxwk10 = ELpror * EPempr;
                xxminp = xxminp + xxwk10;
             Endif;
             xxtmpr = xxtmpr + EPempr;
          Endif;
          Reade (co#:fnd:fyr:emp#) pep01;
       Enddo;


       // Read Admin/Dart Control Definition File
       Chain (co#:fnd) pex0m;

       If tas11 <> 0 and xxminf = 'Y';
          If ELpror <> 1 and ELpror <> 0;
             xxempr = tas11;
             xxempr = xxempr - xxminp;
             If EXbexp <> 'Y';
                xxempr = xxempr - tms09;
             Endif;
             If ELCofl = 'Y';
                eval(h) xxwk10 = ELpror * tas03;
                xxempr = xxempr - xxwk10;
                eval(h) xxwk10 = ELpror * tas20;
                xxempr = xxempr - xxwk10;
             Else;
                xxempr = xxempr - tas03;
                xxempr = xxempr - tas20;
             Endif;
             eval(h) xxwk10 = ELpror * tms02;
             xxempr = xxempr + xxwk10;
             eval(h) xxwk10 = ELpror * tms06;
             xxempr = xxempr + xxwk10;
             eval(h) xxwk10 = ELpror * tms18;
             xxempr = xxempr + xxwk10;
             eval(h) xxwk10 = ELpror * tms14;
             xxempr = xxempr + xxwk10;
             eval(h) xxwk10 = ELpror * tms44;
             xxempr = xxempr + xxwk10;
             eval(h) xxwk10 = ELpror * tms15;
             xxempr = xxempr + xxwk10;
             eval(h) xxwk10 = ELpror * tms16;
             xxempr = xxempr + xxwk10;
             eval(h) xxwk10 = ELpror * tms19;
             xxempr = xxempr - xxwk10;
             eval(h) xxwk10 = ELpror * xsexpa;
             xxempr = xxempr - xxwk10;
             eval(h) xxwk10 = ELpror * tms07;
             xxempr = xxempr + xxwk10;
          Else;
             xxempr = tas11 - xxtmpr;
             If exbexp <> 'Y';
                xxempr = xxempr - tms09;
             Endif;
             xxempr = xxempr + tms02;
             xxempr = xxempr + tms06;
             xxempr = xxempr + tms18;
             xxempr = xxempr + tms14;
             xxempr = xxempr + tms44;
             xxempr = xxempr + tms15;
             xxempr = xxempr + tms16;
             xxempr = xxempr - tms19;
             xxempr = xxempr - tas03;
             xxempr = xxempr - tas20;
             xxempr = xxempr - xsexpa;
             xxempr = xxempr + tms07;
             xxempr = xxempr - tasvlc;
          Endif;
       Endif;

       retBalToMin = xxempr;

       Return retBalToMin;

      /End-Free

     pGetEmpBalToMin   e
      *** End Add *****  07/07/23  *******************************************
      *** Begin Add *** 09/14/23 *******************************************


     P*====================================================================
     P* GetEmpEnv - Get the Environment of the Policy
     P*
     P*  GetEmpEnv  - returns Environment
     P*   co#       - passed in for validation  (required)
     P*   group     - passed in for validation  (required)
     P*   emp#      - passed in for validation  (required)
     P*   div       - passed in for validation  (required)
     P*====================================================================
     PGetEmpEnv        b                   export
     DGetEmpEnv        PI             1
     D co#                                 like(EMco#)  Const
     D group                               like(EMfnd)  Const
     D emp#                                like(EMemp#) Const
     D div                                 like(EMdiv)  Const

     D chrEmpEnv       s              1
     D strQteSql       s            200

      /Free

       strQteSql = 'Select ''Q'' From ' + RtvQuoteLib('*QTE') + '/WMEMP ' +
                   'Where EMco#=' + %EditC(co#:'X') + ' And ' +
                   'EMfnd=' + %EditC(group:'X') + ' And ' +
                   'EMemp#=' + %EditC(emp#:'X') + ' And ' +
                   'EMdiv=' + %EditC(div:'X');

        Exec SQL prepare sqlQuote from :strQteSql;
        Exec SQL declare cursorQuote cursor for sqlQuote;
        Exec SQL open cursorQuote;

        chrEmpEnv = ' ';
        Exec SQL fetch next from cursorQuote into :chrEmpEnv;

        Exec SQL close cursorQuote;

        Return chrEmpEnv;

     PGetEmpEnv        e
      *** End   Add *** 09/14/23 *******************************************
