     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE WPICLMPOP - Profound Claim Detail
     h/DEFINE   PROFOUNDUI
     H nomain
     H DFTNAME(WPICLMPOP)
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Handle Pop Ups
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 2/6/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:
     H*
     H*  04/08/25 C:333  Support: 118812 (Project: WC2998)
     H*                 - Recompiled as DSPF WTCLMDTL changed.
     H*
     H*  03/06/25 C:333  Project WC2998
     H*                  Changes to CMS Settlement Reporting.
     H*                  - Recompiled.
     H*                                                                       *
     H*  10/10/24 C:053 Support 152/117512                                    *
     H*                 Add field for EDI reporting.                          *
     H*
     H*  03/04/24 C:342 Project: WC2826
     H*                 - Claims Contacts
     H*                 - Delete data from wmcnpcrt when corresponding treating
     H*                   physician record is deleted using delete link
     H*
     H*  04/10/23 C:333 Support: 108889 (UBIC)
     H*                 - Recompiled as DSPF WTCLMDTL changed.
     H*
     H*  12/06/22 C:291 Project: 152WBZ
     H*                 Add Settlement Information record.
     h*
     H*  06/23/21 C:318 Project WC2302 - Auto tab closure for
     H*                deletion.
     h*
     h*  11/13/20 C:180 Project: C133WRV
     h*                 Add IDC defaults
     h*
     h*  08/18/20 C:053 Project: WC2759
     h*                 Allow overrides on some Work History errors
     H*
     h*  05/26/20 C:180 Project: WC2711C - Restore WC2711
     h*                          - Redesign AWW grid processing - All code cleared.
     h*                          - Added check box for primary comp rate
     h*                            to aww grid in WTCLMDTL.
     h*                          - Additional validation of effective date,
     h*                            and for primary comp rate check box.
     h*                          - Checked for active state comp rate (new logicals added)
     h*
     h*  05/14/20 C:180 Project: WC2711B - Remove WC2711 as it never
     h*                          happened (No comments).
     h*
     H*  01/16/20 C:295 Support: 86011
     H*                 Fix Diagnosis code to more 50 choices in display file.
     H*
     h*  10/30/19 C:212 Project: WC2706
     h*                 Add delete link to Body part Grid
     H*
     h*  10/16/19 C:079 Support: 99/84271
     h*                 Reapply WC2681 change for Legal Issues field
     H*
     h*  07/02/19 C:079 Project: WC2681
     h*                 Make trial grid issues type ahead on description
     H*
     h*  06/17/19 C:079 Project: WC2678
     h*                 Convert Trial Grid Petition field to use sys table
     H*
     h*  05/30/18 C:079 Project: WC2515
     h*                 Add fields for release 3.1
     H*
     h*  05/01/18 C:079 Project: WC1917
     h*                 Body part and location changes
     H*
     h*  04/19/18 C:053 Support: 46001
     h*                 Correct ICD10 dropdown to exclude V,W,X and Y codes
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     h*
     h*  02/14/17 C:212 Project: 90MHY
     h*                 Recompile over changed WTCLMDTL
     h*
     h*  01/31/17 C:306 Project: WC2410
     h*                 Recompile over changed WTCLMDTL
     h*
     h*  06/08/16 C:062 Support: 41904(99)
     h*                 Correct Threshold processing to include new reserves
     h*
     h*  05/24/16 C:309 Support: PHW41670
     h*                 Recompile only
     h*
     h* 05/11/16 C:062 Support: PHW41408
     h*                Multiple reserve codes of the same type are ok.
     h*
     h* 03/18/16 C:306 Support: PHW40181
     h*                Fixed the reserve code type hidden field on display file
     h*                Recompile over WPICLMPOP.
     h*
     h* 12/02/15 C:212 Project: 25MUD
     h*                Recompile over changed WTCLMDTL
     h*
     h* 12/01/15 C:180 Support 38594
     h*                Correct and restrict list of ICD10 codes when
     h*                dealing with Medicare/Non-Medicare beneficiaries.
     h*
     h*  09/22/15 C:267 Support: PHW35142
     h*                 Updated UI by changing the error fields for
     h*                 each subfile record to be unique.  Previously,
     h*                 each subfile record was assigned erReserve1 error.
     h*                 This was for claim reserves edit window.
     h*
     h*  07/02/15 C:306 Support: SEQ86962 / 31805                    *
     h*                 Added the treating physician email validation*
     h*                 check                                        *
     h*                                                              *
     h*
     h* 06/10/15 C:136 Project: 114WLF Priss 62
     h*                Ensure that a couple of the edits are
     h*                specific to the rsv code array
     h*
     h*  05/25/15 C:300 Project: WC2043 (Priss #22)
     h*                 Modify Prior ICD code Information Window
     h*
     h*  04/03/15 C:180 Support 31933
     h*                 Enhance edit to not allow reserve type more than once.
     h*
     h*  04/27/15 C:279 Project: 114WLF  Priss: 47
     h*                 Corrected criteria for displaying reserve worksheet
     h*                 pop-up.
     h*
     h*  04/20/15 C:300 Project: WC2043
     h*                 Display Prior ICD code Information Window
     h*
     h*  04/20/15 C:279 Project: WC2043
     h*                 Modified to use ICD-9 or ICD-10 code based on date of
     h*                 injury.
     h*
     h*  04/17/15 C:300 Support: PHW34297                            *
     h*                 Remove "Actual RTW date edits (WC2067)"      *
     h*                 for Medical Only claims.                     *
     h*                 And New edits on 'Add/Edit Work History'     *
     h*
     h*  03/24/15 C:180 Support 33808
     h*                 Validate cumulative reserve type
     h*
     h*  03/10/15 C:062 Support: 33142 (136)
     h*                 Corredt error message.
     h*
     h*  01/08/15 C:298 Redmine: 4866
     h*                 Corrected the field width of 'Impairment %'
     h*                 in rcdParts on screen.
     h*
     h*  12/01/14 C:279 Project: 114WLF
     h*                 Modified to use Reserve Worksheet.
     h*                 - Re-writing the ValidateRsvCd subprocedure and removed the
     h*                   commented out lines to make it readable.
     H*
     h*  11/12/14 C:062 Support: WC114 87046 and 87092
     h*                 Correct reserve limit edit.
     H*
     h*  11/05/14 C:180 Support  WC114 87092
     H*                 Fix addt'l issues with reserve authority edits.
     H*
     h*  10/20/14 C:180 Support  WC114 87046
     h*                 1. Remove incorrect fix dated 05/19/14.
     h*                    Note fix intended for RED4112 will need to be redone.
     h*  09/03/14 C:282 Redmine: 4600
     h*                 Added the handling to update the claimaint record
     h*                 when AWW History record get updated.
     h*
     h*  08/13/14 C:279 Project: WC2067
     h*                 Modified to add edit for Work History grid.
     h*
     h*  07/22/14 C:282 Redmine: 4559
     h*                 Corrected the passing parameter in 'ClcRemRsvLim'
     h*                 in order to retrieve the reserve limit amount.
     h*
     h*  06/04/14 C:062 Support: WC025 85640
     h*                 Correct reserve edits.
     h*
     h*  05/19/14 C:298 Redmine: 4112
     h*                 Added Error message to prevent user to enter reserve amount
     h*                 if user has zero authority limit.
     h*
     h*  03/14/14 C:295 Support: WC114 84963
     h*                 Add the Delete link for Trial Information Grid.
     h*
     h*  02/21/14 C:295 Support: WA102 82434
     h*                 Added error message to prevent the screen to blowup
     h*                 overflowing ELOUTS.
     h*
     h*  09/02/13 C:190 Project: 147WBZ
     h*                 Removed edit that makes the 'Total Amount Due'
     h*                 field required in the EDI "Employer Paid
     h*                 Information" box.
     h*
     h*  08/30/13 C:062 Support: WC146 82365
     h*                 Correct diagnosis selection.
     h*
     h*  06/12/13 C:062 Support: WC146 82553
     h*                 Correct reserve limit edit.
     h*
     h*  06/03/13 C:283 Support: WC146 82361
     h*                 Add the Delete link for Diagnostic Code.
     h*
     h*  04/23/13 C:062 Redmine: 2762
     h*                 Correct reserve editing.
     h*
     h*  01/15/13 C:279 Project: WC2115
     h*                 Edit correction for Verdict on Trial Add screen
     h*
     h*  10/25/12 C:284 Project: 114WGD
     h*                 CSI Retrofit of Custom Code to Base
     h*
     h**********************************************************************
     fwpiClmPop cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     ExtFile('WPICLMPOP')
     f                                     UsrOpn
     fwmcmp     uf   e             Disk    Rename(pcm01:pcm0p)
     fsadap     uf a e             Disk    ReName(pda01:pda0p)
     fsadal     if   e           k Disk    ReName(pda01:pda00)
     fwda3p     uf a e             Disk    ReName(pa301:pa30p)
     fwdell     uf   e           k Disk    ReName(pel01:pel00)
     fwdmsl     if   e           k Disk    ReName(pms01:pms00)
     fwdmsp     uf a e             Disk    ReName(pms01:pms0p)
     fwdr3p     uf a e             Disk    ReName(pr301:pr30p)
     Fwmadl     if   e           k Disk    ReName(pad01:pad00)
     fwmawp     uf a e             Disk    Rename(paw01:paw0p)
     fwmbnp     uf a e             Disk    ReName(pbn01:pbn0p)
     fwmbyp     uf a e             Disk    ReName(pby01:pby0p)
     fwmbyl     if   e           k Disk    ReName(pby01:pby00)
     fwmtil     if   e           k Disk    ReName(pti01:pti00)
     fwmtip     uf a e             Disk    ReName(pti01:pti0p)
     fwmtpp     uf a e             Disk    ReName(ptp01:ptp0p)
     fwmtpl     if   e           k Disk    ReName(ptp01:ptp00)
     fwmwhp     uf a e             Disk    ReName(pwh01:pwh0p)
      ***** beg add ******* 04/17/15 ***************************************
     fwmwhl     if   e           k disk    rename(pwh01:pwh02)
      ***** end add ******* 04/17/15 ***************************************
     fwmwip     uf a e             Disk    ReName(pwi01:pwi0p)
     fwtabl     if   e           k Disk    ReName(prt01:prt00)
     fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     fwmfnl     if   e           k Disk    ReName(pfn01:pfn00)
     Fwarpl     if   e           k Disk    ReName(prp01:prp00)
     Fwmrhl2    o    e           k Disk    ReName(prh01:prh02)
     fwmpdl     if   e           k Disk    ReName(ppd01:ppd00)
      *** beg change *** 05/26/20 ******************************************
     f**wdrbl2    if   e           k Disk    ReName(prb01:prb00)
     fwdrbl2    if   e           k Disk    ReName(prb01:prb00)
      *** end change *** 05/26/20 ******************************************
      ***** beg add ******* 08/13/14 ***************************************
     fwmlll     if   e           k disk    rename(pll01:pll00)
      ***** end add ******* 08/13/14 ***************************************
      ***** beg add ******* 12/01/14 ***************************************
     fwmcwl     if   e           k disk    rename(pcw01:pcw00)
      ***** end add ******* 12/01/14 ***************************************
     f*** Beg Add *** 10/25/12 *********************************************
     ffawhp     uf a e             Disk    ReName(pwh01:pwh0f) prefix(f:1)
     f                                     UsrOpn
     ffawhl     if   e           k Disk    ReName(pwh01:pwh00)
     f                                     UsrOpn
     ffmcmp     if   e             Disk    ReName(pcm01:pcm0f) Prefix(fm:2)
     f                                     UsrOpn
     ffawhl5    if   e           k Disk    ReName(pwh01:pwh05)
     f                                     UsrOpn
     f*** End Add *** 10/25/12 *********************************************
      ***** beg add ******* 04/20/15 ***************************************
     fsmdil     if   e           k disk    rename(pdi01:pdi00)
     fwmicd10l  if   e           k disk    rename(pic01:pic00)
      ***** end add ******* 04/20/15 ***************************************
      ***** Beg Add 12/01/15 ***********************************************
     fwmcdl     if   e           k Disk    ReName(pcd01:pcd00)
      ***** End Add 12/01/15 ***********************************************
      ***** Beg Add 12/01/15 ***********************************************
     fwmcml1    uf   e           k Disk
      ***** End Add 12/01/15 ***********************************************
      ***** Beg Add 05/26/20 ***********************************************
     fwdr1l1    if   e           k Disk
     fwmawl6    if   e           k Disk    Rename(paw01:paw06)
     fwmawl4    if   e           k Disk    Rename(paw01:paw04)
     fwmawl1    if   e           k Disk
      ***** End Add 05/26/20 ***********************************************
      *** Beg Add *** 11/13/20 *****************************************
     fwmcxp2    if   e           k Disk
      *** End  Add *** 11/13/20 *****************************************
      *** Beg Add ***** 12/06/22 ********************************************
     Fwdstltrk  o    e             Disk    ReName(pst01:pst0p)
      *** End Add ***** 12/06/22 ********************************************
      ***** Beg Add *** 03/04/24 *******************************************
     fwmcnpcrt  uf a e           k Disk    ReName(pcn01:pcn0p_cr)
     f                                     prefix(CR_)
      ***** End Add *** 03/04/24 *******************************************
     d* Calculate Remaining Reserve Limit for User
     D ClcRemRsvLim    pr                  ExtPgm('SRRSVL')
     d  company#                           Like(CMco#)
     d  group#                             Like(CMfnd)
     d  year                               Like(CMfyr)
     d  case                               Like(CMcase)
     d  reserveCode                        Like(CMrt01)
     d  limit                        11  2

     d* Write Auto Darts
     D WrtAutoDart     pr                  ExtPgm('WA700R')
     d  company#                           Like(CMco#)
     d  group#                             Like(CMfnd)
     d  year                               Like(CMfyr)
     d  case                               Like(CMcase)
     d  dartsKey                     10
     d  comment                     256

     d* Get Darts Amount Criteria
     d GetAmtCrit      pr            11  2
     d dartsKey                      10

     d* Get the total Request Amounts
     d GetRequestAmt   pr             9  2
     d rsvCd                          2
      ****** beg  add ****** 12/01/14 ****************************
     d clmRrn#                        9  0 const options(*nopass)

      * Get Remaining Reserves by Reserve Type
     d GetRemRsv       pr            11  2
     d reserveType                    1    Const

      * Get Paid by Reserve Type
     d GetPdRsv        pr            11  2
     d reserveType                    1    Const
      ****** end  add ****** 12/01/14 ****************************

     d* Initialize the error indicators
     d InzErrInd       pr

     d* Update Claim Reserve Buckets
     d UpdateClmRsv    pr
     d clmRrnIn                       9  0

     d* Validate Reserves
     d ValidateRsv     pr
     d clmRrnIn                       9  0
     d statusIn                       1

     d* Validate Reserve Codes
     d ValidateRsvCd   pr

     d* Write Reserve History
     d WriteRsvHst     pr
     d*** Beg Add *** 10/25/12 ***************************************
     d* Get the RRN of the WCMC case
     d GetRrn          pr                  ExtPgm('FA201R')
     d   ##Co#                        3
     d   ##Fnd                        3
     d   ##Fyr                        3
     d   ##Case                       7
     d   @@Rrn1                       9  0
     d   @@Rtcd                       1
     d*** End Add *** 10/25/12 ***************************************

      ***** beg add ***** 04/20/15 ***********************************
      * get the ICD code status
     d GetICDSts       pr                  extpgm('WA120R')
     d @@InjuryDate                   7  0
     d @@ICDSts                       1
      ***** end add ***** 04/20/15 ***********************************
      ***** beg add ***** 12/01/15 ***********************************
      * get the ICD code status
     d GetSysFlags     pr                  ExtPgm('SA001R')
     d   wcmsInst                     1
     d   wcmcInst                     1
     d   wcmcInt                      1
      ***** End add ***** 12/01/15 ***********************************

      ***** beg add ***** 05/26/20 ***********************************
      * get the ICD code status
     d GetBenTypSql    pr
     d   BenTypSel                  200
     d   BenTypFrom                 100
     d   BenTypWher                 200
      ***** End add ***** 05/26/20 ***********************************

      ***** beg add ***** 05/25/15 ***********************************
     d ICDEfDtDA       ds             8    dtaara(ICD10EFDT)
     d strEfDt                 1      8
      ***** end add ***** 05/25/15 ***********************************

     D/copy wPrClmPop
     D/copy sPrDate
     D/copy wPrEmplr
     D/copy sPrString
     D/copy wPrUsrPrms
     D/copy wPrClmErrM
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D/copy sPrGenErr
     D/copy wPrGenPop
     D/copy sPrAtrium
      ******* beg add ******* 12/01/14 *****************************************
     d/copy wPrClmErr
      ******* end add ******* 12/01/14 *****************************************
      ******* beg add ******* 05/26/20 *****************************************
     d/copy wPrEdi
      ******* end add ******* 05/26/20 *****************************************
      //*** Beg Add *** 09/06/22 ***********************************************
     d/copy wPrClaims
      //*** End Add *** 09/06/22 ***********************************************

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)
     D claimDetail   e ds                  extname(wtClmDtl:rcdClmDt0)

     D*Variables
     D kkCaMs          s                   like(MScaMs)
     D kkSuDt          s                   like(TIsuDt)
     D kkTrTp          s                   like(TItrTp)
     D kkJudg          s                   like(TIjudg)
     D kkIssu          s                   like(TIissu)
     D kkVerd          s                   like(TIverd)
     D kkSub#          s                   like(DAsub#) inz(0)
     D kkSeq#          s                   like(BYseq#)
     D kkFein          s                   like(PDfein)
     D kkMod           s                   like(PDmod)
     D*** Beg Add *** 10/25/12 ************************************************
     D kkWkdt          s                   like(WHwkdt)
     D khSeq#          s                   like(WHseq#)
     D intSeq#         s                   like(FHseq#)
     D isWrkDt         s                   like(WHwkdt)
      ******* beg add ****** 12/01/14 *****************************************
     d intIndTot       s             13  2
     d intMedTot       s             13  2
     d intIndTotal     s             13  2
     d intMedTotal     s             13  2
     d intClmRrn       s              9  0
     d isWrkSht        s               n
     d strRrn          s              9
     d strTitle        s             50
      ******* end add ****** 12/01/14 *****************************************
      ******* beg add ****** 04/27/15 *****************************************
     d isInd           s               n   inz(*off)
     d isMed           s               n   inz(*off)
     d isExp           s               n   inz(*off)
     d isLgl           s               n   inz(*off)
     d intTotal        s             15  2
      ******* end add ****** 04/27/15 *****************************************
      ******* beg add ****** 04/20/15 ***********************************
     d @@ICDSts        s              1    inz
     d rtnSts          s              1    inz
      ******* end add ****** 04/20/15 ***********************************
      ***** beg add ***** 12/01/15 ***********************************
     d   wcmsInst      S              1    inz
     d   wcmcInst      S              1    inz
     d   wcmcInt       S              1    inz
      ***** End add ***** 12/01/15 ***********************************
       //FA201R parameters
     d##Co#            S              3    Inz(*Blanks)
     d##Fnd            S              3    Inz(*Blanks)
     d##Fyr            S              3    Inz(*Blanks)
     d##Case           S              7    Inz(*Blanks)
     d@@Rrn1           S              9  0 Inz(*Zeros)
     d@@Rtcd           S              1    Inz(*Blanks)
     D*** End Add *** 10/25/12 ************************************************
     D errorExists     s               n   inz(*off)
     D i               s              3  0
     D j               s              3  0
     D x               s              3  0
     D kkKey           s                   like(RTkey)
     D sumRC           s             11  2
     D int             s              3  0
     D rcAmountTot     s             11  2
     D xxRqPy          S              9  2
     D limAmt          s             11  2
     D rsvLimit        s             11  2
     D strWk11         s             11  2
     D dartsKey        s             10
     D comment         s            256
     D rsvChg          s               n
     D isError         s               n
     D wk              s              2    dim(10)
     d rsvAmtCrit      s             11  2
     d*** Beg Add *** 02/21/14 ***************************************
     d WkElouts        s             11  2
     d*** End Add *** 02/21/14 ***************************************
     d*** Beg Add *** 04/03/15 ***************************************
     D wkt             s              1    dim(10)
     d*** End Add *** 04/03/15 ***************************************
     *** Begin Add *** 05/26/20 *********************************************
     d svAwwPcr        s                   Like(AwPcr)    inz
     d svAwwEfdt       s              8S 0 inz
     *** End   Add *** 05/26/20 *********************************************
      ***** Beg Add *** 03/04/24 *******************************************
     d conType         s             25    inz
      ***** End Add *** 03/04/24 *******************************************

     D                 ds
     D  zipCode                1      9  0
     D  zipCode1               1      5  0
     D  zipCode2               6      9  0

     D                 ds
     D  phoneNumber            1     10  0
     D  areaCode               1      3  0
     D  phone#                 4     10  0

     D                 ds
     D  busPhoneNum            1     15  0
     D  scBusPhone             1     10  0
     D  scBusExtI             11     15  0

     D                 ds
     D  taxID#                 1     12  0
     D  fein                   1      9  0
     D  modifier              10     12  0

     D  groupRsvDs     DS
     D  FNrs01
     D  FNrs02
     D  FNrs03
     D  FNrs04
     D  FNrs05
     D  FNrs06
     D  FNrs07
     D  FNrs08
     D  FNrs09
     D  FNrs10
     D  groupRsv                      2    Overlay(groupRsvDs) Dim(10)

     D                 ds
     D  remRsvCuml             1     90  2 Dim(10)
     D  WKrc01                 1      9  2
     D  WKrc02                10     18  2
     D  WKrc03                19     27  2
     D  WKrc04                28     36  2
     D  WKrc05                37     45  2
     D  WKrc06                46     54  2
     D  WKrc07                55     63  2
     D  WKrc08                64     72  2
     D  WKrc09                73     81  2
     D  WKrc10                82     90  2

     D  rsvCodeDs      ds
     D  CMrt01
     D  CMrt02
     D  CMrt03
     D  CMrt04
     D  CMrt05
     D  CMrt06
     D  CMrt07
     D  CMrt08
     D  CMrt09
     D  CMrt10
     D  rsvCode                       2    Overlay(rsvCodeDs) Dim(10)

     D  rsvPaidDs      ds
     D  CMar01
     D  CMar02
     D  CMar03
     D  CMar04
     D  CMar05
     D  CMar06
     D  CMar07
     D  CMar08
     D  CMar09
     D  CMar10
     D  rsvPaid                       9  2 Overlay(rsvPaidDs) Dim(10)

     D  rsvRemainDs    ds
     D  CMrr01
     D  CMrr02
     D  CMrr03
     D  CMrr04
     D  CMrr05
     D  CMrr06
     D  CMrr07
     D  CMrr08
     D  CMrr09
     D  CMrr10
     D  rsvRemain                     9  2 Overlay(rsvRemainDs) Dim(10)

     D  rsvHstCdDs     ds
     D  RHrt01
     D  RHrt02
     D  RHrt03
     D  RHrt04
     D  RHrt05
     D  RHrt06
     D  RHrt07
     D  RHrt08
     D  RHrt09
     D  RHrt10
     D  rsvHstCd                      2    Overlay(rsvHstCdDs) Dim(10)

     D  rsvHstChgDs    ds
     D  RHsu01
     D  RHsu02
     D  RHsu03
     D  RHsu04
     D  RHsu05
     D  RHsu06
     D  RHsu07
     D  RHsu08
     D  RHsu09
     D  RHsu10
     D  rsvHstChg                     9  2 Overlay(rsvHstChgDs) Dim(10)

     D  scRsvCodeDs    ds
     D  scReserve1
     D  scReserve2
     D  scReserve3
     D  scReserve4
     D  scReserve5
     D  scReserve6
     D  scReserve7
     D  scReserve8
     D  scReserve9
     D  scReserv10
     D  scRsvCode                     2    Overlay(scRsvCodeDs) Dim(10)

     D  scRsvChgDs     ds
     D  scReservC1
     D  scReservC2
     D  scReservC3
     D  scReservC4
     D  scReservC5
     D  scReservC6
     D  scReservC7
     D  scReservC8
     D  scReservC9
     D  scReserC10
     D  scRsvChg                      9  2 Overlay(scRsvChgDs) Dim(10)

     D  scRsvTypDs     ds                  INZ
     D  scTp01
     D  scTp02
     D  scTp03
     D  scTp04
     D  scTp05
     D  scTp06
     D  scTp07
     D  scTp08
     D  scTp09
     D  scTp10
     D  scRsvTyp                      1    Overlay(scRsvTypDs) Dim(10)

     D  erRsvCodeDs    ds
     D  erReserve1
     D  erReserve2
     D  erReserve3
     D  erReserve4
     D  erReserve5
     D  erReserve6
     D  erReserve7
     D  erReserve8
     D  erReserve9
     D  erReserv10
     D  erRsvCode                     1    Overlay(erRsvCodeDs) Dim(10)
     D                                     Inz(*Off)

     D  erRsvChgDs     ds
     D  erReservC1
     D  erReservC2
     D  erReservC3
     D  erReservC4
     D  erReservC5
     D  erReservC6
     D  erReservC7
     D  erReservC8
     D  erReservC9
     D  erReserC10
     D  erRsvChg                      1    Overlay(erRsvChgDs) Dim(10) inz(*Off)

     D  msRsvCodeDs    ds
     D  msReserve1
     D  msReserve2
     D  msReserve3
     D  msReserve4
     D  msReserve5
     D  msReserve6
     D  msReserve7
     D  msReserve8
     D  msReserve9
     D  msReserv10
     D  msRsvCode                    10    Overlay(msRsvCodeDs) Dim(10)

     D  msRsvChgDs     ds
     D  msReservC1
     D  msReservC2
     D  msReservC3
     D  msReservC4
     D  msReservC5
     D  msReservC6
     D  msReservC7
     D  msReservC8
     D  msReservC9
     D  msReserC10
     D  msRsvChg                     10    Overlay(msRsvChgDs) Dim(10)

     D rsvCodeChk      S              2    Dim(10)

     C     key1SADAL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkSub#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2SADAL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkSub#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkSeq#

     C     keyWDELL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMemp#
     C                   kfld                    CMdiv
     C                   kfld                    CMfyr

     C     keyWDMSL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkCaMs

     C     key1WMBYL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMBYL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkSeq#

     C     keyWMCXL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd

     C     keyWMFNL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd

     C     keyWMPDL      Klist
     C                   Kfld                    kkFein
     C                   Kfld                    kkMod

     C     keyWMTIL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkSuDt
     C                   Kfld                    kkTrTp
     C                   Kfld                    kkJudg
     C                   Kfld                    kkIssu
     C*                  Kfld                    kkVerd

     C     key1WMTPL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMTPL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkSeq#

     C     keyWTABL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkKey
     C*** Beg Add ***10/25/12 **********************************************
     C     keyFawhl      Klist
     C                   kfld                    CMco#
     C                   kfld                    FMsub#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase
     C                   kfld                    kkWkdt
     C*
     C     keyFawhl1     Klist
     C                   kfld                    CMco#
     C                   kfld                    FMsub#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase
     C*
     C     keyFawhl2     Klist
     C                   kfld                    CMco#
     C                   kfld                    FMsub#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase
     C                   kfld                    khSeq#
     C*** End Add *** 10/25/12 *********************************************
       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // ChkClmUpdated- Check to see if claim has been updated
       //===================================================================
     P ChkClmUpdated   B                   Export
     D ChkClmUpdated   PI              n
     d claimRrn                       9  0
     d claimDate                      7  0
     d claimTime                      6  0

     d updated         s               n   Inz(*Off)

      /Free

       Open wpiClmPop;
       Chain(n) claimRrn pcm0p;
       If %Found(WMCMP);
          If CMdt<>claimDate or CMtime<>claimTime;
             scClmUpd='This record has already been updated by ' +
              %Trim(CMuser) + '.';
             ExFmt rcdClmUpd;
             updated=*On;
          EndIf;
       EndIf;

       Close wpiClmPop;

       If updated=*On;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free

     P ChkClmUpdated   E
       //===================================================================
       // EditAddAww - Edit/Add AWW History
       //===================================================================
     P EditAddAww      B                   Export
     D EditAddAww      PI              n
     d claimRRN                       9  0
     d awwRRN                         9  0
     d addAww                          n
     d edtAww                          n

     d kkEfDt          s                   Like(CMacDt)

     C     key1WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt
     C                   Kfld                    kkEfDt

     C     key2WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt

      /Free
      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open wpiClmPop;
       scLsn=*Blanks;

       //*** Begin Add *** 05/26/20 ***************************************
       // Initialize screen fields
       msBenTyp   = *blanks;
       erBenTyp   = *Off;
       mgAwwPcr   = *blanks;
       erAwwPcr   = *Off;
       mgAwwEfdtI = *blanks;
       erAwwEfdtI = *Off;
       scAEText   = 'Save';
       scBenTyp = *blanks;
       scAwwAmtI = *Zeros;
       scCompRatI = *Zeros;
       scCalcRatI = *Zeros;
       scMaxCompI = *Zeros;
       scAwwEfdtI = *Zeros;
       scAwwPCR   = 'N';
       svAwwPCR  =  *blanks;
       svAwwEfdt = *Zeros;

       Chain(n) claimRRN pcm0p;


       Select;
       When addAww = *On;
          scAwwHdg = 'Add AWW History';
          scAddAww=*On;
          editAww=*Off;
          isBenTyp = *Off;
          GetBenTypSql(BenTypSel:BenTypFrom:BenTypWher);
          scTranDtI=DateTo8(SystemDate());
       When edtAww = *On;
          scAwwHdg = 'Edit AWW History';
          scAddAww=*Off;
          editAww=*On;
          isBenTyp = *On;
          Exsr LoadAww;
          Exsr CalcAww;
       Endsl;

       DoU errorExists=*Off and scEdtAdd=*On;
          scCalc=*Off;
          Exfmt rcdAww;
          If scCancel = *Off;
             erBenTyp = *Off;
             erAwwPcr = *Off;
             erAwwEfDtI = *Off;
             errorExists=ErrorAww(edtAww);

             If errorExists=*Off;
                exsr CalcAww;

                 If scEdtAdd = *On and  scCalc=*Off;
                    UpdAddAww(claimRRN:awwRRN:addAww:edtAww);
                 EndIf;

             Endif;
          Else;
             Leave;
          Endif;

       EndDo;

       Close wpiClmPop;
       Return scCancel;

       //===================================================================
       // LoadAww - Load Average Weekly Wage Information
       //===================================================================
       Begsr LoadAww;
       Chain(n) awwRRN paw0p;
       If %Found(WMAWP);
          scBenTyp = AWbr2;
          scTranDtI=dtMDYY(AWtrDt);
          scAwwAmtI=AWaww;
          scCompRatI=AWcrt;
          scAwwPCR = AWpcr;
          scAwwEfDtI=dtMDYY(AWefDt);

          svAwwPcr = AWpcr;
          svAwwEfdt = dtMDYY(AWefDt);
       EndIf;
       Endsr;
       //===================================================================
       // CalcAww - Calculate Average Weekly Wage Information
       //===================================================================
       Begsr CalcAww;

       //Benefit type Comp rate
       kkEfDt=CMacDt;
       Setgt (CMco#:CMfnd:CMjrst:scBenTyp:kkefdt) PR101;
       Readpe (CMco#:CMfnd:CMjrst:scBenTyp) PR101;
       If not %eof(wdr1l1);
          scCalcRatI = scAwwAmtI * R1rate/100;
       Endif;

       SetGt key1WDRBL prb00;
       ReadPe key2WDRBL prb00;
       If not %Eof(WDRBL2);

          //Default rate if benefit type Comp rate absent
          If %eof(wdr1l1);
             scCalcRatI=scAwwAmtI * RBcmpr / 100;
          Endif;

         //Minimum rate
         If scCalcRatI<RBmnrt;
            scCalcRatI=RBmnrt;
         EndIf;

         //Maximum rate
         If scCalcRatI>RBpacp;
            scCalcRatI=RBpacp;
         EndIf;
          scMaxCompI=RBpacp;
       EndIf;

       //Comp rate
       If scCompRatI=0;
          scCompRatI=scCalcRatI;
       EndIf;

       Endsr;
       //*** End   Add *** 05/26/20 ***************************************
      /End-Free
     P EditAddAww      E
       //===================================================================
       // EditAddBenefit - Edit/Add Benefit Information
       //===================================================================
     P EditAddBenefit  B                   Export
     D EditAddBenefit  PI              n
     d claimRRN                       9  0
     d benefitRRN                     9  0
     d addBenefit                      n
     d edtBenefit                      n
     d delBenefit                      n
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open  wpiClmPop;
       readBene=*Off;
       disBene=*Off;
       Chain(n) claimRRN pcm0p;
       If delBenefit=*On;
          scAEText='Delete';
       Else;
          scAEText='Save';
       EndIf;
       DoU errorExists=*Off;
          // Load Fields
          If edtBenefit=*On or delBenefit=*On;
             If delBenefit=*On;
                disBene=*On;
             EndIf;
             Chain(n) benefitRRN pbn0p;
             If %Found(WMBNP);
                scBenStDtI=dtMDYY(BNstDt);
                scAdjCodeI=%Subst(BNaCod:1:1);
                scPmtAdCdI=%Subst(BNaCod:2:3);
                scBenAmtI=BNamt;
                scBenEnDtI=dtMDYY(BNenDt);
                scBenStsI=BNstat;
       //*** Beg Add *** 05/30/18 **************************
                scOffAmt=BNwIaO;
       //*** End Add *** 05/30/18 **************************
             EndIf;
             If delBenefit=*On;
                readBene=*On;
             EndIf;
          Else;
             scBenStDtI=*Zeros;
             scAdjCodeI=*Blanks;
             scPmtAdCdI=*Blanks;
             scBenAmtI=*Zeros;
             scBenEnDtI=*Zeros;
             scBenStsI='A';
       //*** Beg Add *** 05/30/18 *******************************
             scBenStsI=*Zeros;
       //*** End Add *** 05/30/18 *******************************
          Endif;
          Exfmt rcdBenefit;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorBenefit();
             If errorExists=*Off;
                UpdAddBenefit(benefitRRN:addBenefit:edtBenefit:delBenefit);
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddBenefit  E
       //===================================================================
       // EditAddDiag - Edit/Add Diagnosis Codes
       //===================================================================
     P EditAddDiag     B                   Export
     D EditAddDiag     PI              n
     d claimRRN                       9  0
     d diagRRN                        9  0
     d addDiag                         n
     d edtDiag                         n
      *** Beg Add ********* 06/03/13 ********************
     d deleteDiag                      n
      *** End Add ********* 06/03/13 ********************
      *** Begin Add *** 08/30/2013 **************************************************************
     d cmsSub                          n
      *** Begin Add *** 08/30/2013 **************************************************************
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open  wpiClmPop;

       // Location Select
       scLocSel='GTSTAT<>''I'' AND GTTBID=''ZZIL''';

       // Site Select
       scSiteSel='GTSTAT<>''I'' AND GTTBID=''ZZIT''';

       //*** Begin Add *** 08/30/2013 **************************************************************
       // Diagnosis Code Select
       If cmsSub;
          selDiagCd = 'DIstat<>''I'' And DIcms=''I''';
       Else;
          selDiagCd = 'DIstat<>''I''';
       EndIf;
       //*** End   Add *** 08/30/2013 **************************************************************

       Chain(n) claimRRN pcm0p;

       //****** beg add ****** 04/20/15 ***************************************
       // check if ICD-9 or ICD-10 codes are active
       @@ICDSts = ' ';
       GetICDSts(CMacdt:@@ICDSts);

       //*** beg chg *** 04/19/18 ************************************      ***
       //selDiagCd1 = 'ICstat <> ''I''';
       selDiagCd1 = 'ICstat <> ''I'' +
          and ICcode NOT LIKE ''V%'' +
          and ICcode NOT LIKE ''W%'' +
          and ICcode NOT LIKE ''X%'' +
          and ICcode NOT LIKE ''Y%''';
       //*** end chg *** 04/19/18 ************************************      ***
       //****** end add ****** 04/20/15 ***************************************
       //****** Beg Add *** 12/01/15 ******************************************
       selDiagCd2 = 'ICstat <> ''I''';
       *in80 = *off ;
       *in81 = *off;
       *in82 = *off;
       // Get System Flags
       GetSysFlags(wcmsInst:wcmcInst:wcmcInt);
       // Load Demographic Field
       If wcmcInst = 'Y' and wcmcInt = 'Y';
          Chain CMss# pcd00;
             If %Found(wmcdl) and CDmdBn = 'Y';
                *in81=*on;
             EndIf;
       Endif;
       //****** End Add *** 12/01/15 ******************************************

       scAEText='Save';
       DoU errorExists=*Off;
          // Load Fields
          //****** beg add ****** 04/20/15 *********************************
          isPriorCd  = *off;
        //*** Begin delete *** 12/01/15 ****************************
        //***  isNotICD9  = *off;
        //***  isNotICD10 = *off;
        //*** End   delete *** 12/01/15 ****************************
          clear scDiagCode;

          If @@ICDSts = '1';
          //*** Beg change *** 12/01/15 ******************************
          //*** isNotICD9  = *off;
          //*** isNotICD10 = *on;
             *in80= *on;
             *in81= *off;
          //*** End change *** 12/01/15 ******************************
          Else;
       //***** Beg Chg *** 12/01/15 ******************************************
       //*** isNotICD9  = *on;
       //*** isNotICD10 = *off;
             *in82  = *on;
             if *in81;
                *in82 = *off;
             Endif;
       //***** End Chg *** 12/01/15 ******************************************
          Endif;
          //****** end add ****** 04/20/15 *********************************

          // *** Beg Chg ********* 06/03/13 ********************
          //If edtDiag=*On;
          If edtDiag=*On or deleteDiag = *On;
          // *** End Chg ********* 06/03/13 ********************
             Chain(n) diagRRN pda0p;
             If %Found(SADAP);
                //****** beg chg ****** 04/20/15 *********************************
                // scDiagCd=DAicd9 + DAlosT;

                If @@ICDSts = '1';
                   If DAicd1001 <> ' ';
                      scDiagCd = ' ';
                      isPriorCd  = *on;
                      scDiagCode = DAicd1001 + '  ' +DAlosT;

                      Chain (DAicd1001:DAlost) pic00;
                      If %found(WMICD10L);
                         scDiagCode = %trim(scDiagCode) + ' - ' + %trim(ICdsc1);
                      Endif;
                   Else;
                      scDiagCd = DAicd9 + DAlosT;
                   Endif;
                ElseIf @@ICDSts = '2';
                   If DAicd9 <> ' ';
                      scDiagCd1 = ' ';
                      //*** Begin Add *** 12/01/15 ************************************/
                      scDiagCd2 = ' ';
                      //*** End Add *** 12/01/15   ************************************/
                      isPriorCd  = *on;
                      scDiagCode = DAicd9 + '  ' + DAlosT;

                      Chain (DAicd9:DAlost) pdi00;
                      If %found(SMDIL);
                         scDiagCode = %trim(scDiagCode) + ' - ' + %trim(DIdsc1);
                      Endif;
                   Else;
                    //*** Begin Change  *** 12/01/15 **************************************/
                      //***scDiagCd1 = DAicd1001 + DAlosT;
                      If *in81;
                         scDiagCd1 = DAicd1001 + DAlosT;
                      else;
                         scDiagCd2 = DAicd1001 + DAlosT;
                      endif;
                    //*** End   Change  *** 12/01/15 **************************************/
                   Endif;
                Endif;
                //****** end chg ****** 04/20/15 *********************************
                scLoca=DAiLoc;
                scSite=DAiSit;
                scStat=DAiSts;
                scFrDate=dtMDYY(DAisDt);
                scToDate=dtMDYY(DAexDt);
                scPrim=DAprim;
             EndIf;
          Else;
             scDiagCd=*Blanks;
             //****** beg add ****** 04/20/15 *********************************
             scDiagCd1 = *Blanks;
             //****** end add ****** 04/20/15 *********************************
             //****** beg add ****** 12/01/15 *********************************
             scDiagCd2 = *Blanks;
             //****** end add ****** 12/01/15 *********************************
             scLoca=*Blanks;
             scSite=*Blanks;
             scStat=*Blanks;
             scFrDate=dtMDYY(0);
             scToDate=dtMDYY(0);
             scPrim=*Blanks;
          Endif;
          // *** Beg Add ********* 06/03/13 ********************
          readDiag = *Off;
          If deleteDiag = *On;
             readDiag = *On;
             scAEtext = 'Delete';
          Endif;
          // *** End Add ********* 06/03/13 ********************
          Exfmt rcdDiag;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorDiag();
             If errorExists=*Off;
                // *** Beg Chg ********* 06/03/13 ********************
                //UpdAddDiag(diagRRN:addDiag:edtDiag);
                UpdAddDiag(diagRRN:addDiag:edtDiag:deleteDiag);
                // *** End Chg ********* 06/03/13 ********************
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddDiag     E
       //****** beg add ****** 04/20/15 ***************************************
       //===================================================================
       // ShowICDWdw - Show Prior ICD Code Information Window
       //===================================================================
     P ShowICDWdw      B                   Export
     D ShowICDWdw      PI              n
     d claimRRN                       9  0
     d @@StsICD                       1
      /Free
       rtnSts = *Off;

       //*** beg add ***** 05/25/15 ***********************************
       // Read Date from ICD10 Data Area
       In ICDEfDtDA;
       scICD10Dt = %dec(strEfDt:8:0);
       //*** end add ***** 05/25/15 ***********************************

       Open  wpiClmPop;
       Chain(n) claimRrn pcm0p;
       If %Found(WMCMP);
          If @@StsICD = '2' and CMain1 = *blanks;
             If CMainj <> *blanks;
                scCurICD9c = CMainj;
                //*** beg add ***** 05/25/15 ***********************************
                Chain CMainj pdi00;
                If %found(SMDIL) and DIdsc1 <> *blanks;
                   scCurICD9c = CMainj + ' - ' + %trim(DIdsc1);
                Endif;
                //*** end add ***** 05/25/15 ***********************************
                IsICD10vs  = *On;
                IsICD10ds  = *Off;
                ExFmt rcdICDWdw;
             Endif;
          EndIf;
       EndIf;

       If scICD10cd <> *blanks and scSave = *on;
          Chain claimRrn pcm0p;
          If %Found(WMCMP);
             CMain1 = scICD10cd;
             Update pcm0p;
             rtnSts = *On;
          EndIf;
       EndIf;

       Close wpiClmPop;

       Return rtnSts;
      /End-Free

     P ShowICDWdw      E
       //****** end add ****** 04/20/15 ***************************************

       //===================================================================
       // EditAddEarning - Edit/Add Earning Information
       //===================================================================
     P EditAddEarning  B                   Export
     D EditAddEarning  PI              n
     d claimRRN                       9  0
     d earningRRN                     9  0
     d addEarning                      n
     d edtEarning                      n
     d delEarning                      n
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open  wpiClmPop;
       readEarn=*Off;
       Chain(n) claimRRN pcm0p;
       If delEarning=*On;
          scAEText='Delete';
       Else;
          scAEText='Save';
       EndIf;
       DoU errorExists=*Off;
          // Load Fields
          If edtEarning=*On or delEarning=*On;
             Chain(n) earningRRN pr30p;
             If %Found(WDR3P);
                scRedWeekI=R3reWk;
                scAcRedAmI=R3aRe;
                scDmRedAmI=R3dRe;
       //*** Beg Add *** 05/30/18 ***************************************
                scRedStrDt=dtMdYy2(R3reSd);
                scRedEndDt=dtMdYy2(R3reEd);
                scNetAmt=R3reWa;
       //*** End Add *** 05/30/18 ***************************************
             EndIf;
             If delEarning=*On;
                readEarn=*On;
             EndIf;
          Else;
             // fields here
             scRedWeekI=*Zeros;
             scAcRedAmI=*Zeros;
             scDmRedAmI=*Zeros;
       //*** Beg Add *** 05/30/18 ***************************************
             scRedStrDt=*Zeros;
             scRedEndDt=*Zeros;
             scNetAmt=*Zeros;
       //*** End Add *** 05/30/18 ***************************************
          Endif;
          Exfmt rcdEarning;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorEarning();
             If errorExists=*Off;
                UpdAddEarning(earningRRN:addEarning:edtEarning:delEarning);
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddEarning  E
       //===================================================================
       // EditAddMult - Edit/Add Multiple Settlement
       //===================================================================
     P EditAddMult     B                   Export
     D EditAddMult     PI              n
     d claimRRN                       9  0
     d multRRN                        9  0
     d addMult                         n
     d edtMult                         n
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open  wpiClmPop;
       Chain(n) claimRRN pcm0p;
       scAEText='Save';
       DoU errorExists=*Off;
          // Load Fields
          If edtMult=*On;
             Chain(n) multRRN pms0p;
             If %Found(WDMSP);
                scMthSettI=MScaMs;
                scTpocAmtI=MStpoA;
                scTpocDtI=dtMDYY(MStpoD);
                scDlayDtI=dtMDYY(MSdlyD);
             EndIf;
          Else;
             scMthSettI=*Blanks;
             scTpocAmtI=*Zeros;
             scTpocDtI=*Zeros;
             scDlayDtI=*Zeros;
          Endif;
          Exfmt rcdMult;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorMult(addMult);
             If errorExists=*Off;
                UpdAddMult(multRRN:addMult:edtMult);
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddMult     E
       //===================================================================
       // EditAddPaid - Edit/Add Employer Paid Information
       //===================================================================
     P EditAddPaid     B                   Export
     D EditAddPaid     PI              n
     d claimRRN                       9  0
     d paidRRN                        9  0
     d addPaid                         n
     d edtPaid                         n
     d delPaid                         n
      /Free
      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************

       Open  wpiClmPop;
       readPaid=*Off;
       disPaid=*Off;
       Chain(n) claimRRN pcm0p;
       If delPaid=*On;
          scAEText='Delete';
       Else;
          scAEText='Save';
       EndIf;
       DoU errorExists=*Off;
          // Load Fields
          If edtPaid=*On or delPaid=*On;
             If delPaid=*On;
                disPaid=*On;
             EndIf;
             Chain(n) paidRRN pa30p;
             If %Found(WDA3P);
                scPmtCodeI=A3btc;
                scTotPaidI=A3paid;
                scEmStrDtI=dtMDYY2(A3stDt);
                scEmEndDtI=dtMDYY2(A3enDt);
             EndIf;
             If delPaid=*On;
                readPaid=*On;
             EndIf;
          Else;
             scPmtCodeI=*Blanks;
             scTotPaidI=*Zeros;
             scEmStrDtI=*Zeros;
             scEmEndDtI=*Zeros;
          Endif;
          Exfmt rcdPaid;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorPaid();
             If errorExists=*Off;
                UpdAddPaid(paidRRN:addPaid:edtPaid:delPaid);
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddPaid     E
       //===================================================================
       // EditAddPart - Edit/Add Body Parts
       //===================================================================
     P EditAddPart     B                   Export
     D EditAddPart     PI              n
     d claimRRN                       9  0
     d partRRN                        9  0
     d addPart                         n
     d edtPart                         n
     d***** Beg Add *** 10/30/19 ********************************
     d delPart                         n
     d***** End Add *** 10/30/19 ********************************
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open  wpiClmPop;
       Chain(n) claimRRN pcm0p;
       scAEText='Save';
       DoU errorExists=*Off;
          // Load Fields
      //***** Beg Chg *** 10/30/19 ********************************
      //***If edtPart=*On;
          If edtPart=*On Or delPart=*On;
      //***** End Chg *** 10/30/19 ********************************
             Chain(n) partRRN pby0p;
             If %Found(WMBYP);
                scPartCd=BYkey;
                scImpPct=BYpct;
       //*** Beg Add *** 05/01/18 **********************************
                scLoc=BYiMlC;
       //*** End Add *** 05/01/18 **********************************
             EndIf;
          Else;
             scPartCd=*Blanks;
             scImpPct=*Zeros;
       //*** Beg Add *** 05/01/18 **********************************
             scLoc=*blanks;
       //*** End Add *** 05/01/18 **********************************
          Endif;
          //***** Beg Add *** 10/30/19 *************************
          readParts = *Off;
          If delPart = *On;
             scAEtext = 'Delete';
             readParts = *On;
          Endif;
          //***** End Add *** 10/30/19 *************************
          Exfmt RcdParts;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorPart();
             If errorExists=*Off;
          //***** Beg Chg *** 10/30/19 *************************
          //*****UpdAddPart(partRRN:addPart:edtPart);
                UpdAddPart(partRRN:addPart:edtPart:delPart);
          //***** End Chg *** 10/30/19 *************************
             Endif;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddPart     E
       //===================================================================
       // EditAddPhys - Edit/Add Treating Physicians
       //===================================================================
     P EditAddPhys     B                   Export
     D EditAddPhys     PI              n
     d claimRRN                       9  0
     d physRRN                        9  0
     d addPhys                         n
     d edtPhys                         n
     d delPhys                         n
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open  wpiClmPop;
       readPhys=*Off;
       Chain(n) claimRRN pcm0p;
       If delPhys=*On;
          scAEText='Delete';
       Else;
          scAEText='Save';
       EndIf;

       scPhyList='PDFEIN || '' '' || PDMOD || ''-'' || ' +
        'CASE WHEN PDDBA1=''Y'' THEN TRIM(PDADR1) || ''-'' || ' +
        'TRIM(PDADR2) ELSE ' +
        'TRIM(PDNAME) || ''-'' || TRIM(PDADR1) || '' '' || ' +
        'TRIM(PDADR2) END ' +
        ' || ''- '' || TRIM(PDCTY) || '', '' || ' +
        'PDST || '' '' || SUBSTR(DIGITS(PDZIP),1,5)';

       // Load Fields
       If edtPhys=*On or delPhys=*On;
          If delPhys=*On;
             readPhys=*On;
          EndIf;
          Chain(n) physRRN ptp0p;
          If %Found(WMTPP);
             scPhyFeI=TPphyF*1000 + TPphyM;
             scPhyNameI=TPphyN;
             scPhyAdrI=TPphyA;
             scPhyCityI=TPphyC;
             scPhyStI=TPphyS;
             scPhyZipI=TPphyZ;
             scPhyZipI=TPphyZ;
             scPhyZipI=TPphyZ;
             scPhyPhnI=TPphyR*10000000 + TPphyP;
             scPhyFaxI=TPfxAr*10000000 + TPfax#;
             scRoleI=TPpRol;
             scFrDtI=dtMDYY(TPfrDt);
             scToDtI=dtMDYY(TPtoDt);
             scEndDatI=TPeDat;
             scClNameI=TPclin;
             scPhysEmAd=TPemAd;
          EndIf;
       Else;
          scPhyFeI=*Zeros;
          scPhyNameI=*Blanks;
          scPhyAdrI=*Blanks;
          scPhyCityI=*Blanks;
          scPhyStI=*Blanks;
          scPhyZipI=*Zeros;
          scPhyPhnI=*Zeros;
          scPhyFaxI=*Zeros;
          scRoleI=*Blanks;
          scFrDtI=0;
          scToDtI=0;
          scEndDatI=*Blanks;
          scClNameI=*Blanks;
          scPhysEmAd=*Blanks;
       Endif;

       DoU errorExists=*Off And (scCancel=*On or scEdtAdd=*On);
          If scPhyFeI <> 0;
             taxID#=scPhyFeI;
             kkFein=fein;
             KKMod=modifier;
             Chain keyWMPDL ppd00;
             If %Found(WMPDL);

                If PDfsn <> *Blanks;
                   scPhyNameI=PDname + ', ' + PDfsn;
                Else;
                   scPhyNameI=PDname;
                EndIf;

                If PDdba1 = 'Y';
                   scPhyAdrI=PDadr2;
                Else;
                   scPhyAdrI=PDadr1;
                EndIf;

                scPhyCityI=PDcty;
                scPhyStI=PDst;
                scPhyZipI=PDzip;
                If scPhyPhnI=0;
                   scPhyPhnI=PDarea*10000000 + PDphn#;
                EndIf;
                If scPhyFaxI=0;
                   scPhyFaxI=PDfxAr*10000000 + PDfax#;
                EndIf;
                If scPhysEmAd=*Blanks;
                   scPhysEmAd=PDemAd;
                EndIf;
             EndIf;
          EndIf;

          Exfmt rcdPhys;
          If scCancel=*Off and scEdtAdd=*On;
             errorExists=*Off;
             errorExists=ErrorPhys();
             If errorExists=*Off;
                UpdAddPhys(physRRN:addPhys:edtPhys:delPhys);
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddPhys     E
       //*** Beg Add ***** 12/06/22 ****************************************
       //===================================================================
       // EditAddSettle - Edit/Add Settlement Information
       //===================================================================
     P EditAddSettlement...
     P                 B                   Export
     D EditAddSettlement...
     D                 PI              n
     d claimRRN                       9  0
     d settleRrn                      9  0
     d addSettle                       n

       OutCmRrn = claimRRN;
       Open  wpiClmPop;
       Chain(n) claimRRN pcm0p;
       scAEText='Save';

       // Load Fields
       scCurrAuth = *Zeros;                  //Current Authority
       scDFrmOpp  = *Zeros;                  //Current Demand from Opposing Council
       Clear scAAprDt;                       //Date Authority Approved
       Clear scDtOfDmnd;                     //Date of Demand
       Dou errorExists = *Off;
          Exfmt rcdSettle;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorSettlement(addSettle);
             If errorExists=*Off;
                STcursaut = scCurrAuth;
                STautaprdt = dtCYMD(scAAprDt);
                STcurdoc = scDFrmOpp;
                STdocdt = dtCYMD(scDtOfDmnd);
                STco# = CMco#;
                STfnd = CMfnd;
                STfyr = CMfyr;
                STcase = CMcase;
                STuser = q1User;
                STpgm = q1Pgm;
                STtime = TimeTo6(SystemTime());
                STdt = DateTo7(SystemDate());
                Write pst0p;
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

     P EditAddSettlement...
     P                 E
       //*** End Add ***** 12/06/22 ****************************************
       //===================================================================
       // EditAddTrial - Edit/Add Trial Information
       //===================================================================
     P EditAddTrial    B                   Export
     D EditAddTrial    PI              n
     d claimRRN                       9  0
     d trialRRN                       9  0
     d addTrial                        n
     d edtTrial                        n
      *** End Add ********* 03/14/14 ********************
     d delTrial                        n
      *** End Add ********* 03/14/14 ********************
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open  wpiClmPop;
       Chain(n) claimRRN pcm0p;
       scAEText='Save';

       // clear error indicators
       erVerdDtI = *Off;
       erVerdictI = *Off;

       // Load Fields
          // *** Beg Chg ********* 04/14/14 ********************
       //***** If edtTrial = *On;
       If edtTrial = *On or delTrial = *On;
          // *** End Chg ********* 04/14/14 ********************
          Chain(n) trialRRN pti0p;
          If %Found(WMTIP);
             scSuitDtI  = dtMDYY(TIsuDt);
             scTrTypeI  = TItrTp;
             scJudgeI   = TIjudg;
             scLegIssI  = TIissu;
             scCrtLvlI  = TIcLvl;
             scVerdictI = TIverd;
             scVerdDtI  = dtMDYY(TIveDt);
             scPetitI   = TIpeti;
          EndIf;
       Else;
          scSuitDtI = *Zeros;
          scTrTypeI = *Blanks;
          scJudgeI  = *Blanks;
          scLegIssI = *Blanks;
          scCrtLvlI = *Blanks;
          scVerdictI = *Blanks;
          scVerdDtI = *Zeros;
          scPetitI = *Blanks;
       Endif;
       // *** Beg Add ********* 03/14/14 ********************
        //**************readDiag = *Off;
        If delTrial = *On;
        //**************   readDiag = *On;
           scAEtext = 'Delete';
        Endif;
        // *** End Add ********* 03/14/14 ********************
       DoU errorExists = *Off;
          Exfmt rcdTrial;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorTrial(addTrial);
             If errorExists=*Off;
                // *** Beg Chg ********* 03/14/14 ********************
                //***** UpdAddTrial(trialRRN:addTrial:edtTrial);
                UpdAddTrial(trialRRN:addTrial:edtTrial:delTrial);
                // *** Beg Chg ********* 03/14/14 ********************
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddTrial    E
       //===================================================================
       // EditAddWitness - Edit/Add Witness Information
       //===================================================================
     P EditAddWitness  B                   Export
     D EditAddWitness  PI              n
     d claimRRN                       9  0
     d witnessRRN                     9  0
     d addWitness                      n
     d edtWitness                      n
     d delWitness                      n
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       readWitn=*Off;
       Open  wpiClmPop;
       Chain(n) claimRRN pcm0p;
       If delWitness=*On;
          scAEText='Delete';
       Else;
          scAEText='Save';
       EndIf;
       DoU errorExists=*Off;
          // Load Fields
          If edtWitness=*On or delWitness=*On;
             Chain(n) witnessRRN pwi0p;
             If %Found(WMWIP);
                scWitEmadI=WIemAd;
                scWitNameI=WIname;
                busPhoneNum=WIbPh#;
             EndIf;
             If delWitness=*On;
                readWitn=*On;
             EndIf;
          Else;
             scWitEmadI=*Blanks;
             scWitNameI=*Blanks;
             busPhoneNum=*Zeros;
          Endif;
          Exfmt rcdWitness;
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorWitness();
             If errorExists=*Off;
                UpdAddWitness(witnessRRN:addWitness:edtWitness:delWitness);
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddWitness  E
       //===================================================================
       // EditAddWork - Edit/Add Work Information
       //===================================================================
     P EditAddWork     B                   Export
     D EditAddWork     PI              n
     d claimRRN                       9  0
     d workRRN                        9  0
     d addWork                         n
     d edtWork                         n
     d delWork                         n

      ******** beg add ******** 08/13/14 ***********************************
     d intStlDt        s              7  0
     d isErrFnd        s               n
     d strMsg          s            256
      ******** end add ******** 08/13/14 ***********************************
      /Free

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = claimRRN;
      //**** End Add  ***  06/23/21  ****************************************
       Open  wpiClmPop;
       Chain(n) claimRRN pcm0p;
       If delWork=*On;
          scAEText='Delete';
       Else;
          scAEText='Save';
       EndIf;
       DoU errorExists=*Off;
          // Load Fields
          If edtWork=*On or delWork=*On;
             Chain(n) workRRN pwh0p;
             If %Found(WMWHP);
                scWorkDtI=dtMDYY(WHdate);
                scWorkStsI=WHwkSt;
                scRtwQuaI=WHrtwQ;
       //*** Beg Add *** 05/30/18 **************************
                scEmpDisDt=dtMDYY(WHcDeK);
                scAdmDisDt=dtMDYY(WHcDaK);
                scLastDay=dtMDYY(WHcDlW);
                scRTWSmEmp=WHlRsE;
       //*** End Add *** 05/30/18 **************************
             EndIf;
             If delWork=*On;
                readWork=*On;
                disWork=*On;
             Else;
                readWork=*Off;
                disWork=*Off;
             EndIf;
          Else;
             scWorkDtI=*Zeros;
             scWorkStsI=*Blanks;
             scRtwQuaI=*Blanks;
       //*** Beg Add *** 05/30/18 **************************
             scEmpDisDt=*Zeros;
             scAdmDisDt=*Zeros;
             scLastDay=*Zeros;
             scRTWSmEmp=*blank;
       //*** End Add *** 05/30/18 **************************
          Endif;

          //******** beg add ******* 04/17/15 **********************************
          If CMltfl <> 'M';
          //******** end add ******* 04/17/15 **********************************
          //********* beg add ******** 08/13/14 ********************************
          isErrFnd = *off;

          If addWork = *on;
             readWork = *off;
             disWork  = *off;
             intStlDt = 0;
             strMsg   = ' ';

             Chain (CMco#:CMfnd:CMfyr:CMcase) pll00;
             If %found(WMLLL);
                intStlDt = LLstdt;
             Endif;

             // Validate for Actual RTW date, Date of Death and Date of Settlement.
             If CMrtw = 0;
          //*** beg del *** 08/18/20 ******************************
          //*** isErrFnd = *on; ***********************************
          //*** end del *** 08/18/20 ******************************
                strMsg = 'Claimant has not returned to work yet. Work history +
                 information can''t be added.';
          //*** beg add *** 08/18/20 ******************************
                If Not PopTwoBtn(strMsg:'Error':'Override':'OK');
                Leave;
                Endif;
          //*** end add *** 08/18/20 ******************************
             Endif;

             If isErrFnd = *off and CMdod > 0;
                isErrFnd = *on;
                strMsg = 'Date of Death exist for this claimant. Work history +
                 information can''t be added.';
             Endif;

             If isErrFnd = *off and intStlDt > 0;
                isErrFnd = *on;
                strMsg = 'Date of Settlement exist for this claimant. Work +
                 history information can''t be added.';
             Endif;

             If isErrFnd = *on;
                PopOneBtn(strMsg:'Error');
                Leave;
             Endif;
          Endif;
          //********* end add ******** 08/13/14 ********************************
          //******** beg add ******* 04/17/15 **********************************
          Endif;
          //******** end add ******* 04/17/15 **********************************

          Exfmt rcdWork;

          //******** beg add ******* 04/17/15 **********************************
          If addWork = *on and scWorkStsI= 'R';
             Chain(n) key1WMBYL pwh02;
             If Not %Found(WMWHL);
                strMsg = 'Only Start Losing Time or Release to Work are +
                  allowed as the first record in the grid';
          //*** beg chg *** 08/18/20 ******************************
          //*** PopOneBtn(strMsg:'Error');*************************
                If Not PopTwoBtn(strMsg:'Error':'Override':'OK');
          //*** end chg *** 08/18/20 ******************************
                Leave;
          //*** beg add *** 08/18/20 ******************************
                Endif;
          //*** end add *** 08/18/20 ******************************
             Endif;
          Endif;

          If (addWork = *on or edtWork=*On) and scWorkStsI= 'R';
             If scRtwQuaI <> '1' and scRtwQuaI <> '2';
                strMsg = 'Improper code was selected.  RTW Qualifier must +
                  match the Work History Status';
                PopOneBtn(strMsg:'Error');
                Leave;
             Endif;
          Endif;

          If (addWork = *on or edtWork=*On) and scWorkStsI= 'E';
             If scRtwQuaI <> '5' and scRtwQuaI <> '6';
                strMsg = 'Improper code was selected.  RTW Qualifier must +
                  match the Work History Status';
                PopOneBtn(strMsg:'Error');
                Leave;
             Endif;
          Endif;
          //******** end add ******* 04/17/15 **********************************
          errorExists=*Off;
          If scCancel=*Off;
             errorExists=ErrorWork();
             If errorExists=*Off;
                UpdAddWork(workRRN:addWork:edtWork:delWork);
             EndIf;
          EndIf;
       EndDo;

       Close wpiClmPop;
       Return scCancel;

      /End-Free
     P EditAddWork     E
       //*** Beg Add *** 10/25/12 ******************************************
       //===================================================================
       // EditAddWrk - Edit/Add Work Information for CSI
       //===================================================================
     P EditAddWrk      B                   Export
     D EditAddWrk      PI              n
     d claimRRN                       9  0
     d workRRN                        9  0
     d addWork                         n
     d edtWork                         n
     d delWork                         n


      /Free

       Open  wpiClmPop;
       Open  fawhp;
       Open  fmcmp;
       Chain(n) claimRRN pcm0p;
       ##Co#=%editc(CMco#:'X');
       ##Fnd=%editc(CMfnd:'X');
       ##Fyr=%editc(CMfyr:'X');
       ##Case=%editc(CMcase:'X');

       GetRrn(##Co#:##Fnd:##Fyr:##Case:@@Rrn1:@@Rtcd);
       IF @@Rtcd <> 'F';
          Chain @@Rrn1 pcm0f;
          If delWork=*On;
             scAETextC='Delete';
          Else;
             scAETextC='Save';
          EndIf;
          scWrkCdeCh='GTTBID=''WKCD'' AND GTSTAT<>''I'' AND GTCO#='
           +%editc(CMco#:'X');
          scLvlDtyCh = 'DLSTAT<>''I''';

          DoU errorExists=*Off;
             // Load Fields
            If edtWork=*On or delWork=*On;
                Chain(n) workRRN pwh0f;
                If %Found(FAWHP);
                   scWrkDt=dtMDYY(FHwkdt);
                   isWrkDt=dtCYMD(scWrkdt);
                   scWrkCode=FHcode;
                   scLvlDuty=FHlvl;
                EndIf;
               If delWork=*On;
                   readWrk=*On;
                   disWrk=*On;
                Else;
                   readWrk=*Off;
                   disWrk=*Off;
                EndIf;
             Else;
                readWrk=*Off;
                disWrk=*Off;
                scWrkDt=*Zeros;
                scWrkCode=*Blanks;
                scLvlDuty=*Blanks;
             Endif;
             Exfmt rcdWrk;
             errorExists=*Off;
             If scCancelC=*Off;
                If delWork <> *On;
                   errorExists=ErrorWrk(addWork:edtWork);
                Endif;
                If errorExists=*Off;
                   UpdAddWrk(workRRN:addWork:edtWork:delWork);
                EndIf;
             EndIf;
          EndDo;
       EndIf;

       Close wpiClmPop;
       Close fawhp;
       Close fmcmp;
       Return scCancelC;

      /End-Free
     P EditAddWrk      E
       //*** End Add *** 10/25/12 ******************************************
       //===================================================================
       // ErrorAww - Error AWW History
       //===================================================================
     P ErrorAww        B
     D ErrorAww        PI              n
     d edtAww                          n
     D error           s               n   inz(*off)

     d kkEfDt          s                   Like(CMacDt)
      *** Begin Add *** 05/26/20 ***********************************************
     d CountPcr        s              5  0 Inz
     d CountEfdt       s              5  0 Inz
      *** End   Add *** 05/26/20 ***********************************************

     C     key1WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt
     C                   Kfld                    kkEfDt

     C     key2WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt

      /Free
       //*** Begin Add *** 05/26/20 ******************************************
       // Benefit type is required
       If scBenTyp = *blanks;
          msBenTyp = 'WCR0974';
          erBenTyp = *On;
          error=*On;
       Else;
          msBenTyp = *blanks;
          erBenTyp = *Off;
       Endif;

       //Effective date must be valid if entered
       mgAwwEfDtI=' ';
       erAwwEfDtI=*Off;
       If scAwwEfDtI<>0 And dtCheck(scAwwEfDtI)=0;
          mgAwwEfDtI='Invalid Date';
          erAwwEfDtI=*On;
          error=*On;
       Endif;

       //Effective date cannot be into the future.
       If dtCYMD(scAwwEfdtI) > Dateto7(SystemDate()) and error = *Off;
          mgAwwEfDtI = 'WCR4831';
          erAwwEfDti = *On;
          error = *On;
       Endif;

       //Effective date cannot already exist
       If scAwwEfDtI > *zero and error = *Off;
          CountEfdt = *Zeros;
          kkEfdt = dtCYMD(scAwwEfDtI);
          Setll (CMco#:CMfnd:CMfyr:CMcase:kkEfdt) paw04;
          If %equal(WMAWL4);
             Reade (CMco#:CMfnd:CMfyr:CMcase:kkEfdt) paw04;

             Dow not %eof (WMAWL4) and CountEfDt <=1;
                CountEfdt = CountEfdt + 1;
                reade (CMco#:CMfnd:CMfyr:CMcase:kkEfdt) paw04;
             Enddo;

             If svAwwEfDt = scAwwEfDtI;
                CountEfdt = CountEfdt - 1;
             Endif;

             If CountEfDt > *zeros;
                mgAwwEfDtI = 'WCR4832';
                erAwwEfDti = *On;
                error = *On;
             Endif;

          Endif;
       Endif;

        //Only one Primary Comp Rate is allowed.
       If scAwwPcr = 'Y';
          CountPcr = *zeros;
          Setll (CMco#:CMfnd:CMfyr:CMcase) paw06;
          If %equal(WMAWL6);
             reade (CMco#:CMfnd:CMfyr:CMcase) paw06;

             Dow not %eof (WMAWL6) and CountPcr <=1;
                CountPcr = CountPcr + 1;
                reade (CMco#:CMfnd:CMfyr:CMcase) paw06;
             Enddo;

             If svAwwPcr = 'Y';
                CountPcr = CountPcr - 1;
             Endif;

             If CountPcr > *zeros;
                mgAwwPcr = 'WCR4833';
                erAwwPcr= *On;
                error = *On;
             Else;
               mgAwwPcr = *blanks;
               erAwwPcr = *Off;
             Endif ;

          Endif;
       Endif;

       //*** End   Add *** 05/26/20 ******************************************


       If edtAww = *Off;
          mgAwwAmtI=' ';
          erAwwAmtI=*Off;
          If scAwwAmtI=0 and CMltFl='I';
             mgAwwAmtI='AWW can''t be zero since this is an ' +
              'Indemnity claim.';
             erAwwAmtI=*On;
             error=*On;
          EndIf;
       EndIf;

       mgCompRatI=' ';
       erCompRatI=*Off;

       If CMltFl='I';
          kkEfDt=CMacDt;
          SetGt key1WDRBL prb00;
          ReadPe key2WDRBL prb00;

          If not %Eof(WDRBL2);

             If scCompRatI > *Zero and scCompRatI <RBmnRt;
                mgCompRatI='WC30048';
                erCompRatI=*On;
                error=*On;
             EndIf;

             If error=*Off;
                If scCompRatI > *Zero and scCompRatI > RBpacp;
                   ExFmt rcdCompWdw;
                   If scCompCnl=*On;
                      error=*On;
                   EndIf;
                EndIf;
             EndIf;

          EndIf;
       EndIf;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorAww        E
       //===================================================================
       // ErrorBenefit - Error Benefit Information
       //===================================================================
     P ErrorBenefit    B
     D ErrorBenefit    PI              n
     D error           s               n   inz(*off)
      /Free

       //Start date must be valid
       mgBenStDtI=' ';
       erBenStDtI=*Off;
       If dtCheck(scBenStDtI)=0;
          mgBenStDtI='Invalid Date';
          erBenStDtI=*On;
          error=*On;
       Endif;

       //End date must be valid if entered
       mgBenEnDtI=' ';
       erBenEnDtI=*Off;
       If scBenEnDtI<>0 And dtCheck(scBenEnDtI)=0;
          mgBenEnDtI='Invalid Date';
          erBenEnDtI=*On;
          error=*On;
       Endif;

       //Dates must be ascending
       mgBenEnDtI=' ';
       erBenEnDtI=*Off;
       If scBenEnDtI<>0;
          If dtCYMD(scBenEnDtI) < dtCYMD(scBenStDtI);
             mgBenEnDtI='Dates must be in ascending order.';
             erBenEnDtI=*On;
             error=*On;
          EndIf;
       Endif;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorBenefit    E
       //===================================================================
       // ErrorDiag - Error Check Diagnosis Code
       //===================================================================
     P ErrorDiag       B
     D ErrorDiag       PI              n
     D error           s               n   inz(*off)
      /Free

       // Diagnosis code is Required
       mgDiagCd=' ';
       erDiagCd=*Off;
       //***** beg add ***** 04/20/15 ************************************
       mgDiagCd1 = ' ';
       erDiagCd1 = *Off;

       If readDiag <> *on;
       If @@ICDSts = '1';
       //***** end add ***** 04/20/15 ************************************
       If scDiagCd=*Blanks;
          mgDiagCd='Diagnosis Code is required.';
          erDiagCd=*On;
          error=*On;
       Else;
       EndIf;
       //***** beg add ***** 04/20/15 ************************************
       ElseIf @@ICDSts = '2';
       //***** beg chg ***** 12/01/15 ************************************
       //*****   If scDiagCd1 = *Blanks;
          If scDiagCd1 = *Blanks and *in81;
       //***** end chg ***** 12/01/15 ************************************
             mgDiagCd1 = 'Diagnosis Code is required.';
             erDiagCd1 = *On;
             error = *On;
          Endif;
       //***** beg add ***** 12/01/15 ************************************
          If scDiagCd2 = *Blanks and *in82;
             mgDiagCd2 = 'Diagnosis Code is required.';
             erDiagCd2 = *On;
             error = *On;
          Endif;
       //***** end add ***** 12/01/15 ************************************
       Endif;
       Endif;
       //***** end add ***** 04/20/15 ************************************

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorDiag       E
       //===================================================================
       // ErrorEarning - Error Earnings Information
       //===================================================================
     P ErrorEarning    B
     D ErrorEarning    PI              n
     D error           s               n   inz(*off)
      /Free

       //*** Beg Add *** 05/30/18 *****************************************
       // Reduced Earning Start Date must be valid if entered
       mgRedStrDt=' ';
       erRedStrDt=*Off;
       If scRedStrDt>0;
          If DtCheck(scRedStrDt) = 0;
             mgRedStrDt='Start Date Must be a Valid Date';
             erRedStrDt=*On;
             error=*On;
          EndIf;
       EndIf;
       // Reduced Earning End Date must be valid if entered
       mgRedEndDt=' ';
       erRedEndDt=*Off;
       If error = *Off and scRedEndDt>0;
          If DtCheck(scRedEndDt) = 0;
             mgRedEndDt='End Date Must be a Valid Date';
             erRedEndDt=*On;
             error=*On;
          EndIf;
       EndIf;
       // Start Date Must be Less Than or Equal to End Date
       mgRedStrD2=' ';
       erRedStrD2=*Off;
       If error = *Off and scRedStrDt>0 and scRedStrDt > scRedEndDt;
          mgRedStrD2='Start Date Must be <= End Date';
          erRedStrD2=*On;
          error=*On;
       EndIf;
       //*** End Add *** 05/30/18 *****************************************

       // # of Weeks is Required
       mgRedWeekI=' ';
       erRedWeekI=*Off;
       If scRedWeekI=0;
          mgRedWeekI='# of Weeks is Required';
          erRedWeekI=*On;
          error=*On;
       EndIf;

       If error=*On;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorEarning    E
       //===================================================================
       // ErrorMult - Error Multiple Settlement
       //===================================================================
     P ErrorMult       B
     D ErrorMult       PI              n
     d addMult                         n
     D error           s               n   inz(*off)
      /Free

       // Method of Settlement must be entered.
       mgMthSettI=' ';
       erMthSettI=*Off;
       If scMthSettI=*Blanks;
          mgMthSettI='Code must be entered.';
          erMthSettI=*On;
          error=*On;
       Else;
          If addMult=*On;
             Chain keyWDMSL pms00;
             If %Found(WDMSL);
                mgMthSettI='Duplicate Entry.';
                erMthSettI=*ON;
                error=*On;
             EndIf;
          EndIf;
       EndIf;

       // TPOC Date Must be valid if entered.
       erTpocDtI=*Off;
       If scTpocDtI<>*Zeros And dtCheck(scTpocDtI)=0;
          erTpocDtI=*On;
          error=*On;
       Endif;

       // Delay Date Must be valid if entered.
       erDlayDtI=*Off;
       If scDlayDtI<>*Zeros And dtCheck(scDlayDtI)=0;
          erDlayDtI=*On;
          error=*On;
       Endif;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorMult       E
       //===================================================================
       // ErrorPaid - Error Employer Paid Information
       //===================================================================
     P ErrorPaid       B
     D ErrorPaid       PI              n
     D error           s               n   inz(*off)
      /Free

       // Pmt Code Must be Entered
       mgPmtCodeI=' ';
       erPmtCodeI=*Off;
       If scPmtCodeI=*Blanks;
          mgPmtCodeI='Payment Code is Required.';
          erPmtCodeI=*On;
          error=*On;
       EndIf;

       // Total Paid is Required
       mgTotPaidI=' ';
       erTotPaidI=*Off;
         //*** Begin Del *** 09/02/13 ***************************
         //* If scTotPaidI=*Zeros;
         //*    mgTotPaidI='Total Paid is Required.';
         //*    erTotPaidI=*On;
         //*    error=*On;
         //* EndIf;
         //*** End   Del *** 09/02/13 ***************************

       //Start date must be valid
       mgEmStrDtI=' ';
       erEmStrDtI=*Off;
       If dtCheck(scEmStrDtI)=0;
          mgEmStrDtI='Invalid Date';
          erEmStrDtI=*On;
          error=*On;
       Endif;

       //End date must be valid if entered
       mgEmEndDtI=' ';
       erEmEndDtI=*Off;
       If scEmEndDtI<>0 And dtCheck(scEmEndDtI)=0;
          mgEmEndDtI='Invalid Date';
          erEmEndDtI=*On;
          error=*On;
       Endif;

       //Dates must be ascending
       mgEmEndDtI=' ';
       erEmEndDtI=*Off;
       If scEmEndDtI<>0;
          If dtCYMD(scEmEndDtI) < dtCYMD(scEmStrDtI);
             mgEmEndDtI='Dates must be in ascending order.';
             erEmEndDtI=*On;
             error=*On;
          EndIf;
       Endif;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorPaid       E
       //===================================================================
       // ErrorPart - Error Check Body Part
       //===================================================================
     P ErrorPart       B
     D ErrorPart       PI              n
     D error           s               n   inz(*off)
      /Free

       // Body Code must be entered.
       mgPartCd=' ';
       erPartCd=*Off;
       If scPartCd=*Blanks;
          mgPartCd='Code must be entered.';
          erPartCd=*On;
          error=*On;
       EndIf;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorPart       E
       //===================================================================
       // ErrorPhys - Error Check Treating Physician
       //===================================================================
     P ErrorPhys       B
     D ErrorPhys       PI              n
     D error           s               n   inz(*off)
     D provName        s                   like(scPhyNameI)
     D ##type          s              1
      /Free

       // Must enter either Fein/Mod or Name
       mgPhyNameI=' ';
       erPhyNameI=*Off;
       erPhyFeI=*Off;
       erPhyNameI=*Off;
       If scPhyFeI=*Zeros And scPhyNameI=*Blanks;
          mgPhyNameI='Must enter either FEIN/Mod or a name.';
          erPhyNameI=*On;
          erPhyFeI=*On;
          erPhyNameI=*On;
          error=*On;
       EndIf;

       //From date must be valid
       mgFrDtI=' ';
       erFrDtI=*Off;
       If dtCheck(scFrDtI)=0;
          mgFrDtI='Invalid Date';
          erFrDtI=*On;
          error=*On;
       Endif;

       //Fein and Name can't be entered unless the fein name matches
       //the name entered
       mgPhyNameI=' ';
       erPhyNameI=*Off;
       taxID#=scPhyFeI;
       kkFein=fein;
       KKMod=modifier;
       Chain keyWMPDL ppd00;
       If %Found(WMPDL);
           If PDfsn <> *Blanks;
              provName=PDname + ', ' + PDfsn;
           Else;
              provName=PDname;
           EndIf;
           If scPhyNameI<>provName and scphyNameI<>*Blanks;
              mgPhyNameI='Fein and Name can''t be both be entered.';
              erPhyNameI=*On;
              error=*On;
           EndIf;
       EndIf;

       //To date must be valid if entered
       mgToDtI=' ';
       erToDtI=*Off;
       If scToDtI<>0 And dtCheck(scToDtI)=0;
          mgToDtI='Invalid Date';
          erToDtI=*On;
          error=*On;
       Endif;

       //Dates must be ascending
       mgToDtI=' ';
       erToDtI=*Off;
       If scToDtI<>0;
          If dtCYMD(scToDtI) < dtCYMD(scFrDtI);
             mgToDtI='Dates must be in ascending order.';
             erToDtI=*On;
             error=*On;
          EndIf;
       Endif;

       // Check Phone#
       ##type = 'P';
       erPhyPhnI = Check_Phone#(scPhyPhnI:##type:msPhyPhnI);
       If erPhyPhnI = *on;
          error = *on;
       Endif;

       ##type = 'F';
       erPhyFaxI = Check_Phone#(scPhyFaxI:##type:msPhyFaxI);
       If erPhyFaxI = *on;
          error = *on;
       Endif;
       // **** Beg Changes 07/02/15************** //

       // Validate Treating Physician EmailId
       erPhyEmAd = *off;
       If %Len(%trim(scPhysEmAd)) > 0;
          erPhyEmAd = Check_Email(scPhysEmAd:mgPhyEmAd);
          If erPhyEmAd = *On;
             error = *on;
          EndIf;
       EndIf;
       // **** End Changes 07/02/15************** //

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorPhys       E
       //*** Beg Add ***** 12/06/22 ******************************************
       //===================================================================
       // ErrorSettlement - Error Settlement Information
       //===================================================================
     P ErrorSettlement...
     P                 B
     D ErrorSettlement...
     D                 PI              n
     D addSettle                       n
     D error           s               n   inz(*off)

       // Current Authority or Current Demand from Opposing Council must be entered.
       mgCurrAuth = ' ';
       erCurrAuth = *Off;
       mgDFrmOpp = ' ';
       erDFrmOpp = *Off;
       If scCurrAuth = *Zeros and scDFrmOpp = *Zeros;
          mgCurrAuth = 'Current Authority or Current Demand from Opposing ' +
           'Council must be entered.';
          mgDFrmOpp = 'Current Authority or Current Demand from Opposing ' +
           'Council must be entered.';
          erCurrAuth = *On;
          erDFrmOpp = *On;
          error=*On;
       EndIf;

       mgAAprDt = ' ';
       erAAprDt = *Off;
       If scAAprDt = *Zeros and scCurrAuth <> 0 or
        scCurrAuth = 0 and scAAprDt <> *Zeros;
          //Date Authority Approved is required if Current Authority amount is entered.
          If scAAprDt = *Zeros and scCurrAuth <> 0;
             mgAAprDt = 'Date Authority Approved is required if Current ' +
              'Authority amount is entered.';
             erAAprDt = *On;
             error=*On;
          EndIf;
          // Current Authority amount is required if Date Authority Approved is entered
          If scCurrAuth = 0 and scAAprDt <> *Zeros;
             mgCurrAuth = 'Current Authority amount is required if ' +
              'Date Authority Approved is entered.';
             erCurrAuth = *On;
             error=*On;
          Endif;
       Else;
          If scAAprDt <> *Zeros And dtCheck(scAAprDt)=0;
             mgAAprDt='Invalid Date';
             erAAprDt=*On;
             error=*On;
          Endif;
       Endif;

       mgDtOfDmnd = ' ';
       erDtOfDmnd = *Off;
       //Date of Demand is required if Current Demand from Opposing Council Amount is entered
       If scDtOfDmnd = *Zeros and scDFrmOpp <> 0 or
        scDFrmOpp = 0 and scDtOfDmnd <> *Zeros;
          If scDtOfDmnd = *Zeros and scDFrmOpp <> 0;
             mgDtOfDmnd = 'Date of Demand is required if Current Demand from ' +
              'Opposing Council is entered.';
             erDtOfDmnd = *On;
             error=*On;
          Endif;
       // Current Demand from Opposing Counsil is required if Date of Demand is entered
          If scDFrmOpp = 0 and scDtOfDmnd <> *Zeros;
             mgDFrmOpp = 'Current Demand from Opposing Council is required ' +
              'if Date of Demand is entered.';
             erDFrmOpp = *On;
             error=*On;
          Endif;
       Else;
          If scDtOfDmnd <> *Zeros And dtCheck(scDtOfDmnd)=0;
             mgDtOfDmnd='Invalid Date';
             erDtOfDmnd=*On;
             error=*On;
          Endif;
       Endif;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

     P ErrorSettlement...
     P                 E
      //*** End Add ***** 12/06/22 ******************************************
       //===================================================================
       // ErrorTrial - Error Trial Information
       //===================================================================
     P ErrorTrial      B
     D ErrorTrial      PI              n
     D addTrial                        n
     D error           s               n   inz(*off)
      /Free

       // Suit Date Must be entered.
       mgSuitDtI=' ';
       erSuitDtI=*Off;
       If dtCheck(scSuitDtI)=0;
          mgSuitDtI='Invalid Date';
          erSuitDtI=*On;
          error=*On;
       Endif;

       // Trial Type must be entered.
       mgTrTypeI=' ';
       erTrTypeI=*Off;
       If scTrTypeI=*Blanks;
          mgTrTypeI='Trial Type must be entered.';
          erTrTypeI=*On;
          error=*On;
       EndIf;

       // Judge must be entered.
       mgJudgeI=' ';
       erJudgeI=*Off;
       If scJudgeI=*Blanks;
          mgJudgeI='Judge must be entered.';
          erJudgeI=*On;
          error=*On;
       EndIf;

       // Court level must be entered.
       mgCrtLvlI = ' ';
       erCrtLvlI = *Off;
       If scCrtLvlI = *Blanks;
          mgCrtLvlI = 'Court Level must be entered.';
          erCrtLvlI = *On;
          error = *On;
       EndIf;

       // Legal Issue must be entered.
       mgLegIssI=' ';
       erLegIssI=*Off;
       If scLegIssI=*Blanks;
          mgLegIssI='Legal Issue must be entered.';
          erLegIssI=*On;
          error=*On;
       EndIf;

       // Check for Duplicates
       mgSuitDtI=' ';
       erSuitDtI=*Off;
       If error=*Off and addTrial=*On;
          Chain keyWMTIL pti00;
          If %Found(WMTIL);
             mgSuitDtI='Duplicate Entry.';
             erSuitDtI=*ON;
             error=*On;
          EndIf;
       EndIf;

       // Verdict Date must be valid when entered.
       mgVerdDtI=' ';
       erVerdDtI=*Off;
       If scVerdDtI<>*Zeros And dtCheck(scVerdDtI)=0;
          mgVerdDtI='Invalid Date';
          erVerdDtI=*On;
          error=*On;
       Endif;

       // Verdict and Verdict Date are mutually inclusive.
       mgVerdictI=' ';
       erVerdictI=*Off;
       If (scVerdictI = *Blanks and scVerdDtI <> *zeros and erVerdDtI = *Off)
        or (scVerdictI <> *Blanks and scVerdDtI = *zeros);
          mgVerdictI='Verdict and Verdict Date are mutually inclusive.';
          mgVerdDtI ='Verdict and Verdict Date are mutually inclusive.';
          erVerdictI=*On;
          erVerdDtI =*On;
          error=*On;
       EndIf;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorTrial      E
       //===================================================================
       // ErrorWitness - Error Witness Information
       //===================================================================
     P ErrorWitness    B
     D ErrorWitness    PI              n
     D error           s               n   inz(*off)
     D ##type          s              1
      /Free

       ##type = 'P';
       erBusPhone = Check_Phone#(scBusPhone:##type:msBusPhone);
       If erBusPhone = *on;
          error = *on;
       Endif;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorWitness    E
       //===================================================================
       // ErrorWork - Error Work History
       //===================================================================
     P ErrorWork       B
     D ErrorWork       PI              n
     D error           s               n   inz(*off)
     D dateValueIn     s              8  0
     D errorMsg        s             50
     D errorMsgId      s             10
      /Free

       // Work History Date is Required
       mgWorkDtI=*Blanks;
       erWorkDtI=*Off;
       dateValueIn=scWorkDtI;
       If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
          mgWorkDtI=errorMsg;
          erWorkDtI=*On;
          error=*On;
       EndIf;

       //******** beg add ******* 04/17/15 *************************************
       If CMltfl <> 'M';
       //******** end add ******* 04/17/15 *************************************
       //********* beg add ******** 08/13/14 **********************************
       // Work History Date should be greater than actual RTW
       If erWorkDtI = *off and CMrtw > 0 and dtCYMD(scWorkDtI) < CMrtw;
          mgWorkDtI = 'Work Date should be greater than Actual RTW Date.';
          erWorkDtI = *On;
          error = *On;
       EndIf;
       //********* end add ******** 08/13/14 **********************************
       //******** beg add ******* 04/17/15 *************************************
       Endif;
       //******** end add ******* 04/17/15 *************************************

       // Work History Status is Required.
       mgWorkStsI=' ';
       erWorkStsI=*Off;
       If scWorkStsI=*Blanks;
          mgWorkStsI='Work History Status is required.';
          erWorkStsI=*On;
          error=*On;
       EndIf;

       //*** Beg Add *** 05/30/18 ******************************************
       // Date Last Day Worked Must be Valid
       If scLastDay <> *Zeros;
          mgLastDay=*Blanks;
          erLastDay=*Off;
          dateValueIn=scLastDay;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgLastDay=errorMsg;
             erLastDay=*On;
             error=*On;
          EndIf;
       EndIf;
       // Date Employer had Knowledge of Disablity must be valid
       If scEmpDisDt <> *Zeros;
          mgEmpKnow=*Blanks;
          erEmpKnow=*Off;
          dateValueIn=scEmpDisDt;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgEmpKnow=errorMsg;
             erEmpKnow=*On;
             error=*On;
          EndIf;
       EndIf;
       // Date Admin had Knowledge of Disablity must be valid
       If scAdmDisDt <> *Zeros;
          mgAdmKnow=*Blanks;
          erAdmKnow=*Off;
          dateValueIn=scAdmDisDt;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgAdmKnow=errorMsg;
             erAdmKnow=*On;
             error=*On;
          EndIf;
       EndIf;
       //*** End Add *** 05/30/18 ******************************************

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorWork       E
       //*** Beg Add *** 10/25/12 *********************************************
       //===================================================================
       // ErrorWrk - Error Work History for CSI
       //===================================================================
     P ErrorWrk        B
     D ErrorWrk        PI              n
     D addWork                         n
     D edtWork                         n
     D error           s               n   inz(*off)
     D dateValueIn     s              8  0
     D errorMsg        s             50
     D errorMsgId      s             10
      /Free

       msWrkFilC='FSMESG';
       //Work History Date is Required
       msWrkDtC=*Blanks;
       erWrkDtC=*Off;
       dateValueIn=scWrkDt;
       If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
          msWrkFilD='WMESGG';
          msWrkDtC=errorMsgId;
          erWrkDtC=*On;
          error=*On;
       Endif;

       dateValueIn=dtCYMD(scWrkDt);
       If error <> *On;
          msWrkFilD='FSMESG';
          If dateValueIn < CMacdt;
             msWrkDtC='MCR0572';
             erWrkDtC=*On;
             error=*On;
          Endif;

          Open fawhl;
          If addWork = *On;
             kkWkdt = dateValueIn;
             Chain keyFawhl pwh00;
             If %found(Fawhl) and error <> *On;
                msWrkDtC='MCR0900';
                erWrkDtC=*On;
                error=*On;
             Endif;
          Endif;

          If edtWork = *On;
             If dateValueIn <> isWrkDt;
                kkWkdt = dateValueIn;
                Chain keyFAWHL pwh00;
                If %found(fawhl) and error <> *On;
                    msWrkDtC='MCR0900';
                    erWrkDtC=*On;
                    error=*On;
                Endif;
             Endif;
          Endif;
          Close fawhl;
       Endif;

       // Check for Valid Work Code.
       msWrkCode=' ';
       erWrkCode=*Off;
       If scWrkCode = *Blanks;
             msWrkCode='MCR2434';
             erWrkCode=*On;
             error=*On;
          Endif;

       //Make sure that LDW, LWP and RTW, LWR are entered in correct order.
       //LDW - No duty starts
       //RTW - No duty ends
       //LWP - Light duty starts
       //LWR - Light duty ends

       If scWrkCode = 'LDW' or scWrkCode = 'LWP'
        or scWrkCode = 'RTW' or scWrkCode = 'LWR';
          kkWkdt = dateValueIn ;
          Open fawhl;
          Setll keyFAWHL pwh00;
          Reade KeyFAWHL1 pwh00;
          Dow not %eof(FAWHL);
             If whCode = 'LDW' or whCode = 'LWP' or whCode = 'RTW'
              or whCode = 'LWR';
                If addWork = *Off and WHwkdt<>dateValueIn;
                   Leave;
                Endif;
                If addWork = *On;
                   Leave;
                Endif;
             Endif;
             Reade keyFAWHL1 pwh00;
          Enddo;
          If %eof(FAWHL);
           //If there are previous entries then must use  a start code.
             If (scWrkCode='RTW' or scWrkCode='LWR') and error <> *On;
                msWrkCode='MC40332';
                erWrkCode=*On;
                error=*On;
             Endif;
          Else;
           //If the preious code is start no duty code then stop no duty code is required.
             If WHcode='LDW' and scWrkCode<>'RTW' and error <> *On;
                msWrkCode='MC40333';
                erWrkCode=*On;
                error=*On;
             Endif;
           //If the previous code is start light duty code then
           //the stop light duty code is required.
             If WHcode='LWP' and scWrkCode<>'LWR' and error <> *On;
                msWrkCode='MC40334';
                erWrkCode=*On;
                error=*On;
             Endif;
           //If previous code is a stop code then only a start code can be entered.
             If WHcode = 'RTW' or WHcode = 'LWR';
                If (scWrkCode = 'RTW' or scWrkCode = 'LWR') and error <> *On;
                   msWrkCode='MC40332';
                   erWrkCode=*On;
                   error=*On;
                Endif;
             Endif;
          Endif;
          Close fawhl;

        //Check next code if there is one.
          If error=*Off;

             Open fawhl;
             Setgt keyFAWHL pwh00;
             Readpe KeyFAWHL1 pwh00;
             Dow not %eof(FAWHL);
                If scWrkCode = 'LDW' or scWrkCode = 'LWP' or scWrkCode = 'RTW'
                 or scWrkCode = 'LWR';
                   If addWork = *Off and WHwkdt<>dateValueIn;
                      Leave;
                   Endif;
                   If addWork = *On;
                      Leave;
                   Endif;
                Endif;
                Readpe keyFAWHL1 pwh00;
             Enddo;
             If not %eof(FAWHL);
              //If the next code is a stop no duty code, then the start no duty
              //code is required.
                If WHcode='RTW' and scWrkCode<>'LDW';
                   msWrkCode='MC40335';
                   erWrkCode=*On;
                   error=*On;
                Endif;

              //If the next code is a stop light duty code, then
              //the start light duty code is required.
                If WHcode='LWR' and scWrkCode<>'LWP';
                   msWrkCode='MC40336';
                   erWrkCode=*On;
                   error=*On;
                Endif;

              //If the next code is a start code then only a stop
              //code can be entered.
                If WHcode='LDW' or WHcode='LWP';
                   If scWrkCode='LDW' or scWrkCode='LWP';
                      msWrkCode='MC40337';
                      erWrkCode=*On;
                      error=*On;
                   Endif;
                Endif;
             Endif;
             Close fawhl;
          Endif;
       Endif;
       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorWrk        E
       //*** End Add *** 10/25/12 *********************************************
       //===================================================================
       // GetAmtCrit - Get Darts Amount Criteria
       //===================================================================
     P GetAmtCrit      B                   Export
     D GetAmtCrit      PI            11  2
     d dartsKeyIn                    10
     d dartsKey        s             10
     d amtCrit         s             11  2 Inz(0)
     d i               s              3  0

     C     keyWMADL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    dartsKey
      /Free

       dartsKey=dartsKeyIn;
       Chain keyWMADL pad00;
       If %Found(WMADL);
          DoU %Eof(WMADL);
             If ADstat<>'I';
                i=%LookUp(ADreCd:scRsvCode);
                If i<>0;
                   amtCrit=ADamt;
                EndIf;
             EndIf;
             ReadE keyWMADL pad00;
          EndDo;
       EndIf;

       Return amtCrit;

      /end-Free
     P GetAmtCrit      E
       //===================================================================
       // GetRequestAmt - Get the total request amount
       //===================================================================
     P GetRequestAmt   B                   Export
     D GetRequestAmt   PI             9  2
     d rsvCdIn                        2
      ****** beg add ****** 12/01/14 ***************************************
     d clmRrn#                        9  0 const options(*nopass)
      ****** end add ****** 12/01/14 ***************************************

     d rsvCd           s              2
     dreqAmt           s              9  2 Inz(0)

     C     keyWARPL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase
      /Free
       rsvCd=rsvCdIn;

       //****** beg add ****** 12/01/14 ***************************************
       If %parms > 1;
          Chain(n) clmRrn# pcm0p;
       Endif;
       //****** end add ****** 12/01/14 ***************************************

       Chain keyWARPL prp00;
       If %Found(WARPL);
          DoU %Eof(WARPL);
             If RPstat=*Blanks And RPreCd=rsvCd;
                reqAmt=reqAmt+RPamt;
             EndIf;
             Reade keyWARPL prp00;
          EndDo;
       EndIf;

       Return reqAmt;

      /end-Free
     P GetRequestAmt   E
      *================================================================
      *InzErrInd  =  Initialize the Error Indicators
      *================================================================
     P InzErrInd       B
     D InzErrInd       PI
      /Free

       Reset erRsvCode;
       Reset erRsvChg;
       erExRs     = *off;
       isError    = *off;

      /end-Free
     P InzErrInd       E
       //+++++==============================================================
       // SelChgRes - User selected to change the Reserves
       //==========++++=====================================================
     P SelChgRes       B                   Export
     D SelChgRes       PI
     D clmRrnIn                       9  0
     D statusIn                       1
     D clmRrn          s              9  0
     D status          s              1
      /Free

       Open  wpiClmPop;
       clmRrn=clmRrnIn;
       status=statusIn;
       InzErrInd();
       Clear rcdChgRsv;
      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = clmRrnIn;
      //**** End Add  ***  06/23/21  ****************************************
       DoU  isError = *Off;
         Exfmt rcdChgRsv;
         Select;
         When SaveCR = *on;
          ValidateRsv(clmRrn:status);
          If isError = *Off;
            // MoveScDb();
            UpdateClmRsv(clmRrn);
            WriteRsvHst();
            Leave;
          Endif;
         When CancelCR = *on;
          InzErrInd();
          leave;
         EndSl;
         //****** beg add ******** 12/01/14 *****************************
         If isWrkSht = *on;
            isWrkSht = *off;
            InzErrInd();
            leave;
         Endif;
         //****** end add ******** 12/01/14 *****************************
       Enddo;
       Close wpiClmPop;

      /End-free
     P SelChgRes       E
       //===================================================================
       // UpdAddAww - Update/Edit AWW History
       //===================================================================
     P UpdAddAww       B
     D UpdAddAww       PI
     d claimRRN                       9  0
     d awwRRN                         9  0
     d addAww                          n
     d edtAww                          n

     d kkEfDt          s                   Like(CMacDt)
      /Free

       If edtAww=*On;
          Chain awwRRN paw0p;
       EndIf;

       If addAww=*On;
          AWco#=CMco#;
          AWfnd=CMfnd;
          AWfyr=CMfyr;
          AWcase=CMcase;
          AWstat='A';
       Endif;

       AWbr2 = scBenTyp;
       AWtrDt=dtCYMD(scTranDtI);
       AWaww=scAwwAmtI;

       If scCompRatI=0;
          AWcrt=scCalcRatI;
       Else;
          AWcrt=scCompRatI;
       EndIf;
       //*** Beg Add *** 10/10/24 **********************************
       AWcalc=scCalcRatI;
       //*** Eng Add *** 10/10/24 **********************************

       AWpcr = scAwwpcr;
       AWefDt=dtCYMD(scAwwEfDtI);

       AWuser=q1User;
       AWdt=DateTo7(SystemDate());
       AWtime=TimeTo6(SystemTime());
       AWpgm=q1Pgm;

       If edtAww=*On;
          Update paw0p;
       EndIf;

       If addAww=*On;
          Write paw0p;
       EndIf;

       //Update Claim Master
       Chain claimRRN pcm0p;
       If %Found(WMCMP);

       //Update based on Primary Comp Rate
          If scAwwPcr = 'Y';
             CMaww=AWaww;
             CMcrt=AWcrt;
             //*** Beg Add *** 10/10/24 *****************************
             CMcalc = AWcalc;
             //*** Eng Add *** 10/10/24 *****************************
          Else;
       // Update not based on Primary Comp Rate
             Setll (CMco#:CMfnd:CMfyr:CMcase) paw06;
             If not %equal(WMAWL6);
                  Exsr GetPcr;
             Endif;
          Endif;

       //*** Begin Add *** 11/13/20 **********************************
       //Load Pre-Injury Wage if not previously loaded.

          If addAww = *On and CMprWW = *Zero;
             chain (CMco#:CMfnd) PCX02;
             If %found(WMCXP2);
                If CXloadAww = 'Y';
                   CMprWW = scAwwAmtI;
                Endif;
             Endif;
          Endif;
       //*** End   Add *** 11/13/20 **********************************

          CMuser=q1User;
          CMpgm=q1Pgm;
          CMtime=TimeTo6(SystemTime());
          CMdt=DateTo7(SystemDate());
          Update pcm0p;
       EndIf;

       //===================================================================
       // GetPcr - Get/Calculate Primary Comp Rate
       //===================================================================
       Begsr GetPcr;
       kkEfdt = DateTo7(SystemDate());
       Setgt (CMco#:CMfnd:CMfyr:CMcase:kkEfdt) paw04;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) paw04;

        If not %eof (WMAWL4) and AWefdt > *Zero;
           CMaww = AWaww;
           CMcrt = AWcrt;
           //*** Beg Add *** 10/10/24 *****************************
           CMcalc = AWcalc;
           //*** Eng Add *** 10/10/24 *****************************
        Else;
           If AddAww = *On;
              Setgt (CMco#:CMfnd:CMfyr:CMcase:kkEfdt) paw01;
              Readpe (CMco#:CMfnd:CMfyr:CMcase) paw01;
              If not %eof(WMAWL1);
                 CMaww = AWaww;
                 CMcrt = AWcrt;
                 //*** Beg Add *** 10/10/24 ***************************
                 CMcalc = AWcalc;
                 //*** Eng Add *** 10/10/24 ***************************
              Endif;
           Endif;
        Endif;
       Endsr;
       //** End Add  *** 05/26/20 ***********************************************
      /End-Free
     P UpdAddAww       E
       //===================================================================
       // UpdAddBenefit - Update/Edit Benefit Information
       //===================================================================
     P UpdAddBenefit   B
     D UpdAddBenefit   PI
     d benefitRRN                     9  0
     d addBenefit                      n
     d edtBenefit                      n
     d delBenefit                      n
      /Free
       If addBenefit=*On;
          BNco#=CMco#;
          BNfnd=CMfnd;
          BNfyr=CMfyr;
          BNcase=CMcase;
       EndIf;
       If edtBenefit=*On or delBenefit=*On;
          Chain benefitRRN pbn0p;
       EndIf;
       If delBenefit=*On;
          Delete pbn0p;
       Else;
          BNstDt=dtCYMD(scBenStDtI);
          BNaCod=scAdjCodeI + scPmtAdCdI;
          BNamt=scBenAmtI;
          BNenDt=dtCYMD(scBenEnDtI);
       //*** Beg Add *** 05/30/18 *******************************
          BNwIaO=scOffAmt;
       //*** End Add *** 05/30/18 *******************************
          BNuser=q1User;
          BNpgm=q1Pgm;
          BNtime=TimeTo6(SystemTime());
          BNdt=DateTo7(SystemDate());
          BNstat=scBenStsI;
          If edtBenefit=*On;
             Update pbn0p;
          EndIf;
          If addBenefit=*On;
             Write pbn0p;
          EndIf;
       EndIf;
      /End-Free
     P UpdAddBenefit   E
       //===================================================================
       // UpdAddDiag - Update/Edit Diagnosis Code
       //===================================================================
     P UpdAddDiag      B
     D UpdAddDiag      PI
     d diagRRN                        9  0
     d addDiag                         n
     d edtDiag                         n
      *** Beg Add ********* 06/03/13 ********************
     d deleteDiag                      n
      *** End Add ********* 06/03/13 ********************
      /Free
       If addDiag=*On;
          kkSeq#=*Hival;
          SetGt key2SADAL pda00;
          ReadPE key1SADAL pda00;
          If %Eof(SADAL);
             DAseq#=1;
          Else;
             DAseq#=DAseq#+1;
          EndIf;
          DAco#=CMco#;
          DAsub#=0;
          DAfnd=CMfnd;
          DAfyr=CMfyr;
          DAcase=CMcase;
          DASTAT='A';
          DAiUsr=q1User;
          DAiPgm=q1Pgm;
          DAiTim=TimeTo6(SystemTime());
          DAiDt=DateTo7(SystemDate());
       EndIf;
       If edtDiag=*On;
          Chain diagRRN pda0p;
       EndIf;
       //****** beg chg ******* 04/20/15 *****************************
       // DAicd9=%Subst(scDiagCd:1:5);
       // DAlosT=%Subst(scDiagCd:6:1);
       //*** Beg Delete ****  12/01/15 *******************************
       //*** If @@ICDSts = '1';
       //***   DAicd9 = %Subst(scDiagCd:1:5);
       //***   DAlosT = %Subst(scDiagCd:6:1);
       //***   DAICD1001 = ' ';
       //***   DAicdInd  = ' ';
       //*** ElseIf @@ICDSts = '2' ;
       //***   DAICD1001 = %Subst(scDiagCd1:1:7);
       //***   DAlosT = %Subst(scDiagCd1:8:1);
       //***   DAicd9 = ' ';
       //***   DAicdInd = 'Y';
       //*** Endif;
       //**** End Delete ****  12/01/15 *******************************
       //*** Beg Add     ****  12/01/15 *******************************
       Select;
         When @@ICDSts = '1';
              DAicd9 = %Subst(scDiagCd:1:5);
              DAlosT = %Subst(scDiagCd:6:1);
              DAICD1001 = ' ';
              DAicdInd  = ' ';
         When @@ICDSts = '2' and *in81;
              DAICD1001 = %Subst(scDiagCd1:1:7);
              DAlosT = %Subst(scDiagCd1:8:1);
              DAicd9 = ' ';
              DAicdInd = 'Y';
         When @@ICDSts = '2' and *in82;
              DAICD1001 = %Subst(scDiagCd2:1:7);
              DAlosT = %Subst(scDiagCd2:8:1);
              DAicd9 = ' ';
              DAicdInd = 'Y';
       Endsl;
       //*** End Add     ****  12/01/15 *******************************
       //****** end chg ******* 04/20/15 *****************************
       DAiLoc=scLoca;
       DAiSit=scSite;
       DAiSts=scStat;
       DAisDt=dtCYMD(scFrDate);
       DAexDt=dtCYMD(scToDate);
       DAprim=scPrim;
       DAuser=q1User;
       DApgm=q1Pgm;
       DAtime=TimeTo6(SystemTime());
       DAdt=DateTo7(SystemDate());
       If edtDiag=*On;
          Update pda0p;
       EndIf;
       If addDiag=*On;
          Write pda0p;
       EndIf;
       // *** Beg Add ********* 06/03/13 ********************
       If deleteDiag=*On;
          Chain diagRRN pda0p;
          Delete pda0p;
       EndIf;
       // *** End Add ********* 06/03/13 ********************
      /End-Free
     P UpdAddDiag      E
       //===================================================================
       // UpdAddBenefit - Update/Edit Earnings Information
       //===================================================================
     P UpdAddEarning   B
     D UpdAddEarning   PI
     d earningRRN                     9  0
     d addEarning                      n
     d edtEarning                      n
     d delEarning                      n
      /Free
       If addEarning=*On;
          R3co#=CMco#;
          R3fnd=CMfnd;
          R3fyr=CMfyr;
          R3case=CMcase;
       EndIf;
       If edtEarning=*On or delEarning=*On;
          Chain earningRRN pr30p;
       EndIf;
       If delEarning=*On;
          Delete pr30p;
       Else;
          // Populate Fields
          R3reWk=scRedWeekI;
          R3aRe=scAcRedAmI;
          R3dRe=scDmRedAmI;
       //*** Beg Add *** 05/30/18 ***************************************
          R3reSd=dtYyMd(scRedStrDt);
          R3reEd=dtYyMd(scRedEndDt);
          R3reWa=scNetAmt;
       //*** End Add *** 05/30/18 ***************************************
          If edtEarning=*On;
             Update pr30p;
          EndIf;
          If addEarning=*On;
             Write pr30p;
          EndIf;
       EndIf;
      /End-Free
     P UpdAddEarning   E
       //===================================================================
       // UpdAddMult - Update/Edit Multiple Settlement
       //===================================================================
     P UpdAddMult      B
     D UpdAddMult      PI
     d multRRN                        9  0
     d addMult                         n
     d edtMult                         n
      /Free
       If addMult=*On;
          MSco#=CMco#;
          MSfnd=CMfnd;
          MSfyr=CMfyr;
          MScase=CMcase;
          MSSTAT='A';
          MSiUsr=q1User;
          MSiPgm=q1Pgm;
          MSiTim=TimeTo6(SystemTime());
          MSiDt=DateTo7(SystemDate());
       EndIf;
       If edtMult=*On;
          Chain multRRN pms0p;
       EndIf;
       MScaMs=scMthSettI;
       MStpoA=scTpocAmtI;
       MStpoD=dtCYMD(scTpocDtI);
       MSdlyD=dtCYMD(scDlayDtI);
       MSuser=q1User;
       MSpgm=q1Pgm;
       MStime=TimeTo6(SystemTime());
       MSdt=DateTo7(SystemDate());
       If edtMult=*On;
          Update pms0p;
       EndIf;
       If addMult=*On;
          Write pms0p;
       EndIf;
      /End-Free
     P UpdAddMult      E
       //===================================================================
       // UpdAddPaid - Update/Edit Employer Paid Information
       //===================================================================
     P UpdAddPaid      B
     D UpdAddPaid      PI
     d paidRRN                        9  0
     d addPaid                         n
     d edtPaid                         n
     d delPaid                         n
      /Free
       If addPaid=*On;
          A3co#=CMco#;
          A3fnd=CMfnd;
          A3fyr=CMfyr;
          A3case=CMcase;
       EndIf;
       If edtPaid=*On or delPaid=*On;
          Chain paidRRN pa30p;
       EndIf;
       If delPaid=*On;
          Delete pa30p;
       Else;
          A3btc=scPmtCodeI;
          A3paid=scTotPaidI;
          A3stDt=dtYYMD(scEmStrDtI);
          A3enDt=dtYYMD(scEmEndDtI);
          A3user=q1User;
          A3pgm=q1Pgm;
          A3time=TimeTo6(SystemTime());
          A3dt=DateTo7(SystemDate());
          If edtPaid=*On;
             Update pa30p;
          EndIf;
          If addPaid=*On;
             A3iUsr=q1User;
             A3iPgm=q1Pgm;
             A3iTim=TimeTo6(SystemTime());
             A3iDt=DateTo7(SystemDate());
             Write pa30p;
          EndIf;
       EndIf;
      /End-Free
     P UpdAddPaid      E
       //===================================================================
       // UpdAddPart - Update/Edit Body Part
       //===================================================================
     P UpdAddPart      B
     D UpdAddPart      PI
     d partRRN                        9  0
     d addPart                         n
     d edtPart                         n
     d***** Beg Add *** 10/30/19 ********************************
     d delPart                         n
     d***** End Add *** 10/30/19 ********************************
      /Free
       If addPart=*On;
          kkSeq#=*Hival;
          SetGt key2WMBYL pby00;
          ReadPE key1WMBYL pby00;
          If %Eof(WMBYL);
             BYseq#=1;
          Else;
             BYseq#=BYseq#+1;
          EndIf;
          BYco#=CMco#;
          BYfnd=CMfnd;
          BYfyr=CMfyr;
          BYcase=CMcase;
          BYSTAT='A';
       EndIf;
       If edtPart=*On;
          Chain partRRN pby0p;
       EndIf;
       BYkey=scPartCd;
       BYpct=scImpPct;
       //*** Beg Add *** 05/01/18 **********************************
       BYiMlC=scLoc;
       //*** End Add *** 05/01/18 **********************************
       BYuser=q1User;
       BYpgm=q1Pgm;
       BYtime=TimeTo6(SystemTime());
       BYdt=DateTo7(SystemDate());
       If edtPart=*On;
          Update pby0p;
       EndIf;
       If addPart=*On;
          Write pby0p;
       EndIf;
       //***** Beg Add *** 10/30/19 *****************************
       If delPart = *On;
          Chain partRRN pby0p;
          Delete pby0p;
       Endif;
       //***** Beg Add *** 10/30/19 *****************************
      /End-Free
     P UpdAddPart      E
       //===================================================================
       // UpdAddPhys - Update/Edit Treating Physician
       //===================================================================
     P UpdAddPhys      B
     D UpdAddPhys      PI
     d physRRN                        9  0
     d addPhys                         n
     d edtPhys                         n
     d delPhys                         n
      /Free
       If addPhys=*On;
          kkSeq#=*Hival;
          SetGt key2WMTPL ptp00;
          ReadPE key1WMTPL ptp00;
          If %Eof(WMTPL);
             TPseq#=1;
          Else;
             TPseq#=TPseq#+1;
          EndIf;
          TPco#=CMco#;
          TPfnd=CMfnd;
          TPfyr=CMfyr;
          TPcase=CMcase;
          TPSTAT='A';
       EndIf;
       If edtPhys=*On or delPhys=*On;
          Chain physRRN ptp0p;
       EndIf;
       If delPhys=*On;
          //*** Beg Add *** 03/04/24 *******************************************
          // Delete from Wmcnpcrt file when we delete treating physician records
          ConType    = 'Treating Physician';
          Setll (TPco#: TPfnd: TPcase: conType:TPseq#) pcn0p_cr;
          Reade (TPco#: TPfnd: TPcase: conType:TPseq#) pcn0p_cr;
          Dow not %eof(WMCNPCRT);
             Delete pcn0p_cr;
             Reade (TPco#: TPfnd: TPcase: conType:TPseq#) pcn0p_cr;
          Enddo;
          //*** End Add *** 03/04/24 *******************************************
          Delete ptp0p;
       Else;
          If scPhyFeI <> 0;
             taxID#=scPhyFeI;
             kkFein=fein;
             KKMod=modifier;
             Chain keyWMPDL ppd00;
             If %Found(WMPDL);

                If PDfsn <> *Blanks;
                   scPhyNameI=PDname + ', ' + PDfsn;
                Else;
                   scPhyNameI=PDname;
                EndIf;

                If PDdba1 = 'Y';
                   scPhyAdrI=PDadr2;
                Else;
                   scPhyAdrI=PDadr1;
                EndIf;

                scPhyCityI=PDcty;
                scPhyStI=PDst;
                scPhyZipI=PDzip;
                If scPhyPhnI=0;
                   scPhyPhnI=PDarea*10000000 + PDphn#;
                EndIf;
                If scPhyFaxI=0;
                   scPhyFaxI=PDfxAr*10000000 + PDfax#;
                EndIf;
                If scPhysEmAd=*Blanks;
                   scPhysEmAd=PDemAd;
                EndIf;
             EndIf;
          EndIf;
          taxID#=scPhyFeI;
          TPphyF=fein;
          TPphyM=modifier;
          TPphyN=scPhyNameI;
          TPphyA=scPhyAdrI;
          TPphyC=scPhyCityI;
          TPphyS=scPhyStI;
          TPphyZ=scPhyZipI;
          phoneNumber=scPhyPhnI;
          TPphyR=areaCode;
          TPphyP=phone#;
          phoneNumber=scPhyFaxI;
          TPfxAr=areaCode;
          TPfax#=phone#;
          TPpRol=scRoleI;
          TPfrDt=dtCYMD(scFrDtI);
          TPtoDt=dtCYMD(scToDtI);
          TPeDat=scEndDatI;
          TPclin=scClNameI;
          TPemAd=scPhysEmAd;
          TPuser=q1User;
          TPpgm=q1Pgm;
          TPtime=TimeTo6(SystemTime());
          TPdt=DateTo7(SystemDate());
          If edtPhys=*On;
             Update ptp0p;
          EndIf;
          If addPhys=*On;
             Write ptp0p;
          EndIf;
       EndIf;
      /End-Free
     P UpdAddPhys      E
       //===================================================================
       // UpdAddTrial - Update/Edit Trial Information
       //===================================================================
     P UpdAddTrial     B
     D UpdAddTrial     PI
     d trialRRN                       9  0
     d addTrial                        n
     d edtTrial                        n
      *** Beg Add ********* 03/14/14 ********************
     d delTrial                        n
      *** End Add ********* 03/14/14 ********************
      /Free
       If addTrial=*On;
          TIco#=CMco#;
          TIfnd=CMfnd;
          TIfyr=CMfyr;
          TIcase=CMcase;
          TISTAT='A';
          TIiUsr=q1User;
          TIiPgm=q1Pgm;
          TIiTm=TimeTo6(SystemTime());
          TIiDt=DateTo7(SystemDate());
       EndIf;
          // *** Beg Chg ********* 03/14/14 ********************
       //***** If edtTrial=*On;
       If edtTrial=*On or delTrial=*On;
          // *** End Chg ********* 03/14/14 ********************
          Chain trialRRN pti0p;
       EndIf;
       TIsuDt=dtCYMD(scSuitDtI);
       TItrTp=scTrTypeI;
       TIjudg=scJudgeI;
       TIissu=scLegIssI;
       TIcLvl=scCrtLvlI;
       TIverd=scVerdictI;
       TIveDt=dtCYMD(scVerdDtI);
       TIpeti=scPetitI;
       TIuser=q1User;
       TIpgm=q1Pgm;
       TItime=TimeTo6(SystemTime());
       TIdt=DateTo7(SystemDate());
       If edtTrial=*On;
          Update pti0p;
       EndIf;
       If addTrial=*On;
          Write pti0p;
       EndIf;
       // *** Beg Add ********* 03/14/14 ********************
       If delTrial=*On;
          Chain trialRRN pti0p;
          Delete pti0p;
       EndIf;
       // *** End Add ********* 03/14/14 ********************
      /End-Free
     P UpdAddTrial     E
       //===================================================================
       // UpdAddWitness - Update/Edit Witness Information
       //===================================================================
     P UpdAddWitness   B
     D UpdAddWitness   PI
     d witnessRRN                     9  0
     d addWitness                      n
     d edtWitness                      n
     d delWitness                      n
      /Free
       If addWitness=*On;
          WIco#=CMco#;
          WIfnd=CMfnd;
          WIfyr=CMfyr;
          WIcase=CMcase;
       EndIf;
       If edtWitness=*On or delWitness=*On;
          Chain witnessRRN pwi0p;
       EndIf;
       If delWitness=*On;
          Delete pwi0p;
       Else;
          WIemAd=scWitEmadI;
          WIname=scWitNameI;
          WIbPh#=busPhoneNum;
          WIuser=q1User;
          WIpgm=q1Pgm;
          WItime=TimeTo6(SystemTime());
          WIdt=DateTo7(SystemDate());
          If edtWitness=*On;
             Update pwi0p;
          EndIf;
          If addWitness=*On;
             Write pwi0p;
          EndIf;
       EndIf;
      /End-Free
     P UpdAddWitness   E
       //===================================================================
       // UpdAddWork - Update/Edit Work Information
       //===================================================================
     P UpdAddWork      B
     D UpdAddWork      PI
     d workRRN                        9  0
     d addWork                         n
     d edtWork                         n
     d delWork                         n
      /Free
       If addWork=*On;
          WHco#=CMco#;
          WHfnd=CMfnd;
          WHfyr=CMfyr;
          WHcase=CMcase;
          WHstat='A';
          WHiUsr=q1User;
          WHiPgm=q1Pgm;
          WHiTim=TimeTo6(SystemTime());
          WHiDt=DateTo7(SystemDate());
       EndIf;
       If edtWork=*On or delWork=*On;
          Chain workRRN pwh0p;
       EndIf;
       If delWork=*On;
          Delete pwh0p;
       Else;
          WHdate=dtCYMD(scWorkDtI);
          WHwkSt=scWorkStsI;
          WHrtwQ=scRtwQuaI;
       //*** Beg Add *** 05/30/18 **************************
          WHcDeK=dtCYMD(scEmpDisDt);
          WHcDaK=dtCYMD(scAdmDisDt);
          WHcDlW=dtCYMD(scLastDay);
          WHlRsE=scRTWSmEmp;
       //*** End Add *** 05/30/18 **************************
          WHuser=q1User;
          WHpgm=q1Pgm;
          WHtime=TimeTo6(SystemTime());
          WHdt=DateTo7(SystemDate());
          If edtWork=*On;
             Update pwh0p;
          EndIf;
          If addWork=*On;
             Write pwh0p;
          Endif;
       Endif;
       //*** Beg Add *** 05/30/18 *********************************
       //
       // Update Claim Master with most recent updates to Work History
       //
       Chain (WHco#:WHfnd:WHfyr:WHcase) pcm01;
       If %Found (wmcml1);
       //
       // Get the most recent work history record
       //
          Setgt (CMco#:CMfnd:CMfyr:CMcase:*hival) pwh02;
          Readpe (CMco#:CMfnd:CMfyr:CMcase) pwh02;
          If Not %Eof(wmwhl);
             //
             CMlrse=WHlrse;
             CMlrtc=WHrtwq;
             //
             // Date Employer had knowledge of disability and Date admin
             // had knowledge of disablity
             //
             If WHcDeK > 0;
                CMcDeK = WHcDeK;
             Endif;
             If WHcDaK > 0;
                CMcDaK = WHcDaK;
             Endif;
             //
             // Date disablity Began/RTW and Date Last Day Worked
             //
             Dow WHwKst <> 'L' and Not %Eof(wmwhl);
                Readpe (CMco#:CMfnd:CMfyr:CMcase) pwh02;
             Enddo;
             If WHwKsT = 'L' and WHdate > 0;
                CMcDdB = WHdate;
             Endif;
             If WHwKsT = 'L' and WHcDlW > 0;
                CMcDlW = WHcDlW;
             Endif;

             CMuser=q1User;
             CMpgm=q1Pgm;
             CMtime=TimeTo6(SystemTime());
             CMdt=DateTo7(SystemDate());
             Update pcm01;
             Endif;
          EndIf;

       //*** End Add *** 05/30/18 *************************************
      /End-Free
     P UpdAddWork      E
       //*** Beg Add *** 10/25/12 *********************************************
       //===================================================================
       // UpdAddWrk - Update/Edit Work Information for CSI
       //===================================================================
     P UpdAddWrk       B
     D UpdAddWrk       PI
     d workRRN                        9  0
     d addWork                         n
     d edtWork                         n
     d delWork                         n
      /Free
       khSeq#=*hival;
       Open fawhl5;
       Setgt keyFAWHL2 pwh05;
       Readpe keyFAWHL1 pwh05;
       If not %eof(FAWHL5);
          intSeq#=WHseq# + 1;
       Else;
          intSeq#=1;
       Endif;
       Close fawhl5;

       If addWork=*On;
          Clear pwh0f;
          FHco#=FMco#;
          FHsub#=FMsub#;
          FHfnd=FMfnd;
          FHfyr=FMfyr;
          FHcase=FMcase;
          FHcode=scWrkCode;
          FHwkdt=dtCYMD(scWrkdt);
          FHlvl=scLvlDuty;
          FHseq#=intSeq#;
          FHstat='A';
          FHiUsr=q1User;
          FHiPgm=q1Pgm;
          FHiTim=TimeTo6(SystemTime());
          FHiDt=DateTo7(SystemDate());
       EndIf;
       If edtWork=*On or delWork=*On;
          Chain workRRN pwh0f;
       EndIf;
       If delWork=*On;
          Delete pwh0f;
       Else;
          FHwkdt=dtCYMD(scWrkDt);
          FHcode=scWrkCode;
          FHlvl=scLvlDuty;
          FHuser=q1User;
          FHpgm=q1Pgm;
          FHtime=TimeTo6(SystemTime());
          FHdt=DateTo7(SystemDate());
          If edtWork=*On;
             Update pwh0f;
          EndIf;
          If addWork=*On;
             Write pwh0f;
          EndIf;
       EndIf;
      /End-Free
     P UpdAddWrk       E
       //*** End Add *** 10/25/12 *********************************************
       //===================================================================
       // UpdateClmRsv - Update Claim Reserve Buckets
       //===================================================================
     P UpdateClmRsv    B
     D UpdateClmRsv    PI
     d clmRrnIn                       9  0
     d clmRrn          s              9  0
      /Free

       If wkRc01<>0 Or wkRc02<>0 Or wkRc03<>0 Or wkRc04<>0 Or wkRc05<>0 Or
          wkRc06<>0 Or wkRc07<>0 Or wkRc08<>0 Or wkRc09<>0 Or wkRc10<>0;
          clmRrn=clmRrnIn;
          Chain clmRrn pcm0p;
          If %Found(WMCMP);
             For i=1 to 10;
                If scRsvCode(i) <> *Blanks;
                   j=%LookUp(scRsvCode(i):rsvCode);
                   If j=0;
                      j=%LookUp(*Blank:rsvCode);
                      If j<>0;
                         rsvCode(j)=scRsvCode(i);
                         rsvRemain(j)=rsvRemain(j) + remRsvCuml(i);
                      EndIf;
                   Else;
                      rsvRemain(j)=rsvRemain(j) + remRsvCuml(i);
                   EndIf;
                EndIf;
             EndFor;
             CMuser=q1User;
             CMpgm=q1Pgm;
             CMtime=TimeTo6(SystemTime());
             CMdt=DateTo7(SystemDate());
             Update pcm0p;
          EndIf;
       EndIf;

      /End-Free
     P UpdateClmRsv    E
       //===================================================================
       // ValidateRsv =  Validate the screen fields
       //===================================================================
     P ValidateRsv     B
     D ValidateRsv     PI
     D clmRrnIn                       9  0
     D statusIn                       1
     D clmRrn          s              9  0
     D status          s              1
      ****** beg add ****** 12/01/14 ********************************
     d strMsg          s            256
      ****** end add ****** 12/01/14 ********************************
      /free

       clmRrn=clmRrnIn;
       status=StatusIn;

       // Reset Error Indicators
       InzErrInd();

       // Convert the lowercase to uppercase
       For i=1 to %Elem(scRsvCode);
          scRsvCode(i) = Upper(scRsvCode(i));
       EndFor;

       // Reset Arrays
       Clear   remRsvCuml;
       Clear   rsvCodeChk;

       // Give the pop window for the explanation of Change in reserve
       If scExRs = *blanks;
          msExRs = 'This field cannot be left blank.';
          erExRs = *on;
          isError = *on;
       Endif;

       // Reserves cannot be changed if closing claim or claim already close
       // or claim already closed

       If status='C';
          For x=1 to %Elem(scRsvCode);
             If scRsvCode(x) <> *Blanks;
                erRsvCode(x) = *On;
                msRsvCode(x) = 'WCR0631';
                isError = *On;
             EndIf;
          EndFor;
       EndIf;

       //
       // Reload the reserve, payment, reserve code, and total
       // incurred arrays in the case that there has been a change due
       // to a pmt or reserve chg by another user. A chain will do this!!!
       //
       Chain(n) clmRrn pcm0p;
       If %Found(WMCMP);
          For i=1 to 10;
             kkKey=rsvCode(i);
             Chain keyWTABL prt00;
             If %Found(WTABL);
                scRsvTyp(i)=RTtype;
             Else;
                scRsvTyp(i)=*Blanks;
             EndIf;
          EndFor;

          //
          // If another user changed the status since this screen was
          // accessed, send an error message.
          //
          If status<>CMstat;
          // errClmSts = *on;
          // ErrMsgsts = 'WCR1075';
          // isError   =  *on;
          EndIf;
       EndIf;

       //****** beg add ********* 12/01/14 *********************************
       intClmRrn = clmRrn;
       //****** beg add ********* 12/01/14 *********************************

       // Validate all the reserve codes
       ValidateRsvCd();

       // Cumulative Reserve Limits
       // Total reserve change may not exceed the user's reserve
       // authority limit.

       //*** Begin Chg *** 11/12/14 **********************************
       //*** If getRsvAutLim(q1User) <> 0;
       If getRsvLimType(q1User) <> 'I';
       //*** End   Chg *** 11/12/14 **********************************
       sumRC = %Xfoot(remRsvCuml);
       rsvLimit=getRsvAutLim(q1User);
       //*** Begin Chg *** 11/12/14 **********************************
       //*If sumRC>rsvLimit and rsvLimit<>0;
          If sumRC>limAmt;
       //*** End   Chg *** 11/12/14 **********************************
             For i=1 to %Elem(scRsvChg);
                //*** Begin Chg *** 06/12/2013 ***********************
                // erRsvChg(i) = *On;
                // msRsvChg(i) = 'WCR0574';
                If remRsvCuml(i)<>0;
                   erRsvChg(i) = *On;
                   msRsvChg(i) = 'WCR0574';
                EndIf;
                //*** End   Chg *** 06/12/2013 ***********************
             Endfor;
                //*** Begin Chg *** 06/12/2013 ***********************
             isError = *On;
                //*** End   Chg *** 06/12/2013 ***********************
          EndIf;
       EndIf;

       //******* beg add ******** 12/01/14 ***************************
       Chain CMco# pcw00;
       If %found(WMCWL);
          //******** beg chg ******** 04/27/15 ********************************
          // intIndTotal = 0;
          // intMedTotal = 0;

          // intIndTotal = GetRemRsv('C') + GetPdRsv('C') + intIndTot;
          // intMedTotal = GetRemRsv('M') + GetPdRsv('M') + intMedTot;

          // If ((CMltfl = 'I' and CWinfl = 'Y' and intIndTot > CWinth) or
          //  (CMltfl = 'M' and CWmofl = 'Y' and intMedTot > CWmoth)) and
          //  CMstat <> 'C';

          If isInd = *off;
             intTotal = GetRemRsv('C') + GetPdRsv('C') + intTotal;
          Endif;
          If isMed = *off;
             intTotal = GetRemRsv('M') + GetPdRsv('M') + intTotal;
          Endif;
          If isExp = *off;
             intTotal = GetRemRsv('E') + GetPdRsv('E') + intTotal;
          Endif;
          If isLgl = *off;
             intTotal = GetRemRsv('L') + GetPdRsv('L') + intTotal;
          Endif;

          If ((CMltfl = 'I' and CWinfl = 'Y' and intTotal > CWinth) or
           (CMltfl = 'M' and CWmofl = 'Y' and intTotal > CWmoth)) and
           CMstat <> 'C';
          //******** end chg ******** 04/27/15 ********************************
             btnCanRsvW = *off;
             btnCnfRsvW = *off;

             strTitle = 'Reserve Worksheet';
             strRrn = %EditC(clmRrn:'X');
             scWrkURL = CreateURL('STDRVPGM':strTitle:strRrn:'C':'WTCLMRWK'
              :'00000');
             Exfmt rcdRsvWrk;

             Select;
             When btnCnfRsvW = *on;
                btnCnfRsvW = *off;
                strMsg = 'The Claim has been opened as a Reserve Worksheet in +
                 another tab. Once the reserve worksheet is finished, click OK +
                 to reload the screen.';
                PopOneBtn(strMsg:'Claim Reserve Worksheet Open');

             When btnCanRsvW = *on;
                btnCanRsvW = *off;
             Endsl;
             isError = *On;
             isWrkSht = *on;
          Endif;
       Endif;
       //******* end add ******** 12/01/14 ***************************

      /End-Free

     P ValidateRsv     E
       //================================================================
       // ValidateRscCd - Validate the Reserve Codes
       //================================================================
     P ValidateRsvCd   B
     D ValidateRsvCd   PI

       //******* beg add ******* 12/01/14 *****************************
       //*** beg chg ***** 06/10/15 ***********************************
     d*** arrErrInd       s              1    dim(25)
     d arrErrInd       s              1    dim(45)
       //*** end chg ***** 06/10/15 ***********************************
       //******* end add ******* 12/01/14 *****************************
      /free

       // Clear wk array
       Clear wk;
       //**** Beg add **** 04/03/15 ************************************
       // Clear wkt array
       Clear wkt;
       //**** End add **** 04/03/15 ************************************
       //

       //******* beg add ******* 12/01/14 *****************************
       intIndTot = 0;
       intMedTot = 0;

       EdtRsvChgs(intClmRrn:scRsvCode:scRsvChg:arrErrInd);
       //******* end add ******* 12/01/14 *****************************
       //******* beg add ******* 04/27/15 *****************************
       isInd = *off;
       isMed = *off;
       isExp = *off;
       isLgl = *off;
       intTotal = 0;
       //******* end add ******* 04/27/15 *****************************

       // Retrieve Claim Darts Record
       Clear CXrChg;
       Chain keyWMCXL pcx00;

       // Edit Reserve changes
       for i=1 to %Elem(scRsvCode);
         //**** Beg add **** 04/03/15 ************************************
          rttype = *blanks;
         //**** End add **** 04/03/15 ************************************
          If scRsvCode(i) <> *Blanks Or scRsvChg(i) <> *Zeros;

             // Reserve code and amount are mutually inclusive.  If the
             // 'Claim Reserve Change' control flag indicates that amount
             // entered is the total incurred, the amount CAN be zero.

             // *** Begin Chg *** 06/04/14 ****************************
             //If (scRsvCode(i) <> *Blanks And scRsvTyp(i) = *Zeros And
             //    CXrChg <> 'Y') Or (scRsvCode(i) = *Blanks And
             //    scRsvTyp(i) <> *Zeros And CXrChg <> 'Y');

             If (scRsvCode(i) <> *Blanks And scRsvChg(i) = *Zeros And
                 CXrChg <> 'Y') Or (scRsvCode(i) = *Blanks And
                 scRsvChg(i) <> *Zeros And CXrChg <> 'Y');
             // *** End   Chg *** 06/04/14 ****************************
                If erRsvCode(i) <> *On;
                   erRsvCode(i)=*On;
                   msRsvCode(i)='WCR0544';
                EndIf;
                isError=*On;
             EndIf;
          EndIf;

          // No need to process these edits if no code
          If scRsvCode(i)<>*Blanks;

             //  A reserve code cannot be used more than once.
             If %LookUp(scRsvCode(i):wk) <> *Zero And scRsvCode(i)<>*Blanks;
                If erRsvCode(i)<>*On;
                   erRsvCode(i)=*On;
                   msRsvCode(i)='WCR0557';
                Endif;
                isError=*On;
             EndIf;

             // Validate reserve code
             kkKey=scRsvCode(i);
             Chain keyWTABL prt00;
             If (%Found(WTABL) And RTstat='I') or Not %Found(WTABL);
                If erRsvCode(I)<>*On;
                   erRsvCode(i)=*On;
                   msRsvCode(i)='WCR0513';
                EndIf;
                isError=*On;
             EndIf;
             //
             //*** Begin Del *** 05/11/16 *************************************
             // **** Begin Add **** 04/03/15 **********************************
             //  A reserve type cannot be used more than once
             //***If %LookUp(rttype:wkt) <> *Zero and rttype <> *Blanks
             //***   and %found(WTABL);
             //***   If erRsvCode(i)<>*On;
             //***      erRsvCode(i)=*On;
             //***      msRsvCode(i)='WCR0557';
             //***   Endif;
             //***   isError=*On;
             //***EndIf;
             // **** End    Add **** 04/03/15 **********************************
             //*** End   Del *** 05/11/16 *************************************
             //
             // Reserve code must be valid for Group
             Clear groupRsv;
             Chain keyWMFNL pfn00;
             If %LookUp(scRsvCode(i):groupRsv)=0;
                If erRsvCode(i) <> *On;
                   erRsvCode(i)=*On;
                   msRsvCode(i)='WC30126';
                EndIf;
                isError=*On;
             EndIf;
             //***** Begin add *** 03/24/15 ***********************************
             //
             // Cumulative reserve type must be authorized for user
             //
             If GetRsvLimType(q1User) = 'C' and
                ValRsvType(q1User:rttype) = *Off;
                If erRsvCode(i) <> *On;
                   erRsvCode(i)=*On;
                   msRsvCode(i)='WC40032';
                EndIf;
                   isError=*On;
             EndIf;
             //***** End   add *** 03/24/15 ***********************************
             //
             // Comp Reserves can only be entered on an indemnity claim.
             //****** beg chg ********** 12/01/14 *****************************
             If RTtype='C' and arrErrInd(1) = '1';
             //****** end chg ********** 12/01/14 *****************************
                If erRsvcode(i) <> *On;
                   erRsvCode(i)=*On;
                   msRsvCode(i)='WCR1248';
                EndIf;
                isError=*On;
             EndIf;

             // Is Medical Excluded?
             //****** beg chg ********** 12/01/14 *****************************
             If RTtype = 'M' and arrErrInd(2) = '1';
             //****** end chg ********** 12/01/14 *****************************
                If erRsvCode(i) <> *On;
                   erRsvCode(i)=*On;
                   //*** Begin Chg *** 03/10/15 ***************************
                   //*** msRsvCode(i)='WCR1248';
                   msRsvCode(i)='WC40022';
                   //*** End   Chg *** 03/10/15 ***************************
                EndIf;
                isError=*On;
             EndIf;

             // If the entered reserve code does not already exist in the
             // claim, verify that there are not already 10 reserve codes
             // setup in the claim.

             // Remaining Reserves Cannot be Less Than 0
             int=%LookUp(scRsvCode(i):rsvCode);
         //*** Begin Del *** 04/23/13 ***************************
         //* If int = 0;
         //*    int=1;
         //* EndIf;
         //*** End   Del *** 04/23/13 ***************************

             If CXrChg='Y' And int<>0;
         //*** Begin Chg *** 04/23/13 ***************************
         //***  remRsvCuml(i) = scRsvChg(i) - (rsvRemain(int) + rsvPaid(int));
                remRsvCuml(i) = scRsvChg(i) -
                 (rsvRemain(int) + rsvPaid(int));
         //*** End   Chg *** 04/23/13 ***************************
             Else;
                remRsvCuml(i) = scRsvChg(i);
             EndIf;
         //*** Begin Chg *** 02/21/14 ***************************
             //***** beg chg ****** 12/01/14 ************************
             //******* beg chg ******* 04/27/15 *****************************
             // If int <> 0;
                // If RTtype = 'C';
                //    intIndTot = intIndTot + scRsvChg(i);
                // Endif;
                // If RTtype = 'M';
                //    intMedTot = intMedTot + scRsvChg(i);
                // Endif;

             //*** Begin Add *** 06/08/16 ***********************
             //***If int <> 0;
             If RTtype <> ' ';
             //*** End   Add *** 06/08/16 ***********************
                intTotal = intTotal + scRsvChg(i);

                Select;
                When RTtype = 'C';
                   isInd = *on;
                When RTtype = 'M';
                   isMed = *on;
                When RTtype = 'E';
                   isExp = *on;
                When RTtype = 'L';
                   isLgl = *on;
                Endsl;
                //******* end chg ******* 04/27/15 *****************************
             Endif;

             If arrErrInd(4) = '1';
             //****** end chg ********** 12/01/14 *****************************
                     If erRsvChg(i) <> *On;
                     erRsvChg(i)=*On;
                     msRsvChg(i)='WC40156';
                     Endif;
                     isError=*On;
                    EndIf;
         //*** End Chg *** 02/21/14 *****************************

         //*** Begin Chg *** 04/23/13 ***************************
         //* If (rsvRemain(int) + remRsvCuml(i)) < 0;
         //*   If erRsvcode(i) <> *On;
         //*      erRsvCode(i) = *On;
         //*      msRsvCode(i) = 'WCR0552';
         //*   EndIf;
         //*   isError=*On;
         //* EndIf;
             //****** beg chg ********** 12/01/14 *****************************
             If arrErrInd(i + 4) = '1';
                If erRsvcode(i) <> *On;
                   erRsvCode(i) = *On;
                   msRsvCode(i) = 'WCR0552';
                EndIf;
                isError=*On;
             EndIf;
             //****** end chg ********** 12/01/14 *****************************
         //*** End   Chg *** 04/23/13 ***************************

             // If rsv change is negative make sure remaining rsv
             // will be enough to cover any requests for payments
             //****** beg chg ********** 12/01/14 *****************************
         //*** Begin Chg *** 04/23/13 ***************************
         //***  If xxRqPy > (rsvRemain(int) + remRsvCuml(i));
         //***     If erRsvCode(i) <> *On;
         //***        erRsvCode(i)=*On;
         //***        msRsvCode(i)='WCR0635';
         //***     EndIf;
         //***     isError=*On;
         //***  EndIf;
         //*** End   Chg *** 04/23/13 ***************************

         //*** beg chg ***** 06/10/15 ***************************
         //*** If arrErrInd(16) = '1';
             If arrErrInd(i + 24) = '1';
         //*** end chg ***** 06/10/15 ***************************
                If erRsvCode(i) <> *On;
                   erRsvCode(i) = *On;
                   msRsvCode(i) = 'WCR0635';
                Endif;
                isError = *on;
             Endif;
             //****** end chg ********** 12/01/14 *****************************

             // Do not allow the user to exceed their reserve authority limit

             //*** Begin Del *** 11/12/2014 **********************************
             //***** Beg Chg ***** 05/19/14 **********************************
             //*****If remRsvCuml(i) > 0 And getRsvAutLim(q1User) = 0;
             //*** If getRsvAutLim(q1User) = 0;
             //*If erRsvChg(i) <> *On;
             //**  erRsvChg(i)=*On;
             //**  msRsvChg(i)='WCR0574';
             //*Endif;
             //*isError=*On;
             //*Endif;
             //*** If remRsvCuml(i) > 0 and erRsvChg(i) <> *On;
             //*** End   Del *** 11/12/2014 **********************************
             //***** End Chg ***** 05/19/14 **********************************
             //*** Begin Add *** 11/12/2014 **********************************
             If remRsvCuml(i) > 0;
             //*** End   Add *** 11/12/2014 **********************************
                limAmt=0;
             //***** Beg Chg ***** 07/22/14 **********************************
             // ClcRemRsvLim(CMco#:CMfnd:CMfyr:CMcase:rsvCode(i):limAmt);
                ClcRemRsvLim(CMco#:CMfnd:CMfyr:CMcase:scRsvCode(i):limAmt);
             //***** End Chg ***** 07/22/14 **********************************

                // For cumulative edits consider previous screen amounts
                // in the total.

                //*** Begin Del *** 11/12/2014 **********************************
                //*rcAmountTot=%Xfoot(remRsvCuml);
                //*limAmt=(limAmt-rcAmountTot)+remRsvCuml(i);
                //*** End   Del *** 11/12/2014 **********************************

             //***** Begin Chg ***** 11/12/2014 **********************************
             //*If limAmt<>0;
             //*** If remRsvCuml(i) > limAmt;
             //***    If erRsvChg(i) <> *On;
             //***       erRsvChg(i)=*On;
             //***       msRsvChg(i)='WCR0574';
             //***    Endif;
             //***    isError=*On;
             //*** EndIf;
             //*EndIf;
                //****** beg chg ********** 12/01/14 *****************************
                //*** beg chg ***** 06/10/15 *************************************
                //*** If arrErrInd(15) = '1';
                If arrErrInd(i + 14) = '1';
                //*** end chg ***** 06/10/15 *************************************
                //****** end chg ********** 12/01/14 *****************************
                   If erRsvChg(i) <> *On;
                      erRsvChg(i)=*On;
                      msRsvChg(i)='WCR0574';
                   Endif;
                   isError=*on;
                EndIf;
             //***** End   Chg ***** 11/12/2014 **********************************
             Endif;

             // If the 'Claim Reserve Change' control flag indicates
             // that amount entered is the total incurred and the amount
             // is zero, display a warning window indicating that the total
             // incurred will be reduced to zero.

             If IsError <> *On; // And chgClmSts <> '1';
                If CXrChg = 'Y' And scRsvCode(i) <> *Blanks And
                 scRsvChg(i) = *zeros;
                   scWdw2RC=scRsvCode(i);
                   ExFmt rcdWDW02;
                   If BtnCnlWDW2=*On;
                      isError=*On;
                   EndIf;
                EndIf;
             EndIf;
          EndIf;

          // Reset control indicator

          wk(i)=scRsvCode(i);
          // Begin add *** 04/03/15 *********************************************
          wkt(i) = rttype;
          // End add ***** 04/03/15 *********************************************
       EndFor;

      /end-free
     P ValidateRsvCd   E
       //===================================================================
       // WriteRsvHst - Write Reserve History
       //===================================================================
     P WriteRsvHst     B
     D WriteRsvHst     PI
      /Free

       If wkRc01<>0 Or wkRc02<>0 Or wkRc03<>0 Or wkRc04<>0 Or wkRc05<>0 Or
          wkRc06<>0 Or wkRc07<>0 Or wkRc08<>0 Or wkRc09<>0 Or wkRc10<>0;

          Clear prh02;
          RHco#=CMco#;
          RHfnd=CMfnd;
          RHfyr=CMfyr;
          RHcase=CMcase;
          RHefDt=DateTo7(SystemDate());
          RHexpl=scExRs;
          RHuser=q1User;
          RHiUsr=q1User;
          RHpgm=q1Pgm;
          RHiPgm=q1Pgm;
          RHtime=TimeTo6(SystemTime());
          RHiTim=TimeTo6(SystemTime());
          RHdt=DateTo7(SystemDate());
          RHiDt=DateTo7(SystemDate());

          For i=1 to 10;
             If remRsvCuml(i) <> 0;
                j=%LookUp(scRsvCode(i):rsvCode);
                rsvHstCd(j)=scRsvCode(i);
                rsvHstChg(j)=remRsvCuml(i);
                //
                // Auto-DARTS: Now check large reserve change amount
                //
                dartsKey='LARGERSV';
                rsvAmtCrit=getAmtCrit(dartsKey);
                If rsvAmtCrit<>0 And remRsvCuml(i)>rsvAmtCrit;
                   comment=*Blanks;
                   WrtAutoDart(CMco#:CMfnd:CMfYr:CMcase:dartsKey:Comment);
                EndIf;
             Endif;
          EndFor;
        //***  Begin Add  ***  08/15/17  ************************
          RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
        //***  End   Add  ***  08/15/17  *************************

          Write prh02;
       //
       //  Update Employer Detail
       //
          Chain keyWDELL pel00;
          If %Found(WDELL);
             ELoutS=ELoutS + %Xfoot(remRsvCuml);
             Update pel00;
          EndIf;
       EndIf;

      /End-Free
     P WriteRsvHst     E
       //******** beg  add ********** 12/01/14 *****************************
       //===================================================================
       // GetPdRsv - Get paid  by reserve type.
       //===================================================================
     p GetPdRsv        b
     d GetPdRsv        pi            11  2
     d reserveType                    1    const

     d paidAmt         s             11  2 inz(0)
     d intCount        s              5  0
      /free

       For intCount = 1 to 10;
          kkkey = rsvCode(intCount);
          Chain (CMco#:kkkey) prt00;
          If %found(wtabl) and RTtype = reserveType;
             paidAmt += rsvPaid(intCount);
          EndIf;
       EndFor;

       Return paidAmt;

      /end-free
     p GetPdRsv        e
       //===================================================================
       // GetRemRsv - Get the remaining reserves by reserve type.
       //===================================================================
     p GetRemRsv       b
     d GetRemRsv       pi            11  2
     d reserveType                    1    const

     d remainingAmt    s             11  2 inz(0)
     d intCount        s              5  0
      /free

       For intCount = 1 to 10;
          kkkey = rsvCode(intCount);
          Chain (CMco#:kkkey) prt00;
          If %found(wtabl) and RTtype = reserveType;
             remainingAmt += rsvRemain(intCount);
          EndIf;
       EndFor;

       Return remainingAmt;

      /end-free
     p GetRemRsv       e
       //******** end  add ********** 12/01/14 *****************************
       //*** Begin Add *** 05/26/20 ****************************************
       //===================================================================
       // GetBenTypSql - Get Benefit Type Dropdown Sql
       //===================================================================
     p GetBenTypSql    b
     d GetBenTypSql    pi
     d  BenTypSel                   200
     d  BenTypFrom                  100
     d  BenTypWher                  200
      /free
       BenTypSel = 'TRIM(B2KEY)||''-''||TRIM(B2DESC)';
       BenTypFrom = 'WTB2P';
       BenTypWher = 'B2STAT = ''A'' AND (B2RC01 =''CO'' or B2RC02 =''CO'' +
                     or B2RC03 = ''CO'' or B2RC04 = ''CO'' or B2RC05 = ''CO'' +
                     or B2RC06 = ''CO'' or B2RC07 = ''CO'' or B2RC08 = ''CO'' +
                     or B2RC09 = ''CO'' or B2RC10 = ''CO'')';

      /end-free
     p GetBenTypSql    e
       //*** End   Add *** 05/26/20 ****************************************
