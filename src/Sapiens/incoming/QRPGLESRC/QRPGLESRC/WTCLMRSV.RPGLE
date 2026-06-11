     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE WTCLMRSV - Claim Reserve
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     H BNDDIR('WTBNDDIR')
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Claim Reserve Info.
     h*
     h*  Called from: Profound UI
     h*  Calls to: RTVCLTID
     h*
     h*  Created on 3/6/2012 by C:189 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     h* 12/10/25 C:079 Support: 120209
     h*                Correct Duplicate Payment Release Logic
     h*
     h* 07/11/25 C:053 Support: 118380
     h*                - Use proper message for ##rc 09, 10 & 11
     h*
     h* 02/06/25 C:333 Support: 118454
     h*                - Medical/Travel Reimbursement Request
     h*                  Commenting FEIN/MOD edit at payement release
     h*
     h* 09/29/23 C:344 Support 114152
     h*                Inactivate the Edit Link on Group Check line items on
     h*                the Payment Detail Screen.
     h*
     h* 05/09/23 C:344 Project: WC2430
     h*                Add an Edit Link to the Payment Information grid
     h*                which in turn will execute the Payment Updates Popup
     h*                Window proc.
     h*
     H* 04/04/23 C:342 Support 129/108807
     H*               1)Fix Screen Positioning on payment screen
     h*               2)Added subfile record number in SflPenReq
     h*
     H* 06/23/21 C:318 Project WC2302 - Auto tab closure for
     H*                deletion.
     h*
     h* 06/24/21 C:062 Ticket: 96220(129)
     h*                Correct EFT button.
     h*
     h* 06/10/21 C:180 Project:133WSO
     h*                Show MICR button for ePay payment if user security
     h*                allows changing payment request from ePay to MICR.
    H*
    H*  04/19/21  C:079 Support: 90/95537
    H*                  Add Bank Code to API
     h*
     h*  12/16/20  C:79  Project: WC2749/92674
     h*                  Fix overlapping Check# field
     h*
     h*  10/22/20  C:62  Project: WC2749
     h*                  Replace EFT check box with radio buttons that
     h*                  include V-Pay
     h*
     h*  08/05/20  C:33  Project: SY409
     h*                  Standardize payee name retrieval
     h*
     h* 07/30/20 C:180 Project: 152WBE
     h*                Allow closing of claim with outstanding PPO fees check
     h*                requests.
     h*
     h* 07/20/20 C:079 Project: WC2749
     h*                E-Pay Changes to column headings, etc.               k
     h*
     h*  06/09/20  C:298 Project: SY420
     h*                  Mask SSN
     h*
     h* 11/25/19 C:291 Support: 133WRI
     h*                Write Claim History(WMCHP) record first then update
     h*                Claims (WMCMP) record for SPITRIGGER to get latest
     h*                Claim history record.
     h*
     h* 11/08/19 C:079 Support: 099/82052
     h*                Change Managed Care Service programs to be API's      aft
     h*
     h* 10/21/19 C:079 Support: 152/84105
     h*                Prevent Fee Checks from auto releasing when provider  aft
     h*                check is being released                               aft
     h*
     h* 09/12/19 C:300 Suppot: 82148 (LUBA)
     h*                Unable to see ADD button on EFT Payments Info screen, aft
     h*                Setting Sec level C 116 using security application editor
     h*
     h* 08/15/19 C:079 Project: 152WAB Support: 152/81384
     h*                Display invoice number for client 152
     h*
     h* 12/19/18 C:062 Ticket: 75632(114)
     h*                Expand array size.
     H*
     H* 09/10/18 C:79  Project: 146WHY
     H*                Add Recevied date to the payment detail tab
     H*
     H* 07/10/18 C:62  Project: 71849(136)
     H*                -Release once.
     H*
     H* 06/27/18 C:62  Project: 71510(146)
     H*                -Don't allow release of request if reserve
     H*                 not valid for claim
     H*                -Prevent History grid from disappearing
     H*
     H* 02/27/18 C:62  Project: 64521(129)                            *
     H*                Rename SDCMP fields                            *
     H*
     H* 02/20/18 C:62  Project: 64369(129)                            *
     H*                Don't clear diary fields: RPDSEQ and RPSENT    *
     H*
     H* 01/19/18 C:62  Project: 51625(102)                            *
     H*                Correct duplicate payment process              *
     H*
     H* 08/16/17 C:62  Project: WC2501                                *
     H*                Delete recurring remarks if claim is closed    *
     H*
     H* 08/16/17 C:62  Project: WC2501                                *
     H*                Delete recurring remarks if claim is closed    *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
      *                                                                    *
      *  06/12/17 C:062 Support 47122(99)                                  *
      *                 Fixed Duplicate payent processing                  *
      *                                                                    *
      *  12/08/16 C:180 Support 44631                                      *
      *                 Fix cause of closed claim with outstanding         *
      *                 reserves.                                          *
      *                                                                    *
      *  11/02/16 C:053 Support 43959
      *                 User cannot release payment requests.
      *
      *  09/13/16 C:180 Support 42764                                      *
      *                 Do not allow unauthorized user to release payments.*
      *                                                                    *
      *  08/18/16 C:307 Support: PHW42700                                  *
      *                 Code changes to correct the blurred cheque no.     *
      *                 in the Claim Payment Detail box.                   *
      *                                                                    *
      *  03/23/16 C:308 Project: ELEVATE                                   *
      *                 Recompile only                                     *
      *                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
     h*                                                                    *
     h*  12/08/15 C:300 Support: 34634                                     *
      *                 In payment request if the user accidentally clicks *
      *                 on the EOB link in payment without EOB, Java script*
      *                 opens EOB link -- fix this issue                   *
     h*                                                                    *
     h*  10/27/15 C:306 Support: 38028                                     *
      *                 To fix the Reserve worksheet threshold amount issue*
      *                                                                    *
     h*  10/22/15 C:180 Support: 38007
      *                 Add authority edit to Payment request              *
      *
     h*  10/14/15 C:300 Support: 37399                                     *
     h*                 Update UI by updating dollar fields with commas    *
     h*                 on claim Payment Information Grid.                 *
      *                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
     h*
     h*  09/25/15 C:242 Phaseware 37520
     h*                 Program errors on Reserve Change. Change CHAIN to
     h*                 NO Lock.
     h*
     h*  09/01/15 C:308 Phasware: 36744
     h*                 Display Deductible history Tab for large Deductible
     h*                 also on Claim.
     h*
     h*  07/31/15 C:308 Support: 36212
     h*                 Compiled for changes in WTCLMRSV screen
     h*
     h*  07/23/15 C:136 Support: 36153
     h*                 Added bank code parameter to GetEftStatus
     h*                 procedure
     h*
     h*  07/06/15 C:306 Support: 33063
     h*                 Changed the length of SCEOB# and SCEOB
     h*                 in WTCLMRSV display file. Need to be
     h*                 recompiled.
     h*
     h*  06/17/15 C:180 Support: 33077
     h*                 Fixed edits during closing of claims.
     h*
     h*  06/01/15 C:308 Support: 34367
     h*                 Corrected check# position for RMLA.
     h*
     h*  05/06/15 C:062 Project: 114MRH
     h*                 Populate Check Type
     h*
     h*  05/06/15 C:306 Phasware: 34591
     h*                 Recompiled over Action panel
     h*
     h*  04/20/15 C:62  Support: 34074(133)
     h*                 Open up fields by correcting logic.
     h*
     h*  04/16/15 C:65  Project: SY251
     h*                 Add API call to launch new Department letters.
     h*
     h*  04/16/15 C:280 Phasware: 32843 T.H.E.
     h*                 Correct issue not passing parm correctly
     h*                 which stops other edits
     h*
     h*  04/10/15 C:180 Support: 37074
     h*                 Add weeks/days calc
     h*
     h* 02/19/15  C:062 Support: 32371 (025)
     h*                 Move routine to prevent record lock.
     h*
     h* 12/11/14  C:283 Support: WC114 87162
     h*                 Do not lock the file WArpp until update is required.
     h*
     h*  12/09/14 C:282 Support: WA099 87514
     h*                 Corrected the code to fetch the valid record
     h*                 from WTPSL.
     h*
     h*  12/01/14 C:279 Project: 114WLF
     h*                 Modified to use Reserve Worksheet.
     H*
     h* 11/21/14  C:290 Redmine: 87391
     h*                 Added server side script to calculate the week/days
     h*                 on Edit Request for payment screen.(compile only)
     h*
     h* 11/18/14  C:296 Redmine: #4824
     h*                 Reposition screen field scChk# and scChk1# so they
     h*                 overlap 100% (only display file is changed).
     h*
     h* 10/01/14  C:062 Support: WC045 86769
     h*                 Correct release edit.
     h*
     h*  09/23/14 C:279 Support: WA099 86678
     h*                 Condition added for Weeks and Days.
     h*
     h* 09/09/14  C:296 Support: WC2299 86710
     h*                 Populate rrn from FAEOP, rrn that will be passed
     h*                 to F0340C.
     h*
     h* 08/20/14  C:062 Support: WC025 86549
     h*                 Add release edits to request edit screen.
     h*
     h* 08/05/14  C:282 Support: WA099 83882
     h*                 Added the handling of Reverse Deductible Recovery
     h*                 link in the program.
     h*
     h* 08/04/14  C:300 Project: WC2247
     h*                 Claims Financial Summary Changes
     h*                 Filter Handling for (Gross Paid to Date) column
     h*
     h* 07/11/14  C:279 Project: 111WBL
     h*                 Add Total Excess Recovery field on screen.
     h*
     h* 07/11/14  C:300 Project: 114WNM
     h*                 Add EOB Number to Payment Detail Grid Number
     h*
     h* 06/13/14  C:279 Redmine: 4313
     h*                 Modified to enable EOB# link in inquiry mode.
     h*
     h* 06/04/14  C:242 Support: Redmine #4210
     h*                 Check# & Link not showing in Claim Reserve/Payment Infor
     h*                 - Payment Detail Tab - Payment Information detail box
     h*                 Screen changes, warranted recompile only.
     h*
     h* 05/20/14  C:300 Project: 147WCX
     h*                 Add Image Link to Reserve/Payment Info Screen
     h*                 Display link only if an image is attached to EOB
     h*
     h* 05/08/14  C:283 Support: WC114 85578
     h*                 Change the Data Area only if deafult filter
     h*                 is there from "Paid to Date" or "Recoveries"
     h*
     h* 01/24/14  C:279 Redmine: SY266/3855
     h*                 Modified to Add Payment Edit using API WA011R.
     h*
     h* 01/22/14  C:290 Support: 84567
     h*                 Corrected the logic to populate the Check# in the
     h*                 exported excel.(Includes Display File changes also)
     h*
     h* 01/13/14  C:189 Redmine: 3588
     h*                 Only client users should appear in the user search and
     h*                 not PDS users (display file change only)
     h*
     h* 03/06/13  C:282 Project: 129WSC #Priss 28
     H*                 Replace legal literal with control field
     H*                 for cltid 129 only.
     h*
     h* 11/25/13  C:282 Project: 129WSC
     h*                 Added the handling to generate the Auto Letter.
     h*
     h* 10/25/13  C:295 Support: WA099 83962
     h*                 Retrofitting -Corrected processing for disabled check# l
     h*
     h* 12/10/13  C:295 Support: WA099 84316
     h*                 Display correct Font if the client does not have module.
     h*                 Changes in WTCLMRSV display file.
     h*
     h* 11/04/13  C:287 Support: WC034 82448
     h*                 Display correct total of weeks/days for payment info.
     h*
     h* 10/25/13  C:279 Support: WC099 83962
     h*                 Corrected processing for disabled check# link.
     h*
     h*  10/01/13 C:053 Project: 25WSS
     h*                 Client retrofit.
     h*
     h* 09/18/13  C:279 Support: WC046 83107
     h*                 Modified to correct the Payment Info Filter.
     h*
     h* 09/17/13  C:290 Support: WC099 83387
     h*                 Initially enabled the check link for every record.
     h*
     h* 09/12/13  C:279 Support: WC099 83340
     h*                 Modified to correct the Payment Info Filter.
     h*
     h* 09/03/13  C:189 Redmine: 3015
     h*                 Use include instead of ignore for action panel record
     h*                 format
     h*
     h*  07/15/13 C:283 Support: WC099 82849
     h*                 Added the save button on display file(Pmt Exlc)
     h*                 Save button works as enter so handling
     h*                 in program not required. Re-Compiled.
     h*
     h*  06/22/13 C:062 Project:SY268
     h*                 Deal with record locks.
     h*
     h*  05/20/13 C:279 Redmine: 2604
     h*                 Condition added for displaying EOB# based on
     h*                 managed care setup.
     h*
     h* 05/17/13  C:279 Redmine: 2849
     h*                 Corrected Button response on pressing Enter
     h*                 for Claim close process.
     h*
     h* 05/16/13  C:279 Redmine: 2611
     h*                 Fix date sorting grid.
     h*
     h* 05/06/13  C:279 Redmine: 2847
     h*                 Modified to correct the Payment Info Filter.
     h*
     h* 04/29/13  C:279 Redmine: 2813
     h*                 Corrected Payee Filter for service date range.
     h*
     h* 04/12/13  C:33  Project: WC2115  #2636
     h*                 Add messages for EFT indicating module not
     h*                  installed.
     h*
     h* 04/03/13 C:189 Project: 133WNP
     h*                Show a Transform window instead of a display file
     h*                for CMS Benefit flag edit.
     h*
     h* 03/14/13 C:282 Redmine: 2374
     h*                Spacing within Wk/days and spliting the service date
     h*                column into two parts.
     h*
     h* 12/18/12 C:062 Project 114WGD
     h*                Removed - Process to exclude hospital bills for users hav
     h*                Title as 'CLIENT' from Payment Information grid under
     h*                Payment Detail Tab for the client 114.
     h*
     h* 09/14/12 C:282 Project 114WGD
     h*             -  Process to exclude hospital bills for users having
     h*                Title as 'CLIENT' from Payment Information grid under
     h*                Payment Detail Tab for the client 114.
     h*             -  Under Payment detail tab changed the lable
     h*                'Mailing Date' to 'Processed Date' for the client 114.
     h*
     h*  10/25/12 C:284 Project: 114WGD
     h*                 CSI Retrofit of Custom Code to Base. Invoice# inclusion
     h*
     h**********************************************************************
     fwtClmRsv  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(SflPayee:rrnPayee)
     f                                     Sfile(SflReserve:rrnHst)
     f                                     Sfile(SflPmtSum:rrnPmtSum)
     f                                     Sfile(SflstsHst:rrnSts)
     f                                     Sfile(SflTrnSum:rrnTrans)
     f                                     Sfile(SflValSum:rrnVal)
     f                                     Sfile(SflPenReq:rrnReq)
     f                                     Sfile(SflDedHst:rrnDedHst)
     f                                     Sfile(SflDedHstr:rrnDedHst)
     f                                     Sfile(SflEftInfo:rrnEftInfo)
     f                                     InfDs(fids01)
     fwtActPnl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     Include(rcdClmPnl)
     fwmcmp     uf   e             Disk
     f                                     ReName(pcm01:dbrcdf)
     f                                     InfDs(dbfds)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     f                                     InfDs(dbfds2)
     fwmcdl     if   e           k Disk    ReName(pcd01:pcd00)
     fwmahl     if   e           k Disk    ReName(pah01:pah00)
     fwmagl     if   e           k Disk    ReName(pag01:pag00)
     fwmall     if   e           k Disk    ReName(pal01:pal00)
     fwmfnl     if   e           k Disk    ReName(pfn01:pfn00)
     fwdpal     if   e           k Disk    ReName(ppa01:ppa00)
     fwdpal5    if   e           k Disk    ReName(ppa01:ppa05)
     f                                     infds(dbpapfds)
     fwdpap     uf   e             Disk    ReName(ppa01:ppa0p)
     Fwdrpl3    if   e           k Disk
     F                                     ReName(prp01:prp03)
     Fwdrpl     if   e           k Disk    ReName(prp01:prp000)
     Fwdacl     if   e           k Disk    ReName(pac01:pac00)
     Fwmnrl     if   e           k Disk    ReName(pnr01:pnr00)
     FWmcml1    if   e           k Disk
     F
     Fwmpdl     if   e           k Disk    ReName(ppd01:ppd00)
     fwmcwl     if   e           k disk    rename(pcw01:pcw00)
     fwdrwl     uf   e           k disk    rename(prw01:prw00) prefix(c)
     fwtabl     if   e           k Disk
     f                                     Ignore(prs01)
     f                                     Ignore(psy01)
     f                                     Ignore(pid01)
     f                                     ReName(prt01:prt00)
     f                                     ReName(ppc01:ppc00)
     f                                     ReName(pb201:pb200)
     f                                     ReName(pb701:pb700)
     Fwmupl     if   e           k Disk    Rename(pup01:pup00)
     Fwmupl1    if   e           k Disk
     Fwdrcl     if   e           k Disk    ReName(prc01:prc00)
     Fwmrsl     if   e           k Disk    ReName(prs01:prs00)
     Fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     Fwtpsl     if   e           k Disk    ReName(pps01:pps00)
     Fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     Fwacpl     if   e           k Disk    ReName(pcp01:pcp00)
     FWmrhp     if   e             Disk    ReName(prh01:prh0p)
     FWmrhl     if   e           k Disk    ReName(prh01:prh00)
     FWmrhl2    O    e           k Disk    ReName(prh01:prh02) prefix(A)
     FWmrhl5    if   e           k Disk
     F                                     ReName(prh01:prh05)
     f                                     infds(dbmrhfds)
     FWdf1l     if   e           k Disk    ReName(pf101:pf100)
     FWmadl     if   e           k Disk    ReName(pad01:pad00)
     FWdell     uf   e           k Disk    ReName(pel01:pel00)
     FWarpl     if   e           k Disk    ReName(prp01:prp00)
     f                                     infds(dbarpfds)
     FWarpln    uf   e           k Disk    ReName(prp01:prp0n)
     FWarpp     uf   e             Disk    ReName(prp01:prp0p)
     FSdcmla    uf   e           k Disk    ReName(pcm01:pcm0a) Prefix(C2:2)
     Fwmthl     o    e             disk    ReName(pth01:pth00)
     Fwmchl5    if a e           k disk    ReName(pch01:pch05)
     fwmefl3    if   e           k disk    ReName(pef01:pef03)
     f                                     infds(effds0)
     Fstctl1    if   e           k disk
     F
     Fwaidl1    uf a e           k disk
     F
     Fwmbil2    if   e           k disk    ReName(pbi01:pbi02)
     F
     Fwdddl     if   e           k disk    ReName(pdd01:pdd00)
     f                                     infds(dbddlfds)
     Fssylf     if   e           k disk    ReName(swc01:swc00)
     f*** Beg Chg *** 05/09/23 *****************************************
     f***                                  Ignore(ssy01)
     F                                     ReName(ssy01:ssy00)
     f*** End Chg *** 05/09/23 *****************************************
     f                                     Ignore(spc01)
     f                                     Ignore(src01)
     f                                     Ignore(sbc01)
     f                                     Ignore(scs01)
     fwtddl2    if   e           k disk    ReName(pdd01:pdd02)
     f*** Beg Add *** 10/01/13 *****************************************
     fwmrwl     if   e           k Disk
     f*** Beg Add *** 10/01/13 *****************************************
     f***** beg add ***** 07/11/14 *********** 111WBL ********************
     fwdrel     if   e           k disk    rename(pre01:pre00)
     f***** end add ***** 07/11/14 ***************************************
     f*** Beg *** App security ****** Adhoc **********
     fsdofl1    if   e           k disk
     f*** End *** App security ****** Adhoc **********
     f***** Beg Add *** 09/01/15 *****************************************
     fwmldl1    if   e           k disk    rename(pld01:pld01)
     f***** End Add *** 09/01/15 *****************************************
      *** Begin Add *** 07/30/20 *****************************************
       DCL-F WTPSL1 KEYED;
      *** End   Add *** 07/30/20 *****************************************
      *** Begin Add *** 06/10/21 *****************************************
       Dcl-F Wsckl usage(*input) Keyed;
       Dcl-F Wsckl1sec disk usage(*input) keyed;
      *** End   Add *** 06/10/21 *****************************************
     D*WDPAP RRN
     D
     D dbpapfds        ds
     D WDPAPrrn              397    400b 0
     D
     D*WARPP RRN
     D
     D dbarpfds        ds
     D WARPPrrn              397    400b 0
     D
     D*WMRHP RRN
     D
     D dbmrhfds        ds
     D WMRHPrrn              397    400b 0
     D
     D*WDDDL RRN
     D
     D dbddlfds        ds
     D WDDDLrrn              397    400b 0
     D
     D*WMEFL RRN
     D
     D effds0          ds
     D  efrrn                397    400b 0
     D
     d* Load Action Panel procedure Prototype and variables
      /Copy sCopyClmPr
      /copy sPrDate
     d***  Begin Add  ***  04/12/13  **************************************
     d/copy sPrTable
     d***  End   Add  ***  04/12/13  **************************************
      /copy wPrGroup
      /copy wPrEmplr
      /copy wPrClaims
      //***  Begin Add  ***  08/05/20  ************************************
      /copy wPrClmPmt
      //***  End   Add  ***  08/05/20  ************************************
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
      //*** Beg Del *** 11/08/19 *******************************************
      //***/copy fPrCase
      //*** End Del *** 11/08/19 *******************************************
      /copy wPrAgent
      /copy wPrContact
      /copy wPrUsrPrms
      /copy wPrLoc
      /copy sPrString
      /copy sPrAtrium
      /copy sPrGenPop
      /copy wPrBank
      /copy wPrEft
      /copy wPrClmPop
      ***  Begin Add  ***  06/09/20  ****************************
      /copy sPrMask
      /copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
       //*** begin add ***  04/16/15  ****************************************
     d/copy sPrLetter
       //*** begin add ***  04/16/15  ****************************************
       //*** begin add ***  10/22/20  ****************************************
     d/copy wPrEpay
       //*** begin add ***  10/22/20  ****************************************
       //*** Begin Add ***  05/09/23  ****************************************
     d/copy wPrPmtPop
       //*** End   Add ***  05/09/23  ****************************************

     d* Proto-types

     d*** Beg *** App security ****** Adhoc **********
     DCheckSecurity    pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inCTYP                         1    const
     D inCLVL                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D inCTP2                         1    const
     D inCTP3                         1    const
     D inCTP4                         1    const
     D inCTP5                         1    const
     D inCTP6                         1    const
     d*** End *** App security ****** Adhoc **********
      *** Begin Add *** 07/30/20 *******************************************
     D inCLV2                         3    const
     D inCLV3                         3    const
     D inCLV4                         3    const
     D inCLV5                         3    const
     D inCLV6                         3    const
     D inCTP7                         1    const
     D inCTP8                         1    const
     D inCTP9                         1    const
     D inCTP0                         1    const
     D inCLV7                         3    const
     D inCLV8                         3    const
     D inCLV9                         3    const
     D inCLV0                         3    const
      *** End   Add *** 07/30/20 *******************************************
     D*** Beg Add *** 11/08/19 *********************************************
     DGetAdjState      pr                  EXTPGM('FA787R')
     D inCo#                          3  0
     D inFnd                          3  0
     D inFyr                          3  0
     D inCase                         7  0
     D OutAdjState                    2
     DGetFaeopRrn      pr                  EXTPGM('FA788R')
     D inCo#                          3  0
     D inFnd                          3  0
     D inFyr                          3  0
     D inCase                         7  0
     D inEob#                         9  0
     D OutFaeopRrn                    9
     D*** End Add *** 11/08/19 *********************************************
     d* Filter  procedure
     d Applyfilter     pr

      *** Beg Add *** 11/25/13 *************************************************
     d* Auto Letterse
     d AutoLetters     pr                  ExtPgm('WA049R')
     d  inRRNClm                      9  0
     d  ##rtcd                        1
     d  lostTimeHold                  1
     d  strClmSts                     1
      *** End Add *** 11/25/13 *************************************************

     d* Build tab header
     d BldTabHdr       pr

     d* Change Duplicate Payment Exclusion
     d ChangePmtExcl   pr
     d pmtRRN                         9  0

     d* Clear Filter
     d ClearFilter     pr

     d* Clear Subfiles
     d ClrSubFilesRsv  pr

     d* Clear Subfiles
     d ClrSubFilesPay  pr

     d* Check duplicate payment authority issues
     d ChkDupPmtAuth   pr              n
     D  inFromDate                    7  0
     D  inToDate                      7  0
     d*** End   Add *** 06/12/17 *******************************************

     d* Check if employer processing deductible
     d ChkEmpDed       pr

     d*** Begin Del *** 12/18/2012 *****************************************
      * Check if Hospital bill Record
     d*ChkHospPBill    pr
     d*** End   Del *** 12/18/2012 *****************************************

     d* Continue delete process
     d ContDelProc     pr

     d* Display main screen
     d DisplayMain     pr

     d* Edit request for payment
     d EdtRequest      pr

     d* FunRet  procedure
     d FunRet          pr

     d* Get the amount
     d GetAmount       pr
     d*** Begin  Add *** 02/19/15 **********************************

     d* Get the Delete Status Code
     d GetDelStatus    pr
     d*** End    Add *** 02/19/15 **********************************

     d* Get the summary info
     d GetSumInfo      pr

     d* Get Payee Information
     d GetPayee        pr

     d* Get  Payee Information
     d GetPayeeInfo    pr

     d* Load Payee Grid
     d GetPayeeName    pr

     d* Get Data for Deductible recovery reversal
     d GetRevAmt       pr

     d* Load Data is status grid
     d GetStsInfo      pr

     d* Load edit request screen
     d LodEdtReq       pr

     d* Load Payee
     d LoadPayee       pr

     d* Load payment summary
     d LoadPmtSum      pr

     d* Load Benefit Chart
     d LodBenChart     pr

     d* Load History grid
     d LodHstInfo      pr

     d* Load Reserve grid
     d LodRsvInfo      pr

     d* Load Request grid
     d LodReqInfo      pr

     d* Load Deductible History Info
     d LodDedInfo      pr

     d* Load status grid
     d LodStsInfo      pr

     d* Process filter selection
     d ProcessFilter   pr

     d* Process duplicate payment exclusion
     d ProcDupEx       pr

     d* Process Change Record
     d ProcChgRec      pr

     d* Process EFT Information
     d ProcEftInfo     pr

     d* Process release process
     d ProcRlsRqs      pr

     d* Process pending/release process
     d ProcPenRls      pr

     d* Process delete process for Pending check request
     d ProcReqDel      pr

     d* Save the changes
     d SaveEdtReq      pr

     d* Change the claim status
     d Selchgclm       pr

     d* Load History grid
     d SrChg           pr

     d* SumCLP  procedure
     d SumCLP          pr

     d* Reserve Tab Grids
     d TabReserveInfo  pr

     d* Payment Tab Grids
     d TabPaymentInfo  pr

     d* Request Tab Grids
     d TabRequestInfo  pr

     d* Deductible Tab Grids
     d TabDedHistory   pr

     d* EFT Info Tab Grids
     d TabEftInfo      pr

     d* Validate for EFT processing
     d ValidEFT        pr

     d* Validate for displaying delete link
     d ValidDspDel     pr

     d* Validate conditions on Change of Status
     d ValidateSts     pr

     d* Validate edit request selection
     d ValidEdtReq     pr

     d* Validate filter selection
     d ValidFltr       pr

     d* Write darts
     d WriteDarts      pr

     d*** Beg Add *** 10/25/12 *************************************************
     d* Validate Invoice #
     d ValidInv        pr                  Extpgm('WA081R')
     d RPco#                          3  0
     d RPfnd                          3  0
     d RPfyr                          3  0
     d RPcase                         7  0
     d RPseq                          5  0
     d RPfein                         9  0
     d scInv#                        10
     d xxFkey                         2
     d isValid1                       1
     d*** End Add *** 10/25/12 *************************************************
     d* Call external programs

     D GetName         pr                  ExtPgm('SRNAME')
     D ##Last                              like(##Last)
     D ##Frst                              like(##Frst)
     D ##Term                              like(##Term)

     D GetAut          pr                  ExtPgm('WA067R')
     D ##co                                like(##co)
     D ##fnd                               like(##fnd)
     D ##fyrc                              like(##fyrc)
     D ##casec                             like(##Casec)
     D ##user                              like(q1user)
     D ##rtcd                              like(##rtcd)

     D GetSecAut       pr                  ExtPgm('SRCCSC')
     D ##utyp                              like(##utyp)
     D ##ulvl                              like(##ulvl)
     D ##ctyp                              like(##ctyp)
     D ##clvl                              like(##clvl)
     D ##exec                              like(##exec)
     D ##res                               like(##res)
     D ##ctp2                              like(CXdpp2)
     D ##ctp3                              like(CXdpp2)
     D ##ctp4                              like(CXdpp2)
     D ##ctp5                              like(CXdpp2)
     D ##ctp6                              like(CXdpp2)

     d GetEombPBTyp    pr                  ExtPgm('FA033R')
     d ##co#                               like(RPco#)
     d ##wcf#                              like(PAfnd)
     d ##fyr                               like(PAfyr)
     d ##case                              like(PAcase)
     d ##eob#                              like(PAeob#)
     d ##rcn#                              like(PAfnd)
     d ##fein                              like(PAFein)
     d ##mod                               like(PAmod)
     d ##chk#                              like(PAfein)
     d ##othr                              like(PAamt)
     d ##ast                               like(PAtrcd)
     d ##binv                              like(PAfein)
     d ##ninv                              like(PAfein)
     d ##sub#                              like(PAbasq)
     d ##fnd#                              like(PAfnd)
     d ##rrn                               like(PAfein)
     d ##rtcd                              like(PAtype)
     d ##acct                              like(PAdesc)
     d ##btyp                              like(PAtype)

      ****** beg add ****** 09/12/13 *************************
      * Check Object
     dGetObject        pr                  extpgm('SRCHKOBJ')
     d outName                             like(outName)
     d outType                             like(outType)
     d outRtcd                             like(outRtcd)
      ****** end add ****** 09/12/13 *************************

     D ProcDedRec      pr                  ExtPgm('W8025R')
     D ##co                                like(##co)
     D ##fnd                               like(##fnd)
     D ##fyrc                              like(##fyrc)
     D ##casec                             like(##Casec)
     D ##seq#                              like(##seq#)
     D ##amt#                              like(##amt#)
     D ##rtcd                              like(##rtcd)

     D GetClmInfo      pr                  ExtPgm('WA006R')
     D ##co#                               like(RPco#)
     D ##fnd#                              like(RPfnd)
     D ##fyrc#                             like(RPfyr)
     D ##casec#                            like(RPcase)
     D ##strDtl1                           like(##strDtl1)
     D ##strdtl2                           like(##strDtl2)

     D ValidEdt        pr                  ExtPgm('WA014R')
     D ##co#                               like(RPco#)
     D ##fnd#                              like(RPfnd)
     D ##fyrc#                             like(RPfyr)
     D ##casec#                            like(RPcase)
     D ##grp                               like(RPpgtp)
     D ##ptdt                              like(scPtDtEdt)
     D ##rcvd                              like(scPtDtEdt)
     D ##frdt                              like(scPtDtEdt)
     D ##todt                              like(scPtDtEdt)
     D ##retp                              like(RPretp)
     D ##recd                              like(RPrecd)
     D ##br2                               like(RPbr2)
     D ##br7                               like(RPbr7)
     D ##sts                               like(RPsts)
     D ##ltcd                              like(RPltcd)
     D ##day                               like(RPdays)
     D ##rc                                like(RPltcd)
     D ##eob                               like(RPeob#)
     D ##amt                               like(RPamt)
     D ##rcn                               like(##rcn)
     D ##ekey                              like(##ekey)

     D ValidDup        pr                  ExtPgm('WA019R')
     D ##co#                               like(RPco#)
     D ##fnd#                              like(RPfnd)
     D ##fyrc#                             like(RPfyr)
     D ##casec#                            like(RPcase)
     D ##seq                               like(RPseq#)
     D ##from                              like(##from)
     D ##to                                like(##to)
     D ##amt                               like(RPamt)
     D ##recd                              like(RPrecd)
     D ##fein                              like(RPfein)
     D ##fndnam                            like(##fndnam)
     D ##clmnam                            like(##clmnam)
     D ##prvnam                            like(##prvnam)
     D ##inerr                             like(##inerr)
     D ##retp                              like(RPretp)
     D ##mod                               like(##mod)
     D ##rtcd                              like(##rtcd)

      ****** beg add ******** 01/24/14 ***************************
     D ValidPmtEdt     pr                  ExtPgm('WA011R')
     D ##co#                               like(RPco#)
     D ##fnd#                              like(RPfnd)
     D ##fyrc#                             like(RPfyr)
     D ##casec#                            like(RPcase)
     D ##rc                                like(RPltcd)
      ****** end add ******** 01/24/14 ***************************

     D ValidRls        pr                  ExtPgm('WA012R')
     D ##co#                               like(RPco#)
     D ##fnd#                              like(RPfnd)
     D ##fyrc#                             like(RPfyr)
     D ##casec#                            like(RPcase)
     D ##trcd                              like(RPtrcd)
     D ##fein                              like(RPfein)
     D ##mod                               like(##mod)
     D ##rc                                like(RPltcd)
     D ##rcd                               like(RPrecd)
     D ##rtp                               like(RPretp)
     D ##typ                               like(##typ)

     D ProcDarts       pr                  ExtPgm('WA018R')
     D*** Begin Chg *** 02/27/18 *******************************
     D*##ckey                              like(CMkey)
     D ##ckey                              like(C2key)
     D*** End   Chg *** 02/27/18 *******************************
     D ##cseq                              like(RPdseq)
     D ##rtcd                              like(##rtcd)

     D ValidClm        pr                  ExtPgm('SRPRCC')
     D ##co#                               like(RPco#)
     D ##fnd#                              like(RPfnd)
     D ##fyrc#                             like(RPfyr)
     D ##casec#                            like(RPcase)
     D ##recd                              like(RPrecd)
     D ##type                              like(##type)
     D ##amt                               like(RPamt)

     D ValidAmt        pr                  ExtPgm('WA013R')
     D ##co#                               like(RPco#)
     D ##fnd#                              like(RPfnd)
     D ##fyrc#                             like(RPfyr)
     D ##casec#                            like(RPcase)
     D ##eob                               like(RPeob#)
     D ##seq                               like(RPseq#)
     D ##amt                               like(RPamt)
     D ##recd                              like(RPrecd)
     D ##retp                              like(RPretp)
     D ##rc                                like(RPltcd)
     D ##amnt                              like(RPamt)
     D ##rcns                              like(##rcns)
     D ##trcd                              like(RPtrcd)

     D****** beg add ****** 05/20/13 ************************************
     D GetSystem       pr                  ExtPgm('SA001R')
     D ##Wcif                              like(##rtcd)
     D ##Mcif                              like(##rtcd)
     D ##Wcin                              like(##rtcd)
     D****** end add ****** 05/20/13 ************************************

     D* System Data Structures

     D dbfds           ds
     D  flstcm           *status
     D  dbrrn                397    400b 0

     D dbfds2          ds
     D  dbrrn2               397    400b 0

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)

     D                 ds
     D  intzip                 1      9  0
     D  intzip1                1      5  0
     D  intzip2                6      9  0

     D*Variables

     D @@RRN           s              9
     D @@Mode          s              1
     D @@Next          s             10
     D @@TabIndex      s              5
     D @@inType        s              1    inz
     D outClientID     s              3    inz('000')
     D addMode         s               n   inz(*off)
     D chgMode         s               n   inz(*off)
     D dspMode         s               n   inz(*off)
     D firstTime       s               n   inz(*off)
     D first           s               n   inz(*off)
     D fstTime         s               n   inz(*off)
      ******* beg add ****** 12/01/14 *****************************************
     d intIndTotal     s             13  2
     d intMedTotal     s             13  2
     d isWrkSht        s               n
     d strRrn#         s              9
     d strMsg          s            256
     d strTitle        s             50
      ******* end add ****** 12/01/14 *****************************************
      *** Begin Add *** 10/22/20 *******************************************
     d #2Co#           s              3
     d #2Fnd           s              3
     d #2Appl          s              2    Inz('WC')
     d #2Date          s              8
     d #2Call          s              1    Inz(' ')
     d #2eftC          s              1
     d #2HdCy          s              1
     d #2ChkQ          s             10
     d #2Ftyp          s             10
     d #2EfDt          s              7
     d #2CpyQ          s             10    Inz
     d #2CkTp          s             10    Inz
     d #2Coid          s             20    Inz
     d #2RtCd          s              1    Inz
     d outAcct         s             20    Inz
      *** End   Add *** 10/22/20 *******************************************

     D inRRNClm        s              9  0
     D inMode          s              1

      ****** beg add ****** 09/12/13 ********************************
     d ##FltrTyp       s              1    DTAARA(TYPDTAARA)
     d outName         s             10
     d outType         s              7
     d outRtcd         s              1
      ****** end add ****** 09/12/13 ********************************
     D ##Last          s             56
     D ##Frst          s             15
     D ##UTyp          s              1
     D ##UlVl          s              3
     D ##CTyp          s              1
     D ##ClVl          s              3
     D ##Exec          s              1
     D ##Res           s              1
     d ##eftMod        s              1
     d ##eftTyp        s              1
     d ##eftRrn        s              9  0
     d ##cmrrn         s              9  0
     d***** beg add ****** 05/20/13 *********************************
     D ##Wcif          s                   like(##rtcd)
     D ##Mcif          s                   like(##rtcd)
     D ##Wcin          s                   like(##rtcd)
     d***** end add ****** 05/20/13 *********************************
     D intDat          s              7  0
     D intTrdt         s              7  0
     D intDays         s              5  0
     D intDate         s              8  0
     D intAdate        s              7  0
     D intEfDt         s              7  0
     D intWk           s              4  0
      **** Beg Chg *** 03/14/13 *****************************************
     D***** intWkDays       s              2  0
     D intWkDays       s              5  0
      **** End Chg *** 03/14/13 *****************************************
     D intTrnWk        s              5  0
     D intValWk        s              5  0
     D intTrnDys       s              5  0
     D intValDys       s              5  0
     D intDtlDes2      s                   Like(GTDesc)
     D intDtlDes7      s                   Like(GTDesc)
     D intsvAmt        s                   like(RPamt) inz
     D intsvFdt        s                   like(RPfrdt)
     D intsvTdt        s                   like(RPtodt)
     D intTime         s                   like(RHtime)
     D kkTrDt          s                   like(PAtrDt)
     D intwk10         s             10  0
     D intFromDate     s              7  0
     D intToDate       s              7  0
     D inTime          s                   like(CHtime)
     D inEfdt          s                   like(CHefdt)
     D intDti          s              8  0
     D intDto          s             10
     D intErr          s              1
     D intfFmt         s              8
     D inttFmt         s              8
     D intTab          s              5  0
     D strCLTID        s              3
     d strSvEftc       s                   like(RPeftc)
     D strFein         s              9
     D strAppl         s              2
     D strCtrd         s             15
     D strRmCd         s              5
     D strRvCd         s                   like(RCRvCd)
     D strEdiM         s              1  0
     D strsvSts        s                   like(RPsts)
     D strsvStat       s                   like(RPstat)
     D strMode         s              1
     D strDtlAdr2      s             40
     D strDtlCsz       s             40
     D strDtlRmk       s             30
     D strDtlRmk1      s             30
     D strDtlRmk2      s             30
     D strDtlRmk3      s             30
     D strClmSts       s              1
     D strOldStat      s                   like(RPstat)
     D strNewStat      s                   like(RPstat)
     D strOldSts       s                   like(RPsts)
     D strNewSts       s                   like(RPsts)
     D strSts          s                   like(RPsts)
     D strSort         s                   like(RPsort)
     d strKey          s             30
     D strKy           s             10
     D strLtFl         s                   like(CMltfl)
     D strWk11         s             11  2
     D strCheckTitle   s             50
     D strEobTitle     s             50
     D strEobRrn       s              9
     D strAdjstate     s              2
     D strQueue        s             10
     D strPmtRrn       s              9
     D decCcHg         s              9  2
     D decMcHg         s              9  2
     D decLcHg         s              9  2
     D decEcHg         s              9  2
     D decCrSv         s              9  2
     D decMrSv         s              9  2
     D decLrSv         s              9  2
     D decErSv         s              9  2
     D decCiNc         s              9  2
     D decMiNc         s              9  2
     D decLiNc         s              9  2
     D decEiNc         s              9  2
     D decToTi         s             10  2
     D decAmt          s                   like(PAAmt)
     D decTrsv         s             11  2
     D decTamt         s             11  2
     D isError         s               n
     D isErrorS        s               n
     D isValid         s               n
     D*** Beg Add *** 10/25/12 *************************************************
     D isValid1        s              1    Inz(*Zero)
     D xxFkey          s              2    Inz(*Blanks)
     D*** End Add *** 10/25/12 *************************************************
     D*** Beg Add ***** 11/02/16 *************************************
     D isSecErr        s               n
     D*** End Add ***** 11/02/16 *************************************
     D isInvalid       s               n
     D isRels          s               n
     D savTime         s                   like(CMactm)
     D savDate         s                   like(CMacdt)
     D sysDt7          s              7  0
     Di                s              2  0
     DX                s              2  0
     D fnRet#          s             11  2
     D retPr#          s              5  2

     D ##Key           s                   like(NAkey)
     D ##Key#          s                   like(RSrtcd)
     D ##Term          s              1
     D ##Ekey          s             31
     D ##Ekey1         s             30
     d ##date          s              7
     d ##eftc          s              1
     D ##Rtcd          s              1
     D ##Co            s              3
     D ##fnd           s              3
     D ##seq#          s              5
     D ##appl          s              2
     D ##cseq          s                   like(RPdseq)
     D ##ctp2          s                   like(CXdpp2)
     D ##ctp3          s                   like(CXdpp3)
     D ##ctp4          s                   like(CXdpp4)
     D ##ctp5          s                   like(CXdpp5)
     D ##ctp6          s                   like(CXdpp2)
     D ##dta           s             75
     D ##cktp          s             10
     d ##ptyp          s              1
     d ##pre           s              1
     d ##inerr         s               n
     D ##trdt          s              7
     D ##SvEob#        s                   like(RPeob#)
     D ##ftyp          s                   like(BIftyp)
     D ##chkq          s                   like(BIcpyq)
     D ##amt#          s              9
     D ##TbId          s                   like(GTtbid)
     D ##Fyrc          s              3
     D ##Casec         s              7
     D ##User          s             10
     D ##err           s              1
     D ##seq           s                   like(RPseq#)
     D ##amnt          s                   like(RPamt)
     D ##rcns          s              3  0
     D ##trcd          s                   like(RPtrcd)
     D ##strDtl1       s             79
     D ##strdtl2       s             79
     D ##from          s              8  0
     D ##to            s              8  0
     D ##fein          s                   like(RPfein)
     D ##fndnam        s             51
     D ##clmnam        s             36
     D ##prvnam        s             36
     D ##mod           s                   like(RPmod)
     D ##co#           s                   like(RPco#)
     D ##fnd#          s                   like(RPfnd)
     D ##fyrc#         s                   like(RPfyr)
     D ##casec#        s                   like(RPcase)
     D ##grp           s                   like(RPpgtp)
     D ##ptdt          s                   like(scPtDtEdt)
     D ##rcvd          s                   like(scPtDtEdt)
     D ##frdt          s                   like(scPtDtEdt)
     D ##todt          s                   like(scPtDtEdt)
     D ##retp          s                   like(RPretp)
     D ##recd          s                   like(RPrecd)
     D ##rcd           s                   like(RPrecd)
     D ##rtp           s                   like(RPretp)
     D ##typ           s              1
     D ##type          s              1
     D ##br2           s                   like(RPbr2)
     D ##br7           s                   like(RPbr7)
     D ##sts           s                   like(RPsts)
     D ##ltcd          s                   like(RPltcd)
     D ##day           s                   like(RPdays)
     D ##rc            s                   like(RPltcd)
     D ##eob           s                   like(RPeob#)
     D ##amt           s                   like(RPamt)
     D ##diff          s                   like(RPamt)
     D ##rcn           s              3  0
     D ##dcex1         s                   like(ELdcex)
     D ##dcex2         s                   like(EL2dex)
     D ##ddcd          s                   like(ELdcde)
     D ##ddef          s                   like(ELdcef)
     D ##st            s                   like(DDst)
     d ##wcf#          s                   like(PAfnd)
     d ##fyr           s                   like(PAfyr)
     d ##case          s                   like(PAcase)
     d ##eob#          s                   like(PAeob#)
     d ##rcn#          s                   like(PAfnd)
     d ##chk#          s                   like(PAfein)
     d ##othr          s                   like(PAamt)
     d ##ast           s                   like(PAtrcd)
     d ##binv          s                   like(PAfein)
     d ##ninv          s                   like(PAfein)
     d ##sub#          s                   like(PAbasq)
     d ##rrn           s                   like(PAfein)
     d ##acct          s                   like(PAdesc)
     d ##btyp          s                   like(PAtype)
     d flgRcdUpdated   s              1
     d SVdt            s              7  0
     d SVtime          s              6  0
     d*** Beg *** App security ****** Adhoc **********
     D userLevel       s              3  0 inz
     D userType        s                   inz like(ofty01)
     D flgRes          s              1    inz
     D flgofEnv        s              1    inz
     D svShwEftLnk     s              1    inz
     d*** End *** App security ****** Adhoc **********
       //*** Begin Add *** 06/10/21 *******************************
       Dcl-S inMICRauth char(10)           inz('CHGMICR');
       Dcl-Ds KeyWSCKL1SEC likerec(PCKS01:*KEY);
       //*** End  Add *** 06/10/21 ********************************
      *** Beg Chg ********* 11/25/13 ***********************************
     D lostTimeHold    s              1
      *** End Chg ********* 11/25/13 ***********************************
      *** Beg Chg ********* 10/27/15 ***********************************
     d intTotIncd      s             13  2
      *** End Chg ********* 10/27/15 ***********************************
      *** Beg Chg ********* 05/09/23 ***********************************
     D flgNewPay       s               n   inz
      *** End Chg ********* 05/09/23 ***********************************

     D szds            ds
     D sz              s              1    Dim(13)
     D st              s              2    Dim(1)
     D nf              c                   const('NOT FOUND')
     D NOTFND          c                   CONST('Code Not Found')
     D rsnChg          c                   Const('Claim Closed')
     D lo              c                   'abcdefghijklmnopqrs-
     D                                     tuvwxyz'
     D up              c                   'ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ'
     D rlsd            s               n   Dim(9999) Inz(*Off)

      *  Payment Transaction Code Arrays
     D CD              S              2    DIM(300)
     D DS              S             40    DIM(300)
     D AM              S             11  2 DIM(300)
     D WK#             S              5  0 DIM(300)
     D DA              S              5  0 DIM(300)
      *  Payment Value 1 Arrays
     D CD3             S              2    DIM(300)
     D DS3             S             40    DIM(300)
     D AM3             S             11  2 DIM(300)
     D WK3             S              5  0 DIM(300)
     D DA3             S              5  0 DIM(300)
     D SIZE            C                   CONST(300)
     D XX              S              4  0

     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  RHrt01                 1      2
     D  RHrt02                 3      4
     D  RHrt03                 5      6
     D  RHrt04                 7      8
     D  RHrt05                 9     10
     D  RHrt06                11     12
     D  RHrt07                13     14
     D  RHrt08                15     16
     D  RHrt09                17     18
     D  RHrt10                19     20

     D                 DS
     D  SU                     1     90  2
     D                                     DIM(10)
     D  RHsu01                 1      9  2
     D  RHsu02                10     18  2
     D  RHsu03                19     27  2
     D  RHsu04                28     36  2
     D  RHsu05                37     45  2
     D  RHsu06                46     54  2
     D  RHsu07                55     63  2
     D  RHsu08                64     72  2
     D  RHsu09                73     81  2
     D  RHsu10                82     90  2
      *
     D                 ds
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMrr01                 1      9  2
     D  CMrr02                10     18  2
     D  CMrr03                19     27  2
     D  CMrr04                28     36  2
     D  CMrr05                37     45  2
     D  CMrr06                46     54  2
     D  CMrr07                55     63  2
     D  CMrr08                64     72  2
     D  CMrr09                73     81  2
     D  CMrr10                82     90  2
      *
     D                 DS
     D  RT#                    1     20
     D                                     DIM(10)
     D  CMrt01                 1      2
     D  CMrt02                 3      4
     D  CMrt03                 5      6
     D  CMrt04                 7      8
     D  CMrt05                 9     10
     D  CMrt06                11     12
     D  CMrt07                13     14
     D  CMrt08                15     16
     D  CMrt09                17     18
     D  CMrt10                19     20
      *
     D                 ds
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMar01                 1      9  2
     D  CMar02                10     18  2
     D  CMar03                19     27  2
     D  CMar04                28     36  2
     D  CMar05                37     45  2
     D  CMar06                46     54  2
     D  CMar07                55     63  2
     D  CMar08                64     72  2
     D  CMar09                73     81  2
     D  CMar10                82     90  2
      *
     D                 ds
     D  RC                     1     90  2 inz
     D                                     DIM(10)
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
      *
     D                 DS                  INZ
     D  TP                     1     10
     D                                     DIM(10)
     D  SCtp01                 1      1
     D  SCtp02                 2      2
     D  SCtp03                 3      3
     D  SCtp04                 4      4
     D  SCtp05                 5      5
     D  SCtp06                 6      6
     D  SCtp07                 7      7
     D  SCtp08                 8      8
     D  SCtp09                 9      9
     D  SCtp10                10     10
      *
     D                 DS                  inz
     D  IR                     1     20
     D                                     DIM(10)
     D  scReserve1             1      2
     D  scReserve2             3      4
     D  scReserve3             5      6
     D  scReserve4             7      8
     D  scReserve5             9     10
     D  scReserve6            11     12
     D  scReserve7            13     14
     D  scReserve8            15     16
     D  scReserve9            17     18
     D  scReserv10            19     20
      *
     D                 DS
     D                                     inz
     D  IS                     1     90  2
     D                                     DIM(10)
     D  scReservC1             1      9  2
     D  scReservC2            10     18  2
     D  scReservC3            19     27  2
     D  scReservC4            28     36  2
     D  scReservC5            37     45  2
     D  scReservC6            46     54  2
     D  scReservC7            55     63  2
     D  scReservC8            64     72  2
     D  scReservC9            73     81  2
     D  scReserC10            82     90  2
      *
     D                 DS
     D  FRS                    1     20
     D                                     DIM(10)
     D  FNrs01                 1      2
     D  FNrs02                 3      4
     D  FNrs03                 5      6
     D  FNrs04                 7      8
     D  FNrs05                 9     10
     D  FNrs06                11     12
     D  FNrs07                13     14
     D  FNrs08                15     16
     D  FNrs09                17     18
     D  FNrs10                19     20
     D                 DS
     D  TI                     1     90  2
     D                                     DIM(10)
     D  SCti01                 1      9  2
     D  SCti02                10     18  2
     D  SCti03                19     27  2
     D  SCti04                28     36  2
     D  SCti05                37     45  2
     D  SCti06                46     54  2
     D  SCti07                55     63  2
     D  SCti08                64     72  2
     D  SCti09                73     81  2
     D  SCti10                82     90  2
     D
     D                 DS                  INZ
     D  qcKey                  1     40
     D  qcCo#                  1      3  0
     D  qcFnd                  4      6  0
     D  qcFyr                  7      9  0
     D  qcCase                10     16  0
     D
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)
     D  ARHrt01                1      2
     D  ARHrt02                3      4
     D  ARHrt03                5      6
     D  ARHrt04                7      8
     D  ARHrt05                9     10
     D  ARHrt06               11     12
     D  ARHrt07               13     14
     D  ARHrt08               15     16
     D  ARHrt09               17     18
     D  ARHrt10               19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)
     D  ARHsu01                1      9  2
     D  ARHsu02               10     18  2
     D  ARHsu03               19     27  2
     D  ARHsu04               28     36  2
     D  ARHsu05               37     45  2
     D  ARHsu06               46     54  2
     D  ARHsu07               55     63  2
     D  ARHsu08               64     72  2
     D  ARHsu09               73     81  2
     D  ARHsu10               82     90  2
      *
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
      *
     D                 ds
     Ddsdate#                         8  0
     Dyy                       1      4
     Dmm                       5      6
     Ddy                       7      8
      *
     DdsFilter         ds                  likerec(rcdFilter)
      *
     D*** Begin Chg *** 12/19/18 *******************************
     D*wk2******       S              3  0 dim(9999)
     D*da2******       S              1  0 dim(9999)
     D*am2******       S             11  2 dim(9999)
     D*cdp******       S              2    dim(9999)
     D wk2             S              3  0 dim(99999)
     D da2             S              1  0 dim(99999)
     D am2             S             11  2 dim(99999)
     D cdp             S              2    dim(99999)
     d pmtCount        S              5  0
     D*** End   Chg *** 12/19/18 *******************************
     D wk              S              2    dim(10)
     D pt              S              2    dim(10)
     D pa              S              9  2 dim(10)
     D rmk             S             49    dim(4) ctdata perrcd(1)
      *
     D                 DS
     D  DATA                   1     75
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D  QXSEQ#                30     34  0
     D  QXEOB#                35     43
     D  QXFEIN                44     52  0
     D  QXMOD                 53     55  0
     D  QXDSQ#                56     60  0
      *
      *  DARTS Key Field structure
     D                 DS
     D  ##CKEY                 1     40
     D  CCO#                   1      3
     D  CFND                   4      6
     D  CFYR                   7      9
     D  CCASE                 10     16
     d***  Begin Add  ***  06/23/21  ****************************************
     d OutCmRrnTm      s              9  0
     d***  End   Add  ***  06/23/21  ****************************************
     D*
     d***  Begin Add  ***  04/04/23  ****************************************
     d svrrnReq        s              9  0 inz
     d***  End   Add  ***  04/04/23  ****************************************
     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Next
     C                   Parm                    @@TabIndex
     C                   Parm                    @@Type            1
      *
     C     keyWMECL1     Klist
     C                   Kfld                    EMco#
     C                   Kfld                    EMfnd
     C                   Kfld                    EMemp#
     C                   Kfld                    EMdiv
     C                   Kfld                    kkPcnt
      *
     C     keyWTDDL      Klist
     C                   Kfld                    EMco#
     C                   Kfld                    EMfnd
     C                   Kfld                    ##st
     C                   Kfld                    ##ddcd
     C                   Kfld                    ##ddef
      *
     C     keyWTDDL2     Klist
     C                   Kfld                    EMco#
     C                   Kfld                    EMfnd
     C                   Kfld                    ##st
     C                   Kfld                    ##ddcd
      *
      * EFT Account File Key List
     c     keyWMEFL3     klist
     c                   kfld                    CMco#
     c                   kfld                    CMfnd
     c                   kfld                    CMfyr
     c                   kfld                    CMcase

     C     keyWMALL      Klist
     C                   Kfld                    EMco#
     C                   Kfld                    EMfnd
     C                   Kfld                    EMemp#
     C                   Kfld                    EMdiv
     C                   Kfld                    CMctg1
      *
     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

     C     keyWMFNL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
      *
      *
     C     keyWTABL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    ##Key
      *
     C     keyWMRSL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    ##Key#
      *
     C     keyWDPAL      Klist
     C                   Kfld                    CMCo#
     C                   Kfld                    CMFnd
     C                   Kfld                    CMFyr
     C                   Kfld                    CMCase
      *
     C     key1WDPAL5    Klist
     C                   Kfld                    CMCo#
     C                   Kfld                    CMFnd
     C                   Kfld                    CMFyr
     C                   Kfld                    CMCase
      *
     C     key2WDPAL5    Klist
     C                   Kfld                    CMCo#
     C                   Kfld                    CMFnd
     C                   Kfld                    CMFyr
     C                   Kfld                    CMCase
     C                   Kfld                    kkTrDt

     C     keyWDACL      Klist
     C                   Kfld                    CMCo#
     C                   Kfld                    CMFnd
     C                   Kfld                    CMFyr
     C                   Kfld                    CMCase

     C     keyWDACL#     Klist
     C                   Kfld                    CMCo#
     C                   Kfld                    CMFnd
     C                   Kfld                    CMFyr
     C                   Kfld                    CMCase
     C                   Kfld                    EFseq#

     C     keyWDDDL      Klist
     C                   Kfld                    CMCo#
     C                   Kfld                    CMFnd
     C                   Kfld                    CMFyr
     C                   Kfld                    CMCase

     C     keyWmrhl5     klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase
     C                   kfld                    intEfDt
     C                   KFLD                    intTime
     C*
     C     keyWmrhl5#    klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase

     C     keyWDELL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMemp#
     C                   kfld                    CMdiv
     C                   kfld                    CMfyr

     C     keyWARPL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase

     C     keyWARPLN     klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase
     C                   kfld                    ##SvEob#

     C     keyWMUPL      klist
     C                   kfld                    q1user

     C     keyWDRCL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    strRvCd
     C                   kfld                    sysdt7
      *
     C     keyWDRCL#     klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    strRvCd
      *
     C     keySDCML4     Klist
     C                   KFLD                    QCkey
      *
     C     keyWMCDL      Klist
     C                   Kfld                    CMss#
      *
     C     keyWMCXL#     klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
      *
     C     keyWMADL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    strKy
      *
     C     keySTCTL1     klist
     C                   kfld                    strRmCd
      *
     C     keyWMCHL5     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    inEfdt
     C                   Kfld                    inTime

     C     keyWMCHL#     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     keyWDF1L      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
      *
     C     dtcymd1       plist
     C                   parm                    intDti
     C                   parm      *blanks       intfFmt
     C                   parm      '*CYMD'       inttFmt
     C                   parm                    intDto
     C                   parm                    intErr
      *
     C     keyWMBIL2     klist
     C                   kfld                    Cmco#
     C                   kfld                    ##appl
     C                   kfld                    Cmfnd
     C                   kfld                    PAcktp
     C                   kfld                    PAtrdt
      *
     C     keyWMBIL2#    klist
     C                   kfld                    Cmco#
     C                   kfld                    ##appl
     C                   kfld                    Cmfnd
     C                   kfld                    PAcktp
      *
      /Free

       // Perform the intial start Processing
       exsr  ProgramStart ;

       // Load the action panel for both tabs
       LoadActPan();

       // Load data in all the tabs
       TabReserveInfo();
       TabRequestInfo();
       TabDedHistory();
       TabEftInfo();

       // Display the screen

       // Dow (AddChgR = *on) or (ChgClmSts = *on) or (scFilter = *on) or
       // (scClrFltr = *on) or (scDedRec = *on) or (scDupPmtE = *on) or
       // (scChkFlag = *on) or (scEob# = *on) or (scReqEdit = *on) or
       // (scPenRls = *on) or (scReqDel = *on);

       DoU scMenuRsp <> *Blanks;
          TabPaymentInfo();
          DisplayMain();

          If refrshData=*On;
             // Load data in all the tabs
             TabPaymentInfo();
             TabReserveInfo();
             TabRequestInfo();
             TabDedHistory();
             TabEftInfo();
             refrshData=*Off;
          EndIf;

          Select;
       //*** begin chg ***  01/07/16  ****************************************
       //*** begin add ***  04/16/15  ****************************************
       //*****   When btnDocWndw = *on;
       //*****      btnDocWndw = *off;
       //*****      DocWindow(scActRRN:'CS');
       //*** end add *****  04/16/15  ****************************************
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;
       //*** end chg *****  01/07/16  ****************************************
          When TabClick = *On;
            TabClick = *Off;

          When scMenuRsp <> *Blanks;
            @@next = scMenuRsp;
            // @@tabIndex = '00000';
            Leave;

          //******* beg add ******** 12/01/14 ***************************
          When isWrkSht = *on and AddChgR = *on;
             AddChgR = *off;
             refrshData = *on;

             strMsg = 'The Claim has been opened as a Reserve Worksheet in +
              another tab. Once the reserve worksheet is finished, click OK +
              to reload the screen.';

             PopOneBtn(strMsg:'Claim Reserve Worksheet Open');

             // Refresh data in all the tabs
             TabPaymentInfo();
             TabReserveInfo();
             TabRequestInfo();
             TabDedHistory();
             TabEftInfo();
          //******* end add ******** 12/01/14 ***************************

          When AddChgR = *on;
             SelChgRes(inRRNClm:CMstat);
             TabReserveInfo();

          When ChgClmSts = *on;
             Selchgclm();

          When scFilter = *on;
             Applyfilter();

          When scClrFltr = *on;
             ClearFilter();

          Other;
             ProcChgRec();

          Endsl;

       Enddo;

       ExSr  ProgramEnd;

       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;


          If %parms()>0;
             inRRNClm = %int(@@RRN);
             inMode = @@Mode;
          Else;
             inMode='C';
          EndIf;
      //**** Begin Add  ***  06/23/21  ****************************************
          OutCmRrn = inRRNClm;
      //**** End Add  ***  06/23/21  ****************************************

          //*** Begin Add *** 01/19/18 ***********************
          Reset rlsd;
          //*** End   Add *** 01/19/18 ***********************

          // Set the initial tab
          TabIndex  = 0;
          If %parms()>3;
             tabIndex=%Dec(@@TabIndex:5:0);
          Else;
             tabIndex=0;
          EndIf;

          Chain(n) inRRNClm dbrcdf;
          GetClientID(outClientID);
          SflCsrRrnP = 1;
          //TabIndex  = 0;

          If CMltFl='R';
             shwRsvAdd=*Off;
          Else;
             shwRsvAdd=*On;
          EndIf;

          showClmSts=*On;
          shwAddEft=*On;
          shwEftLnk=*On;
          scRowNum=1;

          Select;
          When inMode = 'D';
             shwRsvAdd=*Off;
             showClmSts=*Off;
             shwAddEft=*Off;
             shwEftLnk=*Off;
             dspMode = *on;
          When inMode = 'A';
             addMode = *on;
          When inMode = 'C';
             chgMode = *on;
          EndSl;
          scActMode = inMode;

          //***  Begin Add  ***  04/12/13  ********************************
          // Check for EFT module installed
          isShowEFT = *on;
          isMsgEFT  = *off;
          If not EFTInstalled;
             shwAddEFT = *off;
             shwEFTLnk = *off;
             isShowEFT = *off;
             isMsgEFT  = *on;
          Endif;
          //***  End   Add  ***  04/12/13  ********************************

          // Set the Action Panel Height
          APnlHeight=1550;

          //*** Beg Add *** 05/09/23 *******************************************
          // Verify New Payment System (Install Flag is blank)
          Chain(n) ('SY') ssy00;
          If %Found(SSYLF) and %subst(SYFL16:3:1) = ' ';
             flgNewPay = *on;
          Else;
             flgNewPay = *off;
          Endif;
          //*** End Add *** 05/09/23 *******************************************

          // Retrieve the Client ID
          GetClientID(strCLTID);

          //*** Beg Add *** 10/25/12 *******************************************
          //*** Beg Chg *** 08/15/19 *******************************************
          //***If strCLTID = '114';
          If strCLTID = '114' or strCltId = '152';
          //*** End Chg *** 08/15/19 *******************************************
             dspInv# = *On;
          Else;
             dspInv# = *Off;
          Endif;
          //*** End Add *** 10/25/12 *******************************************

          //***** beg add ***** 07/11/14 ******* 111WBL ************************
          dspExcRec = *Off;
          If strCLTID = '111';
             dspExcRec = *On;
          Endif;
          //***** end add ***** 07/11/14 ***************************************

          // Populate hidden fields
          scActCltId=strCLTID;
          scActRrn=inRrnClm;

          // Check the authority on claim  and get all the character values

          ##co =    %editc(CMCo#:'X')  ;
          ##fnd =   %editc(CMFnd:'X')  ;
          ##fyrc =  %editc(CMfyr:'X')  ;
          ##Casec =  %editc(CMCase:'X') ;

          ##rtcd =  *BLANKS;
          ##user =  q1user;
          getAut(##co:##fnd:##fyrc:##casec:##user:##rtcd);

          // Set the pointers for setup files and get the system date

          Chain keyWMFNL pfn00;
          Chain keyWMUPL pup00;
          sysDt7 = DateTo7(SystemDate);

          // Either test box or list bow will be shown depending on flag

          Chain keyWMCXL#  pcx00;
          If %found();
             If CXopcl = 'Y' ;
                listrsn = *on;
             else;
                Txtrsn = *on;
             Endif;
          Endif;

          // Save time & Date
          savTime = CMtime;
          savDate = CMdt ;

          // Initialize filter and flags
          clear dsFilter;
      //**** Begin Add  ***  06/23/21  ****************************************
          OutCmRrnTm = OutCmRrn;
      //**** End Add  ***  06/23/21  ****************************************
          clear rcdFilter;
      //**** Begin Add  ***  06/23/21  ****************************************
          OutCmRrn = OutCmRrnTm;
      //**** End Add  ***  06/23/21  ****************************************
          clear rcdDedRec;
          isRels     = *off;
          dspChk#    = *off;
          dspEob#    = *off;
          dspEftSts  = *on ;
          dspReqGrp  = *on ;
          dspReqDel  = *on ;
          dspClrFltr = *off;
          showNext07=*Off;
          showPrev07=*Off;
          addPop07=*Off;

          If dspMode = *on;
             dspChk# = *on;
             //****** beg del ********* 06/13/14 ****************************
             // dspEob# = *on;
             //****** end del ********* 06/13/14 ****************************
          Endif;

          // Diaplay/non-display the delete Link
          ValidDspDel();

          // Build tab header
          BldTabHdr();

          // Set the initial Filter
          //***** beg chg ****** 09/12/13 ***********************
          // If %parms()>4;
          //    @@inType = @@type;

          outName = 'TYPDTAARA';
          outType = '*DTAARA';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          If outRtcd <> '1';
             In *lock ##FltrTyp;
             @@inType = ##FltrTyp;
          Endif;
          //***** end chg ****** 09/12/13 ***********************
             clear scFltrRCod;
             setll CMco# prt00;
             reade CMco# prt00;
             Dow not %eof;
                //***** beg chg ****** 09/12/13 ***************************
                //If ((@@Type = '1' or @@Type = '2') and RTtype = 'C') or
                //   ((@@Type = '3' or @@Type = '4') and RTtype = 'M') or
                //   ((@@Type = '5' or @@Type = '6') and RTtype = 'E') or
                //   ((@@Type = '7' or @@Type = '8') and RTtype = 'L') or
                //   ((@@Type = '9' or @@Type = '0'));
                //***** Beg Chg ****** 08/04/14 ***************************
                //If ((@@inType = '1' or @@inType = '2') and RTtype = 'C') or
                //   ((@@inType = '3' or @@inType = '4') and RTtype = 'M') or
                //   ((@@inType = '5' or @@inType = '6') and RTtype = 'E') or
                //   ((@@inType = '7' or @@inType = '8') and RTtype = 'L') or
                //   ((@@inType = '9' or @@inType = '0'));
                If ((@@inType = '1' or @@inType = '2' or @@inType = 'A')
                 and RTtype = 'C') or
                 ((@@inType = '3' or @@inType = '4' or @@inType = 'B')
                 and RTtype = 'M') or
                 ((@@inType = '5' or @@inType = '6' or @@inType = 'C')
                 and RTtype = 'E') or
                 ((@@inType = '7' or @@inType = '8' or @@inType = 'D')
                 and RTtype = 'L') or
                 ((@@inType = '9' or @@inType = '0' or @@inType = 'E'));
                //***** End Chg ****** 08/04/14 ***************************
                //***** end chg ****** 09/12/13 ***************************
                   If scFltrRCod = *blanks;
                      scFltrRCod = %trim(RTkey);
                   Else;
                      scFltrRCod = %trim(scFltrRCod) + ',' + %trim(RTkey);
                   Endif;
                Endif;
                reade CMco# prt00;
             Enddo;
             //***** beg chg ****** 09/12/13 **********************************
             //If @@Type = '2' or @@Type = '4' or @@Type = '6' or @@Type = '8' o
             //   @@Type = '0';
             If @@inType = '2' or @@inType = '4' or @@inType = '6' or
              @@inType = '8' or @@inType = '0';
             //***** end chg ****** 09/12/13 **********************************
                scFltrTrTy = 'R';
             Endif;
             If scFltrRCod <> *blanks or scFltrTrTy <> *blanks;
                dspClrFltr = *on;
             Endif;
          //***** beg del ****** 09/12/13 ***********************
          // EndIf;
          //***** end del ****** 09/12/13 ***********************

       //* Beg *** App security ****** Adhoc **********
       // ***** Beg Del ****** 09/12/19 **************************
       //***** flgofEnv = 'L';
       //// Get user sec level/type
       //*****userLevel = %Editc(GetUsrLvl(Q1user:userType):'X');
       //// Check user authority for EFT Links
       //*****Chain(n) ('WTCLMRSV':flgofEnv:'1':2) pof01;
       //*****If %found(sdofl1);
       //*****  CheckSecurity(userType:userLevel:OFty01:%Editc(OFlvl:'X'):'1'
       //*****   :flgRes:OFty02:OFty03:OFty04:OFty05:OFty06);
       //*****  If flgRes <> '0';
       //    //Deny
       //*****    shwEftLnk = *Off;
       //*****  Endif;
       //*****Endif;
       //// Check user authority for EFT Links
       //*****Chain(n) ('WTCLMRSV':flgofEnv:'2':6) pof01;
       //*****If %found(sdofl1);
       //*****  CheckSecurity(userType:userLevel:OFty01:%Editc(OFlvl:'X'):'1'
       //*****   :flgRes:OFty02:OFty03:OFty04:OFty05:OFty06);
       //*****  If flgRes <> '0';
       //*****    //Deny
       //*****    shwAddEft = *Off;
       //*****  Endif;
       //*****Endif;
       // ***** End Del ****** 09/12/19 **************************
       svShwEftLnk = shwEftLnk;
       //* End *** App security ****** Adhoc **********

       //****** beg add ***** 05/20/13 ****************
       // get the system values
       GetSystem(##Wcif:##Mcif:##Wcin);

       dspEOBMngC = *off;
       If ##MCif <> 'Y';
          dspEOBMngC = *on;
       Endif;
       //****** end add ***** 05/20/13 ****************
       //*** Beg Add ***  03/06/13  *******************
       If strCLTID = '129';
          Chain keyWMcxl# pcx00;
          If %Found(wmcxl);
             If CXlgrh <> *Blanks;
                scLEGL = CXlgrh;
             Else;
                scLEGL = '     Legal';
             Endif;
          Endif;
          scRsvClmHd = 'Date Changed' + ',' + 'Compensation' + ',' +
                       'Medical' + ',' + scLegl + ',' + 'Expense' + ',' +
                       'User' + ',' + 'Reason' + ',';
       Else;
          scRsvClmHd = 'Date Changed' + ',' + 'Compensation' + ',' +
                       'Medical' + ',' + '     Legal' + ',' + 'Expense' + ',' +
                       'User' + ',' + 'Reason' + ',';
          scLEGL = 'Legal';
       EndIf;
       //*** End Add ***  03/06/13  *******************

       //*** Begin Add *** 07/30/20 ********************************************
         userLevel = GetUsrLvl(Q1USER);
         userType =  GetUsrType(Q1USER);
       //*** End   Add *** 07/30/20 ********************************************
       endsr;
       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;

       //@@Next = scMenuRsp;
       *inlr = *on;
       Return;

       endsr;
      /end-Free
       //===================================================================
       // ApplyFilter  - Apply the filter
       //===================================================================
     P ApplyFilter     B
     D ApplyFilter     PI
      /Free

       scFilter = *off;
       isValid = *off;

       // Loop until filter applied or cancelled.
       DoU CnlFilter = *On or (AppFilter = *On and isValid =*off);
          Exfmt  rcdFilter;
          isValid = *off;

          // If cancel then restore previous filter selection.
          If CnlFilter  = *on;
             scFltrSfDt  =  dsfilter.scFltrSfDt;
             scFltrStDt  =  dsfilter.scFltrStDt;
             scFltrFE   =  dsfilter.scFltrFE;
             scFltrChk# =  dsfilter.scFltrChk#;
             scFltrEOMB =  dsfilter.scFltrEOMB;
             scFltrGrpN =  dsfilter.scFltrGrpN;
             scFltrIntR =  dsfilter.scFltrIntR;
             scFltrTrTy =  dsfilter.scFltrTrTy;
             scFltrPayC =  dsfilter.scFltrPayC;
             scFltrPVal =  dsfilter.scFltrPVal;
             scFltrRCod =  dsfilter.scFltrRCod;
             Leave;
          Endif;

          // If filter applied.
          If   Appfilter  = *on;
             // Validate filter selection and if valid save filter selection.
             ValidFltr();
             If isValid = *off;
                dsfilter.scFltrSfDt = scFltrSfDt;
                dsfilter.scFltrStDt = scFltrStDt;
                dsfilter.scFltrFE   = scFltrFE;
                dsfilter.scFltrChk# = scFltrChk#;
                dsfilter.scFltrEOMB = scFltrEOMB;
                dsfilter.scFltrGrpN = scFltrGrpN;
                dsfilter.scFltrIntR = scFltrIntR;
                dsfilter.scFltrTrTy = scFltrTrTy;
                dsfilter.scFltrPayC = scFltrPayC;
                dsfilter.scFltrPVal = scFltrPVal;
                dsfilter.scFltrRCod = scFltrRCod;
             Endif;
          Endif;

       Enddo;

       If isValid = *on;
          scFltrSfDt = 0;
          scFltrStDt = 0;
          intFromDate = 0;
          intToDate = 0;
          isValid = *off;
       Endif;

       // If filter selection made, clear filter should be available.
       If intFromDate > 0 or intToDate > 0 or scFltrChk# > 0 or scFltrEOMB > 0
        or scFltrGrpN <> ' ' or  scFltrIntR <> ' ' or  scFltrTrTy <> ' '
        or scFltrPayC <> ' ' or  scFltrPVal <> ' ' or  scFltrRCod <> ' '
        or scFltrFE <> 0;
          dspClrFltr = *on;
       Else;
          dspClrFltr = *off;
       Endif;

       TabPaymentInfo();

      /End-Free
     P ApplyFilter     E
       //===================================================================
       // BldTabHdr - Build tab header
       //===================================================================
     P BldTabHdr       B
     D BldTabHdr       PI
      /Free

       clear intTab;
       clear tabHeader;
       tabHeader = 'Reserve Detail' + ',' + 'Payment Detail' + ',' +
        'Pending Check Requests';
       intTab = 2;

       Chain 'WC' swc00;
       If WCfl06 = 'Y';
          tabHeader = %trim(tabHeader) + ',' + 'Deductible History';
          intTab = intTab + 1;
       Endif;

       tabHeader = %trim(tabHeader) + ',' + 'EFT Information';
       intTab = intTab + 1;

      /End-Free
     P BldTabHdr       E
       //===================================================================
       // ChangePmtExcl - Change Payment Exclusion
       //===================================================================
     P ChangePmtExcl   B
     D ChangePmtExcl   PI
     D  pmtRrn#                       9  0

     D  svDupPmtEx     s              1

     C     keyWDRPL3     Klist
     C                   Kfld                    PACo#
     C                   Kfld                    PAFnd
     C                   Kfld                    PAFyr
     C                   Kfld                    PACase
     C                   Kfld                    PASeq#
      /Free

       Chain(n) scRRN# ppa0p;
       If %found(WDPAP);
          // Load Check Information
          Chain keyWDRPL3 prp03;
          If %Found(WDRPL3);
             scPayName=RPpNam;
             scPayAdr1=RPadr1;
             If RPadr2<>' ';
                scPayAdr2=RPadr2;
                scPayAdr3=%Trim(RPcty) + ', ' + RPst + ' ' +
                 %EditW(RPzip:'     -    ');
             Else;
                scPayAdr2=%Trim(RPcty) + ', ' + RPst + ' ' +
                 %EditW(RPzip:'     -    ');
                scPayAdr3=' ';
             EndIf;

             ##Key=RPrmk1;
             Chain keyWTABL prc01;
             If %Found(WTABL);
                scRemark1D=RCdesc;
             EndIf;

             ##Key=RPrmk2;
             Chain keyWTABL prc01;
             If %Found(WTABL);
                scRemark2D=RCdesc;
             EndIf;

             ##Key=RPrmk3;
             Chain keyWTABL prc01;
             If %Found(WTABL);
                scRemark3D=RCdesc;
             EndIf;

             scRemark=RPrmDs;

          EndIf;

          If PArqDt=0;
             scPostDate=dtMDYY(PApoDt);
          Else;
             scPostDate=dtMDYY(PArqDt);
          EndIf;

          scTranDate=dtMDYY(PAtrDt);
          scSerFrDt=dtMDYY(PAfrDt);
          scSerToDt=dtMDYY(PAtoDt);
          scPayAmt=PAamt;
          scCheck#=PAchk#;
          scPayDesc=PAdesc;

          ##Key=PAbr2;
          Chain keyWTABL pb200;
          If %Found(WTABL);
             scPayVal1D=B2desc;
          EndIf;

          ##Key=PAbr7;
          Chain keyWTABL pb700;
          If %Found(WTABL);
             scPayVal2D=B7desc;
          EndIf;

          ##Key=PAtrCd;
          Chain keyWTABL ppc00;
          If %Found(WTABL);
             scTranCdDs=PCdesc;
          EndIf;

          ##Key=PAreCd;
          Chain keyWTABL prt00;
          If %Found(WTABL);
             scResvCdDs=RTdesc;
          EndIf;

          Select;
          When Patype='P';
             scPayType='Payment';
          When Patype='M';
             scPayType='Manual Payment';
          When Patype='V';
             scPayType='Void';
          When Patype='R';
             scPayType='Recovery';
          When Patype='A';
             scPayType='Adjustment';
          When Patype='S';
             scPayType='Stop';
          When Patype='Z';
             scPayType='Reverse';
          EndSl;

          ##Key=PAreCd;
          Chain keyWTABL prt00;
          If %Found(WTABL);
             scResvCdDs=RTdesc;
          EndIf;

          // Duplicate Payment Exclusion Field
          If PAdpCd=0;
             scDupPmtEx='N';
          Else;
             scDupPmtEx='Y';
          EndIf;

          svDupPmtEx=scDupPmtEx;

          ExFmt rcdChgDupE;

       EndIf;

       If cancPmtExc=*Off And svDupPmtEx<>scDupPmtEx;
          Chain pmtRrn# ppa0p;
          If %found(WDPAP);
             PAuser = q1user;
             PApgm  = q1pgm;
             PAdt   = Dateto7(SystemDate());
             PAtime = Timeto6(SystemTime());
             PAdpcu = q1user;
             If PAdpcd = *zero;
                PAdpcd = PAdt;
             Else;
                PAdpcd = *zero;
             Endif;
             Update ppa0p;
          Endif;
       EndIf;

      /End-Free
     P ChangePmtExcl   E
       //===================================================================
       // ClearFilter - Clear filter
       //===================================================================
     P ClearFilter     B
     D ClearFilter     PI
      /Free

       clear dsFilter;
      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrnTm = OutCmRrn;
      //**** End Add  ***  06/23/21  ****************************************
       clear rcdFilter;
      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = OutCmRrnTm;
      //**** End Add  ***  06/23/21  ****************************************
       dspClrFltr = *off;
       scClrFltr = *off;
       intFromDate = 0;
       intToDate = 0;
       clear @@inType;
       //***** beg add ****** 09/12/13 ***********************
       outName = 'TYPDTAARA';
       outType = '*DTAARA';
       outRtcd = ' ';
       GetObject(outName:outType:outRtcd);
       //***** beg chg ****** 05/08/14 ***********************
       //************* If outRtcd <> '1';
       If outRtcd <> '1' and ##FltrTyp <> ' ';
       //***** end chg ****** 05/08/14 ***********************
          ##FltrTyp = ' ';
          Out ##FltrTyp;
       Endif;
       //***** end add ****** 09/12/13 ***********************

       TabPaymentInfo();

      /End-Free
     P ClearFilter     E
       //===================================================================
       // ClrSubFilesRsv - Clears SubFiles - Reserves
       //===================================================================
     P ClrSubFilesRsv  B
     D ClrSubFilesRsv  PI
      /Free

       rrnPmtSum  = 0;
       clrReserve = *on;
       dspReserve = *off;
       rrnSts     = 0;
       clrStatus  = *on;
       dspStatus  = *off;
       //*** Begin Add *** 06/27/18 ************************
       rrnHst     = 0;
       clrHist    = *on;
       dspHist    = *off;
       //*** End   Add *** 06/27/18 ************************
       Write rcdRsvDtl;

      /End-Free
     P ClrSubfilesRsv  E
       //===================================================================
       // ClrSubFilesPay - Clears SubFiles - Payments
       //===================================================================
     P ClrSubFilesPay  B
     D ClrSubFilesPay  PI
      /Free

       rrnVal     = 0;
       rrnTrans   = 0;
       clrTranSum = *on;
       clrValSum  = *on;
       dspTranSum = *off;
       dspValSum  = *off;
       Write rcdPayDtl;

      /End-Free
     P ClrSubfilesPay  E
       //*** Begin Add *** 06/12/17 *******************************************
       //===================================================================
       // ChkDupPmtAuth - Check Duplicate Payment Authority
       //===================================================================
     P ChkDupPmtAuth   B
     D ChkDupPmtAuth   PI              n
     d  inFromDate                    7  0
     d  inToDate                      7  0

     D returnCode      s              1    Inz(' ')
     D claimLine1      s             79    Inz(' ')
     D claimLine2      s             79    Inz(' ')
     D groupName       s             51    Inz(' ')
     D claimName       s             36    Inz(' ')
     D payeeName       s             36    Inz(' ')
     D fromDate        s              8  0
     D toDate          s              8  0
     d inError         s               n   Inz(*Off)

      /Free

       //*** Begin Add *** 01/19/18 ******************************
       If scRlsFst = *On;
          isValid = *Off;
          Return *Off;
       EndIf;
       //*** End   Add *** 01/19/18 ******************************

       GetClmInfo(RPco#:RPfnd:RPfYr:RPcase:claimLine1:claimLine2);

       fromDate  = inFromDate;
       toDate    = inToDate;
       groupName = %Subst(claimLine1:12:51);
       claimName = %Subst(claimLine2:39:22);
       payeeName = %Subst(RPpNam:1:36);

       //*** Beg Add *** 12/10/25 ****************************************
       returnCode = 'R';
       //*** End Add *** 12/10/25 ****************************************
       ValidDup(RPco#:RPfnd:RPfYr:RPcase:RPseq#:fromDate:toDate:
       RPamt:RPrecd:RPfein:groupName:claimName:payeeName:inError:
       RPretp:RPmod:returnCode);

       isValid = inError;

       //*** Begin Add *** 01/19/18 *********************88
       //**********If returnCode = '2' or returnCode = '3';
       //*** Begin Chg *** 07/10/18 ********************************
       //*** If returnCode <> ' ' Or inError <> ' ';
       If returnCode <> ' ' Or inError = *On;
       //*** End   Chg *** 07/10/18 ********************************
       //*** End   Add *** 01/19/18 *********************88
          //*** Begin Add *** 01/19/18 *********************88
          //scRlsFst = *On;
          If returnCode = '2' Or returnCode = '3';
             rlsd(rrnReq) = *Off;
          Else;
             rlsd(rrnReq) = *On;
          EndIf;
          //*** End   Add *** 01/19/18 *********************88
          Return *On;
       Else;
          Return *Off;
       Endif;

      /End-Free
     P ChkDupPmtAuth   E
       //*** End   Add *** 06/12/17 *******************************************
       //===================================================================
       // ChkEmpDed - Check if employer is deductible processing
       //===================================================================
     P ChkEmpDed       B
     D ChkEmpDed       PI
      /Free

       //*** beg chg ***** 09/01/15 *******************
       //***Chain(n) keyWDELL pel00;
       //***If ELdcex = 0;
       //***   ##dcex1 = *hival;
       //***Else;
       //***   ##dcex1 = ELdcex;
       //***Endif;

       //***If EL2dex = 0;
       //***   ##dcex2 = *hival;
       //***Else;
       //***   ##dcex2 = EL2dex;
       //***Endif;

       //***Select;
       //***When ELdcd2 <> *blanks and EL2def <= CMacdt and ##dcex2 >= CMacdt;
       //***   ##ddcd = ELdcd2;
       //***   ##ddef = EL2def;
       //***   scDedDspM = *off;
       //***When ELdcde <> *blanks and ELdcef <= CMacdt and ##dcex1 >= CMacdt;
       //***   ##ddcd = ELdcde;
       //***   ##ddef = ELdcef;
       //***   scDedDspM = *off;
       //***Other;
       //***   scDedDspM = *on;
       //***Endsl;

       //***If scDedDspM = *off;
       //***   ##Key#  = ELrtcd;
       //***   Chain keyWMRSL prs00;
       //***   ##st = RSjrst;
       //***  SetGt keyWTDDL pdd02;
       //***   Readpe keyWTDDL2 pdd02;
       //***   Select;
       //***   When DDtype = 'N';
       //***     scNDedDsp = *on;
       //***      scDedDsp = *off;
       //***   Other;
       //***      scDedDsp = *on;
       //***      scNDedDsp = *off;
       //***   Endsl;
       //***Endif;

       // Display Deductible History if policy is set up for deductible
         scDedDspM = *on;
         scDedDsp = *off;
         scNDedDsp = *off;
       //*** beg chg ***** 09/25/15 *******************
         //****** Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
         Chain(n) (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
       //*** end chg ***** 09/25/15 *******************
         // Look for valid small deductible set up in Policy Detail
         If ELdcde <> *blank;
            If ELdcef <> ELdcex or EL2def <> EL2dex;
               scDedDspM = *off;
               Chain (ELco#:ELrtcd) prs00;
               Setgt (ELco#:ELfnd:RSjrst:ELdcde:ELpfdt) pdd02;
               Readpe (ELco#:ELfnd:RSjrst:ELdcde) pdd02;
               Dow not(%eof);
                  If DDstat = 'A';
                     If DDtype = 'N';
                        scNDedDsp = *on;
                     Else;
                        scDedDsp = *on;
                     Endif;
                     Leave;
                  Endif;
               Readpe (ELco#:ELfnd:RSjrst:ELdcde:ELpfdt) pdd02;
               Enddo;
            Endif;
         Else;
         // Look for valid large deductible set up in Policy Detail
            Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pld01;
            If %found and (LDefd1 <> LDexp1 or LDefd2 <> LDexp2);
               scDedDspM = *off;
               If LDtyp1 = 'N';
                  scNDedDsp = *on;
               Else;
                  scDedDsp = *on;
               Endif;
            Endif;
         Endif;
       //*** end chg ***** 09/01/15 *******************

      /End-Free
     P ChkEmpDed       E
       // Begin Del *** 12/18/2012 *****************************************
       //===================================================================
       // ChkHospPBill - Check if Hospital bill Record
       //===================================================================
     P*ChkHospPBill    B
     D*ChkHospPBill    PI
      /Free

       //Chain keyWMUPL pup00;
       //If %Found(WMupl) and UPtitl = 'CLIENT';

       // ##co#  =  PAco#;
       // ##wcf# =  PAfnd;
       // ##fyr  =  PAfyr;
       // ##case =  PAcase;
       // ##eob# =  PAeob#;
       // ##rcn  =  PAeobs;
       // ##fein =  *zeros;
       // ##mod  =  *zeros;
       // ##chk# =  *zeros;
       // ##othr =  *zeros;
       ///##ast  =  *blanks;
       // ##binv =  *zeros;
       // ##ninv =  *zeros;
       // ##sub# =  *zeros;
       // ##fnd  =  *zeros;
       // ##rrn  =  *zeros;
       //   ##rtcd =  *blanks;
       //   ##acct =  *blanks;
       //   ##btyp =  *blanks;

       //   GetEombPBTyp(##co#:##wcf#:##fyr:##case:##eob#:##rcn#:
       //                 ##fein:##mod:##chk#:##othr:##ast:##binv:
       //                 ##ninv:##sub#:##fnd#:##rrn:##rtcd:##acct:
       //                 ##btyp);
       //
       //EndIf;     // %Found(WMupl) and UPtitl = 'CLIENT';

      /End-Free
     P* ChkHospPBill    E
       // End   Del *** 12/18/2012 *****************************************
       //===================================================================
       // ContDelProc - Continue delete process
       //===================================================================
     P ContDelProc     B
     D ContDelProc     PI
      /Free

       // Complete DARTS diary
       WriteDarts();

       // If Claim Closed, Decrease Reserves in Claim & Reserve History
       If strSvStat = ' ' and CMstat = 'C';
          ##type = 'D';
          ##recd =  RPrecd;
          ##amt  =  RPamt;

          Unlock WDELL;
          ValidClm(##co#:##fnd#:##fyrc#:##casec#:##recd:##type:##amt);
       Endif;

       // If an EOMB is attached and the request being deleted is not
       //  the bill review or medical review payment, then delete all
       //  requests with the corresponding EOB#.
       If RPeob# > 0;
          clear ##SvEob#;
          ##SvEob# = RPeob#;
          Chain (RPco#:RPfein:RPmod) pnr00;
          If not %found(WMNRL) or NRgrty = 'O';

             // Read all requested payments with the corresponding EOB#.
             Setll keyWARPLN prp0n;
             Reade keyWARPLN prp0n;
             Dow not %eof(WARPLN);

                If RPstat <> 'D';
                   strSvStat = RPstat;
                   RPsort = strSort;
                   RPsts  = strSts;
                   RPstat = 'D';
                   RPuser  = q1user;
                   RPpgm   = q1pgm;
                   RPdt    = Dateto7(SystemDate());
                   RPtime  = Timeto6(SystemTime());

                   update prp0n;

                   // Complete DARTS Diary
                   WriteDarts();

                   //  If the claim is closed and payment is released, decrease
                   //  the reserves in claim master and reserve history.
                   If CMstat = 'C' and strSvStat = ' ';
                      ##co#     =  RPco#;
                      ##fnd#    =  RPfnd;
                      ##fyrc#   =  RPfyr;
                      ##casec#  =  RPcase;
                      ##recd    =  RPrecd;
                      ##amt     =  RPamt;
                      ##type    =  'D';

                      Unlock WDELL;
                      ValidClm(##co#:##fnd#:##fyrc#:##casec#:##recd:
                       ##type:##amt);
                   Endif;
          //*** Begin Add *** 06/22/13 ***********************************
                Else;
                   Unlock WARPLN;
          //*** End   Add *** 06/22/13 ***********************************
                Endif;
                Reade keyWARPLN prp0n;
             Enddo;

          Endif;
       Endif;
       unlock WARPLN;

      /End-Free
     P ContDelProc     E
       //===================================================================
       // DisplayMain - Display main screen
       //===================================================================
     P DisplayMain     B
     D DisplayMain     PI
      /Free
       APnlHeight = 890;
       Select;
       When TabIndex = 0;
       // *** Beg Add ****** 10/01/13 *******************
         // Reserve Worksheet Pending
          If outClientID = '025';
             Chain   key1WDPAL5 PRW01;
             If %found() and RWsts = 'P';
                shwRsvAdd=*Off;
                showClmSts=*Off;
                dspMode = *on;
                BtnCntWdW4 = *off;
                Dou (BtnCntWdW4 = *on);
                   Write rcdClmPnl;
                   Write rcdPanel;
                   Exfmt rcdWDW04;
                EndDo;
             EndIf;
         EndIf;
       // *** End Add ****** 10/01/13 *******************
         // TabReserveinfo();
         TabHeight = 860;
         APnlHeight = TabHeight + 70;
         Write rcdClmPnl;
         Write rcdPanel;
         Exfmt rcdRsvDtl;
       When TabIndex = 1;
         // TabPaymentinfo();
         //*** Begin Chg *** 05/09/2023 ***********************************
         // TabHeight = 940;
         TabHeight = 960;
         //*** End   Chg *** 05/09/2023 ***********************************
         APnlHeight = TabHeight + 70;
         If outClientID = '114';
            lblMailDt = 'Processed Date';
         Else;
            lblMailDt = 'Mailing Date';
         EndIf;
         Write rcdClmPnl;
         Write rcdPanel;
         //*** Begin Add *** 12/19/18 *************************************
         If pmtCount > 9999;
            strMsg = 'There are more than 9999 payments so not all of '   +
                     'these payments can be displayed on the screen at ' +
                     'once.  Please use the filter to restrict the view.';

            PopOneBtn(strMsg:'Payment Detail');
         EndIf;
         //*** End   Add *** 12/19/18 *************************************
         Exfmt rcdPayDtl;
       // *** Beg Add *** 08/05/14 *****************************************
         If lnkRvDedRc = *On;
         If SflCsrRrnP = 0;
            SflCsrRrnP = 1;
         EndIf;
             Chain SflCsrRrnP SflPayee;
             Chain keyWMFNL pfn00;
             If %Found(wmfnl);
                If FNmep1 = scPV1Code Or
                   FNcop1 = scPV1Code Or
                   FNexp1 = scPV1Code Or
                   FNlep1 = scPV1Code;
                      GetRevAmt();
                Endif;
             Endif;
         Endif;
       // *** End Add *** 08/05/14 *****************************************
       When TabIndex = 2;
         // TabRequestinfo();
         TabHeight = 860;
         APnlHeight = TabHeight + 70;
         Write rcdClmPnl;
         Write rcdPanel;
         Exfmt rcdPndChk;
       When TabIndex = 3 and TabIndex <> intTab;
         // TabDedHistory();
         TabHeight = 860;
         APnlHeight = TabHeight + 70;
         Write rcdClmPnl;
         Write rcdPanel;
         Exfmt rcdDedHst;
         scRowNum=1;
       When TabIndex = 4 or TabIndex = intTab;
         TabHeight = 860;
         APnlHeight = TabHeight + 70;
         Write rcdClmPnl;
         Write rcdPanel;
         Exfmt rcdEftInfo;
       EndSl;
       Read rcdClmPnl;
       Read rcdPanel;

       //Readc sflPayee;
       //Readc sflPenReq;

      /End-Free
     P DisplayMain     E
       //===================================================================
       // EdtRequest - Edit request for payment
       //===================================================================
     P EdtRequest      B
     D EdtRequest      PI
      /Free

       scReqEdit = *off;
       scPenRls  = *off;
       scReqDel  = *off;
       dspInv2#  = *off;

       LodEdtReq();
       // *** Beg Chg *** 04/04/23 *****************************************
       //**** LodReqInfo();
       // *** End Chg *** 04/04/23 *****************************************

      /End-Free
     P EdtRequest      E
      *================================================================
      *FUNRET - TO CALCULATE THE FUND RETENTION PERCENTAGE FROM WDF1P
      *================================================================
     P FunRet          B
     D FunRet          PI

     C     keyWDF1L      CHAIN     PF100                              75
     C     *IN75         IFEQ      *OFF
     C                   Z-ADD     F1EMRT        FNRET#
     C     FNRET#        IFNE      *ZERO
     C     decTrsv       DIV       FNRET#        RETPR#
     C     RETPR#        MULT      100           RETPR#
     C                   ENDIF
     C                   ENDIF
     P FunRet          E
      *================================================================
      *  GetAmount  - Get the Amount
      *================================================================
     P GetAmount       B
     D GetAmount       PI
      *
      /free
       If  PAtype = 'S'or PAtype = 'V' or PAtype = 'R' ;
          scAmount  =  -1 * PAamt ;
       else ;
          scAmount  =  PAamt ;
       Endif;

      /end-free

     P GetAmount       E
       //*** Begin  Add *** 02/19/15 **********************************
       //===================================================================
       // GetDelStatus - Get the Delete Status Code
       //===================================================================
     P GetDelStatus    B
     D GetDelStatus    PI
      /Free

       strSts    =  'DE';
       strSort   =  'X';
       strSvStat =  RPstat;

       // Find "Deleted" Payment Status code
       Setll RPco# pps00;
       Reade RPco# pps00;
       DoU %eof(WTPSL) or (PStype = 'D' and PSstat <> 'I');
          Reade RPco# pps00;
       Enddo;

       If PStype = 'D' And PSstat <> 'I';
          strSts  = PSkey;
          strSort = PSsrt;
       Endif;

      /End-Free
     P GetDelStatus    E
       //*** End    Add *** 02/19/15 **********************************
      *================================================================
      *  GetSumInfo - Get the summary Info
      *================================================================
     P GetSumInfo      B
     D GetSumInfo      PI
      *
      /free

       //rrnTrans   = 0;
       //clrTranSum = *on;
       //dspTranSum = *off;
       //Write rcdPayDtl;
       dspTranSum =  *on;
       clrTranSum =  *off;
       Clear scTrnTtlA;
       Clear scValTtlA;
       Clear scTrnTtlWk;
       Clear scValTtlWk;
       Clear intTrnWk;
       Clear intValWk;
       Clear intTrnDys;
       Clear intValDys;
       XX = 1;

       Dow XX <= size;
          If WK#(XX) < *Zero and DA(XX) > *Zero;
             WK#(XX) = WK#(XX) + 1;
             DA (XX) = DA(XX) - 7;
          Endif;

          If WK#(XX) > *Zero and DA(XX) < *Zero;
             WK#(XX) = WK#(XX) - 1;
             DA(XX)  = DA(XX)  + 7;
          Endif;

          If AM(XX) <> *Zero or WK#(XX) <> *Zero or DA(XX) <> *Zero;
             scTrnSmCd  = CD(XX);
             scTrnSmDec = DS(XX);
             scTrnSmAmt = AM(XX);
             scTrnSmWkD = %Char(WK#(XX)) + '/' + %Char(DA(XX));
             scTrnTtlA  = scTrnTtlA + AM(XX);
             intTrnWk   = intTrnWk  + WK#(XX);
             intTrnDys  = intTrnDys + DA(XX);
             rrnTrans = rrnTrans + 1;
             Write SflTrnSum;
          Endif;
       XX = XX + 1;

       Enddo;

       //rrnVal     = 0;
       //clrValSum  = *on;
       //dspValSum  = *off;
       //Write rcdPayDtl;
       dspValSum  =  *on;
       clrValSum  =  *off;
       XX = 1;

       Dow XX <= size;
          If WK3(XX) < *Zero and DA3(XX) > *Zero;
             WK3(XX) = WK3(XX) + 1;
             DA3(XX) = DA3(XX) - 7;
          Endif;

          If WK3(XX) > *Zero and DA3(XX) < *Zero;
             WK3(XX) = WK3(XX) - 1;
             DA3(XX) = DA3(XX) + 7;
          Endif;

          If AM3(XX) <> *Zero or WK3(XX) <> *Zero or DA3(XX) <> *Zero;
             scValSmCd  = CD3(XX);
             scValSmDc  = DS3(XX);
             scValSmAmt = AM3(XX);
             scValSmWkD = %Char(WK3(XX)) + '/' + %Char(DA3(XX));
             scValTtlA  = scValTtlA + AM3(XX);
             intValWk   = intValWk  + WK3(XX);
             intValDys  = intValDys + DA3(XX);
             rrnVal = rrnVal + 1;
             Write SflValSum;
          Endif;
          XX = XX + 1;

       Enddo;

       intTrnWk   = intTrnWk  + %Div(intTrnDys:7);
       intTrnDys  = %Rem(intTrnDys:7);
       intValWk   = intValWk  + %Div(intValDys:7);
       intValDys  = %Rem(intValDys:7);

       scTrnTtlWk = %char(intTrnWk) + '/' + %char(intTrnDys);
       scValttlWk = %char(intValWk) + '/' + %char(intValDys);

      /End-Free
     P GetSumInfo      E
     C*
      *================================================================
      *  GetPayee - Assign Payee Address
      *================================================================
     P GetPayee        B
     D GetPayee        PI
      *
     C     keyWTGTL      klist
     C                   Kfld                    RPco#
     C                   Kfld                    ##TbId
     C                   Kfld                    ##KEY

      /Free
       strDtlAdr2  = RPAdr2 ;
       intzip      = RPZip  ;

       //LOAD CITY, STATE AND ZIP

       strDtlCsz = %trimR(RPcty) + ', ' + RPst + ' ' +
        %editc(intZip1:'X') + '-'+ %editc(intZip2:'X');

       If RPctry <> *blanks and RPctry <> 'USA' ;
          ##TbId =  'CTRY';
          ##KEY  =  RPctry ;
          Chain   keyWTGTL  PGT00;

          If %found();
             strCtrd  = GTDESC ;
          Else;
             strCtrd  = NF;
          Endif;

          If RPAdr2 =  *BLANKS ;
             strDtlAdr2 =   Rpadr3;
             strDtlCsz  =  strCtrd;
          Else;
             strDtlCsz =  RPAdr3;
          Endif;
       Endif;

       strDtlRmk =  RPrmds;
       If RPrmk1 <> *blanks;
          ##Key  = ' '  ;
          ##Key  = RPrmk1;
          Chain keyWTABL PRC01;
          strDtlRmk1 =  RCDESC;
       Else;
          strDtlRmk1 =  *blanks;
       Endif;

       If RPrmk2 <> *blanks;
          ##Key  = ' '  ;
          ##Key  = RPrmk2;
          Chain keyWTABL PRC01;
          strDtlrmk2 =  RCdesc;
       Else;
          strDtlrmk2 =  *blanks;
       Endif;

       If RPrmk3 <> *blanks;
          ##Key  = ' '  ;
          ##Key  = RPrmk3;
          Chain keyWTABL PRC01;
          strDtlrmk3 =  RCdesc;
       Else;
          strDtlrmk3 =  *blanks;
       Endif;

      /end-Free


     P GetPayee        E
       //===================================================================
       // GetPayeeInfo - Load the values of Payee Subfile  (GRID)
       //===================================================================
     P GetPayeeInfo    B
     D GetPayeeInfo    PI
      /Free

       // Get the payee Name
       GetPayeeName();

       // Get the Amount
       GetAmount();

       // ***** beg chg ****** 05/16/13 *******************
       // Posting Date
       // If PArqDt=0;
       //    scPostDt=dtMDYY(PAPoDt);
       // Else;
       //    scPostDt=dtMDYY(PArqDt);
       // EndIf;

       // Transaction Date
       // scTransdt =  dtMDYY(PATrDt);

       // Posting Date
       If PArqDt=0;
          scPostDt = CvtToDate(PApodt);
       Else;
          scPostDt = CvtToDate(PArqdt);
       EndIf;

       // Transaction Date
       scTransdt = CvtToDate(PAtrdt);
       // ***** end chg ****** 05/16/13 *******************

       // Check#
       // ***** Beg Add ****** 10/25/13 *******************
       dspChk# = *off;
       dspchkLnk = *on;
       clear scChk1#;
       clear scChk#;
       clear scTlTpChk;
       // Disable check link if not valid for print
       // ***** Beg Add ****** 10/25/13 *******************
       //*** Begin Chg *** 05/06/15 ************************
       //*** If PAtype = 'R';
       //*** Beg Chg *** 07/20/20 **************************
       //***If PAtype = 'R' Or PAeftC = 'E';
       If PAtype = 'R' Or PAeftC = 'E' or PaeftC= 'V';
       //*** End Chg *** 07/20/20 **************************
       //*** End   Chg *** 05/06/15 ************************
          dspChk# = *on;
          dspchkLnk = *off;
          scChk1# = PAchk# ;
          //*** Begin Chg *** 05/06/15 ************************
          //*** scTlTpChk = 'Check not issued from CompSuite';
          If PAeftC = 'E';
             scTlTpChk = 'This is not a check, it is EFT';
          Else;
          //*** Beg Add *** 07/20/20 **************************
             If PAeftC = 'V';
                scTlTpChk = 'This is not a check, it is E-Pay';
             Else;
          //*** End Add *** 07/20/20 **************************
                scTlTpChk = 'Check not issued from CompSuite';
          //*** Beg Add *** 07/20/20 **************************
             EndIf;
          //*** End Add *** 07/20/20 **************************
          EndIf;
          //*** End   Chg *** 05/06/15 ************************
       // ***** End Add ****** 10/25/13 *******************
       // ***** Beg Del ****** 10/25/13 *******************
       //*****If PAeob# > 0 or PApgtp ='G';
       //*****   dspChk# = *on;
       //*****   dspchkLnk = *off;
       //*****   scChk1# = PAchk# ;
       //*****   If PApgtp = 'G';
       //*****      scTlTpChk = 'Check# link disabled for Group Check flag =''N'
       //*****   ElseIf PAeob# > 0 ;
       //*****      scTlTpChk = 'Check# link disabled as EOB# exist for this +
       //*****     record.';
       //*****   Endif;
       // ***** End Del ****** 10/25/13 *******************
       Else;
       // ***** End Add ****** 10/25/13 *******************
          scChk# = PAchk# ;
          // ***** Beg Add ****** 01/22/14 *******************
          scChk1# = PAchk# ;
          // ***** End Add ****** 01/22/14 *******************
          strCheckTitle = 'Print Check#: ' + %Trim(%EditC(PAchk#:'4'));
          strPmtRrn = %EditC(WDPAPrrn:'X');
          scChkLink = CreateURL('W7019CW':strCheckTitle:strPmtRrn);
          //*** Begin Add *** 05/06/15 ************************
          scTlTpChk = 'Reprint file copy of the check';
          //*** End   Add *** 05/06/15 ************************
       // ***** Beg Add ****** 10/25/13 *******************
       Endif;
       // ***** End Add ****** 10/25/13 *******************
       //*** Begin Add *** 05/06/15 *********************************
       If PAeftC = 'E';
          scChkType = 'E';
       Else;
          //*** Beg Add *** 07/20/20 ********************************
          If PAeftC = 'V';
             scChkType = 'V';
          Else;
          //*** End Add *** 07/20/20 ********************************
             scChkType = 'C';
          //*** Beg Add *** 07/20/20 ********************************
          EndIf;
          //*** End Add *** 07/20/20 ********************************
       EndIf;
       //*** End   Add *** 05/06/15 *********************************

       // ***** Beg Del ****** 10/25/13 *******************
       // ***** Beg Add ****** 09/17/13 *******************
       // dspChk# = *off;
       // ***** End Add ****** 09/17/13 *******************
       // Disable check link if not valid for print
       // If PAeob# > 0 or PApgtp ='G';
       //   dspChk# = *on;
       //Endif;
       // ***** End Del ****** 10/25/13 *******************

       // Service "From" Date
       // ***** beg chg ****** 05/16/13 *******************
       // scSerFdt  =  dtMDYY(PAFrDt);
       scSerFdt = CvtToDate(PAfrdt);
       // ***** end chg ****** 05/16/13 *******************

       // Service "To" Date
       // ***** beg chg ****** 05/16/13 *******************
       // scSerTdt  =  dtMDYY(PAToDt);
       scSerTdt = CvtToDate(PAtodt);
       // ***** end chg ****** 05/16/13 *******************

       // Weeks/Days
       // *** Beg Chg *** 03/14/13 *****************************************
       // scWkDys = %Char(PAWks) + '/' + %char(PADays);
       // ***** Begin Delete **** 11/04/13 ***SEQ82448********************
       //   scWk    = %Char(PAWks);
       //   scDys   = %char(PADays);
       // ***** Begin Delete **** 11/04/13 ***SEQ82448********************
       // ***** Begin Add  **** 11/04/13 ***SEQ82448********************

       //****** beg chg ******** 09/23/14 **********************************
       // If  PAtype = 'S'or PAtype = 'V' or PAtype = 'R' ;
       //    If PAWks <> 0 ;
       //      scWk  =  %Char(-1 * PAWks) ;
       //    EndIf ;
       //    If PADays <> 0 ;
       //      scDys =  %Char(-1 * PADays) ;
       //    EndIf ;
       // Else ;
       //    scWk  = %Char(PAWks) ;
       //    scDys = %char(PADays) ;
       // Endif;

       scWk  = '0';
       scDys = '0';

       If ((PAtype = 'P' or PAtype = 'M' or PAtype = 'A' or Patype = 'Z') and
        PAamt > 0) or ((PAtype = 'S' or PAtype = 'V' or Patype = 'R') and
        PAamt < 0);
          scWk  = %char(PAwks);
          scDys = %char(PAdays);
          intWkDays += PAdays;
          intWk     += PAwks;
       Else ;
          If PAwks <> 0 ;
            scWk   = %char(-1 * PAwks);
            intWk += PAwks * -1;
          EndIf ;
          If PAdays <> 0 ;
            scDys      = %char(-1 * PAdays);
            intWkDays += PAdays * -1;
          EndIf ;
       Endif;
       //****** end chg ******** 09/23/14 **********************************

       // ***** End  Add **** 11/04/13 ***SEQ82448********************
       //*** End Chg *** 03/14/13 *****************************************

       // PV1 Code
       scPV1Code = PABr2 ;

       // *** Beg Add *** 08/05/14 *****************************************
       // Sequence
       scSeq# = PASeq#;
       // *** End Add *** 08/05/14 *****************************************

       //Pass RRN
       scRRN#   = WDPAPrrn;

       // Total Wk/Days
       // ***** Begin Delete **** 11/04/13 ***SEQ82448********************
       // intWkDays = intWkDays + PAdays;
       // intWk   = intWk   + PAwks;
       // ***** End Delete ****** 11/04/13 ***SEQ82448********************
       // ***** Begin Add **** 11/04/13 ***SEQ82448********************
       //****** beg chg ******** 09/23/14 **********************************
       // If  PAtype = 'S'or PAtype = 'V' or PAtype = 'R' ;
       //    If PADays <> 0 ;
       //       intWkDays += PAdays * -1 ;
       //    EndIf ;
       //    If PAwks <> 0 ;
       //       intWk += PAwks  * -1 ;
       //    EndIf ;
       // Else ;
       //   intWkDays += PAdays ;
       //       intWk += PAwks ;
       // EndIf ;
       // ***** End Add ****** 11/04/13 ***SEQ82448********************
       // *** Beg Chg *** 03/14/13 *****************************************
       // intWk   = intWk + %Div(intWkDays:7);
       // intWkDays = %Rem(intWkDays:7);
       // scTtlWkDys = %Char(intWk) + '/' + %Char(intWkDays);
       //    scTtlWk   = %Char(intWk);
       //    scTtlDays = %Char(intWkDays);
       //*** End Chg *** 03/14/13 *****************************************

       intWk     = intWk + %Div(intWkDays:7);
       intWkDays = %Rem(intWkDays:7);
       scTtlWk   = %Char(intWk);
       scTtlDays = %Char(intWkDays);
       //****** end chg ******** 09/23/14 **********************************

       // Total Amount
       scTtlAmt = scTtlAmt + scAmount;

       // ***** Beg Add ****** 07/11/14 *******************
       // EOB#
       dspEOB# = *off;
       dspEobLnk = *on;
       scEob# = PAeob#;
       scEob#2 = PAeob#;

       If PAeob# = 0 or ##Mcif <> 'Y';
          dspEobLnk = *off;
          // ***** Beg Add ****** 12/08/15 *******************
          dspEOB# = *on;
          // ***** End Add ****** 12/08/15 *******************
       Else;
          scEobtip = 'Reprint EOB - ' + %Trim(%EditC(PAeob#:'4'));
          strEobTitle = 'Print Eob#: ' + %Trim(%EditC(PAeob#:'4'));
          //***** Beg Chg ****** 09/09/14 *******************
          //strEobRrn = %EditC(WDPAPrrn:'X');

          If PAxFyr<>0 Or PAxCas<>0;
          //*** Beg Chg *** 11/08/19 ***********************************
             //***strEobRrn =
             //**%EditC(GetFaeopRrn(PAco#:PAfnd:PAxFyr:PAxCas:PAeob#):'X');
          Callp GetFaeopRrn(PAco#:PAfnd:PAxfYr:PAxCas:PAeob#:strEobRrn);
          //*** End Chg *** 11/08/19 ***********************************
             If strEobRrn='000000000';
          //*** Beg Chg *** 11/08/19 ***********************************
          //*** strEobRrn =
          //***  %EditC(GetFaeopRrn(PAco#:PAfnd:PAfYr:PAcase:PAeob#):'X');
          Callp GetFaeopRrn(PAco#:PAfnd:PAfYr:PAcase:PAeob#:strEobRrn);
          //*** End Chg *** 11/08/19 ***********************************
             Endif;
          Else;
          //*** Beg Chg *** 11/08/19 ***********************************
          //*strEobRrn =
          //*  %EditC(GetFaeopRrn(PAco#:PAfnd:PAfYr:PAcase:PAeob#):'X');
          Callp GetFaeopRrn(PAco#:PAfnd:PAfYr:PAcase:PAeob#:strEobRrn);
          //*** End Chg *** 11/08/19 ***********************************
          EndIf;
          //***** End Chg ****** 09/09/14 *******************
          //*** Beg Chg *** 11/08/19 ***************************************
          //***strAdjState = GetAdjstate(PAco#:PAfnd:PAfYr:PAcase);
          Callp GetAdjstate(PAco#:PAfnd:PAfYr:PAcase:strAdjState);
          //*** End Chg *** 11/08/19 ***************************************
          strMode='D';
          strQueue='QPRINT';
          scEobLink = CreateURL('F0340C':strEobTitle:strEobRrn:strAdjState:
           strMode:strQueue);
       Endif;
       // ***** End Add ****** 07/11/14 *******************
      /End-Free
     P GetPayeeInfo    E
      *================================================================
      *  GetPayeeName  - Assign Payee Name from WDRPP, WMPDP, or WMCDP
      *================================================================
     P GetPayeeName    B
     D GetPayeeName    PI
      *
      *     Access Payment Detail Detail file
      *
     C     keyWDRPL3     Klist
     C                   Kfld                    PACo#
     C                   Kfld                    PAFnd
     C                   Kfld                    PAFyr
     C                   Kfld                    PACase
     C                   Kfld                    PASeq#

     C     keyWDRPL      Klist
     C                   Kfld                    PACo#
     C                   Kfld                    PAFnd
     C                   Kfld                    PAFyr
     C                   Kfld                    PACase
     C                   Kfld                    PAChk#

     C     keyWMCML1     Klist
     C                   Kfld                    PACo#
     C                   Kfld                    PAFnd
     C                   Kfld                    PAFyr
     C                   Kfld                    PACase
      *
     C     keyWMPDL1     Klist
     C                   Kfld                    PAFein
     C                   KFLD                    PAMod
      *
     C     keyWTGTL      klist
     C                   Kfld                    PAco#
     C                   Kfld                    ##TbId
     C                   Kfld                    ##KEY
      *

      /Free
       //***  Begin Add  ***  08/05/20  ***********************
       scPayeeNam = GetCheckPayeeName(PAco#:PAfnd:PAfyr:PAcase:PAseq#);
       Return;
       //***  End   Add  ***  08/05/20  ***********************
       Chain  keyWDRPL3 prp03;
       If not %Found();
          intTrdt = PATrdt;
          Chain  keyWDRPL  prp000;

          If   not %Found();
             intTrdt  =  PAPodt;
             Chain  keyWDRPL  prp000;
          Endif;
       Endif;

       //Record is found in WDRPP

       If  %found();
          scPayeeNam = RPPnam ;
       //Record Not found in WDRPP
       else;
       //Access Claim Master
          Chain  keyWMCML1  pcm01 ;
          If not %found();
             CMSs# = *zeros ;
          Endif;

       // If FEIN# > 0 & FEIN# NE CMSS#, access Payee Demographics

          strFein  =   %editc(PAfein:'X') ;
          If PAfein <> 0 and strFein <> CMSs#
          and PAretp  <>  'C'  ;

       //  Access Payee Demographics
             Chain   keyWMPDL1   ppd00  ;
             If not %found();
                scPayeeNam =  *BLANKS ;
             else ;
                If  CDFsn = *blanks  ;
                   scPayeeNam   = PDName ;
                else ;
                   ##Last =  PDName ;
                   ##Frst =  PDFsn  ;
                   ##Term =  '1'    ;
                   GetName(##Last:##Frst:##Term);
                   scPayeeNam = ##Last;
                Endif ;
             Endif;
         //  FEIN# = 0 Or FEIN# = CMSS#, access Claimant Demographics
          Else ;

        // Access Claimant Demographics
        //
             Chain  keyWMCDL  pcd00;
             If   not %Found();
                scPayeeNam =  *BLANKS ;
             else ;
                If  CDFsn = *blanks  ;
                   scPayeeNam  = CDLsn ;
                else ;
                   ##Last =  CDLsn  ;
                   ##Frst =  CDFsn  ;
                   ##Term =  '1'    ;
                   GetName(##Last:##Frst:##Term);
                   scPayeeNam  =  ##last;
                Endif ;
             Endif ;
          Endif ;
       Endif ;

      /end-Free
     P GetPayeeName    E
      *================================================================
      *GetRevAmt    - Get the reversal amount and process
      *================================================================
     P GetRevAmt       B
     D GetRevAmt       PI

      /free

       erDedAmt = *off;
       scDedAmt = -1 * scAmount;
       isInvalid= *off;

       Dou (scDedCnt = *on and isInvalid = *off) or scDedCan = *on;
          Exfmt rcdDedRec;
          isInvalid= *off;

          If scDedCan = *on;
             Clear scDedAmt;
             Leave;
          Endif;

          If scDedCnt = *on;

             // Reversal amount can not be less than or equal to zero.
             If scDedAmt <= 0;
                msDedAmt  = 'WCR0502';
                isInvalid = *on;
                erDedAmt  = *on;
             Endif;

             // Reversal amount can't be greater than original recovery
             If scDedAmt > (-1 * scAmount);
                msDedAmt  = 'WC30195';
                isInvalid = *on;
                erDedAmt  = *on;
             Endif;

             // If no error process net deductible recovery reversal
             If isInvalid = *off;
            // *** Beg Chg *** 08/05/14 ****************************************
            //  ##seq# =  %editc(PAseq#:'X');
                ##seq# =  %editc(scSeq#:'X');
            // *** End Chg *** 08/05/14 ****************************************
                ##amt# =  %editc(scDedAmt:'X');
                ##rtcd =  *blanks;
                ProcDedRec(##co:##fnd:##fyrc:##casec:##seq#:##amt#:##rtcd);
             Endif;

          Endif;
       Enddo;

      /end-free

     P GetRevAmt       E
      *================================================================
      *GetStsInfo   - Load the values in the grid
      *================================================================
     P GetStsInfo      B
     D GetStsInfo      PI

      *
      /free
       Select;
       When CHocst = 'O';
            scStsStats  =  'Open';
       When CHocst = 'C';
          scStsStats  =  'Close';
       Other;
          scStsStats  =  '  ';
       Endsl;

       scStsreasn   =  CHexpl ;
       scstsUser    =  getUsrName(CHuser) ;
       scStsEffdt = CvtToDate(CHefdt);

      /end-free

     P GetStsInfo      E
       //===================================================================
       // LodEdtReq    - Loads the edit request information
       //===================================================================
     P LodEdtReq       B
     D LodEdtReq       PI
      /Free

       //*** Beg Add *** 04/10/15 *********************************************
       scCO#  = RPCo# ;
       //*** End Add *** 04/10/15 *********************************************

       //*** Begin Add 12/08/16 ***********************************************
       intsvAmt = *zeros;
       //*** Begin Add 12/08/16 ***********************************************

      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrnTm = OutCmRrn;
      //**** End Add  ***  06/23/21  ****************************************
       Clear rcdReqEdit;
      //**** Begin Add  ***  06/23/21  ****************************************
       OutCmRrn = OutCmRrnTm;
      //**** End Add  ***  06/23/21  ****************************************
       Chain(n) scReqRrn# prp0p;

       scPayName = RPpnam;
       scPayAdr1 = RPadr1;

       //get the Payee Address information as per the logic
       GetPayee();

       scPayAdr2 = strDtlAdr2;
       If scPayAdr2=*Blanks;
          scPayAdr3 = strDtlcsz;
       Else;
          scPayAdr2 = strDtlcsz;
       EndIf;

       scRemark  = RPrmds;
       scRemark1 = RPrmk1;
       scRemark2 = RPrmk2;
       scRemark3 = RPrmk3;
       scPtDtEdt = dtMDYY(RPtrdt);
       scSerFrDt = dtMDYY(RPfrdt);
       scSerToDt = dtMDYY(RPtodt);
       scRcvdDt  = dtMDYY(RPrcvd);
       scPayStsCd= RPsts;
       strsvSts  = RPsts;
       scPayVal1 = RPbr2;
       scPayVal2 = RPbr7;
       scPayDesc = RPdesc;
       scPayAmt  = RPamt;
       scPdTime  = RPltcd;
       scWeekEdt = RPwks;
       scDayEdt  = RPdays;
       scGrpChk = RPpgTp;
       scFinCmp = RPfnal;
       scEFTEdt = RPeftc;
       //*** Begin Add *** 10/22/20 ***************************************
       // Initialize what type is diplayed
       dspMicrSts = *Off;
       dspEftSts  = *Off;
       dspEpaySts = *Off;

       // Determine what is a valid bank code
       #2co#  = %EditC(RPco#:'X');
       #2fnd  = %EditC(RPfnd:'X');
       #2Appl = 'WC';
       #2Date = %EditC(DateTo8(systemDate()):'X');
       #2Call = ' ';
       #2HdCy = ' ';
       #2ChkQ = ' ';
       #2Ftyp = ' ';
       #2EfDt = ' ';
       #2CpyQ = ' ';
       #2CkTp = ' ';
       #2Coid = ' ';
       #2RtCd = ' ';

       // Is MICR Valid
       #2eftC = ' ';
       outAcct = getBankAcc(#2Co#:#2Fnd:#2Appl:#2date:#2Call:#2HdCy:
                  #2ChkQ:#2Ftyp:#2EfDt:#2CpyQ:#2CkTp:#2EfTc:
                  #2Coid:#2RtCd);
       If RPeftC = ' ' or outAcct <> ' ';
          dspMicrSts = *On;
       EndIf;

       // Is EFT Valid
       ValidEFT();
       If strmode <> 'X' Or RPeftC = 'E';
          dspEftSts = *On;
       Else;
          dspEftSts = *Off;
       EndIf;

       // Is EPay Valid
       #2Call = ' ';
       #2HdCy = ' ';
       #2ChkQ = ' ';
       #2Ftyp = ' ';
       #2EfDt = ' ';
       #2CpyQ = ' ';
       #2CkTp = ' ';
       #2Coid = ' ';
       #2RtCd = ' ';
       #2eftC = 'V';
       outAcct = getBankAcc(#2Co#:#2Fnd:#2Appl:#2date:#2Call:#2HdCy:
                  #2ChkQ:#2Ftyp:#2EfDt:#2CpyQ:#2CkTp:#2EfTc:
                  #2Coid:#2RtCd);

       //*** Beg Chg *** 04/19/21 ***************************************
       //***If getWcEPay(RPco#:#2Appl:RPfnd:outAcct:RPtrDt:RPreTp)
       //*** <> *blanks Or RPeftC = 'V';
       If getWcEPay(RPco#:#2Appl:RPfnd:outAcct:RPtrDt:RPreTp:RPckTp)
        <> *blanks Or RPeftC = 'V';
       //*** End Chg *** 04/19/21 ***************************************
          dspEpaySts = *On;
          //*** Begin Change *** 06/10/21 ************************************
          //If RPeftC <> ' ';
          //   dspMicrSts = *Off;
          //EndIf;
          Select;
          When RPeftc = 'E';
             dspMicrSts = *Off;
          When RPeftc = 'V' and not MICRauth(RPco#:outAcct:inMICRauth);
             dspMicrSts = *Off;
          Endsl;
          //*** End   Change *** 06/10/21 ************************************
       Endif;

       // Now Let's set which buttons are displayed
       Select;
       When dspMicrSts = *On;
          posMICR = 560;
          If dspEftSts = *On;
             posEFT  = 665;
             If dspEpaySts = *On;
                posEPAY = 770;
             EndIf;
          Else;
             If dspEpaySts = *On;
                posEPAY = 665;
             EndIf;
          EndIf;
       When dspEftSts  = *On;
          posEFT  = 560;
          If dspEpaySts = *On;
             posEPAY = 665;
          EndIf;
       When dspEpaySts = *On;
          posEPAY = 560;
       EndSl;
       //*** End   Add *** 10/22/20 ***************************************
       svDt     = RPdt;
       svTime   = RPtime;
       //*** Beg Add *** 10/25/12 *********************************************
       If strCLTID = '114';
          If (RPretp = 'E') or (RPretp = 'L')
           or (RPretp = 'M' and RPrecd = 'RE');
             dspInv2# = *On;
             scInv# = RPinv#;
          Endif;
       Endif;
       //*** End Add *** 10/25/12 *********************************************

       //*** Begin Chg *** 10/01/14 *******************************************
       //*** If RPstat = 'Y';
       //*** Begin Chg *** 04/20/15 *******************************************
       //***If RPstat = ' ';
       If RPstat = 'Y';
       //*** End   Chg *** 04/20/15 *******************************************
       //*** End   Chg *** 10/01/14 *******************************************
          dspreq = *on;
          dspgrp = *on;
       Else;
          dspreq = *off;
          dspgrp = *off;
       Endif;

       //Validate group check
       dspreqgrp = *on;
       If RPpgtp <> 'G' and RPstat <> 'D';
          Chain (RPco#:RPfein:RPmod) pnr00;
          If not %found(WMNRL);
             dspgrp = *on;
             dspreqgrp = *off;
          Else;
       //*** Begin Chg *** 10/01/14 *******************************************
       //*** If RPstat <> 'Y';
             If RPstat <> ' ';
       //*** End   Chg *** 10/01/14 *******************************************
                dspgrp = *off;
             Endif;
             dspreqgrp = *on;
          Endif;
       Endif;

       //*** Begin Del *** 10/22/20 ****************************************
       //Validate for EFT setup
       //*** dspEftSts = *on;
       //*** ValidEFT();
       //*** If strmode ='X';
       //***    dspEftSts = *off;
       //*** Else;
       //***    dspEftSts = *on;
       //*** EndIf;
       //*** End   Del *** 10/22/20 ****************************************

       isValid = *off;

       // Loop until filter applied or cancelled.

       DoU scCanEdt = *On or (scSaveEdt = *On and isValid = *off);

          Exfmt  rcdReqEdit;

          isValid = *off;
          clear scErrMsg;

          If scCanEdt = *on;
             Leave;
          Endif;

          //If chgEftSts = *on;
          //   Chain scReqRrn# prp0p;
          //   chgEftSts = *off;
          //   ValidEFT();
          //   If strmode = 'X';
          //      clear scErrMsg;
          //      scErrMsg = 'Bank account not setup for EFT processing. Check +
          //                  EFT information for account.';
          //      If scErrMsg <> ' ';
          //         Dou scErrCan = *on;
          //            Exfmt rcdChkErr;
          //         Enddo;
          //      Endif;
          //   Else;
          //      If RPeftc <> strSvEftc;
          //         scEftEdt = RPeftc;
          //         RPuser   = q1user;
          //         RPpgm    = q1pgm;
          //         RPdt     = Dateto7(SystemDate());
          //         RPtime   = Timeto6(SystemTime());
          //         update prp0p;
          //      Endif;
          //   Endif;
          //Endif;

          If scSaveEdt = *on;
             Chain scReqRrn# prp0p;
             ValidEdtReq();
             if isValid = *off;
                //*** Begin Add *** 06/10/21 *****************************
                If scEftEdt = *blank and RPeftc = 'V' and RPpgtp = 'G' and
                   dspEpaySts = *On;
                   Exfmt rcdWdW06;
                   If btnMICRcan = *On;
                      btnMICRcan = *Off;
                      Iter;
                   Endif;
                Endif;
                //*** End   Add *** 06/10/21 *****************************
                SaveEdtReq();
                update prp0p;
          //*** Begin Add *** 02/19/15 ***********************************
                If strOldStat <> strNewStat;
                   If strNewStat = 'D';
                      GetDelStatus();
                      strSvStat = strOldStat;
                      ContDelProc();
                   EndIf;
                EndIf;
          //*** End   Add *** 02/19/15 ************************************
          //*** Begin Add *** 06/22/13 ***********************************
             Else;
                Unlock WARPP;
          //*** End   Add *** 06/22/13 ***********************************
             Endif;
          Endif;
       Enddo;

       Unlock WARPP;

      /End-Free
     P LodEdtReq       E
       //===================================================================
       // LoadPayee    - Loads the Payement Information
       //===================================================================
     P LoadPayee       B
     D LoadPayee       PI
      /Free

       //****** beg add ***** 05/20/14 ****************
       // Display link if Managed Care is installed

       EOBImMngC = *off;

       If ##MCif <> 'Y';
          EOBImMngC = *on;
       Endif;

       //****** end add ***** 05/20/14 ****************

       rrnPayee   = 0;
       clrPayee   = *on;
       dspPayee   = *off;
       Write rcdPayDtl;
       dspPayee  = *on;
       clrPayee  = *off;

       // *** Beg Chg *** 03/14/13 *****************************************
       // Clear scTtlWkDys;
       Clear scTtlWk;
       Clear scTtlDays;
       //*** End Chg *** 03/14/13 *****************************************
       Clear scTtlAmt;
       Clear intWk;
       Clear intWkDays;

       //
       //Retrive the information to be shown on grid

       //*** Begin Add *** 12/19/18 *****************************
       pmtCount = 0;
       //*** End   Add *** 12/19/18 *****************************
       kkTrDt=*Hival;
       SetGt  Key2WDPAL5  ppa05;
       ReadPe Key1WDPAL5  ppa05;
       Dow Not %Eof(Wdpal5);
          ProcessFilter();

          //*** Begin Add *** 12/19/18 ************************************
          pmtCount = pmtCount + 1;
          If pmtCount > 9999;
             Leave;
          EndIf;
          //*** End   Add *** 12/19/18 ************************************

          If @@inType = '2' or @@inType = '4' or @@inType = '6' or
             @@inType = '8' or @@inType = '0';
             ##Key = PAbr2;
             Chain keyWTABL pb200;
             If not %Found(WTABL) or B2rcvR <> 'Y';
                isValid = *off;
             EndIf;
          Endif;
          //***** Beg Add ****** 08/04/14 *******************************   ***
          // Process all transaction types except R (Recovery)              ***
          If (@@inType = 'A' or @@inType = 'B' or @@inType = 'C' or
           @@inType = 'D' or @@inType = 'E') and PAtype = 'R';
             ##Key = PAbr2;
             Chain keyWTABL pb200;
             If not %Found(WTABL) or B2rcvR = 'Y';
                isValid = *off;
             EndIf;
          Endif;
          //***** End Add ****** 08/04/14 *************************************

          If isValid = *on;
          // Process to exclude hospital bills for users having Title as
          // 'CLIENT' from payment Information grid under the payment Detail
          // tab for the client 114.

             chgPmtExcl=*Off;
       //*** Begin Add *** 05/09/23 ****************************************
             payEdtLnk=*Off;
       //*** End   Add *** 05/09/23 ****************************************
             selPayee=*Off;
       // Begin Del *** 12/18/2012 *****************************************
       //    If outClientID = '114';
       //       ChkHospPBill();
       //       If ##btyp <> 'H';
       //          GetPayeeInfo();
       //          rrnPayee  = rrnPayee + 1;
       //          selPayee=*Off;
       //          If rrnPayee=scRowNum;
       //             selPayee=*On;
       //          EndIf;
       //          shwPmtLnk=*On;
       //          If dspMode=*On;
       //             shwPmtLnk=*Off;
       //          EndIf;
       //          Write sflPayee;
       //       EndIf;
       //    Else;
       // End   Del *** 12/18/2012 *****************************************
             GetPayeeInfo();
             rrnPayee = rrnPayee + 1;
             selPayee=*Off;
             If rrnPayee=scRowNum;
                selPayee=*On;
             EndIf;
             shwPmtLnk=*On;
             If dspMode=*On;
                shwPmtLnk=*Off;
             EndIf;
             //*** Begin Add *** 05/09/2023 ********************************
                // If New Payment System (Install Flag is blank) then
                // allow the Edit Link to show.
                //*** Begin Chg *** 09/29/2023 *** 114152 ******************
                //***If flgNewPay;
                If flgNewPay and PApgtp <> 'G';
                //*** End   Chg *** 09/29/2023 *** 114152 ******************
                   shwEdtLnk = *on;
                Else;
                   shwEdtLnk = *off;
                Endif;
             //*** End   Add *** 05/09/2023 ********************************
             Write sflPayee;
       // Begin Del *** 12/18/2012 *****************************************
       //    EndIf;
       // End   Del *** 12/18/2012 *****************************************
          Endif;
          ReadPe Key1WDPAL5  ppa05;
       Enddo;

       //***** Beg Chg ******** 08/18/16 ***********************************
       //***** beg add ******** 06/01/15 ***********************************
       //If strCLTID = '090';
       //   scChkLeft = 277;
       //Else;
       //   scChkLeft = 275;
       //Endif;
         scChkLeft = 275;
       //***** beg add ******** 06/01/15 ***********************************
       //***** End Chg ******** 08/18/16 ***********************************

      /End-Free
     P LoadPayee       E
      *================================================================
      *LoadPmtSum   -  Load the data in arrays for payment summary Grids  (Exist
      *================================================================
     P LoadPmtSum      B
     D LoadPmtSum      PI

     C     PAKEY         KLIST
     C                   KFLD                    CMco#
     C                   KFLD                    CMfnd
     C                   KFLD                    CMfyr
     C                   KFLD                    CMcase

     C     TBKEY         KLIST
     C                   KFLD                    UPCO#

      *   Load Payment Codes into array
      *
     C                   MOVE      *BLANKS       CD
     C                   MOVE      *BLANKS       DS
     C                   Z-ADD     *ZERO         AM
     C                   Z-ADD     *ZERO         WK#
     C                   Z-ADD     *ZERO         DA
     C                   Z-ADD     1             X                 3 0
     C     TBKEY         CHAIN     PPC00                              50
      *
     C     *IN50         DOWEQ     '0'
     C     X             ANDLT     SIZE
     C                   MOVEL     PCKEY         CD(X)
     C                   MOVEL     PCDESC        DS(X)
     C                   ADD       1             X
     C     TBKEY         READE     PPC00                                  50
     C                   ENDDO
      *
     C     X             IFLE      SIZE
     C                   MOVEL     NOTFND        DS(X)
     C                   Z-ADD     X             NOTFD             3 0
     C                   ENDIF
      *
      *   Load Payment Value 1 Codes into array
      *
     C                   MOVE      *BLANKS       CD3
     C                   MOVE      *BLANKS       DS3
     C                   Z-ADD     *ZERO         AM3
     C                   Z-ADD     *ZERO         WK3
     C                   Z-ADD     *ZERO         DA3
     C                   Z-ADD     1             X                 3 0
     C     TBKEY         CHAIN     PB200                              50
      *
     C     *IN50         DOWEQ     '0'
     C     X             ANDLT     SIZE
     C                   MOVEL     B2KEY         CD3(X)
     C                   MOVEL     B2DESC        DS3(X)
     C                   ADD       1             X
     C     TBKEY         READE     PB200                                  50
     C                   ENDDO
      *
     C     X             IFLE      SIZE
     C                   MOVEL     NOTFND        DS3(X)
     C                   Z-ADD     X             NOTFD2            3 0
     C                   ENDIF
      *
      *   Access ALL Claim Payment Detail records
      *
      *
     C     PAKEY         CHAIN     PPA00                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
      *   Find Payment Code index
      *
     C                   Z-ADD     1             X                 3 0
     C     PATRCD        LOOKUP    CD(X)                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     NOTFD         X
     C                   ENDIF
      *
      *   Find Payment Value 1 code index
      *
     C                   Z-ADD     1             Y                 3 0
     C     PABR2         LOOKUP    CD3(Y)                                 60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     NOTFD2        Y
     C                   ENDIF
      *
      *   Accumulate Amounts for Payment Code and Payment Value 1
      *
      *
     C     PATYPE        IFEQ      'P'
     C     PATYPE        OREQ      'M'
     C     PATYPE        OREQ      'A'
     C     PATYPE        OREQ      'Z'
     C                   ADD       PAAMT         AM(X)
     C                   ADD       PAAMT         AM3(Y)
     C                   ELSE
     C                   SUB       PAAMT         AM(X)
     C                   SUB       PAAMT         AM3(Y)
     C                   ENDIF
      *
      *  Accumulate Days and Weeks

      *  Increment Weeks and Days Arrays
      *
     C     PATYPE        IFEQ      'P'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'M'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'A'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'Z'
     C     PAAMT         ANDGT     0
     C     PATYPE        OREQ      'R'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'S'
     C     PAAMT         ANDLT     0
     C     PATYPE        OREQ      'V'
     C     PAAMT         ANDLT     0
      *
     C                   Z-ADD     DA(X)         ##DAYS            5 0
     C                   Z-ADD     WK#(X)        ##WKS             5 0
      *
     C                   ADD       PAWKS         ##WKS
     C     ##DAYS        ADD       PADAYS        XXDAYS            5 0
     C     XXDAYS        IFGE      7
     C     XXDAYS        DIV       7             XXWEEK            1 0
     C                   ADD       XXWEEK        ##WKS
     C                   MULT      7             XXWEEK
     C     XXDAYS        SUB       XXWEEK        ##DAYS
     C                   ELSE
     C                   Z-ADD     XXDAYS        ##DAYS
     C                   END
      *
     C                   Z-ADD     ##DAYS        DA(X)
     C                   Z-ADD     ##WKS         WK#(X)
      *
     C                   Z-ADD     DA3(Y)        ##DAYS
     C                   Z-ADD     WK3(Y)        ##WKS
      *
     C                   ADD       PAWKS         ##WKS
     C     ##DAYS        ADD       PADAYS        XXDAYS            5 0
     C     XXDAYS        IFGE      7
     C     XXDAYS        DIV       7             XXWEEK            1 0
     C                   ADD       XXWEEK        ##WKS
     C                   MULT      7             XXWEEK
     C     XXDAYS        SUB       XXWEEK        ##DAYS
     C                   ELSE
     C                   Z-ADD     XXDAYS        ##DAYS
     C                   END
      *
     C                   Z-ADD     ##DAYS        DA3(Y)
     C                   Z-ADD     ##WKS         WK3(Y)
      *
     C                   ELSE
      *
     C                   Z-ADD     DA(X)         ##DAYS            5 0
     C                   Z-ADD     WK#(X)        ##WKS             5 0
      *
     C                   SUB       PAWKS         ##WKS
     C     ##DAYS        SUB       PADAYS        XXDAYS
     C     XXDAYS        IFLT      0
     C                   SUB       1             ##WKS
     C     7             ADD       XXDAYS        ##DAYS
     C                   ELSE
     C                   Z-ADD     XXDAYS        ##DAYS
     C                   END
     C                   Z-ADD     ##DAYS        DA(X)
     C                   Z-ADD     ##WKS         WK#(X)
      *
     C                   Z-ADD     DA3(Y)        ##DAYS
     C                   Z-ADD     WK3(Y)        ##WKS
     C                   SUB       PAWKS         ##WKS
     C     ##DAYS        SUB       PADAYS        XXDAYS
     C     XXDAYS        IFLT      0
     C                   SUB       1             ##WKS
     C     7             ADD       XXDAYS        ##DAYS
     C                   ELSE
     C                   Z-ADD     XXDAYS        ##DAYS
     C                   END
      *
     C                   Z-ADD     ##DAYS        DA3(Y)
     C                   Z-ADD     ##WKS         WK3(Y)
      *
     C                   END
      *
     C     NEXT          TAG
      *
     C     PAKEY         READE     PPA00                                  60
     C                   ENDDO
     C*
     C                   CallP     GetSumInfo
     C*
     P LoadPmtSum      E
      *================================================================
      *LodBenChart  - Load data for Benefit Chart
      *================================================================
     P LodBenChart     B
     D LodBenChart     PI

      /Free
       // Build Benefits SQL Select for Chart

       scBenVal = 'case when patype in (''R'', ''S'', ''V'') then ' +
        'paamt*-1 else paamt end';

       scBenSelec = 'pafnd=' + %EditC(PAfnd:'X') +
        ' and PAfyr=' + %EditC(PAfyr:'X') +
        ' and PAcase=' + %EditC(PAcase:'X') + ' and PAretp=''C''';

      /End-Free
     P LodBenChart     E
      *================================================================
      *LodHstInfo   - Load history data in Reserve tab
      *================================================================
     P LodHstInfo      B
     D LodHstInfo      PI
      /Free

       //*** Begin Del *** 06/27/18 ****************************
       //**** rrnHst     = 0;
       //**** clrHist    = *on;
       //**** dspHist    = *off;
       //**** Write rcdRsvDtl;
       //*** End   Del *** 06/27/18 ****************************
       dspHist   = *on;
       clrHist   = *off;
       intEfDt   = 9999999;

       //Retrive the information to be shown on grid

       Setll  keyWmrhl5  prh05;
       Reade  keyWmrhl5# prh05;
       Dow Not %Eof(Wmrhl5);

         // Populate the change eff date
          scHstDatCh = CvtToDate(RHefdt);

         // Populate the User
         //
          scHstUser = getUsrName(RHuser);

         // Populate the reason of Change
         //
          scHstReasn = RHexpl;

         // Populate the RRn#
         //
          scHstRRn# = WMRHPRrn;

          SrChg();

         // Populate compensation/Medial/legal/Expense
         //
          scHstComp  =  decCcHg  ;
          scHstMedic =  decMcHg  ;
          scHstLegal =  decLcHg  ;
          scHstExpen =  decEcHg  ;

          rrnHst    = rrnHst + 1;
          selHist=*Off;
          If rrnHst=1;
             selHist=*On;
          EndIf;
          Write SflReserve;
          Reade keyWmrhl5# prh05;
       Enddo;

      /End-Free
     P LodHstInfo      E
      *================================================================
      *LodReqInfo   - Load Pending Check data in Request tab
      *================================================================
     P LodReqInfo      B
     D LodReqInfo      PI
      /Free

       rrnReq  = 0;
       clrRequest = *on;
       dspRequest = *off;
       Write rcdPndChk;
       dspRequest = *on;
       clrRequest = *off;
       scRlsFst   = *off;
       //*** Begin Add *** 04/04/23 ****************************
       scRowSel = 1;
       //*** End   Add *** 04/04/23 ****************************
       //Retrive the information to be shown on grid

       Setll  keyWARPL prp00;
       Reade(n)  keyWARPL prp00;
       Dow Not %Eof(Warpl);

          // Populate the Payee
          scReqPayee = RPpnam;

          // Populate the Service date
          scReqFdt   = CvtToDate(RPfrdt);
          scReqTdt   = CvtToDate(RPtodt);

          // Populate the Post date
          scReqPdt   = CvtToDate(RPtrdt);

          // Populate the date received
          scReqRdt   = CvtToDate(RPrcvd);

          // Populate the RRn#
          scReqRrn#  = WARPPRrn;

          // Populate the amount
          scReqAmt   = RPamt;

          // Populate the release/pending Link
          If RPstat = ' ';
             scReqPnRls = 'Pend';
          Else;
             scReqPnRls = 'Release';
          Endif;

          scReqPV1   = RPbr2;
          scReqResCd = RPrecd;

          If RPpgTp='G';
             scReqGrp='Yes';
          Else;
             scReqGrp='No';
          EndIf;

          //*** Beg Chg *** 07/20/20 ***************************************
          //***If RPeftC='E';
          //***   scReqEft='Yes';
          //***Else;
          //***   scReqEft='No';
          //***EndIf;
          Select;
          When RPeftC = 'E';
             scReqType = 'EFT';
          When RPeftC = 'V';
             scReqType = 'E-Pay';
          Other;
             scReqType = 'Check';
          Endsl;
          //*** End Chg *** 07/20/20 ***************************************

          scReqSts=RPsts;


          rrnReq = rrnReq + 1;

          //*** Begin Add *** 01/19/18 ******************************
          scRlsFst = rlsd(rrnReq);
          //*** End   Add *** 01/19/18 ******************************

          selRequest=*Off;
          //*** Begin Chg *** 04/04/23 ******************************
          //*** If rrnReq=1;
          //***    selRequest=*On;
          //*** EndIf;
          // set the selection on 1st record for first load
          If svrrnReq = *zeros;
             If rrnReq=1;
                selRequest=*On;
             EndIf;
          Else;
             If svrrnReq = rrnReq;
                selRequest=*On;
             Endif;
          Endif;
          //*** End   Chg *** 04/04/23 ******************************
          shwReqLnk=*On;
          If dspMode=*On;
             dspReqDel=*Off;
             shwReqLnk=*Off;
          EndIf;
          Write SflPenReq;
          Reade(n) keyWARPL prp00;
       Enddo;

       If rrnReq > 0;
          scNoRecFnd = *off;
          scRecFnd = *on;
          //*** Beg Add *** 10/25/12 ******************************************
          //*** Beg Chg *** 08/15/19 ******************************************
          //***If strCLTID = '114';
          If strCLTID = '114' or strCltId = '152';
          //*** End Chg *** 08/15/19 ******************************************
             dspInv1# = *On;
          Else;
             dspInv1# = *Off;
          Endif;
          //*** End Add *** 10/25/12 ******************************************
       Else;
          scNoRecFnd = *on;
          scRecFnd = *off;
          //*** Beg Add *** 10/25/12 ******************************************
          dspInv1#  = *Off;
          //*** End Add *** 10/25/12 ******************************************
       Endif;

       //*** Begin Add *** 04/04/23 ******************************
       rrnReq = 1;
       If svrrnReq  <> 0;
          rrnReq = svrrnReq;
          scRowsel = svrrnReq;
          svrrnReq = 0;
       Endif;
       //*** End   Add *** 04/04/23 ******************************
      /End-Free
     P LodReqInfo      E
      *================================================================
      * LodDedInfo   - Load deductible history data in tab
      *================================================================
     P LodDedInfo      B
     D LodDedInfo      PI
      /Free

       ChkEmpDed();

       rrnDedHst = 0;
       clrDedHst = *on;
       dspDedHst = *off;
       Write rcdDedHst;
       dspDedHst = *on;
       clrDedHst = *off;
       clear scTtlAmBld;
       clear scTtlAmRcv;
       clear scTtlAmBln;

       //Retrive the information to be shown on grid

       Setll keyWDDDL pdd00;
       Reade keyWDDDL pdd00;
       Dow Not %Eof(WDDDL);

          // populate the year
          scDedBYr   = DDbfyr;

          // populate the billing date
          scDedBPtDt = CvtToDate(DDpodt);

          // Populate the reserve code
          scDedResCd = DDrecd;

          // Populate the amount
          scDedAmtBl = DDamtb;
          scDedAmtRc = DDamtr;
          scDedAmBln = DDamtb - DDamtr;
          scTtlAmBld = scTtlAmBld + scDedAmtBl;
          scTtlAmRcv = scTtlAmRcv + scDedAmtRc;
          scTtlAmBln = scTtlAmBln + scDedAmBln;

          // Populate the RRn#
          scDedRrn#  = WDDDLRrn;

          rrnDedHst = rrnDedHst + 1;
          Write SflDedHst;
          Write SflDedHstr;
          Reade keyWDDDL pdd00;
       Enddo;

      /End-Free
     P LodDedInfo      E
      *================================================================
      *LodRsvInfo   - It will load The Reserve Grid (W0070R12/MVDBSC)
      *================================================================
     P LodRsvInfo      B
     D LodRsvInfo      PI

      *
      *
     C     CXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  SCREEN 1
      *
      *  BLANK RSV TYPE AND CHANGES
      *
      *
      *  LOAD THE RESERVE, PAYMENT, RESERVE CODE, AND TOTAL INCURRED
      /free

       Chain(n) inRRNClm dbrcdf;

       //rrnPmtSum  = 0;
       //clrReserve = *on;
       //dspReserve = *off;
       //Write rcdRsvDtl;
       dspReserve = *on;
       ClrReserve = *off;
       //******* beg add ******** 12/01/14 ***************************
       //******* beg add ******** 10/27/15 ***************************
       // intIndTotal = 0;
       // intMedTotal = 0;
       intTotIncd  = 0;
       //******* end add ******** 10/27/15 ***************************
       //******* end add ******** 12/01/14 ***************************

       //
       //Retrive the information to be shown on grid

       For i = 1 to 10;

          scRsvRemR = RR(i);
          scRsvResC = RT#(i);
          scRsvPmtR = AR(i);
          scRsvTotI = RR(i) + AR(i);
          TI(i)     = RR(i) + AR(i);

          ##key = RT#(i);
          Chain keyWTABL prt00  ;
            If %found();
               scRsvResT =  RTtype ;
            Else ;
               scRsvResT =  *blanks;
            Endif;

          If RT#(i) <> *blanks;
             rrnPmtSum  = rrnPmtSum + 1;
             Write sflPmtSum;
          Endif;
          //******* beg add ******** 12/01/14 ***************************
          clear scWrkURL;
          clear scAddEdtTT;
          isWrkSht = *off;
          scAddEdtLb = 'Add/Edit';

          Chain CMco# pcw00;
          If %found(WMCWL);
       //******* beg chg ******** 10/27/15 ***************************
       //    If RTtype = 'C';
       //       intIndTotal = intIndTotal + scRsvTotI;
       //    Endif;
       //    If RTtype = 'M';
       //       intMedTotal = intMedTotal + scRsvTotI;
       //    Endif;
       //
       //   Accumalating the Reserve incured total for all reserve types.

             intTotIncd = intTotIncd + scRsvTotI;
       //******* end chg ******** 10/27/15 ***************************
          Endif;
          //******* end add ******** 12/01/14 ***************************

       EndFor;

       //******* beg add ******** 12/01/14 ***************************
       //******* beg chg ******** 10/27/15 ***************************
       // If ((CMltfl = 'I' and CWinfl = 'Y' and intIndTotal >= CWinth) or
       // (CMltfl = 'M' and CWmofl = 'Y' and intMedTotal >= CWmoth)) and
       // CMstat <> 'C';

       If ((CMltfl = 'I' and CWinfl = 'Y' and intTotIncd >= CWinth) or
        (CMltfl = 'M' and CWmofl = 'Y' and intTotIncd >= CWmoth)) and
        CMstat <> 'C';
       //******* End chg ******** 10/27/15 ***************************
          scAddEdtLb = 'Worksheet';
          isWrkSht = *on;
          refrshData = *On;
          scAddEdtTT = 'A Reserve Worksheet must be used to change +
           reserves because the claim has reached or passed the threshold +
           amount or the claim type indicates that a Reserve Worksheet is +
           required.';

          strTitle = 'Reserve Worksheet';
          strRrn#  = %EditC(inRRNClm:'X');
          scWrkURL = CreateURL('STDRVPGM':strTitle:strRrn#:'C':'WTCLMRWK'
           :'00000');
       Endif;
       //******* end add ******** 12/01/14 ***************************

       scRsvTtlRR = %Xfoot(RR);
       scRsvTtlMR = %Xfoot(AR);
       scRsvTtlTI = %Xfoot(RR) + %Xfoot(AR);
       strClmsts  = CMstat ;
       strLtFl    = CMltfl ;
       If CMstat  = 'O';
         Clmsts = 'Close  ';
         scClmsts = 'O';
       Else;
         Clmsts = 'Reopen ';
         scClmsts = 'C';
       Endif;

      /end-free

      *
     C*    Read payments.     25WQO
     C*
     C                   Callp     SumCLP
     C                   Callp     FunRet
     C*
     C*    Auto DARTS
     C*    Based on % of Specific Retention of total incurred on the
     C*    claim
     C*    Get 'CLMRETEN' control record(s).
     C*
     C                   MOVEL(P)  'CLMRETEN'    strKy
     C     keyWMADL      CHAIN     PAD00                              51
     C*
     C                   MOVE      *ZEROS        WK#RET            5 2
     C*
     C     *IN51         DOWEQ     *OFF
     C     ADSTAT        IFNE      'I'
     C                   MOVE      ADGRET        WK#RET
     C     RETPR#        IFGE      WK#RET
     C                   MOVEL(P)  'CLMRETEN'    strKy
     C*
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      strKy         @@DKEY           10
     C                   PARM      *BLANKS       @@COMM          256
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C     keyWMADL      READE     PAD00                                  51
     C*
     C                   ENDDO
     C*
     C                   MOVE      *BLANK        SCEXPL
     C                   MOVE      *BLANK        SCEXCD
     C*
     C
      *
      *
     P LodRsvInfo      E
      *================================================================
      *LodStsInfo   - It will load The Status  Grid (W0575R)
      *================================================================
     P LodStsInfo      B
     D LodStsInfo      PI

      /free

       inEfdt   = 9999999;
       inTime   = 999999;
       //rrnSts     = 0;
       //clrStatus  = *on;
       //dspStatus  = *off;
       //Write rcdRsvDtl;
       dspStatus = *on;
       ClrStatus = *off;

       Setll KeyWMCHL5 pch05;
       Reade KeyWMCHL# pch05;

       Dow Not %Eof(wmchl5);
          GetStsInfo();
          rrnSts  = rrnSts + 1;
          Write SflStsHst;
          Reade KeyWMCHL# pch05;
       Enddo;

      /end-free

     P LodStsInfo      E
       //===================================================================
       // MoveDbSc - Move Database fields to Screen fields
       //===================================================================
     P MoveDbSc        B
     D MoveDbSc        PI
      /Free

       clrSubFilesPay();

       LoadPayee();

       // Load chart now for the values
       LoadPmtSum();

       // Load chart now for the values
       LodBenChart();

      /End-Free
     P MoveDbSc        E
       //===================================================================
       // MoveScDb - Move Screen fields to Database fields   (Existing)
       //===================================================================
     P MoveScDb        B
     D MoveScDb        PI

     C     inRRNClm      CHAIN     DBRCDF                             9697
      *
     C     *IN97         IFEQ      *ON
     C     FLSTCM        ANDEQ     01218
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD            1
     C                   GOTO      ENDP
     C                   END
     C
      *  If the claim is being closed, reduce remaining reserves to 0.
      *
     C     scClmSts      IFEQ      'C'
     C     strClmSts     ANDEQ     'O'
      *
     C                   DO        10            X
     C     RT#(X)        IFNE      *BLANKS
     C                   MOVE      RT#(X)        IR(X)
     C                   Z-SUB     RR(X)         RC(X)
     C                   END
     C                   ENDDO
      *
      *  ADD THE AMOUNT OF ANY RELEASED PAYMENTS BACK INTO THE REMAINING
      *  RESERVES SO THE PAYMENTS CAN BE PROCESSED.
      *
     C     keyWARPL      CHAIN     PRP00                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     RPSTAT        IFEQ      ' '
      *
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    IR(X)                                  61
     C     *IN61         IFEQ      *ON
     C                   ADD       RPAMT         RC(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    IR(X)                                  61
     C   60              MOVE      RPRECD        IR(X)
     C   60              Z-ADD     RPAMT         RC(X)
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     keyWARPL      READE     PRP00                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *                                                       BFSTAT = 'O'
      *
      *   If in update mode, reload the remaining reserve array in
      *     case there has been a change due to a pmt or reserve chg
      *     by another user.
      *
      *
      *  LOOP THRU CODES
      *
     C                   DO        10            I
     C     IR(I)         IFNE      *BLANKS
      *
     C                   Z-ADD     1             X
     C     IR(I)         LOOKUP    RT#(X)                                 60
     C     *IN60         IFEQ      '1'
     C                   ADD       RC(I)         RR(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RT#(X)                                 60
     C   60              MOVE      IR(I)         RT#(X)
     C   60              ADD       RC(I)         RR(X)
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDDO
      *
     C                   MOVEL     scClmsts      CMstat
     C                   MOVE      Q1USER        CMuser
     C                   MOVE      Q1PGM         CMpgm
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        intDti
     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     intDto        XXDT              7 0
     C                   Z-ADD     XXDT          CMDT
     C                   Z-ADD     DSTIME        CMTIME
      *
      * IF THE CLAIM WAS OPEN AND HAD BEEN CLOSED ,UPDATE CLAIM CLOSED DATE
      *
     C                   IF        CMstat = 'C' and strClmSts = 'O'
     C                   EVAL      CMCLDT = XXDT
     C*** Begin Add *** 08/16/17 ********************************************
      /free
       //
       // Delete recdurring diaries on closed claims
       Chain qcKey pcm0a;
       If %Found(sdcmla);
          DoU %Eof(sdcmla);
             //*** Begin Chg *** 02/27/18 ***********************
             //********** If CMdcDT = 0;
             //**********    Chain CMrmcd pct01;
             If C2dcDT = 0;
                Chain C2rmcd pct01;
             //*** End   Chg *** 02/27/18 ***********************
                If %Found(stctl1) And CTrdPl <> 0;
                   //*** Begin Chg *** 02/27/18 ***********************
                   //********** CMstat = 'I';
                   C2stat = 'I';
                   //*** End   Chg *** 02/27/18 ***********************
                EndIf;
             EndIf;
             Update pcm0a;
             ReadE qcKey pcm0a;
          EndDo;
       EndIf;
      /end-free
     C*** End   Add *** 08/16/17 ********************************************
     C                   ENDIF
      *
     C                   IF        CMstat = 'O' and strClmSts = 'C'
     C                   EVAL      CMCLDT = 0
     C                   ENDIF
      *
      *** Beg Add ***** 11/25/19 *************************************
      * Write Status change history.
      * This code is moved here to write Claim History record before
      * writing Claim record so when Claim trigger fires and calls SPITRIGGER
      * program, Claim status change History record will be available.
     C                   If        scClmSts <> strClmSts
     C                   MOVE      CMCO#         CHCO#
     C                   MOVE      CMFND         CHFND
     C                   Z-ADD     CMFYR         CHFYR
     C                   MOVE      CMCASE        CHCASE
      *
     C                   Z-ADD     XXDT          CHEFDT
     C
     C                   IF        LISTrsn = *on
     C                   MOVEL     SCEXCD        CHEXPL
     C                   ELSE
     C                   MOVEL     SCEXPL        CHEXPL
     C                   ENDIF
     C
     C                   MOVEL     scClmSts      CHOCST
     C                   MOVE      *BLANK        CHSTAT
     C                   MOVE      Q1USER        CHUSER
     C                   MOVE      Q1PGM         CHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        intDti
     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     intDto        XXDT              7 0
     C                   Z-ADD     XXDT          CHDT
     C                   Z-ADD     DSTIME        CHTIME
     C                   Write     Pch05
     C                   Endif
      *** End Add ***** 11/25/19 *************************************
     C*UPDATR (W0070R12)
      *
      *
     C
     C                   Update    dbrcdf
     C                   Unlock    Wmcmp
     C
     C                   FEOD      WMCMP
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        intDti
     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     intDto        XXDT              7 0
      *
     C*  Auto-DARTS
     C*    Check to see if status updated; if not ADD, call WA700R.
     C     CMSTAT        IFNE      STRCLMSTS
     C     chgMode       ANDEQ     *ON
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      'CLMCHG04'    @@DKEY           10
     C                   PARM      *BLANKS       @@COMM          256
     C                   ENDIF
      *
     C                   MOVEL     'LARGERSV'    strKy            10
     C                   Z-ADD     *ZEROS        #RSVAM           11 2
     C     keyWMADL      CHAIN     PAD00                              60
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      *OFF          *IN93
     C                   Z-ADD     1             R                 3 0
     C     ADSTAT        IFNE      'I'
     C     ADRECD        LOOKUP    IR(R)                                  93
     C     *IN93         IFEQ      *ON
     C                   Z-ADD     ADAMT         #RSVAM
     C                   ENDIF
     C                   ENDIF
     C     keyWMADL      READE     PAD00                                  60
     C                   ENDDO
      // *** Beg Chg ********* 11/25/13 ***********************************
      /Free
       lostTimeHold=CMltFl;
       If strClmSts <> CMstat;
          AutoLetters(inRRNClm:##rtcd:lostTimeHold:strClmSts);
       EndIf;
      /End-Free
      // *** End Chg ********* 11/25/13 ***********************************
      *
      ** MOVE FIELDS FOR RESERVE HISTORY
      *
     C                   MOVE      *BLANKS       XXRH              1
     C     WKRC01        IFNE      *ZERO
     C     WKRC02        ORNE      *ZERO
     C     WKRC03        ORNE      *ZERO
     C     WKRC04        ORNE      *ZERO
     C     WKRC05        ORNE      *ZERO
     C     WKRC06        ORNE      *ZERO
     C     WKRC07        ORNE      *ZERO
     C     WKRC08        ORNE      *ZERO
     C     WKRC09        ORNE      *ZERO
     C     WKRC10        ORNE      *ZERO
      *
     C                   MOVE      '1'           XXRH
     C                   MOVE      CMCO#         RHCO#
     C                   MOVE      CMFND         RHFND
     C                   Z-ADD     CMFYR         RHFYR
     C                   MOVE      CMCASE        RHCASE
     C                   CLEAR                   RRT
     C                   CLEAR                   RSU
     C
     C                   DO        10            X                 3 0
     C
     C     RC(X)         IFNE      *ZERO
     C                   Z-ADD     1             Y                 3 0
     C     IR(X)         LOOKUP    RT#(Y)                                 99
     C                   MOVE      IR(X)         RRT(Y)
     C                   Z-ADD     RC(X)         RSU(Y)
     C
     C* Auto-DARTS: Now check large reserve change amount
     C*
     C     #RSVAM        IFNE      *ZERO
     C     RC(X)         ANDGE     #RSVAM
     C
     C*AUDRT
     C*
     C                   CALL      'WA700R'
     C                   PARM      CMCO#         @@CO#             3 0
     C                   PARM      CMFND         @@FND             3 0
     C                   PARM      CMFYR         @@FYR             3 0
     C                   PARM      CMCASE        @@CASE            7 0
     C                   PARM      'LARGERSV'    @@DKEY           10
     C                   PARM      *BLANKS       @@COMM          256
     C*
     C                   ENDIF
      *
     C
     C                   ENDIF
     C
     C                   ENDDO
      *
     C                   MOVE      CMCO#         ARHCO#
     C                   MOVE      CMFND         ARHFND
     C                   Z-ADD     CMFYR         ARHFYR
     C                   MOVE      CMCASE        ARHCASE
     C                   Z-ADD     XXDT          ARHEFDT
     C                   MOVEL     rsnchg        ARHEXPL
     C                   MOVE      *BLANKS       ARHSTAT
     C                   MOVE      *BLANKS       ARHCLST
     C                   MOVE      Q1USER        ARHUSER
     C                   MOVE      Q1PGM         ARHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        ARHTIME
      **
     C                   Z-ADD     DSDATE        intDti
     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     intDto        ARHDT

     C                   MOVEL     intDto        ARHIDT
      *
     C                   MOVE      Q1USER        ARHIUSR
     C                   MOVE      Q1PGM         ARHIPGM

     C                   Z-ADD     DSTIME        ARHITIM
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       ARHinitRsv = GetInitRsvFlag(ARHco#:ARHfnd:ARHfyr:ARHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
      *
     C                   END
      *
      *  MOVE FIELDS FOR TRANSFER LOST TIME - MED ONLY CHANGE HISTORY
      *
     C                   MOVE      *BLANKS       XXTH              1
     C     STRLTFL       IFNE      CMLTFL
     C                   MOVE      '1'           XXTH
     C                   MOVE      CMCO#         THCO#
     C                   MOVE      CMFND         THFND
     C                   Z-ADD     CMFYR         THFYR
     C                   MOVE      CMCASE        THCASE
      *
     C                   MOVE      STRLTFL       THFROM
     C                   MOVE      CMLTFL        THTO
     C                   Z-ADD     XXDT          THTRDT
     C                   MOVE      *BLANK        THSTAT
     C                   MOVE      Q1USER        THUSER
     C                   MOVE      Q1PGM         THPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        intDti
     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     intDto        XXDT              7 0
     C                   Z-ADD     XXDT          THDT
     C                   Z-ADD     DSTIME        THTIME
     C                   END
      *
      *** Beg Del *** 11/25/19 ***********************
      *** Below code is moved above before writing WMCMP record.
      *  MOVE FIELDS FOR STATUS CHANGE HISTORY
      *
      ***                MOVE      *BLANKS       XXCH              1
      ***  scClmSts      IFNE      strClmSts
      ***                MOVE      '1'           XXCH
      ***                MOVE      CMCO#         CHCO#
      ***                MOVE      CMFND         CHFND
      ***                Z-ADD     CMFYR         CHFYR
      ***                MOVE      CMCASE        CHCASE
      *
      ***                Z-ADD     XXDT          CHEFDT
      ***
      ***                IF        LISTrsn = *on
      ***                MOVEL     SCEXCD        CHEXPL
      ***                ELSE
      ***                MOVEL     SCEXPL        CHEXPL
      ***                ENDIF
      ***
      ***                MOVEL     scClmSts      CHOCST
      ***                MOVE      *BLANK        CHSTAT
      ***                MOVE      Q1USER        CHUSER
      ***                MOVE      Q1PGM         CHPGM
      ***                TIME                    DSDTTM
      ***                Z-ADD     DSDATE        intDti
      ***                CALL      'SRCVTDAT'    DTCYMD1
      ***                MOVEL     intDto        XXDT              7 0
      ***                Z-ADD     XXDT          CHDT
      ***                Z-ADD     DSTIME        CHTIME
      ***                END
      *** End Del *** 11/25/19 ***********************
      *
      *  Update the Oustanding Reserve bucket in the Emp Detail file.
      *
     C                   MOVE      CMEMP#        XXEMP#            9 0
     C                   MOVE      CMDIV         XXDIV             3 0
     C                   Z-ADD     CMFYR         XXFYR             3 0
     C     keyWDELL      CHAIN     PEL00                              42
     C     *IN42         IFEQ      '0'
     C                   XFOOT     RC            XXCHG             9 2
     C                   ADD       XXCHG         ELOUTS
     C                   UPDATE    PEL00
     C                   Unlock    WDELL
     C                   END
      *
      *  IF CLAIM IS REOPENED, WRITE A DARTS DIARY REMARK.
      *
     C     strClmSts     IFEQ      'C'
     C     scClmSts      ANDEQ     'O'
     C     CXROPN        ANDNE     *BLANK
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     CMco#         QCCO#
     C                   Z-ADD     CMFnd         QCFND
     C                   Z-ADD     CMfyr         QCFYR
     C                   Z-ADD     CMcase        QCCASE
     C
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVE      *BLANKS       Q#COMM
     C                   MOVE      *BLANKS       Q#DCID
     C                   MOVE      *BLANKS       Q#PNAM
     C                   MOVE      *BLANKS       Q#USER
     C                   MOVE      *BLANKS       Q#PGM
     C                   MOVE      *BLANKS       Q#FOLD
     C                   MOVE      *BLANKS       Q#IPGM
     C                   MOVE      *BLANKS       Q#RMCD
     C                   MOVE(P)   *BLANKS       Q#CODE            2
     C     CMCLEX        CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  UPUSR         Q#USER           10
     C                   TIME                    DSDTTM
     C                   Z-ADD     CXRODY        SRDAYS
      **
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDATE
     C                   MOVE      *BLANKS       intErr
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE            8 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM                    intErr            1
     C                   MOVE(P)   SRDATE        Q#CDAT            8
     C                   MOVE      CXROPN        strRmCd
     C     keySTCTL1     CHAIN     PCT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  RMK(1)        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL(P)  'Y'           Q#DIFL            1
     C                   MOVE(P)   CXROPN        Q#RMCD            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL(P)  CTDEPG        Q#DCID           12
     C                   MOVEL(P)  'W0070R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVEL(P)  QCKEY         Q#FOLD           80
     C                   ELSE
     C                   MOVE      *BLANKS       Q#FOLD
     C                   ENDIF
     C                   MOVE(P)   *BLANKS       Q#APID            2
     C                   MOVE(P)   *BLANKS       Q#IDOC           12
     C                   MOVE(P)   *BLANKS       Q#IQUE           10
     C                   MOVE(P)   *BLANK        Q#IMGF            1
     C                   MOVE(P)   *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVE(P)   *BLANK        Q#QPTY            1
     C                   MOVE(P)   *BLANKS       Q#DCTP            5
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C     Q#DCID        IFNE      *BLANKS
     C                   MOVE      'Y'           Q#FL01            1
     C                   ELSE
     C                   MOVE      *BLANKS       Q#FL01
     C                   ENDIF
     C     CTDPRG        IFGT      *ZERO
     C                   MOVE      'Y'           Q#FL02            1
     C                   ELSE
     C                   MOVE      *BLANKS       Q#FL02
     C                   ENDIF
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
      *
     C                   ENDIF

     C                   ENDIF
      **
     C                   ENDIF
      *                                                      BFSTAT = 'C'
      *
      ** WRITE HISTORY RESERVE
      **
     C     XXRH          IFEQ      '1'
     C                   WRITE     PRH02
     C                   CLEAR                   RRT
     C                   CLEAR                   RSU
     C                   CLEAR                   RC
     C                   CLEAR                   IS
     C                   END
      *
      ** WRITE TRANSFER HISTORY
      *
     C     XXTH          IFEQ      '1'
     C                   WRITE     PTH00
     C                   END
      *
      ** WRITE STATUS CHANGE HISTORY
      *
      *** Beg Del *** 11/25/19 ***********************
     C**   XXCH          IFEQ      '1'
     C**                 WRITE     PCH05
     C**                 END
      *** End Del *** 11/25/19 ***********************
      *
      *
     C     strCLTID      IFEQ      '025'
     C*  When a claim is closed, delete the claim index suspense record
     C*    if it exists.
     C
     C     strclmSts     IFEQ      'O'
     C     scClmSts      ANDEQ     'C'
     C
     C     keyWDPAL      CHAIN     PID01                              60
     C  N60              DELETE    PID01
     C
     C                   ENDIF
     C
     C*  Wnen a claim is reopened, create a claim index suspense record
     C*    if the claim index DARTS remark code is not blank, and the
     C*    suspense record does not already exist.
     C*
     C     strClmSts     IFEQ      'C'
     C     scClmSts      ANDEQ     'O'
     C
     C     keyWDPAL      CHAIN(N)  PID01                              60
     C     *IN60         IFEQ      *ON
     C     CXAIC         ANDNE     *BLANKS
     C
     C                   Z-ADD     CMCO#         IDCO#
     C                   Z-ADD     CMFND         IDFND
     C                   Z-ADD     CMFYR         IDFYR
     C                   Z-ADD     CMCASE        IDCASE
     C                   MOVE      *BLANKS       IDCLEX
     C
     C                   MOVE      DSDATE        SRDATE
     C                   Z-ADD     CXAICD        SRDAYS
     C                   MOVE      *BLANKS       intErr
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    intErr

     C                   MOVE      SRDATE        intDti
     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     intDto        IDASDT

     C                   MOVEL     Q1PGM         IDPGM
     C                   MOVEL     Q1USER        IDUSER
     C                   MOVE      DSDATE        intDti

     C                   CALL      'SRCVTDAT'    DTCYMD1
     C                   MOVEL     intDto        IDDT
     C                   Z-ADD     DSTIME        IDTIME
     C                   MOVEL     Q1USER        IDIUSR
     C                   Z-ADD     IDDT          IDIDT

     C                   WRITE     PID01
     C                   ENDIF

E02  C                   ENDIF
      *
      *
     C                   Endif
      *
      /free
       //******** beg add ******* 12/01/14 *************************************
       // If the claim has been closed, delete pending resere worksheet history
       // if any exists.
       If strclmSts = 'O' and scClmSts = 'C';
          Chain (CMco#:CMfnd:CMfyr:CMcase) prw00;
          If %found and CRWstat = 'P';
             Delete prw00;
          Endif;
       Endif;
       Unlock WDRWL;
       //******** end add ******* 12/01/14 *************************************
      /end-free

     C                   Clear                   IR
     C                   Clear                   IS
     C                   Clear                   PRH02
     C                   Clear                   PTH00
     C                   Clear                   PCH05
     C                   Clear                   PEL00
      *
     C     ENDP          TAG
      *
     P MoveScDb        E
       //===================================================================
       // ProcessFilter - Process the filter selection.
       //===================================================================
     P ProcessFilter   B
     D ProcessFilter   PI
      /Free

       isValid = *off;
       If (intFromDate = 0 or (intFromDate <= PAfrdt and intToDate >= PAtodt))
        and (scFltrFE   = 0   or (scFltrFE   = PAfein))
        and (scFltrChk# = 0   or (scFltrChk# = PAchk#))
        and (scFltrEOMB = 0   or (scFltrEOMB = PAeob#))
        and (scFltrGrpN = ' ' or (scFltrGrpN = PApgtp))
        and (scFltrIntR = ' ' or (scFltrIntR = PArqst))
        and (scFltrTrTy = ' ' or (scFltrTrTy = PAtype))
        //*** Beg Chg ***** 09/18/13 ****************************************
        // and (scFltrPayC = ' ' or (%scan(PAtrcd:scFltrPayC) <> 0))
        // and (scFltrPVal = ' ' or (%scan(PAbr2 :scFltrPVal) <> 0))
        and (scFltrPayC=' ' or (PAtrcd<>' ' and %scan(PAtrcd:scFltrPayC) <> 0))
        and (scFltrPVal=' ' or (PAbr2 <>' ' and %scan(PAbr2 :scFltrPVal) <> 0))
        //****** Beg Chg ***** 05/06/13 *********************************
        // and (scFltrRCod = ' ' or (%scan(PArecd:scFltrRCod) <> 0));
        // and (scFltrRCod = ' ' or (%scan(%trim(PArecd):scFltrRCod) <> 0));
        and (scFltrRCod = ' ' or (PArecd <> ' ' and
        %scan(%trim(PArecd):scFltrRCod) <> 0));
        //****** End Chg ***** 05/06/13 *********************************
        //**** End Chg ***** 09/18/13 ****************************************
          isValid = *on;
       EndIf;

      /End-Free
     P ProcessFilter   E
       //===================================================================
       // ProcDupEx - Process duplicate payment ecxlusion.
       //===================================================================
     P ProcDupEx       B
     D ProcDupEx       PI
      /Free

       Chain scRRN# ppa0p;
       If %found(WDPAP);
          PAuser = q1user;
          PApgm  = q1pgm;
          PAdt   = Dateto7(SystemDate());
          PAtime = Timeto6(SystemTime());
          PAdpcu = q1user;
          If PAdpcd = *zero;
             PAdpcd = PAdt;
          Else;
             PAdpcd = *zero;
          Endif;
          Update ppa0p;

       Endif;

      /End-Free
     P ProcDupEx       E
       //===================================================================
       // ProcChgRec  - Process the change record.
       //===================================================================
     P ProcChgRec      B
     D ProcChgRec      PI
      /Free

       first = *off;

       scRowNum=1;
       ReadC sflPayee;
       DoW Not %Eof;
          // Change Duplicate Payment Exclusion
          If chgPmtExcl = *On;
             scRowNum=rrnPayee;
             ChangePmtExcl(scRRN#);
          EndIf;
          //*** Beg Add *** 05/09/23 ***************************************
          // Edit Link To The External Payment Detail Popup Procedure
          If payEdtLnk = *On;
             PayDetailPrc(scRRN#);
          EndIf;
          //*** End Add *** 05/09/23 ***************************************
          ReadC sflPayee;
       EndDo;

       ReadC sflPenReq;
       DoW Not %Eof;
          //*** Begin Add *** 04/04/23 **************************
             svrrnReq = rrnReq;        //Save Rrn value for edited record
          //*** End   Add *** 04/04/23 **************************
          // Edit Payment request
          If scReqEdit = *on;
             EdtRequest();
             LodReqInfo();
          Endif;

          // Pending/release process
          If scPenRls = *on;
             ProcPenRls();
          //*** Begin Add *** 10/01/14 **************************
             ClrSubFilesRsv();
             LodRsvInfo();
          //*** End   Add *** 10/01/14 **************************
             LodReqInfo();
             LodHstInfo();
          //*** Begin Add *** 06/27/18 **************************
             LodStsInfo();
          //*** End   Add *** 06/27/18 **************************
          Endif;

          // Delete pending check request process
          If scReqDel = *on;
             ProcReqDel();
          //*** Begin Add *** 10/01/14 **************************
             ClrSubFilesRsv();
             LodRsvInfo();
          //*** End   Add *** 10/01/14 **************************
             LodReqInfo();
          //*** Begin Add *** 10/01/14 **************************
             LodHstInfo();
          //*** End   Add *** 10/01/14 **************************
          //*** Begin Add *** 06/27/18 **************************
             LodStsInfo();
          //*** End   Add *** 06/27/18 **************************
          Endif;
          ReadC sflPenReq;
       EndDo;

       If BtnAddEft = *On;
          BtnAddEft = *Off;
          ##cmrrn = GetClaimRRN(CMco#:CMfnd:CMfyr:CMcase);
          ##eftMod = 'A';
          ##eftTyp = 'C';
          ##eftRrn = 0;
          eftAdd(##eftMod:##eftTyp:CMfnd:##eftRrn:##cmrrn);
          first = *on;
       Endif;

       // read subfile change record
       Readc SflEftInfo;
       Dow not %eof;
          Select;
          When scEdtEftLc = *On;
             scEdtEftLc =*Off;
             ##eftMod = 'E';
             ##eftTyp = scEftType;
             ##cmrrn  = 0;
             ##eftRrn = scLocRrn;
             eftAdd(##eftMod:##eftTyp:CMfnd:##eftRrn:##cmrrn);
             first = *on;
          When scEftBnkLc = *On;
             scEftBnkLc =*Off;
             ##eftRrn = scLocRrn;
             EFTBANKINFO(##eftRrn);
             first = *on;
          When scRqPNtLc = *On;
             scRqPNtLc =*Off;
             ##eftRrn = scLocRrn;
             If DateTo8(scLocPNtDt) = 0;
                ReqPreNote(##eftRrn);
             Else;
                ClrPreNote(##eftRrn);
             Endif;
             first = *on;
          EndSl;
          Readc SflEftInfo;
       Enddo;
       If first = *on;
          TabEftInfo();
       Endif;

      /End-Free
     P ProcChgRec      E
       //===================================================================
       // Process EFT information data
       //===================================================================
     P ProcEftInfo     B
     D ProcEftInfo     PI
     C     keyWMPDL      Klist
     C                   Kfld                    EFfein
     C                   KFLD                    EFmod
      *
      /free

       Exsr ClearEftSub;
       Exsr LoadEftInfo;

       //===================================================================
       // ClearEftSub - Clear Subfile for EFT information
       //===================================================================
       BegSr ClearEftSub;

          rrnEFTInfo = 0;
          clrEFTInfo = *On;
          dspEFTInfo = *Off;
          Write rcdEftInfo;
          clrEFTInfo = *Off;
          dspEFTInfo = *On;

       EndSr;
       //===================================================================
       // LoadEftInfo - Load Subfile for EFT information
       //===================================================================
       BegSr LoadEftInfo;

          Setll keyWMEFL3 pef03;
          Reade keyWMEFL3 pef03;
          Dow not %eof(WMEFL3);
             Clear SflEftInfo;
             // ***** beg chg ****** 05/15/13 **************
             // scLocEffDt = dtMDYY(EFefdt);
             // scLocExpDt = dtMDYY(EFexdt);
             // scLocPNtDt = dtMDYY(EFpdt);
             // scLocAppDt = dtMDYY(EFadt);
             // scLocFTrDt = dtMDYY(EFftdt);
             // scLocLTrDt = dtMDYY(EFltdt);
             scLocEfDat = CvtToDate(EFefdt);
             scLocExpDt = CvtToDate(EFexdt);
             scLocPNtDt = CvtToDate(EFpdt);
             scLocAppDt = CvtToDate(EFadt);
             scLocFTrDt = CvtToDate(EFftdt);
             scLocLTrDt = CvtToDate(EFltdt);
             // ***** end chg ****** 05/15/13 **************
             If EFpdt = 0;
                scNoteLnk = 'Request Pre-note';
             Else;
                scNoteLnk = 'Clear Pre-note';
             Endif;
             //If inMode = 'D';
             //   shwEftLnk = *off;
             //Else;
             //   shwEftLnk = *on;
             //Endif;
             shwEftLnk = svShwEftLnk;

             // Get Payee name and address
             scLocPName = %trim(EFpnam);
             If EFtype = 'D';
                Chain keyWDACL# pac00;
                scEftAddr = ACadr1;
             Endif;
             scEftType  = EFtype;
             rrnEFTInfo = rrnEFTInfo + 1;
             If rrnEFTInfo = 1;
                scSelRow = 'A';
             Else;
                Clear scSelRow;
             Endif;
             scLocRrn = efrrn;

             Write SflEftInfo;
             Reade keyWMEFL3 pef03;
          Enddo;
          rrnEFTInfo = 1;

       EndSr;
      /end-free
     P ProcEftInfo     E

       //===================================================================
       // ProcRlsRqs  - Process the pending/release request
       //===================================================================
     P ProcRlsRqs      B
     D ProcRlsRqs      PI
      /Free

       isValid = *off;
       clear scErrMsg;

       // Validate Payment status code.
       If RPretp = 'M';
          Chain(n) keyWDELL pel00;
          If ELexmf = 'Y';
             scErrMsg = 'Medical information cannot be entered for the +
                         associated Employer/Policy year.';
             Dou scErrCan = *on;
                Exfmt rcdChkErr;
             Enddo;
             isValid = *on;
          Endif;
       Endif;

       If isValid = *off;
          ##co#     =  RPco#;
          ##fnd#    =  RPfnd;
          ##fyrc#   =  RPfyr;
          ##casec#  =  RPcase;
          ##trcd    =  RPtrcd;
          ##eob     =  RPeob#;
          ##amt     =  scReqAmt;
          ##retp    =  RPretp;
          ##recd    =  RPrecd;
          ##rc      =  *blanks;
          ##fein    =  RPfein;
          ##mod     =  RPmod;

          //****** beg add ******* 01/24/14 *********************************
          ValidPmtEdt(##co#:##fnd#:##fyrc#:##casec#:##rc);
          If ##rc = ' ';
          //****** end add ******* 01/24/14 *********************************

          Validrls(##co#:##fnd#:##fyrc#:##casec#:##trcd:##fein:##mod:##rc:
           ##rcd:##rtp:##typ);
          //****** beg add ******* 01/24/14 *********************************
          Endif;
          //****** end add ******* 01/24/14 *********************************

          clear scErrMsg;
          If ##rc = '08';
             scErrMsg = 'Claim/DARTS Control record is not found.';
          Endif;

          //****** beg add ******* 01/24/14 *********************************
          If ##rc = '01' and scErrMsg = ' ';
             scErrMsg = 'Group Master record not found.';
          Endif;

          If ##rc = '02' and scErrMsg = ' ';
             scErrMsg = 'Group Detail record not found.';
          Endif;

          If ##rc = '03' and scErrMsg = ' ';
             scErrMsg = 'Group Expiration Date is less than today''s''' +
              ' date.';
          Endif;

          If ##rc = '04' and scErrMsg = ' ';
             scErrMsg = 'Group Master Auxiliary record was not found.';
          Endif;

          If ##rc = '05' and scErrMsg = ' ';
             scErrMsg = 'Check Processing is disallowed for this group';
          Endif;

          If ##rc = '06' and scErrMsg = ' ';
             scErrMsg = 'Claim Master record is not found.';
          Endif;

          If ##rc = '07' and scErrMsg = ' ';
             scErrMsg = 'This user profile is not defined to the system. +
              Contact the security officer for more information.';
          Endif;

          If ##rc = '09' and scErrMsg = ' ';
          //****** beg chg ******* 07/11/25 *********************************
          //*** scErrMsg = 'Invalid table value found in field.'; ***//
                scErrMsg = 'You do not have the security level required to +
                           process a Denied Claim.';
          //****** end chg ******* 07/11/25 *********************************
          Endif;
          //****** end add ******* 01/24/14 *********************************

          //****** beg chg ******* 07/11/25 *********************************
          //*** If (##rc = '10' or ##rc = '11') and scErrMsg = ' '; ***//
          //*** scErrMsg = 'Invalid table value found in field.'; ***/
          If ##rc = '10' and scErrMsg = ' ';
                scErrMsg = 'Invalid payment code found in field.';
          //****** end chg ******* 07/11/25 *********************************
          Endif;

          //****** beg add ******* 07/11/25 *********************************
             If ##rc = '11' and scErrMsg = ' ';
                scErrMsg = 'Invalid reserve code found in field.';
             Endif;
          //****** end add ******* 07/11/25 *********************************

          If ##rc = '13' and scErrMsg = ' ';
             scErrMsg = 'Lost time flag must equal "I" for this claim.';
          Endif;

          If ##rc = '14' and scErrMsg = ' ';
             scErrMsg = 'Controverted Claim Reserve type is not defined in +
                         Claim/DARTS Control record.';
          Endif;
          //*** Beg Del ***** 02/06/25 ****************************
          //***If ##rc = '15' and scErrMsg = ' ';
          //***   scErrMsg = 'You must enter a FEIN/MOD.';
          //***Endif;
          //*** End Del ***** 02/06/25 ****************************

          If ##rc = '16' and scErrMsg = ' ';
             scErrMsg = 'Invalid payee.';
          Endif;

          If ##rc = '17' and scErrMsg = ' ';
             scErrMsg = 'The social security number entered is not active.';
          Endif;

          If ##rc = '41' and scErrMsg = ' ';
             scErrMsg='Payments are not allowed on "Record Only" type claims.';
          Endif;

          If scErrMsg <> ' ';
             Dou scErrCan = *on;
                Exfmt rcdChkErr;
             Enddo;
             isValid = *on;
          Endif;

          If ##rc = ' ' and isvalid = *off;
             ##amt   = scReqAmt;
             ##amnt  = scReqAmt;
             ##rcns  = %rem(RPeobs:1000);
             ##seq   = RPseq#;
             ##rc    = *blanks;

             ValidAmt(##co#:##fnd#:##fyrc#:##casec#:##eob:##seq:##amt:##recd:
              ##retp:##rc:##amnt:##rcns:##trcd);

             If ##rc = '09';
                scErrMsg = 'You do not have the security level required to +
                           process a Denied Claim.';
             Endif;

             If ##rc = '18' and scErrMsg = ' ';
                scErrMsg = 'This user profile is not defined to the system. +
                            Contact the security officer for more information.';
             Endif;

             If ##rc = '19' and scErrMsg = ' ';
                scErrMsg = 'Reserve change exceeds reserve authority limit.';
             Endif;

             If ##rc = '20' and scErrMsg = ' ';
                scErrMsg = 'Remaining reserve may not be made negative.';
             Endif;

             If ##rc = '21' and scErrMsg = ' ';
                scErrMsg = 'Reserve Cushion for Reserve Code & Claim Type is +
                            not valid.';
             Endif;

             If ##rc = '22' and scErrMsg = ' ';
                scErrMsg = 'Remaining reserve will not be large enough to cover'
                           + ' existing requests for payments.';

             Endif;

             If ##rc = '23' and scErrMsg = ' ';
                scErrMsg = 'Amount exceeds the reserve cushion amount set up +
                            for the payment type.';
             Endif;

             If ##rc = '43' and scErrMsg = ' ';
                scErrMsg='Amount exceeds payment authority limit.';
             Endif;

             //****** beg add ******* 01/24/14 *********************************
             If ##rc = '12' and scErrMsg = ' ';
                scErrMsg = 'Reserve code in payment code is invalid for claim.';
             Endif;

             If ##rc = '50' and scErrMsg = ' ';
                scErrMsg = 'Negative total amount being paid on the reserve.';
             Endif;
             //****** end add ******* 01/24/14 *********************************

             If scErrMsg <> ' ';
                Dou scErrCan = *on;
                   Exfmt rcdChkErr;
                Enddo;
                isValid = *on;
             Endif;
          Endif;

          // Check for Duplicate or Possible Duplicate Payment
          If isValid = *off;
          //*** Begin Del *** 06/12/17 ******************************
          //*##from*=*RPfrdt;
          //*##to***=*Rptodt;
          //*clear*##strDtl1;
          //*clear*##strdtl2;
          //**********
          //*getClmInfo(##co#:##fnd#:##fyrc#:##casec#:##strDtl1
          //**:##strDtl2);
          //*##fndnam*= %Subst(##strDtl1:12:51);
          //*##clmnam*= %Subst(##strDtl2:39:22);
          //*##prvnam*= scPayName;
          //*##rtcd*=* *Blanks;
          //*##fein*=* RPfein;
          //*##mod**=* RPmod;
          //*##inerr=* *off;
          //*** End   Del *** 06/12/17 ******************************

             //*** Begin Del *** 01/19/18 ***********************
             //If scRlsFst = *off;
             //*** End   Del *** 01/19/18 ***********************
             //*** Begin Chg *** 06/12/17 ******************************
             //**********
             //*ValidDup(##co#:##fnd#:##fyrc#:##casec#:##seq:##from:##to:
             //*##amt:##recd:##fein:##fndnam:##clmnam:##prvnam:##inerr:
             //*##retp:##mod:##rtcd);
                // isValid = ##inerr;
       // Begin Add ***** 11/02/16 *************************************
             //*If*##rtcd = '2' or ##rtcd = '3';
             //****isSecErr = *on;
             //*Else;****
             //****isSecErr = *off;
             //*Endif;***
       // End Add ***** 11/02/16 *************************************
             If ChkDupPmtAuth(RPfrDt:RPtoDt) = *On;
                isSecErr = *On;
             Else;
                isSecErr = *Off;
             EndIf;
             //*** End   Chg *** 06/12/17 ******************************

                // Access Claims/DARTS Control file for duplicate payment
                Chain keyWMCXL# pcx00;
                //*** Beg Chg *** 12/10/25 *********************************
                //*** If %found(WMCXL) and ##rtcd = '1';
                If %found(WMCXL) and (isSecErr = *on or scRlsFst = *on);
                //*** End Chg *** 12/10/25 *********************************

                   // Security level and Type check
                   ##UTyp = UPtype;
                   ##UlVl = %editc(UPlvl:'X');
                   ##CTyp = CXdpt1;
                   ##ClVl = %editc(CXdplv:'X');
                   ##Exec = '1';
                   ##Res  = *on;
                   ##ctp2 = CXdpt2;
                   ##ctp3 = CXdpt3;
                   ##ctp4 = CXdpt4;
                   ##ctp5 = CXdpt5;
                   ##ctp6 = ' ';

                   GetSecAut(##utyp:##ulvl:##ctyp:##clvl:##exec:##res:##ctp2:
                             ##ctp3:##ctp4:##ctp5:##ctp6);

                   If ##res = *on;
                       scErrMsg = 'User does not have authority to release a +
                                  duplicate payment.';
                       isValid = *on;
                       Dou scErrCan = *on;
                          Exfmt rcdChkErr;
                       Enddo;
                   Endif;
                Endif;

                If isValid = *on;
                   scRlsFst = *on;
                Endif;
             //*** Begin Del *** 01/19/18 ***********************
             //***Endif;
             //*** End   Del *** 01/19/18 ***********************
          Endif;
       Endif;

      /End-Free
     P ProcRlsRqs      E
       //===================================================================
       // ProcPenRls  - Process the pending/release request
       //===================================================================
     P ProcPenRls      B
     D ProcPenRls      PI

     d*** Begin Add *** 06/12/17 ***************************************
     d error           s               n   Inz(*Off)
     d*** End   Add *** 06/12/17 ***************************************

      /Free

       scPenRls   = *off;
       scReqDel   = *off;
       isValid    = *off;
       // Begin Del ***** 11/02/16 *************************************
       // Begin Add ***** 09/13/16 *************************************
       // ##inerr = *off;
       // End Add *****   09/13/16 *************************************
       // End Del ***** 11/02/16 *************************************

       clear scErrMsg;
       Chain scReqRrn# prp0p;

       If RPstat = 'D';
          scErrMsg = 'Invalid option - you are attempting to Release a Deleted +
                      Request for Payment.';
       Endif;

       //*** Begin Chg *** 10/01/14 *******************************************
       //*** If RPstat = 'Y';
       If RPstat = ' ' and scReqPnRls = 'Release';
       //*** End   Chg *** 10/01/14 *******************************************
          scErrMsg = 'Invalid option - you are attempting to Release a Released'
                     + ' payment.';
       Endif;

       //*** Begin Add *** 10/01/14 *******************************************
       If RPstat = 'P' and scReqPnRls = 'Pend';
          scErrMsg = 'Invalid option - you are attempting to Pend a Pended'
                     + ' payment.';
       Endif;
       //*** End   Add *** 10/01/14 *******************************************
       //*** Begin Add *** 06/12/17 *******************************************
       // Let's check authority to release a payment if dupliate
       //*If scReqPnRls = 'Release' And RPstat = 'P';
       //*   intsvAmt = scPayAmt;
       //*   intsvFdt = dtCYMD(scSerFrDt);
       //*   intsvTdt = dtCYMD(scSerToDt);
       //*   If ChkDupPmtAuth(RPfrDt:RPtoDt) = *On;
       //*      scErrMsg = ' ';
       //*      error    = *On;
       //*   EndIf;
       //*EndIf;
       //*** End   Add *** 06/12/17 *******************************************

       //*** Begin Add *** 07/30/20 *******************************************
       If not ChkRlsPayAuth(RPco#);
          scErrMsg = 'Insufficient authority to release the payment.';
       Endif;
       //*** End   Add *** 07/30/20 *******************************************

       // display error window if error found
       If scErrMsg <> ' ';
          Dou scErrCan = *on;
             Exfmt rcdChkErr;
          Enddo;
       Endif;

       //*** Begin Add *** 10/01/14 *******************************************
       //
       // If error was encountered then skip the following processing
       //*** Begin Add *** 10/01/14 *******************************************
       //*If*scErrMsg = ' ';
          If scErrMsg = ' ' And error = *Off;
       //*** End   Add *** 10/01/14 *******************************************
       //*** End   Add *** 10/01/14 *******************************************
          If RPstat = 'P';
             ProcRlsRqs();
          Endif;

          // Begin change 09/13/16 ************************************
          // Check for error from ProcRlsRqs
          //If (RPstat = ' ' or RPstat = 'P') and isValid = *off;
          // Begin Chg ***** 11/02/16 *************************************
          // If (RPstat = ' ' or RPstat = 'P') and isValid = *off;
              ##inerr = *off;
          If (RPstat = ' ' or RPstat = 'P') and isValid = *off and
              isSecErr = *off;
          // End Chg ***** 11/02/16 *************************************
          // End change 09/13/16 ************************************
             // Access System Control File to obtain default "Pend" & "Release"
             // codes.
             Chain keyWMCXL# pcx00;
             If %found(WMCXL);

                If scReqPnRls = 'Pend';
                   ##key  = CXpmts;
                   RPsts  = CXpmts;
                   RPstat = 'P';
                   //*** Begin Del *** 02/20/18 ***************************
                   //*** RPsent = *Blanks;
                   //*** RPdseq = *Zeros;
                   //*** End   Del *** 02/20/18 ***************************
                Endif;
                If scReqPnRls = 'Release';
                   ##key  = CXpsc2;
                   RPsts  = CXpsc2;
                   RPstat = ' ';
                   RPrusr   = q1user;
                   RPrpgm   = q1pgm;
                   RPrdt    = Dateto7(SystemDate());
                   RPrtm    = Timeto6(SystemTime());

                   // Execute DARTS Complete Diary logic
                   WriteDarts();
                Endif;

                // Access Payment Status File to obtain SORT Sequence value
                Chain keyWTABL pps00;
                If %found(WTPSL) and PSstat <> 'I';
                   RPsort = PSsrt;
                Endif;

                RPuser  = q1user;
                RPpgm   = q1pgm;
                RPdt    = Dateto7(SystemDate());
                RPtime  = Timeto6(SystemTime());
                update prp0p;

                // Check Claim Master record to determine if CLOSED
                If CMstat = 'C';

                   // For "Pend", Subtract Amount from Claim Remaining Reserves
                   // For "Release", Allocate Amount to Claim Remaining Reserves
                   If scReqPnRls = 'Pend';
                      ##type = 'D';
                   Else;
                      ##type = 'R';
                   Endif;

                   ##co#     =  RPco#;
                   ##fnd#    =  RPfnd;
                   ##fyrc#   =  RPfyr;
                   ##casec#  =  RPcase;
                   ##recd    =  RPrecd;
                   ##amt     =  RPamt;

                   Unlock WDELL;
                   ValidClm(##co#:##fnd#:##fyrc#:##casec#:##recd:##type:##amt);

                Endif;

                // If releasing a payment and an EOMB is attached and the reques
                // being released is not the bill review or medical review payme
                // then release all requests with the corresponding EOB#.
             //*** Beg Chg *** 10/21/19 ***********************************
             //*** If scReqPnRls = 'Release' and RPeob# > 0;
                If scReqPnRls = 'Release' and RPeob# > 0 and strCltId <> '152';
             //*** End Chg *** 10/21/19 ***********************************
                   clear ##SvEob#;
                   ##SvEob# = RPeob#;
                   Chain (RPco#:RPfein:RPmod) pnr00;
                   If not %found(WMNRL) or NRgrty = 'O';

                      // Read all requested payments with the corresponding EOB#
                      Setll keyWARPLN prp0n;
                      Reade keyWARPLN prp0n;
                      Dow not %eof(WARPLN);

                         If RPstat = 'P';
                            strSvStat = RPstat;
                            // Update Request for Payment record with "Released"
                            RPstat = ' ';
                            RPsts  = CXpsc2;

                            Chain keyWTABL pps00;
                            If %found(WTPSL) and PSstat <> 'I';
                               RPsort = PSsrt;
                            Endif;

                            RPuser  = q1user;
                            RPpgm   = q1pgm;
                            RPdt    = Dateto7(SystemDate());
                            RPtime  = Timeto6(SystemTime());
                            RPrusr   = q1user;
                            RPrpgm   = q1pgm;
                            RPrdt    = Dateto7(SystemDate());
                            RPrtm    = Timeto6(SystemTime());

                            update prp0n;

                            // Execute DARTS Complete Diary logic
                            WriteDarts();

                            //  If claim is closed, increase reserves in claim m
                            //  and reserve history.
                            If CMstat = 'C';
                               ##co#     =  RPco#;
                               ##fnd#    =  RPfnd;
                               ##fyrc#   =  RPfyr;
                               ##casec#  =  RPcase;
                               ##recd    =  RPrecd;
                               ##amt     =  RPamt;
                               ##type    =  'R';

                               Unlock WDELL;
                               ValidClm(##co#:##fnd#:##fyrc#:##casec#:##recd:
                                ##type:##amt);
                            Endif;
          //*** Begin Add *** 06/22/13 ***********************************
                         Else;
                            Unlock WARPLN;
          //*** End   Add *** 06/22/13 ***********************************
                         Endif;
                         Reade keyWARPLN prp0n;
                      Enddo;

                   Endif;
                Endif;
             Endif;
       // Begin Add ***** 11/02/16 *************************************
             isSecErr = *off;
       // End Add ***** 11/02/16 *************************************
          Endif;
       //*** Begin Add *** 10/01/14 *******************************************
       Endif;
       //*** End   Add *** 10/01/14 *******************************************
       unlock WARPLN;
       unlock WARPP;

      /End-Free
     P ProcPenRls      E
       //===================================================================
       // ProcReqDel  - Process the pending check request deletion
       //===================================================================
     P ProcReqDel      B
     D ProcReqDel      PI
      /Free

       scReqDel   = *off;
       scPenRls   = *off;
       clear scErrMsg;
       isValid = *off;
       //*** Begin Chg *** 12/11/14 ***********************************
       //*************** Chain scReqRrn# prp0p;
       Chain(N) scReqRrn# prp0p;
       //*** End   Chg *** 12/11/14 ***********************************

       // Invalid Option
       If RPstat = 'D';
          scErrMsg = 'Invalid option - Request for Payment has already been +
                      deleted.';
       Endif;

       If scErrMsg = ' ';
          // Access Claims/DARTS Control file
          Chain keyWMCXL# pcx00;
          If not %found(WMCXL);
             scErrMsg = 'Claim/DARTS Control record is not found.';
          Else;
             If RPeob# > 0;
                // 2) User security level and type Delete EOB check
                ##UTyp = UPtype;
                ##UlVl = %editc(UPlvl:'X');
                ##CTyp = CXckdt;
                ##ClVl = %editc(CXckdl:'X');
                ##Exec = '1';
                ##Res  = *on;
                ##ctp2 = CXckd2;
                ##ctp3 = CXckd3;
                ##ctp4 = CXckd4;
                ##ctp5 = CXckd5;
                ##ctp6 = ' ';

                GetSecAut(##utyp:##ulvl:##ctyp:##clvl:##exec:##res:##ctp2:
                          ##ctp3:##ctp4:##ctp5:##ctp6);

                If ##res = *on;
                    scErrMsg = 'Security is not vaild to delete payment with +
                               a attached eob.';
                Endif;
             EndIf;
          EndIf;
       Endif;

       If scErrMsg <> ' ';
          Dou scErrCan = *on;
             Exfmt rcdChkErr;
          Enddo;
       Endif;

       // If RPSTAT = 'Y', do not process this change.
       If RPstat <> 'Y' and scErrMsg = ' ';
          ##co#     =  RPco#;
          ##fnd#    =  RPfnd;
          ##fyrc#   =  RPfyr;
          ##casec#  =  RPcase;
          //*** Begin  Chg *** 02/19/15 **********************************
          //*** strSts    =  'DE';
          //*** strSort   =  'X';
          //*** strSvStat =  RPstat;

          // Find "Deleted" Payment Status code
          //*** Setll ##co# pps00;
          //*** Reade ##co# pps00;
          //*** Dow not %eof(WTPSL) or (PStype = 'D' and PSstat <> 'I');
          //***    Reade ##co# pps00;
          //*** Enddo;

          //*** If PStype  = 'D';
          //***    strSts  = PSkey;
          //***    strSort = PSsrt;
          //*** Endif;
          GetDelStatus();
          //*** End    Chg *** 02/19/15 **********************************
          // Update Request for Payment record with "Deleted" status
          //*** Begin Add *** 12/11/14 ***********************************
          Chain scReqRrn# prp0p;
          //*** End   Add *** 12/11/14 ***********************************
          RPsts  = strSts;
          RPsort = strSort;
          strSvStat = RPstat;
          RPstat = 'D';
          RPuser = q1user;
          RPpgm  = q1pgm;
          RPdt    = Dateto7(SystemDate());
          RPtime  = Timeto6(SystemTime());
          update prp0p;

          // continue delete process.
          ContDelProc();
       Endif;
       //*** Begin Del *** 12/11/14 ***********************************
       //************** Unlock WARPP;
       //*** End   Del *** 12/11/14 ***********************************

      /End-Free
     P ProcReqDel      E
      *================================================================
      *Selchgclm    - User selected to change the claim status
      *================================================================
     P Selchgclm       B
     D Selchgclm       PI
      /Free
        //Dow  ChgClmSts  = *on ;
          If   ClmSts = 'Reopen';
             scClmsts = 'O';
          else;
             scClmsts = 'C';
          Endif;
       clear IR;
       clear IS;
       // Validate();
       ValidateSts();

       If   chgMode and isErrorS <> *on ;
          MoveScDb();
          TabReserveInfo();
       Else;
          If CMstat = 'O';
             Clmsts = 'Close  ';
             scClmsts = 'O';
          Else;
             Clmsts = 'Reopen ';
             scClmsts = 'C';
          Endif;
       Endif;

        //Enddo;
      /End-free
     P Selchgclm       E
      *================================================================
      *SRCHG        - Reserve Calculation  -Copied for Grid
      *================================================================
     P SrChg           B
     D SrChg           PI


     C     PAKEY         KLIST
     C                   KFLD                    CMco#
     C                   KFLD                    CMfnd
     C                   KFLD                    CMfyr
     C                   KFLD                    CMcase

     C                   Z-ADD     0             decCcHg
     C                   Z-ADD     0             decMcHg
     C                   Z-ADD     0             decLcHg
     C                   Z-ADD     0             decEcHg

     C                   Move      *blanks       ##KEY
     C*
     C                   DO        10            Z                 2 0
     C*
     C     RT(Z)         IFNE      *BLANKS
     C                   MOVE      *BLANKS       strKy
     C                   MOVEL     RT(Z)         ##KEY
     C     keyWTABL      CHAIN     PRT00                              60
     C*
     C     RTTYPE        IFEQ      'C'
     C                   ADD       SU(Z)         decCcHg
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   ADD       SU(Z)         decMcHg
     C                   END
     C     RTTYPE        IFEQ      'L'
     C                   ADD       SU(Z)         decLcHg
     C                   END
     C     RTTYPE        IFEQ      'E'
     C                   ADD       SU(Z)         decEcHg
     C                   END
     C*
     C                   END
     C                   END
     C*
     P SrChg           E
      *================================================================
      *SUMCLP  - summarize claims payment data
      *================================================================
     P SumCLP          B
     D SumCLP          PI

     C                   MOVEL     CMCO#         QQCO#
     C                   MOVEL     CMFND         QQFND
     C                   MOVEL     CMFYR         QQFYR
     C                   MOVEL     CMCASE        QQCASE
     C                   MOVEA     *BLANKS       CDP
     C                   MOVEA     *ZEROS        WK2
     C                   MOVEA     *ZEROS        DA2
     C                   MOVEA     *ZEROS        AM2

     C                   CALL      'WA576R5'
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM      *BLANKS       QQTRCD            2
     C                   PARM      *BLANKS       QQTYPE            1
     C                   PARM      *BLANKS       QQRECD            2
     C                   PARM      *BLANKS       QQRETP            1
     C                   PARM                    CDP
     C                   PARM                    WK2
     C                   PARM                    DA2
     C                   PARM      *ZEROS        decTamt
     C                   PARM      *ZEROS        decTrsv
     C                   PARM                    AM2
     P SumCLP          E
       //===================================================================
       // UpdateRecord - Update DataBase
       //===================================================================
     P UpdateRecord    B
     D UpdateRecord    PI
      /Free

       Chain inRRNClm dbrcdf;
       If %Found;
          MoveScDb();
          Update  dbrcdf;
       EndIf;

      /End-Free
     P UpdateRecord    E
       //===================================================================
       // SaveEdtReq - Save the changes for edit request
       //===================================================================
     P SaveEdtReq      B
     D SaveEdtReq      PI
      /Free

       RPrmds  =  scRemark;
       RPrmk1  =  scRemark1;
       RPrmk2  =  scRemark2;
       RPrmk3  =  scRemark3;
       RPtrdt  =  dtCYMD(scPtDtEdt);
       RPfrdt  =  dtCYMD(scSerFrDt);
       RPtodt  =  dtCYMD(scSerToDt);
       RPrcvd  =  dtCYMD(scRcvdDt);
       RPsts   =  scPayStsCd;
       RPbr2   =  scPayVal1;
       RPbr7   =  scPayVal2;
       RPdesc  =  scPayDesc;
       RPamt   =  scPayAmt;
       RPltcd  =  scPdTime;
       RPwks   =  scWeekEdt;
       RPdays  =  scDayEdt;
       RPeftC  = scEftEdt;
       RPpgTp  = scGrpChk;
       RPfnal  = scFinCmp;
       RPuser  = q1user;
       RPpgm   = q1pgm;
       RPdt    = Dateto7(SystemDate());
       RPtime  = Timeto6(SystemTime());
       //*** Beg Add *** 10/25/12 *********************************************
          RPinv# = scInv#;
       //*** End Add *** 10/25/12 *********************************************

       // If Payment Status changed, assign RP fields with "New" info
       IF strOldSts <> strNewSts;
          RPsts  = strNewSts;
       Endif;

       If strOldStat <> strNewStat;
          RPstat = strNewStat;
          //*** Begin Del *** 02/20/18 ***************************
          //*** If RPstat = 'P';
          // Assign DARTS Diary Sent flag and Sequence #
          //***    RPsent = *Blanks;
          //***    RPdseq = *Zeros;
          //*** Endif;
          //*** End   Del *** 02/20/18 ***************************
          If RPstat = ' ';
             RPrdt  = RPDT;
             RPrtm  = RPtime;
             RPrusr = q1user;
             RPrpgm = q1pgm;
          Endif;
       Endif;

       //  Call API to determine the Bank "Code".
       ##appl= 'WC';
       //Load DATA values
       QXtrdt = RPtrdt;
       QXfrdt = RPfrdt;
       QXtodt = RPtodt;
       QXtrcd = RPtrcd;
       QXrecd = RPrecd;
       QXpv1  = RPbr2 ;
       QXpv2  = RPbr7 ;
       QXseq# = RPcmsq;
       QXeob# = %Char(RPeob#);
       QXfein = RPfein;
       QXmod  = RPmod ;
       QXdsq# = RPdsq#;
       ##dta = data;

       ##cktp = GetBankCode(##co:##fnd:##fyrc:##casec:##appl:
                 ##dta: ##rtcd: ##eftc);

       If strCLTID <> '144' or
        (RPcktp <> 'CPAMED' and RPcktp <> 'CPARX');
          RPcktp = ##cktp;
       Endif;

       // If Payment Status changed, determine specific change and process
       // accordingly.
       If strOldStat <> strNewStat;
          Select;

          //*** Begin Del *** 02/19/15 ***********************************
          //*** When strNewStat = 'D';
          //***   strSvStat = strOldStat;
          //***   ContDelProc();
          //*** End   Del *** 02/19/15 ***********************************

          When strNewStat = 'P' and strOldStat = ' ' and CMstat = 'C';
             ##type = 'D';
             Unlock WDELL;
             ValidClm(##co#:##fnd#:##fyrc#:##casec#:##recd:##type:##amt);

          When (strNewStat = ' ' and strOldStat = 'P' and CMstat = 'C') or
           (strNewStat = ' ' and strOldStat = 'D' and CMstat = 'C');
             ##type = 'R';
             Unlock WDELL;
             ValidClm(##co#:##fnd#:##fyrc#:##casec#:##recd:##type:##amt);
             If strOldStat = 'P';
                // Execute DARTS Complete Diary logic
                WriteDarts();
             Endif;

          Endsl;

       Endif;

       // When changing the amount of a released pmt on a closed claim,
       //  create a reserve history record for the difference.

       // Begin change *** 12/08/16 *****************************************
       //   If intsvAmt <> scpayAmt and strNewStat = ' ' and strOldStat = ' ' an
       //   CMstat = 'C';
       If intsvAmt <> *zero and intsvAmt <> scpayAmt and
          strNewStat = ' ' and strOldStat = ' ' and CMstat = 'C';
       // End   change *** 12/08/16 *****************************************

          ##type = 'R';
          clear ##diff;
          ##diff = RPamt - intsvAmt;
          Unlock WDELL;
          ValidClm(##co#:##fnd#:##fyrc#:##casec#:##recd:##type:##diff);
       Endif;

      /End-Free
     P SaveEdtReq      E
       //===================================================================
       // TabPaymentinfo  - Processing for Payment Tab
       //===================================================================
     P TabPaymentInfo  B
     D TabPaymentInfo  PI
      /Free

       // Load the subfile grid for Payment Info Tab
       MoveDbSc();

       // Load the Detail records & Chart for Payment Info Tab

      /End-Free
     P TabPaymentInfo  E
       //===================================================================
       // TabReserveinfo  - Processing for Reserve Tab
       //===================================================================
     P TabReserveInfo  B
     D TabReserveInfo  PI
      /Free

       ClrSubFilesRsv();
       LodHstInfo();
       LodRsvInfo();
       LodStsInfo();

       //******** beg add ****** 07/11/14 ********** 111WBL ****************
       If strCltId = '111';
          Exsr LodExcRec;
       Endif;

       //===================================================================
       // Load Total Excess Recovery
       //===================================================================
       Begsr LodExcRec;

       scTtExcRec = 0;

       Setll (CMco#:CMfnd:CMfyr:CMcase) pre00;
       Reade (CMco#:CMfnd:CMfyr:CMcase) pre00;
       Dow not %eof;
          If RErccd = '700' or RErccd = '725' or RErccd = '750' or
           RErccd = '775';
             scTtExcRec = scTtExcRec + REamt;
          Endif;
          Reade (CMco#:CMfnd:CMfyr:CMcase) pre00;
       Enddo;

       Endsr;
       //******** end add ****** 07/11/14 **********************************

      /End-Free
     P TabReserveInfo  E
       //===================================================================
       // TabRequestinfo  - Processing for Request for payment Tab
       //===================================================================
     P TabRequestInfo  B
     D TabRequestInfo  PI
      /Free

       LodReqInfo();

      /End-Free
     P TabRequestInfo  E
       //===================================================================
       // TabDedHistory  - Processing for Deductible History Tab
       //===================================================================
     P TabDedHistory   B
     D TabDedHistory   PI
      /Free

       LodDedInfo();

      /End-Free
     P TabDedHistory   E
       //===================================================================
       // TabEftInfo - Processing for EFT Information Tab
       //===================================================================
     P TabEftInfo      B
     D TabEftInfo      PI
      /Free

       ProcEftInfo();

      /End-Free
     P TabEftInfo      E
       //===================================================================
       // ValidEdtReq - Validate edit request screen selection
       //===================================================================
     P ValidEdtReq     B
     D ValidEdtReq     PI
     D*** Begin Add *** 08/20/14 **************************************
     D newStsType      s              1    inz
     D oldStsType      s              1    inz
     D*** End   Add *** 08/20/14 **************************************
     D*** Begin Add *** 06/12/17 **************************************
     D noAuth          s               n   Inz
     D*** End   Add *** 06/12/17 **************************************
      /Free

       isValid    = *off;
       //*** Beg Add 10/25/12 *************************************************
       isValid1   = *Zero;
       //*** End Add 10/25/12 *************************************************
       erSerFrDt  = *off;
       erSerToDt  = *off;
       erPayStsCd = *off;
       erPtDtEdt  = *off;
       erPayAmt   = *off;
       erPdTime   = *off;
       erDayEdt   = *off;
       erRcvdDt   = *off;
       erPayVal1  = *off;
       erPayVal2  = *off;
       erGrpChk   = *off;
       ##co#      = RPco#;
       ##fnd#     = RPfnd;
       ##fyrc#    = RPfyr;
       ##casec#   = RPcase;

       // Check if updated since screen displayed
       flgRcdUpdated = RcdUpdatedErr('WARPP':scReqRrn#:'RPdt':'RPtime':
        'RPuser':SVdt:SVtime);
       Select;
       When flgRcdUpdated = '0'; // No error
       When flgRcdUpdated = '1'; // Error
          isValid = *on;
          Return;
       When flgRcdUpdated = '2'; // Error with refresh
       Endsl;

       // Validate Payment status code.
       ##key = scPayStsCd;
       Chain keyWTABL pps00;
       RPsort = PSsrt;
       If RPretp = 'M';
          Chain(n) keyWDELL pel00;
          If ELexmf = 'Y' and PStype = 'R';
             MSPayStsCd = 'WC40022';
             erPayStsCd = *on;
             isValid = *on;
          Endif;
       Endif;

       // If Payment status code is changed
       If scPayStsCd <> RPsts;
          strOldStat = RPstat;
          strOldSts  = RPsts;
          strNewSts  = scPayStsCd;
          If PStype  = 'R';
             strNewStat = ' ';
          Else;
             strNewStat = PStype;
          Endif;
       //*** Begin Add *** 08/20/14 **************************************
          // Get the Old Status Code Type
          ##Key=RPsts;
          Chain keyWTABL pps00;
          If %Found(WTABL);
             oldStsType=PStype;
          EndIf;

          // Get the New Status Code Type
          ##Key=scPayStsCd;
          Chain keyWTABL pps00;
          If %Found(WTABL);
             newStsType=PStype;
          EndIf;

          // If request is currently pended and the status code is changed to a
          // released type then perform the following edits

          If oldStsType='P' and newStsType='R';

             ##co#     =  RPco#;
             ##fnd#    =  RPfnd;
             ##fyrc#   =  RPfyr;
             ##casec#  =  RPcase;
             ##trcd    =  RPtrcd;
             ##eob     =  RPeob#;
             ##amt     =  scReqAmt;
             ##retp    =  RPretp;
             ##recd    =  RPrecd;
             ##rc      =  *blanks;
             ##fein    =  RPfein;
             ##mod     =  RPmod;

             ValidPmtEdt(##co#:##fnd#:##fyrc#:##casec#:##rc);
             If ##rc = ' ';

                Validrls(##co#:##fnd#:##fyrc#:##casec#:##trcd:##fein:##mod:
                 ##rc:##rcd:##rtp:##typ);
             Endif;

             clear scErrMsg;
             If ##rc = '08';
                scErrMsg = 'Claim/DARTS Control record is not found.';
             Endif;

             If ##rc = '01' and scErrMsg = ' ';
                scErrMsg = 'Group Master record not found.';
             Endif;

             If ##rc = '02' and scErrMsg = ' ';
                scErrMsg = 'Group Detail record not found.';
             Endif;

             If ##rc = '03' and scErrMsg = ' ';
                scErrMsg = 'Group Expiration Date is less than today''s''' +
                 ' date.';
             Endif;

             If ##rc = '04' and scErrMsg = ' ';
                scErrMsg = 'Group Master Auxiliary record was not found.';
             Endif;

             If ##rc = '05' and scErrMsg = ' ';
                scErrMsg = 'Check Processing is disallowed for this group';
             Endif;

             If ##rc = '06' and scErrMsg = ' ';
                scErrMsg = 'Claim Master record is not found.';
             Endif;

             If ##rc = '07' and scErrMsg = ' ';
                scErrMsg = 'This user profile is not defined to the system. +
                 Contact the security officer for more information.';
             Endif;

             If ##rc = '09' and scErrMsg = ' ';
          //****** beg chg ******* 07/11/25 *********************************
          //*** scErrMsg = 'Invalid table value found in field.'; ***//
                scErrMsg = 'You do not have the security level required to +
                           process a Denied Claim.';
          //****** end chg ******* 07/11/25 *********************************
             Endif;

             If ##rc = '10' and scErrMsg = ' ';
                scErrMsg = 'Invalid payment code found in field.';
             Endif;

             If ##rc = '11' and scErrMsg = ' ';
                scErrMsg = 'Invalid reserve code found in field.';
             Endif;

             If ##rc = '13' and scErrMsg = ' ';
                scErrMsg = 'Lost time flag must equal "I" for this claim.';
             Endif;

             If ##rc = '14' and scErrMsg = ' ';
                scErrMsg = 'Controverted Claim Reserve type is not defined in +
                            Claim/DARTS Control record.';
             Endif;

             //*** Beg Del ***** 02/06/25 ****************************
             //***If ##rc = '15' and scErrMsg = ' ';
             //***   scErrMsg = 'You must enter a FEIN/MOD.';
             //***Endif;
             //*** End Del ***** 02/06/25 ****************************

             If ##rc = '16' and scErrMsg = ' ';
                scErrMsg = 'Invalid payee.';
             Endif;

             If ##rc = '17' and scErrMsg = ' ';
                scErrMsg = 'The social security number entered is not active.';
             Endif;

             If ##rc = '41' and scErrMsg = ' ';
                scErrMsg='Payments are not allowed on ' +
                           '"Record Only" type claims.';
             Endif;

             If scErrMsg <> ' ';
                Dou scErrCan = *on;
                   Exfmt rcdChkErr;
                Enddo;
                isValid = *on;
             Endif;
          EndIf;
       //*** End   Add *** 08/20/14 **************************************
       Else;
          strOldStat = RPstat;
          strOldSts  = RPsts;
          strNewSts  = RPsts;
          strNewStat = RPstat;
       Endif;

       // If Payment Status is "Delete", skip edits.
       If strNewStat <> 'D';

          // Validate Group check flag
          scGrpChk = Upper(scGrpChk);

          // BUILD ##EKEY AND PASS IT TO WA014R TO CHECK TO ENSURE THAT PAYEE
          // EFT IS SET UP FOR EFT PAYMENT.

          ##ekey = ' ';
          If RPfein = 0;
             intwk10 = %dec(((%editc(RPfyr:'X'))+ (%editc(RPcase:'X'))):10:0);
             If RPdsq# = 0;
                ##Ekey = %editc(intwk10:'X') + 'C';
             Else;
                ##Ekey = %editc(RPdsq#:'X')+ %editc(intwk10:'X') + 'D';
             Endif;
          Else;
             ##Ekey = %editc(RPmod:'X')+ %editc(RPfein:'X') + 'P';
          Endif;

        // *** Beg Chg *** 04/16/15 *******************************************

        //***** ##grp     =  scGrpChk; ******

          If scGrpChk = *blanks;
             ##grp = 'N';
          Else;
            If scGrpChk = 'G';
              ##grp = 'Y';
            Endif;
          Endif;

        // *** End Chg *** 04/16/15 *******************************************

          ##ptdt    =  scPtDtEdt;
          ##rcvd    =  scRcvdDt;
          ##frdt    =  scSerFrDt;
          ##todt    =  scSerToDt;
          ##retp    =  RPretp;
          ##recd    =  RPrecd;
          ##br2     =  scPayVal1;
          ##br7     =  scPayVal2;
          ##sts     =  scPayStsCd;
          ##ltcd    =  scPdTime;
          ##day     =  scDayEdt;
          ##rc      =  *blanks;
          ##eob     =  RPeob#;
          ##amt     =  scPayAmt;
          ##rcn     =  RPeobs;

          ValidEdt(##co#:##fnd#:##fyrc#:##casec#:##grp:##ptdt:##rcvd:##frdt:
           ##todt:##retp:##recd:##br2:##br7:##sts:##ltcd:##day:##rc:##eob:
           ##amt:##rcn:##Ekey);

          If not(strCLTID = '114' and RPfnd = 440 and ##rc = '33') and
            ##rc <> ' ' and  ##rc <> '40' and isValid = *off;

             // Posting date
             If ##rc = '25';
                msPtDtEdt = 'WCR0500';
                erPtDtEdt = *on;
                isValid = *on;
             Endif;

             // Amount
             If ##rc = '39';
                msPayAmt = 'WCR0502';
                erpayAmt = *on;
                isValid  = *on;
             Endif;

             // Paid on time
             If ##rc = '38';
                msPdTime = 'WCR0513';
                erPdTime = *on;
                isValid  = *on;
             Endif;

             // Service from date
             If ##rc = '26';
                msSerFrDt = 'WCR0500';
                erSerFrDt = *on;
                isValid   = *on;
             Endif;

             If ##rc = '27' and isvalid = *off;
                msSerFrDt = 'WCR0572';
                erSerFrDt = *on;
                isValid   = *on;
             Endif;

             If ##rc = '29' and isvalid = *off;
                msSerFrDt = 'WCR0529';
                erSerFrDt = *on;
                isValid   = *on;
             Endif;

             If ##rc = '42' and isvalid = *off;
                msSerFrDt = 'WC30061';
                erSerFrDt = *on;
                isValid   = *on;
             Endif;

             // Service to date
             If ##rc = '28';
                msSerToDt = 'WCR0500';
                erSerToDt = *on;
                isValid   = *on;
             Endif;

             If ##rc = '29' and isValid = *off;
                msSerToDt = 'WCR0529';
                erSerToDt = *on;
                isValid   = *on;
             Endif;

             // Days
             If ##rc = '37';
                msDayEdt = 'WCR0541';
                erDayEdt = *on;
                isValid  = *on;
             Endif;

             // Receiving date
             If ##rc = '34';
                msRcvdDt = 'WCR0500';
                erRcvdDt = *on;
                isValid  = *on;
             Endif;
             If ##rc = '35' and isValid = *off;
                msRcvdDt = 'WCR0572';
                erRcvdDt = *on;
                isValid  = *on;
             Endif;

             // Payment status code
             If ##rc = '36';
                msPayStsCd = 'WCR1047';
                erPayStsCd = *on;
                isValid  = *on;
             Endif;

             // Payment value1
             If ##rc = '30';
                msPayVal1 = 'WCR2493';
                erPayVal1 = *on;
                isValid  = *on;
             Endif;
             If ##rc = '31';
                msPayVal1 = 'WC30046';
                erPayVal1 = *on;
                isValid  = *on;
             Endif;

             // Payment value2
             If ##rc = '32';
                msPayVal2 = 'WCR0513';
                erPayVal2 = *on;
                isValid  = *on;
             Endif;
             If ##rc = '33';
                msPayVal1 = 'WC30047';
                erPayVal1 = *on;
                isValid  = *on;
             Endif;
          Endif;

        // *** Beg Add *** 10/25/12 *******************************************
          If scInv# <> RPinv# and dspInv2# <> '0';
             validInv(RPco# : RPfnd : RPfyr : RPcase : RPseq# : RPfein :
              scInv# : xxFkey : isValid1);
             If isValid1 = *On;
                isValid = *On;
             Endif;
          Endif;
        // *** End Add *** 10/25/12 *******************************************
          // Payment Transaction Description can not be Blank.
          If scPayDesc = ' ';
             msPayDesc = 'WCR0503';
             erPaydesc = *on;
             isValid = *on;
          Endif;

          // Only Check Remaining Reserves If Payment is "Released".

          If strNewStat = ' ';

             // Payment status code
             If CMltfl = 'R';
                msPayStsCd = 'WC30319';
                erPayStsCd = *on;
                isValid  = *on;
             Endif;

             ##amnt  = scPayAmt;
             ##rcns  = %rem(RPeobs:1000);
             ##trcd  = RPtrcd;
             ##seq   = RPseq#;
             ##rc    = *blanks;

             ValidAmt(##co#:##fnd#:##fyrc#:##casec#:##eob:##seq:##amt:##recd:
              ##retp:##rc:##amnt:##rcns:##trcd);

             // Amount
             If ##rc = '19';
                msPayAmt = 'WCR0574';
                erpayAmt = *on;
                isValid  = *on;
             Endif;
             If ##rc = '20' and isvalid = *off;
                msPayAmt = 'WCR0552';
                erpayAmt = *on;
                isValid  = *on;
             Endif;
             If ##rc = '21' and isvalid = *off;
                msPayAmt = 'WC30044';
                erpayAmt = *on;
                isValid  = *on;
             Endif;
             If ##rc = '22' and isvalid = *off;
                msPayAmt = 'WCR0635';
                erpayAmt = *on;
                isValid  = *on;
             Endif;
             If ##rc = '23' and isvalid = *off;
                msPayAmt = 'WCR1065';
                erpayAmt = *on;
                isValid  = *on;
             Endif;
             If ##rc = '39' and isvalid = *off;
                msPayAmt = 'WCR0502';
                erpayAmt = *on;
                isValid  = *on;
             Endif;

             // Begin Add *** 10/22/15 ****************************************
             If ##rc = '43' and isvalid = *off;
                msPayAmt = 'WCR1104';
                erpayAmt = *on;
                isValid  = *on;
             Endif;
             // End   Add *** 10/22/15 ****************************************
             //****** beg add ******* 06/27/18 *********************************
             If ##rc = '12' and isValid = *Off;
                msPayStsCd = 'WCR0519';
                erPayStsCd = *On;
                isValid    = *On;
             Endif;

             If ##rc = '50' and isValid = *Off;
                msPayStsCd = 'WCR0552';
                erPayStsCd = *On;
                isValid    = *On;
             Endif;
             //****** end add ******* 06/27/18 *********************************


             // Check for Duplicate or Possible Duplicate Payment

             If isValid = *off and (strsvSts <> scPayStsCd) and
              scPayStsCd = 'PY' and isRels = *off;
             //*** Begin Chg *** 06/12/17 ******************************
             //*##from*=*dtCYMD(scSerFrDt);
             //*##to***=*dtCYMD(scSerToDt);
             //*isRels*=**on;
             //*clear*intsvAmt;
             //*clear*intsvFdt;
             //*clear*intsvTdt;
             //*intsvAmt*= scPayAmt;
             //*intsvFdt*= ##from;
             //*intsvTdt*= ##to;
             //*clear*##strDtl1;
             //*clear*##strdtl2;

             //*getClmInfo(##co#:##fnd#:##fyrc#:##casec#:##strDtl1
             //**:##strDtl2);
             //*##fndnam*= %Subst(##strDtl1:12:51);
             //*##clmnam*= %Subst(##strDtl2:39:22);
             //*##prvnam*= scPayName;
             //*##rtcd*=* *Blanks;
             //*##fein*=* RPfein;
             //*##mod**=* RPmod;
             //*##inerr=* *off;
             //**********
             //*ValidDup(##co#:##fnd#:##fyrc#:##casec#:##seq:##from:##to:
             //**##amt:##recd:##fein:##fndnam:##clmnam:##prvnam:##inerr:
             //**##retp:##mod:##rtcd);
             //*isValid*= ##inerr;
                isRels = *on;
                intsvAmt = scPayAmt;
                intsvFdt = dtCYMD(scSerFrDt);
                intsvTdt = dtCYMD(scSerToDt);
                noAuth = ChkDupPmtAuth(intSvFdt:intSvTdt);
             //*** End   Chg *** 06/12/17 ******************************
             Endif;
          Endif;
       Endif;

      /End-Free
     P ValidEdtReq     E
       //===================================================================
       // ValidEFT - Validate for EFT processing
       //===================================================================
     P ValidEFT        B
     D ValidEFT        PI
      *** beg add ***** 07/23/15 *************************************
     d ##outRRN        s              9
     d ##EobType       s              9
      *** end add ***** 07/23/15 *************************************
      /Free

       strSvEftc = RPeftc;
       clear strmode;

       Select;
       When RPeftc = 'E';
          //*** Begin Del *** 10/22/20 ****************************
          //*** RPeftc = ' ' ;
          //*** End   Del *** 10/22/20 ****************************

       //*** Begin Chg *** 10/22/20 ****************************
       //*** When RPeftc = ' ';
       When RPeftc = ' ' Or RPeftC = 'V';
       //*** End   Chg *** 10/22/20 ****************************
          //*** Begin Chg *** 10/22/20 ************************************
          // Populated passing parameters
          //*** ##appl= 'WC';
          //Load DATA values
          //*** QXtrdt = RPtrdt;
          //*** QXfrdt = RPfrdt;
          //*** QXtodt = RPtodt;
          //*** QXtrcd = RPtrcd;
          //*** QXrecd = RPrecd;
          //*** QXpv1  = RPbr2 ;
          //*** QXpv2  = RPbr7 ;
          //*** QXseq# = RPcmsq;
          //*** QXeob# = %Char(RPeob#);
          //*** QXfein = RPfein;
          //*** QXmod  = RPmod ;
          //*** QXdsq# = RPdsq#;
          //*** ##dta = data;

          //*** ##cktp = GetBankCode(##co:##fnd:##fyrc:##casec:##appl:
          //***     ##dta: ##rtcd: ##eftc);

          //*** If ##eftc <> 'E';
          #2Call = ' ';
          #2HdCy = ' ';
          #2ChkQ = ' ';
          #2Ftyp = ' ';
          #2EfDt = ' ';
          #2CpyQ = ' ';
          #2CkTp = ' ';
          #2Coid = ' ';
          #2RtCd = ' ';
          #2eftC = 'E';
          outAcct = getBankAcc(#2Co#:#2Fnd:#2Appl:#2date:#2Call:#2HdCy:
                     #2ChkQ:#2Ftyp:#2EfDt:#2CpyQ:#2CkTp:#2EfTc:
                     #2Coid:#2RtCd);
          If outAcct = ' ';
             strmode = 'X';
          Else;

             //Determine type of Record
             If RPfein = 0;
                strKey =  %editC(RPfyr:'X');
                strKey =  %Trim(strKey) + %editC(RPcase:'X');

                //*** Begin Chg *** 06/24/21 *******************************
                //*** Begin Add *** 10/22/20 *************************
                //***ACseq# = 0;
                //*** End   Add *** 10/22/20 *************************
                //***Chain keyWDACL pac00;
                //***If ACseq# =  0;
                //***   ##ptyp = 'C';
                //***Else;
                //***   strKey = %Trim(strKey) + %editC(ACseq#:'X');
                //***   ##ptyp = 'D';
                //***Endif;
                If RPdSq# = 0;
                   ##pTyp = 'C';
                Else;
                   strKey = %Trim(strKey) + %editC(RPdSq#:'X');
                   ##pTyp = 'D';
                EndIf;
                //*** End   Chg *** 06/24/21 *******************************
             Else;
                strKey = %editc(RPfein:'X');
                strKey = %Trim(strKey) + %editC(RPmod:'X');
                ##ptyp = 'P';
             Endif;

             ##ekey1 = strKey;
             ##date = %Char(RPdt);
             ##pre = *Off;

             //Call the Procedure to retrieve EFT status.
          //*** beg chg ***** 07/23/15 ***************************
          //***  If GetEFTStatus(##co:##ptyp:##fnd:##ekey1:##date:
          //***   ##pre);
             If GetEFTStatus(##co:##ptyp:##fnd:##ekey1:##date:
              ##pre:##OutRRN:##EobType:##cktp);
          //*** end chg ***** 07/23/15 ***************************
                //*** Begin Del *** 10/22/20 *************************
                //*** RPEftc = 'E';
                //*** End   Del *** 10/22/20 *************************
             Else;
                strmode = 'X' ;
             EndIf;
          Endif;
       Endsl;

      /End-Free
     P ValidEFT        E
       //===================================================================
       // ValidDspDel - Validate for displaying delete link
       //===================================================================
     P ValidDspDel     B
     D ValidDspDel     PI
      /Free

       // Access Claims/DARTS Control file
       Chain keyWMCXL# pcx00;
       If %found(WMCXL);
          // User security level and type DELETE check
          ##UTyp = UPtype;
          ##UlVl = %editc(UPlvl:'X');
          ##CTyp = CXdppt;
          ##ClVl = %editc(CXdltd:'X');
          ##Exec = '1';
          ##Res  = *on;
          ##ctp2 = CXdpp2;
          ##ctp3 = CXdpp3;
          ##ctp4 = CXdpp4;
          ##ctp5 = CXdpp5;
          ##ctp6 = ' ';

          GetSecAut(##utyp:##ulvl:##ctyp:##clvl:##exec:##res:##ctp2:
                    ##ctp3:##ctp4:##ctp5:##ctp6);

          If ##res = *on;
             dspReqDel = *off;
          Else;
             dspReqDel = *on;
          Endif;
       Endif;

      /End-Free
     P ValidDspDel     E
       //===================================================================
       // ValidFltr - Validate filter parameter
       //===================================================================
     P ValidFltr       B
     D ValidFltr       PI
      /Free

       isValid = *off;
       erFltrSfDt = *off;
       erFltrStDt = *off;

       // If From date is valid.
       If scFltrSfDt > 0;
          intFromDate = dtcymd(scFltrSfDt);
          If intFromDate = 0;
             msFltrSfDt = 'WCR0500';
             erFltrSfDt = *on;
             isValid = *on;
          Endif;
       Else;
          intFromDate = 0;
       Endif;

       // If To date is valid.
       If scFltrStDt > 0;
          intToDate = dtcymd(scFltrStDt);
          If intToDate = 0;
             msFltrStDt = 'WCR0500';
             erFltrStDt = *on;
             isValid = *on;
          Endif;
       Else;
          intToDate = 0;
       Endif;

       // If only To date entered.
       If isValid = *off and scFltrSfDt = 0 and scFltrStDt > 0;
          msFltrSfDt = 'WCR2246';
          erFltrSfDt = *on;
          isValid = *on;
       Endif;

       // If only From date entered.
       If isValid = *off and scFltrSfDt > 0 and scFltrStDt = 0;
          msFltrStDt = 'WCR2247';
          erFltrStDt = *on;
          isValid = *on;
       Endif;

       // If From date > To date
       //******* beg chg ****** 04/29/13 ***************************
       // If isValid = *off and scFltrSfDt > 0 and scFltrStDt > 0
       //                   and scFltrSfDt > scFltrStDt;
       If isValid = *off and intFromDate > 0 and intToDate > 0
                         and intFromDate > intToDate;
       //******* end chg ****** 04/29/13 ***************************
          msFltrStDt = 'WCR1502';
          msFltrSfDt = 'WCR1502';
          erFltrStDt = *on;
          erFltrSfDt = *on;
          isValid = *on;
       Endif;

      /End-Free
     P ValidFltr       E
       //===================================================================
       // WriteDarts - Write Dart remarks
       //===================================================================
     P WriteDarts      B
     D WriteDarts      PI
      /Free

       // Execute DARTS Complete Diary logic
       ##ckey = *Blanks;
       CCO#   = %editc(RPCo#:'X');
       CFND   = %editc(RPFnd:'X');
       CFYR   = %editc(RPfyr:'X');
       CCASE  = %editc(RPcase:'X');
       ##CSEQ = RPdseq;
       ##rtcd = 'D';

       ProcDarts(##ckey:##cseq:##rtcd);

      /End-Free
     P WriteDarts      E
      *================================================================
      *ValidateSts =  Validate the Status (Existing ERRCK1 W0070R12)
      *================================================================
     P ValidateSts     B
     D ValidateSts     PI
      *** Begin Add *** 07/30/20 **************************************
       DCL-S isPPOfees ind;
      *** End   Add *** 07/30/20 **************************************

      /free
       errClmSts =  *off;
       isError   =  *off;
       isErrorS  =  *off;
      /end-free
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  If the claim is being closed, process various edits.
      *
B1   C     scClmSts      IFEQ      'C'
     C     strClmSts     ANDEQ     'O'
      *
      *  Read the Request Payment file.
      *
     C                   CLEAR                   isErrorS
      *
     C                   CLEAR                   PT
     C                   CLEAR                   PA
     C     keyWARPL      CHAIN     PRP00                              60
     C     *IN60         DOWEQ     '0'
      *
      *  Send an error msg if any pended pmts exist and the 'Allow claim
      *    closing for pended pmts' flag (WMCXP/CXACCP) = N.
      *
      *** Begin Change *** 07/30/20 *******************************************
     C*    RPSTAT        IFEQ      'P'
     C*    CXACCP        ANDNE     'Y'
     C*                  EVAL      errClmSts  = *on
     C*                  EVAL      errMsgSts= 'WCR2374'
     C*                  EVAL      isErrorS   = *on
     C*                  LEAVE
     C*                  ENDIF
       If strCltId = '152' and
          RPfein = 812854849 and RPsts = 'PO' and RPbr2 = 'NA';
          isPPOfees = *On;
       Else;
          isPPOfees = *Off;
       Endif;

       If isPPOfees = *Off and RPstat = 'P' and CXaccp <> 'Y';
          errClmSts  = *on ;
          errMsgSts= 'WCR2374';
          isErrorS   = *on ;
          Leave;
       Endif;
      *** End   Change *** 07/30/20 *******************************************
      *
      *  Accumulate released pmts by reserve code.
      *
     C     RPSTAT        IFEQ      ' '
     C                   Z-ADD     1             X
     C     RPRECD        LOOKUP    PT(X)                                  61
     C     *IN61         IFEQ      '1'
     C                   ADD       RPAMT         PA(X)
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    PT(X)                                  61
     C     *IN61         IFEQ      '1'
     C                   MOVE      RPRECD        PT(X)
     C                   Z-ADD     RPAMT         PA(X)
     C                   END
     C                   END
     C                   END
      ***** Begin change 06/17/15 ******************************************
     C****       keyWARPL      READE     PRP000
     C     keyWARPL      READE     PRP00                                  60
      ***** End   change 06/17/15 ******************************************
     C                   ENDDO
      *
      *  The claim cannot be closed if the amt of pmt requests
      *    exceed the reserve cushion setup.
      *
     C                   DO        10            X
      *
     C     PT(X)         IFNE      *BLANKS

     C                   MOVE      PT(X)         strRvCd
     C     keyWDRCL      SETGT     PRC00
     C     keyWDRCL#     READPE    PRC00                                  60
      *
     C     CMLTFL        IFEQ      'I'
     C     PA(X)         ANDGT     RCLAMT
     C     CMLTFL        OREQ      'M'
     C     PA(X)         ANDGT     RCMAMT
     C     *IN60         OREQ      '1'
     C                   EVAL      errClmSts  = *on
     C                   EVAL      errMsgSts = 'WCR2375'
     C                   EVAL      IserrorS =  *on
     C                   LEAVE
     C                   ENDIF

     C                   ENDIF

     C                   ENDDO
     C*
     C* Do not allow an indemnity claim to be closed if no comp paymnts
     C*
     C                   If        IserrorS <>  *on
     C     strCLTID      IFEQ      '133'
     C
     C     CMLTFL        IFEQ      'I'
     C     scClmSts      ANDEQ     'C'
     C
     C*  If closing LT claim, reminder to stop payment.
     C                   DOU       BtnCntWdW5 = *ON
     C                   EXFMT     rcdWDW05
     C
     C                   ENDDO
     C*
     C                   CALL      'WA723R'
     C                   PARM      CMCO#         $$CO#             3 0
     C                   PARM      CMFND         $$FND             3 0
     C                   PARM      CMFYR         $$FYR             3 0
     C                   PARM      CMCASE        $$CASE            7 0
     C                   PARM      *BLANKS       $$RTCD            1

     C     $$RTCD        IFEQ      'N'

     C******** beg chg ****** 05/17/13 *********************
     C********           DO
     C                   DOU       BtnCntWdW3 = *on
     C******** end chg ****** 05/17/13 *********************
     C                   EXFMT     rcdWDW03
     C
     C     BtnCnlWdW3    IFEQ      *ON
     C                   Eval      isErrorS = *on
     C                   LEAVE
     C                   ENDIF
     C
     C******** beg del ****** 05/17/13 *********************
     C*****BtnCntWdW3    IFEQ      *ON
     C*****              LEAVE
     C*****              ENDIF
     C******** end del ****** 05/17/13 *********************
     C
     C                   ENDDO
     C
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   ENDIF
     C
     C                   IF        isErrorS = *off
     C
     C                   MOVE      *BLANKS       XXMDBN
     C     keyWMCDL      CHAIN     PCD00                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CDMDBN        XXMDBN            1
     C                   ENDIF
     C     strCLTID      IFEQ      '133'
     C     XXMDBN        ANDEQ     'Y'
     C*** Beg chg *** 04/03/13 *************************************************
     C******** Beg Chg ****** 05/17/13 ********************************
     C*****              EXFMT     rcdCMSErr
     C*****btnCnlCMS     IFEQ      *on
     C*************      CALL      'WA117R'
     C*************      PARM      'R'           @@CALL            1
     C*************      PARM                    @@KL              1
     C*************      PARM                    @@YES             1
     C*************     @@YES         IFEQ      ' '
     C*************     @@KL          OREQ      '1'
     C*** End chg *** 04/03/13 *************************************************
     C******             EVAL      iserrorS = *on
     C******             ENDIF
     C
     C                   DOU       btnCntCMS = *on
     C                   EXFMT     rcdCMSErr
     C     btnCnlCMS     IFEQ      *on
     C                   EVAL      iserrorS = *on
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C******** End Chg ****** 05/17/13 ********************************
     C                   ENDIF
     C                   ENDIF
      *
      *  If active DARTS Diaries exist, display an informational
      *    window to inform the user and allow them to continue
      *    procesing the claim closing.
      *
     C                   IF        isErrorS = *off
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      CMCO#         QCCO#
     C                   MOVE      CMFND         QCFND
     C                   MOVE      CMFYR         QCFYR
     C                   MOVE      CMCASE        QCCASE
      *
     C*** Begin Chg *** 08/16/17 *************************************
     C*****keySDCML4     CHAIN     PCM0A                              60
     C     keySDCML4     CHAIN(n)  PCM0A                              60
     C*** End   Chg *** 08/16/17 *************************************
     C     *IN60         DOWEQ     *OFF
      *
     C*** Begin Chg *** 02/27/18 *************************************
     C*****CMDCDT        IFEQ      0
     C*****CMDIFL        ANDEQ     'Y'
     C     C2DCDT        IFEQ      0
     C     C2DIFL        ANDEQ     'Y'
     C*** End   Chg *** 02/27/18 *************************************

     C                   EXFMT     rcdwdw01
      *
     C     btnCnlWdw1    IFEQ      *ON
     C                   eval      iserrorS = *on
     C                   LEAVE
     C                   ENDIF
      *
     C     btnCntWdw1    IFEQ      *ON
     C                   LEAVE
     c                   ENDIF
      *
     C                   ENDIF
      *
     C*** Begin Chg *** 08/16/17 *************************************
     C*****keySDCML4     READE     PCM0A                                  60
     C     keySDCML4     READE(n)  PCM0A                                  60
     C*** End   Chg *** 08/16/17 *************************************
     C                   ENDDO
      *
      *
     C                   ENDIF
     C                   ENDIF
      *
      /free
         //  Give the pop window for the explanation of Status change

              If        scclmsts <> strclmsts  and iserrorS = '0';
                  Clear  scExpl;
                  Clear  scexcd;
                  DoU  (scExpl <> *blanks  or scexcd <> *blanks)
                       and BtnCntSWdw  = *on;

                       Exfmt     rcdRsnSWdw;

                       If        BtnCnlSWdw  = *on;
                       Eval      ISerrorS = *on;
                       Leave;
                       Endif;

                  Enddo;

              Endif;

        // Passed all the edits
      /end-free

     C*Make sure no other user has updated the claim in the meanwhile
     C
     C     inRRNClm      CHAIN(N)  DBRCDF                             99
     C
     C                   If        savTime  <> CMTime and savDate <> CMdt
     C                             and CMuser <> Q1user
     C
     C                   CALL      'WA017R'
     C                   PARM      *BLANK        QQUSER           10
     C                   PARM                    QQRTCD            1
     C                   Eval      ISerrorS = *on
     C                   END
     C
     P ValidateSts     E
      *** Begin Add *** 07/30/20 *******************************************
       //===================================================================
       // * Check Release Payment Authority
       //===================================================================
       DCL-PROC ChkRlsPayAuth;
          DCL-PI *N IND;
          @@Co packed(3:0)  Const;
          END-PI;
          DCL-S isRlsPayAuth ind ;
          DCL-S flgResponse char(1) ;

          isRlsPayAuth = *Off;                               //Not authorized

          chain (@@Co:'PY') WTPSL1;
          If %found(WTPSL1) and (PStyp1 <> *blank or PStyp2 <> *blank or
             PStyp3 <> *blank or PStyp4 <> *blank or PStyp5 <> *blank or
             PStyp6 <> *blank or PStyp7 <> *blank or PStyp8 <> *blank or
             PStyp9 <> *blank or PStyp10 <> *blank);
             CheckSecurity(userType:%Editc(userLevel:'X'):
                PStyp1:%Editc(PSlvl1:'X'):'1':flgResponse:
                PStyp2:PStyp3:PStyp4:PStyp5:PStyp6:
                %Editc(PSlvl2:'X'):%Editc(PSlvl3:'X'):%Editc(PSlvl4:'X'):
                %Editc(PSlvl5:'X'):%Editc(PSlvl6:'X'):
                PStyp7:PStyp8:PStyp9:PStyp10:
                %Editc(PSlvl7:'X'):%Editc(PSlvl8:'X'):%Editc(PSlvl9:'X'):
                %Editc(PSlvl10:'X'));
             If flgResponse = *On;
                isRlsPayAuth = *Off;
             Else;
                isRlsPayAuth = *On;                        //Authorized
             Endif;

          Else;
             isRlsPayAuth = *On;
          Endif;

          Return isRlsPayAuth;
       END-PROC ChkRlsPayAuth;
      *** End   Add *** 07/30/20 *******************************************
      *** Begin Add *** 06/10/21 *******************************************
       //===================================================================
       // * Check authority to change payment type to MICR.
       //===================================================================
       Dcl-Proc MICRAuth;
        Dcl-Pi *N Ind;
        inCo# packed(3:0);
        inAcct char(20);
        inReqAuth char(10) const;
        End-Pi;

        Dcl-S hasMICRauth ind ;
        Dcl-S strAuth char(1);

        hasMICRauth = *Off;                               //Not authorized

        //Retrieve authority
        clear pck01;
        chain (inCo#:inAcct) pck01;
        If %found(wsckl);
           KeyWSCKL1SEC.CKSePMiCo# = CKco#;
           KeyWSCKL1SEC.CKSePMiAcc = CKacct;
           KeyWSCKL1SEC.CKSePMiId  = 'CHGMICR';
           Chain %kds(KeyWSCKL1SEC) PCKS01;
           If %found(wsckl1sec) and CKSepMiId = inReqAuth and
              (CKSepMIsT1 <> *blank or CKSepMIsT2 <> *blank or
               CKSepMIsT3 <> *blank or CKSepMIsT4 <> *blank or
               CKSepMIsT5 <> *blank or CKSepMIsT6 <> *blank or
               CKSepMIsT7 <> *blank or CKSepMIsT8 <> *blank or
               CKSepMIsT9 <> *blank or CKSepMIsTA <> *blank);
               CheckSecurity(userType:%Editc(userLevel:'X'):
                CKSepMIsT1:%Editc(CKSepMIsL1:'X'):'1':strAuth:
                CKSepMIsT2:CKSepMIsT3:CKSepMIsT4:CKSepMIsT5:CKSepMIsT6:
                %Editc(CKSepMIsL2:'X'):%Editc(CKSepMIsL3:'X'):
                %Editc(CKSepMIsL4:'X'):%Editc(CKSepMIsL5:'X'):
                %Editc(CKSepMIsL6:'X'):
                CKSepMIsT7:CKSepMIsT8:CKSepMIsT9:CKSepMIsTA:
                %Editc(CKSepMIsL7:'X'):%Editc(CKSepMIsL8:'X'):
                %Editc(CKSepMIsL9:'X'):%Editc(CKSepMIsLA:'X'));
                If strAuth = '0';
                   hasMICRauth = *On;
                Endif;
           Endif;
        Endif;

        return hasMICRauth;

       End-Proc ;
      *** End   Add *** 06/10/21 *******************************************
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /Copy ScopyClmPi
