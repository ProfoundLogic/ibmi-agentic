     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE WTCLMDTL - Profound Claim Detail
     h/DEFINE   PROFOUNDUI
     h DFTACTGRP(*no) ACTGRP('QILE')
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Display Claim Summary Info.
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 2/6/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:
     h*
     h*  05/30/25 C:344  Support 119141 (ORISKA)
     h*                  Set maximum record property to 2500 rows for the
     h*                  scLocation field in the rcdClmDt0 format in the
     h*                  the WTCLMDTL Profound display file.
     h*
     h*  04/08/25 C:333  Support: 118812 (Project: WC2998)
     h*                 - Recompiled as DSPF WTCLMDTL changed.
     h*
     h*  03/06/25 C:333  Project WC2998
     h*                  Changes to CMS Settlement Reporting.
     h*
     h*  01/14/25 C:340  Project WC2995
     h*                  Recompiled as DSPF WTCLMDTL changed.
     h*
     h*  04/10/24 C:079 Ticket: 146/115349      Sync by: 333
     h*                 Update CMltFl After ErrorCheck0
     H*
     H*  03/22/24 C:333 Project: WC2826
     H*                 Claims Contacts.
     H*                 DSPF - Dynamic value for LblAdjuster (rcdClmDt1).
     H*                      - changes in rcdClmDt1 onload event.
     H*
     H*  08/17/23 C:079 Support: 113594-129
     H*                 - Following DSPF changes:
     H*                   Decrease height on FROI tab boxes so they dont
     H*                   overlay the field heading
     H*
     H*  04/10/23 C:333 Support: 108889 (UBIC)
     H*                 - Following DSPF changes:
     H*                   - added hidden textbox to keep current index and
     H*                     Previous User input (Txb_N_Input,Txb_N_Index,
     H*                     Txb_C_Input,Txb_C_Index,Txb_P_Input,Txb_P_Index)
     H*                   - Added 'onkeypress' event for Select boxes (DrpNature
     H*                     DrpCause, DrpPart).
     h*
     H*  12/06/22 C:291 Project: 152WBZ
     H*                 Add Settlement Information grid on Legal Information tab
     h*
     H*  09/06/22 C:079 Project: WC2823
     H*                 - FROI Change State Heading/Fields, etc.
     h*
     H*  04/18/22 C:291 Project: WC2852
     H*                 - Recompile: Display file changes for Cat# 13
     h*
     H*  03/25/22 C:333 Project: SY434
     H*                 - Update Grids for Sorting Filtering Upgrade
     h*
     H*  01/25/22 C:079 Project 133WSP
     H*                 Allow Staff Code Update in Inquiry Mode
     h*
     H*  10/15/21 C:079 Support 146/98735
     H*                 AWW is not updating pre injury wage
     h*
     h*  08/24/21 C:180 Support: 97319
     h*                 Do not prevent location entry when the premises
     h*                 check box is checked. These two are not mutually
     h*                 exclusive (per client). Screen changes only.
     h*
     H*  06/23/21 C:318 Project WC2302 - Auto tab closure for
     H*                deletion.
     h*
     h*  07/14/21 C:079 Project: WC2816
     h*                 USL&H checkbox will be read only
     h*
     h*  04/04/21 C:180  Project: WC2732
     h*                  Change multi claim link to call new multi claim
     h*                  app. No changes here. See Copy book SCOPYCLMPI.
     h*
     h*  02/24/21 C:318  Support: 94631
     h*                  Added Shortcut Key for Clearnce Cheeck/Next Button in d
     h*                  to correct validation on ssn if enter pressed.
     h*
     h*  11/11/20  C:318 Support: 92686
     h*                  Corrected spelling of "Partial" in Display File.
     h*                  Recompile the module.
     h*
     h*  06/09/20  C:298 Project: SY420
     h*                  Mask SSN
     h*
     h* 05/27/20 C:180  Project: WC2711C - Add primary comp rate check box.
     h*
     h* 05/14/20 C:180  Project: WC2711B - Remove WC2711 like it never
     h*                 happened (No comments).
     h*
     h* 11/25/19  C:079 Project: 152WAW - Recompile
     h*                 Added check box fields: ESE, IAQ, Student
     h*                 aggresssion, Partial Claim Denial, 120 day
     h*                 review
     H*
     h*  10/30/19 C:212 Project: WC2706
     h*                 Add Delete link to % of Body Part Grid
     h*
     h*  10/16/19 C:079 Support: 99-84271
     h*                 Adjust Occupation Code field
     h*
     h*  09/30/19 C:079 Support: 146-82862
     h*                 Use Generic table ID GNDR for Gender
     H*
     h*  08/19/19 C:079 Project: WC2604
     h*                 IDC Changes
     H*
     h*  07/02/19 C:079 Project: WC2681
     h*                 Make Occupation a type ahead on description
     H*
     H*  06/17/19 C:180 Support: 71987
     H*                 Capture class extension
     H*                 Dspf changes to class code dropdown
     H*                 See WPICLMDAT for additional changes.
     h*
     h*  05/22/19 C:079 Support: 152/79622
     h*                 Change to use isModEnabled for Managed Care
     h*
     h*  11/26/18 C:079 Project: 136WHX
     h*                 Add TPA Claim Number for client 136
     h*
     h*  08/31/18 C:079 Support: 146/72945
     h*                 Add Order by property on drop down fields
     h*                                                                    *
     h*  05/30/18 C:180 Project: WC2515                                    *
     h*                 Recompile over changed WTCLMDTL DSPF               *
     h*                 See WPICLMDAT module for changes.                  *
     h*                                                                    *
     h*  05/30/18 C:079 Project: WC2515                                    *
     h*                 Add fields for Work History Grid.                  *
     h*                                                                    *
     h*  05/01/18 C:079 Project: WC1917                                    *
     h*                 Add fields for location and part                   *
     h*                                                                    *
     h*  02/14/17 C:212 Project: 90MHY                                     *
     h*                 PBM Eligible changes (added scPBME in rcdClmDt2)   *
     h*                                                                    *
     h*  01/31/17 C:306 Project: WC2410                                    *
     h*                 Recompile over changed WTCLMDTL                    *
     h*                                                                    *
     h*  07/25/16 C:136 Project: 114WYQ                                    *
     h*                 Add Disable SMS Texting checkbox                   *
     h*                                                                    *
     h*  05/24/16 C:309 Support: 41670 Display file changes only.          *
     h*                 Changed screen field "Prior Claim#" length to 25A  *
     h*                                                                    *
     h*  05/09/16 C:300 Project: 114WXV                                    *
     h*                 Add Cell Number to Claimant Demographics.          *
     h*                                                                    *
     h*  04/22/16 C:298 Project: WC2429                                    *
     h*                 Added popup to indicate Claim data has been saved. *
     h*                                                                    *
     h*  04/12/16 C:309 Project: 114MXO                                    *
     h*                 Made change in screen - rcdClmDt2                  *
     h*                 changed field "scRXEL" description to "No Rx Card" *
     h*                                                                    *
     h*  03/23/16 C:308 Project: Elevate                                   *
     h*                 Genericize the toolbar programming               *
      *                                                                    *
      * 01/07/16 C:65  Project: WC2401                                     *
      *                Change the coding done under WC2254 to call an      *
    *                API instead of directly calling the various         *
      *                programs for windows here.                          *
     h*                                                                    *
     h*  11/27/15 C:298 Support: 37892                                     *
     h*                 Added an Onload event to populate Accient location *
     h*                 address when employer premises is checked.         *
     h*
     h*  12/11/15 C:53  Project: C133WPK                                   *
     h*                 Add 2 new fields to screen for 133 only
     h*                                                                    *
     h*  12/02/15 C:309 Project: 25MUD                                     *
     h*                 Added a new field "Prior Claim#' on            ion *
     h*                 rcdClmDt0 - Claim Information                  ion *
     h*                                                                    *
     h*  10/14/15 C:300 Support: 37399                                     *
     h*                 Update UI by updating dollar fields with commas    *
     h*                 1) Compensation Rate in Wage Information Grid      *
     h*                 2) Specific Retention on Management Tab.           *
     h*                 3) Settlement Amount on Legal Tab.                 *
     h*                 4) TPOC Amount in Multiple Settlement grid on Legal*
     h*                                                                    *
     h*  10/01/15 C:308 Project: C114WVO                                   *
     h*                 Recompile only due to change in Screen fields      *
     h*                                                                    *
      *  09/17/15 C:302 Project: wc2254                                    *
      *                 Recompile only                                     *
     h*
     h*  06/23/15 C:306 Support: 31812
     h*                 Changed the Email address field attribute.
     h*                 (Recompilation Only).
     h*
     h*  05/25/15 C:300 Project: WC2043 (Priss #23)
     h*                 ICD code confirmation Window showing incorrectly
     h*
     h*  03/19/15 C:298 Support: 32825
     h*                 To prevent calling of Error Checks when returning     *
     h*                 from Documents Window Pop Up.                         *
     h*                                                                       *
     h*  04/20/15 C:300 Project: WC2043
     h*                 Modified CMS alleged cause of injury field to use  *
     h*                 ICD-9 or ICD-10 code based on date of injury.      *
     h*                                                                    *
     h*  04/16/15 C:65  Project: SY251                                     *
     h*                 Add API call to launch new Department letters.     *
     h*
     h*  04/06/15 C:180 Support: 32678 Display file changes only.             *
     h*                 Changed tab index for campus code.
     h*
     h*  11/11/14 C:300 Project: 114WPG (Remove changes)                      *
     h*                 Add ODG Link to Claims Medical Tab for client '114' . *
     h*
     h*  10/29/14 C:180 Support: 87143 Display file changes only.             *
     h*                 Changed unchecked attribute for checkbox              *
     h*                 Death Result of Injury to *blanks                     *
     h*                                                                       *
     h*  10/14/14 C:291 RED 3161                                              *
     h*                 Change the date definition on the grid from           *
     h*                 Number to date data type.                             *
     h*
     h*  10/09/14 C:189 Redmine: 3029
     h*                 Tab description is not appropriate when there is no   *
     h*                 specific state and value is A1                        *
     h*
     h*  09/26/14 C:282 Support: WA146 86804
     h*                 Set the 'Show zero balance' property on the field
     h*                 sc#Dep of Claim Infomation screen. Recompile the
     h*                 program.
     h*
     h* 09/08/14  C:300 Project: 114WPG                                       *
     h*                 Add ODG Link to Claims Medical Tab for client '114' . *
     h*
     h* 08/25/14  C:279 Project: 114WOP                                       *
     h*                 Added Alternate State Claim # field for client '114'. *
     h*
     h* 07/28/14  C:295 Project: 114WMD                                       *
     h*                 Compilation only as the changes are in Action Panel   *
     h*
     h*  08/04/14 C:283 Support: WC146 84781                                  *
     h*                 Re-Compile Only.                                      *
     h*
     h*  06/26/14 C:267 Support: 85792                                        *
     h*                 UI: added a new label widget for Employer Premises    *
     h*
     h*  06/09/14 C:300 Redmine: 4300                                         *
     h*                 Completing Mandatory info on STATE screen             *
     h*                 unable to save data                                   *
     h*
     h*  05/14/14 C:53  Redmine: 4186                                         *
     h*                 Add California fields                                 *
     h*
     h*  03/27/14 C:180 Support: WC025 85146 V14R3                            *
     h*                 Fix choice selection for Nature, Part, and Cause      *
     h*                 EDI tables - Recompiled only - See changes            *
     h*                 in WPICLMDAT.
     h*
     h*  03/14/14 C:295 Support: WC114 84963
     h*                 Add the Delete link for Trial Information Grid.
     h*
     h*  03/04/14 C:180 Support: WC114 84947                                  *
     h*                 Add MCO code.
     h*                 Changes made to DSPF only - PGM just recompiled.
     h*
     h*  02/18/14 C:283 Support: WA045 84644
     h*                 Removed the OnChange event from the Subrogated
     h*                 field on the Display File. Re-Compiled the pgm.
     h*
     h*  02/12/14 C:180 SUPPORT PRISS 027 - WC2234
     h*                 Fix Work Days Schedule code field.
     h*
     h*  02/08/14 C:180 SUPPORT PRISS 027 - WC2234
     h*                 Fix values for Work Week Type Code
     h*                 Changes made to DSPF only - PGM just recompiled.
     h*
     h*  02/05/14 C:282 Project: SY336 Priss 14
     h*                 Corrected the layout of the Claim display
     h*                 file.
     h*
     h*  01/28/14 C:062 Support: WC114 84740
     h*                 Only show included icd9 codes.
     h*
     h*  01/15/14  C:53 Project: 147WCJ
     h*                 Determine claim start date for EDI
     h*
     h*  01/14/14 C:295 Support: WC114 84281
     h*                 Recompile only
     h*
     h*  11/25/13 C:282 Project: 129WSC
     h*                 Add new fields for claims for client 129 only.
     h*                 - Restitution
     h*                 - 14-6
     h*                 - Loss transfer
     h*                 - Misc
     h*                 - Apportionment %
     h*                 - Apportionment Effective Date
     h*
     h*  12/12/13 C:287 Project:WC2234
     h*                 Add new fields for claims EDI Rel3.
     h*                 CMpspa, CMwwtc, CMwdsc, CMesid
     h*
     h*  10/11/13 C:283 Support: WC133 83757
     h*                 Recompile only. ScAccTime formatting
     h*                 changed to show the leading zeros.
     h*
     h*  10/01/13 C:053 Project: 25WSS
     h*                 Client retrofit.
     h*
     h*  09/26/13 C:291 Redmine: 1195
     h*                 Recompile only
     h*
     h*  09/23/13 C:062 Support: WC114 83507
     h*                 Recompile only
     h*
     h*  09/17/13 C:062 Support: op114 83508
     h*                 Compile only
     h*
     h*  08/30/13 C:062 Support: WC146 82365
     h*                 Pass the CMS submission flag.
     h*
     h*  08/23/13 C:282 Redmine: 3227
     h*                 Added the CMS Submission Edit flag in
     h*                 Management Information screen.
     h*
     h*  06/21/13 C:65  Redmine: 3034
     h*                 Change county text box into a dropdown.
     h*
     h*  06/11/13 C:282 Redmine: 3034
     h*                 Added the hidden outfield scJob, scJobn,
     h*                 scCusr, scCo#, scMod in rcdClmDt0.
     h*                 Added onblur event on Claimant Zip and Place Zip.
     h*
     h*  06/03/13 C:283 Support: WC146 82361
     h*                 Add the Delete link for Diagnostic Code.
     h*
     h*  05/28/13 C:282 Project: 63WBU
     h*                 BRAUMS Retrofit Custom Code to Base
     h*
     h*  02/20/13 C:282 Project:  147WAO
     h*                 Load/update new field Rate Of Pay, Per, Hours Per WeeK
     h*                 under FL State Information in dislay file.
     h*                 Only Recompile the program.
     h*
     h*  04/08/13 C:190 Project: 45WJH
     h*                 Changed Special Disability Fund from a checkbox to
     h*                 a drop-down for LCTA.
     h*
     h*  03/04/13 C:280 Redmine: 2499
     h*                 change to tab to accident description 2 from
     h*                 accident description 1
     h*
     h*  01/09/13 C:284 Project: 146WFQ
     h*                 Added new fields scFeet and scInch in display file.
     h*                 Only Recompile the program.
     h*
     h*  10/25/12 C:284 Project: 114WGD
     h*                 CSI Retrofit of Custom Code to Base
     H*                                                               *
     H* 01/09/12 C:251 Project: 114MCX                                *
     H*                Set CSI flag for diff screens                  *
     h*
     h**********************************************************************
     fwtClmDtl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     ExtFile('WTCLMDTL')
     f                                     Sfile(sflAww:rrnAww)
     f                                     Sfile(sflBenefit:rrnBenefit)
     f                                     Sfile(sflDiag:rrnDiag)
     f                                     Sfile(sflEarning:rrnEarning)
     f                                     Sfile(sflImr:rrnImr)
     f                                     Sfile(sflMult:rrnMult)
     f                                     Sfile(sflPaid:rrnPaid)
     f                                     Sfile(sflParts:rrnParts)
     f                                     Sfile(sflPhys:rrnPhys)
     f                                     Sfile(sflTrial:rrnTrial)
     f                                     Sfile(sflWitness:rrnWitness)
     f                                     Sfile(sflWork:rrnWork)
     f                                     Sfile(sflWrkGrd:rrnWrk)
      *** Beg Add ***** 12/06/22 ********************************************
     f                                     Sfile(sflSettle:rrnSettle)
      *** End Add ***** 12/06/22 ********************************************
     f                                     Ignore(rcdClrInfo)
     f                                     Ignore(rcdConfClm)
     f                                     InfDs(fids01)
     fwtActPnl  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdClmPnl)
      // Claim Master <=====================================================
     fwmcmp     if   e             Disk    ReName(pcm01:dbrcdf)
     f                                     InfDs(dbfds)
      // Employer Master <==================================================
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     f                                     InfDs(dbfds2)
      // Table codes <======================================================
     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     fwtabl     if   e           k Disk    ReName(ppa01:ppa00)
      // Grids <============================================================
      // Aww History
     fwmawl1    if   e           k Disk    infds(wmawl1fds)
      // Benefit
     fwmbnl     if   e           k Disk    ReName(pbn01:pbn00)
     f                                     infds(wmbnlfds)
      // Diagnosis Code
     fsadal     uf a e           k Disk    ReName(pda01:pda00)
     f                                     infds(sadalfds)
     fsmdil     if   e           k Disk    ReName(pdi01:pdi00)
     fwmicd10l  if   e           k disk    rename(pic01:pic00)
      // Earnings
     fwdr3l     if   e           k Disk    ReName(pr301:pr300)
     f                                     infds(wdr3lfds)
      // Multiple Settlement
     fwdmsl     uf a e           k Disk    ReName(pms01:pms00)
     f                                     infds(wdmslfds)
      // Employer Paid
     fwda3l     if   e           k Disk    ReName(pa301:pa300)
     f                                     infds(wda3lfds)
      // Body Part
     fwmbyl     uf a e           k Disk    ReName(pby01:pby00)
     f                                     infds(wmbylfds)
      // I/M/R History
     fwmthl     if   e           k Disk    ReName(pth01:pth00)
      // Trail Information
     fwmtil     uf a e           k Disk    ReName(pti01:pti00)
     f                                     infds(wmtilfds)
      // Treating Physician
     fwmtpl1    uf a e           k Disk    ReName(ptp01:ptp01)
     f                                     infds(wmtplfds)
      // Work History
     fwmwhl     if   e           k Disk    ReName(pwh01:pwh00)
     f                                     infds(wmwhlfds)
      // Witness Information
     fwmwil     if   e           k Disk    ReName(pwi01:pwi00)
     f                                     infds(wmwilfds)
      //*** Beg Add ***** 12/06/22 ***************************************
      // Settlement Tracking file
     fwdstltrkl if   e           k Disk    ReName(pst01:pst00)
     f                                     infds(wdstltrklfds)
      //*** End Add ***** 12/06/22 ***************************************
      // Comp Reimbursement File
     fwdrbl     if   e           k Disk    ReName(prb01:prb00)
     fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     Fwmcdl     if   e           k Disk    ReName(pcd01:pcd00)
     Fwdntp     o    e             Disk    ReName(pnt01:pnt0p)
     ffawhl     if   e           k Disk    Rename(pwh01:pwh0f)
     f                                     infds(fawhlfds) UsrOpn
     fftdll     if   e           k Disk    Rename(pdl01:pdl00) UsrOpn
     fftgtl     if   e           k Disk    Rename(pgt01:pgt0f) UsrOpn
     ffmcmp     if   e             Disk    Rename(pcm01:pcm0f) Prefix(fm:2)
     f                                     UsrOpn
     fwmupl     if   e           k Disk    Rename(pup01:pup00)
     f*** Beg Add *** 01/25/22 *************************************************
     fsmupl     if   e           k Disk    Rename(pup01:pup01) Prefix(U1:2)
     f*** End Add *** 01/25/22 *************************************************
     fwdr1l1    if   e           k Disk
     fssylf     if   e           k Disk    rename(swc01:swc00)
     f                                     ignore(ssy01)
     f                                     ignore(spc01)
     f                                     ignore(src01)
     f                                     ignore(sbc01)
     f                                     ignore(scs01)
     f

     d* Proto-types

     d* Create WCMC Case
     d ChangeWCMCdemo  pr                  ExtPgm('FA002R')
     d   qmRtCd                       1
     d   qmSS#                        9
     d   qmAcDt                       7
     d   qmCo#                        3
     d   qmSub#                       5
     d   qmFnd                        3
     d   qmFyr                        3
     d   qmCase                       7
     d   qmLsN                       30
     d   qmFsN                       15
     d   qmMid                        1
     d   qmAdr1                      40
     d   qmAdr2                      40
     d   qmCty                       25
     d   qmSt                         2
     d   qmZip                        9
     d   qmDtoB                       7
     d   qmSex                        1
     d   qmArea                       3
     d   qmPhn#                       7
     d   qmMsts                       1
     d   qmWara                       3
     d   qmWph#                       7
     d   qmCtry                       3
     d   qmAdr3                      40
     d   qmCara                       3
     d   qmCphn                       7

     d* Create WCMC Case
     d ChangeWCMC      pr                  ExtPgm('FA003R')
     d   qmRtCd                       1
     d   qmSS#                        9
     d   qmAcDt                       7
     d   qmCo#                        3
     d   qmSub#                       5
     d   qmFnd                        3
     d   qmFyr                        3
     d   qmCase                       7
     d   qmEmp#                       9
     d   qmDiv                        5
     d   qmAcTm                       6
     d   qmDtRp                       7
     d   qmDtLw                       7
     d   qmAcDs                      60
     d   qmMcon                       1
     d   qmEntD                       7
     d   qmRcvD                       7
     d   qmMMI                        7
     d   qm1a1                        1
     d   qm10a1                      10
     d   qmLtFl                       1
     d   qmScl#                      25
     d   qmMcTm                       4
     d   qmStWk                       2
     d   qmDeDt                       7
     d   qmDeem                       2
     d   qmCmNu                       3
     d   qmCmSu                       3
     d   qmClad                       3
     d   qmRrn                        9  0
     d   qmLsN                       30
     d   qmFsN                       15
     d   qmMid                        1
     d   qmDtRe                       7
     d   qmClSu                       3
     d   qm#Dep                       3
     d   qmOcDs                      40
     d   qmHiDt                       7  0
     d   qmEsts                       2
     d   qmWkPt                       2
     d   qmPrex                       1
     d   qmAcD2                      60
     d   qmCvFl                       1
     d   qmMca                        1
     d   qmMmiP                       5  2
     d   qmOdis                       1
     d   qmYrlx                       4  0
     d   qmDbDt                       7  0
     d   qmHrDy                       3  0
     d   qmDyWk                       1  0
     d   qmClFa                       3
     d   qmDny                        1
     d   qmImp                        1
     d   qmJrSt                       2
     d   qmPoSt                       2

     d* Get System Flags
     d GetSysFlags     pr                  ExtPgm('SA001R')
     d   wcmsInst                     1
     d   wcmcInst                     1
     d   wcmcInt                      1

     d* Get System Flags
     d EdiLogApi       pr                  ExtPgm('W0070R6')
     d   strCompany#                  3
     d   strGroup                     3
     d   strYear                      3
     d   strCase                      7
     d   strActFlag                   1
     d   strMessage                   7
     d   strReturnCod                 1
     d   strEDIDate                   7

     d* GetNameChgRmCd - Get Name Change Remark code
     d GetNameChgRmCd  pr             5

     d* GetNmAdChgRmCd - Get Name/Address Remark code
     d GetNmAdChgRmCd  pr             5

     d* Excess Reinsurance - Api
     d GetSpecRet      pr                  ExtPgm('WA010R')
     d   rrnClm                       9  0
     d   specRet                     13  2
     d   excessRein                   1

     d* Get the RRN of the WCMC case
     d GetRrn          pr                  ExtPgm('FA201R')
     d   ##Co#                        3
     d   ##Fnd                        3
     d   ##Fyr                        3
     d   ##Case                       7
     d   @@Rrn1                       9  0
     d   @@Rtcd                       1

      * get the ICD code status
     d GetICDSts       pr                  extpgm('WA120R')
     d @@InjuryDate                   7  0
     d @@ICDSts                       1

     d* Check Add and Edit functions
     d ChkAddEdit      pr

     d* Choose State Screen
     d ChooseStates    pr

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* Error Check
     d ErrorCheck      pr              n

     d* Load AWW History Grid
     d LoadAww         pr

     d* Load Benefit Adjustment Grid
     d LoadBenefit     pr

     d* Load Body Part Grid
     d LoadBodyPart    pr

     d* Load Diagnosis Grid
     d LoadDiagCode    pr

     d* Load Reduced Earnings Grid
     d LoadEarnings    pr

     d* Load Employer Paid Grid
     d LoadEmpPaid     pr

     d* Load I/M/R History Grid
     d LoadImr         pr

     d* Load Multiple Settlement Grid
     d LoadMult        pr

     d* Load Tab Information
     d LoadTab         pr

     d* Load Treating Physician Grid
     d LoadTreatPhys   pr

     d* Load Trial Information Grid
     d LoadTrial       pr

     d* Load Witness Grid
     d LoadWitness     pr

     d* Load Work History Grid
     d LoadWork        pr

     d* Load Work History Grid  for client 114
     d LoadWrkGrd      pr

     d* Resets Edit Add Indicators
     d ResetEditAdd    pr

     d* Check Security Level/Type
     d SecLvlChk       pr                  ExtPgm('SRCCSC')
     d   inUTyp                       1
     d   inULvl                       3
     d   inCTyp                       1
     d   inCLvl                       3
     d   inExTyp                      1
     d   outResult                    1
     d   inTyp2                       1
     d   inTyp3                       1
     d   inTyp4                       1
     d   inTyp5                       1
     d   inTyp6                       1

     d*Update Name
     d UpdateName      pr                  ExtPgm('WA085R')
     d   type                         1
     d   claimRrn                     9  0
     d   returnCode                   1

     d* Write Auto Dart For Address/Name Change
     d WriteAutoDart   pr                  ExtPgm('WA074R')
     d   company                      3  0
     d   group                        3  0
     d   year                         3  0
     d   case                         7  0
     d   fein#                        9  0
     d   modifier#                    3  0
     d   remarkCode                   5
     d   returnCode                   1
     d   changeAdr                  116
     d   change                       1

     d* Write Darts
     d WriteDarts      pr                  ExtPgm('DA700R')
     d   q#key                       40
     d   q#comm                     256
     d   q#cDat                       8
     d   q#dcId                      12
     d   q#pNam                      10
     d   q#secl                       3
     d   q#user                      10
     d   q#pgm                       10
     d   q#fold                      80
     d   q#diFl                       1
     d   q#iPgm                      10
     d   q#fl01                       1
     d   q#fl02                       1
     d   q#rmCd                       5
     d   q#seq#                       5
     d   q#sbyl                       1
     d   q#com2                      19
     d   q#apId                       2
     d   q#iDoc                      12
     d   q#iQue                      10
     d   q#imgF                       1
     d   q#iPth                      10
     d   q#pIdx                       5
     d   q#qPty                       1
     d   q#dcTp                       5
     d   q#csId                       2
     d   q#code                       2

     d* Write Name Change History Record
     d WriteNameHist   pr
     d   lastName                    30
     d   firstName                   15
     d   midInit                      1
     d   address1                    40
     d   address2                    40
     d   city                        25
     d   state                        2
     d   zipCode                      9s 0
     d   eMail                       60

     d* General Prototypes
     d/copy dPrRemark
     d/copy wPrUsrPrms
     d/copy sPrAtrium
     d/copy sPrMask
     d/copy sPrSecure
     d/copy sPrRelay
     d/copy sPrString
     d/copy wPrGroup
     d/copy sPrDate
     d/copy wPrClaims
     d/copy wPrAgent
     d/copy wPrContact
     d/copy wPrLoc
     d/copy wPrEmplr
     d/copy wPrClmPop
     d/copy wPrClmErr
     d/copy wPrClmDat
     d/copy wPrEdi
     d* Load Action Panel procedure Prototype and variables
     d/copy sCopyClmPr
     d/copy sPrLetter
     d/copy sprGenPop

     D* System Data Structures
     D dbfds           ds
     D  flstcm           *status
     D  dbrrn                397    400b 0
     D dbfds2          ds
     D  dbrrn2               397    400b 0
     D sadalfds        ds
     D SADAPrrn              397    400b 0
     D wda3lfds        ds
     D WDA3Prrn              397    400b 0
     D wdr3lfds        ds
     D WDR3Prrn              397    400b 0
     D wdmslfds        ds
     D WDMSPrrn              397    400b 0
     D wmawl1fds       ds
     D WMAWPrrn              397    400b 0
     D wmbnlfds        ds
     D WMBNPrrn              397    400b 0
     D wmbylfds        ds
     D WMBYPrrn              397    400b 0
     D wmtplfds        ds
     D WMTPPrrn              397    400b 0
     D wmtilfds        ds
     D WMTIPrrn              397    400b 0
     D wmwhlfds        ds
     D WMWHPrrn              397    400b 0
     D wmwilfds        ds
     D WMWIPrrn              397    400b 0
     D fawhlfds        ds
     D FAWHPrrn              397    400b 0
      *** Beg Add ***** 12/06/22 **********************************************
     D wdstltrklfds    ds
     D WDstltrkrrn           397    400b 0
      *** End Add ***** 12/06/22 **********************************************

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)
     D claimInfo     e ds                  ExtName(wtClmDtl:rcdClmDt0) inz
     D mgmtInfo      e ds                  ExtName(wtClmDtl:rcdClmDt1) inz
     D medicalInfo   e ds                  ExtName(wtClmDtl:rcdClmDt2) inz
     D benefitInfo   e ds                  ExtName(wtClmDtl:rcdClmDt3) inz
     D legalInfo     e ds                  ExtName(wtClmDtl:rcdClmDt4) inz
     D a1Info        e ds                  ExtName(wtClmDtl:rcdClmDtA1)
     D alInfo        e ds                  ExtName(wtClmDtl:rcdClmDtAL)
     D caInfo        e ds                  ExtName(wtClmDtl:rcdClmDtCA)
     D deInfo        e ds                  ExtName(wtClmDtl:rcdClmDtDE)
     D flInfo        e ds                  ExtName(wtClmDtl:rcdClmDtFL)
     D gaInfo        e ds                  ExtName(wtClmDtl:rcdClmDtGA)
     D laInfo        e ds                  ExtName(wtClmDtl:rcdClmDtLA)
     D maInfo        e ds                  ExtName(wtClmDtl:rcdClmDtMA)
     D ncInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNC)
     D nyInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNY)
     D paInfo        e ds                  ExtName(wtClmDtl:rcdClmDtPA)
     D wvInfo        e ds                  ExtName(wtClmDtl:rcdClmDtWV)
     D niInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNI)
     D diaryInfo     e ds                  ExtName(wtClmDtl:rcdClmDt6) inz
     D screenPtr       s               *
     D*** Beg Add *** 04/10/24 ********************************************
     D screenPtr2      s               *
     D*** End Add *** 04/10/24 ********************************************

     D*Variables
     D @@RRN           s              9
     D @@Mode          s              1
     D @@Next          s             10
     D @@TabIndex      s              5
     D outClientID     s              3    inz('000')
     D chgMode         s               n   inz(*off)
     D dspMode         s               n   inz(*off)
     D inRrnClm        s              9  0
     D inState         s              2
     D inMode          s              1
     D inSpecRet       s             13  2 inz(*Zeros)
     D inExcessRein    s              1    inz(*Blanks)
     D inRrnFile       s              9  0
     D inAddMode       s               n
     D inEdtMode       s               n
     D inDelMode       s               n
     D inCmsSub        s               n
     D xxBusPhone      s             15
     D errorExists     s               n   inz(*off)
     D addEdit         s               n   inz(*off)
     D cancel          s               n   inz(*off)
     D kkKey           s                   like(PAkey)
     D kkSeq#          s                   like(BYseq#)
     D kkSub#          s                   like(DAsub#) inz(0)
     D kkCaMs          s                   like(MScaMs)
     D kkSuDt          s                   like(TIsuDt)
     D kkTrTp          s                   like(TItrTp)
     D kkJudg          s                   like(TIjudg)
     D kkIssu          s                   like(TIissu)
     D kkVerd          s                   like(TIverd)
     D kkTrDt          s                   like(AWtrDt)
     D kkTime          s                   like(AWtime)
     D kkTbId          s                   like(GTtbId)
     D kkBtc           s                   like(A3btc)
     D kkDate          s                   like(WHdate)
     D kkWkSt          s                   like(WHwkSt)
     D kkReWk          s                   like(R3reWk)
     D kkEfDt          s                   like(RBefDt)
     D fromDate        s               d   datfmt(*usa)
     D toDate          s               d   datfmt(*usa)
     D years           s              3  0
     D months          s              2  0
     D yearText        s             10
     D monthText       s             10
     D testInt         s             20
     D length          s              5  0
     D position        s              5  0
     D position2       s              5  0
     D froiState       s              3    Inz(*Blanks)
     d fein#           s              9  0
     d modifier#       s              3  0
     d remarkCode      s              5
     d returnCode      s              1
     d chgAddress      s            116
     d change          s              1
     d type            s              1
     d mode            s              1    Inz('C')
     d svltfl          s              1
     d strEDIWork      s              7  0
     d strEDIDate      s              7

     d ZeroDate        s               D   inz datfmt(*ISO)

     d strCompany#     s              3
     d strGroup        s              3
     d strYear         s              3
     d strCase         s              7
     d strActFlag      s              1    Inz('T')
     d strMessage      s              7    Inz(*Blanks)
     d strReturnCod    s              1    Inz(*Blanks)

     d q#key           s             40
     d q#comm          s            256
     d q#cDat          s              8
     d q#dcId          s             12    Inz(*Blanks)
     d q#pNam          s             10    Inz('W0070R')
     d q#secl          s              3    Inz('000')
     d q#user          s             10
     d q#pgm           s             10
     d q#fold          s             80    Inz(*Blanks)
     d q#diFl          s              1    Inz(*Blanks)
     d q#iPgm          s             10
     d q#fl01          s              1    Inz(*Blanks)
     d q#fl02          s              1    Inz(*Blanks)
     d q#rmCd          s              5
     d q#seq#          s              5    Inz('00000')
     d q#sbyl          s              1    Inz('N')
     d q#com2          s             19    Inz(*Blanks)
     d q#apId          s              2    Inz(*Blanks)
     d q#iDoc          s             12    Inz(*Blanks)
     d q#iQue          s             10    Inz(*Blanks)
     d q#imgF          s              1    Inz(*Blanks)
     d q#iPth          s             10    Inz(*Blanks)
     d q#pIdx          s              5    Inz('00000')
     d q#qPty          s              1    Inz(*Blanks)
     d q#dcTp          s              5    Inz(*Blanks)
     d q#csId          s              2    Inz(*Blanks)
     d q#code          s              2    Inz(*Blanks)
     d isICDsave       s              1    inz
     d isICDFirst      s              1    inz
     d @@ICDSts        s              1    inz
     d isICD10         s              1    inz
     d isShowConf      s              1    inz
     d isConfmsg       s              1    inz
     d strConfmsg      s            256    inz

       // FA002R And FA003R Shared parameters
     d   qmRtCd        s              1
     d   qmSS#         s              9
     d   qmAcDt        s              7
     d   qmCo#         s              3
     d   qmSub#        s              5
     d   qmFnd         s              3
     d   qmFyr         s              3
     d   qmCase        s              7
     d   qmLsN         s             30
     d   qmFsN         s             15
     d   qmMid         s              1

       // FA002R parameters
     d   qmAdr1        s             40
     d   qmAdr2        s             40
     d   qmCty         s             25
     d   qmSt          s              2
     d   qmZip         s              9
     d   qmDtoB        s              7
     d   qmSex         s              1
     d   qmArea        s              3
     d   qmPhn#        s              7
     d   qmMsts        s              1
     d   qmWara        s              3
     d   qmWph#        s              7
     d   qmCtry        s              3
     d   qmAdr3        s             40
     d   qmCara        s              3
     d   qmCphn        s              7

       // FA003R parameters
     d   qmEmp#        s              9
     d   qmDiv         s              5
     d   qmAcTm        s              6
     d   qmDtRp        s              7
     d   qmDtLw        s              7
     d   qmAcDs        s             60
     d   qmMcon        s              1
     d   qmEntD        s              7
     d   qmRcvD        s              7
     d   qmMMI         s              7
     d   qm1a1         s              1
     d   qm10a1        s             10
     d   qmLtFl        s              1
     d   qmScl#        s             25
     d   qmMcTm        s              4
     d   qmStWk        s              2
     d   qmDeDt        s              7
     d   qmDeem        s              2
     d   qmCmNu        s              3
     d   qmCmSu        s              3
     d   qmClad        s              3
     d   qmRrn         s              9  0
     d   qmDtRe        s              7
     d   qmClSu        s              3
     d   qm#Dep        s              3
     d   qmOcDs        s             40
     d   qmHiDt        s              7  0
     d   qmEsts        s              2
     d   qmWkPt        s              2
     d   qmPrex        s              1
     d   qmAcD2        s             60
     d   qmCvFl        s              1
     d   qmMca         s              1
     d   qmMmiP        s              5  2
     d   qmOdis        s              1
     d   qmYrlx        s              4  0
     d   qmDbDt        s              7  0
     d   qmHrDy        s              3  0
     d   qmDyWk        s              1  0
     d   qmClFa        s              3
     d   qmDny         s              1
     d   qmImp         s              1
     d   qmJrSt        s              2
     d   qmPoSt        s              2

       // SA001R parameters
     d   wcmsInst      s              1    Inz(*Blanks)
     d   wcmcInst      s              1    Inz(*Blanks)
     d   wcmcInt       s              1    Inz(*Blanks)

       //FA201R parameters
     d##Co#            S              3    Inz(*Blanks)
     d##Fnd            S              3    Inz(*Blanks)
     d##Fyr            S              3    Inz(*Blanks)
     d##Case           S              7    Inz(*Blanks)
     d@@Rrn1           S              9  0 Inz(*Zeros)
     d@@Rtcd           S              1    Inz(*Blanks)
     d indSecLChk      s              1    inz
     d strExTyp        s              1    inz
     d strTyp6         s              1    inz
     d strPBMUL        s              3    inz
     d strPBMCL        s              3    inz
     d*** Beg Add *** 09/06/22 *********************************
     d inStateA1       s              2    Inz('A1')
     D screenPtrA1     s               *
     d*** End Add *** 09/06/22 *********************************

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
     D  busArea                1      3  0
     D  busPhone1              4      6  0
     D  busPhone2              7     10  0
     D  busExt                11     15  0

     D                 ds
     D  taxID#                 1     12  0
     D  fein                   1      9  0
     D  modifier              10     12  0

     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@Mode
     C                   Parm                    @@Next
     C                   Parm                    @@TabIndex
      *

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

     C     keySMDIL      Klist
     C                   Kfld                    DAicd9
     C                   Kfld                    DAlost

     C     key1WDA3L     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WDA3L     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkBtc

     C     key1WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt
     C                   Kfld                    kkEfDt

     C     key2WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt

     C     key1WMAWL1    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMAWL1    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkTrDt
     C                   Kfld                    kkTime

     C     key1WMBNL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMBNL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkSeq#

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

     C     keyWMEML      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv

     C     key1WDMSL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WDMSL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkCaMs

     C     key1WDR3L     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WDR3L     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkReWk

     C     key1WMTHL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMTHL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkTrDt
     C                   Kfld                    kkTime

     C     key1WMTIL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMTIL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkSuDt
     C                   Kfld                    kkTrTp
     C                   Kfld                    kkJudg
     C                   Kfld                    kkIssu
     C                   Kfld                    kkVerd

     C     keyWMTPL1     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key1WMWHL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMWHL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkDate
     C                   Kfld                    kkWkSt

     C     keyWMWIL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     keyWTABL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkKey

     C     keyWTGTL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkTbId
     C                   Kfld                    kkKey
     C     key1FAWHL     Klist
     C                   Kfld                    FMco#
     C                   Kfld                    FMsub#
     C                   Kfld                    FMfnd
     C                   Kfld                    FMfyr
     C                   Kfld                    FMcase
     C                   Kfld                    kkDate

     C     key2fawhl     Klist
     C                   Kfld                    FMco#
     C                   Kfld                    FMsub#
     C                   Kfld                    FMfnd
     C                   Kfld                    FMfyr
     C                   Kfld                    FMcase

     C     keyFTDLL      Klist
     C                   Kfld                    FMco#
     C                   Kfld                    kkKey

     C     keyFTGTL      Klist
     C                   Kfld                    FMco#
     C                   Kfld                    kkTbId
     C                   Kfld                    kkKey

     C     keySSYLF      Klist
     C                   Kfld                    ##KeyWc           2

      /Free

       ExSr  ProgramStart;

       // check if ICD-9 or ICD-10 codes are active
       // GetICDSts will returns '1' for ICD-9 and '2' for ICD-10
       @@ICDSts  = ' ';
       isICD10 = ' ';
       GetICDSts(CMacdt:@@ICDSts);
       If @@ICDSts = '2';
          isICD10 = '2';
       EndIf;

       isICDFirst =*Off;
       isShowConf = *Off;
       isConfmsg  = *Off;
       strConfmsg = 'There are invalid ICD codes on the claim (Press OK to ' +
        'continue or Press Cancel to go back and correct)';

       DoU scMenuRsp<>*Blanks;
          If (errorExists=*Off and addEdit=*Off and tabClick=*Off)
             Or refrshData=*On;
             MoveDbSc();
          EndIf;
          If outClientID = '114';
             isCSI = *on;
             isCSI2 = *on;
          else;
             isBase= *on;
          EndIf;

          // check if ICD-9 or ICD-10 codes are active
          // GetICDSts will returns '1' for ICD-9 and '2' for ICD-10
          @@ICDSts = ' ';
          isICD10 = ' ';
          GetICDSts(CMacdt:@@ICDSts);
          If @@ICDSts = '2';
             isICD10 = '2';
          EndIf;

          IsICD9vs   = *off;
          IsICD9ds   = *on;
          IsICD10vs  = *off;
          IsICD10ds  = *on;

          If @@ICDSts = '1';
             IsICD9vs   = *on;
             IsICD9ds   = *off;
          Else;
             IsICD10vs  = *on;
             IsICD10ds  = *off;
          Endif;

          ResetEditAdd();
          LoadTab();
          Select;
          When btnShowWin = *on;
             PopupWindow(txtShowWin:scActRRN:'CS':txtWinData);
             btnShowWin = *off;

          When tabClick=*Off;
             ChkAddEdit();
             If chgMode=*On and scMenuRsp=*Blanks and
                 addEdit=*Off And refrshData=*Off;
                errorExists=ErrorCheck();
             EndIf;
             If chgMode and scMenuRsp=*Blanks and errorExists=*Off and
                addEdit=*Off And refrshData=*Off;
                // Show soft edit confirmation window for prior ICD codes
                If (scCmsCause <> *blanks and isICD10 = '2'
                 and scCmsCaus2 = *blanks)
                 or (scCmsCaus2 <> *blanks and isICD10 <> '2'
                 and scCmsCause = *blanks);
                   isShowConf= *on;
                EndIf;

                If isShowConf = *On;
                   isConfmsg = PopTwoBtn(strConfmsg:'ICD Codes Confirmation':
                    'OK':'Cancel':425:600);
                Endif;
                If isConfmsg = *On or isShowConf = *Off;
                   isConfmsg  = *off;
                // Update Claim Data
                UpdateRecord();
                Exfmt rcdsave;
                Endif;
                isShowConf = *off;
                // Check to see If Edi Log should be generated
                If IsEdi(inRrnClm)=*On;
                   strCompany#=%EditC(CMco#:'X');
                   strGroup=%EditC(CMfnd:'X');
                   strYear=%EditC(CMfYr:'X');
                   strCase=%EditC(CMcase:'X');
                   If CMgt1='Y';
                         strEDIWork=*zeros;
                      If cmltFl='I' and svltFl<>'I';
                         strEDIWork=DateTo7(SystemDate());
                         strEDIDate=%EditC(strEDIWork:'X');
                      Else;
                         kkTrDt=*hival;
                         kkTime=*hival;
                         SetGT key2WMTHL pth00;
                         DoU THto='I' or %eof(WMTHL);
                            ReadPE key1WMTHL pth00;
                         EndDO;
                         If not %Eof(WMTHL);
                            strEDIDate=%EditC(THtrDt:'X');
                         EndIf;
                      EndIf;
                   EndIf;
                   EdiLogApi(strCompany#:strGroup:strYear:strCase:
                    strActFlag:strMessage:strReturnCod:strEDIDate);
                EndIf;
             EndIf;
          Endsl;
       EndDo;

       ExSr  ProgramEnd;

       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;

          If %Parms()>0;
             @@Next=scMenuRsp;
          EndIf;

          If %Parms()>3;
             @@tabIndex='00000';
          EndIf;

          *inlr=*on;
          Return;

       endsr;
       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

          //Load default values or passed in values
          If %parms()>0 and %Int(@@Rrn)<>0;
             inRrnClm=%Int(@@Rrn);
             inMode=@@Mode;
          Else;
             inRrnClm=41;
             inMode='C';
          EndIf;

          OutCmRrn0= inRrnClm;
          OutCmRrn1= inRrnClm;
          OutCmRrn2= inRrnClm;
          // Set the action panel mode
          scActMode=inMode;

          // Set the initial tab
          If %parms()>3;
             tabIndex=%Dec(@@TabIndex:5:0);
          Else;
             tabIndex=0;
          EndIf;

          // Access the claim record
          Chain inRRNClm dbrcdf;
             svLtFl=cmltFl;

          // Get the client id
          GetClientID(outClientID);

          // Populate hidden fields
          scActCltId=outClientID;
          scActRrn=inRrnClm;

          If CMpspa = 'E';
            scPSPA = 'Y';
          Else;
            scPSPA = 'N';
          EndIf;
          scPSPA = CMpspa ;
          scWWTC = CMwwtc ;
          if CMWDS = *Blanks;
             CMWDS = 'NNNNNNN';
          Endif;
          scWDSSu = %SUBST(CMwds:1:1);
          scWDSM  = %SUBST(CMwds:2:1);
          scWDST  = %SUBST(CMwds:3:1);
          scWDSW  = %SUBST(CMwds:4:1);
          scWDSR  = %SUBST(CMwds:5:1);
          scWDSF  = %SUBST(CMwds:6:1);
          scWDSSa = %SUBST(CMwds:7:1);
          scESID = CMesid ;

          scCO#=CMco#;
          scMod=inMode;
          If outClientID = '063';
          scChoiceOp = 'TRIM(CAKEY) || ' +  '''-'''  + ' || TRIM(CADESC) || ' +
                        ''' - ''' + ' || CAPNTS';
          Else;
          scChoiceOp = 'TRIM(CAKEY) || ' + '''-''' + ' || CADESC';
          EndIf;

          // Set the Mode
          noNameAdrC=*off;
          dspMode0=*off;
          dspMode1=*off;
          dspMode2=*off;
          dspMode3=*off;
          dspMode4=*off;
          dspModeA1=*off;
          dspModeAL=*off;
          dspModeCA=*off;
          dspModeDE=*off;
          dspModeFL=*off;
          dspModeGA=*off;
          dspModeLA=*off;
          dspModeMA=*off;
          dspModeNC=*off;
          dspModeNY=*off;
          dspModePA=*off;
          dspModeWV=*off;
          dspMode6=*off;
          shwAdd00=*On;
          shwAdd02=*On;
          shwAdd03=*On;
          shwAdd04=*On;
          tabClick=*Off;
          dspMode7=*off;
          //*** Beg Add *** 01/25/22 ****************************************
          dspModeStf = *off;
          //*** End Add *** 01/25/22 ****************************************

          Select;
          When inMode='D';
             noNameAdrC=*on;
             dspMode=*on;
             dspMode0=*on;
             dspMode1=*on;
             dspMode2=*on;
             dspMode3=*on;
             dspMode4=*on;
             dspModeA1=*on;
             dspModeAL=*on;
             dspModeCA=*on;
             dspModeDE=*on;
             dspModeFL=*on;
             dspModeGA=*on;
             dspModeLA=*on;
             dspModeMA=*on;
             dspModeNC=*on;
             dspModeNY=*on;
             dspModePA=*on;
             dspModeWV=*on;
             dspMode6=*on;
             shwAdd00=*Off;
             shwAdd02=*Off;
             shwAdd03=*Off;
             shwAdd04=*Off;
             dspMode7=*On;
             //*** Beg Add *** 01/25/22 ****************************************
             // Access User parameters to check for allowing staff code update
             dspModeStf = *on;
             Chain(n) q1user pup01;
             If %found(smupl) And U1stafUpd = 'Y';
                dspModeStf = *off;
             Endif;
             //*** End Add *** 01/25/22 ****************************************
          When inMode='C';
             chgMode=*on;
             If getChgAddrFlag(q1User)<>'Y';
                noNameAdrC=*On;
             EndIf;
          EndSl;

          // Show Colorado Fields if Jurisdiction State is CO
          colorado=*Off;
          If CMjrSt='CO';
             colorado=*On;
          EndIf;
          // Show California Fields if Jurisdiction State is CA
          california=*Off;
          If CMjrSt='CA';
             california=*On;
             californi1=*On;
          EndIf;

          // Assign Tab Headings
          froiState=%Trim(getFroiState(CMco#:CMjrSt));
          //*** Beg Chg *** 09/06/22 ***************************************
          //***If froiState<>'AL' and froiState<>'CA' and froiState<>'DE' and
          //*** froiState<>'FL' and froiState<>'GA' and froiState<>'LA' and
          //*** froiState<>'MA' and froiState<>'NC' and froiState<>'NY' and
          //*** froiState<>'PA' and froiState<>'WV';
          //***   froiState='n/a';
          //***   tabHeader='Claim Detail,Management,Medical,Benefits,Legal,' +
          //***           'IA-1' + ',Quick Diary';
          //***else;
          //***   tabHeader='Claim Detail,Management,Medical,Benefits,Legal,' +
          //***           'States-' + froiState + ',Quick Diary';
          //***EndIf;
          tabHeader='Claim Detail,Management,Medical,Benefits,Legal,FROI,' +
           'Quick Diary';
          //*** End Chg *** 09/06/22 ***************************************
          //*** Beg Add *** 09/06/22 ***************************************
          A1Height = 530;
          topA1Btns = 590;
          //*** End Add *** 09/06/22 ***************************************
          // Conditioning CSI and non-CSI fields
          If outClientID = '114';
             isCSI = *on;
             isCSI2 = *on;
             // Conditioning Alternate State Claim #
             dspAltStCl = *on;
             // Conditioning ODG Website Link
          else;
             isBase= *on;
             // Conditioning Alternate State Claim #
             dspAltStCl = *off;
             // Conditioning ODG Website Link
          EndIf;

          // Conditioning CSI and non-CSI fields based on group
          If outClientID = '114' and (CMfnd = 440 or CMfnd = 460);
             isCSIG = *on;
             If CMfnd = 440;
                ccopt1 = '093';
                ccopt2 = '094';
             else;
                ccopt1 = '100';
                ccopt2 = '101';
             EndIf;
          else;
             isCSIG = *off;
          EndIf;

          ##KeyWc = 'WC';
          Chain keySSYLF swc00;
          If %found(SSYLF) and WCFL52 = 'Y';
             dspRtwSts = *on;
          Else;
             dspRtwSts = *off;
          EndIf;

          // Conditioning LCTA and non-LCTA fields
          If outClientID = '045';
             isLCTA = *on;
          else;
             isNLCTA= *on;
          EndIf;

          // Conditioning BCI fields
          If outClientID = '063';
             isBIC = *On;
          Else;
             isBIC = *Off;
          EndIf;

          // Conditioning RMS fields
          If outClientID = '090';

             // Access User parameters
             Chain(n) q1user pup00;
             If %found(wmupl);
                // Access Claims/DARTS Control file
                Chain(n) (CMco#:CMfnd) pcx00;
                If %found(wmcxl);
                  // If PBM eligible is checked, verify if user has access
                   If CXPBME = 'Y';
                      strPBMUL = %editC(UPlvl:'X');
                      strPBMCL = %editC(CXPBMEL:'X');
                      strExTyp = '1';
                      strTyp6 = *blanks;
                      indSecLChk = '1';
                      // Security level and Type check
                      callp SecLvlChk (UPtype:strPBMUL:CXPBME1:strPBMCL:
                       strExTyp:indSecLChk:CXPBME2:CXPBME3:CXPBME4:
                       CXPBME5:strTyp6);
                      if indSecLChk = '0';
                         vsbPBME = *on;
                      else;
                         vsbPBME = *off;
                      endif;
                   EndIf;
                EndIf;
             EndIf;
          Else;
             vsbPBME = *off;
          EndIf;

             dspPriorCl = *Off;
          // Conditioning TASB fields
          If outClientID = '025';
             isTASB = *On;
             isTASB1 = *On;
             dspPriorCl = *On;
             scFlxClmHd = 'Prior Claim #';
          Else;
             isTASB = *Off;
             isTASB1 = *Off;
          EndIf;
          // Conditioning CoverRisk fields
          If outClientID = '129';
             isFCC  = *On;
             isFCC1 = *On;
          Else;
             isFCC  = *Off;
             isFCC1 = *Off;
          EndIf;
          // Conditioning LUBA fields
          If outClientID = '133';
             isLUBA = *on;
          else;
          EndIf;
          If outClientID = '136';
             scFlxClmHd = 'TPA Claim #';
             dspPriorCl = *On;
          EndIf;
       isSmsVisbl = isModEnabled('RelaySMS');
       isSmsDsbld = isSmsDisabled('CS':dbrrn:'OW':'WTCLMDTL');
       If isModEnabled('IDC');
          isIDCVisbl = *On;
          isPeDisVis = *On;
          isDisLVis = *On;
       Endif;
       If OutClientID = '152';
          isClt152 = *on;
       Else;
          isClt152 = *off;
       Endif;

       endsr;
      /End-Free
       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // ChkAddEdit - Check to see if Add/Edit indicators set off
       //===================================================================
     P ChkAddEdit      B
     D ChkAddEdit      PI
      /Free

       // Reset Indicators
       addEdit=*Off;
       posWitness=*Off;
       posPhys=*Off;
       posPart=*Off;
       posDiag=*Off;
       posWork=*Off;
       posAww=*Off;
       posBenefit=*Off;
       posEarning=*Off;
       posPaid=*Off;
       posTrial=*Off;
       posMult=*Off;
       //*** Beg Add ***** 12/06/22 *********************************
       posSettle=*Off;
       //*** End Add ***** 12/06/22 *********************************

       ReadC sflAww;
       ReadC sflBenefit;
       ReadC sflEarning;
       ReadC sflMult;
       ReadC sflPaid;
       ReadC sflTrial;
       ReadC sflWitness;
       ReadC sflWork;
       ReadC sflWrkGrd;
       //*** Beg Add ***** 12/06/22 *********************************
       ReadC sflSettle;
       //*** End Add ***** 12/06/22 *********************************

       select;
       when addAww=*On or edtAww=*On;
          If addAww=*On;
             inRrnFile=0;
          Else;
             inRrnFile=scAwwRrn;
          EndIf;
          inAddMode=addAww;
          inEdtMode=edtAww;
          cancel=editAddAww(inRrnClm:inRrnFile:inAddMode:inEdtMode);
          posAww=*On;
          addEdit=*On;
          Chain inRrnClm dbrcdf;
       when addBenefit=*On or edtBenefit=*On or delBenefit=*On;
          If addBenefit=*On;
             inRrnFile=0;
          Else;
             inRrnFile=scBeneRrn;
          EndIf;
          inAddMode=addBenefit;
          inEdtMode=edtBenefit;
          inDelMode=delBenefit;
          cancel=
           editAddBenefit(inRrnClm:inRrnFile:inAddMode:inEdtMode:inDelMode);
           posBenefit=*On;
          addEdit=*On;
       when addEarning=*On or edtEarning=*On or delEarning=*On;
          If addEarning=*On;
             inRrnFile=0;
          Else;
             inRrnFile=scEarnRrn;
          EndIf;
          inAddMode=addEarning;
          inEdtMode=edtEarning;
          inDelMode=delEarning;
          cancel=
           editAddEarning(inRrnClm:inRrnFile:inAddMode:inEdtMode:inDelMode);
          addEdit=*On;
          posEarning=*On;
       when addMult=*On or edtMult=*On;
          If addMult=*On;
             inRrnFile=0;
          Else;
             inRrnFile=scMultRrn;
          EndIf;
          inAddMode=addMult;
          inEdtMode=edtMult;
          cancel=editAddMult(inRrnClm:inRrnFile:inAddMode:inEdtMode);
          addEdit=*On;
          posMult=*On;
       when addPaid=*On or edtPaid=*On or delPaid=*On;
          If addPaid=*On;
             inRrnFile=0;
          Else;
             inRrnFile=scPaidRrn;
          EndIf;
          inAddMode=addPaid;
          inEdtMode=edtPaid;
          inDelMode=delPaid;
          cancel=
           editAddPaid(inRrnClm:inRrnFile:inAddMode:inEdtMode:inDelMode);
          addEdit=*On;
          posPaid=*On;
       when addTrial=*On or edtTrial=*On or delTrial=*On;
          If addTrial=*On;
             inRrnFile=0;
          Else;
             inRrnFile=scTrialRrn;
          EndIf;
          inAddMode=addTrial;
          inEdtMode=edtTrial;
          inDelMode=delTrial;
          cancel=editAddTrial(inRrnClm:inRrnFile:inAddMode:inEdtMode:inDelMode);
          posTrial=*On;
          addEdit=*On;
       //*** Beg Add ***** 12/06/22 ************************************
       when addSettle=*On;
          inRrnFile=0;
          inAddMode=addSettle;
          cancel=editAddSettlement(inRrnClm:inRrnFile:inAddMode);
          posSettle=*On;
          addEdit=*On;
       //*** End Add ***** 12/06/22 ************************************
       when addWitness=*On or edtWitness=*On or delWitness=*On;
          If addWitness=*On;
             inRrnFile=0;
          Else;
             inRrnFile=scWitnRrn;
          EndIf;
          inAddMode=addWitness;
          inEdtMode=edtWitness;
          inDelMode=delWitness;
          cancel=
           editAddWitness(inRrnClm:inRrnFile:inAddMode:inEdtMode:inDelMode);
          posWitness=*On;
          cancel=focActiv;
          addEdit=*On;
       when addWork=*On or edtWork=*On or delWork=*On
        or edtWrk=*On or delWrk=*On;
          If scActCltId <> '114';
             If addWork=*On;
                inRrnFile=0;
             Else;
                inRrnFile=scWorkRrn;
             EndIf;
             inAddMode=addWork;
             inEdtMode=edtWork;
             inDelMode=delWork;
             cancel=
              editAddWork(inRrnClm:inRrnFile:inAddMode:inEdtMode:inDelMode);
             addEdit=*On;
             posWork=*On;
          Else;
             If addWork=*On;
                inRrnFile=0;
             Else;
                inRrnFile=scWrkRrn;
             EndIf;
             inAddMode=addWork;
             inEdtMode=edtWrk;
             inDelMode=delWrk;
             cancel=
              editAddWrk(inRrnClm:inRrnFile:inAddMode:inEdtMode:inDelMode);
             addEdit=*On;
             posWork=*On;
          Endif;
       EndSl;

       ReadC sflDiag;
       Dow Not %Eof Or addDiag=*On;
          If addDiag=*On or edtDiag=*On or deleteDiag=*On;
             If addDiag=*On;
                inRrnFile=0;
             Else;
                inRrnFile=scDiagRrn;
             EndIf;
             inAddMode=addDiag;
             inEdtMode=edtDiag;
             inDelMode=deleteDiag;
             inCmsSub=*Off;
             If scCmsSub='Y';
                inCmsSub=*On;
             EndIf;
             cancel=editAddDiag(inRrnClm:inRrnFile:inAddMode:inEdtMode:
                                inDelMode:inCmsSub);
             addEdit=*On;
             posDiag=*On;
             Leave;
          EndIf;
          ReadC sflDiag;
       EndDo;

       ReadC sflParts;
       DoW Not %Eof Or addPart=*On;
          If addPart=*On or edtPart=*On or delPart=*On;
             If addPart=*On;
                inRrnFile=0;
             Else;
                inRrnFile=scPartRrn;
             EndIf;
             inAddMode=addPart;
             inEdtMode=edtPart;
             inDelMode=delPart;
             cancel=editAddPart(inRrnClm:inRrnFile:inAddMode:inEdtMode
                                :inDelMode);
             addEdit=*On;
             posPart=*On;
             Leave;
          EndIf;
          ReadC sflParts;
       EndDo;

       ReadC sflPhys;
       DoW Not %Eof Or addPhys=*On;
          If addPhys=*On or edtPhys=*On or delPhys=*On;
             If addPhys=*On;
                inRrnFile=0;
             Else;
                inRrnFile=scPhysRrn;
             EndIf;
             inAddMode=addPhys;
             inEdtMode=edtPhys;
             inDelMode=delPhys;
             cancel=editAddPhys(inRrnClm:inRrnFile:inAddMode:inEdtMode:
              inDelMode);
             addEdit=*On;
             posPhys=*On;
             Leave;
          EndIf;
          ReadC sflPhys;
       EndDo;

      /End-Free
     P ChkAddEdit      E
       //===================================================================
       // ChooseStates - Choose State Screen to Display
       //===================================================================
     P ChooseStates    B
     D ChooseStates    PI
      /Free

       // get the FROI Information
       froiState=GetFroiState(CMco#:CMjrSt);

       Select;
       // A1 - Data
       //*** Beg Del *** 09/06/22 *********************************************
       //***When froiState='A1';
       //*** Exfmt rcdClmDtA1;
       //*** End Del *** 09/06/22 *********************************************
       // AL - Data
       When froiState='AL';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***  Exfmt rcdClmDtAL;
          Write rcdClmDtAL;
          //*** End Chg *** 09/06/22 ******************************************
       // CA - Data
       When froiState='CA';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***  Exfmt rcdClmDtCA;
          Write rcdClmDtCA;
          //*** End Chg *** 09/06/22 ******************************************
       // DE - Data
       When froiState='DE';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtDE;
          Write rcdClmDtDE;
          //*** End Chg *** 09/06/22 ******************************************
       // FL - Data
       When froiState='FL';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtFL;
          Write rcdClmDtFL;
          //*** End Chg *** 09/06/22 ******************************************
       // GA - Data
       When froiState='GA';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtGA;
          Write rcdClmDtGA;
          //*** End Chg *** 09/06/22 ******************************************
       // LA - Data
       When froiState='LA';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtLA;
          Write rcdClmDtLA;
          //*** End Chg *** 09/06/22 ******************************************
       // MA - Data
       When froiState='MA';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtMA;
          Write rcdClmDtMA;
          //*** End Chg *** 09/06/22 ******************************************
       // NC - Data
       When froiState='NC';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtNC;
          Write rcdClmDtNC;
          //*** End Chg *** 09/06/22 ******************************************
       // NY - Data
       When froiState='NY';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtNY;
          Write rcdClmDtNY;
          //*** End Chg *** 09/06/22 ******************************************
       // PA - Data
       When froiState='PA';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtPA;
          Write rcdClmDtPA;
          //*** End Chg *** 09/06/22 ******************************************
       // WV - Data
       When froiState='WV';
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtWV;
          Write rcdClmDtWV;
          //*** End Chg *** 09/06/22 ******************************************
       Other;
          //*** Beg Chg *** 09/06/22 ******************************************
          //***Exfmt rcdClmDtNI;
          Write rcdClmDtNI;
          //*** End Chg *** 09/06/22 ******************************************
       EndSl;
       //*** Beg Add *** 09/06/22 *********************************************
       Exfmt rcdClmDtA1;

       Select;
       // AL - Data
       When froiState='AL';
          Read rcdClmDtAL;
       // CA - Data
       When froiState='CA';
          Read rcdClmDtCA;
       // DE - Data
       When froiState='DE';
          Read rcdClmDtDE;
       // FL - Data
       When froiState='FL';
          Read rcdClmDtFL;
       // GA - Data
       When froiState='GA';
          Read rcdClmDtGA;
       // LA - Data
       When froiState='LA';
          Read rcdClmDtLA;
       // MA - Data
       When froiState='MA';
          Read rcdClmDtMA;
       // NC - Data
       When froiState='NC';
          Read rcdClmDtNC;
       // NY - Data
       When froiState='NY';
          Read rcdClmDtNY;
       // PA - Data
       When froiState='PA';
          Read rcdClmDtPA;
       // WV - Data
       When froiState='WV';
          Read rcdClmDtWV;
       Other;
          Read rcdClmDtNI;
       EndSL;
       //*** End Add *** 09/06/22 *********************************************

      /End-Free
     P ChooseStates    E
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnWitness=0;
       rrnImr=0;
       clrWitness=*On;
       clrImr=*On;
       dspWitness=*Off;
       dspImr=*Off;
       write rcdClmDt0;

       rrnPhys=0;
       rrnParts=0;
       rrnDiag=0;
       clrPhys=*On;
       clrParts=*On;
       clrDiag=*On;
       dspPhys=*Off;
       dspParts=*Off;
       dspDiag=*Off;
       write rcdClmDt2;

       rrnWork=0;
       rrnAww=0;
       rrnBenefit=0;
       rrnPaid=0;
       rrnEarning=0;
       clrWork=*On;
       clrAww=*On;
       clrBenefit=*On;
       clrPaid=*On;
       clrEarning=*On;
       dspWork=*Off;
       rrnWrk=0;
       dspWrk=*Off;
       clrWrk=*On;
       dspAww=*Off;
       dspBenefit=*Off;
       dspPaid=*Off;
       dspEarning=*Off;
       write rcdClmDt3;

       rrnTrial=0;
       rrnMult=0;
       clrTrial=*On;
       clrMult=*On;
       dspTrial=*Off;
       dspMult=*Off;
       //*** Beg Add ***** 12/06/22 ***************************************
       rrnSettle=0;
       clrSettle=*On;
       dspSettle=*Off;
       //*** End Add ***** 12/06/22 ***************************************
       write rcdClmDt4;

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // ErrorCheck - Validate the screen fields.
       //===================================================================
     P ErrorCheck      B
     D ErrorCheck      PI              n
     D error           s               n   inz(*off)
     D error0          s               n   inz(*off)
     D error1          s               n   inz(*off)
     D error2          s               n   inz(*off)
     D error3          s               n   inz(*off)
     D error4          s               n   inz(*off)
     D error5          s               n   inz(*off)
     D error6          s               n   inz(*off)
     D errorC          s               n   inz(*off)
      /Free

       // Check to see If claim was already Updated
       errorC=ChkClmUpdated(inRrnClm:CMdt:CMtime);
       If errorC=*Off;

          // get the FROI Information
          froiState=GetFroiState(CMco#:CMjrSt);

          // Claim Information Error Check
          screenPtr=%Addr(claimInfo);
          error0=errorCheck0(inRrnClm:screenPtr);

          // Management Information Error Check
          screenPtr=%Addr(mgmtInfo);
          error1=errorCheck1(inRrnClm:screenPtr:mode);

          // Medical Information Error Check
          screenPtr=%Addr(medicalInfo);
          error2=errorCheck2(inRrnClm:screenPtr);

          // Benefit Information Error Check
          screenPtr=%Addr(benefitInfo);
          //*** Beg Chg *** 04/10/24 ***************************************
          //***error3=errorCheck3(inRrnClm:screenPtr);
          screenPtr2=%Addr(claimInfo);
          error3=errorCheck3(inRrnClm:screenPtr:screenPtr2);
          //*** End Chg *** 04/10/24 ***************************************

          If error3 = '0';
          // Access the claim record
             Chain inRRNClm dbrcdf;
          Endif;

          // Legal Information Error Check
          screenPtr=%Addr(legalInfo);
          error4=errorCheck4(inRrnClm:screenPtr);

          // State Information Error Check
          Select;
          When froiState='A1';
             screenPtr=%Addr(a1Info);
          When froiState='AL';
             screenPtr=%Addr(alInfo);
          When froiState='CA';
             screenPtr=%Addr(caInfo);
          When froiState='DE';
             screenPtr=%Addr(deInfo);
          When froiState='FL';
             screenPtr=%Addr(flInfo);
          When froiState='GA';
             screenPtr=%Addr(gaInfo);
          When froiState='LA';
             screenPtr=%Addr(laInfo);
          When froiState='MA';
             screenPtr=%Addr(maInfo);
          When froiState='NC';
             screenPtr=%Addr(ncInfo);
          When froiState='NY';
             screenPtr=%Addr(nyInfo);
          When froiState='PA';
             screenPtr=%Addr(paInfo);
          When froiState='WV';
             screenPtr=%Addr(wvInfo);
          EndSl;
          //*** Beg Chg *** 09/06/22 ******************************************
          //***error5=errorCheck5(inRrnClm:screenPtr:froiState);
             screenPtrA1=%Addr(a1Info);
             error5=errorCheck5(inRrnClm:ScreenPtrA1:ScreenPtr:froiState);
          //*** End Chg *** 09/06/22 ******************************************

          // Quick Diary Error Check
          screenPtr=%Addr(diaryInfo);
          error6=errorCheck6(inRrnClm:screenPtr);

       EndIf;

       Select;
       When error0=*on;
          tabIndex=0;
       When error1=*on;
          tabIndex=1;
       When error2=*on;
          tabIndex=2;
       When error3=*on;
          tabIndex=3;
       When error4=*on;
          tabIndex=4;
       When error5=*on;
          tabIndex=5;
       When error6=*on;
          tabIndex=6;
       EndSl;

       error=*Off;
       If error0 Or error1 Or error2 Or error3 Or error4 Or
        error5 Or error6 Or errorC;
          error=*On;
       EndIf;

       Chain CMss# pcd00;
       // Check Name Change if All other edits have been passed
       If error=*Off;
          If GetNmAdChgRmCd()<>*Blanks;
             // Check for change in name or address
             If CMlsn<>*Blanks;  // If Blank this must be add mode
                If CMlsn<>scLsn Or CMfsn<>scFsn Or CMmid<>scMid Or
                 scClmAdr1<>CDadr1 Or scClmadr2<>CDadr2 Or
                 scClmCty<>CDcty Or scClmSt<>CDst Or scClmZip<>CDzip;
                   // What changed? Name And/Or Address
                   chgAddress=*Blanks;
                   change='N';
                   If scClmAdr1<>CDadr1 Or scClmAdr2<>CDadr2 Or
                      scClmCty<>CDcty Or scClmSt<>CDst Or scClmZip<>CDzip;
                      chgAddress=%Trim(CDadr1) + ' ' + %Trim(CDadr2) +  ' ' +
                       %Trim(CDcty) + ' ' + %Trim(CDst) + ' ' +
                       %Subst(%EditC(CDzip:'X'):1:5) + ' ' +
                       %Subst(%EditC(CDzip:'X'):1:5);
                      If CMlsn<>scLsn Or CMfsn<>scFsn Or CMmid<>scMid;
                         change='Y';
                      EndIf;
                   EndIf;
                   fein#=0;
                   modifier#=0;
                   remarkCode=GetNmAdChgRmCd();
                   WriteAutoDart(CMco#:CMfnd:CMfYr:CMcase:fein#:
                    modifier#:remarkCode:returnCode:chgAddress:change);
                   If returnCode<>'Y';
                      error=*On;
                   Else;
                      // Write Name Change History Record
                      WriteNameHist(scLsn:scFsn:scMid:scClmAdr1:scClmAdr2:
                       scClmCty:scClmSt:scClmZip:scClmEmAd);
                   EndIf;
                EndIf;
             Endif;
          EndIf;
       EndIf;

       If error=*Off;
          // Create Change Name Diary
          If GetNameChgRmCd()<>*Blanks;
             // Check for change in name or address
             If CMlsn<>*Blanks;  // If Blank this must be add mode
                If CMlsn<>scLsn Or CMfsn<>scFsn Or CMmid<>scMid;
                   // Write Name Change Diary
                   q#key=%EditC(CMco#:'X') + %EditC(CMfnd:'X') +
                    %EditC(CMfYr:'X') + %EditC(CMcase:'X');
                   q#comm='Old claimant name: ' + %Trim(CMlsn) + ', ' +
                    %Trim(CMfsn);
                   If CMmid<>*Blanks;
                      q#comm=%Trim(q#comm) + ' ' + CMmid + '.';
                   EndIf;
                   q#cDat=%EditC(DateTo8(systemDate()):'X');
                   q#user=Q1user;
                   q#pgm=Q1pgm;
                   q#iPgm=Q1pgm;
                   q#rmcd=GetNameChgRmCd();
                   WriteDarts(q#key:q#comm:q#cDat:q#dcId:q#pNam:q#secl:q#user:
                    q#pgm:q#fold:q#diFl:q#iPgm:q#fl01:q#fl02:q#rmCd:q#seq#:
                    q#sbyl:q#com2:q#apId:q#iDoc:q#iQue:q#imgF:q#iPth:q#pIdx:
                    q#qPty:q#dcTp:q#csId:q#code);
                   type='C';
                   returnCode=' ';
                   UpdateName(type:inRrnClm:returnCode);
                EndIf;
             EndIf;
          EndIf;
       EndIf;

       If error=*On;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheck      E
       //===================================================================
       // Get Name Change Remark Code
       //===================================================================
     P GetNameChgRmCd  B                   Export
     D GetNameChgRmCd  PI             5
     D remarkCode      s              5    Inz(*Blanks)
      /Free

       Chain keyWMCXL pcx00;
       If %Found(WMCXL);
          eval remarkCode=CXrcnc;
       EndIf;

       Return remarkCode;

      /End-Free
     P GetNameChgRmCd  E
       //===================================================================
       // Get Name/Addres Change Remark Code
       //===================================================================
     P GetNmAdChgRmCd  B                   Export
     D GetNmAdChgRmCd  PI             5
     D remarkCode      s              5    Inz(*Blanks)
      /Free

       Chain keyWMCXL pcx00;
       If %Found(WMCXL);
          eval remarkCode=CXnmAd;
       EndIf;

       Return remarkCode;

      /End-Free
     P GetNmAdChgRmCd  E
       //===================================================================
       // LoadAWW - Loads the AWW History Data into Aww Grid
       //===================================================================
     P LoadAww         B
     D LoadAww         PI
      /Free

       clrAww=*Off;
       dspAww=*On;

       kkTrDt=*Hival;
       kkTime=*Hival;
       SetGt key2WMAWL1 paw01;
       ReadPe key1WMAWL1 paw01;
       DoW Not %Eof(WMAWL1);
          scAwwG=AWaww;
          scCompRatG=AWcrt;
          scTranDtG=dtMDYY(AWtrDt);
          kkEfDt=CMacDt;

          scBenTyp = AWbr2;
          Setgt (CMco#:CMfnd:CMjrst:scBenTyp:kkEfdt) PR101;
          Readpe (CMco#:CMfnd:CMjrst:scBenTyp) PR101;
          If not %eof(WDR1L1);
             scCalcRatG = AWaww * R1rate/100;
          Endif;

          SetGt key1WDRBL prb00;
          ReadPe key2WDRBL prb00;
          If not %Eof(WDRBL);
             If %eof(WDR1L1);
                scCalcRatG=AWaww * RBcmpr / 100;
             Endif;

             If scCalcRatG<RBmnrt;
                scCalcRatG=RBmnrt;
             EndIf;
             If scCalcRatG>RBpacp;
                scCalcRatG=RBpacp;
             EndIf;
             scMaxRateG=RBpacp;

          EndIf;

          If %eof(WDR1L1) and %eof(WDRBL);
             scCalcRatG=0;
             scMaxRateG=0;
          Endif;
          shwAwwLnk=*On;
          If dspMode3=*On;
             shwAwwLnk=*Off;
          EndIf;

          scAwwEfDtG=dtMDYY(AWefDt);
          scAwwRrn=WMAWPrrn;
          edtAww=*Off;

          scAwPcrG = AwPcr;

             rrnAww=rrnAww+1;
             write sflAww;
          ReadPe key1WMAWL1 paw01;
       EndDo;
       // Load pre-injury wage screen field in case it has been updated.
       If scWagePre = 0;
          scWagePre = CMprWW;
       Endif;

      /End-Free
     P LoadAww         E
       //===================================================================
       // LoadBenefit - Loads the Benefit Adjustment Data into Benefit Grid
       //===================================================================
     P LoadBenefit     B
     D LoadBenefit     PI
      /Free

       clrBenefit=*Off;
       dspBenefit=*On;

       Chain key1WMBNL pbn00;
       If %Found(WMBNL);
          DoU %Eof(WMBNL);
             scStrDateG=dtMDYY(BNstDt);
             scAdjCodeG=%Subst(BNaCod:1:1);

             kkTbId='BADJ';
             kkKey=%Subst(BNaCod:1:1);
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scAdjCdDsG=GTdesc;
             Else;
                scAdjCdDsG=*Blanks;
             EndIf;

             scPmtCodeG=%Subst(BNaCod:2:3);

             kkTbId='PADJ';
             kkKey=%Subst(BNaCod:2:3);
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scPmtCdDsG=GTdesc;
             Else;
                scPmtCdDsG=*Blanks;
             EndIf;

             scBenAmtG=BNamt;
             scEndDateG=dtMDYY(BNenDt);
             scAdjStsG=BNstat;
             scBeneRrn=WMBNPrrn;

             shwBeneLnk=*On;
             If dspMode3=*On;
                shwBeneLnk=*Off;
             EndIf;

             edtBenefit=*Off;
             delBenefit=*Off;
             rrnBenefit=rrnBenefit+1;
             write sflBenefit;
             ReadE key1WMBNL pbn00;
          EndDo;
       EndIf;

      /End-Free
     P LoadBenefit     E
       //===================================================================
       // LoadBodyPart - Loads the Body Part Data into Part Grid
       //===================================================================
     P LoadBodyPart    B
     D LoadBodyPart    PI
      /Free

       clrParts=*Off;
       dspParts=*On;

       Chain key1WMBYL pby00;
       If %Found(WMBYL);
          DoU %Eof(WMBYL);
             scPartS=BYkey;
             kkKey=BYkey;
             kkTbid = 'PART';
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scPartDscS=GTdesc;
             Else;
                scPartDscS=*Blanks;
             EndIf;
             scImpairS=BYpct;
             scPartStsS=BYstat;
             scPartSeq=BYseq#;
             scPartRrn=WMBYPrrn;
             kkTbId='PLOC';
             kkKey=BYiMlC;
             Chain keyWTGTL pgt00;
             If %Found(wtgtl);
                scLoc=BYiMlC + ' - ' + %Trim(GTdesc);
             Endif;

             shwPartLnk=*On;
             If dspMode2=*On;
                shwPartLnk=*Off;
             EndIf;

             delPart=*Off;
             edtPart=*Off;
             rrnParts=rrnParts+1;
             write sflParts;
             ReadE key1WMBYL pby00;
          EndDo;
       EndIf;

      /End-Free
     P LoadBodyPart    E
       //===================================================================
       // LoadDiagCode - Loads the Diagnosis Data into Diagnosis Grid
       //===================================================================
     P LoadDiagCode    B
     D LoadDiagCode    PI
      /Free

       clrDiag=*Off;
       dspDiag=*On;
       isShowConf= *off;

       Chain key1SADAL pda00;
       If %Found(SADAL);
          DoU %Eof(SADAL);
             scDiagS=DAicd9;
             Chain keySMDIL pdi00;
             If %Found(SMDIL);
                scDescS=DIdsc1;
             Else;
                scDescS=*Blanks;
             EndIf;
             If scDiags = *blanks;
                scDiagS = DAicd1001;

                Chain (DAicd1001:DAlost) pic00;
                If %found(WMICD10L);
                   scDescS=ICdsc1;
                Else;
                   scDescS=*Blanks;
                EndIf;
             EndIf;
             scPrimS=DAprim;
             scTypeS=DAlosT;
             scDiagSeq=DAseq#;
             scDiagRrn=SADAPrrn;

             shwDiagLnk=*On;
             If dspMode2=*On;
                shwDiagLnk=*Off;
             EndIf;

             edtDiag=*Off;
             deleteDiag=*Off;
             rrnDiag=rrnDiag+1;
             If rrnDiag=1;
                selDiag=*On;
             Else;
                selDiag=*Off;
             EndIf;
             showExclm = *off;
             If (DAicd9 <> *blanks and isICD10 = '2')
              or (DAicd1001 <> *blanks and isICD10 <> '2');
                showExclm = *on;
                isShowConf= *on;
             EndIf;
             write sflDiag;
             ReadE key1SADAL pda00;
          EndDo;
       EndIf;

       // Build the ODG Website Link


      /End-Free
     P LoadDiagCode    E
       //===================================================================
       // LoadEarnings - Loads the Reduced Earnings Data into Earnings Grid
       //===================================================================
     P LoadEarnings    B
     D LoadEarnings    PI
      /Free

       clrEarning=*Off;
       dspEarning=*On;

       Chain key1WDR3L pr300;
       If %Found(WDR3L);
          DoU %Eof(WDR3L);
             scWeekNumG=R3reWk;
             scActRedG=R3are;
             scDeemRedG=R3dre;
             scEarnRrn=WDR3Prrn;

             shwEarnLnk=*On;
             If dspMode3=*On;
                shwEarnLnk=*Off;
             EndIf;

             delEarning=*Off;
             edtEarning=*Off;
             rrnEarning=rrnEarning+1;
             write sflEarning;
             ReadE key1WDR3L pr300;
          EndDo;
       EndIf;

      /End-Free
     P LoadEarnings    E
       //===================================================================
       // LoadImr - Loads the I/M/R Data into Grid
       //===================================================================
     P LoadImr         B
     D LoadImr         PI
      /Free

       clrImr=*Off;
       dspImr=*On;

       Chain key1WMTHL pth00;
       If %Found(WMTHL);
          DoU %Eof(WMTHL);
             scImrTrDt=dtMDYY(THtrDt);
             scTranFrom=*Blanks;
             scTranTo=*Blanks;

             Select;
             When THfrom='I';
                scTranFrom='Indemnity';
             When THfrom='M';
                scTranFrom='Medical Only';
             When THfrom='R';
                scTranFrom='Record Only';
             EndSl;

             Select;
             When THto='I';
                scTranTo='Indemnity';
             When THto='M';
                scTranTo='Medical Only';
             When THto='R';
                scTranTo='Record Only';
             EndSl;

             rrnImr=rrnImr+1;
             write sflImr;
             ReadE key1WMTHL pth00;
          EndDo;
       EndIf;

      /End-Free
     P LoadImr         E
       //===================================================================
       // LoadMult - Loads the Multiple Settlement Data into Grid
       //===================================================================
     P LoadMult        B
     D LoadMult        PI
      /Free

       clrMult=*Off;
       dspMult=*On;

       Chain key1WDMSL pms00;
       If %Found(WDMSL);
          DoU %Eof(WDMSL);

             kkTbId='CAMS';
             kkKey=MScaMs;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scMthSettO=GTdesc;
             Else;
                scMthSettO=*Blanks;
             EndIf;

             scTpocAmtO=MStpoA;
             scTpocDtO=CvtToDate(MStpoD);
             scDlayDtO=CvtToDate(MSdlyD);
             scMultRrn=WDMSPrrn;

             shwMultLnk=*On;
             If dspMode4=*On;
                shwMultLnk=*Off;
             EndIf;

             edtMult=*Off;
             rrnMult=rrnMult+1;
             write sflMult;
             ReadE key1WDMSL pms00;
          EndDo;
       EndIf;

      /End-Free
     P LoadMult        E
       //===================================================================
       // LoadEmpPaid - Loads the Employer Paid Data into Paid Grid
       //===================================================================
     P LoadEmpPaid     B
     D LoadEmpPaid     PI
      /Free

       clrPaid=*Off;
       dspPaid=*On;

       Chain key1WDA3L pa300;
       If %Found(WDA3L);
          DoU %Eof(WDA3L);

             kkTbId='PADJ';
             kkKey=A3btc;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scEmpPyCdG=A3btc + '-' + GTdesc;
             Else;
                scEmpPyCdG=A3btc;
             EndIf;

             scTotPaidG=A3Paid;
             scEmStrDtG=CvtToDate(A3stDt);
             scEmEndDtG=CvtToDate(A3enDt);
             scPaidRrn=WDA3Prrn;

             shwPaidLnk=*On;
             If dspMode3=*On;
                shwPaidLnk=*Off;
             EndIf;

             delPaid=*Off;
             edtPaid=*Off;
             rrnPaid=rrnPaid+1;
             write sflPaid;
             ReadE key1WDA3L pa300;
          EndDo;
       EndIf;

      /End-Free
     P LoadEmpPaid     E
       //===================================================================
       // LoadTab - Load Tab Information
       //===================================================================
     P LoadTab         B
     D LoadTab         PI
      /Free
          Select;
          When TabIndex = 0;
             tabHeight=1450;
             aPnlHeight=tabHeight + 60;
             Write rcdClmPnl;
             Write rcdClmDtl;
             ExFmt rcdClmDt0;
             Read rcdClmPnl;
             Read rcdClmDtl;
          When TabIndex = 1;
             tabHeight=920;
             aPnlHeight=tabHeight + 60;
             Write rcdClmPnl;
             Write rcdClmDtl;
             ExFmt rcdClmDt1;
             Read rcdClmPnl;
             Read rcdClmDtl;
          When TabIndex = 2;
             tabHeight=1250;
             aPnlHeight=tabHeight + 60;
             // Display Prior ICD code Information Window
             If isICDFirst =*Off;
                isICDFirst =*On;
                isICDsave =*Off;
                isICDsave = ShowICDWdw(inRrnclm:@@ICDSts);
             Endif;
             If isICDsave =*On;
                isICDsave =*Off;
                refrshData=*On;
             Else;
             Write rcdClmPnl;
             Write rcdClmDtl;
             ExFmt rcdClmDt2;
             Read rcdClmPnl;
             Read rcdClmDtl;
             Endif;
          When TabIndex = 3;
             tabHeight=1540;
             aPnlHeight=tabHeight + 60;
             Write rcdClmPnl;
             Write rcdClmDtl;
             ExFmt rcdClmDt3;
             Read rcdClmPnl;
             Read rcdClmDtl;
          When TabIndex = 4;
             //*** Beg Chg ***** 12/06/22 **********************************
             //***tabHeight=900;
             tabHeight=1140;
             //*** End Chg ***** 12/06/22 **********************************
             aPnlHeight=tabHeight + 60;
             Write rcdClmPnl;
             Write rcdClmDtl;
             ExFmt rcdClmDt4;
             Read rcdClmPnl;
             Read rcdClmDtl;
          When TabIndex = 5;
             //*** Beg Chg *** 09/06/22 ************************************
             //***tabHeight=910;
             Select;
             when froiState = 'NY';
                tabHeight = 1180;
             when froiState = 'PA';
                tabHeight = 850;
             when froiState = 'n/a';
                tabHeight = 600;
             Other;
                tabHeight = 910;
             Endsl;
             //*** End Chg *** 09/06/22 ************************************
             aPnlHeight=tabHeight + 60;
             Write rcdClmPnl;
             Write rcdClmDtl;

             // Choose State Screen
             ChooseStates();

             Read rcdClmPnl;
             Read rcdClmDtl;
          When TabIndex = 6;
             tabHeight=860;
             aPnlHeight=tabHeight + 60;
             Write rcdClmPnl;
             Write rcdClmDtl;
             ExFmt rcdClmDt6;
             Read rcdClmPnl;
             Read rcdClmDtl;
          EndSl;
      /End-Free
     P LoadTab         E
       //===================================================================
       // LoadTreatPhys - Loads the Treating Phys Data into Diagnosis Grid
       //===================================================================
     P LoadTreatPhys   B
     D LoadTreatPhys   PI
      /Free

       clrPhys=*Off;
       dspPhys=*On;

       Chain keyWMTPL1 ptp01;
       If %Found(WMTPL1);
          DoU %Eof(WMTPL1);
             scFrDtS=dtMDYY(TPfrDt);
             scToDtS=dtMDYY(TPtoDt);
             scPhysNamS=TPphyN;
             kkTbId='PHCD';
             kkKey=TPpRol;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scRoleS=%Trim(GTdesc);
             Else;
                scRoleS=TPpRol;
             EndIf;
             scClinicS=TPclin;
             scPhysEmlO=TPemAd;
             scPhysEmTO=TPemAd;
             If scPhysEmTO=*Blanks;
                scPhysEmTO=noEmail;
             EndIf;
             scPhysSeq=TPseq#;
             scPhysRrn=WMTPPrrn;

             shwPhysLnk=*On;
             If dspMode3=*On;
                shwPhysLnk=*Off;
             EndIf;

             delPhys=*Off;
             edtPhys=*Off;
             rrnPhys=rrnPhys+1;
             If rrnPhys=1;
                selPhys=*On;
             Else;
                selPhys=*Off;
             EndIf;
             write sflPhys;
             ReadE keyWMTPL1 ptp01;
          EndDo;
       EndIf;

      /End-Free
     P LoadTreatPhys   E
       //*** Beg Add ***** 12/06/22 *******************************************
       //===================================================================
       // LoadSettlement - Load the Settlement Information Data into Grid
       //===================================================================
     P LoadSettlement  B
     D LoadSettlement  PI
      /Free

       clrSettle=*Off;
       dspSettle=*On;

       Setgt  (CMco#:CMfnd:CMfyr:CMcase) pst00;
       Readpe (CMco#:CMfnd:CMfyr:CMcase) pst00;
       DoW Not %Eof(WDSTLTRKL);
          scCurAuth = STCurSAut;                 //Current Authority
          scAAprDt = CvtToDate(STAutAprDt);      //Date Authority Approved date
          scCurDmnd = STCurDOC;                  //Current Demand from Opposing
          scDmndDt = CvtToDate(STDocDt);         //Date of Demand
          scUpdateDt = CvtToDate(STDt);          //Date of Update
          scTmOfUpdt = %Time(STtime);            //Time of Update

          scSetlRrn=WDstltrkrrn;
          rrnSettle=rrnSettle+1;
          Write sflSettle;
          Readpe (CMco#:CMfnd:CMfyr:CMcase) pst00;
       EndDo;

      /End-Free
     P LoadSettlement  E
       //*** End Add ***** 12/06/22 *******************************************
       //===================================================================
       // LoadTrial - Loads the Trial Information Data into Grid
       //===================================================================
     P LoadTrial       B
     D LoadTrial       PI
      /Free

       clrTrial=*Off;
       dspTrial=*On;

       Chain key1WMTIL pti00;
       If %Found(WMTIL);
          DoU %Eof(WMTIL);
             scSuitDtO=dtMDYY(TIsuDt);

             kkTbId='TRTP';
             kkKey=TItrTP;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scTrTypeO=GTdesc;
             Else;
                scTrTypeO=*Blanks;
             EndIf;

             kkTbId='JUDG';
             kkKey=TIjudg;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scJudgeO=GTdesc;
             Else;
                scJudgeO=*Blanks;
             EndIf;

             kkTbId='ISSU';
             kkKey=TIissu;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scLegIssO=GTdesc;
             Else;
                scLegIssO=*Blanks;
             EndIf;

             kkTbId='CLVL';
             kkKey=TIcLvl;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scCrtLvlO=GTdesc;
             Else;
                scCrtLvlO=*Blanks;
             EndIf;

             kkTbId='VERD';
             kkKey=TIverd;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scVerdictO=GTdesc;
             Else;
                scVerdictO=*Blanks;
             EndIf;

             scVerdDtO=dtMDYY(TIveDt);

             Select;
             When TIpeti='N';
                scPetitO='Petition has not been filed.';
             When TIpeti='P';
                scPetitO='Petition on file.';
             When TIpeti='Y';
                scPetitO='Petition has been filed.';
             Other;
                scPetitO=' ';
             EndSl;

             scTrialRrn=WMTIPrrn;

             shwTriaLnk=*On;
             If dspMode4=*On;
                shwTriaLnk=*Off;
             EndIf;

             edtTrial=*Off;
             delTrial=*Off;
             rrnTrial=rrnTrial+1;
             write sflTrial;
             ReadE key1WMTIL pti00;
          EndDo;
       EndIf;

      /End-Free
     P LoadTrial       E
       //===================================================================
       // LoadWitness - Loads the Witness Data into Witness Grid
       //===================================================================
     P LoadWitness     B
     D LoadWitness     PI
      /Free

       //rrnWitness=0;
       //clrWitness=*On;
       //dspWitness=*Off;
       //write rcdClmDt0;
       clrWitness=*Off;
       dspWitness=*On;

       Chain keyWMWIL pwi00;
       If %Found(WMWIL);
          DoU %Eof(WMWIL);
             scWitnName=WIname;
             scWitEmad=WIemAd;
             If WIemAd=*Blanks;
                scWitEmadT=noEmail;
             Else;
                scWitEmadT=WIemAd;
             EndIf;

             busPhoneNum=WIbPh#;

             If busArea<>0;
                scBusPhone='(' + %EditC(busArea:'X') + ') ' +
                 %EditC(busPhone1:'X') + '-' + %EditC(busPhone2:'X');
             Else;
                scBusPhone=%EditC(busPhone1:'X') + '-' +
                 %EditC(busPhone2:'X');
             EndIf;

             If busExt<>0;
                scBusPhone=%Trim(scBusPhone) + ' ext. ' +
                 %Trim(%EditC(busExt:'4'));
             EndIf;

             scWitnName=WIname;
             scWitnRrn=WMWIPrrn;

             shwWitnLnk=*On;
             If dspMode0=*On;
                shwWitnLnk=*Off;
             EndIf;

             delWitness=*Off;
             edtWitness=*Off;
             rrnWitness=rrnWitness+1;
             write sflWitness;
             ReadE keyWMWIL pwi00;
          EndDo;
       EndIf;

      /End-Free
     P LoadWitness     E
       //===================================================================
       // LoadWork - Loads the Work History Data into Work History Grid
       //===================================================================
     P LoadWork        B
     D LoadWork        PI
      /Free

       clrWork=*Off;
       dspWork=*On;

       Chain key1WMWHL pwh00;
       If %Found(WMWHL);
          DoU %Eof(WMWHL);
             scWorkDtG=dtMDYY(WHdate);
             scWorkStsG=WHwkSt;

             Select;
             When WHwkSt='E';
                scWorkStsG=%Trim(scWorkStsG) + ' - Release to RTW';
             When WHwkSt='L';
                scWorkStsG=%Trim(scWorkStsG) + ' - Start Losing Time';
             When WHwkSt='R';
                scWorkStsG=%Trim(scWorkStsG) + ' - Returned to Work';
             EndSl;

             kkTbId='RTWQ';
             kkKey=WHrtwQ;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL);
                scRtwQuaG=%Trim(WHrtwQ) + ' - ' + GTdesc;
             Else;
                scRtwQuaG=WHrtwQ;
             EndIf;

             scWorkRrn=WMWHPrrn;
             delWork=*Off;
             edtWork=*Off;

             shwWorkLnk=*On;
             If dspMode2=*On;
                shwWorkLnk=*Off;
             EndIf;
             scLastDay = dtMDYY(WHcDlW);
             scCdEk = dtMDYY(WHcDeK);
             scCdAk = dtMDYY(WHcDaK);

             rrnWork=rrnWork+1;
             write sflWork;
             ReadE key1WMWHL pwh00;
          EndDo;
       EndIf;

      /End-Free
     P LoadWork        E
       //===================================================================
       // LoadWrkGrd - Loads the Work History Data for client 114
       //===================================================================
     P LoadWrkGrd      B
     D LoadWrkGrd      PI
      /Free

       Open fmcmp;
       Open ftgtl;
       Open ftdll;
       Open fawhl;
       clrWrk=*Off;
       dspWrk=*On;
       kkdate = *hival;
       scWorkCde=*blanks;
       scWorkDtc= ZeroDate;
       scLvlDuty=*blanks;
       sclvlDesc=*blanks;
       ##Co#=%editc(CMco#:'X');
       ##Fnd=%editc(CMfnd:'X');
       ##Fyr=%editc(CMfyr:'X');
       ##Case=%editc(CMcase:'X');

       GetRrn(##Co#:##Fnd:##Fyr:##Case:@@Rrn1:@@Rtcd);
       If @@Rtcd <> 'F';
          Chain @@Rrn1 pcm0f;
          Setll key1FAWHL pwh0f;
          Reade key2FAWHL pwh0f;
          DoW not %Eof(FAWHL);
             kkTbid='WKCD';
             kkkey=whCode;
             Chain keyFTGTL pgt0f;
             If %found(FTGTL);
                scWorkDesc=gtDesc;
             Else;
                scWorkDesc=*blanks;
             Endif;
             scWorkcde=whcode;
             scWorkDtC=CvtToDate(WHwkdt);
             scLvlDuty=WHlvl;

             scLvlDesc=*blanks;
             If Whlvl <> *blanks;
                kkkey = whlvl;
                Chain keyFTDLL pdl00;
             If %found(FTDLL);
                scLvlDesc=dlDesc;
             Endif;
             Endif;

             scWrkRrn=FAWHPrrn;
             delWrk=*Off;
             edtWrk=*Off;

             shwWrkLnk=*On;
             If dspMode2=*On;
                shwWrkLnk=*Off;
             EndIf;

             rrnWrk=rrnWrk+1;
             write sflWrkGrd;
             ReadE key2FAWHL pwh0f;
          EndDo;
       EndIf;
       Close fmcmp;
       Close ftgtl;
       Close ftdll;
       Close fawhl;

      /End-Free
     P LoadWrkGrd      E
       //===================================================================
       // MoveDbSc - Move Database fields to Screen fields
       //===================================================================
     P MoveDbSc        B
     D MoveDbSc        PI
      /Free

       // Access the claim record
       Chain inRRNClm dbrcdf;

       // Load The Tab Panels <================

       // Loads Action Panel Fields
       LoadActPan();

       // Load Claim Info Panel - Tab 1
       screenPtr=%Addr(claimInfo);
       LoadClmInfo(inRrnClm:screenPtr);

       // Load Management Info Panel - Tab 2
       screenPtr=%Addr(mgmtInfo);
       LoadMgmtInfo(inRrnClm:screenPtr);

       // Load Medical Info Panel - Tab 3
       screenPtr=%Addr(medicalInfo);
       LoadMedInfo(inRrnClm:screenPtr);

       // Load Benefit Info Panel - Tab 4
       screenPtr=%Addr(benefitInfo);
       LoadBenInfo(inRrnClm:screenPtr);

       // Load Legal Info Panel - Tab 5
       screenPtr=%Addr(legalInfo);
       LoadLegInfo(inRrnClm:screenPtr);

       // get the FROI Information
       froiState=GetFroiState(CMco#:CMjrSt);

       // Load State Info Panel - Tab 6

       //*** Beg Add *** 09/06/22 ********************************************
       // Turn off indicator for buttons
       addCanVis=*Off;
       a1LineVis = *On;
       //*** End Add *** 09/06/22 ********************************************

       Select;
       When froiState='A1';
          screenPtr=%Addr(a1Info);
          //*** Beg Add *** 09/06/22 *******************************************
          a1LineVis = *Off;
          //*** End Add *** 09/06/22 *******************************************
       When froiState='AL';
          screenPtr=%Addr(alInfo);
       When froiState='CA';
          screenPtr=%Addr(caInfo);
       When froiState='DE';
          screenPtr=%Addr(deInfo);
       When froiState='FL';
          screenPtr=%Addr(flInfo);
       When froiState='GA';
          screenPtr=%Addr(gaInfo);
       When froiState='LA';
          screenPtr=%Addr(laInfo);
       When froiState='MA';
          screenPtr=%Addr(maInfo);
       When froiState='NC';
          screenPtr=%Addr(ncInfo);
       When froiState='NY';
          screenPtr=%Addr(nyInfo);
       When froiState='PA';
          screenPtr=%Addr(paInfo);
       When froiState='WV';
          screenPtr=%Addr(wvInfo);
       Other;
          screenPtr=%Addr(niInfo);
       EndSl;

       If froiState='n/a';
          inState='na';
       Else;
          inState=froiState;
       EndIf;

       LoadStateInfo(inRrnClm:screenPtr:inState);
       //*** Beg Add *** 09/06/22 ********************************************
       // Always load A1 if froi state <> A1 and not na
       If inState <> 'A1';
          screenPtr=%Addr(a1Info);
          LoadStateInfo(inRrnClm:screenPtr:inStateA1);
       Endif;
       //*** End Add *** 09/06/22 ********************************************

       // Load Quick Diary Panel - Tab 7
       screenPtr=%Addr(diaryInfo);
       LoadQuickDiary(inRrnClm:screenPtr);

       // Load The Grids <==================

       // Clear Grids
       ClrSubFiles();

       // Load AWW History Grid
       //
       LoadAww();

       // Load Benefit Adjustment Grid
       //
       LoadBenefit();

       // Load Body Part Grid
       //
       LoadBodyPart();

       // Load Diagnosis Grid
       LoadDiagCode();

       // Load Reduced Earnings Grid
       LoadEarnings();

       // Load Employer Paid Grid
       //
       LoadEmpPaid();

       // Load Multiple Settlement Grid
       LoadMult();

       // Load Treating Physician Grid
       LoadTreatPhys();

       // Load Trial Information Grid
       LoadTrial();

       //*** Beg Add ***** 12/06/22 ***************************************
       // Load Settlement Information Grid
       LoadSettlement();
       //*** End Add ***** 12/06/22 ***************************************

       // Load Work History Grid
        If scActCltId = '114';
           dspWrkGrdC = *On;
           dspWrkGrd  = *Off;
           LoadWrkGrd();
        Else;
          dspWrkGrdC = *Off;
          dspWrkGrd  = *On;
          LoadWork();
        Endif;

       // Load I/M/R History Grid
       LoadImr();

       // Load Witness Grid
       LoadWitness();

      /End-Free
     P MoveDbSc        E
       //===================================================================
       // ResetEditAdd - Reset Edit Add Indicators
       //===================================================================
     P ResetEditAdd    B
     D ResetEditAdd    PI
      /Free


       If addEdit=*On;

          // clrSubFiles();

          // Load Body Part Grid
          If addPart=*On Or edtPart=*On Or delPart=*On;
             rrnParts=0;
             clrParts=*On;
             dspParts=*Off;
             write rcdClmDt2;
             LoadBodyPart();
          EndIf;

          // Load Diagnosis Grid
          If addDiag=*On Or edtDiag=*On or deleteDiag=*On;
             rrnDiag=0;
             clrDiag=*On;
             dspDiag=*Off;
             write rcdClmDt2;
             LoadDiagCode();
          EndIf;

          // Load Multiple Settlement Grid
          If addMult=*On Or edtMult=*On;
             rrnMult=0;
             clrMult=*On;
             dspMult=*Off;
             write rcdClmDt4;
             LoadMult();
          EndIf;

          // Load Treating Physician Grid
          If addPhys=*On Or edtPhys=*On Or delPhys=*On;
             rrnPhys=0;
             clrPhys=*On;
             dspPhys=*Off;
             write rcdClmDt2;
             LoadTreatPhys();
          EndIf;

          // Load Trial Information Grid
          If addTrial=*On Or edtTrial=*On or delTrial=*On;
             rrnTrial=0;
             clrTrial=*On;
             dspTrial=*Off;
             write rcdClmDt4;
             LoadTrial();
          EndIf;

          //*** Beg Add ***** 12/06/22 **************************************
          // Load Settlement Information Grid
          If addSettle=*On;
             rrnSettle=0;
             clrSettle=*On;
             dspSettle=*Off;
             write rcdClmDt4;
             LoadSettlement();
          EndIf;
          //*** End Add ***** 12/06/22 **************************************
          // Load Work Information Grid
          If addWork=*On Or edtWork=*On Or delWork=*On
           Or edtWrk=*On Or delWrk=*On;
             rrnWork=0;
             clrWork=*On;
             dspWork=*Off;
             rrnWrk=0;
             clrWrk=*On;
             dspWrk=*Off;
             write rcdClmDt3;
             If scActCltId = '114';
                LoadWrkGrd();
             Else;
                LoadWork();
             Endif;
          EndIf;

          // Load Wage Information Grid
          If addAww=*On Or edtAww=*On;
             rrnAww=0;
             clrAww=*On;
             dspAww=*Off;
             write rcdClmDt3;
             LoadAww();
          EndIf;

          // Load Benefit Information Grid
          If addBenefit=*On Or edtBenefit=*On Or delBenefit=*On;
             rrnBenefit=0;
             clrBenefit=*On;
             dspBenefit=*Off;
             write rcdClmDt3;
             LoadBenefit();
          EndIf;

          // Load Paid Information Grid
          If addPaid=*On Or edtPaid=*On Or delPaid=*On;
             rrnPaid=0;
             clrPaid=*On;
             dspPaid=*Off;
             write rcdClmDt3;
             LoadEmpPaid();
          EndIf;

          // Load Earning Information Grid
          If addEarning=*On Or edtEarning=*On Or delEarning=*On;
             rrnEarning=0;
             clrEarning=*On;
             dspEarning=*Off;
             write rcdClmDt3;
             LoadEarnings();
          EndIf;

          // Load Witness Information Grid
          If addWitness=*On Or edtWitness=*On Or delWitness=*On;
             rrnWitness=0;
             clrWitness=*On;
             dspWitness=*Off;
             write rcdClmDt0;
             LoadWitness();
          EndIf;

       EndIf;

       // Add Indicators
       addAww=*Off;
       addBenefit=*Off;
       addDiag=*Off;
       addEarning=*Off;
       addMult=*Off;
       addPaid=*Off;
       addPart=*Off;
       addPhys=*Off;
       addTrial=*Off;
       addWitness=*Off;
       addWork=*Off;
       //*** Beg Add ***** 12/06/22 **************************************
       addSettle=*Off;
       //*** End Add ***** 12/06/22 **************************************
       // Deletion Indicators
       delBenefit=*Off;
       delEarning=*Off;
       delPaid=*Off;
       delPhys=*Off;
       delWitness=*Off;
       delWork=*Off;
       delWrk=*Off;
       delTrial=*Off;
       // Edit Indicators
       edtAww=*Off;
       edtBenefit=*Off;
       edtDiag=*Off;
       deleteDiag=*Off;
       edtEarning=*Off;
       edtMult=*Off;
       edtPaid=*Off;
       edtPart=*Off;
       delPart=*Off;
       edtPhys=*Off;
       edtTrial=*Off;
       edtWitness=*Off;
       edtWork=*Off;
       edtWrk=*Off;
       // General Indicators
       cancel=*Off;
       saveData=*Off;
       refrshData=*Off;

      /End-Free
     P ResetEditAdd    E
       //===================================================================
       // UpdateRecord - Update DataBase
       //===================================================================
     P UpdateRecord    B
     D UpdateRecord    PI
      /Free

       // Update Claim Info - Tab 1
       screenPtr=%Addr(claimInfo);
       UpdClmInfo(inRrnClm:screenPtr);

       // Update Management Info  - Tab 2
       screenPtr=%Addr(mgmtInfo);
       UpdMgmtInfo(inRrnClm:screenPtr);

       // Update Medical Info - Tab 3
       screenPtr=%Addr(medicalInfo);
       UpdMedInfo(inRrnClm:screenPtr);

       // Update Benefit Info - Tab 4
       screenPtr=%Addr(benefitInfo);
       UpdBenInfo(inRrnClm:screenPtr);

       // Update Legal Info - Tab 5
       screenPtr=%Addr(legalInfo);
       UpdLegInfo(inRrnClm:screenPtr);

       // get the FROI Information
       froiState=GetFroiState(CMco#:CMjrSt);

       // Update State Info  - Tab 6
       Select;
       When froiState='A1';
          screenPtr=%Addr(a1Info);
       When froiState='AL';
          screenPtr=%Addr(alInfo);
       When froiState='CA';
          screenPtr=%Addr(caInfo);
       When froiState='DE';
          screenPtr=%Addr(deInfo);
       When froiState='FL';
          screenPtr=%Addr(flInfo);
       When froiState='GA';
          screenPtr=%Addr(gaInfo);
       When froiState='LA';
          screenPtr=%Addr(laInfo);
       When froiState='MA';
          screenPtr=%Addr(maInfo);
       When froiState='NC';
          screenPtr=%Addr(ncInfo);
       When froiState='NY';
          screenPtr=%Addr(nyInfo);
       When froiState='PA';
          screenPtr=%Addr(paInfo);
       When froiState='WV';
          screenPtr=%Addr(wvInfo);
       Other;
          screenPtr=%Addr(niInfo);
       EndSl;

       If froiState='n/a';
          inState='na';
       Else;
          inState=froiState;
       EndIf;

       UpdStateInfo(inRrnClm:screenPtr:inState);
       //*** Beg Add *** 09/06/22 *********************************************
       // Always update A1 unless na
       If inState <> 'A1';
          screenPtr=%Addr(a1Info);
          UpdStateInfo(inRrnClm:screenPtr:inStateA1);
       Endif;
       //*** End Add *** 09/06/22 *********************************************

       screenPtr=%Addr(diaryInfo);
       UpdQuickDiary(inRrnClm:screenPtr);

       // Access the claim record
       Chain inRRNClm dbrcdf;
       // Access the claim demographic record
       Chain CMss# pcd00;

       // Update Managed Care If Installed

       // Get System Flags to see if WCMC installed
       GetSysFlags(wcmsInst:wcmcInst:wcmcInt);

       // Change Managed Care Claim
       If isModEnabled('Managed Care') and
          isModEnabled('Work Comp Interface');
          // Parameters Shared by FA002R and FA003R
          qmRtCd=*Blanks;
          qmSS#=CMss#;
          qmAcDt=%EditC(CMacDt:'X');
          qmCo#=%EditC(CMco#:'X');
          qmSub#=*Blanks;
          qmFnd=%EditC(CMfnd:'X');
          qmFyr=%EditC(CMfYr:'X');
          qmCase=%EditC(CMcase:'X');

          // Parameters for FA002R
          qmLsN=CDLsn;
          qmFsN=CDfsn;
          qmMid=CDmid;
          qmAdr1=CDadr1;
          qmAdr2=CDadr2;
          qmCty=CDcty;
          qmSt=CDst;
          qmZip=%editc(CDzip:'X');
          qmDtoB=%editc(CDdtob:'X');
          qmSex=CDsex;
          qmArea=%editc(CDarea:'X');
          qmPhn#=%editc(CDphn#:'X');
          qmMsts=CDmsts;
          qmWara=%editc(CDwara:'X');
          qmWph#=%editc(CDwph#:'X');
          qmCtry=CDctry;
          qmAdr3=CDadr3;
          qmCara=%editc(CDcara:'X');
          qmCphn=%editc(CDcphn:'X');

          // Parameters for FA003R
          qmEmp#=%EditC(CMemp#:'X');
          qmDiv=%EditC(CMdiv:'X');
          qmAcTm=%EditC(CMacTm:'X');
          qmDtRp=%EditC(CMdtRp:'X');
          qmDtLw=%EditC(CMdtLw:'X');
          qmAcDs=CMacDs;
          qmMcon=CMmCon;
          qmEntD=%EditC(CMentD:'X');
          qmRcvD=%EditC(CMrcvD:'X');
          qmMMI=%EditC(CMmmi:'X');
          qm1a1=*Blanks;
          qm10a1=*Blanks;
          qmLtFl=CMltFl;
          qmScl#=CMacl#;
          qmMcTm=*Blanks;
          qmStWk=*Blanks;
          qmDeDt=%EditC(CMdod:'X');
          If CMdth='Y';
             qmDeem='DI';
          Else;
             qmDeem=*Blanks;
          EndIf;
          qmCmNu=CMnrse;
          qmCmSu=*Blanks;
          qmClad=CMclEx;
          qmRrn=inRrnClm;
          qmLsn=CMlsn;
          qmFsn=CMfsn;
          qmMid=CMmid;
          qmDtRe=%EditC(CMdtRe:'X');
          qmClSu=CMsupr;
          qm#Dep=%EditC(CM#dep:'X');
          qmOcDs=CMocDs;
          qmHiDt=CMhiDt;
          qmEsts=CMeSts;
          qmWkPt=CMshft;
          qmPrex=CMprEx;
          qmAcD2=CMacD2;
          qmCvFl=CMcvFl;
          qmMca=CMmco;
          qmMmiP=CMiPct;
          qmOdis=CMoDis;
          qmYrlx=CMyrLx;
          qmDbDt=CMdbDt;
          qmHrDy=CMhrsD;
          qmDyWk=CMdays;
          qmClFa=CMfeAd;
          qmDny=CMdny;
          qmImp=CMimp;
          qmJrSt=CMjrSt;
          qmPoSt=CMjrSt;

          // Update WCMC Claim Demographic
          ChangeWCMCdemo(qmRtCd:qmSS#:qmAcDt:qmCo#:qmSub#:qmFnd:
           qmFyr:qmCase:qmLsN:qmFsN:qmMid:qmAdr1:qmAdr2:
           qmCty:qmSt:qmZip:qmDtoB:qmSex:qmArea:qmPhn#:
           qmMsts:qmWara:qmWph#:qmCtry:qmAdr3:qmCara:qmCphn);

          // Update WCMC Claim Master and History
          ChangeWCMC(qmRtCd:qmSS#:qmAcDt:qmCo#:qmSub#:qmFnd:qmFyr:qmCase:
           qmEmp#:qmDiv:qmAcTm:qmDtRp:qmDtLw:qmAcDs:qmMcon:qmEntD:qmRcvD:
           qmMMI:qm1a1:qm10a1:qmLtFl:qmScl#:qmMcTm:qmStWk:qmDeDt:qmDeem:
           qmCmNu:qmCmSu:qmClad:qmRrn:qmLsn:qmFsn:qmMid:qmDtRe:qmClSu:
           qm#Dep:qmOcDs:qmHiDt:qmEsts:qmWkPt:qmPrex:qmAcD2:qmCvFl:qmMca:
           qmMmiP:qmOdis:qmYrlx:qmDbDt:qmHrDy:qmDyWk:qmClFa:qmDny:qmImp:
           qmJrSt:qmPoSt);
       EndIf;

      /End-Free
     P UpdateRecord    E
       //===================================================================
       // WriteNameHist - Write Name Histroy Record
       //===================================================================
     P WriteNameHist   B
     D WriteNameHist   PI
     d   lastName                    30
     d   firstName                   15
     d   midInit                      1
     d   address1                    40
     d   address2                    40
     d   city                        25
     d   state                        2
     d   zipCode                      9s 0
     d   eMail                       60

     D xType1          s              1    Inz(' ')
     D xType2          s              1    Inz(' ')
      /Free

       Clear pnt0p;

       If lastName<>CMlsn Or firstName<>CMfsn Or midInit<>CMmid;
          xType1='N';
          NTtype='N';
       EndIf;

       If address1<>CDadr1 Or address2<>CDadr2 Or city<>CDcty Or
          state<>CDst Or zipCode<>CDzip;
          xType2='A';
          NTtype='A';
       Endif;

       If xType1<>' ' And xType2<>' ';
          NTtype='B';
       Endif;

       // Only write if a change was done
       If NTtype<>' ';
          NToNam=%Trim(CMlsn) + ', ' + %Trim(CMfsn) + ' ' +  CMmid;
          NTnNam=%Trim(lastName) + ', ' + %Trim(firstName) + ' ' +
           midInit;
          NToAd1=CDadr1;
          NToAd2=CDadr2;
          NToSt=CDst;
          NToCty=CDcty;
          NToZip=CDZip;
          NTnAd1=address1;
          NTnAd2=address2;
          NTnSt=state;
          NTnCty=city;
          NTnZip=zipCode;
          NTco#=CMco#;
          NTfnd=CMfnd;
          NTfYr=CMfYr;
          NTcase=CMcase;
          NToMad=CDeMad;
          NTnMad=Email;

          NTpgm=q1Pgm;
          NTuser=q1User;
          NTstat='A';
          NTtime=TimeTo6(SystemTime());
          NTdt=DateTo7(SystemDate());
          NTtrDt=DateTo7(SystemDate());

          Write pnt0p;
       EndIf;


      /End-Free
     P WriteNameHist   E
       //===================================================================
       // * utility procedure definitions
       //===================================================================
      /copy ScopyClmPi
