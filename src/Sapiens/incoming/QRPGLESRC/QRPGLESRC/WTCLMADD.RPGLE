     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE WTCLMADD - Profound Claim Add
     h/DEFINE   PROFOUNDUI
     h DFTACTGRP(*no) ACTGRP('QILE')
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Will Add Claims.
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 2/6/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:                                                         *
     h*
     h*  04/08/25 C:333  Support: 118812 (Project: WC2998)
     h*                 - Recompiled as DSPF WTCLMDTL changed.
     h*
     h*  03/06/25 C:333  Project WC2998
     h*                  Changes to CMS Settlement Reporting.
     h*                  - Recompiled.
     h*                                                                    *
     h*  01/14/25 C:340  Project WC2995                                    *
     h*                  Recompiled as DSPF WTCLMDTL changed.              *
     h*                                                                    *
     h*  04/10/24 C:079 Ticket: 146/115349                                 *
     h*                 Update CMltFl After ErrorCheck0                    *
     h*                                                                    *
     H*  03/22/24 C:333 Project: WC2826                                    *
     H*                 - Recompiled as DSPF WTCLMDTL changed.             *
     h*                                                                    *
     H*  04/10/23 C:333 Support: 108889 (UBIC)                             *
     H*                 - Recompiled as DSPF WTCLMDTL changed.             *
     h*                                                                    *
     H*  12/06/22 C:291 Project: 152WBZ                                    *
     H*                 Add Settlement Information grid on Legal           *
     h*                 Information tab.                                   *
     h*                                                                    *
     h* 09/06/22 C:079 Project: WC2823                                     *
     h*                FROI Changes                                        *
     h*                                                                    *
     h* 08/29/22 C:340 Project: WC2823                                     *
     h*                FROI Data Field Updates                             *
     h*                                                                    *
     h* 04/22/22 C:062 Ticket: 101942                                      *
     h*                fix load of location from online froi               *
     h*
     h* 02/19/21 C:318 Support: 94631
     h*                Mask SSN on Clearance Info screen
     h*
     h* 06/09/20 C:33  Project: SY420
     h*                Mask SSN
     h*
     h*  03/31/20 C:300 Support: 87812 (SCS)                               *
     h*                 Add Indemnity Data Call new fields on Claim Entry  *
     h*                                                                    *
     h*  11/25/19 C:079 Project: 152WAW - Recompile due to rcdclmdt1 chg   *
     h*                                                                    *
     h*  03/22/17 C:053 Support: 46241                                     *
     h*                 Recompile over changed copy module                 *
     h*                                                                    *
     h*  01/31/17 C:306 Project: WC2410                                    *
     h*                 Recompile over changed WTCLMDTL                    *
     h*                                                                    *
     h*  07/25/16 C:136 Project: 114WYQ                                    *
     h*                 Add Dismiss SMS Texting checkbox                   *
     h*                                                                    *
     h*  07/11/16 C:298 Support:42366                                      *
     h*                 Add "addPopWV" handling.                           *
     h*                                                                    *
     h*  06/13/16 C:062 Support:42000 (99)                                 *
     h*                 Add Group Expiration edit.                         *
     h*                                                                    *
     h*  05/24/16 C:309 Support: PHW41670                                  *
     h*                 Recompile only                                     *
     h*
     h*  05/09/16 C:300 Project: 114WXV                                    *
     h*                 Add Cell Number to Claimant Demographics.          *
     h*
     h*  12/02/15 C:212 Project: 25MUD
     h*                 Recompile over changed WTCLMDTL
     h*
     h*  07/31/15 C:308 Support: 36212
     h*                 Recompiled over WTCLMRSV display file
     h*
     h*  07/06/15 C:306 Support: 33063
     h*                 Recompiled over WTCLMRSV display file
     h*
     h*  04/30/15 C:062 Support 33005(88)
     h*                 Don't write this remark unless cltid=114
     h*
     h*  04/20/15 C:300 Project: WC2043
     h*                 Modified CMS alleged cause of injury field to use  *
     h*                 ICD-9 or ICD-10 code based on date of injury.      *
     h*
     h*  12/10/14 C:295 RED 3161
     h*                 Change the date definition on the grid from
     h*                 Number to date data type.
     h*
     h*  12/01/14 C:279 Project: 114WLF
     h*                 Modified to use Reserve Worksheet.
     H*
     h*  09/26/14 C:282 Support: WA146 86804
     h*                 Set the 'Show zero balance' property on the field
     h*                 sc#Dep of Claim Infomation screen. Recompile the
     h*                 program.
     h*
     h*  09/04/14 C:279 Project: 114WOE
     h*                 Added Claim RRN hidden field on screen.
     h*
     h* 08/25/14  C:279 Project: 114WOP                                       *
     h*                 Added Alternate State Claim # field for client '114'. *
     h*
     h*  08/18/14 C:282 Support: WA133 86355
     h*                 Added the handling 'Changed Special Disability
     h*                 Fund from a checkbox to a drop-down for LCTA'.
     h*
     h*  08/07/14  C:282 Redmine: Redmine 4566
     h*                  Added the handling to show Reserve History grid
     h*                  headings and the Legal Heading.
     h*
     h*  08/04/14 C:283 Support: WC146 84781
     h*                 Re-Compile Only.
     h*
     h*  06/17/14 C:283 Support WC114 85901
     h*                 Correct the showing up of message for
     h*                 a Cancelled Employer.
     h*
     h*  04/22/14 C:062 SUPPORT WC114 85413
     h*                 Restrict group.
     h*
     h*  03/26/14 C:283 Support: WC099 85178
     h*                 Add the handling for Cancelled Employer to check
     h*                 the authority of the user and Allow/Disallow
     h*                 entry of Claim as per setup at Group Level.
     h*
     h*  03/14/14 C:295 Support: WC114 84963
     h*                 Add the Delete link for Trial Information Grid.
     h*
     h*  02/18/14 C:062 Project: 25WTL
     h*                 Retrofit TASB Froi
     h*
     h*  02/12/14 C:180 SUPPORT PRISS 027 - WC2234
     h*                 Fix Work Days Schedule code field.
     h*
     h*  12/12/13 C:287 Project:WC2234
     h*                 Add new fields for claims EDI Rel3.
     h*                 CMpspa, CMwwtc, CMwdsc, CMesid.
     h*
     h*  11/25/13 C:282 Project: 129WSC
     h*                 Add new fields for claims for client 129 only.
     h*                 - Restitution
     h*                 - 14-6
     h*                 - Loss transfer
     h*                 - Misc
     h*                 - Apportionment %
     h*                 - Apportionment Effective Date
     h*                 Added the Lost Time Beg and Claim Sataus Beg
     h*                 parameters in Auto Letter Call.
     h*
     h*  10/01/13 C:053 Project: 25WSS
     h*                 Client retrofit.
     h*
     h*  09/23/13 C:062 Support: WC114 83507
     h*                 Recompile only
     h*
     h*  09/17/13 C:062 Support: op114 83508
     h*                 Compile only
     h*
     h*  07/15/13 C:283 Support: WC099 82849
     h*                 Added the save button on display file(Pmt Exlc)
     h*                 Save button works as enter so handling
     h*                 in program not required. Re-Compiled.
     h*
     h*  06/25/13 C:062 Support WC114 82712
     h*                 Add lgoc if policy is cancelled.
     h*
     h*  06/19/13 C:282 Redmine: 3028
     h*                 Added the AutoLetter procedure to generate the
     h*                 Auto Letter when the claim is added.
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
     h*  01/09/13 C:284 Project: 146WFQ
     h*                 Added new fields scFeet and scInch in display file.
     h*                 Only Recompile the program.
     h*
     h*  04/01/13 C:279 Redmine: 2591
     h*                 Modified to create auto diary on new claim set up
     h*
     h*  10/25/12 C:284 Project: 114WGD
     h*                 CSI Retrofit of Custom Code to Base
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
     f                                     Ignore(rcdClmDtl)
     f                                     rename(QSFLCTL001:XSFLCTL001)
     f                                     rename(QSFLCTL002:XSFLCTL002)
     f                                     rename(QSFLCTL003:XSFLCTL003)
     f                                     rename(QSFLCTL004:XSFLCTL004)
     f                                     rename(QSFLCTL005:XSFLCTL005)
     f                                     rename(QSFLCTL006:XSFLCTL006)
     f                                     rename(QSFLCTL007:XSFLCTL007)
     f                                     rename(QSFLCTL008:XSFLCTL008)
     f                                     rename(QSFLCTL009:XSFLCTL009)
     f                                     rename(QSFLCTL010:XSFLCTL010)
     f                                     InfDs(fids01)
     fwtClmRsv  cf   e             WorkStn Handler('PROFOUNDUI(HANDLER)')
     f                                     ExtFile('WTCLMRSV')
     f                                     Sfile(SflPmtSum:rrnPmtSum)
     f                                     Sfile(SflReserve:rrnHst)
     f                                     Sfile(SflStsHst:rrnSts)
     f                                     Ignore(rcdPayDtl)
     f                                     Ignore(rcdPndChk)
     f                                     Ignore(rcdDedHst)
      // Claim History <====================================================
     Fwmchl5    if a e           k disk    ReName(pch01:pch05)
      // Claim Master <=====================================================
     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
     f                                     InfDs(dbfds)
      // Employer Master <==================================================
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     f                                     InfDs(dbfds2)
     fwdelp     if   e             Disk    ReName(pel01:pel0p)
      // Reserve History <==================================================
     FWmrhl5    if a e           k Disk
     F                                     ReName(prh01:prh05)
     f                                     infds(wmrhl5fds)
      // Table codes <======================================================
     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     fwmcwl     if   e           k disk    rename(pcw01:pcw00)
     fwtabl     if   e           k Disk    ReName(ppa01:ppa00)
     f                                     Rename(prt01:prt00)
     fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
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
      // Comp Reimbursement File
     fwdrbl     if   e           k Disk    ReName(prb01:prb00)

      // Rate Set Code
     fwmrsl     if   e           k Disk    ReName(prs01:prs00)

     ffawhl     if   e           k Disk    Rename(pwh01:pwh0f)
     f                                     infds(fawhlfds) UsrOpn
     fftdll     if   e           k Disk    Rename(pdl01:pdl00) UsrOpn
     fftgtl     if   e           k Disk    Rename(pgt01:pgt0f) UsrOpn
     ffmcmp     if   e             Disk    Rename(pcm01:pcm0f) Prefix(fm:2)
     f                                     UsrOpn
     fwmcdl     if   e           k Disk    Rename(pcd01:pcd00) UsrOpn
     fwmddl     if a e           k Disk    Rename(pdd01:pdd00)
     fwaidl1    if a e           k Disk    Rename(pid01:pid00)
     fwmupl1    if   e           k Disk
     fstctl1    if   e           k Disk
      // FROI Work File
     fwhfrp     if   e             Disk    ReName(pfr01:pfr00) UsrOpn
     fssylf     if   e           k Disk    rename(swc01:swc00)
     f                                     ignore(ssy01)
     f                                     ignore(spc01)
     f                                     ignore(src01)
     f                                     ignore(sbc01)
     f                                     ignore(scs01)
     fwmfnl     if   e           k Disk    Rename(pfn01:pfn00)

     d* Proto-types
     d AutoLetters     pr                  ExtPgm('WA049R')
     d   inRrnClm                     9  0
     d   returnCode                   1
     d   charLTFrm                    1
     d   charCSFrm                    1
     d TasbFroiInfo    pr                  ExtPgm('W9500R01')
     d   inRrnTFFRP                   9  0
     d   inModeTFFRP                  1
     d   outRrnWMCMP                  9  0
     d   outSocSec#                   9
     d   outAccDate                   7  0
     d   outCompany#                  3  0
     d   outGroup                     3  0
     d   outEmployer#                 9  0
     d   outDivision                  5  0

     d* Create WCMC Case
     d CreateWCMCdemo  pr                  ExtPgm('FA002R')
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
     d CreateWCMC      pr                  ExtPgm('FA003R')
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


     d CreateWRKF      pr                  ExtPgm('FA710C')
     d   ##Rtn                        1
     d   #Fname                      10
     d   #Tname                      10

     d DelClmMC        pr                  ExtPgm('F7090R')
     d   ##Call                       1
     d   ##Kl                         1
     d   ##Rrn                        9  0
     d   ##Rrn1                       9  0
     d   ##Rrn2                       9  0
     d   ##Rrn3                       9  0
     d   ##Td                         1
     d   ##Flag                       1
     d   ##Wcms                       1


     d* Delete Claim
     d deleteClaim     pr                  ExtPgm('W0070R04')
     d   exit                         1
     d   cancel                       1
     d   mode                         1
     d   rrn                          9  0

     d* Employer Search
     d EmpSearch       pr                  ExtPgm('WTSEARCH')
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)

     d* Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

     d* IDarts Call
     d IDarts          pr                  ExtPgm('SC010RVB')
     d   dartsKey                    40
     d   programName                 10

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

     d* Assign Buttons
     d AssignBtn       pr
     d step                           3  0

     d* Check Add and Edit functions for Claim Info Pop-ups
     d ChkAddEdtClm    pr

     d* Check Add and Edit functions for Medical Info Pop-ups
     d ChkAddEdtMed    pr

     d* Check Add and Edit functions for Benefit Info Pop-ups
     d ChkAddEdtBen    pr

     d* Choose State Screen
     d ChooseStates    pr

     d* Error Certification Routine
     d ErrorCert       pr              n

     d* Load AWW History Grid
     d LoadAww         pr

     d* Load Benefit Adjustment Grid
     d LoadBenefit     pr

     d* Load Body Part Grid
     d LoadBodyPart    pr

     d* Load Confirm Claim Screen
     d LoadConfClm     pr

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

     d* Load Screen  Sequence
     d LoadScreenSeq   pr

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

     d* Load Reserve History grid
     d LodHstInfo      pr

     d* Load Reserve grid
     d LodRsvInfo      pr

     d* Load Claim Status grid
     d LodStsInfo      pr

     d* Location Exists
     d locExists       pr              n
     d empDiv                        17
     d accidentDate                   7  0 Const

     d* Process Clearance Screen
     d ProcClrInfo     pr

     d* Process Claim Assignment Screen
     d ProcConfClaim   pr

     d* Process Claim Info Screen
     d ProcClmInfo     pr             3  0
     d step                           3  0

     d* Process Management Info Screen
     d ProcMgmtInfo    pr             3  0
     d step                           3  0

     d* Process Medical Info Screen
     d ProcMediInfo    pr             3  0
     d step                           3  0

     d* Process Benefit Info Screen
     d ProcBeneInfo    pr             3  0
     d step                           3  0

     d* Process Reserve Info Screen
     d ProcRsvInfo     pr             3  0
     d step                           3  0

     d* Process State Info Screen
     d ProcStateInfo   pr             3  0
     d step                           3  0

     d* Process Diary Info Screen
     d ProcDiaryInfo   pr             3  0
     d step                           3  0

      *** Beg Add *** 08/29/22 *************************************************
     d* Process Print FROI Screen
     d ProcPrintFROI   pr             3  0
     d step                           3  0
     d
     d FroiPrint       pr                  ExtPgm('W0070R31')
     d  Exit                          1
     d  Cancel                        1
     d  Mode                          1
     d  inRrnClm                      9  0
     d  JurState                      2
     d
      *** End Add *** 08/29/22 *************************************************

     d* Process Remaining Screens
     d ProcClmScreens  pr

     d* Resets Edit Add Indicators - Claim Screen
     d ResetEdtAddClm  pr

     d* Resets Edit Add Indicators - Medical Screen
     d ResetEdtAddMed  pr

     d* Resets Edit Add Indicators - Benefit Screen
     d ResetEdtAddBen  pr

     d* Load History grid
     d SrChg           pr

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
     d @@ICDSts        s              1    inz

     d WrtDrtRemark    pr                  ExtPgm('WA700R')
     d   qqCo#                        3  0
     d   qqFnd                        3  0
     d   qqFyr                        3  0
     d   qqCase                       7  0
     d   qqDKey                      10
     d   qqComm                     256

     d* General Prototypes
     d/copy dPrRemark
     d/copy wPrUsrPrms
     d/copy sPrAtrium
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
     d/copy wPrClmErrM
     d/copy wPrClmDat
     d/copy WTSearchDs
     d/copy wPrEdi
     d/copy sPrGenPop
     d/copy sPrModule
     d/copy sPrMask

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
     D wmrhl5fds       ds
     D WMRHPrrn              397    400b 0
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
     D rsvInfo       e ds                  ExtName(wtClmRsv:rcdRsvDtl) inz
     D screenPtr       s               *
     d*** Beg Add *** 09/06/22 *************************************************
     D screenPtrA1     s               *
     d*** End Add *** 09/06/22 *************************************************
     D*** Beg Add *** 04/10/24 ********************************************
     D screenPtr2      s               *
     D*** End Add *** 04/10/24 ********************************************
     D*Constants
     D noEMail         c                   Const('No E-Mail Available')
     D*Variables
     D @@RRN           s              9
     D @@FroiMode      s              1
     D @@RrnWMCMP      s              9  0
     D @@Mode          s              1
     D @@Next          s             10
     D @@TabIndex      s              5
     D outClientID     s              3    inz('000')
     D addMode         s               n   inz(*off)
     D chgMode         s               n   inz(*off)
     D inRrnClm        s              9  0
     D inRrnFroi       s              9  0
     D inFroiMode      s              1
     D hdRrnFroi       s              9  0
     d inRrnTFFRP      s              9  0
     d inModeTFFRP     s              1
     d outRrnWMCMP     s              9  0
     d outSocSec#      s              9
     d outAccDate      s              7  0
     d outCompany#     s              3  0
     d outGroup        s              3  0
     d outEmployer#    s              9  0
     d outDivision     s              5  0
     D inState         s              2
     d*** Beg Add *** 09/06/22 *******************************************
       dcl-s inStateA1 Char(2);
       dcl-s svScreenPtr Pointer;
     d*** End Add *** 09/06/22 *******************************************
     D inMode          s              1
     D outRRN          s              9  0
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
     D kkCo#           s                   like(EMco#)
     D kkFnd           s                   like(EMfnd)
     D kkEmp#          s                   like(EMemp#)
     D kkDiv           s                   like(EMdiv)
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
     D length          s              5  0
     D position        s              5  0
     D position2       s              5  0
     D froiState       s              3    Inz(*Blanks)
     D nextStep        s               n
     D assignClm       s               n
     D i               s              3  0
     D j               s              3  0
     D decCcHg         s              9  2
     D decMcHg         s              9  2
     D decLcHg         s              9  2
     D decEcHg         s              9  2
     D inEfdt          s              7  0
     D inTime          s                   like(RHtime)
     D polYear         s              3  0
     D polFrDt         s              8  0
     D polToDt         s              8  0
     D cxDate          s              7  0
     d intIndTotal     s             13  2
     d intMedTotal     s             13  2
     d isWrkSht        s               n
     d strRrn#         s              9
     d strMsg          s            256
     d strTitle        s             50
     D xxName          s             40
     D xxMad1          s             40
     D xxMad2          s             40
     D xxMcty          s             25
     D xxMst           s              2
     D xxMzip          s              9  0
     D xxPad1          s             40
     D xxPad2          s             40
     D xxPcty          s             25
     D xxPst           s              2
     D xxPzip          s              9  0
     D xxPhn#          s              7  0
     D xxArea          s              3  0
     D xxEfDt          s              7  0
     D xxDlDt          s              7  0
     D linkTitle       s             50
     D modeIn          s              1    inz('C')
     D callIn          s              1    inz('2')
     D rrnOut          s              9    inz('000000000')
     D status          s              1
     D inSocSec#       s              9
     DhldGroup#        s                   like(scGroup#)
     DhldEmpDivNm      s                   like(scEmpDivNm)
     DhldAccDateI      s                   like(scAccDateI)
     D jurState        s                   like(CMjrSt)
     D mode            s              1    Inz('A')

     D dateValueIn     s              8  0
     D errorMsg        s             50
     D errorMsgId      s             10

     d strCompany#     s              3
     d strGroup        s              3
     d strYear         s              3
     d strCase         s              7
     d strActFlag      s              1    Inz('T')
     d strMessage      s              7    Inz(*Blanks)
     d strReturnCod    s              1    Inz(*Blanks)

     D showPrev        s               n   Inz(*Off)
     D showNext        s               n   Inz(*Off)
     D cancelAdd       s               n   Inz(*Off)
     D prevScreen      s               n   Inz(*Off)
     D nextScreen      s               n   Inz(*Off)
     D writeClm        s               n   Inz(*Off)
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
     d returnCode      s              1
     d ZeroDate        s               D   inz datfmt(*ISO)
     d charLTFrm       s              1    Inz(' ')
     d charCSFrm       s              1    Inz(' ')

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
     d   qmFsn         s             15
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

     d*
       // F7090R parameters
     d   ##Call        s              1    Inz(*blanks)
     d   ##Kl          s              1    Inz(*blanks)
     d   ##Rrn         s              9  0 Inz(*zeros)
     d   ##Rrn1        s              9  0 Inz(*zeros)
     d   ##Rrn2        s              9  0 Inz(*zeros)
     d   ##Rrn3        s              9  0 Inz(*zeros)
     d   ##Td          s              1    Inz(*blanks)
     d   ##Flag        s              1    Inz(*blanks)
     d   ##Wcms        s              1    Inz(*blanks)
     d*
       // FA710C parameters
     d   ##Rtn         s              1    Inz(*blanks)
     d   #Fname        s             10    Inz(*blanks)
     d   #Tname        s             10    Inz(*blanks)

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

     D                 ds
     D  rt                     1     20
     D                                     Dim(10)
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

     D                 ds
     D  su                     1     90  2
     D                                     Dim(10)
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

     D                 ds
     D  ar                     1     90  2
     D                                     Dim(10)
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

     D                 ds
     D  rr                     1     90  2
     D                                     Dim(10)
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

     D                 ds
     D  rt#                    1     20
     D                                     Dim(10)
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

     D                 ds
     D  ti                     1     90  2
     D                                     Dim(10)
     D  scTi01                 1      9  2
     D  scTi02                10     18  2
     D  scTi03                19     27  2
     D  scTi04                28     36  2
     D  scTi05                37     45  2
     D  scTi06                46     54  2
     D  scTi07                55     63  2
     D  scTi08                64     72  2
     D  scTi09                73     81  2
     D  scTi10                82     90  2

      // Arrays

     DscreenSeq        DS                  Dim(10)
     D                                     QUALIFIED
     D screen                        10A
     D buttonType                     1  0
       // 0=No Buttons
       // 1=Cancel, Finish and Next Buttons
       // 2=Previous, Cancel, Finish and Next Buttons
       // 3=Previous, Cancel and Finish Buttons
      *

     C     *entry        Plist
     C                   Parm                    @@RRN
     C                   Parm                    @@FroiMode
     C                   Parm                    @@RrnWMCMP

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

     C     key1WMCHL5    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    inEfdt
     C                   Kfld                    inTime

     C     key2WMCHL5    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     keyWMCXL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd

     C     keyWMEML      Klist
     C                   Kfld                    kkCo#
     C                   Kfld                    kkFnd
     C                   Kfld                    kkEmp#
     C                   Kfld                    kkDiv

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

     C     key1WMRHL5    klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase
     C                   kfld                    inEfDt
     C                   KFLD                    inTime
     C*
     C     key2WMRHL5    klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd
     C                   kfld                    CMfyr
     C                   kfld                    CMcase
     C*
     C     keyWMRSL      klist
     C                   kfld                    ELco#
     C                   kfld                    ELrtCd

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

     C     keyWMDDL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     keySSYLF      Klist
     C                   Kfld                    ##KeyWc           2

      /Free

       ExSr  ProgramStart;

       // Clearance Information
       DoU (cancelConf=*Off and writeClm=*Off) Or cancelAdd1=*On;
          ProcClrInfo();
          If nextStep;
             // Claim Confirmation
             ProcConfClaim();
             If cancelConf=*Off;
                ProcClmScreens();
                If writeClm = *On;
             //    AutoLetters(inRrnClm:returnCode);
                   AutoLetters(inRrnClm:returnCode:charLTFrm:charCSFrm);
                EndIf;
             EndIf;
          EndIf;
          If inRrnFroi <> 0;
             cancelAdd1 = *On;
          Endif;
       EndDo;

       ExSr  ProgramEnd;

       //===================================================================
       // ProgramEnd - Start Processing
       //===================================================================
       Begsr ProgramEnd;

          *inlr=*on;
          Return;

       endsr;
       //===================================================================
       // ProgramStart - Start Processing
       //===================================================================
       Begsr ProgramStart;

          If %parms()>0 and %Int(@@Rrn)<>0;
             inRrnFroi =%Int(@@Rrn);
          EndIf;

          inFroiMode=' ';
          If %parms()>1 and @@FroiMode<>' ';
             inFroiMode = @@FroiMode;
          EndIf;

          // Get the client id
          GetClientID(outClientID);

          // Get System Flags
          GetSysFlags(wcmsInst:wcmcInst:wcmcInt);

          // Show Colorado Fields if Jurisdiction State is CO
          colorado=*Off;
          If CMjrSt='CO';
             colorado=*On;
          EndIf;

          If CMpspa = 'E';
            scPSPA = 'Y';
          Else;
            scPSPA = 'N';
          EndIf;
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

          If outClientID = '063';
          scChoiceOp = 'TRIM(CAKEY) || ' +  '''-'''  + ' || TRIM(CADESC) || ' +
                        ''' - ''' + ' || CAPNTS';
          Else;
          scChoiceOp = 'TRIM(CAKEY) || ' + '''-''' + ' || CADESC';
          EndIf;
          // Conditioning TASB fields
          If outClientID = '025';
             isTASB = *On;
             isTASB1 = *On;
          Else;
             isTASB = *Off;
             isTASB1 = *Off;
          EndIf;

          showClmSts=*Off;
          shwAdd00=*On;
          shwAdd02=*On;
          shwAdd03=*On;
          shwAdd04=*On;
          shwRsvAdd=*On;
          cancelConf=*Off;
          addPop00=*On;
          addPop01=*On;
          addPop02=*On;
          addPop03=*On;
          addPop04=*On;
          addPop06=*On;
          addPop07=*On;
          addPopA1=*On;
          addPopAL=*On;
          addPopCA=*On;
          addPopDE=*On;
          addPopFL=*On;
          addPopGA=*On;
          addPopLA=*On;
          addPopMA=*On;
          addPopNC=*On;
          addPopNY=*On;
          addPopPA=*On;
          addPopNI=*On;
          addPopWV=*On;
          // ***** End Add ***** 07/11/16 *************************************

          ##KeyWc = 'WC';
          Chain keySSYLF swc00;
          If %found(SSYLF) and WCFL52 = 'Y';
             dspRtwSts = *on;
          Else;
             dspRtwSts = *off;
          EndIf;

          // Conditioning CSI and non-CSI fields
          If outClientID = '114';
             isCSI = *on;
             // Conditioning Alternate State Claim #
             dspAltStCl = *on;
          else;
             isBase= *on;
             // Conditioning Alternate State Claim #
             dspAltStCl = *off;
          EndIf;

          // Conditioning BCI fields
          If outClientID = '063';
             isBIC = *On;
          Else;
             isBIC = *Off;
          EndIf;
          // Conditioning CoverRisk fields
          If outClientID = '129';
             isFCC = *On;
             isFCC1 = *On;
          Else;
             isFCC = *Off;
             isFCC1 = *Off;
          EndIf;
          // Conditioning LCTA and non-LCTA fields
          If outClientID = '045';
             isLCTA = *on;
          else;
             isNLCTA= *on;
          EndIf;
       If isModEnabled('IDC');
          isIDCVisbl = *On;
          isPeDisVis = *On;
          isDisLVis = *On;
       Endif;

       endsr;
      /End-Free
       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // AssignBtn - Assigns the Visible Buttons.
       //===================================================================
     P AssignBtn       B
     D AssignBtn       PI
     D stepIn                         3  0
     D step            s              3  0
      /Free

       step=stepIn;

       Select;
       When screenSeq(step).buttonType=1;
          showPrev=*Off;
          showNext=*On;
       When screenSeq(step).buttonType=2;
          showPrev=*On;
          showNext=*On;
       When screenSeq(step).buttonType=3;
          showPrev=*On;
          showNext=*Off;
       EndSl;

      /End-Free
     P AssignBtn       E
       //===================================================================
       // ChkAddEdtClm - Check to see if Add/Edit indicators set off - Claim
       //===================================================================
     P ChkAddEdtClm    B
     D ChkAddEdtClm    PI
      /Free

       // Reset Indicators
       addEdit=*Off;
       edtWitness=*Off;
       delWitness=*Off;

       ReadC sflWitness;

       select;
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
          addEdit=*On;
          posWitness=*On;
       EndSl;

       If addEdit=*Off And prevScreen=*Off And writeClm=*Off And
          cancelAdd=*Off And showNext=*On;
          nextScreen=*On;
       EndIf;

      /End-Free
     P ChkAddEdtClm    E
       //===================================================================
       // ChkAddEdtMed - Check to see if Add/Edit indicators set off - Medic
       //===================================================================
     P ChkAddEdtMed    B
     D ChkAddEdtMed    PI
      /Free

       // Reset Indicators
       addEdit=*Off;
       edtDiag=*Off;
       deleteDiag=*Off;
       edtPart=*Off;
       delPart=*Off;
       edtPhys=*Off;
       delPhys=*Off;

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

       If addEdit=*Off And prevScreen=*Off And writeClm=*Off And
          cancelAdd=*Off And showNext=*On;
          nextScreen=*On;
       EndIf;

      /End-Free
     P ChkAddEdtMed    E
       //===================================================================
       // ChkAddEdtBen - Check to see if Add/Edit indicators set off - Benef
       //===================================================================
     P ChkAddEdtBen    B
     D ChkAddEdtBen    PI
      /Free

       // Reset Indicators
       addEdit=*Off;
       posAww=*Off;
       posBenefit=*Off;
       posEarning=*Off;
       posPaid=*Off;
       posWork=*Off;
       edtAww=*Off;
       edtBenefit=*Off;
       edtEarning=*Off;
       edtPaid=*Off;
       edtWork=*Off;
       edtWrk=*Off;
       delBenefit=*Off;
       delEarning=*Off;
       delPaid=*Off;
       delWork=*Off;
       delWrk=*Off;

       ReadC sflAww;
       ReadC sflBenefit;
       ReadC sflEarning;
       ReadC sflPaid;
       ReadC sflWork;
       ReadC sflWrkGrd;

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
          addEdit=*On;
          posBenefit=*On;
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
       when addWork=*On or edtWork=*On or delWork=*On
        or edtWrk=*On or delWrk=*On;
          If outClientID <> '114';
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

       If addEdit=*Off And prevScreen=*Off And writeClm=*Off And
          cancelAdd=*Off And showNext=*On;
          nextScreen=*On;
       EndIf;

      /End-Free
     P ChkAddEdtBen    E
       //===================================================================
       // ChooseStates - Choose State Screen to Display
       //===================================================================
     P ChooseStates    B
     D ChooseStates    PI
      /Free

       // get the FROI Information
       froiState=GetFroiState(CMco#:scJurState);
       //*** Beg Add *** 09/06/22 ********************************************
       // Set Height for A1 and turn on indicator for buttons
       addCanVis=*Off;
       a1LineVis = *on;
       Select;
       When froistate = 'A1';
          A1Height = 600;
          addCanVis=*On;
          a1LineVis = *off;
       When froistate = 'NY';
          A1Height = 1220;
       When froistate = 'MA';
          A1Height = 1000;
       When froistate = 'NC';
          A1Height = 1000;
       When froistate = 'PA';
          A1Height = 890;
       When froistate = 'WV';
          A1Height = 980;
       When froistate = 'n/a';
          A1Height = 600;
       Other;
          A1Height = 900;
       Endsl;
       // Always load A1 screen if froiState is not A1
       If froiState <> 'A1';
          showPrevA1 = *off;
          showNextA1 = *off;
          Write rcdClmDtA1;
       Endif;
       //*** End Add *** 09/06/22 ********************************************

       Select;
       // A1 - Data
       When froiState='A1';
          nextScrnA1=*Off;
          prevScrnA1=*Off;
          cxAddA1=*Off;
          writeClmA1=*Off;
          Exfmt rcdClmDtA1;
          nextScreen=nextScrnA1;
          prevScreen=prevScrnA1;
          cancelAdd=cxAddA1;
          writeClm=writeClmA1;
       // AL - Data
       When froiState='AL';
          nextScrnAL=*Off;
          prevScrnAL=*Off;
          cxAddAL=*Off;
          writeClmAL=*Off;
          Exfmt rcdClmDtAL;
          nextScreen=nextScrnAL;
          prevScreen=prevScrnAL;
          cancelAdd=cxAddAL;
          writeClm=writeClmAL;
       // CA - Data
       When froiState='CA';
          nextScrnCA=*Off;
          prevScrnCA=*Off;
          cxAddCA=*Off;
          writeClmCA=*Off;
          Exfmt rcdClmDtCA;
          nextScreen=nextScrnCA;
          prevScreen=prevScrnCA;
          cancelAdd=cxAddCA;
          writeClm=writeClmCA;
       // DE - Data
       When froiState='DE';
          nextScrnDE=*Off;
          prevScrnDE=*Off;
          cxAddDE=*Off;
          writeClmDE=*Off;
          Exfmt rcdClmDtDE;
          nextScreen=nextScrnDE;
          prevScreen=prevScrnDE;
          cancelAdd=cxAddDE;
          writeClm=writeClmDE;
       // FL - Data
       When froiState='FL';
          nextScrnFL=*Off;
          prevScrnFL=*Off;
          cxAddFL=*Off;
          writeClmFL=*Off;
          Exfmt rcdClmDtFL;
          nextScreen=nextScrnFL;
          prevScreen=prevScrnFL;
          cancelAdd=cxAddFL;
          writeClm=writeClmFL;
       // GA - Data
       When froiState='GA';
          nextScrnGA=*Off;
          prevScrnGA=*Off;
          cxAddGA=*Off;
          writeClmGA=*Off;
          Exfmt rcdClmDtGA;
          nextScreen=nextScrnGA;
          prevScreen=prevScrnGA;
          cancelAdd=cxAddGA;
          writeClm=writeClmGA;
       // LA - Data
       When froiState='LA';
          nextScrnLA=*Off;
          prevScrnLA=*Off;
          cxAddLA=*Off;
          writeClmLA=*Off;
          Exfmt rcdClmDtLA;
          nextScreen=nextScrnLA;
          prevScreen=prevScrnLA;
          cancelAdd=cxAddLA;
          writeClm=writeClmLA;
       // MA - Data
       When froiState='MA';
          nextScrnMA=*Off;
          prevScrnMA=*Off;
          cxAddMA=*Off;
          writeClmMA=*Off;
          Exfmt rcdClmDtMA;
          nextScreen=nextScrnMA;
          prevScreen=prevScrnMA;
          cancelAdd=cxAddMA;
          writeClm=writeClmMA;
       // NC - Data
       When froiState='NC';
          nextScrnNC=*Off;
          prevScrnNC=*Off;
          cxAddNC=*Off;
          writeClmNC=*Off;
          Exfmt rcdClmDtNC;
          nextScreen=nextScrnNC;
          prevScreen=prevScrnNC;
          cancelAdd=cxAddNC;
          writeClm=writeClmNC;
       // NY - Data
       When froiState='NY';
          nextScrnNY=*Off;
          prevScrnNY=*Off;
          cxAddNY=*Off;
          writeClmNY=*Off;
          Exfmt rcdClmDtNY;
          nextScreen=nextScrnNY;
          prevScreen=prevScrnNY;
          cancelAdd=cxAddNY;
          writeClm=writeClmNY;
       // PA - Data
       When froiState='PA';
          nextScrnPA=*Off;
          prevScrnPA=*Off;
          cxAddPA=*Off;
          writeClmPA=*Off;
          Exfmt rcdClmDtPA;
          nextScreen=nextScrnPA;
          prevScreen=prevScrnPA;
          cancelAdd=cxAddPA;
          writeClm=writeClmPA;
       // WV - Data
       When froiState='WV';
          nextScrnWV=*Off;
          prevScrnWV=*Off;
          cxAddWV=*Off;
          writeClmWV=*Off;
          Exfmt rcdClmDtWV;
          nextScreen=nextScrnWV;
          prevScreen=prevScrnWV;
          cancelAdd=cxAddWV;
          writeClm=writeClmWV;
       Other;
          nextScrnNI=*Off;
          prevScrnNI=*Off;
          cxAddNI=*Off;
          writeClmNI=*Off;
          Exfmt rcdClmDtNI;
          nextScreen=nextScrnNI;
          prevScreen=prevScrnNI;
          cancelAdd=cxAddNI;
          writeClm=writeClmNI;
       EndSl;
       //*** Beg Add *** 09/06/22 *******************************
       If froistate <> 'A1';
          Read rcdClmDtA1;
       Endif;
       //*** End Add *** 09/06/22 *******************************

      /End-Free
     P ChooseStates    E
       //===================================================================
       // ErrorCert - Check Errors on Certification Screen
       //===================================================================
     P ErrorCert       B
     D ErrorCert       PI              n
     D error           s               n   inz
     D grpAccess       s              1    inz
      /Free

       //Social Security # must be valid
       If scSocSec#=*Blanks;
          mgSocSec#='SS# must be entered.';
          erSocSec#=*On;
          error=*On;
       EndIf;

       //Accident date must be valid.
       mgAccDate1=' ';
       erAccDate1=*Off;
       If dtCheck(scAccDateI)=0;
          mgAccDate1='Accident date must be entered and valid.';
          erAccDate1=*On;
          error=*On;
       Else;
          dateValueIn=scAccDateI;
          If ChkFutrDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgAccDate1=errorMsg;
             erAccDate1=*On;
             error=*On;
          Else;
             scAccDateI=dtCheck(scAccDateI);
          EndIf;
       EndIf;

       //Group must be entered.
       If scGroup#=0;
          mgGroup#='Group number must be entered.';
          erGroup#=*On;
          error=*On;
       Else;
          //Must have ALL Authority to Add Claim to this Group
          Callp GroupAuth(kkCo#:scGroup#:q1User:grpAccess);
          If grpAccess='I';
             mgGroup#='You are not allowed to add a claim to this group.';
             erGroup#=*On;
             error=*On;
          Else;
             Chain (UserCo#(q1User):scGroup#) pfn00;
             If %Found(WMFNL) And FNexdt <> 0;
                mgGroup#='You cannot add claims to an expired group.';
                erGroup#=*On;
                error=*On;
             EndIf;
          EndIf;
       EndIf;

       //Employer must be entered.
       If scEmpDiv='00000000000000000' or scEmpDiv='                 ';
          mgEmpDiv='Employer number must be entered.';
          erEmpDiv=*On;
          error=*On;
       EndIf;

       return error;

      /End-Free
     P ErrorCert       E
       //===================================================================
       // LoadAWW - Loads the AWW History Data into Aww Grid
       //===================================================================
     P LoadAww         B
     D LoadAww         PI
      /Free

       rrnAww=0;
       clrAww=*On;
       dspAww=*Off;
       write rcdClmDt3;
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
          SetGt key1WDRBL prb00;
          ReadPe key2WDRBL prb00;
          If not %Eof(WDRBL);
             scCalcRatG=AWaww * RBcmpr / 100;
             If scCalcRatG<RBmnrt;
                scCalcRatG=RBmnrt;
             EndIf;
             If scCalcRatG>RBpacp;
                scCalcRatG=RBpacp;
             EndIf;
             scMaxRateG=RBpacp;
          Else;
             scCalcRatG=0;
             scMaxRateG=0;
          EndIf;

          scAwwEfDtG=dtMDYY(AWefDt);
          scAwwRrn=WMAWPrrn;
          shwAwwLnk=*On;
          edtAww=*Off;
             rrnAww=rrnAww+1;
             write sflAww;
          ReadPe key1WMAWL1 paw01;
       EndDo;

      /End-Free
     P LoadAww         E
       //===================================================================
       // LoadBenefit - Loads the Benefit Adjustment Data into Benefit Grid
       //===================================================================
     P LoadBenefit     B
     D LoadBenefit     PI
      /Free

       rrnBenefit=0;
       clrBenefit=*On;
       dspBenefit=*Off;
       write rcdClmDt3;
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
             delBenefit=*Off;
             edtBenefit=*Off;
             shwBeneLnk=*On;

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

       rrnParts=0;
       clrParts=*On;
       dspParts=*Off;
       write rcdClmDt2;
       clrParts=*Off;
       dspParts=*On;

       Chain key1WMBYL pby00;
       If %Found(WMBYL);
          DoU %Eof(WMBYL);
             scPartS=BYkey;
             kkKey=BYkey;
             Chain keyWTABL ppa00;
             If %Found(WTABL);
                scPartDscS=PAdesc;
             Else;
                scPartDscS=*Blanks;
             EndIf;
             scImpairS=BYpct;
             scPartStsS=BYstat;
             scPartSeq=BYseq#;
             scPartRrn=WMBYPrrn;
             delPart=*Off;
             edtPart=*Off;
             shwPartLnk=*On;
             rrnParts=rrnParts+1;
             write sflParts;
             ReadE key1WMBYL pby00;
          EndDo;
       EndIf;

      /End-Free
     P LoadBodyPart    E
       //===================================================================
       // LoadConfClm - Loads the Confirm Claim Screen
       //===================================================================
     P LoadConfClm     B
     D LoadConfClm     PI
     D locFound        s               n   inz
     D programIn       s             10
     D tabNameIn       s             50
     D modeIn          s             50    inz('C')
     D callIn          s             50    inz(' ')
     D rrnOut          s             50    inz('000000000')
     D empRrn          s              9  0 inz(0)
     D selectIn        s           1024
      /Free

       scGroupInf= %Editc(scGroup#:'X') + ' - ' + getGrpName(scGroup#);
       scEmpInf= EmpDivName(%Dec(%Subst(scEmpDiv:1:9):9:0):
        %Dec(%Subst(scEmpDiv:10:5):5:0):empRrn:scGroup#);

       kkCo#=UserCo#(q1User);
       kkFnd=scGroup#;
       kkEmp#=%Dec(%Subst(scEmpDiv:1:9):9:0);
       kkDiv=%Dec(%Subst(scEmpDiv:10:5):5:0);
       Chain keyWMEML pem00;
       If %Found(WMEML);
          If EMpAd1=*Blanks;
             scEmpAdr1=EMmAd1;
             scEmpAdr2=EMmAd2;
             zipCode=EMmZip;
             If zipCode2=0;
                scEmpAdr3=%Trim(EMmCty) + ', ' + EMmSt + ' ' +
                 %EditW(zipCode1:'     ');
             Else;
                scEmpAdr3=%Trim(EMmCty) + ', ' + EMmSt + ' ' +
                 %EditW(EMmZip:'     -    ');
             EndIf;
             If scEmpAdr2=*Blanks;
                scEmpAdr2=scEmpAdr3;
                scEmpAdr3=*Blanks;
             EndIf;
          Else;
             scEmpAdr1=EMpAd1;
             scEmpAdr2=EMpAd2;
             zipCode=EMpZip;
             If zipCode2=0;
                scEmpAdr3=%Trim(EMpCty) + ', ' + EMpSt + ' ' +
                 %EditW(zipCode1:'     ');
             Else;
                scEmpAdr3=%Trim(EMpCty) + ', ' + EMpSt + ' ' +
                 %EditW(EMpZip:'     -    ');
             EndIf;
             If scEmpAdr2=*Blanks;
                scEmpAdr2=scEmpAdr3;
                scEmpAdr3=*Blanks;
             EndIf;
          EndIf;
       EndIf;

       scPolIncDt=dtMDYY(getEmIncDt(kkCo#:kkFnd:kkEmp#:kkDiv));
       polYear=RtvPolYr(kkCo#:kkFnd:kkEmp#:kkDiv:dtCYMD(scAccDateI));
       scPolYear=polYear;

       // Get mailing address info for location
       locFound=GetLocInfo2(kkCo#:kkFnd:kkEmp#:kkDiv:scLocation:xxName:
        xxMad1:xxMad2:xxMzip:xxMcty:xxMst:xxPad1:xxPad2:xxPzip:xxPcty:xxPst:
        xxPhn#:xxArea:xxEfDt:xxDlDt);

       If scLocation<>*Blanks;
          scLocInf=scLocation + ' - ' + xxName;
          If xxPad1<> *Blanks;
             scLocAdr1=xxPad1;
             scLocAdr2=xxPad2;
             zipCode=xxPzip;
             If zipCode2=0;
                scLocAdr3=%Trim(xxPcty) + ', ' + xxPst + ' ' +
                 %EditW(zipCode1:'     ');
             Else;
                scLocAdr3=%Trim(xxPcty) + ', ' + xxPst + ' ' +
                 %EditW(xxPzip:'     -    ');
             EndIf;
             If scLocAdr2=*Blanks;
                scLocAdr2=scLocAdr3;
                scLocAdr3=*Blanks;
             EndIf;
          Else;
             scLocAdr1=xxMad1;
             scLocAdr2=xxMad2;
             zipCode=xxMzip;
             If zipCode2=0;
                scLocAdr3=%Trim(xxMcty) + ', ' + xxMst + ' ' +
                 %EditW(zipCode1:'     ');
             Else;
                scLocAdr3=%Trim(xxMcty) + ', ' + xxMst + ' ' +
                 %EditW(xxMzip:'     -    ');
             EndIf;
             If scLocAdr2=*Blanks;
                scLocAdr2=scLocAdr3;
                scLocAdr3=*Blanks;
             EndIf;
          EndIf;
          scLocEffDt=dtMDYY(xxEfDt);
       Else;
          scLocAdr1=' ';
          scLocAdr2=' ';
          scLocAdr3=' ';
          scLocInf='No Location';
       EndIf;

       showAssign=*Off;
       cancelLeft=415;
       If polYear<>0;
          polFrDt=dtMDYY(RtvPolFrDt(kkCo#:kkFnd:kkEmp#:kkDiv:polYear));
          polToDt=dtMDYY(RtvPolToDt(kkCo#:kkFnd:kkEmp#:kkDiv:polYear));
          scPolPrd=AlphaUSA(CvtToDate(polFrDt)) + ' - ' +
           AlphaUSA(CvtToDate(polToDt));
          cxDate = *Zeros;
          cxDate = GetCxDate(kkCo#:kkFnd:kkEmp#:kkDiv);
          If cxDate = 0 Or (dtCYMD(scAccDateI) < cxDate and cxDate <> 0);
             scAssign='Assign Claim#';
             noCoverage=*Off;
             showAssign=*On;
          Elseif dtCYMD(scAccDateI) >= cxDate and cxDate <> 0;
             scAssign='Override and Assign Claim#';
             noCoverage=*On;
             If ChkAuthClm(kkCo#:kkFnd)=*On;
                showAssign=*On;
             Else;
                showAssign=*Off;
                cancelLeft=600;
             EndIf;
          Endif;
       Else;
          scAssign='Override and Assign Claim#';
          noCoverage=*On;
          scPolPrd='No Policy period';
          polYear=%Dec(%Subst(%EditC(dtCYMD(scAccDateI):'X'):1:3):3:0);
          If ChkAuthClm(kkCo#:kkFnd)=*On;
             showAssign=*On;
          Else;
             showAssign=*Off;
             cancelLeft=600;
          EndIf;
       EndIf;

       If chkDupClm(kkCo#:scSocSec#)=*On;
          dupClaim=*On;
          programIn='WTSEARCH';
          tabNameIn='View Additional Claims';
          clear dsSrchCrit;
          clear selectIn;
          dsClmSS#=scSocSec#;
          selectIn=dsSrchCrit;
          scDupClUrl=CreateBIGUrl(programIn:tabNameIn:modeIn:callIn:
           rrnOut:selectIn);
       Else;
          dupClaim=*Off;
       EndIf;

      /End-Free
     P LoadConfClm     E
       //===================================================================
       // LoadDiagCode - Loads the Diagnosis Data into Diagnosis Grid
       //===================================================================
     P LoadDiagCode    B
     D LoadDiagCode    PI
      /Free

       rrnDiag=0;
       clrDiag=*On;
       dspDiag=*Off;
       write rcdClmDt2;
       clrDiag=*Off;
       dspDiag=*On;

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
             edtDiag=*Off;
             deleteDiag=*Off;
             shwDiagLnk=*On;
             rrnDiag=rrnDiag+1;
             If rrnDiag=1;
                selDiag=*On;
             Else;
                selDiag=*Off;
             EndIf;
             showExclm = *off;
             If (DAicd9 <> *blanks and @@ICDSts = '2')
             or (DAicd1001 <> *blanks and @@ICDSts <> '2');
                showExclm = *on;
             EndIf;
             write sflDiag;
             ReadE key1SADAL pda00;
          EndDo;
       EndIf;

      /End-Free
     P LoadDiagCode    E
       //===================================================================
       // LoadEarnings - Loads the Reduced Earnings Data into Earnings Grid
       //===================================================================
     P LoadEarnings    B
     D LoadEarnings    PI
      /Free

       rrnEarning=0;
       clrEarning=*On;
       dspEarning=*Off;
       write rcdClmDt3;
       clrEarning=*Off;
       dspEarning=*On;

       Chain key1WDR3L pr300;
       If %Found(WDR3L);
          DoU %Eof(WDR3L);
             scWeekNumG=R3reWk;
             scActRedG=R3are;
             scDeemRedG=R3dre;
             scEarnRrn=WDR3Prrn;
             delBenefit=*Off;
             edtBenefit=*Off;
             shwEarnLnk=*On;
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

       rrnImr=0;
       clrImr=*On;
       dspImr=*Off;
       write rcdClmDt0;
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

       rrnMult=0;
       clrMult=*On;
       dspMult=*Off;
       write rcdClmDt4;
       clrMult=*Off;
       dspMult=*On;

       Chain key1WDMSL pms00;
       If %Found(WDMSL);
          DoU %Eof(WDMSL);
             scMthSettO=MScaMs;
             scTpocAmtO=MStpoA;
             scTpocDtO=CvtToDate(MStpoD);
             scDlayDtO=CvtToDate(MSdlyD);
             scMultRrn=WDMSPrrn;
             edtMult=*Off;
             shwMultLnk=*On;
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

       rrnPaid=0;
       clrPaid=*On;
       dspPaid=*Off;
       write rcdClmDt3;
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
             delPaid=*Off;
             edtPaid=*Off;
             shwPaidLnk=*On;
             rrnPaid=rrnPaid+1;
             write sflPaid;
             ReadE key1WDA3L pa300;
          EndDo;
       EndIf;

      /End-Free
     P LoadEmpPaid     E
       //===================================================================
       // LoadScreenSeq - Load Screen Sequence
       //===================================================================
     P LoadScreenSeq   B
     D LoadScreenSeq   PI
      /Free

       Chain inRrnClm pcm0p;
       isSmsVisbl = isModEnabled('RelaySMS');
       Clear screenSeq;
       Clear pcx00;
       Chain keyWMCXL pcx00;

       // Load Array
       For i=1 to %Elem(screenSeq);
          Select;
          When i=1;
             screenSeq(i).screen='clmInfo';
          When i=2;
             screenSeq(i).screen='mgmtInfo';
          When i=3 and CXmedi='Y';
             screenSeq(i).screen='mediInfo';
          When i=4 and CXbeni='Y';
             screenSeq(i).screen='beniInfo';
          When i=5 and CXrps='Y';
             screenSeq(i).screen='rsvInfo';
          When i=6 and CXstSp='Y';
             screenSeq(i).screen='stateInfo';
          When i=7 and CXdBen='Y';
             screenSeq(i).screen='diaryInfo';
      *** Beg Chg *** 08/29/22 *************************************************
          When i=8 and CXprtFroi='Y';
             screenSeq(i).screen='printFROI';
          When i=9 and CXdart='Y';
             screenSeq(i).screen='iDarts';
      //***   When i=8 and CXdart='Y';
      //***       screenSeq(i).screen='iDarts';
      *** End Chg *** 08/29/22 *************************************************
          EndSl;
       Endfor;

       // Remove blank elements.
       For i=1 to %Elem(screenSeq);
          If screenSeq(i).screen=*Blanks;
             For j=i+1 to %Elem(screenSeq);
                If screenSeq(j).screen<>*Blanks;
                   screenSeq(i).screen=screenSeq(j).screen;
                   screenSeq(j).screen=*Blanks;
                   leave;
                EndIf;
             EndFor;
          EndIf;
       EndFor;

       // Assign Button Types.
       // 0=No Buttons
       // 1=Cancel, Finish and Next Buttons
       // 2=Previous, Cancel, Finish and Next Buttons
       // 3=Previous, Cancel and Finish Buttons
       For i=1 to %Elem(screenSeq);
          Select;
          When i=1;
             screenSeq(i).buttonType = 1;
          Other;
             If screenSeq(i).screen <> *Blanks;
                If screenSeq(i+1).screen <> *Blanks And
                 screenSeq(i+1).screen <> 'iDarts';
                   screenSeq(i).buttonType = 2;
                EndIf;
                If screenSeq(i+1).screen <> *Blanks And
                 screenSeq(i+1).screen = 'iDarts';
                   screenSeq(i).buttonType = 3;
                EndIf;
                If screenSeq(i+1).screen = *Blanks;
                   screenSeq(i).buttonType = 3;
                EndIf;
                If screenSeq(i).screen = 'iDarts';
                   screenSeq(i).buttonType = 0;
                EndIf;
             Else;
                screenSeq(i).buttonType = 0;
             EndIf;
          EndSl;
       EndFor;

      /End-Free
     P LoadScreenSeq   E
       //===================================================================
       // LoadTreatPhys - Loads the Treating Phys Data into Diagnosis Grid
       //===================================================================
     P LoadTreatPhys   B
     D LoadTreatPhys   PI
      /Free

       rrnPhys=0;
       clrPhys=*On;
       dspPhys=*Off;
       write rcdClmDt2;
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
             scPhysSeq=TPseq#;
             scPhysRrn=WMTPPrrn;
             delPhys=*Off;
             edtPhys=*Off;
             shwPhysLnk=*On;
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
       //===================================================================
       // LoadTrial - Loads the Trial Information Data into Grid
       //===================================================================
     P LoadTrial       B
     D LoadTrial       PI
      /Free

       rrnTrial=0;
       clrTrial=*On;
       dspTrial=*Off;
       write rcdClmDt4;
       clrTrial=*Off;
       dspTrial=*On;

       Chain key1WMTIL pti00;
       If %Found(WMTIL);
          DoU %Eof(WMTIL);
             scSuitDtO=dtMDYY(TIsuDt);
             scTrTypeO=TItrTp;
             scJudgeO=TIjudg;
             scLegIssO=TIissu;
             scCrtLvlO=TIcLvl;
             scVerdictO=TIverd;
             scVerdDtO=dtMDYY(TIveDt);
             scPetitO=TIpeti;
             scTrialRrn=WMTIPrrn;
             edtTrial=*Off;
             DelTrial=*Off;
             shwTriaLnk=*On;
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

       rrnWitness=0;
       clrWitness=*On;
       dspWitness=*Off;
       write rcdClmDt0;
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
             delWitness=*Off;
             edtWitness=*Off;
             shwWitnLnk=*On;
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

       rrnWork=0;
       clrWork=*On;
       dspWork=*Off;
       write rcdClmDt3;
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
       rrnWrk=0;
       clrWrk=*On;
       dspWrk=*Off;
       write rcdClmDt3;
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
       // Checks to see if location Exist.
       //===================================================================
     P LocExists       B
     fwmall     if   e           k Disk    ReName(pal01:pal00)

     D LocExists       PI              n
     D empDivIn                      17
     D accDateIn                      7  0 Const

     D wmallData       ds                  LikeRec(pal00)
     D keyWMALL        ds                  LikeRec(pal00:*Key)

      /Free
       keyWMALL.ALco#=kkco#;
       keyWMALL.ALFnd=%Dec(%Subst(empDivIn:15:3):3:0);
       keyWMALL.ALEmp#=%Dec(%Subst(empDivIn:1:9):9:0);
       keyWMALL.ALDiv=%Dec(%Subst(empDivIn:10:5):5:0);

       Chain %Kds(keyWMALL:4) pal00 wmallData;
       If %Found(WMALL);
          DoU %Eof(WMALL);
             If wmallData.ALefDt<=accDateIn And
              (wmallData.ALdlDt=0 Or wmallData.ALdlDt>=accDateIn);
                Return *On;
             EndIf;
          ReadE %Kds(keyWMALL:4) pal00 wmallData;
          EndDo;
       EndIf;

       Return *Off;

      /End-Free
     P LocExists       E
      *================================================================
      *LodHstInfo   - Load history data in Reserve tab
      *================================================================
     P LodHstInfo      B
     D LodHstInfo      PI
      /Free

       rrnHst    = 0;
       clrHist   = *on;
       dspHist   = *off;
       Write rcdRsvDtl;
       clrHist   = *off;
       dspHist   = *on;
       inEfDt   = 9999999;

       //Retrive the information to be shown on grid

       Setll key1WMRHL5 prh05;
       Reade key2WMRHL5 prh05;
       Dow Not %Eof(WMRHL5);

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

          rrnHst = rrnHst + 1;
          selHist=*Off;
          If rrnHst=1;
             selHist=*On;
          EndIf;
          Write SflReserve;
          Reade key2Wmrhl5 prh05;
       Enddo;

      /End-Free
     P LodHstInfo      E
      *================================================================
      *LodRsvInfo   - It will load The Reserve Grid
      *================================================================
     P LodRsvInfo      B
     D LodRsvInfo      PI

      *
     C     CXKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
      *
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2

      /free

       rrnPmtSum   = 0;
       ClrReserve = *on;
       dspReserve = *off;
       Write rcdRsvDtl;
       ClrReserve = *off;
       dspReserve = *on;
       intIndTotal = 0;
       intMedTotal = 0;

       //
       //Retrive the information to be shown on grid

       For i = 1 to 10;

          scRsvRemR = rr(i);
          scRsvResC = rt#(i);
          scRsvPmtR = ar(i);
          scRsvTotI = rr(i) + ar(i);
          ti(i)     = rr(i) + ar(i);

          kkKey = RT#(i);
          Chain keyWTABL prt00  ;
            If %found();
               scRsvResT = RTtype ;
            Else ;
               scRsvResT = *Blanks;
            Endif;

          If rt#(i) <> *Blanks;
             rrnPmtSum  = rrnPmtSum + 1;
             Write sflPmtSum;
          Endif;
          clear scWrkURL;
          clear scAddEdtTT;
          isWrkSht = *off;
          scAddEdtLb = 'Add/Edit';

          Chain CMco# pcw00;
          If %found(WMCWL);
             If RTtype = 'C';
                intIndTotal = intIndTotal + scRsvTotI;
             Endif;
             If RTtype = 'M';
                intMedTotal = intMedTotal + scRsvTotI;
             Endif;
          Endif;
       EndFor;

       If ((CMltfl = 'I' and CWinfl = 'Y' and intIndTotal >= CWinth) or
        (CMltfl = 'M' and CWmofl = 'Y' and intMedTotal >= CWmoth)) and
        CMstat <> 'C';
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

       scRsvTtlRR = %Xfoot(rr);
       scRsvTtlMR = %Xfoot(ar);
       scRsvTtlTI = %Xfoot(rr) + %Xfoot(ar);
       If CMstat  = 'O';
         Clmsts = 'Close  ';
         scClmsts = 'O';
       Else;
         Clmsts = 'Reopen ';
         scClmsts = 'C';
       Endif;

      /end-free
     P LodRsvInfo      E
      *================================================================
      *LodStsInfo   - It will load The Status Grid
      *================================================================
     P LodStsInfo      B
     D LodStsInfo      PI
      /free

       inEfdt   = 9999999;
       inTime   = 999999;
       rrnSts   = 0;
       ClrStatus = *on;
       dspStatus = *off;
       Write rcdRsvDtl;
       ClrStatus = *off;
       dspStatus = *on;

       Setll Key1WMCHL5 pch05;
       Reade Key2WMCHL5 pch05;
       Dow Not %Eof(WMCHL5);
          scStsStats = CHocst  ;
          scStsreasn = CHexpl ;
          scstsUser = getUsrName(CHuser) ;
          scStsEffdt = CvtToDate(CHefdt);
          rrnSts  = rrnSts + 1;
          Write SflStsHst;
          Reade Key2WMCHL5 pch05;
       Enddo;

      /end-free
     P LodStsInfo      E
       //===================================================================
       // ResetEdtAddClm - Reset Edit Add Indicators - Claim Info
       //===================================================================
     P ResetEdtAddClm  B
     D ResetEdtAddClm  PI
      /Free


       // Add Indicators
       addWitness=*Off;
       // Deletion Indicators
       delWitness=*Off;
       // Edit Indicators
       edtWitness=*Off;
       // General Indicators
       cancel=*Off;
       // Position Indicators
       posWitness=*Off;

      /End-Free
     P ResetEdtAddClm  E
       //===================================================================
       // ResetEdtAddMed - Reset Edit Add Indicators - Medical Info
       //===================================================================
     P ResetEdtAddMed  B
     D ResetEdtAddMed  PI
      /Free

       // Add Indicators
       addDiag=*Off;
       addPart=*Off;
       addPhys=*Off;
       // Deletion Indicators
       delPhys=*Off;
       deleteDiag=*Off;
       // Edit Indicators
       edtDiag=*Off;
       edtPart=*Off;
       delPart=*Off;
       edtPhys=*Off;
       // General Indicators
       cancel=*Off;
       // Position Indicators
       posPhys=*Off;
       posPart=*Off;
       posDiag=*Off;

       If cancel=*Off and addEdit=*On;
          // Load Body Part Grid
          If addPart=*On Or edtPart=*On or delPart=*On;
             LoadBodyPart();
          EndIf;

          // Load Diagnosis Grid
          If addDiag=*On Or edtDiag=*On Or deleteDiag=*On;
             LoadDiagCode();
          EndIf;

          // Load Treating Physician Grid
          If addPhys=*On Or edtPhys=*On Or delPhys=*On;
             LoadTreatPhys();
          EndIf;
       EndIf;

      /End-Free
     P ResetEdtAddMed  E
       //===================================================================
       // ResetEdtAddBen - Reset Edit Add Indicators - Benefit Info
       //===================================================================
     P ResetEdtAddBen  B
     D ResetEdtAddBen  PI
      /Free

       // Add Indicators
       addAww=*Off;
       addBenefit=*Off;
       addEarning=*Off;
       addPaid=*Off;
       addWork=*Off;
       // Deletion Indicators
       delBenefit=*Off;
       delEarning=*Off;
       delPaid=*Off;
       delWork=*Off;
       delWrk=*Off;
       // Edit Indicators
       edtAww=*Off;
       edtBenefit=*Off;
       edtEarning=*Off;
       edtPaid=*Off;
       edtWork=*Off;
       edtWrk=*Off;
       // General Indicators
       cancel=*Off;
       // Position Indicators
       posWork=*Off;
       posAww=*Off;
       posPaid=*Off;
       posBenefit=*Off;
       posEarning=*Off;

      /End-Free
     P ResetEdtAddBen  E
       //===================================================================
       // ProcClrInfo - Process the Clearance Screen
       //===================================================================
     P ProcClrInfo     B
     D ProcClrInfo     PI
     D error           s               n
     D modeIn          s              1    inz('E')
     D callIn          s              1    inz('2')
     D empRrn          s              9  0
     D selectIn        s           1024
      /Free

       If cancelConf=*Off;
       Else;
          cancelConf=*Off;
       EndIf;
       scGroup#=GetGrpAuthSql(q1User:grpSelect:grpFrom:grpWhere:'A');
       hldGroup#=scGroup#;
       If scGroup#<>0;
          grpRestr=*On;
       EndIf;
       If scEmpDiv='              ';
          scEmpDiv='00000000000000000';
       EndIf;
       noLoc=*On;
       showLoc=*Off;
       If writeClm=*On;
          writeClm=*Off;
          scSocSec#=*Blanks;
          scAccDateI=*Zeros;
          scGroup#=*Zeros;
          scEmpDiv='00000000000000000';
          scEmpDivNm=*Blanks;
          scLocation=*Blanks;
          hldGroup#=*Zeros;
          hldEmpDivNm=*Blanks;
          hldAccDateI=*Zeros;
       EndIf;

       If inRrnFroi <> 0 and inFroiMode=' ';
          If Not %Open(WHFRP);
             Open WhFRP;
          Endif;
          Chain inRrnFroi PFR00;
          If %found(WHFRP);
             scSocSec#=FRss;
             If FRacdt <> ' ';
                scAccDateI=dtMDYY2(%Int(FRacdt));
             Endif;
             scGroup#=FRfnd;
             scEmpDiv=%EditC(FRemp:'X') + %EditC(FRdiv:'X') +
              %Subst(%EditC(FRfnd:'X'):3:3);
             scEmpDivNm=GetEmpName(FRemp:FRdiv:outRRN:FRfnd:FRco);
             scLocation=FRctg1;
             If scLocation <> ' ';
                scLocList='ALELID || ''-'' || TRIM(ALNAME) || ''-'' || ' +
                 'CASE WHEN ALPAD1<>'' '' THEN TRIM(ALPAD1) || '' '' || ' +
                 'TRIM(ALPAD2) || '' '' || TRIM(ALPCTY) || '', '' || ' +
                 'ALPST || '' '' || SUBSTR(DIGITS(ALPZIP),1,5) ELSE ' +
                 'TRIM(ALMAD1) || '' '' || ' +
                 'TRIM(ALMAD2) || '' '' || TRIM(ALMCTY) || '', '' || ' +
                 'ALMST || '' '' || SUBSTR(DIGITS(ALMZIP),1,5) END';

                scLocSel='ALFND=' + %EditC(scGroup#:'X') + ' AND ALEMP#=' +
                 %Subst(scEmpDiv:1:9) + ' AND ALDIV=' + %Subst(scEmpDiv:10:5);

                If dtCYMD(scAccDateI) <> 0;
                   scLocSel=%Trim(scLocSel) + ' AND ALEFDT <= ' +
                    %EditC(dtCYMD(scAccDateI):'X') + ' AND (ALDLDT=0 OR ' +
                    '(ALDLDT<>0 ' + 'AND  ALDLDT>=' +
                     %EditC(dtCYMD(scAccDateI):'X') + '))';
                EndIf;
             EndIf;
             hldGroup#=FRfnd;
             hldEmpDivNm=GetEmpName(FRemp:FRdiv:outRRN:FRfnd:FRco);
             If FRacdt <> ' ';
                hldAccDateI=dtMDYY2(%Int(FRacdt));
             Endif;
          EndIf;
          If %Open(WHFRP);
             Close WHFRP;
          Endif;
       Else;
          If inRrnFroi <> 0 and inFroiMode='T';

             inRrnTFFRP=inRrnFroi;
             inModeTFFRP='1';
             outRrnWMCMP=0;
             outSocSec#=' ';
             outAccDate=0;
             outCompany#=0;
             outGroup=0;
             outEmployer#=0;
             outDivision=0;
             TasbFroiInfo(inRrnTFFRP:inModeTFFRP:outRrnWMCMP:outSocSec#:
                          outAccDate:outCompany#:outGroup:outEmployer#:
                          outDivision);

             scSocSec#=outSocSec#;
             If outAccDate <> 0;
                scAccDateI=dtMDYY(outAccDate);
             Endif;
             scGroup#=outGroup;
             scEmpDiv=%EditC(outEmployer#:'X') + %EditC(outDivision:'X') +
              %EditC(outGroup:'X');
             scEmpDivNm=GetEmpName(outEmployer#:outDivision:outRRN:outGroup:
                                   outCompany#);
             scLocation=*Blanks;
             hldGroup#=outGroup;
             hldEmpDivNm=scEmpDivNm;
             hldAccDateI=scAccDateI;

          EndIf;
       EndIf;

       DoU cancelAdd1=*On or (nextStep=*On and error=*Off);

          error=*Off;
          nextScreen=*Off;
          cancelAdd1=*Off;
          writeClm=*Off;
          kkCo#=UserCo#(q1User);

          showLoc=locExists(scEmpDiv:dtCYMD(scAccDateI));
          If showLoc=*On;
             noLoc=*Off;
          Else;
             scLocation=*Blanks;
             noLoc=*On;
          EndIf;
          // Set focus
          fcGroup#=*Off;
          fcEmpDivNm=*Off;
          fcLocation=*Off;
          If hldAccDateI<>scAccDateI;
             fcGroup#=*On;
          EndIf;
          If hldGroup#<>scGroup#;
             fcEmpDivNm=*On;
             scLocation=*Blanks;
          EndIf;
          If selEmpSrch=*On and hldEmpDivNm=hldEmpDivNm;
             fcEmpDivNm=*On;
          EndIf;
          If hldEmpDivNm<>scEmpDivNm;
             scLocation=*Blanks;
             If noLoc=*On;
                fcEmpDivNm=*On;
             Else;
                fcLocation=*On;
             EndIf;
          EndIf;

          hldAccDateI=scAccDateI;
          hldGroup#=scGroup#;
          hldEmpDivNm=scEmpDivNm;
          selEmpSrch=*Off;

          scSocSec# = MaskData('SSN':'scSocSec#':scSocSec#);
          Write rcdBlank;
          ExFmt rcdClrInfo;
          scSocSec# = UnMaskData('SSN':'scSocSec#':scSocSec#);
          // Reset Error Feilds
          mgSocSec#=' ';
          erSocSec#=*Off;
          mgAccDate1=' ';
          erAccDate1=*Off;
          mgGroup#=' ';
          erGroup#=*Off;
          mgEmpDiv=' ';
          erEmpDiv=*Off;
          // Check to see if enter was hit
          If hldAccDateI=scAccDateI And hldGroup#=scGroup# And
           selEmpSrch=*Off And hldEmpDivNm=hldEmpDivNm And
           cancelAdd1=*Off;
             nextStep=*On;
          EndIf;
          // Now select the mode
          Select;
          When cancelAdd1=*On;
          When nextStep=*On;
             If scEmpDivNm<>hldEmpDivNm;
                scEmpDiv='00000000000000000';
             EndIf;
             error=ErrorCert();
          Other;
             If selEmpSrch=*On;
                clear dsSrchCrit;
                clear selectIn;
                dsEmpGrp=scGroup#;
                dsEmpName=scEmpDivNm;
                selectIn=dsSrchCrit;
                rrnOut='000000000';
                EmpSearch(modeIn:callIn:rrnOut:selectIn);
                If rrnOut<>'000000000';
                   empRrn=%Dec(rrnOut:9:0);
                   Chain empRrn pel0p;
                   If %Found(WDELP);
                      kkCo#=ELco#;
                      kkFnd=ELfnd;
                      kkEmp#=ELEmp#;
                      kkDiv=ELdiv;
                      Chain keyWMEML pem00;
                      If %Found(WMEML);
                         scEmpDivNm=EMname;
                         scGroup#=EMfnd;
                         scEmpDiv=%EditC(EMemp#:'X') + %EditC(EMdiv:'X') +
                          %EditC(EMfnd:'X');
                         Chain keyWMRSL prs00;
                         If %Found(WMRSL);
                            jurState=RSjrSt;
                         Else;
                            jurState=*Blanks;
                         EndIf;
                      EndIf;
                   EndIf;
                EndIf;
             EndIf;

             scAccDateI=dtCheck(scAccDateI);
             If (scEmpDivNm=' ' Or scEmpDivNm<>hldEmpDivNm) And selEmpSrch=*Off
              And ((scGroup#=HldGroup# And scGroup#<>0) Or scGroup#=0);
                If grpRestr=*Off And scGroup#<>0;
                   scGroup#=0;
                EndIf;
                scEmpDiv='00000000000000000';
             EndIf;

             If scEmpDiv<>'00000000000000000' and scEmpDiv<>'                 '
              and scEmpDivNm <> ' ';
                scGroup#=%Dec(%Subst(scEmpDiv:15:3):3:0);
             EndIf;

             If scEmpDiv<>'00000000000000000' and scEmpDiv<>'                 ';


                scLocList='ALELID || ''-'' || TRIM(ALNAME) || ''-'' || ' +
                 'CASE WHEN ALPAD1<>'' '' THEN TRIM(ALPAD1) || '' '' || ' +
                 'TRIM(ALPAD2) || '' '' || TRIM(ALPCTY) || '', '' || ' +
                 'ALPST || '' '' || SUBSTR(DIGITS(ALPZIP),1,5) ELSE ' +
                 'TRIM(ALMAD1) || '' '' || ' +
                 'TRIM(ALMAD2) || '' '' || TRIM(ALMCTY) || '', '' || ' +
                 'ALMST || '' '' || SUBSTR(DIGITS(ALMZIP),1,5) END';

                scLocSel='ALFND=' + %EditC(scGroup#:'X') + ' AND ALEMP#=' +
                 %Subst(scEmpDiv:1:9) + ' AND ALDIV=' + %Subst(scEmpDiv:10:5);

                If dtCYMD(scAccDateI) <> 0;
                   scLocSel=%Trim(scLocSel) + ' AND ALEFDT <= ' +
                    %EditC(dtCYMD(scAccDateI):'X') + ' AND (ALDLDT=0 OR ' +
                    '(ALDLDT<>0 ' + 'AND  ALDLDT>=' +
                    %EditC(dtCYMD(scAccDateI):'X') + '))';
                EndIf;
             Else;
                noLoc=*On;
                showLoc=*Off;
             EndIf;
          EndSl;
       EndDo;

      /End-Free
     P ProcClrInfo     E
       //===================================================================
       // ProcConfClaim - Process the Confirmation Screen
       //===================================================================
     P ProcConfClaim   B
     D ProcConfClaim   PI
      /Free

       LoadConfClm();

       scSocSec# = MaskData('SSN':'scSocSec#':scSocSec#);

       Write rcdBlank;
       ExFmt rcdConfClm;

       scSocSec# = UnMaskData('SSN':'scSocSec#':scSocSec#);

       If cancelConf=*Off;
          If inFroiMode='T';

             inRrnTFFRP=inRrnFroi;
             inModeTFFRP='2';
             outRrnWMCMP=0;
             outSocSec#=scSocSec#;
             outAccDate=dtCYMD(scAccDateI);
             outCompany#=kkCo#;
             outGroup=kkFnd;
             outEmployer#=kkEmp#;
             outDivision=kkDiv;
             TasbFroiInfo(inRrnTFFRP:inModeTFFRP:outRrnWMCMP:outSocSec#:
                          outAccDate:outCompany#:outGroup:outEmployer#:
                          outDivision);

             inRrnClm=outRrnWMCMP;
             If q1prms > 2;
                @@RrnWMCMP=outRrnWMCMP;
             EndIf;
          Else;
             inRrnClm= WriteClaim(kkCo#:kkFnd:polYear:kkEmp#:kkDiv:
                       scLocation:EMoff#:scSocSec#:dtCYMD(scAccDateI):
                       jurState:EMsic);
          EndIf;
       EndIf;

      /End-Free
     P ProcConfClaim   E
       //===================================================================
       // ProcClmInfo - Process the Claim Info Screen
       //===================================================================
     P ProcClmInfo     B
     D ProcClmInfo     PI             3  0
     D stepIn                         3  0
     Dstep             s              3  0 Inz(1)
     Derror            s               n
      /Free

       eval fcClmInfo=*On;
       step=stepIn;
       AssignBtn(step);

       If showNext=*On;
          showNext00=*On;
       Else;
          showNext00=*Off;
       EndIf;

       If showPrev=*On;
          showPrev00=*On;
       Else;
          showPrev00=*Off;
       EndIf;

       screenPtr=%Addr(claimInfo);
       If inFroiMode='T';
          hdRrnFroi=inRrnFroi;
          inRrnFroi=0;
       EndIf;
       LoadClmInfo(inRrnClm:screenPtr:inRrnFroi);
       If inFroiMode='T';
          inRrnFroi=hdRrnFroi;
       EndIf;
       // Conditioning CSI and non-CSI fields based on group
       If outClientID = '114' and (kkfnd = 440 or kkfnd = 460);
          isCSIG = *on;
          If kkfnd = 440;
             ccopt1 = '093';
             ccopt2 = '094';
          else;
             ccopt1 = '100';
             ccopt2 = '101';
          EndIf;
       else;
          isCSIG = *off;
       EndIf;


       DoU ((nextScreen=*On Or writeClm=*On) And error=*Off) Or
        cancelAdd=*On Or prevScreen=*On;

          // Load the most current claim record
          If error=*Off;
             Chain inRrnClm pcm0p;
          EndIf;

          // Load I/M/R History Grid
          LoadImr();

          // Load Witness Grid
          LoadWitness();

          // Reset Edit Add
          ResetEdtAddClm();

          error=*Off;
          nextScrn00=*Off;
          prevScrn00=*Off;
          cxAdd00=*Off;
          writeClm00=*Off;

          Write rcdBlank;
          scJob = q1job;
          scJobn= q1jobn;
          scCusr= q1cusr;
          scCo# = kkCo#;
          scMod = 'A';
          scRrn# = inRrnClm;

          ExFmt rcdClmDt0;

          eval fcClmInfo=*Off;
          nextScreen=nextScrn00;
          prevScreen=prevScrn00;
          cancelAdd=cxAdd00;
          writeClm=writeClm00;

          chkAddEdtClm();
          If writeClm=*On Or nextScreen=*On;
             error=ChkClmUpdated(inRrnClm:CMdt:CMtime);
             If error =*Off;
                error=ErrorCheck0(inRrnClm:screenPtr);
             EndIf;
             If error = *Off;
                If inFroiMode='T';
                   hdRrnFroi=inRrnFroi;
                   inRrnFroi=0;
                EndIf;
                UpdClmInfo(inRrnClm:screenPtr:inRrnFroi);
                If inFroiMode='T';
                   inRrnFroi=hdRrnFroi;
                EndIf;
                Chain inRrnClm pcm0p;
             EndIf;
          EndIf;
       EndDo;

       Select;
       When prevScreen=*On;
          Return step-1;
       When nextScreen=*On;
          Return step+1;
       Other;
          Return step;
       Endsl;

      /End-Free
     P ProcClmInfo     E
       //===================================================================
       // ProcMgmtInfo - Process the Management Info Screen
       //===================================================================
     P ProcMgmtInfo    B
     D ProcMgmtInfo    PI             3  0
     D stepIn                         3  0
     Dstep             s              3  0 Inz(1)
     Derror            s               n
     DintDay           s              5  0 Inz
     DintDate          s              8  0 Inz
     DintSeq#          s              5  0 Inz
     DintCdSq          s              5  0 Inz
     DintMdSq          s              5  0 Inz
     DintSdSq          s              5  0 Inz
     DintBdSq          s              5  0 Inz
     DintUdSq          s              5  0 Inz
     DintAdSq          s              5  0 Inz
     DintADat          s              7  0 Inz
     DintUDat          s              7  0 Inz
     DintMDat          s              7  0 Inz
     DintSDat          s              7  0 Inz
     DintCDat          s              7  0 Inz
     DintBDat          s              7  0 Inz
     DintRcVd          s              8  0 Inz
     D##staf           s              3
     DQQDKey           s             10
     DQQcomm           s            256
      /Free

       eval fcMgtInfo=*On;
       step=stepIn;
       AssignBtn(step);

       If showNext=*On;
          showNext01=*On;
       Else;
          showNext01=*Off;
       EndIf;

       If showPrev=*On;
          showPrev01=*On;
       Else;
          showPrev01=*Off;
       EndIf;

       screenPtr=%Addr(mgmtInfo);
       LoadMgmtInfo(inRrnClm:screenPtr);
       scAddScr = 'A';
       scJob1   = q1job;
       scJobn1  = q1jobn;
       scCusr1  = q1cusr;

       DoU ((nextScreen=*On Or writeClm=*On) And error=*Off) Or
        cancelAdd=*On Or prevScreen=*On;

          // Load the most current claim record
          If error=*Off;
             Chain inRrnClm pcm0p;
          EndIf;

          error=*Off;
          nextScrn01=*Off;
          prevScrn01=*Off;
          cxAdd01=*Off;
          writeClm01=*Off;

          Write rcdBlank;
          ExFmt rcdClmDt1;

          scAddScr = ' ';
          eval fcMgtInfo=*Off;
          nextScreen=nextScrn01;
          prevScreen=prevScrn01;
          cancelAdd=cxAdd01;
          writeClm=writeClm01;

          If addEdit=*Off And prevScreen=*Off And writeClm=*Off And
             cancelAdd=*Off And showNext=*On;
             nextScreen=*On;
          EndIf;

          If writeClm=*On Or nextScreen=*On;
             error=ChkClmUpdated(inRrnClm:CMdt:CMtime);
             If error =*Off;
                error=ErrorCheck1(inRrnClm:screenPtr:mode);
             EndIf;
             If error = *Off;
                UpdMgmtInfo(inRrnClm:screenPtr);
                If scCsNurse <> *blanks and svCsNurse = *blanks and
                   outclientid = '114';
                   q#key=%EditC(CMco#:'X') + %EditC(CMfnd:'X') +
                   %EditC(CMfYr:'X') + %EditC(CMcase:'X');
                   q#comm=*blanks;
                   q#cDat=%EditC(DateTo8(systemDate()):'X');
                   q#dcId='F0269C1';
                   q#secl= *Blanks;
                   q#user=GetUserId(scCsNurse);
                   q#pgm=Q1pgm;
                   q#fold=q#key;
                   q#diFl='Y';
                   q#iPgm=Q1pgm;
                   q#fl01= 'Y';
                   q#fl02= 'N';
                   q#rmCd='MCIRF';
                   q#seq#= *blanks;
                   WriteDarts(q#key:q#comm:q#cDat:q#dcId:q#pNam:q#secl:q#user:
                   q#pgm:q#fold:q#diFl:q#iPgm:q#fl01:q#fl02:q#rmCd:q#seq#:
                   q#sbyl:q#com2:q#apId:q#iDoc:q#iQue:q#imgF:q#iPth:q#pIdx:
                   q#qPty:q#dcTp:q#csId:q#code);
                EndIf;
                Chain inRrnClm pcm0p;
                Chain(n) keyWMDDL pdd00;
                If not %found(WMDDL);
                   intCdSq = *zeros;
                   intMdSq = *zeros;
                   intSdSq = *zeros;
                   intBdSq = *zeros;
                   intUdSq = *zeros;
                   intADat = *zeros;
                   intUDat = *zeros;
                   intMDat = *zeros;
                   intSDat = *zeros;
                   intCDat = *zeros;
                   intBDat = *zeros;
                   intRcVd = *zeros;

                   If CXrads <> *blanks and CMltfl = 'M';
                      q#rmCd = CXrads;
                      ##staf = CMclex;
                      intDay = CXbadj;
                      intDate = dtFuture(DateTo8(systemDate()):intDay);
                      intMDat = dtCYMD(intDate);
                      exsr srddry;
                      intMdSq = intSeq#;
                   Endif;

                   // If this is an indemnity claim, create the Supervisor and
                   // Manager diaries.
                   If CMltfl = 'I' or outClientID = '025';
                      intRcVd = dtMDYY(CMrcvd);
                      If CXrssd <> *blanks;
                         intDay = CXbspv;
                         If intRcVd = 0 or outClientID = '025';
                            intDate = dtFuture(DateTo8(systemDate()):intDay);
                         Else;
                            intDate = dtFuture(intRcVd:intDay);
                         Endif;
                         q#rmCd = CXrssd;
                         ##staf = CMsupr;
                         intSDat = dtCYMD(intDate);
                         exsr srddry;
                         intSdSq = intSeq#;
                      Endif;

                      If CXrumd <> *blanks;
                         intDay = CXbmgr;
                         If intRcVd = 0 or outClientID = '025';
                            intDate = dtFuture(DateTo8(systemDate()):intDay);
                         Else;
                            intDate = dtFuture(intRcVd:intDay);
                         Endif;
                         q#rmCd = CXrumd;
                         ##staf = CMuman;
                         intUDat = dtCYMD(intDate);
                         exsr srddry;
                         intUdSq = intSeq#;
                      Endif;
                   Endif;

                   // Check parameter to write Adjuster diary entry
                   If CXrad2 <> *blanks and outClientID = '025';
                      intDay = CXbad2;
                      intDate = dtFuture(DateTo8(systemDate()):intDay);
                      q#rmCd = CXrad2;
                      ##staf = CMclex;
                      intADat = dtCYMD(intDate);
                      exsr srddry;
                      intAdSq = intSeq#;
                   Endif;

                   // SR call checks for different conditions to see if
                   // diary write is necessary.
                   If outClientID = '025';
                      intRcVd = dtMDYY(CMrcvd);
                      If CXafa <> *blanks and CMltfl <> 'R';
                         intDay = CXafad;
                         intDate = dtFuture(intRcVd:intDay);
                         q#rmCd = CXafa;
                         ##staf = CMsupr;
                         intSDat = dtCYMD(intDate);
                         exsr srddry;
                         intSdSq = intSeq#;

                         // write Adjuster diary entry
                         ##staf = CMclex;
                         intADat = dtCYMD(intDate);
                         exsr srddry;
                         intAdSq = intSeq#;
                      Endif;
                   Endif;

                   DDco# = CMco#;
                   DDfnd = CMfnd;
                   DDfyr = CMfyr;
                   DDcase = CMcase;
                   DDmdat = intMDat;
                   DDsdat = intSDat;
                   DDbdat = intBDat;
                   DDudat = intUDat;
                   DDmdsq = intMdSq;
                   DDsdsq = intSdSq;
                   DDbdsq = intBdSq;
                   DDudsq = intUdSq;
                   DDclex = CMclex;
                   DDstat = *Blanks;
                   DDuser = q1user;
                   DDpgm = q1pgm;
                   DDtime = TimeTo6(SystemTime());
                   DDdt = DateTo7(SystemDate());
                   write pdd00;

                   QQDKey = 'NEWCLAIM';
                   QQcomm = *blanks;
                   WrtDrtRemark(CMco#:CMfnd:CMfyr:CMcase:QQDKey:QQcomm);

                Endif;

                // Create the claim index suspense record if in ADD mode, the
                // claim index DARTS remark code is not blank, and the suspense
                // record does not already exist.
                If outClientID = '025';
                   Chain(n) keyWMDDL pid00;
                   If not %found and CXaic <> *blanks;
                      IDco# = CMco#;
                      IDfnd = CMfnd;
                      IDfyr = CMfyr;
                      IDcase = CMcase;
                      IDclex = *blanks;
                      intDay = CXaicd;
                      intDate = dtFuture(DateTo8(systemDate()):intDay);
                      IDasdt = dtCYMD(intDate);
                      IDuser = q1user;
                      IDpgm = q1pgm;
                      IDtime = TimeTo6(SystemTime());
                      IDdt = DateTo7(SystemDate());
                      IDiusr = q1user;
                      IDidt = IDdt;
                      write pid00;
                   Endif;
                Endif;

             EndIf;
          EndIf;
       EndDo;

       Select;
       When prevScreen=*On;
          Return step-1;
       When nextScreen=*On;
          Return step+1;
       Other;
          Return step;
       Endsl;

       //=============================================================
       // SRDDRY - This subroutine writes DARTS Diary records.
       //=============================================================
       Begsr srddry;

          q#key = *blanks;
          q#cDat = %EditC(intDate:'X');
          q#key = %EditC(CMco#:'X') + %EditC(CMfnd:'X') +
           %EditC(CMfYr:'X') + %EditC(CMcase:'X');
          Chain ##staf pup01;
          If %found and ##staf <> *blanks;
             Chain q#rmcd pct01;
             If %found;
                q#comm = CTdesc;
                q#com2 = *blanks;
                q#dcId = CTdepg;
                q#pNam = 'W0070R';
                q#secl = *Blanks;
                q#user = UPusr;
                q#pgm = q1pgm;
                q#ipgm = q1pgm;
                If q#dcid <> *blanks;
                   q#fold = q#key;
                   q#fl01 = 'Y';
                Else;
                   q#fold = *blanks;
                   q#fl01 = *blanks;
                Endif;
                q#diFl = CTfl01;
                If CTdprg > 0;
                   q#fl02 = 'Y';
                Else;
                   q#fl02 = *blanks;
                Endif;
                q#seq# = *blanks;
                q#sbyl = 'N';
                q#apId = *Blanks;
                q#iDoc = *Blanks;
                q#iQue = *Blanks;
                q#imgF = *Blanks;
                q#iPth = *Blanks;
                q#pIdx = *Blanks;
                q#qPty = *Blanks;
                q#dcTp = *Blanks;
                q#csId = *Blanks;
                q#code = *Blanks;

                WriteDarts(q#key:q#comm:q#cDat:q#dcId:q#pNam:q#secl:q#user:
                 q#pgm:q#fold:q#diFl:q#iPgm:q#fl01:q#fl02:q#rmCd:q#seq#:q#sbyl:
                 q#com2:q#apId:q#iDoc:q#iQue:q#imgF:q#iPth:q#pIdx:q#qPty:
                 q#dcTp:q#csId:q#code);

                 intseq# = %int(q#seq#);
             Endif;
          Endif;
       Endsr;
       //=============================================================
      /End-Free
     P ProcMgmtInfo    E
       //===================================================================
       // ProcMediInfo - Process the Medical Info Screen
       //===================================================================
     P ProcMediInfo    B
     D ProcMediInfo    PI             3  0
     D stepIn                         3  0
     Dstep             s              3  0 Inz(1)
     Derror            s               n
      /Free

       fcMedInfo=*On;
       step=stepIn;
       AssignBtn(step);

       If showNext=*On;
          showNext02=*On;
       Else;
          showNext02=*Off;
       EndIf;

       If showPrev=*On;
          showPrev02=*On;
       Else;
          showPrev02=*Off;
       EndIf;

       // check if ICD-9 or ICD-10 codes are active
       @@ICDSts = ' ';
       GetICDSts(CMacdt:@@ICDSts);

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
       //****** end add ****** 04/20/15 ****************************   *****

       // Populate job fields
       q1jobmed=q1job;
       q1jobnmed=q1jobn;
       q1cusrmed=q1cusr;

       screenPtr=%Addr(medicalInfo);
       If inFroiMode='T';
          hdRrnFroi=inRrnFroi;
          inRrnFroi=0;
       EndIf;
       LoadMedInfo(inRrnClm:screenPtr:inRrnFroi);
       If inFroiMode='T';
          inRrnFroi=hdRrnFroi;
       EndIf;

       DoU ((nextScreen=*On Or writeClm=*On) And error=*Off) Or
        cancelAdd=*On Or prevScreen=*On;

          // Load the most current claim record
          If error=*Off;
             Chain inRrnClm pcm0p;
          EndIf;

          // Load Body Part Grid
          LoadBodyPart();

          // Load Diagnosis Grid
          LoadDiagCode();

          // Load Treating Physician Grid
          LoadTreatPhys();

          // Reset Edit Add
          ResetEdtAddMed();

          error=*Off;
          nextScrn02=*Off;
          prevScrn02=*Off;
          cxAdd02=*Off;
          writeClm02=*Off;

          Write rcdBlank;
          ExFmt rcdClmDt2;

          fcMedInfo=*Off;
          nextScreen=nextScrn02;
          prevScreen=prevScrn02;
          cancelAdd=cxAdd02;
          writeClm=writeClm02;

          chkAddEdtMed();
          If writeClm=*On Or nextScreen=*On;
             error=ChkClmUpdated(inRrnClm:CMdt:CMtime);
             If error =*Off;
                error=ErrorCheck2(inRrnClm:screenPtr);
             EndIf;
             If error = *Off;
                UpdMedInfo(inRrnClm:screenPtr);
                Chain inRrnClm pcm0p;
             EndIf;
          EndIf;
       EndDo;

       Select;
       When prevScreen=*On;
          Return step-1;
       When nextScreen=*On;
          Return step+1;
       Other;
          Return step;
       Endsl;

      /End-Free
     P ProcMediInfo    E
       //===================================================================
       // ProcBeneInfo - Process the Benefit Info Screen
       //===================================================================
     P ProcBeneInfo    B
     D ProcBeneInfo    PI             3  0
     D stepIn                         3  0
     Dstep             s              3  0 Inz(1)
     Derror            s               n
      /Free

       fcBenInfo=*On;
       step=stepIn;
       AssignBtn(step);

       If showNext=*On;
          showNext03=*On;
       Else;
          showNext03=*Off;
       EndIf;

       If showPrev=*On;
          showPrev03=*On;
       Else;
          showPrev03=*Off;
       EndIf;

       screenPtr=%Addr(benefitInfo);
       If inFroiMode='T';
          hdRrnFroi=inRrnFroi;
          inRrnFroi=0;
       EndIf;
       LoadBenInfo(inRrnClm:screenPtr:inRrnFroi);
       If inFroiMode='T';
          inRrnFroi=hdRrnFroi;
       EndIf;

       DoU ((nextScreen=*On Or writeClm=*On) And error=*Off) Or
        cancelAdd=*On Or prevScreen=*On;

          // Load the most current claim record
          If error=*Off;
             Chain inRrnClm pcm0p;
          EndIf;

          // Load AWW History Grid
          LoadAww();

          // Load Benefit Adjustment Grid
          LoadBenefit();

          // Load Reduced Earnings Grid
          LoadEarnings();

          // Load Employer Paid Grid
          LoadEmpPaid();

          // Load Work History Grid
          If outClientID = '114';
             dspWrkGrdC = *On;
             dspWrkGrd  = *Off;
             LoadWrkGrd();
          Else;
             dspWrkGrdC = *Off;
             dspWrkGrd  = *On;
             LoadWork();
          Endif;

          // Reset Edit Add
          ResetEdtAddBen();

          error=*Off;
          nextScrn03=*Off;
          prevScrn03=*Off;
          cxAdd03=*Off;
          writeClm03=*Off;

          Write rcdBlank;
          ExFmt rcdClmDt3;

          fcBenInfo=*Off;
          nextScreen=nextScrn03;
          prevScreen=prevScrn03;
          cancelAdd=cxAdd03;
          writeClm=writeClm03;

          chkAddEdtBen();
          If writeClm=*On Or nextScreen=*On;
             error=ChkClmUpdated(inRrnClm:CMdt:CMtime);
             If error =*Off;
                //*** Beg Chg *** 04/10/24 **************************
                //***error=ErrorCheck3(inRrnClm:screenPtr);
                screenPtr2=%Addr(claimInfo);
                error=errorCheck3(inRrnClm:screenPtr:screenPtr2);
                //*** End Chg *** 04/10/24 **************************
             EndIf;
             If error = *Off;
                UpdBenInfo(inRrnClm:screenPtr);
                Chain inRrnClm pcm0p;
             EndIf;
          EndIf;
       EndDo;

       Select;
       When prevScreen=*On;
          Return step-1;
       When nextScreen=*On;
          Return step+1;
       Other;
          Return step;
       Endsl;

      /End-Free
     P ProcBeneInfo    E
       //===================================================================
       // ProcRsvInfo - Process the Reserve Info Screen
       //===================================================================
     P ProcRsvInfo     B
     D ProcRsvInfo     PI             3  0
     D stepIn                         3  0
     Dstep             s              3  0 Inz(1)
     Derror            s               n
      /Free

       fcRsvInfo=*On;
       step=stepIn;
       AssignBtn(step);

       If showNext=*On;
          showNext07=*On;
       Else;
          showNext07=*Off;
       EndIf;

       If showPrev=*On;
          showPrev07=*On;
       Else;
          showPrev07=*Off;
       EndIf;

       screenPtr=%Addr(rsvInfo);
       DoU ((nextScreen=*On Or writeClm=*On) And error=*Off) Or
        cancelAdd=*On Or prevScreen=*On;

          // Load the most current claim record
          If error=*Off;
             Chain inRrnClm pcm0p;
             shwRsvAdd=*On;
             If CMltFl='R';
                shwRsvAdd=*Off;
             Endif;
          EndIf;

          // Load Payment History Grid
          LodRsvInfo();

          If outClientID = '129';

             Chain keyWMcxl pcx00;
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
                          'Medical' + ',' + '     Legal' + ',' + 'Expense' +
                          ',' + 'User' + ',' + 'Reason' + ',';
             scLEGL = 'Legal';

          EndIf;
          // Load Reserve History Grid
          LodHstInfo();

          // Load Claim Status Grid
          LodStsInfo();

          error=*Off;
          nextScrn07=*Off;
          prevScrn07=*Off;
          cxAdd07=*Off;
          writeClm07=*Off;
          q1jobrsv=q1job;
          q1jobnrsv=q1jobn;
          q1cusrrsv=q1cusr;

          Write rcdBlank;
          ExFmt rcdRsvDtl;

          fcRsvInfo=*Off;
          nextScreen=nextScrn07;
          prevScreen=prevScrn07;
          cancelAdd=cxAdd07;
          writeClm=writeClm07;

          If isWrkSht = *on and AddChgR = *on;
             AddChgR = *off;

             strMsg = 'The Claim has been opened as a Reserve Worksheet in +
              another tab. Once the reserve worksheet is finished, click OK +
              to reload the screen.';

             PopOneBtn(strMsg:'Claim Reserve Worksheet Open');
          Endif;

          If addChgR=*Off And prevScreen=*Off And writeClm=*Off And
             cancelAdd=*Off And showNext=*On;
             nextScreen=*On;
          EndIf;

          If cancelAdd=*Off and prevScreen=*Off;
             If addChgR=*On;
                status='O';
                SelChgRes(inRrnClm:status);
             EndIf;
          EndIf;
       EndDo;

       Select;
       When prevScreen=*On;
          Return step-1;
       When nextScreen=*On;
          Return step+1;
       Other;
          Return step;
       Endsl;

      /End-Free
     P ProcRsvInfo     E
       //===================================================================
       // ProcStateInfo - Process the State Info Screen
       //===================================================================
     P ProcStateInfo   B
     D ProcStateInfo   PI             3  0
     D stepIn                         3  0
     Dstep             s              3  0 Inz(1)
     Derror            s               n
      /Free

       step=stepIn;
       AssignBtn(step);

       // get the FROI Information
       froiState=GetFroiState(CMco#:scJurState);

       // Load State Info Panel - Tab 6
       //*** Beg Add *** 09/06/22 *********************************************
       // Always load A1 Panel Heading
       scEntHdrA1 = 'FROI Information';
       //*** End Add *** 09/06/22 *********************************************
       Select;
       When froiState='A1';
          screenPtr=%Addr(a1Info);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrA1='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************

          If showNext=*On;
             showNextA1=*On;
          Else;
             showNextA1=*Off;
          EndIf;

          If showPrev=*On;
             showPrevA1=*On;
          Else;
             showPrevA1=*Off;
          EndIf;
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 580;
          //*** End Add *** 09/06/22 ******************************************

       When froiState='AL';
          screenPtr=%Addr(alInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrAL='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          If showNext=*On;
             showNextAL=*On;
          Else;
             showNextAL=*Off;
          EndIf;

          If showPrev=*On;
             showPrevAL=*On;
          Else;
             showPrevAL=*Off;
          EndIf;
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 1030;
          //*** End Add *** 09/06/22 ******************************************

       When froiState='CA';
          screenPtr=%Addr(caInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrCA='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 1030;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextCA=*On;
          Else;
             showNextCA=*Off;
          EndIf;

          If showPrev=*On;
             showPrevCA=*On;
          Else;
             showPrevCA=*Off;
          EndIf;

       When froiState='DE';
          screenPtr=%Addr(deInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrDE='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 1185;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextDE=*On;
          Else;
             showNextDE=*Off;
          EndIf;

          If showPrev=*On;
             showPrevDE=*On;
          Else;
             showPrevDE=*Off;
          EndIf;

       When froiState='FL';
          screenPtr=%Addr(flInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrFL='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 885;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextFL=*On;
          Else;
             showNextFL=*Off;
          EndIf;

          If showPrev=*On;
             showPrevFL=*On;
          Else;
             showPrevFL=*Off;
          EndIf;

       When froiState='GA';
          screenPtr=%Addr(gaInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrGA='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 885;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextGA=*On;
          Else;
             showNextGA=*Off;
          EndIf;

          If showPrev=*On;
             showPrevGA=*On;
          Else;
             showPrevGA=*Off;
          EndIf;

       When froiState='LA';
          screenPtr=%Addr(laInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrLA='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 1130;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextLA=*On;
          Else;
             showNextLA=*Off;
          EndIf;

          If showPrev=*On;
             showPrevLA=*On;
          Else;
             showPrevLA=*Off;
          EndIf;

       When froiState='MA';
          screenPtr=%Addr(maInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrMA='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 885;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextMA=*On;
          Else;
             showNextMA=*Off;
          EndIf;

          If showPrev=*On;
             showPrevMA=*On;
          Else;
             showPrevMA=*Off;
          EndIf;

       When froiState='NC';
          screenPtr=%Addr(ncInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrNC='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 1010;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextNC=*On;
          Else;
             showNextNC=*Off;
          EndIf;

          If showPrev=*On;
             showPrevNC=*On;
          Else;
             showPrevNC=*Off;
          EndIf;

       When froiState='NY';
          screenPtr=%Addr(nyInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrNY='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 1475;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextNY=*On;
          Else;
             showNextNY=*Off;
          EndIf;

          If showPrev=*On;
             showPrevNY=*On;
          Else;
             showPrevNY=*Off;
          EndIf;

       When froiState='PA';
          screenPtr=%Addr(paInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrPA='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 1030;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextPA=*On;
          Else;
             showNextPA=*Off;
          EndIf;

          If showPrev=*On;
             showPrevPA=*On;
          Else;
             showPrevPA=*Off;
          EndIf;

       When froiState='WV';
          screenPtr=%Addr(wvInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrWV='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 980;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextWV=*On;
          Else;
             showNextWV=*Off;
          EndIf;

          If showPrev=*On;
             showPrevWV=*On;
          Else;
             showPrevWV=*Off;
          EndIf;

       Other;
          screenPtr=%Addr(niInfo);
          //*** Beg Del *** 09/06/22 ******************************************
          //***scEntHdrNI='State Information' + ' - ' + froiState;
          //*** End Del *** 09/06/22 ******************************************
          //*** Beg Add *** 09/06/22 ******************************************
          topA1Btns = 575;
          //*** End Add *** 09/06/22 ******************************************

          If showNext=*On;
             showNextNI=*On;
          Else;
             showNextNI=*Off;
          EndIf;

          If showPrev=*On;
             showPrevNI=*On;
          Else;
             showPrevNI=*Off;
          EndIf;

       EndSl;
       //*** Beg Add *** 09/06/22 ********************************************
       // Save screen pointer for later use
       SvScreenPtr = screenPtr;
       //*** End Add *** 09/06/22 ********************************************

       If froiState='n/a';
          inState='na';
       Else;
          inState=froiState;
       EndIf;

       LoadStateInfo(inRrnClm:screenPtr:inState);
       //*** Beg Add *** 09/06/22 ********************************************
       // Always load A1 if froi state <> A1 and not na
       If inState <> 'A1';
          screenPtrA1=%Addr(a1Info);
          inStateA1 = 'A1';
          LoadStateInfo(inRrnClm:screenPtrA1:inStateA1);
       Endif;
       //*** End Add *** 09/06/22 ********************************************

       DoU ((nextScreen=*On Or writeClm=*On) And error=*Off) Or
        cancelAdd=*On Or prevScreen=*On;

          // Load the most current claim record
          If error=*Off;
             Chain inRrnClm pcm0p;
          EndIf;

          error=*Off;
          nextScreen=*Off;
          prevScreen=*Off;
          cancelAdd=*Off;
          writeClm=*Off;

          Write rcdBlank;
          ChooseStates();

          If prevScreen=*Off And writeClm=*Off And
             cancelAdd=*Off And showNext=*On;
             nextScreen=*On;
          EndIf;

          If writeClm=*On Or nextScreen=*On;
       // If cancelAdd=*Off and prevScreen=*Off;
             error=ChkClmUpdated(inRrnClm:CMdt:CMtime);
             If error =*Off;
                //*** Beg Chg *** 09/06/22 *************************************
                //***error=ErrorCheck5(inRrnClm:screenPtr:inState);
                screenPtrA1=%Addr(a1Info);
                error = ErrorCheck5(inRrnClm:ScreenPtrA1:ScreenPtr:inState);
                //*** End Chg *** 09/06/22 *************************************
             EndIf;
             If error = *Off;
                //*** Beg Chg *** 09/06/22 ******************************** ***
                //***UpdStateInfo(inRrnClm:screenPtr:inState);
                // If state is A1 (National) just update A1
                If inState = 'A1';
                   screenPtr=%Addr(a1Info);
                   inStateA1 = 'A1';
                   UpdStateInfo(inRrnClm:screenPtr:inStateA1);
                // If state is Not A1 then update state and A1
                Else;
                   screenPtr = svScreenPtr;
                   UpdStateInfo(inRrnClm:screenPtr:inState);
                   screenPtr=%Addr(a1Info);
                   inStateA1 = 'A1';
                   UpdStateInfo(inRrnClm:screenPtr:inStateA1);
                Endif;
                //*** End Chg *** 09/06/22 ******************************** ***
                Chain inRrnClm pcm0p;
             EndIf;
          EndIf;
       EndDo;

       Select;
       When prevScreen=*On;
          Return step-1;
       When nextScreen=*On;
          Return step+1;
       Other;
          Return step;
       Endsl;

      /End-Free
     P ProcStateInfo   E
       //===================================================================
       // ProcDiaryInfo - Process the Diary Info Screen
       //===================================================================
     P ProcDiaryInfo   B
     D ProcDiaryInfo   PI             3  0
     D stepIn                         3  0
     Dstep             s              3  0 Inz(1)
     Derror            s               n
      /Free

       fcQuickD=*On;
       step=stepIn;
       AssignBtn(step);

       If showNext=*On;
          showNext06=*On;
       Else;
          showNext06=*Off;
       EndIf;

       If showPrev=*On;
          showPrev06=*On;
       Else;
          showPrev06=*Off;
       EndIf;

       screenPtr=%Addr(diaryInfo);
       LoadQuickDiary(inRrnClm:screenPtr);

       DoU ((nextScreen=*On Or writeClm=*On) And error=*Off) Or
        cancelAdd=*On Or prevScreen=*On;

          // Load the most current claim record
          If error=*Off;
             Chain inRrnClm pcm0p;
          EndIf;

          error=*Off;
          nextScrn06=*Off;
          prevScrn06=*Off;
          cxAdd06=*Off;
          writeClm06=*Off;

          Write rcdBlank;
          ExFmt rcdClmDt6;

          fcQuickD=*Off;
          nextScreen=nextScrn06;
          prevScreen=prevScrn06;
          cancelAdd=cxAdd06;
          writeClm=writeClm06;

          If prevScreen=*Off And writeClm=*Off And
             cancelAdd=*Off And showNext=*On;
             nextScreen=*On;
          EndIf;

          If writeClm=*On Or nextScreen=*On;
       // If cancelAdd=*Off and prevScreen=*Off;
             error=ChkClmUpdated(inRrnClm:CMdt:CMtime);
             If error =*Off;
                error=ErrorCheck6(inRrnClm:screenPtr);
             EndIf;
             If error = *Off;
                UpdQuickDiary(inRrnClm:screenPtr);
                Chain inRrnClm pcm0p;
             EndIf;
          EndIf;
       EndDo;

       Select;
       When prevScreen=*On;
          Return step-1;
       When nextScreen=*On;
          Return step+1;
       Other;
          Return step;
       Endsl;

      /End-Free
     P ProcDiaryInfo   E
      *** Beg Add *** 08/29/22 *************************************************
       //===================================================================
       // ProcPrintFROI - Process the Print FROI
       //===================================================================
     P ProcPrintFROI   B
     D ProcPrintFROI   PI             3  0
     D stepIn                         3  0
     D
     Dstep             s              3  0 Inz(1)
     Dexit             s              1
     Derror            s               n
      /Free

       fcQuickD=*On;
       step=stepIn;
       AssignBtn(step);

       nextScreen=*off;
       prevScreen=*off;
       cancelAdd=*off;
       writeClm=*on;

       FroiPrint(exit:cancel:mode:inRrnClm:jurState);

       If writeClm=*On Or nextScreen=*On;
       // If cancelAdd=*Off and prevScreen=*Off;
             error=ChkClmUpdated(inRrnClm:CMdt:CMtime);
             If error =*Off;
                Chain inRrnClm pcm0p;
             EndIf;
          EndIf;

       Select;
       When prevScreen=*On;
          Return step-1;
       When nextScreen=*On;
          Return step+1;
       Other;
          Return step;
       Endsl;

      /End-Free
     P ProcPrintFROI   E
      *** End Add *** 08/29/22 *************************************************
       //===================================================================
       // ProcClmScreens - Process the Claim Screens
       //===================================================================
     P ProcClmScreens  B
     D ProcClmScreens  PI
     Dstep             s              3  0 Inz(1)
     Dfinish           s               n   Inz(*On)
     Dexit             s              1
     Dcancel           s              1
     Dmode             s              1    Inz('E')
     DdartsKey         s             40
     DprogramName      s             10    Inz('W0070R')
      /Free

       loadScreenSeq();

       DoU finish=*On;
          DoU nextScreen=*Off and prevScreen=*Off;
             Select;
             When screenSeq(step).screen='clmInfo';
                step=procClmInfo(step);
             When screenSeq(step).screen='mgmtInfo';
                step=procMgmtInfo(step);
             When screenSeq(step).screen='mediInfo';
                step=procMediInfo(step);
             When screenSeq(step).screen='beniInfo';
                step=procBeneInfo(step);
             When screenSeq(step).screen='rsvInfo';
                step=procRsvInfo(step);
             When screenSeq(step).screen='stateInfo';
                step=procStateInfo(step);
             When screenSeq(step).screen='diaryInfo';
             step=procDiaryInfo(step);
      *** Beg Add *** 08/29/22 *************************************************
             When screenSeq(step).screen='printFROI';
             step=procPrintFROI(step);
      *** End Add *** 08/29/22 *************************************************
             EndSl;
             // Create Managed Care Claim
      *** Beg Chg *** 08/29/22 *************************************************
      //***    If screenSeq(step).screen<>'diaryInfo'
      //***     or screenSeq(step).screen<>'stateInfo';
          If screenSeq(step).screen<>'diaryInfo'
           or screenSeq(step).screen<>'stateInfo'
           or screenSeq(step).screen<>'printFROI';
      *** End Chg *** 08/29/22 *************************************************

             If wcmcInst='Y' And wcmcInt='Y'and cancelAdd<>*On
              and prevScreen<>*On;
                Open  wmcdl;
                Chain inRrnClm pcm0p;
                Chain(n) CMss# pcd00;

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

                // Write WCMC Claim Demographic

                CreateWCMCdemo(qmRtCd:qmSS#:qmAcDt:qmCo#:qmSub#:qmFnd:
                 qmFyr:qmCase:qmLsN:qmFsN:qmMid:qmAdr1:qmAdr2:
                 qmCty:qmSt:qmZip:qmDtoB:qmSex:qmArea:qmPhn#:
                 qmMsts:qmWara:qmWph#:qmCtry:qmAdr3:qmCara:qmCphn);

                // Write WCMC Claim Master and History
                CreateWCMC(qmRtCd:qmSS#:qmAcDt:qmCo#:qmSub#:qmFnd:qmFyr:qmCase:
                 qmEmp#:qmDiv:qmAcTm:qmDtRp:qmDtLw:qmAcDs:qmMcon:qmEntD:qmRcvD:
                 qmMMI:qm1a1:qm10a1:qmLtFl:qmScl#:qmMcTm:qmStWk:qmDeDt:qmDeem:
                 qmCmNu:qmCmSu:qmClad:qmRrn:qmLsn:qmFsn:qmMid:qmDtRe:qmClSu:
                 qm#Dep:qmOcDs:qmHiDt:qmEsts:qmWkPt:qmPrex:qmAcD2:qmCvFl:qmMca:
                 qmMmiP:qmOdis:qmYrlx:qmDbDt:qmHrDy:qmDyWk:qmClFa:qmDny:qmImp:
                 qmJrSt:qmPoSt);

                Close wmcdl;
             EndIf;
          EndIf;
          EndDo;

          // If Cancel is selected then Initiate Claim Deletion
          If cancelAdd=*On;
             exit=*Off;
             cancel=*Off;
             If wcmcInst='Y' And wcmcInt='Y';
                ##Co#=%editc(CMco#:'X');
                ##Fnd=%editc(CMfnd:'X');
                ##Fyr=%editc(CMfyr:'X');
                ##Case=%editc(CMcase:'X');

                GetRrn(##Co#:##Fnd:##Fyr:##Case:@@Rrn1:@@Rtcd);
                If @@Rtcd <> 'F';
                   ##Rtn=' ';
                   #Fname='FAT3PQ';
                   #Tname='FAT3PQ';
                   createWRKF(##Rtn:#Fname:#Tname);

                   ##Rtn=' ';
                   #Fname='FAT3LQ';
                   #Tname='FAT3LQ';
                   createWRKF(##Rtn:#Fname:#Tname);
                //Now you call F7090R to do the delete
                   ##Call=' ';
                   ##Kl=*blanks;
                   ##Rrn=@@Rrn1;
                   ##Rrn1=*Zeros;
                   ##Rrn2=*Zeros;
                   ##Rrn3=*Zeros;
                   ##Td='D';
                   ##Flag='X';
                   ##Wcms='Y';

                   delClmMc(##Call:##Kl:##Rrn:##Rrn1:##Rrn2:##Rrn3:
                    ##Td:##Flag:##Wcms);
                Endif;
             Endif;
             If ##Kl <> '1';
                deleteClaim(exit:cancel:mode:inRrnClm);
             Else;
                cancel=*On;
             EndIf;
             If exit=*On or cancel=*On;
                finish=*Off;
             Else;
                finish=*On;
             EndIf;
          Endif;

          // If Finish is selected then Complete the Claim Addition
          If writeClm=*On;
             // Create Managed Care Claim
             If CXdart='Y';
                dartsKey=%EditC(CMco#:'X') + %EditC(CMfnd:'X') +
                 %EditC(CMfYr:'X') + %EditC(CMcase:'X');
                IDarts(dartsKey:programName);
             EndIf;
             // Check to see If Edi Log should be generated
             If IsEdi(inRrnClm)=*On;
                strCompany#=%EditC(CMco#:'X');
                strGroup=%EditC(CMfnd:'X');
                strYear=%EditC(CMfYr:'X');
                strCase=%EditC(CMcase:'X');
                EdiLogApi(strCompany#:strGroup:strYear:strCase:
                 strActFlag:strMessage:strReturnCod);
             EndIf;
             finish=*On;
          Endif;
       EndDo;

      /End-Free
     P ProcClmScreens  E
      *================================================================
      *SRCHG        - Reserve Calculation  -Copied for Grid
      *================================================================
     P SrChg           B
     D SrChg           PI
      /Free
       decCcHg=0;
       decMcHg=0;
       decLcHg=0;
       decEcHg=0;
       kkKey=*Blanks;

       For i=1 to 10;
          If rt(i) <> *Blanks;
             kkKey=rt(i);
             Chain keyWTABL prt00;
             If RTtype='C';
                decCcHg=decCcHg+su(i);
             EndIf;
             If RTtype='M';
                decMcHg=decMcHg+su(i);
             EndIf;
             If RTtype='L';
                decLcHg=decLcHg+su(i);
             EndIf;
             If RTtype='E';
                decEcHg=decEcHg+su(i);
             EndIf;
          EndIf;
       EndFor;

      /End-Free
     P SrChg           E
