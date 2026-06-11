     h**********************************************************************
     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE WPICLMDAT - Profound Claim Data Processes
     h/DEFINE   PROFOUNDUI
     h nomain
     h DFTNAME(WPICLMDAT)
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Has Claim Data Procedures.
     h*
     h*  Created on 6/25/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h* 05/05/25 C:349  Support: 118852 (UBIC)                             *
     h*                 Allow update of payment payee address when the     *
     h*                 payee name was written to the file prior to the    *
     h*                 fix implemented by ticket 118852 that eliminated   *
     h*                 the double blanks when the payee has no middle name*
     h*                                                                    *
     h* 04/08/25 C:333  Support: 118812 (Project: WC2998)                  *
     h*                - Recompiled as DSPF WTCLMDTL changed.              *
     h*                                                                    *
     h* 03/06/25 C:333  Project: WC2998                                    *
     h*                - Changes to CMS Settlement Reporting.              *
     h*                                                                    *
     h* 01/14/25 C:340  Project: WC2995                                    *
     h*                - Add Indemnity Claim Code.                         *
     h*                                                                    *
     h* 03/22/24 C:333  Project: WC2826                                    *
     h*                - Claims Contacts.                                  *
     h*                                                                    *
     h* 04/10/23 C:333 Support: 108889 (UBIC)                              *
     h*                - Recompiled as DSPF WTCLMDTL changed.              *
     h*                                                                    *
     h* 10/11/22  C:333 Project: WC2824 / Ticket #103843                   *
     h*                 - WC2824 - Error when accepting Online FROI        *
     h*                 Interface from Connect.                            *
     h*                                                                    *
     h* 09/06/22  C:079 Project: WC2823                                    *
     h*                 FROI Updates                                       *
     h*                                                                    *
     h* 04/18/22  C:291 Project: WC2852                                    *
     h*                 Change Catastrophe field to dropdown for Cat #13   *
     h*                 if accident date and state meets the criteria      *
     h*                 defined in WTELECAT table.                         *
     h*                                                                    *
     h* 02/14/22  C:079 Project: WC2797                                    *
     h*                 IAIABC 3.1.4 New Fields                            *
     h*                                                                    *
     h* 10/26/21  C:062 Ticket: 98727(146)                                 *
     h*                 Populate GO fields and load class code without     *
     h*                 dropping leading zero                              *
     h*                                                                    *
     h* 07/29/21  C:053 Support: 96188
     h*                 Bypass ELOUTS update on max value for CLT114
     h*                                                                    *
     h* 07/14/21  C:079 Project: WC2618                                    *
     h*                 Update Exclude USL&H Exclude from EDI              *
     h*                                                                    *
     h* 11/13/20  C:180 Project: 133WRV                                    *
     h*                 Add IDC default values                             *
     h*                                                                    *
     h* 04/03/20  C:062 Project: WC2730                                    *
     h*                 Add IAIABC fields:                                 *
     h*                 1) Collective Bargaining Agreement                 *
     h*                 2) Accident Premises Code                          *
     h*                                                                    *
     h* 03/19/20  C:079 Support: 146/88106                                 *
     h*                 Check for blanks in loss condition code            *
     h*                                                                    *
     h* 02/28/20  C:295 Project: 87674                                     *
     h*                 IDC Field not saving.                              *
     h*                                                                    *
     h* 11/25/19  C:079 Project: 152WAW                                    *
     h*                 Added check box fields: ESE, IAQ, Student          *
     h*                 aggresssion, Partial Claim Denial, 120 day    I    *
     h*                 review                                             *
     h*                                                                    *
     h*  10/31/19 C:180 Project: WC2604                                    *
     h*                 Trigger Quarterly transaction for change to        *
     h*                 non-idc state.                                     *
     H*                                                                    *
     H*  08/19/19 C:079 Project: WC2604                                    *
     H*                 Add Medical Ext. Indicator logic                   *
     H*                 Add Pre-Existing Dis. and Dis/LOEC fields          *
     H*                 Add Loss Condition Code field                      *
     h*                                                                    *
     H*  06/17/19 C:180 Support: 71987                                     *
     H*                 Capture class extension                            *
     H*                                                                    *
     h*  03/28/19 C:298 Support: 77603                                     *
     h*                 Fix to remove hardcoding of 19 and compute         *
     h*                 the remaining length for substring and             *
     h*                 use that.                                          *
     H*                                                                    *
     h*  11/26/18 C:079 Project: 136WHX                                    *
     h*                 Recompile only due to screen field changes         *
     h*                                                                    *
     h*  05/30/18 C:180 Project: WC2515                                    *
     h*                 Add EDI Rel 3.1 fields:                            *
     h*                  - State Claim # - Related                         *
     h*                  - Acquired Indemnity Trough Date                  *
     h*                  - Anticipated Wage Loss Indicator                 *
     h*                  - Partial Denial Effective Date                   *
     h*                                                                    *
     h*  05/01/18 C:079 Project: WC1917                                    *
     h*                 Part code changes                                  *
     h*                                                                    *
     h*  03/19/18 C:053 Project: 146WHT                                    *
     h*                 Split Wage Field for Split Funded Claims           *
     h*                                                                    *
     h*  11/10/17 C:291 Support: WC2502                                    *
     h*                 Pull data from the new WCAB# field if not blank    *
     h*                 otherwise pull from the old field. Save data to    *
     h*                 the new WCAB# field.                               *
     H*                                                                    *
     H* 08/15/17 C:33  Project: SY394                                      *
     H*                Update initial reserve flag                         *
     h*                                                                    *
     h*  03/15/17 C:062 Support: 46185                                     *
     h*                 Don't write to WDDIP if no values in report level  *
     h*                 and status                                         *
     h*                                                                    *
     h*  02/14/17 C:212 Project: 90MHY                                     *
     h*                 PBM Eligible changes (added scPBME in rcdClmDt2)   *
     h*
     h*  01/31/17 C:306 Project : WC2410                                  *
     h*                 Modified code for 'Injury Severity Code' to the   *
     h*                 Medical tab.                                      *
     h*                                                                   *
     h*  09/22/16 C:310 Support : 42724                                   *
     h*                 Modified code to send CMS diary as accoriding to  *
     h*                 Group Claim Admin Dart Setup.                     *
     h*                                                                   *
     h*  07/25/16 C:136 Project: 114WYQ
     h*                 Add Disable SMS Text checkbox
     h*
     h*  05/27/16 C:062 Project: 41767(25)
     h*                 Allow Autoletters to work for TASB.
     h*                                                                    *
     h*  05/24/16 C:309 Support: PHW41670                                  *
     h*                 Recompile only                                     *
     h*                                                                    *
     h*  05/09/16 C:300 Project: 114WXV                                    *
     h*                 Add Cell Number to Claimant Demographics.          *
     h*                                                                    *
     h*  04/04/16 C:062 Project: SY251                                     *
     h*                 Correct New to open process.                       *
     h*                                                                    *
     h*  03/07/16 C:300 Support: PHW40147                                  *
     h*                 Payroll class codes in CompSuite Claims            *
     h*                 Don't show Inactive Payroll class codes            *
     h*                                                                    *
     h*  02/08/16 C:306 Project: WC2403                                    *
     h*                 CMS Reporting Edits                                *
     h*                                                                    *
     h*  12/11/15 C:53  Project: C133WPK                                   *
     h*                 Add 2 new fields to screen for 133 only
     h*
     h*  12/02/15 C:309 Project: 25MUD
     h*                 Added a new field on "rcdClmDt0- claim information"
     h*                 screen
     h*
     h*  10/20/15 C:302 Project: WC2364
     h*                 Do not display Non-ratable class codes or
     h*                 occupational desease classes as well.
     h*
     h*  10/01/15 C:308 Project: C114WVO
     h*                 Allow Update to Alternate Claim Number.
     h*
     h*  08/06/15 C:180 Support: 34396
     h*                 Do not populate HICN from claims demographics,
     h*                 this is a display-only field.
     h*
     h*  07/07/15 C:062 Support: 33029(111)
     h*                 Don't load quick diary if diary completed.
     h*
     h*  05/05/15 C:062 PHASEWARE:334638 (136)
     h*                 Correct CA begin work time.
     h*
     h*  04/20/15 C:300 Project: WC2043
     h*                 Modified CMS alleged cause of injury field to use  *
     h*                 ICD-9 or ICD-10 code based on date of injury.      *
     h*
     h*  02/23/15 C:280 PHASEWARE:32428 CSI
     h*                 Fix concurrent employer phone number error
     h*
     h*  01/16/15 C:180 Support: WC114 87637
     h*                 Fix AWW when downloading from WHFRP.
     h*
     h*  11/03/14 C:290 Support: WC146 87202
     h*                 changed the logic so that program will run fine when
     h*                 try to convert blank values into decimal.
     h*
     h*  10/29/14 C:180 Support: WC146 87143
     h*                 Fix value for Death result of injury.
     h*
     h*  10/17/14 C:290 Support: WC114 85446
     h*                 Modified the type ahead in the MCO Fein# field at Medica
     h*                 tab.
     h*
     h*  10/14/14 C:189 Support : WC133 86574
     h*                 Changed the parameter passing to the WA700R program
     h*                 as auto Darts not getting generated for flag changes
     h*
     h*  09/16/14 C:295 Project : 114WMD Priss:25
     h*                 Modified the logic to write darts remarks when name/addr
     h*                 Changes.
     h*
     h* 08/25/14  C:279 Project: 114WOP
     h*                 Added Alternate State Claim # field.
     h*
     h*  08/20/14 C:282 Support: WC045 86046
     h*                 Modified the logic to write darts remarks when name/addr
     h*                 changes.
     h*
     h*  08/04/14 C:283 Support: WC146 84781
     h*                 Added the AJAX handling for Employer Premises
     h*                 checkbox. Populated the hidden fields for
     h*                 Co#, Group# and Employer# on the Claim Detail
     h*                 screen.
     h*                 Removed the handling for SCpremises = 'Y' as
     h*                 is now working from OnClick event.
     h*
     h*  07/22/14 C:298 Support: WC099 85969
     h*                 Added logic to display inactive staff codes
     h*                 if value of field has not changed.
     h*
     h*  07/21/14 C:283 Support: WA045 86221
     h*                 Correct the Mandatory error message for
     h*                 scPayClass and handle the visibility of
     h*                 asterisk's for scPayClass and scNCCI.
     h*
     h*  07/15/14 C:279 Support: 84827
     h*                 Added field Death Result of Injury.
     h*
     h*  07/04/14 C:290 Support: WC114 85446
     h*                 Changed the MCO Fein field to numeric and
     h*                 modified the MCO edits accordingly to accept
     h*                 the blank and all zeros as valid.
     h*
     h*  07/01/14 C:279 Project: WC2248
     h*                 Modified calculation of Accumulated days lost to
     h*                 take into account Days Per Week value.
     h*
     h*  06/25/14 C:62  Support WC025 85984
     h*                 Update other claims with same ss#
     h*
     h*  06/13/14 C:53  Redmine: 3474
     h*                 Load/save CMIIF
     h*
     h*  06/12/14 C:291 Redmine: 4175
     h*                 Add Phone number formatting for Concurrent Employer
     h*                 phone number under Benefit tab.
     h*
     h*  05/15/14 C:290 Support: WC146 85603
     h*                 Fix bad data value in hire date at Claim Detail tab.
     h*
     h*  05/14/14 C:53  Redmine: 4186
     h*                 Add California fields.
     h*
     h*  05/12/14 C:180 Support: WC025 85474
     h*                 Fix mapping of RTW date in Benefits tab.
     h*
     h*  04/02/14 C:291 Redmine: 4078
     h*                 Add Vocational Rehabilitation check box under the
     h*                 Benefits tab.
     h*
     h*  03/27/14 C:180 Support: WC025 85146 V14R3                            *
     h*                 Fix choice selection for Nature, Part, and Cause      *
     h*                 EDI tables.                                           *
     h*
     h*  03/04/14 C:180 Support: WC114 84947                                  *
     h*                 Add MCO code.
     h*
     h*  02/18/14 C:283 Support: WA045 84644
     h*                 Never disable the entry fields no matter what the value
     h*                 of Subrogated Flag is.
     h*
     h*  12/18/13 C:190 RedMine# 3817
     h*                 -Corrected selection creteria for Campus code.
     h*                 -Changes done in dspf also(info for retrofit- scCampus)
     h*
     h*  12/02/13 C:283 Support: WC133 84254
     h*                 Correctly handle the functionality of Drop Down
     h*                 Payroll Class Code to show the Special Desg.
     h*
     h*  02/12/14 C:180 SUPPORT PRISS 027 - WC2234
     h*                 Fix Work Days Schedule code field.
     h*
     h*  12/12/13 C:287 Project:WC2234
     h*                 Add new fields to be effective January 2014 for
     h*                 claims EDI Rel3.
     h*
     h*  11/25/13 C:282 Project: 129WSC
     h*                 Populate new fields for claims for client 129.
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
     h*                 Retrofit changes
     h*
     h*  09/23/13 C:062 Support: WC114 83507
     h*                 Recompile only
     h*
     h*  09/17/13 C:062 Support: op114 83508
     h*                 Correct how time field is loaded.
     h*
     h*  08/26/13 C:295 Support: WC114 82972
     h*                 Legal Tab Method of Settlement-Drop down
     h*                 Changing the method of settlement field needs to have
     h*                 the choice criteria changed from hard coded value to
     h*                 dynamic selection.
     h*
     h*  08/25/13 C:53  Project 147DAM: Priss 11
     h*                 Correct parsing on description
     h*
     h*  08/23/13 C:282 Redmine: 3227
     h*                 CMS Submission Flag user authority check.
     h*
     h*  08/06/13 C:62  Support: ID114 83071
     h*                 Correct Date Population darts remark.
     h*
     h*  07/30/13 C:62  Support: WC114 82987
     h*                 Add logic to update check request when address
     h*                 changes
     h*
     h*  07/23/13 C:65  Redmine: 3450
     h*                 The diary transfer procedure has been called incorrectly
     h*                 The procedure should not be checking if file field is bl
     h*                 and the old/new codes are reversed when passed in.
     h*
     h*  07/15/13 C:189 Redmine: 3342
     h*                  'Reinsurance Reportable' is not getting stored to datab
     h*
     h*  07/14/13 C:295 Redmine: 3397
     h*                 LUBA Prod - Claim Entry - State screen.
     h*
     h*  05/28/13 C:282 Project: 63WBU
     h*                 BRAUMS Retrofit Custom Code to Base
     h*
     h*  05/30/13 C:283 Support: WC146 82468
     h*                 Populate Lump Sum Flag LLLUMP when loading the
     h*                 screen and also write to database from the screen.
     h*
     h*  05/08/13 C:279 Redmine: 2875
     h*                 Modofied to print the FROI correctky for LA.
     h*
     h*  05/08/13 C:279 Redmine: 2807
     h*                 Modofied to complete old diary when date is
     h*                 changed using quick diary.
     h*
     h*  05/02/13 C:282 Redmine: 2318
     h*                 Claim entry date(CMENTD) field need to
     h*                 be updated based on Lost Time Flag value.
     h*
     h*  02/20/13 C:282 Project:  147WAO
     h*                 Load/update new field Rate Of Pay, Per, Hours Per WeeK
     h*                 under FL State Information.
     h*
     h*  04/09/13 C:279 Redmine: 2634
     h*                 Modified to display correct data on
     h*                 Legal Tab.
     h*
     h* 04/08/13  C:190  Project:  45WJH
     h*                  Load/update new field scLSpecDis for LCTA.
     h*
     h*
     h* 04/08/13  C:190  Project:  45WJH
     h*                  Load/update new field scLSpecDis for LCTA.
     h*
     h*  04/01/13 C:279 Redmine: 2591
     h*                 Get Claim Adjuster field value in save
     h*                 variable.
     h*
     h*  02/18/13 C:189 SUPPORT: WA102 81699
     h*                 Populate CMENTD with the current date
     h*
     h* 01/09/13  C:284  Project:  146WFQ
     h*                  Load/update new field scFeet, scInch and scHeight
     h*                  from/into WMCDP.
     h*
     H* 01/09/12 C:251 Project: 114MCX                                *
     H*                Added 'Rx claims not eligible for payment'     *
     H*                                                               *
     h*
     h**********************************************************************
     fwmcmp     uf a e             Disk    ReName(pcm01:pcm0p)
     f                                     InfDs(dbfds)
     fsdcmla    uf   e           k Disk    ReName(pcm01:scm0a)
     f                                     Prefix(C2:2)
     fstabl     if   e           k Disk    ReName(pst01:pst00)
     fstctl1    if   e           k Disk
     Fwarplc    uf   e           k Disk    Rename(prp01:prp0c)
     Fwmcmlb    uf   e           k Disk    Rename(pcm01:pcm0b) Prefix(C3:2)
     Fwdahp     if a e             Disk    Rename(pah01:pah0p)
     fwde3l     if   e           k Disk    ReName(pe301:pe300)
     f                                     infds(wde3lfds)
     fwde3p     uf a e             Disk    ReName(pe301:pe30p)
     f                                     infds(wde3pfds)
     fwdrbl     if   e           k Disk    ReName(prb01:prb00)
     fwmcdl     uf a e           k Disk    ReName(pcd01:pcd00)
     fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     fwmsbl     uf a e           k Disk    ReName(psb01:psb00)
     fwmtpl1    if   e           k Disk    ReName(ptp01:ptp01)
     fwmpdl     if   e           k Disk    ReName(ppd01:ppd00)
     fwmlll     uf a e           k Disk    ReName(pll01:pll00)
     fwmnil     uf a e           k Disk    ReName(pni01:pni00)
     fwmddl     uf a e           k Disk    ReName(pdd01:pdd00)
     fwmchp     if a e             Disk    ReName(pch01:pch0p)
     fwmfnl     if   e           k Disk    ReName(pfn01:pfn00)
     fwmthp     if a e             Disk    ReName(pth01:pth0p)
     fwmrhp     if a e             Disk    ReName(prh01:prh0p)
     fwmrhL     if   e           k Disk    ReName(prh01:prh00)
     fwtgil     if   e           k Disk    ReName(pgi01:pgi00)
     fwadtp     if a e             Disk    ReName(pdt01:pdt0p)
     fwhfrp     uf   e             Disk    ReName(pfr01:pfr0p)
     fwmnip     o    e             Disk    ReName(pni01:pni0p)
     fwmtpp     o    e             Disk    ReName(ptp01:ptp0p)
     fssylf     if   e           k Disk    ReName(ssy01:psy00)
     F                                     Ignore(spc01)
     F                                     Ignore(sfb01)
     F                                     Ignore(src01)
     F                                     Ignore(sdr01)
     F                                     Ignore(sbc01)
     F                                     Ignore(scs01)
     F                                     Ignore(sgl01)
     F                                     Ignore(sp201)
     fwdell     uf   e           k Disk    ReName(pel01:pel00)
     fwmeml     if   e           k Disk    ReName(pem01:pem00)
     fwmupl1    if   e           k Disk
     fwmcmpx    uf a e           k Disk    usropn prefix(X)
     fwmadl     if   e           k Disk
     fwmchl5    if   e           k Disk    ReName(pch01:pch05)
     fwdf1l     if   e           k Disk    ReName(pf101:pf100)
     fwddil1    uf a e           k Disk
     fwtpal1    if   e           k Disk
     fwmidcqtl  if   e           k Disk
     fwmidctrl  if   e           k Disk
     fwmcxp2    if   e           k Disk
     f*** Beg Add *** 07/14/21 *************************************************
     fwtgtl     if   e           k Disk
     f*** End Add *** 07/14/21 *************************************************
     Ddigits           c                   ' 0123456789'
     Dpos              S             10i 0
     DSvCmsSub         S              1A
     d* Proto-types
      * Check Authority
     d ChkUserSec      pr                  ExtPgm('SRCCSC')
     d userType                       1
     d userLevel                      3
     d claimType1                     1
     d claimLevel                     3
     d execType                       1
     d secResult                      1
     d claimType2                     1
     d claimType3                     1
     d claimType4                     1
     d claimType5                     1
     d claimType6                     1
      *
     d ComAPI          pr                  extpgm('WA001R')
     d inCO#                          3  0
     d inFND                          3  0
     d outConm                       40
     d outFnam                       40
     d outLvl                         3  0
     d outType                        1

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

     d SrName          pr                  extpgm('SRNAME')
     d ##last                        56
     d ##frst                        15
     d ##term                         1
     d SRCHKPGM        pr                  extpgm('SRCHKPGM')
     d ##pgm                         10
     d ##err                          1

     d* Retrieve the Lost Days
     d AutoLetters     pr                  ExtPgm('WA049R')
     d   claimRrn                     9  0
     d   returnCode                   1
     d   lostTimeHold                 1
     d   charCSFrm                    1

     d* Retrieve the Lost Days
     d GetLostDays     pr                  ExtPgm('WA066R')
     d   company#                     3  0
     d   group                        3  0
     d   year                         3  0
     d   case                         7  0
     d   rtw                          7  0
     d   disabilityDt                 7  0
     d   totalLost                    5  0
     d   wcePgm                       1

     d* EDI Control
     d GetControlInfo  pr                  ExtPgm('WA076R')
     d   company#                     3  0
     d   jurState                     2
     d   tranID                       3
     d   group                        3  0
     d   returnCode                   1
     d   ediFlag                      3
     d   IXeLt                        1
     d   IXeLtD                       3  0
     d   IXfroi                       2
     d   IXsDue                       3  0
     d   IXrls                        1
     d   IXcsDt                       7  0
     d   IXeaDc                       5
     d   IXhcPr                       5
     d   IXfnRc                       5
     d   IXhcEd                       1
     d   IXcrEr                       5
     d   IXceIr                       5
     d   IXceCr                       5
     d   IXctAr                       5
     d   IXctRr                       5
     d   IXmo                         1
     d   IXmLtD                       3  0

     d* Write Flag change Diary Remark
     d WrtDrtRemarks   pr                  ExtPgm('WA700R')
     d company#                       3  0 Const
     d group                          3  0 Const
     d year                           3  0 Const
     d case                           7  0 Const
     d diaryKey                      10    Const
     d comment                      256    Const
     d flags                         10    Options(*nopass)

     d* Check for reserve history
     d ChkReserves     pr              n
     d  company#                      3  0 Const
     d  group                         3  0 Const
     d  year                          3  0 Const
     d  case                          7  0 Const

     d* Excess Reinsurance - Api
     d GetSpecRet      pr                  ExtPgm('WA010R')
     d   rrnClm                       9  0
     d   specRet                     13  2
     d   excessRein                   1

     d* Get Lt Day
     d GetEdiLtDays    pr              n
     d  ltDays                        3  0

     d* Get Medical Day
     d GetEdiMeDays    pr              n
     d  meDays                        3  0

     d* Get State Name
     d GetStateName    pr            20
     d   stateAbbrev                  2

     d* Load A1 State Info
     d LoadStateA1     pr
     d statePtrIn                      *

     d* Load AL State Info
     d LoadStateAL     pr
     d statePtrIn                      *

     d* Load CA State Info
     d LoadStateCA     pr
     d statePtrIn                      *

     d* Load DE State Info
     d LoadStateDE     pr
     d statePtrIn                      *

     d* Load FL State Info
     d LoadStateFL     pr
     d statePtrIn                      *

     d* Load GA State Info
     d LoadStateGA     pr
     d statePtrIn                      *

     d* Load LA State Info
     d LoadStateLA     pr
     d statePtrIn                      *

     d* Load MA State Info
     d LoadStateMA     pr
     d statePtrIn                      *

     d* Load NC State Info
     d LoadStateNC     pr
     d statePtrIn                      *

     d* Load NY State Info
     d LoadStateNY     pr
     d statePtrIn                      *

     d* Load PA State Info
     d LoadStatePA     pr
     d statePtrIn                      *

     d* Load WV State Info
     d LoadStateWV     pr
     d statePtrIn                      *

     d* Load NI State Info
     d LoadStateNI     pr
     d statePtrIn                      *

     d* Update Check Request Address info
     d UpdReqAddrs     pr
     D claimPtrIn                      *
     d* Update Claimant Name if Name has Changed
     d UpdClmName      pr
     D claimPtrIn                      *
     d claimRrnIn                     9  0

     d* Update A1 State Info
     d UpdStateInfoA1  pr
     d statePtrIn                      *

     d* Update AL State Info
     d UpdStateInfoAL  pr
     d statePtrIn                      *

     d* Update CA State Info
     d UpdStateInfoCA  pr
     d statePtrIn                      *

     d* Update DE State Info
     d UpdStateInfoDE  pr
     d statePtrIn                      *

     d* Update FL State Info
     d UpdStateInfoFL  pr
     d statePtrIn                      *

     d* Update GA State Info
     d UpdStateInfoGA  pr
     d statePtrIn                      *

     d* Update LA State Info
     d UpdStateInfoLA  pr
     d statePtrIn                      *

     d* Update MA State Info
     d UpdStateInfoMA  pr
     d statePtrIn                      *

     d* Update NC State Info
     d UpdStateInfoNC  pr
     d statePtrIn                      *

     d* Update NY State Info
     d UpdStateInfoNY  pr
     d statePtrIn                      *

     d* Update PA State Info
     d UpdStateInfoPA  pr
     d statePtrIn                      *

     d* Update WV State Info
     d UpdStateInfoWV  pr
     d statePtrIn                      *

     d* Write Addres History
     D WrtAdrHist      pr
     D beforeAfter                    1    const

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     d* Update outstanding reserves
     D UpdOutRsvs      pr
     D company#                       3  0 const
     D group#                         3  0 const
     D employer#                      9  0 const
     D division                       5  0 const
     D year                           3  0 const
     D outRsvs                             const like(ELouts)

     d* Writes I/M/R Record
     D WriteImr        pr
     D company#                       3  0 const
     D group#                         3  0 const
     D year                           3  0 const
     D case                           7  0 const
     D statusOld                      1    const
     D statusNew                      1    const

     d* Writes Reserve History
     D WriteReserves   pr
     D company#                       3  0 const
     D group#                         3  0 const
     D year                           3  0 const
     D case                           7  0 const
     D employer#                      9  0 const
     D division                       5  0 const

     d* Write Diary Transfer Record
     D WriteTransfer   pr
     D category                       1    const
     D oldStaffCode                   3    const
     D newStaffCode                   3    const

     d GetDiaryUr      pr            10
     d strDesc                        3

     d/copy sPrRelay
     d/copy dPrRemark
     d/copy sPrDate
     d/copy wPrClaims
     d/copy wPrClmDat
      /copy wPrClmRsv
      /copy sprmodule
      /copy wpridc
     d/copy wPrEmplr
     d/copy wPrUsrPrms
     d*** Beg Add *** 02/14/22 *************************************************
     d/copy wPrEdi
     d*** End Add *** 02/14/22 *************************************************
     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)
     D dbfds           ds
     D  dbrrn                397    400b 0
     D wde3lfds        ds
     D WDE3Lrrn              397    400b 0
     D wde3pfds        ds
     D WDE3Prrn              397    400b 0

     D*Variables
     D kkSeq#          s                   like(c2seq#)
     D kkFein          s                   like(PDfein)
     D kkMod           s                   like(PDmod)
     D kkEfDt          s                   like(RBefDt)
     D kkCo#           s                   like(CMco#)
     D kkJrSt          s                   like(CMjrSt)
     D kkRmCd          s                   like(CTrmCd)
     D kkState         s                   like(CMjrSt)
     D kkFrDt          s                   like(TPfrDt) Inz(*Hival)
     D kkTbId          s                   like(GItbId)
     D kkSyKey         s                   like(SYkey)  Inz('SY')
     D medSeq#         s                   like(DDmdSq)
     D compSeq#        s                   like(DDcdSq)
     D formSeq#        s                   like(DDbdSq)
     D suprSeq#        s                   like(DDsdSq)
     D unitSeq#        s                   like(DDudSq)
     D medDate         s                   like(DDmDat)
     D compDate        s                   like(DDcDat)
     D formDate        s                   like(DDbDat)
     D suprDate        s                   like(DDsDat)
     D unitDate        s                   like(DDuDat)
     D medDataPgm      s                   like(CTdePg)
     D compDataPgm     s                   like(CTdePg)
     D formDataPgm     s                   like(CTdePg)
     D suprDataPgm     s                   like(CTdePg)
     D unitDataPgm     s                   like(CTdePg)
     D fromDate        s               d   datfmt(*usa)
     D toDate          s               d   datfmt(*usa)
     D inRrnPol        s              9  0
     D years           s              3  0
     D months          s              2  0
     D yearText        s             10
     D monthText       s             10
     D hold20          s             20
     D hold100         s            100
     D cicFlg          s               n   inz(*off)
     d timpd           s              1
     D length          s              5  0
     D position        s              5  0
     D position2       s              5  0
     D inSpecRet       s             13  2 inz(*Zeros)
     D inExcessRein    s              1    inz(*Blanks)
     d totalLost       s              5  0
     d wcePgm          s              1
     d companyIn       s              3  0
     D outCltID        s              3
     d groupIn         s              3  0
     d yearIn          s              3  0
     d caseIn          s              7  0
     d empCount        s              3  0
     d dateError       s               n
     D noEMail         c                   Const('No E-Mail Available')
     D BadDateData     c                   112
     D remlen          s              5  0
     d svClmAdj        s                   like(CMclex)
     D ##last          s             56
     D ##frst          s             15
     D ##term          s              1
     D ##pgm           s             10
     D ##err           s              1
     d xxCnam          s             40
     d ahWritten       s               n   Inz(*Off)
     d userType        s              1
     d userLevel       s              3
     d claimType1      s              1
     d claimLevel      s              3
     d execType        s              1
     d secResult       s              1
     d claimType2      s              1
     d claimType3      s              1
     d claimType4      s              1
     d claimType5      s              1
     d claimType6      s              1
     d compName        s             40
     d fundName        s             40
     d secType         s              1
     d secLevel        s              3  0
     d #ADky           s             10
     d isProcRelayReq  s               n
     d*** Beg Add *** 09/06/22 ************************************************
     d position3       s              5  0
     d decWitnPhn      s             10  0
     d strWitnPhn      s             80
     d strWitnLsn      s             80
     d strWitnFsn      s             80
     d froiState       s              2
     d*** End Add *** 09/06/22 ************************************************

     D dartsSeq#       s              5    Inz('00000')
     D dartsComm       s            256
     D dartsDate       s              8
     D dartsCode       s              5
     D dartsProgram    s             10    Inz('WTCLMDTL')
     D dartsUser       s             10
     D dartsPnam       s             10    Inz('W0070R')
     D dartsDiary      s              1    Inz('Y')
     D dartsDeFlag     s              1
     D dartsPurge      s              1    Inz('N')
     D dartsSecByLvl   s              1    Inz('N')
     D dartsSecLvl     s              3    Inz('000')
     D dartsDocId      s             12    Inz(*Blanks)
     D dartsFolder     s             80    Inz(*Blanks)
     D PHY             s              1    Dim(40)
     D PRPH            s              1    Dim(10)

     d* Write Darts Parameter
     d q#key           s             40
     d q#comm          s            256
     d q#cDat          s              8
     d q#dcId          s             12
     d q#pNam          s             10
     d q#secl          s              3
     d q#user          s             10
     d q#pgm           s             10
     d q#fold          s             80
     d q#diFl          s              1
     d q#iPgm          s             10
     d q#fl01          s              1
     d q#fl02          s              1
     d q#rmCd          s              5
     d q#seq#          s              5
     d q#sbyl          s              1
     d q#com2          s             19
     d q#apId          s              2
     d q#iDoc          s             12
     d q#iQue          s             10
     d q#imgF          s              1
     d q#iPth          s             10
     d q#pIdx          s              5
     d q#qPty          s              1
     d q#dcTp          s              5
     d q#csId          s              2
     d q#code          s              2
     d charPnts        s              7

     d kkKey           s                   Like(PAkey)

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
     D                 ds
     D  zipCode                1      9  0
     D  zipCode1               1      5  0
     D  zipCode2               6      9  0

     D                 ds
     D  phoneNumber            1     10  0
     D  areaCode               1      3  0
     D  phone#                 4     10  0

     D                 DS                  INZ
     D  qcCom1                 1    195
     D  qcFead                 1      3
     D  qcUsnm                 5     35
     D  qcDrt1                36     56
     D  qcCnam                58     87
     D  qcCty                 89    113
     D  qcDrt2               116    129
     D  qcMo                 131    132
     D  qcSls1               133    133
     D  qcDy                 134    135
     D  qcSls2               136    136
     D  qcYr                 137    138
     D                 DS                  INZ
     D  qcCom2                 1     61
     D  qcDrt3                 1     10
     D  qcUser                12     61
     D                 DS                  INZ
     D  CYMD                   1      7  0
     D  Century                1      1
     D  CY1                    2      3
     D  MM                     4      5
     D  DD                     6      7
     D svPctAppr       s              5  2
     D svApprDt        s              8  0
     D intEfdt         s              7  0

     C     keySTCTL1     Klist
     C                   Kfld                    kkRmCd

     C     keySSYFL      Klist
     C                   Kfld                    kksykey

     C     keyWDE3L      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
     C                   Kfld                    CMcase

     C     key1WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt
     C                   Kfld                    kkEfDt

     C     key2WDRBL     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMjrSt

     C     keyWMCXL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd

     C     keyWMDDL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     keyWMEML      Klist
     C                   Kfld                    CMCo#
     C                   Kfld                    CMFnd
     C                   Kfld                    CMEmp#
     C                   Kfld                    CMDiv

     C     keyWMLLL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     keyWMNIL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkState

     C     keyWMPDL      Klist
     C                   Kfld                    kkFein
     C                   Kfld                    kkMod

     C     keyWMSBL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key1WMTPL1    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase

     C     key2WMTPL1    Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    kkFrDt
      *
     C     keyWTGIL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkTbId
      *
     c     keyWMCMPX     Klist
     c                   Kfld                    CMco#
     c                   Kfld                    CMfnd
     c                   Kfld                    CMfyr
     c                   Kfld                    CMcase
      *
     c     keyWTCAL      Klist
     c                   Kfld                    CMco#
     c                   Kfld                    CMfnd
      *
     c     keyWMADL      Klist
     c                   Kfld                    CMco#
     c                   Kfld                    CMfnd
     c                   Kfld                    #Adky
      *
     C     keyWMCHL5     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfyr
     C                   Kfld                    CMcase
     C                   Kfld                    intEfdt
     C

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





       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // ChkReserves - Check Reserves to see if reserves can be setup
       //===================================================================
     P ChkReserves     B
     D ChkReserves     PI              n
     D  company#                      3  0     Const
     D  group#                        3  0     Const
     D  year                          3  0     Const
     D  case                          7  0     Const

     C     keyWMRHL      Klist
     C                   kfld                    company#
     C                   kfld                    group#
     C                   kfld                    year
     C                   kfld                    case

      /Free

       Chain keyWMRHL prh00;
       If %Found(WMRHL);
          DoU %Eof(WMRHL);
            If RHiPgm<>'W8608';
               Return *On;
            Endif;
            ReadE keyWMRHL prh00;
          EndDo;
       EndIf;

       Return *Off;

      /End-Free
     P ChkReserves     E
       //===================================================================
       // GetEdiLtDays - Get EDI LT Days
       //===================================================================
     P GetEdiLtDays    B
     D GetEdiLtDays    PI              n
     D  ltDays                        3  0

     d company         s              3  0
     d jurState        s              2
     d tranID          s              3
     d group           s              3  0
     d returnCode      s              1
     d ediFlag         s              3    Inz(*Blanks)
     d IXeLt           s              1    Inz(*Blanks)
     d IXeLtD          s              3  0 Inz(*Zeros)
     d IXfroi          s              2    Inz(*Blanks)
     d IXsDue          s              3  0 Inz(*Zeros)
     d IXrls           s              1    Inz(*Blanks)
     d IXcsDt          s              7  0 Inz(*Zeros)
     d IXeaDc          s              5    Inz(*Blanks)
     d IXhcPr          s              5    Inz(*Blanks)
     d IXfnRc          s              5    Inz(*Blanks)
     d IXhcEd          s              1    Inz(*Blanks)
     d IXcrEr          s              5    Inz(*Blanks)
     d IXceIr          s              5    Inz(*Blanks)
     d IXceCr          s              5    Inz(*Blanks)
     d IXctAr          s              5    Inz(*Blanks)
     d IXctRr          s              5    Inz(*Blanks)
     d IXmo            s              1    Inz(*Blanks)
     d IXmLtD          s              3  0 Inz(*Zeros)

     d days            s              3  0 Inz(*Zeros)
      /Free

       company=CMco#;
       jurState=CMjrSt;
       tranId='RAT';
       group=CMfnd;
       returnCode=*Blanks;

       GetControlInfo(company:jurState:tranId:group:returnCode:ediFlag:
        IXeLt:IXeLtD:IXfroi:IXsDue:IXrls:IXcsDt:IXeaDc:IXhcPr:IXfnRc:IXhcEd:
        IXcrEr:IXceIr:IXceCr:IXctAr:IXctRr:IXmo:IXmLtD);

       If returnCode<>'Y';
          tranId='148';
          returnCode=*Blanks;

          GetControlInfo(company:jurState:tranId:group:returnCode:ediFlag:
           IXeLt:IXeLtD:IXfroi:IXsDue:IXrls:IXcsDt:IXeaDc:IXhcPr:IXfnRc:
           IXhcEd:IXcrEr:IXceIr:IXceCr:IXctAr:IXctRr:IXmo:IXmLtD);
       Else;
         IXeLt=*Blanks;
       EndIf;

       ltDays=IXeLtD;

       If IXeLt='Y';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P GetEdiLtDays    E
       //===================================================================
       // GetEdiMeDays - Get EDI Medical Days
       //===================================================================
     P GetEdiMeDays    B
     D GetEdiMeDays    PI              n
     D  meDays                        3  0

     d company         s              3  0
     d jurState        s              2
     d tranID          s              3
     d group           s              3  0
     d returnCode      s              1
     d ediFlag         s              3    Inz(*Blanks)
     d IXeLt           s              1    Inz(*Blanks)
     d IXeLtD          s              3  0 Inz(*Zeros)
     d IXfroi          s              2    Inz(*Blanks)
     d IXsDue          s              3  0 Inz(*Zeros)
     d IXrls           s              1    Inz(*Blanks)
     d IXcsDt          s              7  0 Inz(*Zeros)
     d IXeaDc          s              5    Inz(*Blanks)
     d IXhcPr          s              5    Inz(*Blanks)
     d IXfnRc          s              5    Inz(*Blanks)
     d IXhcEd          s              1    Inz(*Blanks)
     d IXcrEr          s              5    Inz(*Blanks)
     d IXceIr          s              5    Inz(*Blanks)
     d IXceCr          s              5    Inz(*Blanks)
     d IXctAr          s              5    Inz(*Blanks)
     d IXctRr          s              5    Inz(*Blanks)
     d IXmo            s              1    Inz(*Blanks)
     d IXmLtD          s              3  0 Inz(*Zeros)

     d days            s              3  0 Inz(*Zeros)
      /Free

       company=CMco#;
       jurState=CMjrSt;
       tranId='RAT';
       group=CMfnd;
       returnCode=*Blanks;

       GetControlInfo(company:jurState:tranId:group:returnCode:ediFlag:
        IXeLt:IXeLtD:IXfroi:IXsDue:IXrls:IXcsDt:IXeaDc:IXhcPr:IXfnRc:IXhcEd:
        IXcrEr:IXceIr:IXceCr:IXctAr:IXctRr:IXmo:IXmLtD);

       If returnCode<>'Y';
          tranId='148';
          returnCode=*Blanks;

          GetControlInfo(company:jurState:tranId:group:returnCode:ediFlag:
           IXeLt:IXeLtD:IXfroi:IXsDue:IXrls:IXcsDt:IXeaDc:IXhcPr:IXfnRc:
           IXhcEd:IXcrEr:IXceIr:IXceCr:IXctAr:IXctRr:IXmo:IXmLtD);
       Else;
          IXmo=*Blanks;
       EndIf;

       meDays=IXmLtD;

       If IXmo='Y';
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P GetEdiMeDays    E
       //===================================================================
       // GetStateName - Get the State Name
       //===================================================================
     P GetStateName    B
     D GetStateName    PI            20
     D  stateAbbrev                   2
     D stateKey        s             10
     D stateName       s             20    inz(*Blanks)
      /Free

       stateKey=stateAbbrev;
       Chain stateKey pst00;
       If %Found(STABL);
          stateName=STname;
       EndIf;

       Return stateName;

      /End-Free
     P GetStateName    E
       //===================================================================
       // LoadClmInfo - Loads the Claim Information Screen
       //===================================================================
     P LoadClmInfo     B                   Export
     D LoadClmInfo     PI
     D claimRrnIn                     9  0
     D claimPtrIn                      *
     D claimFroiIn                    9  0 options(*nopass)
     D claimInfo     e ds                  ExtName(wtClmDtl:rcdClmDt0)
     D                                     based(claimPtr)
     D claimPtr        s               *   inz
     D claimRrn        s              9  0
     D froiRrn         s              9  0
     D ltDays          s              3  0
     D meDays          s              3  0
      /Free

       claimPtr=claimPtrIn;
       claimRrn=claimRrnIn;
       If %parms() >=3;
          froiRrn=claimFroiIn;
       Endif;

       Chain(n) claimRrn pcm0p;
       Chain    keyWMCXL pcx00;

       // Check to see if required in the group setup
       If CXgdrR='Y';
          rqGender=*On;
       EndIf;
       If CXdobR='Y';
          rqBirthDat=*On;
       EndIf;
       If CXdohR='Y';
          rqHireDAte=*On;
       EndIf;

       // Is this a Devereux Group
       devGroup=ChkDevGroup(CMco#:CMfnd);
       devGroup2=devGroup;

       scEMP# = CMemp#;
       scFND = CMfnd;
       scDIV = CMdiv;
       scLocSel0='ALCO#=' + %EditC(CMco#:'X') + ' AND ALFND=' +
        %EditC(CMfnd:'X') + ' AND ALEMP#=' + %EditC(CMEMP#:'X') +
        ' AND ALDIV=' + %EditC(CMDIV:'X');

       scDeptSel='DPCO#=' + %EditC(CMco#:'X') + ' AND DPFND=' +
        %EditC(CMfnd:'X') + ' AND DPEMP#=' + %EditC(CMEMP#:'X') +
        ' AND DPDIV=' + %EditC(CMDIV:'X');

       scCatasel='C2FND=' + %EditC(CMFND:'X') + ' AND C2STAT<>''I''';

       Chain keyWMCXL pcx00;
       If Not %Found(WMCXL) Or %Found(WMCXL) And CXcat2=*Blanks;
          scCataHdr='Category 2';
       Else;
          scCataHdr=CXcat2;
       EndIf;

       // Payroll Query Fields
       scPayFile='WMEPP, WMPRP A';
          scPayChc='DIGITS(EPCLAS) || '' '' || Trim(EPEXT) || '' '' || '+
           'Trim(EPSPEC) || '' - '' || PRDESC';
          scPayVal='DIGITS(EPCLAS) || '' '' || EPEXT || '' '' || '+
           'EPSPEC';
       inRRNPol=GetEmpYrRR(CMfyr:CMemp#:CMdiv:CMfnd:CMco#);



       scPaySel='EPFND=' + %CHAR(CMFND) + ' AND EPFYR=' +
        %CHAR(CMFYR) + ' AND EPEMP#=' + %CHAR(CMEMP#) + ' AND EPDIV=' +
        %CHAR(CMDIV) + ' AND EPCLAS=A.PRCLAS AND A.PREFDT = ' +
        '(SELECT MAX(B.PREFDT) FROM WMPRP B WHERE ' +
        ' A.PRCLAS=B.PRCLAS AND B.PRRTCD=''' +
        GetRateSet(inRRNPol) + ''') AND A.PRRTCD=''' +
        GetRateSet(inRRNPol) + ''' AND A.PROCCD <> ''Y'' '  +
        'AND A.PRNRAT <> ''Y'' AND EPSTAT = ''A''' ;


        scPayOrd='EPCLAS, EPSPEC';
       Clear campusSel;
       campusSel = 'CLco# = ' + %char(CMco#) + ' AND CLfnd = '
        + %char(CMfnd) + ' AND CLemp# = ' + %char(CMemp#)
        + ' AND CLdiv = ' + %char(CMdiv)
        + ' and CLstat <> ''I''';

       scStateClm=CMacl#;
       scStClmRel = CMjrc#;
       scEmpClm#=CMecs#;
       scSupName=CMeSpv;
       scAccDate=cvtToDate(CMacDt);
       scAccTime=CMacTm;
       scAltStClm = CMascl;
       ScPrClm# = CMpds#;
       Chain keyWDF1L pf100;
       If %Found(WDF1L) And F1rlxC<>'Y';
          indPayClas = *On;
       Else;
          indPayClas = *Off;
       Endif;
       scPayClass=%Editc(CMclas : 'X') + ' ' + CMext + ' ' + CMspec;
       scSicCode=CMsic;
       scInInFein=CMiif;
       sc#Dep=CM#Dep;
       scAccDesc1=CMacds;
       scAccDesc2=CMacd2;
       scLocation=CMctg1;
       scNarrativ=CMacln;
       scPremises=CMeAdr;
       If scPremises = 'Y' or dspMode0 = *On;
          indEmpPre = *On;
       Else;
          indEmpPre = *Off;
       Endif;
       scAccPrem=CMaccPrem;
       scColBarg=CMcolBarg;
       scPlcName=CMacNm;
       scPlcAddr=CMacDr;
       scPlcCity=CMaCty;

       scPlcCnty=CMaSt + CMacCy;
       scPlcSt=CMaSt;

       // Place County Select
       scPlcCnSel='SUBSTR(CNKEY,1,2)=''' + scPlcSt + '''';

       If scPlcSt='LA';
          scPlcCntLb='Parish';
       Else;
          scPlcCntLb='County';
       EndIf;

       scPlcZip=CMacZ1*10000 + CMacZ2;
       scJurState=CMjrSt;
       scTPA#=CMoff#;
       scOccDesc=CMocDs;
       scOccCode=CMocCd;
       scDept=CMdept;
       scHireDate=dtMDYY(CMhiDt);
       scTenure=*Blanks;
       dateError = *Off;
       If CMhiDt<>0;
          toDate=cvtToDate(CMacDt);
          Monitor;
             fromDate=%Date(CMhiDt:*CYMD);
          On-Error BadDatedata;
             dateError = *On;
          EndMon;
          If dateError = *Off;
             years=%Diff(toDate:fromDate:*Y);
             months=%Rem(%Diff(toDate:fromDate:*M):12);
          Endif;
          If years=1;
             yearText='year,';
          Else;
             yearText='years,';
          EndIf;
          If months=1;
             monthText='month';
          Else;
             monthText='months';
          EndIf;
          If dateError = *Off;
          scTenure='Tenure ' + %Trim(%EditC(years:'4')) + ' ' +
           %Trim(yearText) + ' ' + %Trim(%EditC(months:'4')) + ' ' +
           %Trim(monthText);
          EndIf;
       EndIf;
       scEmpSts=CmeSts;
       scDays=CMdays;
       scHours=CMhrsD;
       scShift=CMshft;
       scRptEmp=dtMDYY(CMdtRp);
       scReleased=CMssRl;
       scMethod=CMmCon;
       scActStat=CMacSt;
       scDevCauCd=CMdCau;
       scDevRstTp=CMdRet;
       scDevPgm#=CMdPgm;
       // Populate screen fields from import
       If froiRrn <> 0 and %parms() >=3;
          Chain(n) froiRrn PFR0P;
          cicFlg = *On;
          scDevCauCd = FRdcau;
          scDevRstTp = FRdret;
          scDevPgm# = FRdpgm;
       Endif;
       If scDevCauCd='01';
          devGroup2=*On;
       Else;
          devGroup2=*Off;
       EndIf;
       scLostTime=CMltFl;
       If scLostTime = 'R';
          indNCCI = *Off;
       Else;
          indNCCI = *On;
       Endif;
       scPossLoss=CMplFl;
       kkCo#=CMco#;
       kkJrSt=CMjrSt;

       vsLtMore=*Off;
       If GetEdiLtDays(ltDays) = *On;
          If CMltFl='I' or CMltFl=*Blanks;
             vsLtMore=*On;
             scLtMore='If Indemnity, more than ' +
              %Trim(%EditC(ltDays:'4')) + ' days?';
          EndIf;
       EndIf;

      *** Beg Add *** 01/14/25 *** WC2995 *****************************
       scIndmnty = CMindClmCd;
      *** End Add *** 01/14/25 *** WC2995 *****************************

       vsMeMore=*Off;
       If GetEdiMeDays(meDays) = *On;
          If CMltFl='M' or CMltFl=*Blanks;
             vsMeMore=*On;
             scMeMore='If Medical, more than ' +
              %Trim(%EditC(meDays:'4')) + ' days?';
          EndIf;
       EndIf;
       scEDIClmTy = CMeTyp;

       scDaysLt=CMgt1;
       scDaysMe=CMgt2;
       scLateRe=CMltRe;
       GetClientID(outCltID);
       If  outCltID  <> '114';
          scCata2=CMctg2;
       Else;
          scCata2C=CMctg2;
       Endif;

       scCmps=CMcmps;
       If outCltID = '025';
          scCampus=CMcmps;
          svCampus=CMcmps;
       EndIf;
       If outCltID = '146';
          is146=*on;
          scSpltWg=CMspltwg;
       Else;
          is146=*off;
       EndIf;
       scRecDate=dtMDYY(CMrcvD);
       scConDate=dtMDYY(CMtpaD);
       scOthEmpId=CMeId;
       scIdTypeQu=CMeitq;
       scNature=CMnatr;
       scPart=CMpart;
       scCause=CMcaus;
       scFtLoc=CMlocFt;
       scInjCd=CMiMlc;


       // Nature selection Criteria
          selNature ='NACO#=''' + %CHAR(CMCO#)  + ''' AND NASTAT<>''I''';

       // Part selection Criteria
          selPart ='PACO#=''' + %CHAR(CMCO#)  + ''' AND PASTAT<>''I''';

       // Cause selection Criteria
          selCause ='CACO#=''' + %CHAR(CMCO#)  + ''' AND CASTAT<>''I''';



       // NCCI Selection Criteria
       kkTbId=CMjrSt + 'NC';
       Chain keyWTGIL pgi00;
       If Not %Found(WTGIL);
          kkTbId='  NC';
       EndIf;

       selNCCI='GTTBID=''' + kkTbId + ''' AND GTSTAT<>''I''';

       scNCCI=CMncci;

       // Coverage Code Selection Criteria
       kkTbId=CMjrSt + 'LC';
       Chain keyWTGIL pgi00;
       If Not %Found(WTGIL);
          kkTbId='  LC';
       EndIf;

       selCovCode='GTTBID=''' + kkTbId + ''' AND GTSTAT<>''I''';

       scCovCode=CMlsCv;

       scRecType=CMlsRv;

       // Coverage Code Selection Criteria
       kkTbId=CMjrSt + 'CT';
       Chain keyWTGIL pgi00;
       If Not %Found(WTGIL);
          kkTbId='  CT';
       EndIf;

       selTypeCov='GTTBID=''' + kkTbId + ''' AND GTSTAT<>''I''';

       scTypeCov=CMcovT;
       scRsvType=CMrsvT;
          scWWTC = CMwwtc ;
          if CMwds = *BLANKS;
             CMwds = 'NNNNNNN';
          Endif;
          scWDSSu = %SUBST(CMwds:1:1);
          scWDSM  = %SUBST(CMwds:2:1);
          scWDST  = %SUBST(CMwds:3:1);
          scWDSW  = %SUBST(CMwds:4:1);
          scWDSR  = %SUBST(CMwds:5:1);
          scWDSF  = %SUBST(CMwds:6:1);
          scWDSSa = %SUBST(CMwds:7:1);
          scESID = CMesid ;
          scAsLc = CMiDcAcS;
          scLossCd = %Editc(CMLossCd:'X');

       // Loss Condition Code Selection Criteria
       kkTbId=CMjrSt + 'AC';
       Chain keyWTGIL pgi00;
       If Not %Found(WTGIL);
          kkTbId='  AC';
       EndIf;

       selLossCd ='GTTBID=''' + kkTbId + ''' AND GTSTAT<>''I''';

       Chain(n) CMss# pcd00;
       If %Found(wmcdl);

          scLsN=CDlsN;
          scFsN=CDfsN;
          scMid=CDmid;

          scClmAdr1=CDadr1;
          scClmAdr2=CDadr2;
          scProvince=CDadr3;
          scCountry=CDctry;
          scClmCty=CDcty;
          scBirthDat=dtMDYY(CDdtoB);
          scAge=0;
          dateError=*Off;
          If CDdtoB<>0;
             toDate=CvtToDate(CMacDt);
             Monitor;
                fromDate=%Date(CDdtoB:*CYMD);
             On-Error BadDateData;
                dateError=*On;
             EndMon;
             If dateError=*Off;
                scAge=%Diff(toDate:fromDate:*Y);
             EndIf;
          EndIf;
          scGender=CDsex;

          scMarStat=CDmSts;
          //*** Beg Add *** 02/14/22 ***************************************
          scEdul = CDedul;
          //*** End Add *** 02/14/22 ***************************************

          scClmCnty=CDST + CDcnty;
          scClmSt=CDst;

          // Claim County Select
          scClmCnSel='SUBSTR(CNKEY,1,2)=''' + scClmSt + '''';

          scClmZip=CDzip;
          scClmPhn#=CDarea*10000000 + CDphn#;
          scClmWrk#=CDwAra*10000000 + CDwPh#;
          scClmCel#=CDcAra*10000000 + CDcPhn;
          scDisSms = CDDisSms;
          If isSmsDsbld = *on;
             scDisSms = 'Y';
          Else;
             scDisSms = CDDisSms;
          Endif;
          scClmEmad=CDeMad;
          scClmEmadT=CDeMad;
          If scClmEmad=*Blanks;
             scClmEmadT=noEMAIL;
          EndIf;
          scFeet = %subst(CDheigt:1:2);
          scInch = %subst(CDheigt:4:2);
          scWeight = CDwgt;
       Else;
          scLsn=*Blanks;
          scFsn=*Blanks;
          scMid=*Blanks;
          scClmPhn#=*Zeros;
          scClmAdr1=*Blanks;
          scClmAdr2=*Blanks;
          scProvince=*Blanks;
          scCountry=*Blanks;
          scClmCty=*Blanks;
          scClmSt=*Blanks;
          scClmZip=*Zeros;
          scClmCnty=*Blanks;
          scClmPhn#=*Zeros;
          scClmWrk#=*Zeros;
          scClmCel#=*Zeros;
          scDisSms = *blank;
          scClmEmad=*Blanks;
          scClmEmadT=noEmail;
          Clear scBirthDat;
          scGender=*Blanks;
          scAge=*Zeros;
          scMarStat=*Blanks;
          scFeet = *Zeros;
          scInch = *Zeros;
          scWeight = *Zeros;
          //*** Beg Add *** 02/14/22 ***************************************
          scEdul = *zeros;
          //*** End Add *** 02/14/22 ***************************************
       EndIf;

       If scClmSt='LA';
          scClmCntLb='Parish';
       Else;
          scClmCntLb='County';
       EndIf;

       If froiRrn <> 0 and %parms() >=3;
          Chain(n) froiRrn PFR0P;
          cicFlg = *On;
          scLsn = FRlsn;
          scFsn = FRfsn;
          scMid = FRmid;
          //*** Begin Chg *** 10/26/21 *************************************
          //*** scPayClass = %Char(FRclas);
          scPayClass = %EditC(FRclas:'X');
          //*** End   Chg *** 10/26/21 *************************************
          If FRdtrp <> *Blanks;
             scRptEmp = dtMDYY2(%Int(FRdtrp));
          Endif;
          scLostTime = FRltfl;
          scNature   = FRnatr;
          scPart     = FRpart;
          scCause    = FRcaus;
          If FRrcvd <> ' ';
             scRecDate  = dtMDYY2(%Int(FRrcvd));
          Endif;
          scPremises = FReadr;
          If scPremises = 'Y' Or dspMode0 = *On;
             indEmpPre = *On;
          Else;
             indEmpPre = *Off;
          Endif;
          If FRhidt <> ' ';
             scHireDate = dtMDYY2(%Int(FRhidt));
          Endif;
          scJurState = FRjrst;
          scLocation = FRctg1;
          scAccTime  = FRactm;
          scEmpSts   = FRests;
          scOccDesc  = FRocds;
          scAccDesc1 = FR60a3;
          scAccDesc2 = FR60a4;
          If scAccDesc1 = ' ' and scAccDesc2 = ' ';
             scAccDesc1 = FR80a1;
             evalr hold20 = FR80a1;
             hold100 = hold20 + FR80a2;
             scAccDesc2 = hold100;
          Endif;
          sc#Dep    = FRdep;
          scDays    = FRdays;
          scEmpClm# = FRrpt;
          scLocation= FRctg1;
          scDaysLt  = FRgt1;
          scPlcAddr = FRacdr;
          scPlcCity = FRacty;
          scPlcSt   = FRast;
          scPlcZip  = FRaczp;
          IF scPlcAddr = ' ';
             scPlcAddr = FR40A1;
             scPlcCity = FR25A1;
             scPlcSt   = FR2A1;
             scPlcZip  = FR9N2;
          Endif;
          scHours  = FRhrsd;
          scEmpSts = FRests;
          scMethod = 'I';
          Chain(n) FRss pcd00;
          scClmAdr1 = FRadr1;
          scClmAdr2 = FRadr2;
          scProvince= FRadr3;
          scCountry = FRctry;
          scClmCty  = FRcty;
          scClmSt   = FRst;
          scClmZip  = FRzip;
          //*** Begin Add *** 10/26/21 *************************************
          scClmEmad = FRmail;
          //*** End   Add *** 10/26/21 *************************************
          If FRdtoB <> ' ';
             scBirthDat= dtMDYY2(%Int(FRdtoB));
          Endif;
          scClmPhn# = FRarea*10000000 + FRphn;
          //*** Begin Add *** 10/26/21 *************************************
          scClmWrk# = FR3n1*10000000 + FR9n1;
          //*** End   Add *** 10/26/21 *************************************
          scClmCel# = FR3n6*10000000 + FR9n5;
          // MLD - add call to SPIrelay to send SMS Welcome Text?
          scGender  = FRsex;
          scMarStat = FRmSts;
       Endif;
       If outCltID = '063';
          open wmcmpx;
          Chain keyWMCMPX pcm01x;
          If %Found(wmcmpx);
             scLocInStr = XCMinlc;
             scPosition = XCMpos;
             scPoints   = %char(XCMpnts);
          EndIf;
          close wmcmpx;
       EndIf;

      /End-Free
     P LoadClmInfo     E
       //===================================================================
       // LoadMgmtInfo - Loads the Claim Information Screen
       //===================================================================
     P LoadMgmtInfo    B                   Export
     D LoadMgmtInfo    PI
     D claimRrnIn                     9  0
     D mgmtPtrIn                       *
     D mgmtInfo      e ds                  ExtName(wtClmDtl:rcdClmDt1)
     D                                     based(mgmtPtr)
     D mgmtPtr         s               *   inz
     D claimRrn        s              9  0
     D intCo#          s              3  0 inz
     D intFnd          s              3  0 inz
      /Free

       mgmtPtr=mgmtPtrIn;
       claimRrn=claimRrnIn;

       Chain(n) claimRrn pcm0p;

       //*** Beg Add ***** 03/22/24 ***************************
       Chain(n) (CMco#:CMfnd) pcx00;
       If %found() and CXadj <> *Blanks;
          scHdgAdj = CXadj;
       Else;
          scHdgAdj = 'Adjuster';
       Endif;
       //*** End Add ***** 03/22/24 ***************************
       scClmAdj=CMclEx;
       svClmAdj=CMclex;
       scSuperv=CMsupr;
       scAccManag=CMaMan;
       scClNurse=CMuMan;
       scCsNurse=CMnrse;
       svCsNurse=scCsNurse;
       scCsNurse2 = 'UPSTAT <> ''I'' or UPSTAF = ''' + CMNRSE + '''';
       scOutAdj=CMfeAd;
       scFundAdj=CMsdfA;
       scReinAdj=CMeSup;
       svSuperv=CMsupr;
       //*** Beg Add ***** 04/18/22 *************************
       svAccDate = CMacdt;
       svJurStAdd= CMJrSt;
       svCata= CMCata;
       //*** End Add ***** 04/18/22 *************************
       scClmAdj2  = 'UPSTAT <> ''I'' or UPSTAF = ''' + CMCLEX + '''';
       scOutAdj2  = 'UPSTAT <> ''I'' or UPSTAF = ''' + CMFEAD + '''';
       scFundAdj2 = 'UPSTAT <> ''I'' or UPSTAF = ''' + CMSDFA + '''';
       scSuperv2  = 'UPSTAT <> ''I'' or UPSTAF = ''' + CMSUPR + '''';
       scReinAdj2 = 'UPSTAT <> ''I'' or UPSTAF = ''' + CMESUP + '''';
       scAccMana2 = 'UPSTAT <> ''I'' or UPSTAF = ''' + CMAMAN + '''';
       scClNurse2 = 'UPSTAT <> ''I'' or UPSTAF = ''' + CMUMAN + '''';

       scEnterBy=getUsrName(CMiUsr);
       scSeasEmp=CMseaE;
       scEmpLiab=CMliaF;
       scFdEffDt=dtMDYY(CMdyEf);
       scFdResDt=dtMDYY(CMdRes);
       scFdReas1=CMdny1;
       scFdReas2=CMdny2;
       scFdReas3=CMdny3;
       scFdReas4=CMdny4;
       scFdReas5=CMdny5;
       scPdReas=CMpDny;
       scDenReas1=%Subst(CMdnyN:1:50);
       scDenReas2=%Subst(CMdnyN:51:50);
       scDenReas3=%Subst(CMdnyN:101:50);
       scParDenDt=dtMDYY(CMPded);
       scReinClm#=CMreCl;
       //*** Beg Chg ***** 04/18/22 ************************
       //scCatas#=CMcata;
       If CMcata > 0;
          scCatas#=%Char(CMcata);
       Else;
          Clear scCatas#;
       Endif;
       //*** End Chg ***** 04/18/22 ************************
       GetSpecRet(claimRrn:inSpecRet:inExcessRein);
       scSpecRet=inSpecRet;
       // Check if the user has authority to change the CMS Submission
       Chain keyWMCXL pcx00;
       If %Found(wmcxl);
          intCo# = CMco#;
          intFnd = CMfnd;
          ComAPI(intCo#:intFnd:compName:fundName:secLevel:secType);

          userType=secType;
          userLevel=%EditC(secLevel:'X');
          claimType1=CXmbs1;
          claimLevel=%EditC(CXmbs:'X');
          execType='1';
          secResult=*ON;
          claimType2=CXmbs2;
          claimType3=CXmbs3;
          claimType4=CXmbs4;
          claimType5=CXmbs5;
          claimType6=*Blanks;

          isEdtCMS = *Off;
          ChkUserSec(userType:userLevel:claimType1:claimLevel:execType:
           secResult:claimType2:claimType3:claimType4:claimType5:
           claimType6);
          If secResult = *On;
             isEdtCMS = *On;
          EndIf;

       EndIf;
       scCmsSub=CMmbs;
       If  outCltID  = '045';
           scLSpecDis=CMsdf;
       Else;
           scSpecDis=CMsdf;
       Endif;
       scContro=CMcvFl;
       scExcess=CMexcs;
       scAcquired=CMacqr;
       scAcInTrDt=dtMDYY(CMalkd);
       scAWL = CMawli;
       scDenied=CMdny;
       scSubro=CMsubr;
       scLifePen=CMlfpn;
       scSw132a=CMsw;
       scDelayed=CMdly;
       scDwc1=CMdwc1;
       scDecDt=dtMDYY(CMdcsn);

       If outCltID = '025';
          scLifeBen=CMlben;
       EndIf;
       scReinRpt=CMrirp;

       scOsha=CMosha;
       scUslh=CMuslh;
       sc14x6=CM14x6;
       scmisc=CMmisc;
       scrstn=CMrstn;
       sclstx=CMlstx;
       // Load indicators
       scEsE = CMeSe;
       scIaQ = CMiAQ;
       scStuAgr = CMstuAgr;
       scPartDn = CMpartDn;
       sc120Rvw = CM120Rvw;

       // Load Subrogration Information - Management Panel
       Chain(n) keyWMSBL psb00;
       If %Found(WMSBL);
          scRspName=SBrNam;
          scRspAdr1=SBrAd1;
          scRspAdr2=SBrAd2;
          scRspCity=SBrCty;
          scRspSt=SBrSt;
          scRspZip=SBrZip;
          scRspPhn#=SBrPhn;
          scRspEmAd=SBrMad;
          scStLimDt=dtMDYY(SBslDt);
          scInsName=SBiNam;
          scInsAdr1=SBiAd1;
          scInsAdr2=SBiAd2;
          scInsCty=SBiCty;
          scInsSt=SBiSt;
          scInsZip=SBiZip;
          scInsPhn#=SBiPhn;
          scInsEmAd=SBiMad;
          scInsPol#=SBiPol;
          scInsAdj=SBiAdj;
       EndIf;

       // Load Demographic Field
       Chain(n) CMss# pcd00;
       If %Found(wmcdl);
          scMedBen=CDmdBn;
       Else;
          scMedBen=*Blanks;
       EndIf;

       If outCltID  = '133';
          If CMjrSt = 'TX';
             scRptMth = '0,6,18,30,42,54,66,78,90,102,114,126,138';
             scRptLvl = '0,1,2,3,4,5,6,7,8,9,10,11,12';
          Else;
             scRptMth = '0,18,30,42,54,66,78,90,102,114,126,138';
             scRptLvl = '0,1,2,3,4,5,6,7,8,9,10,11';
          EndIf;
          Chain(n) keyWMCHL# pdi01;
          If %Found(WDDIL1);
             scDCIsts=DIstat;
             scValLvl=DIrplv;
          EndIf;
       EndIf;

      /End-Free
     P LoadMgmtInfo    E
       //===================================================================
       // LoadMedInfo - Loads the Medical Information Screen
       //===================================================================
     P LoadMedInfo     B                   Export
     D LoadMedInfo     PI
     D claimRrnIn                     9  0
     D medicalPtrIn                    *
     D claimFroiIn                    9  0 options(*nopass)
     D medicalInfo   e ds                  ExtName(wtClmDtl:rcdClmDt2)
     D                                     based(medicalPtr)
     D medicalPtr      s               *   inz
     D claimRrn        s              9  0
     D froiRrn         s              9  0
      /Free

       GetClientID(outCltID);

       medicalPtr=medicalPtrIn;
       claimRrn=claimRrnIn;
       If %parms() >=3;
          froiRrn=claimFroiIn;
       Endif;

       Chain(n) claimRrn pcm0p;

       scHosList='PDFEIN || '' '' || PDMOD || ''-'' || ' +
        'CASE WHEN PDDBA1=''Y'' THEN TRIM(PDADR1) || ''-'' || ' +
        'TRIM(PDADR2) ELSE ' +
        'TRIM(PDNAME) || ''-'' || TRIM(PDADR1) || '' '' || ' +
        'TRIM(PDADR2) END ' +
        ' || ''- '' || TRIM(PDCTY) || '', '' || ' +
        'PDST || '' '' || SUBSTR(DIGITS(PDZIP),1,5)';

       scMco#List='PDFEIN || '' '' || PDMOD || ''-'' || ' +
        'CASE WHEN PDDBA1=''Y'' THEN TRIM(PDADR1) || ''-'' || ' +
        'TRIM(PDADR2) ELSE ' +
        'TRIM(PDNAME) || ''-'' || TRIM(PDADR1) || '' '' || ' +
        'TRIM(PDADR2) END ' +
        ' || ''- '' || TRIM(PDCTY) || '', '' || ' +
        'PDST || '' '' || SUBSTR(DIGITS(PDZIP),1,5)';
       physExist=*Off;
       kkFrDt=DateTo7(SystemDate());
       SetLl key2WMTPL1 ptp01;
       ReadE key1WMTPL1 ptp01;
       DoW Not %Eof(WMTPL1);
          If TPpRol='TR' And (TPtoDt>=DateTo7(SystemDate()) or TPtoDt=0);
             physExist=*On;
             scPhysFein=TPphyF;
             scPhyName=TPphyN;
             scPhyAdr=TPphyA;
             scPhyCty=TPphyC;
             scPhySt=TPphyS;
             scPhyZip=TPphyZ;
             scPhyPhn#=TPphyR*10000000 + TPphyP;
             scPhyFax#=TPfxAr*10000000 + TPfax#;
             scPhysEmad=TPeMad;
             If scPhysEmad=*Blanks;
                scPhysEmTT=noEmail;
             EndIf;
             Leave;
          EndIf;
          ReadE key1WMTPL1 ptp01;
       EndDo;

       If CMhosF=*Zeros;
          scHospFein=*Blanks;
       Else;
          scHospFein=%EditC(CMhosF:'X') + %EditC(CMhosM:'X');
       EndIf;

       If CMhosF=*Zeros;
          scHosName=CMhosN;
          scHosAdr=CMhosA;
          scHosCty=CMhosC;
          scHosSt=CMhosS;
          scHosZip=CMhosZ;
          scHosPhn#=CMhosR*10000000 + CMhosP;
       Else;
          kkFein=CMhosF;
          kkMod=CMhosM;
          Chain keyWMPDL ppd00;
          If %Found(WMPDL);
             If PDdba1='Y';
                scHosName=PDadr1;
                scHosAdr=PDadr2;
             Else;
                If PDfsn<>*Blanks;
                   scHosName=%Trim(PDfsN) + ' ' + %Trim(PDname);
                Else;
                   scHosName=%Trim(PDname);
                EndIf;
                scHosAdr=PDadr1;
             EndIf;
             scHosCty=PDcty;
             scHosSt=PDst;
             scHosZip=pdZip;
             scHosPhn#=PDarea*10000000 + PDphn#;
          EndIf;
       EndIf;

       scInitTrt=CMiTrt;
       scSurgDate=dtMDYY(CMsgDt);
       scRehabDt=dtMDYY(CMdtRe);
       scAntMmi=dtMDYY(CMaMmi);
       scOcc=CMoDis;
       scYrLstExp=CMyrLx;
       scImpaired=CMimp;
       scPctImp=CMiPct;
       // MCO ode Selection Criteria
       kkTbId='MCO ';
       Chain keyWTGIL pgi00;

       selMcoCode='GTTBID=''' + kkTbId + ''' AND GTSTAT<>''I''';

       scMcoCode=CMmcTp;
       scMcoName=CMmcNm;
          If CMmcoF > *Blanks;
             scMcoFein=%Dec(CMmcoF:9:0);
          Endif;
       scChgPhy=CMcgPh;
       scReqPhy=CMwcPh;
       scPhyDny=CMwdPh;
       scHicn#=CDhicn;
       scCmsCause=CMaInj;
       scCmsCaus2=CMaIn1;
       scCmsDate=dtMDYY(CMcmDi);
       scRelDate=dtMDYY(CMrlw);
       scFinMmi=dtMDYY(CMmmi);
       scMinPay=CMpImp;
       scMrAuth=CMauRl;
       scHanClm=CMhCap;
       scDthClm=CMdth;
       If CMidth = 'N';
          scDthRes = *blanks;
       else;
          scDthRes = CMidth;
       Endif;
       scDeathDt=dtMDYY(CMdod);
       If CMorm = ' ';
          scOrmInd = 'N';
       Else;
          scOrmInd = CMorm;
       EndIf;

       scOrmDate=dtMDYY(CMormD);
       scPreExist=CMprEx;
       If outCltId = '114';
          scRXEL=CMrxel;
       EndIf;
       If outCltId = '090';
          scPBME = CMrxel;
       EndIf;

       If froiRrn <> 0 and %parms() >=3;
          Chain(n) froiRrn PFR0P;
          cicFlg = *On;
          scHosAdr  = FRhosa;
          scHosCty  = FRhosc;
          scHosName = FRhosn;
          scHosPhn# = FR3N2*10000000 + FR9N4;
          scHosSt   = FRhoss;
          scHosZip  = FRhosz;
          scInitTrt = FRitrt;
       Endif;
       scPctAppr=CMapct;
       scApprDt =dtMDYY(CMaeff);
       svPctAppr=CMapct;
       svApprDt =dtMDYY(CMaeff);
       scInjSvCod = CMinjS;
       kkKey = CMpart;
       Chain (CMco#:kkKey) ppa01;
       If %Found(wtpal1);
          scPartDesc = %Trim(CMpart) + ' - ' + %Trim(PAdesc);
       Endif;
          scPeDPct   = CMpEDis;
          scDLOECPct = CMlOeC;

      /End-Free
     P LoadMedInfo     E
       //===================================================================
       // LoadBenInfo - Loads the Benefit Information Screen
       //===================================================================
     P LoadBenInfo     B                   Export
     D LoadBenInfo     PI
     D claimRrnIn                     9  0
     D benefitPtrIn                    *
     D claimFroiIn                    9  0 options(*nopass)
     D benefitInfo   e ds                  ExtName(wtClmDtl:rcdClmDt3)
     D                                     based(benefitPtr)
     D benefitPtr      s               *   inz
     D claimRrn        s              9  0
     D froiRrn         s              9  0
       //*** Beg Add *** 02/14/22 *********************************************
     d inD433          s              1
     d inD440          s              1
     d inD441          s              1
     d inD441D         s              7  0
     d inD442          s              1
     d inRtnCode       s              1
     d outTranId       s              3
     d outFormatId     s              3
     d outJurState     s              2
     d outCompany      s              3  0
       //*** End Add *** 02/14/22 *********************************************

     C     keyWDRBL      klist
     C                   kfld                    CMco#
     C                   kfld                    CMfnd

      /Free

       benefitPtr=benefitPtrIn;
       claimRrn=claimRrnIn;
       If %parms() >=3;
          froiRrn=claimFroiIn;
       Endif;

       Chain(n) claimRrn pcm0p;
       GetLostDays(CMco#:CMfnd:CMfYr:CMcase:
        CMrtw:CMDbDt:totalLost:wcePgm);
       If CMdays > 0;
          totalLost = %intH((totalLost * CMdays)/7);
       Endif;
       scAccDays=totalLost;
       scCmpDayPd=CMlost;
       scPaidInDt=CMpoi;
       scEmpComp=CMagCp;
       scCompCode=CMagCd;
       scLstWkDt=dtMDYY(CMdtLw);
       scDisNotDt=dtMDYY(CMdoDd);
       scDisDt=dtMDYY(CMdbDt);
       scDtNotLt=dtMDYY(CMdnLt);
       scInitDtLt=dtMDYY(CMidLt);
       scFullRC=CMsRcF;
       scPartRC=CMsRcP;
       scEstRtw=dtMDYY(CMeRtw);
       scRelWork=dtMDYY(CMrlW);
       scActRtw=dtMDYY(CMrtw);
       scRtwEmp=CMsame;
       scRtwQua=CMrtwQ;
       scRtwSts=CMrtwS;
       scTaxSts=CMetFs;
       scExempts=CMenEx;
       scSusNarr=CMsusN;
       scSusEfDt=dtMDYY(CMsuEf);
       scNonConCd=CMncPc;
       scTdBeCd =CMtdbe;
       scFullSal=CMfSal;
       scPaidDt=dtMDYY(CMfsDt);
       scWeekPaid=CMfsWk;
       scDaysPaid=CMfsDy;
       scWagePre=CMprWW;
       scOthPmt=CMotWk;
       scMthWage=CMwMth;
       scWagePost=CMpoWw;
       scWagePrd=CMwgPd;
       scDisBen=CMdfBe;
       scRedBen=CMrBac;
       scVocRehb=CMvoRc;
       kkEfDt=CMacDt;
       SetGt key1WDRBL prb00;
       ReadPe key2WDRBL prb00;
       If not %Eof(WDRBL);
          If RBvchr=*Blanks;
             scSalVou='N';
          Else;
             scSalVou='Y';
          EndIf;
       Else;
          scSalVou=*Blanks;
       EndIf;
       If CMpspa = 'E';
         scPSPA = 'Y';
       Else;
         scPSPA = 'N';
       EndIf;

       scPension=CMpeFf;
       scOther=CMotFf;
       scUnEmp=CMueFf;
       scSpecDisa=CMsfFf;
       scSocSec=CMssFf;
       scAmtPaid=CMfsAm;
       // Load concurrent employer info
       scConEmpN1=*Blanks;
       scConEmpP1=*Zeros;
       scConEmpW1=*Blanks;
       scConRrn1=*Zeros;
       scConEmpN2=*Blanks;
       scConEmpP2=*Zeros;
       scConEmpW2=*Blanks;
       scConRrn2=*Zeros;
       eval empCount=0;
       Chain keyWDE3L pe300;
       If %Found(WDE3L);
          DoU %Eof(WDE3L);
             empCount=empCount+1;
             Select;
             When empCount=1;
                scConEmpN1=E3eNam;
                If E3phon <> ' ';

                pos = %check(digits:E3phon);

                if pos > 0;
                  scConEmpP1=*zeros;
                else;
                  scConEmpP1=%Dec(E3phon:10:0);
                endif;

                Else;
                   scConEmpP1 = *Zeros;
                Endif;
                scConEmpW1=E3wage;
                scConRrn1=WDE3Lrrn;
             When empCount=2;
                scConEmpN2=E3eNam;
                If E3phon <> ' ';

                pos = %check(digits:E3phon);

                if pos > 0;
                  scConEmpP2=*zeros;
                else;
                  scConEmpP2=%Dec(E3phon:10:0);
                endif;

                Else;
                   scConEmpP1 = *Zeros;
                Endif;
                scConEmpW2=E3wage;
                scConRrn2=WDE3Lrrn;
             Other;
                Leave;
             EndSl;
             Reade keyWDE3L pe300;
          EndDo;
       EndIf;

       If froiRrn <> 0 and %parms() >=3;
          Chain(n) froiRrn PFR0P;
          cicFlg = *On;
          If FRdbdt <> ' ';
             scDisDt    = dtMDYY2(%Int(FRdbdt));
          Endif;
          If FRdod = *On;
             scDisNotDt = dtMDYY2(%Int(FRdod));
          Endif;
          If FRdtlw <> ' ';
              scLstWkDt  = dtMDYY2(%Int(FRdtlw));
          Endif;
          If FRrtw <> ' ';
             scActRtw   = dtMDYY2(%Int(FRrtw));
          Endif;
          scFullSal  = FRfsal;
          scOthPmt   = FRotwk;
          scPaidInDt = FRpoi;
          scWagePost = FRpoww;
          scWagePre  = FRprww;
          //*** Begin Add *** 10/26/21 *************************************
          scWagePrd = FRfreq;
          //*** End   Add *** 10/26/21 *************************************
       Endif;
       //*** Beg Add *** 02/14/22 **********************************************
       scOpYa = CMoPyA;
       scWhAf = CMwHaF;
       scNtZ  = CMnTz;
       scArDt = dtMdyy(CMaRdT);

       // Determine if fields are to be enabled
       outCompany  = CMco#;
       outJurState = CMjrSt;
       outTranId   = *blanks;
       outFormatId = *blanks;
       inRtnCode   = *blanks;
       isArDtDis = *on;
       isOpYaDis = *on;
       isWhAfDis = *on;
       isNtZdis  = *on;
       EdiScrCtrl(outCompany:outJurState:outTranId:outFormatId:inD433:inD440:
        inD441:inD441D:inD442:inRtnCode);
       If inRtnCode = '0';
          If inD433 = 'Y';
             isOpYaDis = *off;
          Endif;
          If inD440 = 'Y';
             isWhAfDis = *off;
          Endif;
          If inD441 = 'Y' and inD441D <> 0 and CMacDt >= inD441D;
             isArDtDis = *off;
          Endif;
          If inD442 = 'Y';
             isNtZdis = *off;
          Endif;
          If inD433 = 'Y';
             isOpYaDis = *off;
          Endif;
       Endif;
       //*** End Add *** 02/14/22 **********************************************

      /End-Free
     P LoadBenInfo     E
       //===================================================================
       // LoadLegInfo - Loads the Legal Information Screen
       //===================================================================
     P LoadLegInfo     B                   Export
     D LoadLegInfo     PI
     D claimRrnIn                     9  0
     D legalPtrIn                      *
     D legalInfo     e ds                  ExtName(wtClmDtl:rcdClmDt4)
     D                                     based(legalPtr)
     D legalPtr        s               *   inz
     D claimRrn        s              9  0
      /Free

       legalPtr=legalPtrIn;
       claimRrn=claimRrnIn;

       Chain(n) claimRrn pcm0p;

       scLegList='PDFEIN || '' '' || PDMOD || ''-'' || ' +
        'CASE WHEN PDDBA1=''Y'' THEN TRIM(PDADR1) || ''-'' || ' +
        'TRIM(PDADR2) ELSE ' +
        'TRIM(PDNAME) || ''-'' || TRIM(PDADR1) || '' '' || ' +
        'TRIM(PDADR2) END ' +
        ' || ''- '' || TRIM(PDCTY) || '', '' || ' +
        'PDST || '' '' || SUBSTR(DIGITS(PDZIP),1,5)';

          // MS- Selection Criteria
             kkTbId=CMjrSt + 'MS';
             Chain keyWTGIL pgi00;
             If Not %Found(WTGIL);
             kkTbId='  MS';
             EndIf;

             selMS ='GTTBID=''' + kkTbId + ''' AND GTSTAT<>''I''';

       Chain(n) keyWMLLL pll00;
       If %Found(WMLLL);
          scDefFein=%EditC(LLdefA:'X') + %EditC(LLdefM:'X');
          kkFein=LLdefA;
          kkMod=LLdefM;

          scDefName = LLdefN;
          scDefAdr1 = LLdAd1;
          scDefAdr2 = LLdAd2;
          scDefPhn# = LLdefR*10000000 + LLdefP;
          scDefCity = LLdCty;
          scDefSt   = LLdSt;
          scDefZip  = LLdZip;
          scDefEmAd = LLemdD;

          Chain keyWMPDL ppd00;
          If %Found(WMPDL);
             If scDefName = *Blanks;
                If PDfsn <> *Blanks;
                   scDefName=%Trim(PDfsn) + ' ' + PDname;
                Else;
                   scDefName=PDname;
                EndIf;
             EndIf;
             If PDdba1 = 'Y';
                If scDefName = *Blanks;
                   scDefName = PDadr1;
                Endif;
                If scDefAdr1 = *Blanks;
                   scDefAdr1 = PDadr2;
                Endif;
             Else;
                If scDefAdr1 = *Blanks;
                   scDefAdr1 = PDadr1;
                Endif;
                If scDefAdr2 = *Blanks;
                   scDefAdr2 = PDadr2;
                Endif;
             EndIf;
             If scDefCity = *Blanks;
                scDefCity = PDcty;
             Endif;
             If scDefSt = *Blanks;
                scDefSt = PDst;
             Endif;
             If scDefZip = *Zeros;
                scDefZip = PDzip;
             Endif;
             If scDefPhn# = *Zeros;
                scDefPhn# = PDarea*10000000 + PDphn#;
             Endif;
             If scDefEmAd = *Blanks;
                scDefEmAd = PDemAd;
             EndIf;
          EndIf;


          scDefAttDt=dtMDYY(LLdeDt);

          scPlaFein=%EditC(LLplaA:'X') + %EditC(LLplaM:'X');
          kkFein=LLplaA;
          kkMod=LLplaM;

          scPlaName  = LLplaN;
          scPlaAdr1  = LLpAd1;
          scPlaAdr2  = LLpAd2;
          scPlaPhn#  = LLplaR*10000000 + LLplaP;
          scPlaRepNm = LLrfNm;
          scPlaCity  = LLpcty;
          scPlaSt    = LLpSt;
          scPlaZip   = LLpZip;
          scPlaEmAd  = LLemdP;
          If LLWcabN <> *Blanks;
             scWcab  = LLWcabN;              //Load screen field from New WCAB#
          Else;                              //Else load screen field from old W
             scWcab  = LLWcab;
          Endif;
          scIaPhn#  = LLiaAr*10000000 + LLiaPh;

          Chain keyWMPDL ppd00;
          If %Found(WMPDL);
             If scPlaName = *Blanks;
                If PDfsn <> *Blanks;
                   scPlaName = %Trim(PDfsn) + ' ' + PDname;
                Else;
                   scPlaName = PDname;
                EndIf;
             EndIf;
             If PDdba1='Y';
                If scPlaName = *Blanks;
                   scPlaName = PDadr1;
                EndIf;
                If scPlaAdr1 = *Blanks;
                   scPlaAdr1 = PDadr2;
                Endif;
             Else;
                If scPlaAdr1 = *Blanks;
                   scPlaAdr1 = PDadr1;
                Endif;
                If scPlaAdr1 = *Blanks;
                   scPlaAdr2 = PDadr2;
                Endif;
             EndIf;
             If scPlaRepNm = *Blanks;
                scPlaRepNm = PDrfNm;
             Endif;
             If scPlaCity = *Blanks;
                scPlaCity = PDcty;
             Endif;
             If scPlaSt = *Blanks;
                scPlaSt = PDst;
             Endif;
             If scPlaZip = *Zeros;
                scPlaZip = PDzip;
             Endif;
             If scPlaPhn# = *Zeros;
                scPlaPhn# = PDarea*10000000 + PDphn#;
             Endif;
             If scPlaEmAd = *Blanks;
                scPlaEmAd = PDemAd;
             EndIf;
          Endif;
          scLumpSum=LLlump;
          scPlaRepIn=LLclRi;
          scPlaRepDt=dtMDYY(LLdtAp);
          scLitig=LLlitg;
          scMethSett=LLmtSt;
          scSettDt=dtMDYY(LLstDt);
          scSettAmt=LLsAmt;
          scSetIndDt=dtMDYY(LLiScm);
          scSetMedDt=dtMDYY(LLmScm);
          scFraudInd=LLfrau;
          If scFraudInd=*Blanks;
             scFraudInd='00';
          EndIf;
          scSuitDate=dtMDYY(LLlwDt);
          scDelayDt=dtMDYY(LLfDly);
          //*** Beg Add ***** 03/06/25 ***************************
          scStruct = LLstruct;
          scMSAAmt = LLmsaamt;
          scMSAPrd = LLmsaprd;
          scInDepAmt = LLindepamt;
          scADepAmt = LLadepamt;
          scCasCnt# = LLcascnt#;
          scPrAdEIN = LLpradein;
          //*** End Add ***** 03/06/25 ***************************
       Else;
          scDefFein=*Blanks;
          scDefName=*Blanks;
          scDefAttDt=*Zeros;
          scDefAdr1=*Blanks;
          scDefAdr2=*Blanks;
          scDefCity=*Blanks;
          scDefSt=*Blanks;
          scDefZip=*Zeros;
          scDefPhn#=*Zeros;
          scPlaFein=*Blanks;
          scPlaName=*Blanks;
          scPlaRepNm=*Blanks;
          scPlaRepDt=*Zeros;
          scPlaRepIn=*Blanks;
          scPlaAdr1=*Blanks;
          scPlaAdr2=*Blanks;
          scPlaCity=*Blanks;
          scPlaSt=*Blanks;
          scPlaZip=*Zeros;
          scPlaPhn#=*Zeros;
          scLitig=*Blanks;
          scMethSett=*Blanks;
          scSettDt=*Zeros;
          scSettAmt=*Zeros;
          scSetIndDt=*Zeros;
          scSetMedDt=*Zeros;
          scFraudInd='00';
          scSuitDate=*Zeros;
          scDelayDt=*Zeros;
          scDefEmAd=*Blanks;
          scPlaEmAd=*Blanks;
          scLumpSum=*Blanks;
          scWcab  = *blanks;
          scIaPhn#  = *zeros;
          //*** Beg Add ***** 03/06/25 ***************************
          scStruct = *Blanks;
          scMSAAmt = *Zeros;
          scMSAPrd = *Zeros;
          scInDepAmt = *Zeros;
          scADepAmt = *Zeros;
          scCasCnt# = *Blanks;
          scPrAdEIN = *Zeros;
          //*** End Add ***** 03/06/25 ***************************
       EndIf;

      /End-Free
     P LoadLegInfo     E
       //===================================================================
       // LoadStateInfo - Loads the State Data.
       //===================================================================
     P LoadStateInfo   B                   Export
     D LoadStateInfo   PI
     D claimRrnIn                     9  0
     D statePtrIn                      *
     D stateIn                        2

     D state           s              2
     D statePtr        s               *
     D claimRrn        s              9  0
      /Free

       state=stateIn;
       kkState=stateIn;
       statePtr=statePtrIn;
       claimRrn=claimRrnIn;

       Chain(n) claimRrn pcm0p;

       // get Claimant FROI Data.
       Chain(n) keyWMNIL pni00;

       If Not %found;
         Clear Pni00;
       EndIf;

       Select;
       // A1 - Data
       When state='A1';
          loadStateA1(statePtr);
       // AL - Data
       When state='AL';
          loadStateAL(statePtr);
       // CA - Data
       When state='CA';
          loadStateCA(statePtr);
       // DE - Data
       When state='DE';
          loadStateDE(statePtr);
       // FL - Data
       When state='FL';
          loadStateFL(statePtr);
       // GA - Data
       When state='GA';
          loadStateGA(statePtr);
       // LA - Data
       When state='LA';
          loadStateLA(statePtr);
       // MA - Data
       When state='MA';
          loadStateMA(statePtr);
       // NC - Data
       When state='NC';
          loadStateNC(statePtr);
       // NY - Data
       When state='NY';
          loadStateNY(statePtr);
       // PA - Data
       When state='PA';
          loadStatePA(statePtr);
       // WV - Data
       When state='WV';
          loadStateWV(statePtr);
       Other;
          loadStateNI(statePtr);
       EndSl;

      /End-Free
     P LoadStateInfo   E
       //===================================================================
       // LoadStateA1 - Loads the State Data - A1
       //===================================================================
     P LoadStateA1     B
     D LoadStateA1     PI
     D statePtrIn                      *

     D a1Info        e ds                  ExtName(wtClmDtl:rcdClmDtA1)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       //*** Beg Chg *** 09/06/22 **********************************************
       //***scEquip1A1=NI60a1;
       //***scEquip2A1=NI60a2;
       //***scActiv1A1=NI60a3;
       //***scActiv2A1=NI60a4;
       //***scProcess1=NI60a5;
       //***scProcess2=NI60a6;
       //***scEvents1=NI80a1;
       //***scEvents2=NI80a2;
       scEquip1A1 = NI60a1 + NI60a2;
       scActiv1A1 = NI60a3 + NI60a4;
       scProcess1 = NI60a5 + NI60a6;
       scEvents1  = NI80a1 + NI80a2;
       //*** End Chg *** 09/06/22 **********************************************
       scSafe=NI1a1;
       scUsed=NI1a2;
       scWitnA11=%Subst(NI80a3:1:40);
       scWitnA12=%Subst(NI80a3:41:40);
       scHireSt=NI10a2;
       scBegWrkA1=NItim2;

       position=%Scan(',':NIprNm:1);
       length=position-1;
       If length<>-1;
          scPreparer=%Subst(NIprNm:1:length);
          position=position+1;
          scPrepTitl=%Subst(NIprNm:position:19);
       Else;
          //*** Beg Add *** 09/06/22 *******************************************
          If NIprNm = *blanks;
          //*** End Add *** 09/06/22 *******************************************
             scPreparer=*Blanks;
             scPrepTitl=*Blanks;
          //*** Beg Add *** 09/06/22 *******************************************
          Else;
             scPreparer = %Trim(NIprNm);
             scPrepTitl = *blanks;
          EndIf;
       //*** End Add *** 09/06/22 **********************************************
       EndIf;

       If NIprPh='nullnull' or NIprPh=*Blanks;
          NIprPh='0000000000';
       EndIf;
       scPrepPhn=%Dec(NIprPH:10:0);
       //*** Beg Add *** 09/06/22 **********************************************
       scDtPrep = DtMdYy(NIprep);

       // Load A1 screen fields from the Froi fields if not already populated
       froiState = getFroiState(CMco#:CMjrst);
       If froiState <> *blanks and froiState <> 'A1';
          kkState = froiState;
          Chain(n) keyWMNIL pni00;
          Select;
             When kkState = 'AL';
                If scActiv1A1 = *blanks;
                   scActiv1a1 = NI60a3 + NI60a4;
                Endif;
                If scEvents1 = *blanks;
                   scEvents1 = NI80a1 + NI80a2;
                Endif;
             When kkState = 'CA';
                scBegWrkA1 = NItim2;
             When kkState = 'DE';
                scEquip1A1 = NI60a1 + NI60a2;
                scActiv1A1 = NI60a3 + NI60a4;
                scProcess1 = NI60a5 + NI60a6;

                position=%Scan(',':NIprNm:1);
                length=position-1;
                If length<>-1;
                   scPreparer=%Subst(NIprNm:1:length);
                   position=position+1;
                   scPrepTitl=%Subst(NIprNm:position:19);
                Else;
                   scPreparer=*Blanks;
                   scPrepTitl=*Blanks;
                EndIf;
                scBegWrkA1 = NItim2;
             When kkState = 'MA';
                scEquip1A1 = NI60a1 + NI60a2;
                scEvents1  = NI80a1 + NI80a2;
                position=%Scan(',':NIprNm:1);
                length=position-1;
                If length<>-1;
                   scPreparer=%Subst(NIprNm:1:length);
                   position=position+1;
                   scPrepTitl=%Subst(NIprNm:position:19);
                Else;
                   scPreparer=*Blanks;
                   scPrepTitl=*Blanks;
                EndIf;
             When kkState = 'NC';
             When kkState = 'NY';
                scEquip1A1 = NI60a1 + NI60a2;
                scActiv1A1 = NI60a3 + NI60a4;
                scProcess1 = NI60a5 + NI60a6;
                position=%Scan(',':NIprNm:1);
                length=position-1;
                If length<>-1;
                   scPreparer=%Subst(NIprNm:1:length);
                   position=position+1;
                   scPrepTitl=%Subst(NIprNm:position:19);
                Else;
                   scPreparer=*Blanks;
                   scPrepTitl=*Blanks;
                EndIf;
                If NIprPh='nullnull' or NIprPh=*Blanks;
                   NIprPh='0000000000';
                EndIf;
                scPrepPhn=%Dec(NIprPH:10:0);
                scBegWrkA1 = NItim2;
             When kkState = 'PA';
                scEquip1A1 = NI60a1 + NI60a2;
                scEvents1  = NI80a1 + NI80a2;
                scSafe=NI1a1;
                scUsed=NI1a2;
                scBegWrkA1 = NItim2;
                position=%Scan(',':NI80a3:1);
                length=position-1;
                If length>0;
                   strWitnFsn=%Subst(NI80a3:1:length);
                Else;
                   strWitnFsn=*Blanks;
                EndIf;

                position=position+1;
                position2=%Scan(',':NI80a3:position);
                length=position2-position;
                If length>0;
                   strWitnLsn=%Subst(NI80a3:position:length);
                Else;
                   strWitnLsn=*Blanks;
                EndIf;
                scWitnA11 = %Trim(strWitnFsn) + ' ' + %Trim(strWitnLsn);
                position2 = position2 + 1;

                Monitor;
                   scWitnA12 = %Subst(Ni80a3:position2:10);
                On-Error 105;
                   scWitnA12 = *blanks;
                EndMon;
                position=%Scan(',':NIprNm:1);
                length=position-1;
                If length<>-1;
                   scPreparer=%Subst(NIprNm:1:length);
                   position=position+1;
                   scPrepTitl=%Subst(NIprNm:position:19);
                Else;
                   scPreparer=*Blanks;
                   scPrepTitl=*Blanks;
                EndIf;
                If NIprPh='nullnull' or NIprPh=*Blanks;
                   NIprPh='0000000000';
                EndIf;
                scPrepPhn=%Dec(NIprPH:10:0);
             When kkState = 'WV';
                scEvents1  = NI80a1 + NI80a2;
          Endsl;
       Endif;
       //*** End Add *** 09/06/22 **********************************************


      /End-Free
     P LoadStateA1     E
       //===================================================================
       // LoadStateAL - Loads the State Data - AL
       //===================================================================
     P LoadStateAL     B
     D LoadStateAL     PI
     D statePtrIn                      *

     D alInfo        e ds                  ExtName(wtClmDtl:rcdClmDtAL)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scEmpId#=NI20a2;
       scSpecProd=NI40a7;
       scWcmsProv=NI1a1;
       scCarSelf#=NI30a4;

       Monitor;
          scAgeAL=%Dec(NI10a1:10:0);
       On-Error 105;
          scAgeAL=0;
       EndMon;

       scRtwOccAL=NI40a8;
       scLosYear=NI3n2;
       scLosMonth=NI3n3;
       scTimeYear=NI3n4;
       scTimeMnth=NI3n5;
       scOthWage=NI9n1;
       //*** Beg Del *** 09/06/22 **********************************************
       //***scProdInj1=NI60a3;
       //***scProdInj2=NI60a4;
       //*** End Del *** 09/06/22 **********************************************
       //*** Beg Chg *** 09/06/22 **********************************************
       //***scAccOcc1=NI60a1;
       //***scAccOcc2=NI60a2;
       scAccOcc1=NI60a1 + NI60A2;
       //*** End Chg *** 09/06/22 **********************************************
       //*** Beg Del *** 09/06/22 **********************************************
       //***scDescEvn1=NI80a1;
       //***scDescEvn2=NI80a2;
       //*** End Del *** 09/06/22 **********************************************

      /End-Free
     P LoadStateAL     E
       //===================================================================
       // LoadStateCA - Loads the State Data - CA
       //===================================================================
     P LoadStateCA     B
     D LoadStateCA     PI
     D statePtrIn                      *

     D caInfo        e ds                  ExtName(wtClmDtl:rcdClmDtCA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scUnEmpIns=NI30a2;
       scTypeEmp=NI20a1;
       scFormDt=dtMDYY(NIdt1);
       //*** Beg Del *** 09/06/22 **********************************************
       //***scBegWrkCA=NItim2/100;
       //*** End Del *** 09/06/22 **********************************************
       scGrossWag=NI9n3/100;
       scWagePer=NI40a6;
       //*** Beg Chg *** 09/06/22 **********************************************
       //***scEquip1CA=NI80a4;
       //***scEquip2CA=NI80a5;
       //***scActiv1CA=NI80a6;
       //***scActiv2CA=NI80a7;
       scEquip1CA =NI80a4 + NI80a5;
       scActiv1CA= NI80a6 + NI80a7;
       //*** End Chg *** 09/06/22 **********************************************
       scCountyCA=NI30a3;
       scExpDept=NI40a4;
       scHospInp=NI1a5;
       scEmrgRoom=NI1a6;

      /End-Free
     P LoadStateCA     E
       //===================================================================
       // LoadStateDE - Loads the State Data - DE
       //===================================================================
     P LoadStateDE     B
     D LoadStateDE     PI
     D statePtrIn                      *

     D deInfo        e ds                  ExtName(wtClmDtl:rcdClmDtDE)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scCountyDE=NI30a2;

       Monitor;
          scAgeDE=%Dec(NI10a1:10:0);
       On-Error 105;
          scAgeDE=0;
       EndMon;

       scEmpDept=NI40a4;
       scEmpLong=NI20a3;
       scInjDept=NI40a8;
       //*** Beg Del *** 09/06/22 **********************************************
       //***scWorkPrc1=NI60a5;
       //***scWorkPrc2=NI60a6;
       //***scWorkPrc1 = NI60a5 + NI60a6;

       //***position=%Scan(',':NIprNm:1);
       //***length=position-1;
       //***If length<>-1;
       //***   scPersonDE=%Subst(NIprNm:1:length);
       //***   position=position+1;
       //***   length=10;
       //***   scPositDE=%Subst(NIprNm:position:length);
       //***Else;
       //***   scPersonDE=*Blanks;
       //***   scPositDE=*Blanks;
       //***EndIf;
       //*** End Del *** 09/06/22 **********************************************

       //*** Beg Del *** 09/06/22 **********************************************
       //***scEqupDE1=NI60a1;
       //***scEqupDE2=NI60a2;
       //***scActivDE1=NI60a3;
       //***scActivDE2=NI60a4;
       //*** End Del *** 09/06/22 **********************************************
       scRptDate=dtMDYY(NIprep);
       //*** Beg Del *** 09/06/22 **********************************************
       //***scStrTime=NItim2/100;
       //*** End Del *** 09/06/22 **********************************************
       scSameWage=NI1a2;
       scFullPaid=dtMDYY(NIdt2);

      /End-Free
     P LoadStateDE     E
       //===================================================================
       // LoadStateFL - Loads the State Data - FL
       //===================================================================
     P LoadStateFL     B
     D LoadStateFL     PI
     D statePtrIn                      *

     D flInfo        e ds                  ExtName(wtClmDtl:rcdClmDtFL)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scDenyCase=CMdny;
       scIndDeny=NI1a6;
       scMedOnly=NI1a3;
       sc8thDay=dtMDYY(NIdt1);
       scKnow8th=dtMDYY(NIdt2);
       scLstTimFL=NI1a5;
       scMailDate=dtMDYY(NIdt4);
       scDisType=NI10a1;
       scPenAmt=NI9n1/100;
       scIntamt=NI9n2/100;
       //*** Beg Chg *** 09/06/22 *********************************************
       //***scRmrk1FL=NI40a1;
       scRmrk1FL = NI40a1 + NI40a2;
       //*** End Chg *** 09/06/22 *********************************************
       //*** Beg Del *** 09/06/22 *********************************************
       //***scRmrk2FL=NI40a2;
       //*** End Del *** 09/06/22 *********************************************
       scRtPay=NI9n3/100;
       scPer=NI20a9;
       scHrsPrWk=NI3n1;

      /End-Free
     P LoadStateFL     E
       //===================================================================
       // LoadStateGA - Loads the State Data - GA
       //===================================================================
     P LoadStateGA     B
     D LoadStateGA     PI
     D statePtrIn                      *

     D gaInfo        e ds                  ExtName(wtClmDtl:rcdClmDtGA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scEmEmailG=NI40a3;
       scClmEmail=NI40a4;
       scSbwcId#=NI5n3;
       scPerEmail=NI40a5;
       scDaysOff=NI20a1;
       scWeekBen=NI5n2;
       scPayFrom=dtMDYY(NIdt1);
       scTtTpPpCd=NI10a2;
       scPercGA=NI3n1;
       scBodyPart=NI10a3;
       scNumWeeks=NI3n2;
       scUntlDate=dtMDYY(NIdt2);
       scBenefits=NI60a1;

      /End-Free
     P LoadStateGA     E
       //===================================================================
       // LoadStateLA - Loads the State Data - LA
       //===================================================================
     P LoadStateLA     B
     D LoadStateLA     PI
     D statePtrIn                      *

     D laInfo        e ds                  ExtName(wtClmDtl:rcdClmDtLA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scRptDtLA=dtMDYY(NIprep);
       scRtwDate=dtMDYY(CMrtw);
       scSmWageLA=NI1a2;
       scNormTime=NItim2;
       scLastDate=dtMDYY(NIdt2);
       sc7DayDis  = 'N';
       scPossDisp = 'N';
       scMdOnlyLA = 'N';
       scInjDeath = 'N';
       scLumpSett = 'N';
       scAmpDis   = 'N';
       scOtherLA  = 'N';
       If %scan('1':NI20a4:1) <> 0;
          sc7DayDis  = 'Y';
       Endif;
       If %scan('2':NI20a4:1) <> 0;
          scPossDisp = 'Y';
       Endif;
       If %scan('3':NI20a4:1) <> 0;
          scMdOnlyLA = 'Y';
       Endif;
       If %scan('4':NI20a4:1) <> 0;
          scInjDeath = 'Y';
       Endif;
       If %scan('5':NI20a4:1) <> 0;
          scLumpSett = 'Y';
       Endif;
       If %scan('6':NI20a4:1) <> 0;
          scAmpDis   = 'Y';
       Endif;
       If %scan('7':NI20a4:1) <> 0;
          scOtherLA  = 'Y';
       Endif;
       scInjPar=NI20a3;
       scPlaPar=NI30a3;
       scDeptLA=NI40a4;
       scActivLA1=NI60a3;
       scActivLA2=NI60a4;
       scCauseLA1=NI80a1;
       scCauseLA2=NI80a2;
       scOccDisDt=dtMDYY(NIdt3);
       scPersform=NIprNm;
       scEmpAddLA=NI40a1;
       scEmpCtyLA=NI25a1;
       scEmpStLA=NI2a1;
       scEmpZipLA=NI9n2;

      /End-Free
     P LoadStateLA     E
       //===================================================================
       // LoadStateMA - Loads the State Data - MA
       //===================================================================
     P LoadStateMA     B
     D LoadStateMA     PI
     D statePtrIn                      *

     D maInfo        e ds                  ExtName(wtClmDtl:rcdClmDtMA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scAwwInd=NI1a2;
       scIndcode=NI2a2;
       scSlfIns=NI1a1;
       scSlfIns#=NI40a3;
       scBusType=NI20a7;
       sc5thDay=dtMDYY(NIdt1);
       //*** Beg Del *** 09/06/22 *********************************************
       //***scSrcInj1=NI60a1;
       //***scSrcInj2=NI60a2;
       //***scInjOcc1=NI80a1;
       //***scInjOcc2=NI80a2;
       //*** End Del *** 09/06/22 *********************************************
       scPersRpt=NIcont;
       scWorkRel=dtMDYY(NIdt2);
       scInjCode1=NI10a1;
       scInjCode2=NI10a2;
       scInjCode3=NI10a3;
       scBodCode1=NI10a4;
       scBodCode2=NI10a5;
       scBodCode3=NI20a6;
       //*** Beg Chg *** 09/06/22 *********************************************
       //***scWitnMA1=NI40a4;
       //***scWitnMA2=NI40a5;
       scWitnMA1 = NI40a4 + NI40a5;
       //*** End Chg *** 09/06/22 *********************************************
       scRetRegOc=NI1a4;

       //*** Beg Del *** 09/06/22 *********************************************
       //***position=%Scan(',':NIprNm:1);
       //***length=position-1;
       //***If length<>-1;
       //***   scPrepMA=%Subst(NIprNm:1:length);
       //***   position=position+1;
       //***   scPrpTtlMA=%Subst(NIprNm:position:19);
       //***Else;
       //***   scPrepMA=*Blanks;
       //***   scPrpTtlMA=*Blanks;
       //***EndIf;

       //***scPrepDtMA=dtMDYY(NIprep);
       //*** End Del *** 09/06/22 *********************************************

      /End-Free
     P LoadStateMA     E
       //===================================================================
       // LoadStateNC - Loads the State Data - NC
       //===================================================================
     P LoadStateNC     B
     D LoadStateNC     PI
     D statePtrIn                      *

     D ncInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNC)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       //*** Beg Chg *** 09/06/22 **********************************************
       //***scEmpNat1=NI60a1;
       //***scEmpNat2=NI60a2;
       scEmpNat1 = NI60a1 + NI60a2;
       //*** End Chg *** 09/06/22 **********************************************
       scPlantLoc=NI40a1;
       scCountyNC=NI30a1;
       scDeptNC=NI30a2;
       scEmpPrem=NI20a1;
       scDisDate=dtMDYY(CMdbDt);
       scDisAmPm=NI1a3;

       Monitor;
          scEmpYear=%Dec(%Subst(NI20a3:1:2):2:0);
       On-Error 105;
          scEmpYear=0;
       EndMon;

       Monitor;
          scEmpMon=%Dec(%Subst(NI20a3:3:2):2:0);
       On-Error 105;
          scEmpMon=0;
       EndMon;

       Monitor;
          scEmpDay=%Dec(%Subst(NI20a3:5:2):2:0);
       On-Error 105;
          scEmpDay=0;
       EndMon;

       Monitor;
          scWagePerH=%Dec(NI10a1:10:2)/100;
       On-Error 105;
          scWagePerH=0;
       EndMon;

       Monitor;
          scWagePerD=%Dec(NI10a2:10:2)/100;
       On-Error 105;
          scWagePerD=0;
       EndMon;

       Monitor;
          scEstVal=%Dec(NI10a3:10:2)/100;
       On-Error 105;
          scEstVal=0;
       EndMon;

       scPerPrd=NI10a4;
       //*** Beg Chg *** 09/06/22 **********************************************
       //***scInjList1=NI60a3;
       //***scInjList2=NI60a4;
       scInjList1 = NI60a3 + NI60a4;
       //*** End Chg *** 09/06/22 **********************************************

       Monitor;
          scHourRtw=%Dec(%Subst(NI10a5:1:4):4:0);
       On-Error 105;
          scHourRtw=0;
       EndMon;

       scHourAmPm=*Blanks;
       Select;
       When NI1a6='X';
         scHourAmPm='A';
       When NI1a7='X';
         scHourAmPm='P';
       Endsl;
       Monitor;
          scRtwWages=%Dec(%Subst(NI20a2:1:10):10:2)/100;
       On-Error 105;
          scRtwWages=0;
       EndMon;

       scPerPrd2=NI20a4;
       scRtwOcc=NI60a5;
       scPhysTrt=NI1a8;

      /End-Free
     P LoadStateNC     E
       //===================================================================
       // LoadStateNY - Loads the State Data - NY
       //===================================================================
     P LoadStateNY     B
     D LoadStateNY     PI
     D statePtrIn                      *

     D nyInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNY)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scEmpBegDt=NItim2;
       scTotEarn=NI5n1;
       If %Scan('1':NI10a1:1)<>0;
          scMon='1';
       EndIf;
       If %Scan('2':NI10a1:1)<>0;
          scTue='2';
       EndIf;
       If %Scan('3':NI10a1:1)<>0;
          scWed='3';
       EndIf;
       If %Scan('4':NI10a1:1)<>0;
          scThu='4';
       EndIf;
       If %Scan('5':NI10a1:1)<>0;
          scFri='5';
       EndIf;
       If %Scan('6':NI10a1:1)<>0;
          scSat='6';
       EndIf;
       If %Scan('7':NI10a1:1)<>0;
          scSun='7';
       EndIf;
       scCountyNY=NI30a3;
       scDeptNY=NI40a4;
       scEmpProv=NI1a1;
       scMedCareD=dtMDYY(NIdt1);
       scEmpTreat=NI1a2;
       scEmpHos=NI1a3;
       //*** Beg Del *** 09/06/22 *********************************************
       //***scEmpDo1=NI60a5;
       //***scEmpDo2=NI60a6;
       //***scHowOcc1=NI60a3;
       //***scHowOcc2=NI60a4;
       //***scObjInj1=NI60a1;
       //***scObjInj2=NI60a2;
       //*** End Del *** 09/06/22 *********************************************
       scContNY=NI40a5;
       sccontPhNY=NI30a5;
       scComName=%Subst(NI80a7:1:40);
       scComAdr1=%Subst(NI80a7:41:40);
       scComAdr2=%Subst(NI80a8:1:40);
       scComCity=%Subst(NI80a8:41:25);
       scComSt=%Subst(NI80a8:66:2);

       Monitor;
          scComZip=%Dec(%Subst(NI80a8:68:9):9:0);
       On-Error 105;
          scComZip=0;
       EndMon;

       scRelName=%Subst(NI80a5:1:40);
       scRelAdr1=%Subst(NI80a5:41:40);
       scRelAdr2=%Subst(NI80a6:1:40);
       scRelCity=%Subst(NI80a6:41:25);
       scRelSt=%Subst(NI80a6:66:2);

       Monitor;
          scRelZip=%Dec(%Subst(NI80a6:68:9):9:0);
       On-Error 105;
          scRelZip=0;
       EndMon;

       scRelRel=NI10a3;
       //*** Beg Del *** 09/06/22 *********************************************
       //***position=%Scan(',':NIprNm:1);
       //***length=position-1;
       //***If length<>-1;
       //***   scPrepNY=%Subst(NIprNm:1:length);
       //***   position=position+1;
       //***   scPrpTtlNY=%Subst(NIprNm:position:19);
       //***Else;
       //***   scPrepNY=*Blanks;
       //***   scPrpTtlNY=*Blanks;
       //***EndIf;

       //***If NIprPh='nullnull' or NIprPh=*Blanks;
       //***   NIprPh='0000000000';
       //***EndIf;
       //***scPrpPhnNY=%Dec(NIprPH:10:0);
       //*** End Del *** 09/06/22 *********************************************

      /End-Free
     P LoadStateNY     E
       //===================================================================
       // LoadStatePA - Loads the State Data - PA
       //===================================================================
     P LoadStatePA     B
     D LoadStatePA     PI
     D statePtrIn                      *

     D paInfo        e ds                  ExtName(wtClmDtl:rcdClmDtPA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       //*** Beg Del *** 09/06/22 *********************************************
       //***scBegShft=NItim2;
       //*** End Del *** 09/06/22 *********************************************

       position=%Scan(',':NIcont:1);
       length=position-1;
       If length<>-1;
          scContFsn=%Subst(NIcont:1:length);
          position=position+1;
          length=27;
          scContLsn=%Subst(NIcont:position:length);
       Else;
          scContFsn=*Blanks;
          scContLsn=*Blanks;
       EndIf;

       scContPhn#=NIcAr*10000000 + NIcPhn;
       //*** Beg Del *** 09/06/22 *********************************************
       //***scSafeProv=NI1a1;
       //***scSafeUsed=NI1a2;
       //*** End Del *** 09/06/22 *********************************************
       scCountyPA=NI30a4;
       //*** Beg Del *** 09/06/22 *********************************************
       //***scMatUsed1=NI60a1;
       //***scMatUsed2=NI60a2;
       //***scSeqDesc1=NI80a1;
       //***scSeqDesc2=NI80a2;
       //*** End Del *** 09/06/22 *********************************************
       scTools=NI1a3;
       scMechDef=NI1a4;
       scUnSafe=NI1a5;
       scUnSafeCn=NI1a6;
       scAmputate=NI1a7;
       scVehicle=NI1a8;
       scCovered=NI1a9;
       //*** Beg Del *** 09/06/22 *********************************************
       //***position=%Scan(',':NI80a3:1);
       //***length=position-1;
       //***If length>0;
       //***   scWitnFsn=%Subst(NI80a3:1:length);
       //***Else;
       //***   scWitnFsn=*Blanks;
       //***EndIf;

       //***position=position+1;
       //***position2=%Scan(',':NI80a3:position);
       //***length=position2-position;
       //***If length>0;
       //***   scWitnLsn=%Subst(NI80a3:position:length);
       //***Else;
       //***   scWitnLsn=*Blanks;
       //***EndIf;

       //***position2=position2+1;

       //***Monitor;
       //***   scWitnPhn=%Dec(%Subst(Ni80a3:position2:10):10:0);
       //***On-Error 105;
       //***   scWitnPhn=0;
       //***EndMon;
       //*** End Del *** 09/06/22 *********************************************

       scCarBurCd=NI20a4;

       //*** Beg Del *** 09/06/22 *********************************************
       //***position=%Scan(',':NIprNm:1);
       //***length=position-1;
       //***remlen = %len(NIprNm) - position;
       //***If length<>-1;
       //***   scPrepPA=%Subst(NIprNm:1:length);
       //***   position=position+1;
       //***   scPrpTtlPA=%Subst(NIprNm:position:remlen);
       //***Else;
       //***   scPrepPA=*Blanks;
       //***   scPrpTtlPA=*Blanks;
       //***EndIf;

       //***If NIprPh='nullnull' or NIprPh=*Blanks;
       //***   NIprPh='0000000000';
       //***EndIf;
       //***scPrpPhnPA=%Dec(NIprPH:10:0);
       //*** End Del *** 09/06/22 *********************************************
       scPrepDtPA=dtMDYY(NIprep);

      /End-Free
     P LoadStatePA     E
       //===================================================================
       // LoadStateWV - Loads the State Data - WV
       //===================================================================
     P LoadStateWV     B
     D LoadStateWV     PI
     D statePtrIn                      *

     D wvInfo        e ds                  ExtName(wtClmDtl:rcdClmDtWV)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       sc1a8  = NI1a8;
       sc1a9  = NI1a9;

       scDt2  = dtMDYY(NIdt2);
       If scDt2 <> *Zeros;
         scA10  = 'X';
       EndIf;

       sc60A7 = NI60a7;
       //*** Beg Add *** 09/06/22 ********************************************
       //***sc80A1 = NI80a1;
       //***sc80A2 = NI80a2;
       //*** End Add *** 09/06/22 ********************************************
       sc1a3  = NI1a3;
       sc1a4  = NI1a4;
       sc30a5 = NI30a5;
       sc1a5  = NI1a5;
       sc1a6  = NI1a6;
       sc5n1  = NI5n1;
       sc1a7  = NI1a7;
       sc92n1 = NI92n1;
       sc2a2  = NI2a2 ;
       sc1a10 = NI1a10;
       sc92n2 = NI92n2;
       sc2a3  = NI2a3 ;
       sc92n3 = NI92n3;
       sc92n4 = NI92n4;

      /End-Free
     P LoadStateWV     E
       //===================================================================
       // LoadStateNI - Loads the State Data - No Information
       //===================================================================
     P LoadStateNI     B
     D LoadStateNI     PI
     D statePtrIn                      *

     D niInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNI)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       scNoInfoMg='There is no state specific screen for ' +
        %Trim(getStateName(CMjrSt));

      /End-Free
     P LoadStateNI     E
       //===================================================================
       // LoadQuickDiary - Loads the Quick Diary Screen
       //===================================================================
     P LoadQuickDiary  B                   Export
     D LoadQuickDiary  PI
     D claimRrnIn                     9  0
     D diaryPtrIn                      *
     D diaryInfo     e ds                  ExtName(wtClmDtl:rcdClmDt6)
     D                                     based(diaryPtr)
     D diaryPtr        s               *   inz
     D claimRrn        s              9  0

     D                 ds
     D dartsKey                1     40
     D kyCo#                   1      3  0
     D kyFnd                   4      6  0
     D kyFyr                   7      9  0
     D kyCase                 10     16  0

     C     keySDCMLA     Klist
     C                   Kfld                    dartsKey
     C                   Kfld                    kkSeq#

      /Free

       diaryPtr=diaryPtrIn;
       claimRrn=claimRrnIn;

       // Clear Fields
       Clear scMedDate;
       Clear scMedRmCd;
       Clear scMedNote;
       Clear scCompDate;
       Clear scCompRmCd;
       Clear scCompNote;
       Clear scFormDate;
       Clear scFormRmCd;
       Clear scFormNote;
       Clear scSuprDate;
       Clear scSuprRmCd;
       Clear scSuprNote;
       Clear scUnitDate;
       Clear scUnitRmCd;
       Clear scUnitNote;

       Chain(n) claimRrn pcm0p;
       kyCo#=CMco#;
       kyFnd=CMfnd;
       kyFyr=CMfYr;
       kyCase=CMcase;

       // Load the Default Remark Codes along with Descriptions
       Chain keyWMCXL pcx00;
       If %Found(WMCXL);
          //Medical
          scMedRmCd=CXrads;
          kkRmCd=CXrads;
          Chain keySTCTL1 pct01;
          If %Found(STCTL1);
             scMedNote=CTdesc;
             medDataPgm=CTdePg;
          EndIf;
          //Compensation
          scCompRmCd=CXrcad;
          kkRmCd=CXrcad;
          Chain keySTCTL1 pct01;
          If %Found(STCTL1);
             scCompNote=CTdesc;
             compDataPgm=CTdePg;
          EndIf;
          //State Form
          scFormRmCd=CXrsrd;
          kkRmCd=CXrsrd;
          Chain keySTCTL1 pct01;
          If %Found(STCTL1);
             scFormNote=CTdesc;
             formDataPgm=CTdePg;
          EndIf;
          //Supervisor
          scSuprRmCd=CXrssd;
          kkRmCd=CXrssd;
          Chain keySTCTL1 pct01;
          If %Found(STCTL1);
             scSuprNote=CTdesc;
             suprDataPgm=CTdePg;
          EndIf;
          //Unit Manager
          scUnitRmCd=CXrumd;
          kkRmCd=CXrumd;
          Chain keySTCTL1 pct01;
          If %Found(STCTL1);
             scUnitNote=CTdesc;
             unitDataPgm=CTdePg;
          EndIf;
       EndIf;

       // Get Diary Date File record and load Info if entered
       Chain(n) keyWMDDL pdd00;
       If %Found(WMDDL);
          // Initialize sequence #'s
          medSeq#=DDmdSq;
          compSeq#=DDcdSq;
          formSeq#=DDbdSq;
          suprSeq#=DDsdSq;
          unitSeq#=DDudSq;
          medDate=DDmDat;
          compDate=DDcDat;
          formDate=DDbDat;
          suprDate=DDsDat;
          unitDate=DDuDat;
          //Medical
          If DDmDat<>0;
             scMedDate=dtMDYY(DDmDat);
             kkSeq#=DDmdSq;
             Chain(n) keySDCMLA scm0a;
             If %Found(SDCMLA) And C2dcDt = 0;
                scMedRmCd=C2rmCd;
                kkRmCd=C2rmCd;
                Chain keySTCTL1 pct01;
                If %Found(STCTL1);
                   medDataPgm=CTdePg;
                EndIf;
                scMedNote=C2comm;
             Else;
                medDate = 0;
                medSeq# = 0;
                scMedDate = 0;
             EndIf;
          EndIf;
          //Compensation
          If DDcDat<>0;
             scCompDate=dtMDYY(DDcDat);
             kkSeq#=DDcdSq;
             Chain(n) keySDCMLA scm0a;
             If %Found(SDCMLA) And C2dcDt = 0;
                scCompRmCd=C2rmCd;
                kkRmCd=C2rmCd;
                Chain keySTCTL1 pct01;
                If %Found(STCTL1);
                   compDataPgm=CTdePg;
                EndIf;
                scCompNote=C2comm;
             Else;
                compDate = 0;
                compSeq# = 0;
                scCompDate = 0;
             EndIf;
          EndIf;
          //State Form
          If DDbDat<>0;
             scFormDate=dtMDYY(DDbDat);
             kkSeq#=DDbdSq;
             Chain(n) keySDCMLA scm0a;
             If %Found(SDCMLA) And C2dcDt = 0;
                scFormRmCd=C2rmCd;
                kkRmCd=C2rmCd;
                Chain keySTCTL1 pct01;
                If %Found(STCTL1);
                   formDataPgm=CTdePg;
                EndIf;
                scFormNote=C2comm;
             Else;
                formDate = 0;
                formSeq# = 0;
                scFormDate = 0;
             EndIf;
          EndIf;
          //Supervisor
          If DDsDat<>0;
             scSuprDate=dtMDYY(DDsDat);
             kkSeq#=DDsdSq;
             Chain(n) keySDCMLA scm0a;
             If %Found(SDCMLA) And C2dcDt = 0;
                scSuprRmCd=C2rmCd;
                kkRmCd=C2rmCd;
                Chain keySTCTL1 pct01;
                If %Found(STCTL1);
                   suprDataPgm=CTdePg;
                EndIf;
                scSuprNote=C2comm;
             Else;
                suprDate = 0;
                suprSeq# = 0;
                scSuprDate = 0;
             EndIf;
          EndIf;
          //Unit Manager
          If DDuDat<>0;
             scUnitDate=dtMDYY(DDuDat);
             kkSeq#=DDudSq;
             Chain(n) keySDCMLA scm0a;
             If %Found(SDCMLA) And C2dcDt = 0;
                scUnitRmCd=C2rmCd;
                kkRmCd=C2rmCd;
                Chain keySTCTL1 pct01;
                If %Found(STCTL1);
                   unitDataPgm=CTdePg;
                EndIf;
                scUnitNote=C2comm;
             Else;
                unitDate = 0;
                unitSeq# = 0;
                scUnitDate = 0;
             EndIf;
          EndIf;
       Else;
          DDmdSq=0;
          DDcdSq=0;
          DDbdSq=0;
          DDsdSq=0;
          DDudSq=0;
          medSeq#=0;
          compSeq#=0;
          formSeq#=0;
          suprSeq#=0;
          unitSeq#=0;
          medDate=0;
          compDate=0;
          formDate=0;
          suprDate=0;
          unitDate=0;
       EndIf;

      /End-Free
     P LoadQuickDiary  E
       //===================================================================
       // UpdClmInfo - Updates the Claim Information
       //===================================================================
     P UpdClmInfo      B                   Export
     D UpdClmInfo      PI
     D claimRrnIn                     9  0
     D claimPtrIn                      *
     D claimFroiIn                    9  0 options(*nopass)
     D claimInfo     e ds                  ExtName(wtClmDtl:rcdClmDt0)
     D                                     based(claimPtr)
     D claimPtr        s               *   inz
     D claimRrn        s              9  0
     D FroiRrn         s              9  0
     d returnCode      s              1    inz(' ')
     D diaryKey        s             10
     D comment         s            256    Inz(' ')
     D lostTimeHold    s              1    Inz(' ')
     d charCSFrm       s              1    Inz(' ')
     D addMode         s               n   Inz(*Off)
     D inQtrDate       s              8A   Inz
     D SVjrst          s                   like(CMjrst)
       //*** Beg Add *** 09/06/22 *********************************************
       dcl-s connectFlg Ind;
       //*** End Add *** 09/06/22 *********************************************

       claimPtr=claimPtrIn;
       claimRrn=claimRrnIn;
       If %parms() >=3;
          FroiRrn=claimFroiIn;
       Endif;

       Chain claimRrn pcm0p;
       If %Found(WMCMP);

          SVjrst= CMjrst;

          addMode = *Off;
          ##pgm   = 'WTCLMADD';
          ##err   = ' ';
          SRCHKPGM(##pgm:##err);
          If ##err = *On;
             addMode = *On;
          EndIf;
          CMlsn=scLsn;
          CMfsn=scFsn;
          CMmid=scMid;
          CMacl#=scStateClm;
          CMjrc# = scStClmRel;
          CMascl=scAltStClm;
          CMpds# = ScPrclm#;
          CMecs#=scEmpClm#;
          CMeSpv=scSupName;
          CMacDt=DateTo7(scAccDate);
          CMacTm=scAccTime;
          Monitor;
             CMclas=%Dec(%SubSt(scPayClass : 1 : 5) : 5 : 0);
          On-Error 105;
             CMclas = 0;
          EndMon;
          CMext =%SubSt(scPayClass : 7 : 5);
          CMspec=%SubSt(scPayClass : 13 : 1);
          CMsic=scSicCode;
          CMiif=scInInFein;
          CM#Dep=sc#Dep;
          CMacDs=scAccDesc1;
          CMacd2=scAccDesc2;
          CMctg1=scLocation;
          CMacLn=scNarrativ;
          CMeAdr=scPremises;
          CMaccPrem=scAccPrem;
          CMcolBarg=scColBarg;
          Chain keyWMEML pem00;
             CMacNm=scPlcName;
             CMacDr=scPlcAddr;
             CMaCty=scPlcCity;
             CMaSt=scPlcSt;
             zipCode=scPlcZip;
             CMacZ1=zipCode1;
             CMacZ2=zipCode2;
          If scJurState='GA' and scPlcSt<>'GA';
             CMacCy='OUT-OF-STATE';
          Else;
             CMacCy=%Subst(scPlcCnty:3:3);
          EndIf;
          CMjrSt=scJurState;
          CMoff#=scTPA#;
          CMocDs=scOccDesc;
          CMocCd=scOccCode;
          CMdept=scDept;
          CMhiDt=dtCYMD(scHireDate);
          CmeSts=scEmpSts;
          CMdays=scDays;
          CMhrsD=scHours;
          CMshft=scShift;
          CMdtRp=dtCYMD(scRptEmp);
          CMssRl=scReleased;
          CMmCon=scMethod;
          CMacSt=scActStat;
          CMdCau=scDevCauCd;
          If CMdCau='01';
             CMdRet=scDevRstTp;
          Else;
             CMdRet=' ';
          EndIf;
          CMdPgm=scDevPgm#;
          //
          // Write Lost Time History Record if change in Lost Time
          If scLostTime<>CMltFl;
             WriteIMR(CMco#:CMfnd:CMfYr:CMcase:CMltFl:scLostTime);
          EndIf;

      *** Beg Add *** 01/14/25 *** WC2995 *****************************
          CMindClmCd = scIndmnty;
      *** End Add *** 01/14/25 *** WC2995 *****************************

          lostTimeHold=CMltFl;
          charCSFrm=CMstat;
          CMltFl=scLostTime;
          CMplFl=scPossLoss;
          GetClientID(outCltID);
          If outCltID  <> '114';
             CMctg2=scCata2;
          Else;
             CMctg2=scCata2C;
          Endif;
          CMcmps=scCmps;
          If outCltID = '025';
             CMcmps=scCampus;
          EndIf;
          If outCltID = '146';
             CMSpltWg=scSpltWg;
          EndIf;
          CMltRe=scLateRe;
          CMrcvD=dtCYMD(scRecDate);
          CMtpaD=dtCYMD(scConDate);
          CMnatr=scNature;
          CMpart=scPart;
          CMcaus=scCause;
          CMlocFt=scFtLoc;
          CMiMlc=scInjCd;
          CMncci=scNCCI;
          CMlsCv=scCovCode;
          CMlsRv=scRecType;
          CMcovT=scTypeCov;
          CMrsvT=scRsvType;
          CMeId=scOthEmpId;
          CMeitq=scIdTypeQu;
          CMgt1=scDaysLt;
          CMgt2=scDaysMe;
          CMeTyp = scEDIClmTy;
          //
          // Write Reserve Info Information
          WriteReserves(CMco#:CMfnd:CMfYr:CMcase:CMemp#:CMDiv);
          CMuser=q1User;
          CMpgm=q1Pgm;
          CMtime=TimeTo6(SystemTime());
          CMdt=DateTo7(SystemDate());
          If froiRrn <> 0 and %parms() >=3;
             Chain(n) froiRrn PFR0P;
             cicFlg = *On;
             CMaww = FRaww;
             timpd = FR20a9;
          Endif;
          If CMentd = 0 And CMltfl <> 'R';
             CMentD=DateTo7(SystemDate());
          EndIf;
         CMwwtc = scWWTC;
         CMwds = scWDSSu + scWDSM + scWDST + scWDSW + scWDSR + scWDSF +
                 scWDSSa;
         CMesid = scESID;
          CMiDcAcS = scAsLc;
          Monitor;
          CMLossCd = %Dec(scLossCd:2:0);
          On-Error;
             CMLossCd = 0;
          Endmon;
          Update pcm0p;


       // Auto Diary
       If SCncci<>*blanks;
          #ADKY = 'CLMNCCI';
          SetLl keyWMADL pad01;
          ReadE keyWMADL pad01;
          If Not %Eof(WMADL);
                If (SCncci=ADncc1 Or SCncci=ADncc2) Or (SCncci=ADncc3);
             // Write Diary on NCCI <> blanks
                diaryKey='CLMNCCI ';
                WrtDrtRemarks(CMco#:CMfnd:CMfYr:CMcase:diaryKey:comment);
                EndIf;
          EndIf;
       EndIf;


          // Auto Letter
          If Not addMode And (CMltFl<>lostTimeHold Or CMstat<>charCSFrm);
             GetClientID(outCltID);
                   charCSFrm=CMstat;
                   intEfdt = DateTo7(SystemDate());
                   Setll KeyWMCHL5 pch05;
                   Reade KeyWMCHL5 pch05;
                   Dow Not %Eof(wmchl5);
                      If CHocst <> CMstat;
                         charCSFrm=CHOCST;
                         Leave;
                      Else;
                         Reade KeyWMCHL# pch05;
                      EndIf;
                   EndDo;
                AutoLetters(claimRrn:returnCode:lostTimeHold:charCSFrm);
             // Write Diary on Lost Time Change
             If lostTimeHold<>' ';
                diaryKey='CLMCHG02';
                WrtDrtRemarks(CMco#:CMfnd:CMfYr:CMcase:diaryKey:comment);
             EndIf;
          EndIf;
       EndIf;

       // Update Claim Demographics
       Chain CMss# pcd00;

       If CDlsn<>scLsn Or CDfsn<>scFsn Or CDmid<>scMid Or
          CDadr1<>scClmAdr1 Or CDadr2<>scClmAdr2 Or CDcty<>scClmCty Or
          CDst<>scClmSt Or CDzip<>scClmZip;

          // First make change to Payment Requests on this claim.
          UpdReqAddrs(claimPtr);

          // Make change to other claims if the name has changed.
          UpdClmName(claimPtr:claimRrn);

          // Write Auto Diary
             diaryKey='CLMCHG01';
             WrtDrtRemarks(CMco#:CMfnd:CMfYr:CMcase:diaryKey:
               comment);
          WrtAdrHist('B');
       EndIf;

       CDss#=CMss#;
       CDlsn=scLsn;
       CDfsn=scFsn;
       CDmid=scMid;
       CDadr1=scClmAdr1;
       CDadr2=scClmAdr2;
       CDadr3=scProvince;
       CDctry=scCountry;
       CDcty=scClmCty;
       CDdtob=dtCYMD(scBirthDat);
       CDsex=scGender;
       //*** Beg Add *** 02/14/22 ***************************************
       CDedul = scEdul;
       //*** End Add *** 02/14/22 ***************************************
       CDcnty=%Subst(scClmCnty:3:3);
       CDmSts=scMarStat;
       CDst=scClmSt;
       CDzip=scClmZip;
       phoneNumber=scClmPhn#;
       CDarea=areaCode;
       CDphn#=phone#;
       phoneNumber=scClmWrk#;
       CDwara=areaCode;
       CDwph#=phone#;
       phoneNumber=scClmCel#;
       // Check for cell # changes
       isProcRelayReq = *off;
       If isSmsVisbl = *on and scDisSms <> 'Y' and
        (CDcara <> areaCode or CDcphn <> phone#) and
        phone# <> *zeros;
          isProcRelayReq = *on;
       Endif;
       If isSmsDsbld <>  *on;
          CDDisSms = scDisSms;
       Endif;
       CDcara=areaCode;
       CDcphn=phone#;
       CDeMad=scClmEmad;
       CDuser=q1User;
       CDpgm=q1Pgm;
       CDtime=TimeTo6(SystemTime());
       CDdt=DateTo7(SystemDate());
       CDeMad=scClmEmad;
       CDheigt = scFeet + '-' + scInch;
       CDwgt = scWeight;

       If %Found(WMCDL);
          Update pcd00;
          If ahWritten;
             WrtAdrHist('A');
          EndIf;
       Else;
          Write  pcd00;
          WrtAdrHist('B');
       EndIf;
       // Send welcome text message
       If isProcRelayReq = *on;
          ProcRelayReq('CS':dbrrn:'OW');
       Endif;

       //* Write State NOI File
       //*** Beg Add *** 09/06/22 *********************************************
       connectFlg = isModEnabled('Portal Module');
       //*** End Add *** 09/06/22 *********************************************
       //*** Beg Chg *** 09/06/22 *********************************************
       //***If cicFlg = *On and %parms()>=3;
       If (cicFlg = *On and FRiPgm = 'CICENTER' and %parms()>=3)
        or (connectFlg = *on and FRiPgm = 'WIFROI' and %parms()>=3);
       //*** End Chg *** 09/06/22 *********************************************
          NIco# = CMco#;
          NIfnd = CMfnd;
          NIfyr = CMFYR;
          NIcase= CMcase;
          NI1a1 = FR1a1;
          NI1a2 = FR1a2;
          NI60a1= FR60a1;
          NI60a2= FR60a2;
          NI60a3= FR60a3;
          NI60a4= FR60a4;
          NI60a5= FR60a5;
          NI60a6= FR60a6;
          NI80a1= FR80a1;
          NI80a2= FR80a2;
          NI80a3= FR80a3;
          If FRjrst <> 'MA' and FRjrst <> 'NY';
             NI10a2 = FRsthr;
          Endif;

       //* California Specific Fields
          NItim2 = FRtim2;
          NI40a1 = FR40a1;
          NI40a2 = FR40a2;
          NI30a1 = FR30a1;
          NI25a1 = FR25a1;
          NI2a1  = FR2a1;
          NI9n2  = FR9n2;
          NI30a2 = FR30a2;
          NI20a1 = FR20a1;
          NI1a3  = FR1a3;
          NIDt1  = FRDt1;
          NI40a3 = FR40a3;
          NI30A3 = FR30a3;
          NI40a4 = FR40a4;
          NI1a4  = FR1a4;
          NI80a4 = FR80a4;
          NI80a5 = FR80a5;
          NI40a5 = FR40a5;
          NI1a5  = FR1a5;
          NI1a6  = FR1a6;
          NI3n1  = FR3n1;
          NI9n3  = FR9n3;
          NI40a6 = FR40a6;
          NIprnm = FRprnm;
          //***** Beg Chg *** 10/11/22 ***********************************
          //***If FRprep <> ' ' or FRprep <> *zeros;
          If FRprep <> ' ' and FRprep <> *zeros;
          //***** End Chg *** 10/11/22 ***********************************
             NIprep = dtCYMD(%Int(FRprep));
          Endif;
          NIcont = FRcont;
          If FRcar <> ' ';
             NIcar = %Int(FRcar);
          Endif;
          If FRcphn <> ' ';
             NIcphn = %Int(FRcphn);
          Endif;
      /End-Free
     C                   MOVEA     FRPRPH        PRPH
     C     1             DO        10            X
     C     PRPH(X)       IFEQ      'N'
     C     PRPH(X)       OREQ      'U'
     C     PRPH(X)       OREQ      'L'
     C                   MOVE      ' '           PRPH(X)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     PRPH          FRPRPH
     C                   MOVEL(P)  FRPRPH        NIPRPH
      /Free

       //* Add LOUSIANA Specific Fields - 133WBQ and New York - 129WIX
          NI1a7 = FR1a7;
          NI1a8 = FR1a8;
          NI1a9 = FR1a9;
          NI1a10= FR1a10;
          NI10a1= FR10a1;
          If FRjrst = 'MA' or FRjrst = 'NY';
             NI10a2 = FR10a2;
          Endif;
          NI10a3 = FR10a3;
          NI10a4 = FR10a4;
          NI10a5 = FR10a5;
          NI20a2 = FR20a2;
          NI20a3 = FR20a3;
          NI20a4 = FR20a4;
          NI20a5 = FR20a5;
          NI20a6 = FR20a6;
          NI20a7 = FR20a7;
          NI20a8 = FR20a8;
          NI20a9 = FR20a9;
          NI20A0 = FR20a0;
          NI30a4 = FR30a4;
          NI30a5 = FR30a5;
          NI40a7 = FR40a7;
          NI40a8 = FR40a8;
          NI40a9 = FR40a9;
          NI40a0 = FR40a0;
          NI60a7 = FR60a7;
          NI60a8 = FR60a8;
          NI60a9 = FR60a9;
          NI60a0 = FR60a0;
          NI80a6 = FR80a6;
          NI80a7 = FR80a7;
          NI80a8 = FR80a8;
          NI80a9 = FR80a9;
          NI80a0 = FR80a0;
          NI80aa = FR80aa;
          NI3n2  = FR3n2;
          NI3n3  = FR3n3;
          NI3n4  = FR3n4;
          NI3n5  = FR3n5;
          NI3n6  = FR3n6;
          NI3n7  = FR3n7;
          NI3n8  = FR3n8;
          NI3n9  = FR3n9;
          NI3n10 = FR3n10;
          NI5n1  = FR5n1;
          NI5n2  = FR5n2;
          NI5n3  = FR5n3;
          NI5n4  = FR5n4;
          NI5n5  = FR5n5;
          NI9n1  = FR9n1;
          NI9n4  = FR9n4;
          NI9n5  = FR9n5;
          NI1A8  = FR1A8;
          NI1A9  = FR1A9 ;
          NI60A7 = FR60A7;
          NI80A1 = FR80A1;
          NI80A2 = FR80A2;
          NI1A3  = FR1A3 ;
          NI1A4  = FR1A4 ;
          NI30A5  = FR30A5;
          NI1A5  = FR1A5;
          NI1A6  = FR1A6;
          NI1A7  = FR1A7;
          NI2A2  = FR2A2;
          NI2A3  = FR2A3;
          NI5N1  = FR5N1;
          NI92N1 = FR92N1;
          NI92N2 = FR92N2;
          NI92N3 = FR92N3;
          NI92N4 = FR92N4;
          NI92N5 = FR92N5;
          NI92N6 = FR92N6;
          NI92N7 = FR92N7;
          NI92N8 = FR92N8;
          NI92N9 = FR92N9;
          NI92N0 = FR92N0;
          If FRdt2 <> ' ';
             NIdt2  = dtCYMD(%Int(FRdt2));
          Endif;
          If FRdt3 <> ' ';
             NIdt3  = dtCYMD(%Int(FRdt3));
          Endif;
          If FRdt4 <> ' ';
             NIdt4  = dtCYMD(%Int(FRdt4));
          Endif;
          If FRdt5 <> ' ';
             NIdt5  = dtCYMD(%Int(FRdt5));
          Endif;
          NI2a2  = FR2a2;
          NI2a3  = FR2a3;
          NI2a4  = FR2a4;
          NI2a5  = FR2a5;
          NI25a2 = FR25a2;
          NI25a3 = FR25a3;
          NI25a4 = FR25a4;
          NI25a5 = FR25a5;
          NItim1 = FRtim1;
          NItim3 = FRtim3;
          NItim4 = FRtim4;
          NItim5 = FRtim5;
          NIstat = 'A';
          //*** Begin Chg *** 10/26/21 *************************************
          //*** NIst = FRjrst;
          NIst = GetFroiState(CMco#:FRjrSt);
          //*** End   Chg *** 10/26/21 *************************************
          NIdt = DateTo7(SystemDate());
          NIidt= DateTo7(SystemDate());
          NIitm= TimeTo6(SystemTime());
          NItime=TimeTo6(SystemTime());
          NIuser=q1user;
          NIiusr=q1user;
          NIpgm =q1pgm;
          Write PNI0P;
          //*** Beg Add *** 09/06/22 *******************************************
          // Write A1 record for Connect or CICenter
          If NIst <> 'A1';
             If FRiPgm = 'WIFROI';
                NI10A2 = FR10A2;
             Endif;
             NIst = 'A1';
             Write PNI0P;
          Endif;


       //*  WRITE TREATING PHYSICIAN FILE
          If FRphyn <> ' ';
             TPco# = CMco#;
             TPfnd = CMfnd;
             TPfyr = CMfyr;
             TPcase= CMcase;
             TPseq#= 1;
             TPstat= 'A';
      /End-Free
     C                   MOVEA     FRPHYN        PHY
     C     1             DO        40            X                 3 0
     C     PHY(X)        IFEQ      ','
     C                   MOVE      ' '           PHY(X)
     C                   ENDIF
     C                   ENDDO
     C                   MOVEA     PHY           TPPHYN
      /Free
             TPdt  = DateTo7(SystemDate());
             TPidt = DateTo7(SystemDate());
             TPtime= TimeTo6(SystemTime());
             TPuser= q1user;
             TPiusr= q1user;
             TPpgm = q1pgm;
             TPipgm= q1pgm;
             TPphya= FRphya;
             TPphyc= FRphyc;
             TPphys= FRphys;
             TPphyz= FRphyz;
             TPphyr= FRphyr;
             TPphyp= FRphyp;
             Write PTP0P;
          Endif;
       //* UPDATE CICENTER INTERNET NOI FILE
          Chain FroiRrn PFR0P;
          If %Found(WHFRP);
             FRstat = 'T';
             FRfyr  = CMfyr;
             FRcase = CMcase;
             FRuser = q1user;
             FRpgm  = q1pgm;
             FRdate = %Char(DateToYYMD(SystemDate()));
             FRtime = TimeTo6(SystemTime());
             Update PFR0P;
          Endif;
       Endif;
       If outCltID = '063';
          open wmcmpx;
          Chain keyWMCMPX pcm01x;
          If %Found(wmcmpx);
             XCMinlc = scLocInStr;
             XCMpos  = scPosition;
             If scPoints <> ' ';
                XCMpnts = %int(scPoints);
             Else;
                XCMpnts = 0;
             EndIf;
             XCMuser = q1user;
             XCMpgm  = q1pgm;
             XCMdt   = DateTo7(SystemDate());
             XCMtime = TimeTo6(SystemTime());
             Update pcm01x;
          Else;
             XCMco#  = CMco#;
             XCMfnd  = CMfnd;
             XCMfyr  = CMfyr;
             XCMcase = CMcase;
             XCMinlc = scLocInStr;
             XCMpos  = scPosition;
             If scPoints <> ' ';
                XCMpnts = %int(scPoints);
             Else;
                XCMpnts = 0;
             EndIf;
             XCMuser = q1user;
             XCMpgm  = q1pgm;
             XCMdt   = DateTo7(SystemDate());
             XCMtime = TimeTo6(SystemTime());
             XCMiusr = q1user;
             XCMipgm = q1pgm;
             XCMidt  = DateTo7(SystemDate());
             XCMitim = TimeTo6(SystemTime());
             Write pcm01x;
          EndIf;
          close wmcmpx;
       EndIf;

       //** Trigger IDC Quarterly record if state changed from IDC to NON-IDC

       If SVjrst <> CMjrst;
          If isModEnabled('IDC');
             inQtrDate = %Char(%date():*USA0);

             Setll (CMco#:CMfnd:CMfyr:CMcase) WMIDCTRL;
             Setll (CMco#:CMfnd:CMfyr:CMcase) WMIDCQTL;

             If %Equal(WMIDCTRL) or %Equal(WMIDCQTL);  //Previously reported
                If isIdcState(CMco#:SVjrst) = *On and
                   isIdcState(CMco#:CMjrst) = *Off;
                   WriteQtLog(ClaimRrn:inQtrDate);
                Endif;
             Endif;
          Endif;
       Endif;

      /End-Free
     P UpdClmInfo      E
       //===================================================================
       // UpdMgmtInfo - Updates the Management Information
       //===================================================================
     P UpdMgmtInfo     B                   Export
     D UpdMgmtInfo     PI
     D claimRrnIn                     9  0
     D mgmtPtrIn                       *
     D mgmtInfo      e ds                  ExtName(wtClmDtl:rcdClmDt1)
     D                                     based(mgmtPtr)
     D mgmtPtr         s               *   inz
     D claimRrn        s              9  0
     D beforeFlags     s             10
     D afterFlags      s             10
     D diaryKey        s             10    Inz('CLMCHG03')
     D comment         s            256    Inz(' ')
      *** Beg Add ***** 04/18/22 ********************************
     D intCount        s              5  0 inz
     D Cat13Claim      s              1a   Inz('N')
      *** End Add ***** 04/18/22 ********************************
      /Free

       mgmtPtr=mgmtPtrIn;
       claimRrn=claimRrnIn;

       Chain claimRrn pcm0p;
       If %Found(WMCMP);


          If scClmAdj<>CMclEx and CMclEx <> *blanks;
             WriteTransfer('C':CMclEx:scClmAdj);
          Endif;

          If scSuperv<>CMsupr and CMsupr <> *blanks;
             WriteTransfer('S':CMsupr:scSuperv);
          Endif;

          If scAccManag<>CMaMan and CMaMan <> *blanks;
             WriteTransfer('A':CMaMan:scAccManag);
          Endif;

          If scClNurse<>CMuMan and CMuMan <> *blanks;
             WriteTransfer('U':CMuMan:scClNurse);
          Endif;

          If scCsNurse<>CMnrse and CMnrse <> *blanks;
             WriteTransfer('N':CMnrse:scCsNurse);
          Endif;

          If scOutAdj<>CMfeAd and CMfeAd <> *blanks;
             WriteTransfer('F':CMfeAd:scOutAdj);
          Endif;

          If scFundAdj<>CMsdfA and CMsdfA <> *blanks;
             WriteTransfer('D':CMsdfA:scFundAdj);
          Endif;

          If scReinAdj<>CMeSup and CMeSup <> *blanks;
             WriteTransfer('E':CMeSup:scReinAdj);
          Endif;

          // Check and see if Flags have changed
          beforeFlags=CMdny+CMcvFl+CMsubr+CMexcs+CMacqr+CMosha+CMsdf+CMuslh+
                      CMdth;
          afterFlags=scDenied+scContro+scSubro+scExcess+scAcquired+
                     scOsha+scSpecDis+scUslh+CMdth;
          If  outCltID  = '045';
             afterFlags=scDenied+scContro+scSubro+scExcess+scAcquired+
                     scOsha+scLSpecDis+scUslh+CMdth;
          Endif;

          If beforeFlags<>afterFlags;
             WrtDrtRemarks(CMco#:CMfnd:CMfYr:CMcase:diaryKey:
                        comment:AfterFlags);
          EndIf;
          SvCmsSub = CMmbs;

          CMclEx=scClmAdj;
          CMsupr=scSuperv;
          CMaMan=scAccManag;
          CMuMan=scClNurse;
          CMnrse=scCsNurse;
          CMfeAd=scOutAdj;
          CMsdfA=scFundAdj;
          CMeSup=scReinAdj;
          CMseaE=scSeasEmp;
          CMliaF=scEmpLiab;
          CMdyEf=dtCYMD(scFdEffDt);
          CMdRes=dtCYMD(scFdResDt);
          CMdny1=scFdReas1;
          CMdny2=scFdReas2;
          CMdny3=scFdReas3;
          CMdny4=scFdReas4;
          CMdny5=scFdReas5;
          CMpDny=scPdReas;
          CMdnyN=scDenReas1 + scDenReas2 + scDenReas3;
          CMpded=dtCYMD(scParDenDt);
          CMreCl=scReinClm#;
          //*** Beg Chg ***** 04/18/22 ************************
          //CMcata=scCatas#;
          If scCatas# <> *Blank;
             Monitor;
                CMcata = %Int(scCatas#);
             On-Error;
                CMcata = 0;
             EndMon;
          Else;
             CMcata = 0;
          Endif;
          //Changes for Category 12
          If CMnatr = '83' and CMcaus = '83' and CMacdt > 1191201;
             CMcata = 12;
          Else;
             If CMcata = 12;
                CMcata = 0;
             Endif;
          Endif;
          //*** End Chg ***** 04/18/22 ************************
          CMdny=scDenied;
          CMsubr=scSubro;
          CMosha=scOsha;
          //*** Beg Chg *** 07/14/21********************************************
          //***CMuslh=scUslh;
          // Get USL&H flag based on Coverage Code in Generic Table file
          kkTbId=CMjrSt + 'LC';
          kkCo# = CMco#;
          Chain keyWTGIL pgi00;
          If Not %Found(WTGIL);
             kkTbId='  LC';
          EndIf;
          Chain (kkCo#:kkTbId:CMlsCv) pgt01;
          If %Found(wtgtl) and GTstat <> 'I' and %Subst(GTcf01:1:2) = '02';
             CMusLh = 'Y';
          Else;
             CMusLh = 'N';
          Endif;
          //*** End Chg *** 07/14/21********************************************
          CMmbs=scCmsSub;
          CMrstn=scrstn;
          CM14x6=sc14x6;
          CMlstx=sclstx;
          CMmisc=scmisc;
          // Load indicators
          CMeSe = scEsE;
          CMiAq = scIaQ;
          CMstuAgr = scStuAgr;
          CMpartDn = scPartDn;
          CM120Rvw = sc120Rvw;
          CMlfpn=scLifePen;
          CMsw=scSw132a;
          CMdly=scDelayed;
          CMdwc1=scDwc1;
          CMdcsn=dtCYMD(scDecDt);
       If  outCltID  = '045';
           CMsdf=scLSpecDis;
       Else;
           CMsdf=scSpecDis;
       Endif;
          CMcvFl=scContro;
          CMexcs=scExcess;
          CMacqr=scAcquired;
          CMalkd=dtCYMD(scAcInTrDt);
          CMawli=  scAWL;
          CMuser=q1User;
          CMpgm=q1Pgm;
          CMtime=TimeTo6(SystemTime());
          CMdt=DateTo7(SystemDate());
       If outCltID = '025';
          CMlben=scLifeBen;
       EndIf;
          CMrirp=scReinRpt;
          If CMentd = 0 And CMltfl <> 'R';
             CMentD=DateTo7(SystemDate());
          EndIf;
          Update pcm0p;
          //*** Beg Add ***** 04/18/22 *************************
          //If this same claim is changed to a CAT13 claim,
          // the claim should be removed from the multiple Claim
          Reset intCount;
          Cat13Claim = 'N';
          Exec Sql
           Select Count(*)
           Into :intCount
           From wtelecat
           Where CAst = :svJurStAdd and
             :svAccDate >= CAstdt and :svAccDate <= CAendt;
          If intCount > 0;
             Cat13Claim = 'Y';
          Endif;
          If scCatas# <> *Blank;
             If svCata <> %Int(scCatas#) and Cat13Claim = 'Y';
                Exec sql
                 delete from WAMCP
                 where MCco#  = :CMco#  and MCfnd  = :CMfnd
                  and  MCfyr  = :CMfyr  and MCcase = :CMcase
                  and  MCemp# = :CMemp# and MCdiv  = :CMdiv;
             Endif;
          Endif;
          //*** End Add ***** 04/18/22 *************************
       EndIf;

       If  ScCmsSub <> SvCmsSub;
          If ScCmsSub = ' ';
             If (CXrcsc <> ' ');
                If (CXrcdu <> ' ' or CXrcsu <> ' ');
                   q#key=%EditC(CMco#:'X') + %EditC(CMfnd:'X') +
                    %EditC(CMfYr:'X') + %EditC(CMcase:'X');
                   q#cDat = %EditC(DateTo8(systemDate()):'X');
                   If (CXrcdu <> ' ');
                      Select;
                      When CXrcdu = '01';
                         q#user = GetDiaryUr(CMclex);
                      When CXrcdu = '02';
                         q#user = GetDiaryUr(CMsupr);
                      When CXrcdu = '03';
                         q#user =  GetDiaryUr(CMaman);
                      When CXrcdu = '04';
                         q#user =  GetDiaryUr(CMaman);
                      when CXrcdu = '05';
                         q#user =  GetDiaryUr(CMnrse);
                      When CXrcdu = '06';
                         q#user =  GetDiaryUr(CMfead);
                      When CXrcdu = '07';
                         q#user =  GetDiaryUr(CMsdfa);
                      When CXrcdu = '08';
                         q#user = GetDiaryUr(CMesup);
                      Endsl;
                   Else;
                      If (CXrcsu <> ' ');
                         Chain CXrcsu pup01;
                         If %found;
                            q#user =  Upusr;
                         Endif;
                      Endif;
                   Endif;

                   Chain CXrcsc pct01;
                   If %found;
                      q#comm = CTdesc;
                      q#dcId =  CTdepg;
                      q#DIFL = CTfl01;
                      If (q#DCID <> ' ');
                         q#fold = q#Key;
                         q#Fl01  = 'Y';
                      Else;
                         q#Fold = *blanks;
                         q#Fl01 = *blank;
                      Endif;
                      If (Ctdprg <> 0);
                         q#Fl02 = 'Y';
                      Else;
                         q#Fl02 = *blank;
                      Endif;
                   Endif;

                   q#Rmcd = CXrcsc;
                   q#Pnam = 'W0070R';
                   q#Secl = *blanks;
                   q#Pgm  = q1Pgm;
                   q#Ipgm = q1Pgm;
                   q#Seq# = '00000';
                   q#Sbyl = 'N';
                   q#Com2 = *blanks;
                   q#Apid = *blanks;
                   q#Idoc = *blanks;
                   q#Ique = *blanks;
                   q#Imgf = *blanks;
                   q#Ipth = *blanks;
                   q#Pidx = *blanks;
                   q#Qpty = *blanks;
                   q#Dctp = *blanks;
                   q#Csid = *blanks;
                   q#code = *blanks;

                   WriteDarts(q#key:q#comm:q#cDat:q#dcId:q#pNam:q#secl:q#user
                      :q#pgm:q#fold:q#diFl:q#iPgm:q#fl01:q#fl02:q#rmCd:q#seq#:
                      q#sbyl:q#com2:q#apId:q#iDoc:q#iQue:q#imgF:q#iPth:q#pIdx:
                      q#qPty:q#dcTp:q#csId:q#code);
                Endif;
             Endif;
          Endif;
       Endif;

       // Update Subrogration Information
       Chain keyWMSBL psb00;

       SBrNam=scRspName;
       SBrAd1=scRspAdr1;
       SBrAd2=scRspAdr2;
       SBrCty=scRspCity;
       SBrSt=scRspSt;
       SBrZip=scRspZip;
       SBrPhn=scRspPhn#;
       SBrMad=scRspEmAd;
       SBslDt=dtCYMD(scStLimDt);
       SBiNam=scInsName;
       SBiAd1=scInsAdr1;
       SBiAd2=scInsAdr2;
       SBiCty=scInsCty;
       SBiSt=scInsSt;
       SBiZip=scInsZip;
       SBiPhn=scInsPhn#;
       SBiMad=scInsEmAd;
       SBiPol=scInsPol#;
       SBiAdj=scInsAdj;
       SBuser=q1User;
       SBpgm=q1Pgm;
       SBtime=TimeTo6(SystemTime());
       SBdt=DateTo7(SystemDate());

       If %Found(WMSBL);
          Update psb00;
       Else;
          SBco#=CMco#;
          SBfnd=CMfnd;
          SBfYr=CMfYr;
          SBcase=CMcase;
          SBiUsr=q1User;
          SBiPgm=q1Pgm;
          SBiDt=DateTo7(SystemDate());
          Write psb00;
       EndIf;

       // Update Claim Demographics
       Chain CMss# pcd00;
       If %Found(WMCDL);
          CDmdBn=scMedBen;
          CDuser=q1User;
          CDpgm=q1Pgm;
          CDtime=TimeTo6(SystemTime());
          CDdt=DateTo7(SystemDate());
          Update pcd00;
       EndIf;

       If outCltID  = '133';
          Chain keyWMCHL# pdi01;
       // Set maintenance fields
          DIuser=q1User;
          DIpgm=q1Pgm;
          DItime=TimeTo6(SystemTime());
          DIdt=DateTo7(SystemDate());

          If %Found(WDDIL1);
             If CMltFl = 'R';
                DIstat = 'O';
                DIrplv = 0;
             Else;
                DIstat=scDCIsts;
                DIrplv=scValLvl;
             EndIf;
             Update pdi01;
          else;
             If scDCIsts <> ' ' And CMltFl <> 'R';
                dico#=CMco#;
                difnd=CMfnd;
                difYr=CMfYr;
                dicase=CMcase;
                didtrp=CMdtrp;
                dist=CMjrst;
                // Set DCI flags to blanks - these will be set while sampling
                dirnbr = 0;
                dirplv = scValLvl;
                distat = scDCIsts;
                diindc = 'Y';
                dipdci = ' ';
                diopni = ' ';
                dicpnt = ' ';
                dicnpt = ' ';
                disamp = ' ';
                // Set initial fields
                DIiusr=q1User;
                DIipgm=q1Pgm;
                DIitm=TimeTo6(SystemTime());
                DIidt=DateTo7(SystemDate());
                write pdi01;
             EndIf;
          EndIf;
       EndIf;

       If svClmAdj <> CMclex;
          q#key=%EditC(CMco#:'X') + %EditC(CMfnd:'X') +
          %EditC(CMfYr:'X') + %EditC(CMcase:'X');
          q#csid=*blanks;
          q#cDat=%EditC(DateTo8(systemDate()):'X');
          q#dcId = *Blanks;
          q#pnam = 'W0070R';
          q#secl = *Blanks;
          q#sbyl = 'N';
          q#user=q1user;
          q#pgm=q1pgm;
          q#fold=*blanks;
          q#apid=*blanks;
          q#idoc=*blanks;
          q#ique=*blanks;
          q#ique=*blanks;
          q#imgf=*blanks;
          q#ipth=*blanks;
          q#pidx=*blanks;
          q#qpty=*blanks;
          q#dctp=*blanks;
          q#iPgm=Q1pgm;
          qcCom1 = *blanks;
          qcDrt1 = 'CLAIMANT NAME & CITY:';
          qcCNam = *blanks;
          ##last = CDlsn;
          ##frst = CDfsn;
          ##term = ' ';
          srname(##last:##frst:##term);
          qcCNam = ##last;
          qcCty = CDcty;
          qcDrt2 = 'ACCIDENT DATE:';
          qcSls1 = '/';
          qcSls2 = '/';
          CYMD = CMacdt;
          qcYr = CY1;
          qcMo = MM;
          qcDy = DD;
          qcDrt3 = 'USER NAME:';
          qcUser = getUsrName(q1user);

          If svClmAdj <> *blanks and CXrffa <> *blanks;
             Chain svClmAdj pup01;
             If %found;
                qcUsnm = UPusnm;
             Endif;
             qcFead = svClmAdj;
             q#rmcd = CXrffa;
             q#comm = qcCom1 + qcCom2;
             q#com2 = *blanks;
             Chain q#rmcd pct01;
             If %found;
                q#difl = CTfl01;
                q#dcid = CTdepg;
                If q#dcid <> *blanks;
                   q#fold = q#key;
                   q#fl01 = 'Y';
                Else;
                   q#fl01 = *blanks;
                Endif;
                If CTdprg > 0;
                   q#fl02 = 'Y';
                Else;
                   q#fl02 = *blanks;
                Endif;
             Endif;
             q#seq#= *blanks;
             q#code = *blanks;
             WriteDarts(q#key:q#comm:q#cDat:q#dcId:q#pNam:q#secl:q#user
              :q#pgm:q#fold:q#diFl:q#iPgm:q#fl01:q#fl02:q#rmCd:q#seq#:
              q#sbyl:q#com2:q#apId:q#iDoc:q#iQue:q#imgF:q#iPth:q#pIdx:
              q#qPty:q#dcTp:q#csId:q#code);
          EndIf;

          If CMclex <> *blanks and CXrtfa <> *blanks;
             qcfead = *blanks;
             qcUsnm = *blanks;
             Chain CMclex pup01;
             If %found;
                qcUsnm = UPusnm;
             Endif;
             qcFead = CMclex;
             q#rmcd = CXrtfa;
             q#comm = qcCom1 + qcCom2;
             q#com2 = *blanks;
             Chain q#rmcd pct01;
             If %found;
                q#difl = CTfl01;
                q#dcid = CTdepg;
                If q#dcid <> *blanks;
                   q#fold = q#key;
                   q#fl01 = 'Y';
                Else;
                   q#fl01 = *blanks;
                Endif;
                If CTdprg > 0;
                   q#fl02 = 'Y';
                Else;
                   q#fl02 = *blanks;
                Endif;
             Endif;
             q#seq#= *blanks;
             WriteDarts(q#key:q#comm:q#cDat:q#dcId:q#pNam:q#secl:q#user
              :q#pgm:q#fold:q#diFl:q#iPgm:q#fl01:q#fl02:q#rmCd:q#seq#:
              q#sbyl:q#com2:q#apId:q#iDoc:q#iQue:q#imgF:q#iPth:q#pIdx:
              q#qPty:q#dcTp:q#csId:q#code);
          EndIf;
       EndIf;

      /End-Free
     P UpdMgmtInfo     E
       //===================================================================
       // UpdMedInfo - Updates the Medical Information
       //===================================================================
     P UpdMedInfo      B                   Export
     D UpdMedInfo      PI
     D claimRrnIn                     9  0
     D medicalPtrIn                    *
     D medicalInfo   e ds                  ExtName(wtClmDtl:rcdClmDt2)
     D                                     based(medicalPtr)
     D medicalPtr      s               *   inz
     D claimRrn        s              9  0
     D beforeFlags     s             10
     D afterFlags      s             10
     D diaryKey        s             10    Inz('CLMCHG03')
     D comment         s            256    Inz(' ')
      /Free

       GetClientID(outCltID);

       medicalPtr=medicalPtrIn;
       claimRrn=claimRrnIn;

       Chain claimRrn pcm0p;
       If %Found(WMCMP);

          If scHospFein=*Blanks;
             CMhosF=*Zeros;
             CMhosM=*Zeros;
          Else;
             CMhosF=%Dec(%Subst(scHospFein:1:9):9:0);
             CMhosM=%Dec(%Subst(scHospFein:10:3):3:0);
          EndIf;

          If CMhosF<>0;
             kkFein=CMhosF;
             kkMod=CMhosM;
             Chain keyWMPDL ppd00;
             If %Found(wmpdl);
                If PDdba1='Y';
                   CMhosN=PDadr1;
                   CMhosA=PDadr2;
                Else;
                   If PDfsn<>*Blanks;
                      CMhosN=%Trim(PDfsN) + ' ' + %Trim(PDname);
                   Else;
                      CMhosN=%Trim(PDname);
                   EndIf;
                   CMhosA=PDadr1;
                EndIf;
                CMhosC=PDcty;
                CMhosS=PDst;
                CMhosZ=pdZip;
             EndIf;
          Else;
             CMhosN=scHosName;
             CMhosA=scHosAdr;
             CMhosC=scHosCty;
             CMhosS=scHosSt;
             CMhosZ=scHosZip;
          EndIf;
          phoneNumber=scHosPhn#;
          CMhosR=areaCode;
          CMhosP=phone#;

          CMiTrt=scInitTrt;
          CMsgDt=dtCYMD(scSurgDate);
          CMdtRe=dtCYMD(scRehabDt);
          CMaMmi=dtCYMD(scAntMmi);
          CMoDis=scOcc;
          CMyrLx=scYrLstExp;
          CMimp=scImpaired;
          CMiPct=scPctImp;
          CMmcTp=scMcoCode;
          CMinjS = scInjSvCod;
          CMmcNm=scMcoName;
          CMmcof=%editc(scMcoFein:'X');
          CMcgPh=scChgPhy;
          CMwcPh=scReqPhy;
          CMwdPh=scPhyDny;
          CMaInj=scCmsCause;
          CMaIn1=scCmsCaus2;
          CMcmDi=dtCYMD(scCmsDate);
          CMmmi=dtCYMD(scFinMmi);
          CMpImp=scMinPay;
          CMauRl=scMrAuth;
          CMhCap=scHanClm;

          // Check and see if Flags have changed
          beforeFlags=CMdny+CMcvFl+CMsubr+CMexcs+CMacqr+CMosha+CMsdf+CMuslh+
                      CMdth;
          afterFlags=CMdny+CMcvFl+CMsubr+CMexcs+CMacqr+CMosha+CMsdf+CMuslh+
                      scDthClm;

          If beforeFlags<>afterFlags;
             WrtDrtRemarks(CMco#:CMfnd:CMfYr:CMcase:diaryKey:
                        comment:beforeFlags);
          EndIf;

          If outCltId = '114';
             CMrxel = scRXEL;
          EndIf;
          If outCltId = '090';
             CMrxel = scPBME;
          EndIf;
          CMdth=scDthClm;
          CMdod=dtCYMD(scDeathDt);
            if scDthRes = *blanks and CMdod > *Zero;
              CMIdth = 'N';
            else;
              CMidth = scDthRes;
            Endif;
          CMorm=scOrmInd;
          CMormD=dtCYMD(scOrmDate);
          CMprEx=scPreExist;
          CMuser=q1User;
          CMpgm=q1Pgm;
          CMtime=TimeTo6(SystemTime());
          CMdt=DateTo7(SystemDate());
          If CMentd = 0 And CMltfl <> 'R';
             CMentD=DateTo7(SystemDate());
          EndIf;
       If outCltID = '129';
          If (svPctAppr <> scPctAppr Or svApprDt <> scApprDt) And
             (scPctAppr <> *Zero Or scPctAppr <> *Zero)       And
             CXaprm <> *Blanks;

              q#key=%EditC(CMco#:'X') + %EditC(CMfnd:'X') +
                    %EditC(CMfYr:'X') + %EditC(CMcase:'X');
              q#comm = 'Apportionment % Or Apportionment Effective Date' +
                       ' has been entered.';
              q#cDat = %EditC(DateTo8(systemDate()):'X');
              q#dcId = *blanks;
              q#pnam = 'W0070R';
              q#secl = *Blanks;
              q#user = q1user;
              q#pgm  = q1pgm;
              q#fold = *blanks;
              q#difl = *blanks;
              q#ipgm = Q1pgm;
              q#fl01 = *blanks;
              q#fl02 = *blanks;
              q#rmcd = CXaprm;
              q#seq# = *blanks;
              q#sbyl = 'N';
              q#com2 = *blanks;
              q#apId = *blanks;
              q#iDoc = *blanks;
              q#iQue = *blanks;
              q#imgF = *blanks;
              q#iPth = *blanks;
              q#pIdx = *blanks;
              q#qPty = *blanks;
              q#dcTp = *blanks;
              q#csId = *blanks;
              q#code = *blanks;
              WriteDarts(q#key:q#comm:q#cDat:q#dcId:q#pNam:q#secl:q#user:
               q#pgm:q#fold:q#diFl:q#iPgm:q#fl01:q#fl02:q#rmCd:q#seq#:
               q#sbyl:q#com2:q#apId:q#iDoc:q#iQue:q#imgF:q#iPth:q#pIdx:
               q#qPty:q#dcTp:q#csId:q#code);
          EndIf;
       EndIf;
       CMapct = scPctAppr;
       CMaeff = dtCYMD(scApprDt);
         CMpEDis = scPeDPct;
         CMlOeC  = scDLOECPct;
          Update pcm0p;
       EndIf;

      /End-Free
     P UpdMedInfo      E
       //===================================================================
       // UpdBenInfo - Updates the Benefit Information
       //===================================================================
     P UpdBenInfo      B                   Export
     D UpdBenInfo      PI
     D claimRrnIn                     9  0
     D benefitPtrIn                    *
     D benefitInfo   e ds                  ExtName(wtClmDtl:rcdClmDt3)
     D                                     based(benefitPtr)
     D benefitPtr      s               *   inz
     D claimRrn        s              9  0
      /Free

       benefitPtr=benefitPtrIn;
       claimRrn=claimRrnIn;

       Chain claimRrn pcm0p;
       If %Found(WMCMP);
          CMpoi=scPaidInDt;
          CMagCp=scEmpComp;
          CMagCd=scCompCode;
          CMdtLw=dtCYMD(scLstWkDt);
          CMdoDd=dtCYMD(scDisNotDt);
          CMdbDt=dtCYMD(scDisDt);
          CMdnLt=dtCYMD(scDtNotLt);
          CMidLt=dtCYMD(scInitDtLt);
          CMsRcF=scFullRC;
          CMsRcP=scPartRC;
          CMeRtw=dtCYMD(scEstRtw);
          CMrlW=dtCYMD(scRelWork);
          CMrtw=dtCYMD(scActRtw);
          CMsame=scRtwEmp;
          CMrtwQ=scRtwQua;
          CMrtwS=scRtwSts;
          CMetFs=scTaxSts;
          CMenEx=scExempts;
          CMsusN=scSusNarr;
          CMsuEf=dtCYMD(scSusEfDt);
          CMncPc=scNonConCd;
          CMtdbe=scTdBeCd;
          CMfSal=scFullSal;
          CMfsDt=dtCYMD(scPaidDt);
          CMfsWk=scWeekPaid;
          CMfsDy=scDaysPaid;
          CMprWW=scWagePre;
          CMotWk=scOthPmt;
          CMwMth=scMthWage;
          CMpoWw=scWagePost;
          CMwgPd=scWagePrd;
          CMdfBe=scDisBen;
          CMrBac=scRedBen;
          CMvoRc=scVocRehb;
          CMpeFf=scPension;
          CMotFf=scOther;
          CMueFf=scUnEmp;
          CMsfFf=scSpecDisa;
          CMssFf=scSocSec;
          CMfsAm=scAmtPaid;
          //*** Beg Add *** 02/14/22 *******************************************
          CMoPyA = scOpYa;
          CMwHaF = scWhAf;
          CMnTz  = scNtZ;
          CMaRdT = dtCYMD(scArDt);
          //*** End Add *** 02/14/22 *******************************************
          CMuser=q1User;
          CMpgm=q1Pgm;
          CMtime=TimeTo6(SystemTime());
          CMdt=DateTo7(SystemDate());
            If scPSPA = 'Y';
               CMpspa = 'E';
            Else;
               CMpspa = ' ';
            EndIf;
          If CMentd = 0 And CMltfl <> 'R';
             CMentD=DateTo7(SystemDate());
          EndIf;
          Update pcm0p;
          // Update the Concurrent Employer Information
          // First Entry
          If scConEmpN1<>*Blanks Or scConEmpP1<>*Zeros Or scConEmpW1<>*Blanks;
             If scConRrn1<>0;
                Chain scConRrn1 pe30p;
             EndIf;
             E3eNam=scConEmpN1;
             E3phon=%Char(scConEmpP1);
             E3wage=scConEmpW1;
             E3fil =*Blanks;
             If scConRrn1<>0;
                Update pe30p;
             Else;
                E3co#=CMco#;
                E3fnd=CMfnd;
                E3fYr=CMfYr;
                E3case=CMcase;
                Write  pe30p;
                scConRrn1=WDE3Prrn;
             EndIf;
          Else;
             If scConRrn1<>0;
                Delete scConRrn1 pe30p;
             EndIf;
          EndIf;
          // Second Entry
          If scConEmpN2<>*Blanks Or scConEmpP2<>*Zeros Or scConEmpW2<>*Blanks;
             If scConRrn2<>0;
                Chain scConRrn2 pe30p;
             EndIf;
             E3eNam=scConEmpN2;
             E3phon=%Char(scConEmpP2);
             E3wage=scConEmpW2;
             E3fil =*Blanks;
             If scConRrn2<>0;
                Update pe30p;
             Else;
                E3co#=CMco#;
                E3fnd=CMfnd;
                E3fYr=CMfYr;
                E3case=CMcase;
                Write  pe30p;
                scConRrn2=WDE3Prrn;
             EndIf;
          Else;
             If scConRrn2<>0;
                Delete scConRrn2 pe30p;
             EndIf;
          EndIf;
       EndIf;

      /End-Free
     P UpdBenInfo      E
       //===================================================================
       // UpdLegInfo - Updates the Legal Information
       //===================================================================
     P UpdLegInfo      B                   Export
     D UpdLegInfo      PI
     D claimRrnIn                     9  0
     D legalPtrIn                      *
     D legalInfo     e ds                  ExtName(wtClmDtl:rcdClmDt4)
     D                                     based(legalPtr)
     D legalPtr        s               *   inz
     D claimRrn        s              9  0
      /Free

       legalPtr=legalPtrIn;
       claimRrn=claimRrnIn;

       Chain(n) claimRrn pcm0p;
       Chain keyWMLLL pll00;

       Monitor;
          LLdefA=%Dec(%Subst(scDefFein:1:9):9:0);
       On-Error 105;
          LLdefA=0;
       EndMon;

       Monitor;
          LLdefM=%Dec(%Subst(scDefFein:10:3):3:0);
       On-Error 105;
          LLdefM=0;
       EndMon;

       If LLdefA = 0;
          LLdefN=scDefName;
       Else;
          kkFein = LLdefA;
          kkMod = LLdefM;
          Chain keyWMPDL ppd00;
          If %Found(WMPDL);
             If PDfsn <> *Blanks;
                LLdefN = %Trim(PDfsN) + ' ' + %Trim(PDname);
             Else;
                LLdefN = %Trim(PDname);
             EndIf;
          EndIf;
       EndIf;
       LLdeDt=dtCYMD(scDefAttDt);
       LLdAd1=scDefAdr1;
       LLdAd2=scDefAdr2;
       LLdCty=scDefCity;
       LLdSt=scDefSt;
       LLdZip=scDefZip;
       phoneNumber=scDefPhn#;
       LLdefR=areaCode;
       LLdefP=phone#;
       LLemdD=scDefEmAd;

       Monitor;
          LLplaA=%Dec(%Subst(scPlaFein:1:9):9:0);
       On-Error 105;
          LLplaA=0;
       EndMon;

       Monitor;
          LLplaM=%Dec(%Subst(scPlaFein:10:3):3:0);
       On-Error 105;
          LLplaM=0;
       EndMon;

       If LLplaA = 0;
          LLplaN=scPlaName;
       Else;
          kkFein = LLplaA;
          kkMod = LLplaM;
          Chain keyWMPDL ppd00;
          If %Found(WMPDL);
             If PDfsn <> *Blanks;
                LLplaN = %Trim(PDfsN) + ' ' + %Trim(PDname);
             Else;
                LLplaN = %Trim(PDname);
             EndIf;
          EndIf;
       EndIf;
       LLrfNm=scPlaRepNm;
       LLdtAp=dtCYMD(scPlaRepDt);
       LLclRi=scPlaRepIn;
       LLpAd1=scPlaAdr1;
       LLpAd2=scPlaAdr2;
       LLpCty=scPlaCity;
       LLpSt=scPlaSt;
       LLpZip=scPlaZip;
       phoneNumber=scPlaPhn#;
       LLplaR=areaCode;
       LLplaP=phone#;
       LLemdP=scPlaEmAd;
       LLlitg=scLitig;
       LLmtSt=scMethSett;
       LLstDt=dtCYMD(scSettDt);
       LLsAmt=scSettAmt;
       LLiScm=dtCYMD(scSetIndDt);
       LLmScm=dtCYMD(scSetMedDt);
       LLfrau=scFraudInd;
       LLlwDt=dtCYMD(scSuitDate);
       LLfDly=dtCYMD(scDelayDt);
       LLlump=scLumpSum;
          //LLWcab  = scWcab;
          LLWcab  = *Blanks;                  //Clear the old WCAB field
          LLWcabN = scWcab;                   //Save data to the new field
          phoneNumber=scIaPhn#;
          LLiaAr=areaCode;
          LLiaPh=phone#;
       If LLlump='Y' and LLmScm > 0;
          LLmDex = 'Y';
       Else;
          LLmDex = 'N';
       Endif;
       //*** Beg Add ***** 03/06/25 ***************************
       LLstruct = scStruct;
       LLmsaamt = scMSAAmt;
       LLmsaprd = scMSAPrd;
       LLindepamt = scInDepAmt;
       LLadepamt = scADepAmt;
       LLcascnt# = scCasCnt#;
       LLpradein = scPrAdEIN;
       //*** End Add ***** 03/06/25 ***************************

       If %Found(WMLLL);
          LLuser=q1User;
          LLpgm=q1Pgm;
          LLtime=TimeTo6(SystemTime());
          LLdt=DateTo7(SystemDate());
          Update pll00;
       Else;
          LLco#=CMco#;
          LLfnd=CMfnd;
          LLfYr=CMfYr;
          LLcase=CMcase;
          LLiUsr=q1User;
          LLiPgm=q1Pgm;
          LLiTm=TimeTo6(SystemTime());
          LLiDt=DateTo7(SystemDate());
          Write pll00;
       EndIf;

      /End-Free
     P UpdLegInfo      E
       //===================================================================
       // UpdStateInfo - Updates the Claim Information
       //===================================================================
     P UpdStateInfo    B                   Export
     D UpdStateInfo    PI
     D claimRrnIn                     9  0
     D statePtrIn                      *
     D stateIn                        2
     D statePtr        s               *   inz
     D state           s              2
     D claimRrn        s              9  0
     d*** Beg Add *** 09/06/22 **********************************************
     D strNi10a2       s             10
     D strNi60a1       s             60
     D strNi60a2       s             60
     D strNi60a3       s             60
     D strNi60a4       s             60
     D strNi60a5       s             60
     D strNi60a6       s             60
     D strNi80a1       s             80
     D strNi80a2       s             80
     D strNi80a3       s             80
     D intTim2         s                   Like(NItim2)
     D strPrNm         s                   Like(NIPrNm)
     D strPrPh         s                   Like(NIPrPh)
     D strNI1a1        s                   Like(NI1a1)
     D strNI1a2        s                   Like(NI1a2)
     d*** End Add *** 09/06/22 **********************************************
      /Free

       state=StateIn;
       kkState=StateIn;
       claimRrn=claimRrnIn;
       statePtr=statePtrIn;

       Chain(n) claimRrn pcm0p;

       Chain keyWMNIL pni00;
       //*** Beg Add *** 09/06/22 ********************************************
       If Not %Found(WMNIL);
          clear pni00;
       Endif;
       //*** End Add *** 09/06/22 ********************************************

       Select;
       // A1 - Data
       When state='A1';
          updStateInfoA1(statePtr);
          // Load global common fields
          strNi10a2 = NI10a2;
          strNi60a1 = NI60a1;
          strNi60a2 = NI60a2;
          strNi60a3 = NI60a3;
          strNi60a4 = NI60a4;
          strNi60a5 = NI60a5;
          strNi60a6 = NI60a6;
          strNi80a1 = NI80a1;
          strNi80a2 = NI80a2;
          strNi80a3 = NI80a3;
          intTim2   = NItim2;
          strPrNm   = NIprNm;
          strPrPh   = NIprPh;
          strNI1a1  = NI1a1;
          strNI1a2  = NI1a2;
       // AL - Data
       When state='AL';
          updStateInfoAL(statePtr);
       // CA - Data
       When state='CA';
          updStateInfoCA(statePtr);
       // DE - Data
       When state='DE';
          updStateInfoDE(statePtr);
       // FL - Data
       When state='FL';
          updStateInfoFL(statePtr);
       // GA - Data
       When state='GA';
          updStateInfoGA(statePtr);
       // LA - Data
       When state='LA';
          updStateInfoLA(statePtr);
       // MA - Data
       When state='MA';
          updStateInfoMA(statePtr);
       // NC - Data
       When state='NC';
          updStateInfoNC(statePtr);
       // NY - Data
       When state='NY';
          updStateInfoNY(statePtr);
       // PA - Data
       When state='PA';
          updStateInfoPA(statePtr);
       When state='WV';
          updStateInfoWV(statePtr);
       EndSl;

       If %Found(WMNIL);
          NIuser=q1User;
          NIpgm=q1Pgm;
          NItime=TimeTo6(SystemTime());
          NIdt=DateTo7(SystemDate());
          Update pni00;
       Else;
          NIco#=CMco#;
          NIfnd=CMfnd;
          NIfyr=CMfyr;
          NIcase=CMcase;
          NIst=state;
          NIiUsr=q1User;
          NIiTm=TimeTo6(SystemTime());
          NIiDt=DateTo7(SystemDate());
          Write pni00;

       Endif;
       //*** Beg Add *** 09/06/22 **********************************************
       // Update State Records that share fields with A1
       kkState = GetFroiState(CMco#:CMjrSt);
       If state = 'A1' and kkState <> 'A1';
          Chain keyWMNIL pni00;
          If %Found(WMNIL);
             Select;
                When kkState = 'AL';
                   NI60a3 = strNi60a3;
                   NI60a4 = strNi60a4;
                   NI80a1 = strNi80a1;
                   NI80a2 = strNi80a2;
                When kkState = 'CA';
                   NItim2 = intTim2;
                When kkState = 'DE';
                   NI60a1 = strNi60a1;
                   NI60a2 = strNi60a2;
                   NI60a3 = strNi60a3;
                   NI60a4 = strNi60a4;
                   NI60a5 = strNi60a5;
                   NI60a6 = strNi60a6;
                   NIprNm = strPrNm;
                   NItim2 = intTim2;
                When kkState = 'MA';
                   NI60a1 = strNi60a1;
                   NI60a2 = strNi60a2;
                   NI80a1 = strNi80a1;
                   NI80a2 = strNi80a2;
                   NIprNm = strPrNm;
                When kkState = 'NC';
                When kkState = 'NY';
                   NI60a1 = strNi60a1;
                   NI60a2 = strNi60a2;
                   NI60a3 = strNi60a3;
                   NI60a4 = strNi60a4;
                   NI60a5 = strNi60a5;
                   NI60a6 = strNi60a6;
                   NIprNm = strPrNm;
                   NIprPh = strPrPh;
                   NItim2 = intTim2;
                When kkState = 'PA';
                   NItim2 = intTim2;
                   NI1a1  = strNi1A1;
                   NI1a2  = strNi1A2;
                   NI60a1 = strNi60a1;
                   NI60a2 = strNi60a2;
                   NI80a1 = strNi80a1;
                   NI80a2 = strNi80a2;
                   NIprNm = strPrNm;
                   NIprPh = strPrPh;
                   position=%Scan(' ':strNi80a3:1);
                   length=position-1;
                   If length>0;
                      strWitnFsn=%Subst(strNi80a3:1:length);
                   Else;
                      strWitnFsn=*Blanks;
                   EndIf;

                   position=position+1;
                   position2=%Scan(' ':strNi80a3:position);
                   length=position2-position;
                   If length>0;
                      strWitnLsn=%Subst(strNi80a3:position:length);
                   Else;
                      strWitnLsn=*Blanks;
                   EndIf;

                   position2 = position2+1;
                   position3 = 80 - position2 + 1;
                   strWitnPhn = %Subst(strNi80a3:position2:position3);
                   Monitor;
                      decWitnPhn = %Dec(strWitnPhn:10:0);
                   On-Error 105;
                      decWitnPhn=0;
                   EndMon;
                   strWitnPhn = %Editc(decWitnPhn:'X');
                   NI80a3 = %Trim(strWitnFsn) + ',' + %Trim(strWitnLsn) +
                    ',' + %Trim(strWitnPhn);
                When kkState = 'WV';
                   NI80a1 = strNi80a1;
                   NI80a2 = strNi80a2;
             Endsl;

             NIuser=q1User;
             NIpgm=q1Pgm;
             NItime=TimeTo6(SystemTime());
             NIdt=DateTo7(SystemDate());
             Update pni00;
          Endif;
       Endif;
       //*** End Add *** 09/06/22 **********************************************

      /End-Free
     P UpdStateInfo    E
       //===================================================================
       // UpdStateInfoA1 - Updates the State Information - A1
       //===================================================================
     P UpdStateInfoA1  B
     D UpdStateInfoA1  PI
     D statePtrIn                      *

     D a1Info        e ds                  ExtName(wtClmDtl:rcdClmDtA1)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       //*** Beg Chg *** 09/06/22 **********************************************
       //***NI60a1=scEquip1A1;
       //***NI60a2=scEquip2A1;
       //***NI60a3=scActiv1A1;
       //***NI60a4=scActiv2A1;
       //***NI60a5=scProcess1;
       //***NI60a6=scProcess2;
       //***NI80a1=scEvents1;
       //***NI80a2=scEvents2;
       NI60a1 = %Subst(scEquip1A1:1:60);
       NI60a2 = %Subst(scEquip1A1:61:60);
       NI60a3 = %Subst(scActiv1A1:1:60);
       NI60a4 = %Subst(scActiv1A1:61:60);
       NI60a5 = %Subst(scProcess1:1:60);
       NI60a6 = %Subst(scProcess1:61:60);
       NI80a1 = %Subst(scEvents1:1:80);
       NI80a2 = %Subst(scEvents1:81:80);
       //*** End Chg *** 09/06/22 **********************************************
       NI1a1=scSafe;
       NI1a2=scUsed;
       NI80a3=scWitnA11 + scWitnA12;
       NI10a2=scHireSt;
       NItim2=scBegWrkA1;
       NIprNm=%Trim(scPreparer) + ',' + %Trim(scPrepTitl);
       NIprPh=%EditC(scPrepPhn:'X');
       //*** Beg Add *** 09/06/22 **********************************************
       NIprep = DtCyMd(scDtPrep);
       //*** End Add *** 09/06/22 **********************************************

      /End-Free
     P UpdStateInfoA1  E
       //===================================================================
       // UpdStateInfoAL - Updates the State Information - AL
       //===================================================================
     P UpdStateInfoAL  B
     D UpdStateInfoAL  PI
     D statePtrIn                      *

     D alInfo        e ds                  ExtName(wtClmDtl:rcdClmDtAL)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       NI20a2=scEmpId#;
       NI40a7=scSpecProd;
       NI1a1=scWcmsProv;
       NI30a4=scCarSelf#;
       NI10a1=%EditC(scAgeAL:'X');
       NI40a8=scRtwOccAL;
       NI3n2=scLosYear;
       NI3n3=scLosMonth;
       NI3n4=scTimeYear;
       NI3n5=scTimeMnth;
       NI9n1=scOthWage;
       //*** Beg Del *** 09/06/22 **********************************************
       //***NI60a3=scProdInj1;
       //***NI60a4=scProdInj2;
       //*** End Del *** 09/06/22 **********************************************
       //*** Beg Chg *** 09/06/22 **********************************************
       //***NI60a1=scAccOcc1;
       //***NI60a2=scAccOcc2;
       NI60a1 = %Subst(scAccOcc1:1:60);
       NI60A2 = %Subst(scAccOcc1:61:60);
       //*** End Chg *** 09/06/22 **********************************************
       //*** Beg Del *** 09/06/22 **********************************************
       //***NI80a1=scDescEvn1;
       //***NI80a2=scDescEvn2;
       //*** End Del *** 09/06/22 **********************************************

      /End-Free
     P UpdStateInfoAL  E
       //===================================================================
       // UpdStateInfoCA - Updates the State Information - CA
       //===================================================================
     P UpdStateInfoCA  B
     D UpdStateInfoCA  PI
     D statePtrIn                      *

     D caInfo        e ds                  ExtName(wtClmDtl:rcdClmDtCA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       NI30a2=scUnEmpIns;
       NI20a1=scTypeEmp;
       NIdt1=dtCYMD(scFormDt);
       //*** Beg Del *** 09/06/22 **********************************************
       //***NItim2=scBegWrkCA*100;
       //*** End Del *** 09/06/22 **********************************************
       NI9n3=scGrossWag*100;
       NI40a6=scWagePer;
       //*** Beg Chg *** 09/06/22 **********************************************
       //***NI80a4=scEquip1CA;
       //***NI80a5=scEquip2CA;
       //***NI80a6=scActiv1CA;
       //***NI80a7=scActiv2CA;
       NI80a4 = %Subst(scEquip1CA:1:80);
       NI80a5 = %Subst(scEquip1CA:81:80);
       NI80a6 = %Subst(scActiv1CA:1:80);
       NI80a7 = %Subst(scActiv1CA:81:80);
       //*** End Chg *** 09/06/22 **********************************************
       NI30a3=scCountyCA;
       NI40a4=scExpDept;
       NI1a5=scHospInp;
       NI1a6=scEmrgRoom;

      /End-Free
     P UpdStateInfoCA  E
       //===================================================================
       // UpdStateInfoDE - Updates the State Information - DE
       //===================================================================
     P UpdStateInfoDE  B
     D UpdStateInfoDE  PI
     D statePtrIn                      *

     D deInfo        e ds                  ExtName(wtClmDtl:rcdClmDtDE)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       NI30a2=scCountyDE;
       NI10a1=%EditC(scAgeDE:'C');
       NI40a4=scEmpDept;
       NI20a3=scEmpLong;
       NI40a8=scInjDept;
       //*** Beg Del *** 09/06/22 **********************************************
       //***NI60a5=scWorkPrc1;
       //***NI60a6=scWorkPrc2;
       //***NIprNm=%Trim(scPersonDe) + ',' + %Trim(scPositDE);
       //***NI60a1=scEqupDE1;
       //***NI60a2=scEqupDE2;
       //***NI60a3=scActivDE1;
       //***NI60a4=scActivDE2;
       //*** End Del *** 09/06/22 **********************************************
       NIprep=dtCYMD(scRptDate);
       //*** Beg Del *** 09/06/22 **********************************************
       //***NItim2=scStrTime*100;
       //*** End Del *** 09/06/22 **********************************************
       NI1a2=scSameWage;
       NIdt2=dtCYMD(scFullPaid);

      /End-Free
     P UpdStateInfoDE  E
       //===================================================================
       // UpdStateInfoFL - Updates the State Information - FL
       //===================================================================
     P UpdStateInfoFL  B
     D UpdStateInfoFL  PI
     D statePtrIn                      *

     D flInfo        e ds                  ExtName(wtClmDtl:rcdClmDtFL)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       NI1a2=scDenyCase;
       NI1a6=scIndDeny;
       NI1a3=scMedOnly;
       NIdt1=dtCYMD(sc8thDay);
       NIdt2=dtCYMD(scKnow8th);
       NI1a5=scLstTimFL;
       NIdt4=dtCYMD(scMailDate);
       NI10a1=scDisType;
       NI9n1=scPenAmt*100;
       NI9n2=scIntamt*100;
       //*** Beg Chg *** 09/06/22 ********************************************
       //***NI40a1=scRmrk1FL;
       //***NI40a2=scRmrk2FL;
       NI40a1 = %Subst(scRmrk1FL:1:40);
       NI40a2 = %Subst(scRmrk1FL:41:40);
       //*** End Chg *** 09/06/22 ********************************************
       NI9n3=scRtPay*100;
       NI20a9=scPer;
       NI3n1=scHrsPrWk;

      /End-Free
     P UpdStateInfoFL  E
       //===================================================================
       // UpdStateInfoGA - Updates the State Information - GA
       //===================================================================
     P UpdStateInfoGA  B
     D UpdStateInfoGA  PI
     D statePtrIn                      *

     D gaInfo        e ds                  ExtName(wtClmDtl:rcdClmDtGA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       NI40a3=scEmEmailG;
       NI40a4=scClmEmail;
       NI5n3=scSbwcId#;
       NI40a5=scPerEmail;
       NI20a1=scDaysOff;
       NI5n2=scWeekBen;
       NIdt1=dtCYMD(scPayFrom);
       NI10a2=scTtTpPpCd;
       NI3n1=scPercGA;
       NI10a3=scBodyPart;
       NI3n2=scNumWeeks;
       NIdt2=dtCYMD(scUntlDate);
       NI60a1=scBenefits;

      /End-Free
     P UpdStateinfoGA  E
       //===================================================================
       // UpdStateInfoLA - Updates the State Information - LA
       //===================================================================
     P UpdStateInfoLA  B
     D UpdStateInfoLA  PI
     D statePtrIn                      *

     D laInfo        e ds                  ExtName(wtClmDtl:rcdClmDtLA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       NIprep=dtCYMD(scRptDtLA);
       NI1a2=scSmWageLA;
       NItim2=scNormTime;
       NIdt2=dtCYMD(scLastDate);
       clear NI20a4;
       If sc7DayDis = 'Y';
          NI20a4 = %trim(ni20a4) + '1';
       Endif;
       If scPossDisp = 'Y';
          NI20a4 = %trim(ni20a4) + '2';
       Endif;
       If scMdOnlyLA = 'Y';
          NI20a4 = %trim(ni20a4) + '3';
       Endif;
       If scInjDeath = 'Y';
          NI20a4 = %trim(ni20a4) + '4';
       Endif;
       If scLumpSett = 'Y';
          NI20a4 = %trim(ni20a4) + '5';
       Endif;
       If scAmpDis = 'Y';
          NI20a4 = %trim(ni20a4) + '6';
       Endif;
       If scOtherLA = 'Y';
          NI20a4 = %trim(ni20a4) + '7';
       Endif;
       NI20a3=scInjPar;
       NI30a3=scPlaPar;
       NI40a4=scDeptLA;
       NI60a3=scActivLA1;
       NI60a4=scActivLA2;
       NI80a1=scCauseLA1;
       NI80a2=scCauseLA2;
       NIdt3=dtCYMD(scOccDisDt);
       NIprNm=scPersform;
       NI40a1=scEmpAddLA;
       NI25a1=scEmpCtyLA;
       NI2a1=scEmpStLA;
       NI9n2=scEmpZipLA;

      /End-Free
     P UpdStateInfoLA  E
       //===================================================================
       // UpdStateInfoMA - Updates the State Information - MA
       //===================================================================
     P UpdStateInfoMA  B
     D UpdStateInfoMA  PI
     D statePtrIn                      *

     D maInfo        e ds                  ExtName(wtClmDtl:rcdClmDtMA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       NI1a2=scAwwInd;
       NI2a2=scIndcode;
       NI1a1=scSlfIns;
       NI40a3=scSlfIns#;
       NI20a7=scBusType;
       NIdt1=dtCYMD(sc5thDay);
       //*** Beg Del *** 09/06/22 *********************************************
       //***NI60a1=scSrcInj1;
       //***NI60a2=scSrcInj2;
       //***NI80a1=scInjOcc1;
       //***NI80a2=scInjOcc2;
       //*** End Del *** 09/06/22 *********************************************
       NIcont=scPersRpt;
       NIdt2=dtCYMD(scWorkRel);
       NI10a1=scInjCode1;
       NI10a2=scInjCode2;
       NI10a3=scInjCode3;
       NI10a4=scBodCode1;
       NI10a5=scBodCode2;
       NI20a6=scBodCode3;
       //*** Beg Chg *** 09/06/22 ******************************************
       //***NI40a4=scWitnMA1;
       //***NI40a5=scWitnMA2;
       NI40a4 = %Subst(scWitnMA1:1:40);
       NI40a5 = %Subst(scWitnMA1:41:40);
       //*** End Chg *** 09/06/22 ******************************************
       NI1a4=scRetRegOc;
       //*** Beg Del *** 09/06/22 *********************************************
       //***NIprNm=%Trim(scPrepMA) + ',' + %Trim(scPrpTtlMA);
       //***NIprep=dtCYMD(scPrepDtMA);
       //*** End Del *** 09/06/22 *********************************************

      /End-Free
     P UpdStateInfoMA  E
       //===================================================================
       // UpdStateInfoNC - Updates the State Information - NC
       //===================================================================
     P UpdStateInfoNC  B
     D UpdStateInfoNC  PI
     D statePtrIn                      *

     D ncInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNC)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       //*** Beg Chg *** 09/06/22 **********************************************
       //***NI60a1=scEmpNat1;
       //***NI60a2=scEmpNat2;
       NI60a1 = %Subst(scEmpNat1:1:60);
       NI60a2 = %Subst(scEmpNat1:61:60);
       //*** End Chg *** 09/06/22 **********************************************
       NI40a1=scPlantLoc;
       NI30a1=scCountyNC;
       NI30a2=scDeptNC;
       NI20a1=scEmpPrem;
       NI1a3=scDisAmPm;
       NI20a3=%EditC(scEmpYear:'X') + %EditC(scEmpMon:'X') +
          %EditC(scEmpDay:'X');
       NI10a1=%EditC(scWagePerH:'X');
       NI10a2=%EditC(scWagePerD:'X');
       NI10a3=%EditC(scEstVal:'X');
       NI10a4=scPerPrd;
       //*** Beg Chg *** 09/06/22 **********************************************
       //***NI60a3=scInjList1;
       //***NI60a4=scInjList2;
       NI60a3 = %Subst(scInjList1:1:60);
       NI60a4 = %Subst(scInjList1:61:60);
       //*** End Chg *** 09/06/22 **********************************************
       NI10a5=%EditC(scHourRtw:'X');
       If scHourAmPm='A';
          NI1a6='X';
          NI1a7=' ';
       Else;
          NI1a6=' ';
          NI1a7='X';
       EndIf;
       NI20a2=%EditC(scRtwWages:'X');
       NI20a4=scPerPrd2;
       NI60a5=scRtwOcc;
       NI1a8=scPhysTrt;

      /End-Free
     P UpdStateInfoNC  E
       //===================================================================
       // UpdStateInfoNY - Updates the State Information - NY
       //===================================================================
     P UpdStateInfoNY  B
     D UpdStateInfoNY  PI
     D statePtrIn                      *

     D nyInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNY)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       NItim2=scEmpBegDt;
       NI5n1=scTotEarn;
       NI10a1=%Trim(scMon) + %Trim(scTue) + %Trim(scWed) +
        %Trim(scThu) + %Trim(scFri) + %Trim(scSat) + %Trim(scSun);
       NI30a3=scCountyNY;
       NI40a4=scDeptNY;
       NI1a1=scEmpProv;
       NIdt1=dtCYMD(scMedCareD);
       NI1a2=scEmpTreat;
       NI1a3=scEmpHos;
       //*** Beg Del *** 09/06/22 *********************************************
       //***NI60a5=scEmpDo1;
       //***NI60a6=scEmpDo2;
       //***NI60a3=scHowOcc1;
       //***NI60a4=scHowOcc2;
       //***NI60a1=scObjInj1;
       //***NI60a2=scObjInj2;
       //*** End Del *** 09/06/22 *********************************************
       NI40a5=scContNY;
       NI30a5=sccontPhNY;
       NI80a7=scComName + scComAdr1;
       NI80a8=scComAdr2 + scComCity + scComSt + %EditC(scComZip:'X');
       NI80a5=scRelName + scRelAdr1;
       NI80a6=scRelAdr2 + scRelCity + scRelSt + %EditC(scRelZip:'X');
       NI10a3=scRelRel;
       //*** Beg Del *** 09/06/22 *********************************************
       //***NIprNm=%Trim(scPrepNY) + ',' + %Trim(scPrpTtlNY);
       //***NIprPh=%EditC(scPrpPhnNY:'X');
       //*** End Del *** 09/06/22 *********************************************

      /End-Free
     P UpdStateInfoNY  E
       //===================================================================
       // UpdStateInfoPA - Updates the State Information - PA
       //===================================================================
     P UpdStateInfoPA  B
     D UpdStateInfoPA  PI
     D statePtrIn                      *

     D paInfo        e ds                  ExtName(wtClmDtl:rcdClmDtPA)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;
       //*** Beg Del *** 09/06/22 *********************************************
       //***NItim2=scBegShft;
       //*** End Del *** 09/06/22 *********************************************
       NIcont=%Trim(scContFsn) + ',' + %Trim(scContLsn);
       phoneNumber=scContPhn#;
       NIcAr=areaCode;
       NIcPhn=phone#;
       //*** Beg Del *** 09/06/22 *********************************************
       //***NI1a1=scSafeProv;
       //***NI1a2=scSafeUsed;
       //*** End Del *** 09/06/22 *********************************************
       NI30a4=scCountyPA;
       //*** Beg Del *** 09/06/22 *********************************************
       //***NI60a1=scMatUsed1;
       //***NI60a2=scMatUsed2;
       //***NI80a1=scSeqDesc1;
       //***NI80a2=scSeqDesc2;
       //*** End Del *** 09/06/22 *********************************************
       NI1a3=scTools;
       NI1a4=scMechDef;
       NI1a5=scUnSafe;
       NI1a6=scUnSafeCn;
       NI1a7=scAmputate;
       NI1a8=scVehicle;
       NI1a9=scCovered;
       //*** Beg Del *** 09/06/22 *********************************************
       //***NI80a3=%Trim(scWitnFsn) + ',' + %Trim(scWitnLsn) + ',' +
       //*** %EditC(scWitnPhn:'X');
       //*** End Del *** 09/06/22 *********************************************
       NI20a4=scCarBurCd;
       //*** Beg Del *** 09/06/22 *********************************************
       //***NIprNm=%Trim(scPrepPA) + ',' + %Trim(scPrpTtlPA);
       //***NIprPh=%EditC(scPrpPhnPA:'X');
       //*** End Del *** 09/06/22 *********************************************
       NIprep=dtCYMD(scPrepDtPA);

      /End-Free
     P UpdStateInfoPA  E
       //===================================================================
       // UpdStateInfoWV - Updates the State Information - WV
       //===================================================================
     P UpdStateInfoWV  B
     D UpdStateInfoWV  PI
     D statePtrIn                      *

     D wvInfo        e ds                  ExtName(wtClmDtl:rcdClmDtWV)
     D                                     based(statePtr)
     D statePtr        s               *
      /Free

       statePtr=statePtrIn;

       NI1A8  = sc1a8;
       NI1A9  = sc1a9;

       If scDt2 <>  *Zeros;
          NIdt2= DateTo7(CvtToDate(scDt2));
       Else;
          NIdt2 = *Zeros;
       EndIf;

       NI60A7 = sc60a7;
       //*** Beg Del *** 09/06/22 **********************************************
       //***NI80A1 = sc80a1;
       //***NI80A2 = sc80a2;
       //*** End Del *** 09/06/22 **********************************************
       NI1a3  = sc1a3;
       NI1a4  = sc1a4;
       NI30a5 = sc30a5;
       NI1a5  = sc1a5;
       NI1a6  = sc1a6;
       NI5n1  = sc5n1;
       NI1a7  = sc1a7;
       NI92n1 = sc92n1;
       NI2a2  = sc2a2 ;
       NI1a10 = sc1a10;
       NI92n2 = sc92n2;
       NI2a3  = sc2a3 ;
       NI92n3 = sc92n3;
       NI92n4 = sc92n4;

      /End-Free
     P UpdStateInfoWV  E
       //===================================================================
       // UpdQuickDiary - Updates the Quick Diary
       //===================================================================
     P UpdQuickDiary   B                   Export
     D UpdQuickDiary   PI
     D claimRrnIn                     9  0
     D diaryPtrIn                      *
     D diaryInfo     e ds                  ExtName(wtClmDtl:rcdClmDt6)
     D                                     based(diaryPtr)
     D diaryPtr        s               *   inz
     D claimRrn        s              9  0
     D intCnt          s              5  0

     D                 ds
     D dartsKey                1     40
     D company#                1      3  0
     D group                   4      6  0
     D year                    7      9  0
     D case                   10     16  0

     D                 ds
     D SQV                     1     25  0
     D                                     DIM(5)
     D ##sdsq                  1      5  0
     D ##cdsq                  6     10  0
     D ##mdsq                 11     15  0
     D ##bdsq                 16     20  0
     D ##udsq                 21     25  0

     D                 DS
     D  QDT                    1     40  0
     D                                     DIM(5)
     D  ##sdat                 1      8  0
     D  ##cdat                 9     16  0
     D  ##mdat                17     24  0
     D  ##bdat                25     32  0
     D  ##udat                33     40  0
     D                 DS
     D  DT                     1     40  0
     D                                     DIM(5)
     D  intsdat                1      8  0
     D  intcdat                9     16  0
     D  intmdat               17     24  0
     D  intbdat               25     32  0
     D  intudat               33     40  0

     C     keySDCMLA     Klist
     C                   Kfld                    dartsKey
     C                   Kfld                    kkSeq#

      /Free

       diaryPtr=diaryPtrIn;
       claimRrn=claimRrnIn;

       Chain(n) claimRrn pcm0p;
       If %Found(WMCMP);
          company#=CMco#;
          group=CMfnd;
          year=CMfYr;
          case=CMcase;
       EndIf;

       Chain(n) keyWMDDL pdd00;
       If Not %Found(WMDDL);
          Clear pdd00;
       EndIf;

       // Initialize sequence #'s

       // Write Medical Diary
       If medDate<>dtCYMD(scMedDate) and scMedDate<>*Zeros;
          // Load Values
          dartsComm=scMedNote;
          dartsDate=%EditC(dtCheck(scMedDate):'X');
          dartsCode=scMedRmCd;
          dartsUser=getUserId(CMclEx);
          // Write Remark
          WriteRmk(dartsKey:dartsSeq#:dartsComm:dartsDate:dartsCode:
           dartsProgram:dartsUser:dartsPnam:dartsDiary:dartsDeFlag:
           dartsPurge:dartsSecByLvl:dartsSecLvl:dartsDocId:dartsFolder);
          // Load the Seq#
          medSeq#=%Dec(dartsSeq#:5:0);
       EndIf;

       // Write Compensation Diary
       If compDate<>dtCYMD(scCompDate) and scCompDate<>*Zeros;
          // Load Values
          dartsComm=scCompNote;
          dartsDate=%EditC(dtCheck(scCompDate):'X');
          dartsCode=scCompRmCd;
          dartsUser=getUserId(CMclEx);
          // Write Remark
          WriteRmk(dartsKey:dartsSeq#:dartsComm:dartsDate:dartsCode:
           dartsProgram:dartsUser:dartsPnam:dartsDiary:dartsDeFlag:
           dartsPurge:dartsSecByLvl:dartsSecLvl:dartsDocId:dartsFolder);
          // Load the Seq#
          compSeq#=%Dec(dartsSeq#:5:0);
       EndIf;

       // Write State Form Diary
       If formDate<>dtCYMD(scFormDate) and scFormDate<>*Zeros;
          // Load Values
          dartsComm=scFormNote;
          dartsDate=%EditC(dtCheck(scFormDate):'X');
          dartsCode=scFormRmCd;
          dartsUser=getUserId(CMclEx);
          // Write Remark
          WriteRmk(dartsKey:dartsSeq#:dartsComm:dartsDate:dartsCode:
           dartsProgram:dartsUser:dartsPnam:dartsDiary:dartsDeFlag:
           dartsPurge:dartsSecByLvl:dartsSecLvl:dartsDocId:dartsFolder);
          // Load the Seq#
          formSeq#=%Dec(dartsSeq#:5:0);
       EndIf;

       // Write Supervisor Diary
       If suprDate<>dtCYMD(scSuprDate) and scSuprDate<>*Zeros;
          // Load Values
          dartsComm=scSuprNote;
          dartsDate=%EditC(dtCheck(scSuprDate):'X');
          dartsCode=scSuprRmCd;
          dartsUser=getUserId(CMsupr);
          // Write Remark
          WriteRmk(dartsKey:dartsSeq#:dartsComm:dartsDate:dartsCode:
           dartsProgram:dartsUser:dartsPnam:dartsDiary:dartsDeFlag:
           dartsPurge:dartsSecByLvl:dartsSecLvl:dartsDocId:dartsFolder);
          // Load the Seq#
          suprSeq#=%Dec(dartsSeq#:5:0);
       EndIf;

       // Write Unit Manager Diary
       If unitDate<>dtCYMD(scUnitDate) and scUnitDate<>*Zeros;
          // Load Values
          dartsComm=scUnitNote;
          dartsDate=%EditC(dtCheck(scUnitDate):'X');
          dartsCode=scUnitRmCd;
          dartsUser=getUserId(CMuMan);
          // Write Remark
          WriteRmk(dartsKey:dartsSeq#:dartsComm:dartsDate:dartsCode:
           dartsProgram:dartsUser:dartsPnam:dartsDiary:dartsDeFlag:
           dartsPurge:dartsSecByLvl:dartsSecLvl:dartsDocId:dartsFolder);
          // Load the Seq#
          unitSeq#=%Dec(dartsSeq#:5:0);
       EndIf;

       // Write Update WMDDP
       Chain keyWMDDL pdd00;
       If %Found(WMDDL);
          intmdat = dtCYMD(scMedDate);
          intcdat = dtCYMD(scCompDate);
          intbdat = dtCYMD(scFormDate);
          intsdat = dtCYMD(scSuprDate);
          intudat = dtCYMD(scUnitDate);
          ##sdsq  = DDsdsq;
          ##cdsq  = DDcdsq;
          ##mdsq  = DDmdsq;
          ##bdsq  = DDbdsq;
          ##udsq  = DDudsq;
          ##sdat  = suprDate;
          ##cdat  = compDate;
          ##mdat  = medDate;
          ##bdat  = formDate;
          ##udat  = unitDate;

          intCnt = 5;
          dow intCnt > 0;
             If (QDT(intCnt) <> DT(intCnt)) And QDT(intCnt) <> 0;
                If SQV(intCnt) <> 0;
                   kkSeq# = SQV(intCnt);
                   Chain keySDCMLA scm0a;
                   If %Found(SDCMLA);
                      C2dcdt = DateTo7(SystemDate());
                      C2pgm = q1pgm;
                      C2dt = DateTo7(SystemDate());
                      C2fl04 = *Blanks;
                      update scm0a;
                   Endif;
                Endif;
             Endif;
             intCnt = intCnt - 1;
          Enddo;
          DDmDat=dtCYMD(scMedDate);
          DDcDat=dtCYMD(scCompDate);
          DDbDat=dtCYMD(scFormDate);
          DDsDat=dtCYMD(scSuprDate);
          DDuDat=dtCYMD(scUnitDate);
          DDclEx=CMclEx;
          DDmdSq=medSeq#;
          DDcdSq=compSeq#;
          DDbdSq=formSeq#;
          DDsdSq=suprSeq#;
          DDudSq=unitSeq#;
          DDuser=q1User;
          DDpgm=q1Pgm;
          DDtime=TimeTo6(SystemTime());
          DDdt=DateTo7(SystemDate());
          Update pdd00;
       Else;
          Clear pdd00;
          DDco#=CMco#;
          DDfnd=CMfnd;
          DDfyr=CMfyr;
          DDcase=CMcase;
          DDmDat=dtCYMD(scMedDate);
          DDcDat=dtCYMD(scCompDate);
          DDbDat=dtCYMD(scFormDate);
          DDsDat=dtCYMD(scSuprDate);
          DDuDat=dtCYMD(scUnitDate);
          DDclEx=CMclEx;
          DDmdSq=medSeq#;
          DDcdSq=compSeq#;
          DDbdSq=formSeq#;
          DDsdSq=suprSeq#;
          DDudSq=unitSeq#;
          DDuser=q1User;
          DDpgm=q1Pgm;
          DDtime=TimeTo6(SystemTime());
          DDdt=DateTo7(SystemDate());
          Write pdd00;
       EndIf;

      /End-Free
     P UpdQuickDiary   E
       //===================================================================
       // UpdOutRsvs - Update the Outstanding Reserves
       //===================================================================
     P UpdOutRsvs      B
     D UpdOutRsvs      PI
     D company#                       3  0 const
     D group#                         3  0 const
     D employer#                      9  0 const
     D division                       5  0 const
     D year                           3  0 const
     D outRsvs                             const like(ELouts)

      *** Beg Add *** 07/29/21 *****************************************
     D Tstouts         s             11  2
      *** End Add *** 07/29/21 *****************************************

     C     keyWDELL      Klist
     C                   Kfld                    company#
     C                   Kfld                    group#
     C                   Kfld                    employer#
     C                   Kfld                    division
     C                   Kfld                    year
      /Free
       Chain keyWDELL pel00;
       If %Found(WDELL);
      *** Beg Add *** 07/29/21 *****************************************
          GetClientID(outCltID);
          If outCltID  = '114';
             Tstouts=ELouts+outRsvs;
             If Tstouts < 9999999.99;
               ELouts=ELouts+outRsvs;
               Update pel00;
             EndIf;
          Else;
      *** End Add *** 07/29/21 *****************************************
             ELouts=ELouts+outRsvs;
             Update pel00;
      *** Beg Add *** 07/29/21 *****************************************
          EndIf;
      *** End Add *** 07/29/21 *****************************************
       EndIf;

      /End-Free
     P UpdOutRsvs      E
       //===================================================================
       // WriteClaim - Writes Claim Record
       //===================================================================
     P WriteClaim      B                   Export
     D WriteClaim      PI             9  0
     D company#                       3  0 const
     D group#                         3  0 const
     D year                           3  0 const
     D employer#                      9  0 const
     D division                       5  0 const
     D location                      10    const
     D tpa#                           5    const
     D socialSec#                     9    const
     D accidentDate                   7  0 const
     D jurState                       2    const
     D sicCode                        6    const
      /Free
       //
       // Create the Claim Record
       //
       Clear pcm0p;
       CMco#=company#;
       CMfnd=group#;
       CMfYr=year;
       CMcase=GetNextClm#(company#:group#);
       CMemp#=employer#;
       CMdiv=division;
       CMctg1=location;
       CMoff#=tpa#;
       CMss#=socialSec#;
       CMacDt=accidentDate;
       CMuser=q1User;
       CMpgm=q1Pgm;
       CMtime=TimeTo6(SystemTime());
       CMdt=DateTo7(SystemDate());
       CMiUsr=q1User;
       CMiPgm=q1Pgm;
       CMiTm=TimeTo6(SystemTime());
       CMiDt=DateTo7(SystemDate());
       CMstat='O';
       CMentD=DateTo7(SystemDate());
       CMsic=sicCode;
       CMjrSt=jurState;
       CMcec=GetUsrStaf(q1User);

       chain (CMco#:CMfnd) PCX02;
       If %found(WMCXP2);
          Monitor;
             CMLossCd = %dec(CXLossCd:2:0);
          On-error;
             CMLossCd = 0;
          EndMon;
          CMlsrv = CXlsReCd;
          CMcovt = CXcovt;
          CMlscv = CXlscv;
       Endif;

       Write pcm0p;
       //
       // Write Claim History File
       //
       Clear pch0p;
       CHco#=CMco#;
       CHfnd=CMfnd;
       CHfYr=CMfyr;
       CHcase=CMcase;
       CHefDt=DateTo7(SystemDate());
       CHexpl='Original open of claim';
       CHocSt=CMstat;
       CHstat=*Blanks;
       CHuser=q1User;
       CHpgm=q1Pgm;
       CHtime=TimeTo6(SystemTime());
       CHdt=DateTo7(SystemDate());
       Write pch0p;

       Return dbrrn;

      /End-Free
     P WriteClaim      E
       //===================================================================
       // WriteIMR - Write Claim Status Change
       //===================================================================
     P WriteIMR        B
     D WriteIMR        PI
     D company#                       3  0 const
     D group#                         3  0 const
     D year                           3  0 const
     D case                           7  0 const
     D statusOld                      1    const
     D statusNew                      1    const
      /Free
       //
       // Status Change History Record
       //
       Clear pth0p;
       THco#=company#;
       THfnd=group#;
       THfYr=year;
       THcase=case;
       THfrom=statusOld;
       THto=statusNew;
       THuser=q1User;
       THtrDt=DateTo7(SystemDate());
       THstat=*BLanks;
       THpgm=q1Pgm;
       THtime=TimeTo6(SystemTime());
       THdt=DateTo7(SystemDate());

       Write pth0p;

      /End-Free
     P WriteIMR        E
       //===================================================================
       // WriteReserves - Write Initial Reserve Info
       //===================================================================
     P WriteReserves   B
     D WriteReserves   PI
     D company#                       3  0 const
     D group#                         3  0 const
     D year                           3  0 const
     D case                           7  0 const
     D employer#                      9  0 const
     D division                       5  0 const

     D totalIncurred   s             11  2
     D  pos            s              3  0
      /Free
       //
       // Get Group Information
       Chain (company#:group#) pfn00;
       If %Found(WMFNL);
          //
          // Check to see if reserves exist
          totalIncurred=%Xfoot(rsvRemain) + %xFoot(rsvPaid);
          If chkReserves(company#:group#:year:case)=*Off And
             totalIncurred = 0;
          //
          // See if they have Quest Flag seton
             Chain 'SY' psy00;
             If %Found(SSYLF) And %Subst(SYFL11:1:1)='Y';
                CMrt01=FNrs01;
                CMrt02=FNrs02;
                CMrt02=FNrs02;
                CMrt03=FNrs03;
                CMrt04=FNrs04;
                CMrt05=FNrs05;
                CMrt06=FNrs06;
                CMrt07=FNrs07;
                CMrt08=FNrs08;
                CMrt09=FNrs09;
                CMrt10=FNrs10;
             EndIf;
             //
             // Set Medical Default
             //
             Clear prh0p;
             If CMltFl='M' And CMstat='O';
                //
                // Assign Default Medical Value If Employer does exclude
                If FNdfRc<>*Blanks And
                   GetExclMed(company#:group#:employer#:division:year)=' ';
                   pos=%LookUp(FNdfRc:rsvCode);
                   If pos=0;
                      pos=%LookUp(' ':rsvCode);
                   EndIf;
                   rsvCode(pos)=FNdfRc;
                   rsvRemain(pos)=FNdfRa;
                   rsvHstCd(pos)=FNdfRc;
                   rsvHstChg(pos)=FNdfRa;
                Endif;
                //
                // Assign Default Expense
                If FNdfXc<>*Blanks;
                   pos=%LookUp(FNdfXc:rsvCode);
                   If pos=0;
                      pos=%LookUp(' ':rsvCode);
                   EndIf;
                   rsvCode(pos)=FNdfXc;
                   rsvRemain(pos)=FNdfXa;
                   rsvHstCd(pos)=FNdfXc;
                   rsvHstChg(pos)=FNdfXa;
                EndIf;
                //
                // Only create reserve history if there were changes
                If %Xfoot(rsvHstChg)<>0;
                   //
                   // Write Reserve History
                   //
                   RHco#=company#;
                   RHfnd=group#;
                   RHfYr=year;
                   RHcase=case;
                   RHstat=*Blanks;
                   RHuser=q1User;
                   RHefDt=DateTo7(SystemDate());
                   RHexpl='Original Reserves';
                   RHpgm=q1Pgm;
                   RHtime=TimeTo6(SystemTime());
                   RHdt=DateTo7(SystemDate());
                   RHiUsr=RHuser;
                   RHiPgm=RHpgm;
                   RHiTim=RHtime;
                   RHiDt=RHdt;
                   RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);

                   Write prh0p;

                   //
                   // Update the outstanding reserves
                   UpdOutRsvs(Company#:Group#:Employer#:Division:Year:
                    %Xfoot(rsvHstChg));
                EndIf;
             Endif;
          EndIf;
       EndIf;

      /End-Free
     P WriteReserves   E
       //===================================================================
       // WriteTransfer - Write Diary Transfer Record
       //===================================================================
     P WriteTransfer   B
     D WriteTransfer   PI
     D category                       1    const
     D oldStaffCode                   3    const
     D newStaffCode                   3    const
      /Free
       //
       // Status Change History Record
       //
       Clear pdt0p;
       DTco#=CMco#;
       DTfnd=CMfnd;
       DTfYr=CMfYr;
       DTcase=CMcase;
       DTctgy=category;
       DToStf=oldStaffCode;
       DTnStf=newStaffCode;
       DTuser=q1User;
       DTpgm=q1Pgm;
       DTtime=TimeTo6(SystemTime());
       DTdt=DateTo7(SystemDate());

       Write pdt0p;

      /End-Free
     P WriteTransfer   E
       //===================================================================
       // Populate Top Five Payee Graph Data
       //===================================================================
     P PopTopFive      B                   Export
     D PopTopFive      PI
     D  claimRrnIn                    9  0

     D  claimRrn       s              9  0
     D  jobN           s             10
     D  jobU           s             10
     D  job#           s              6s 0
      /Free

       jobN=q1job;
       jobU=q1cUsr;
       job#=q1jobN;
       claimRrn=claimRrnIn;

       Chain(n) claimRrn pcm0p;

       DelTopFive(jobN:jobU:job#);

       exec sql
        insert into wxp5p (P5jobN, P5jobU, P5job#, P5name, P5amt)
        select :jobN, :jobU, :job#, RPpNam,
         sum(Case when PAtype in ('R', 'S', 'V') then PAamt*-1 else PAamt end)
        from wdpap join wdrpp on PAfnd=RPfnd And PAfYr=RPfYr And
         PAcase=RPcase and PAchk#=RPchk# and PAseq#=RPseq#
        where PAfnd=:CMfnd And PAfYr=:CMfYr And PAcase=:CMcase
        group by RPco#, RPfnd, RPfYr, RPcase, RPpNam
        order by sum(Case when PAtype in ('R', 'S', 'V') then
         PAamt*-1 else PAamt end) Desc
        fetch first 5 rows only;

      /End-Free

     P PopTopFive      E
       //===================================================================
       // Delete From Top Five Work File
       //===================================================================
     P DelTopFive      B                   Export
     D DelTopFive      PI
     D  jobNameIn                    10
     D  jobUserIn                    10
     D  jobNumberIn                   6s 0

     D  jobName        s             10
     D  jobUser        s             10
     D  jobNumber      s              6s 0
      /Free

       jobName=jobNameIn;
       jobUser=jobUserIn;
       jobNumber=jobNumberIN;

       exec sql
        delete from wxp5p
        where P5jobN=:jobName And P5jobU=:jobUser And P5job#=:jobNumber;

      /End-Free

     P DelTopFive      E
       //===================================================================
       // ChkDevGroup - Determine if group is a Devereux group
       //===================================================================
     P ChkDevGroup     B                   Export
     D ChkDevGroup     PI              n
     D inCompany                      3  0 const
     D inGroup                        3  0 const

     C     keyWMCXL      Klist
     C                   Kfld                    inCompany
     C                   Kfld                    inGroup
      /Free
       //
       // Check to see if this is a Devereux group
       //
       GetClientID(outCltID);
       If outCltID  = '114';
          Chain keyWMCXL pcx00;
          If %Found(WMCXL) And CXdevF='Y';
             Return *On;
          EndIf;
       EndIf;

       Return *Off;

      /End-Free
     P ChkDevGroup     E
       //===================================================================
       // WriteAdjRmk - Write Adjuster Remark
       //===================================================================
     P WriteAdjRmk     B
     D WriteAdjRmk     PI

      /Free

       q#key=%EditC(CMco#:'X') + %EditC(CMfnd:'X') +
       %EditC(CMfYr:'X') + %EditC(CMcase:'X');
       q#cDat=%EditC(DateTo8(systemDate()):'X');
       q#user=q1User;
       q#pgm=q1pgm;
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

      /End-Free

     P WriteAdjRmk     E
       //===================================================================
       // UpdClmName - Updates Claim Name Across all Claims with SS#.
       //===================================================================
     P UpdClmName      B
     D UpdClmName      PI
     D claimPtrIn                      *
     D claimRrnIn                     9  0
     D claimInfo     e ds                  ExtName(wtClmDtl:rcdClmDt0)
     D                                     based(claimPtr)
     D claimPtr        s               *   inz
     D claimRrn        s              9  0
     D oldClmName      s             60    inz
     C*
     C     keyWMCMLB     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMss#

     C     keyWMCXL      Klist
     C                   Kfld                    C3co#
     C                   Kfld                    C3fnd

      /Free
         claimPtr=claimPtrIn;
         claimRrn=claimRrnIn;
         Chain keyWMCMLB pcm0b;
         If %Found(WMCMLB);
            DoU %Eof(WMCMLB);
               // Update other claims with Nmae if the name changed
               If scLsn<>C3lsN Or scFsn<>C3fsN Or scMid<>C3mid;
                  oldClmName = %Trim(C3lsN) + ', ' + %Trim(C3fsN);
                  If C3mid<> ' ';
                     oldClmName = %Trim(oldClmName) + ' ' + C3mid + '.';
                  Endif;
                  C3fsN = scFsn;
                  C3mid = scMid;
                  C3lsN = scLsn;
                  C3user=q1User;
                  C3pgm=q1Pgm;
                  C3time=TimeTo6(SystemTime());
                  C3dt=DateTo7(SystemDate());

                  Update pcm0b;

                  // Write Darts Remark
                  Chain keyWMCXL pcx00;
                  q#key=%EditC(C3co#:'X') + %EditC(C3fnd:'X') +
                    %EditC(C3fYr:'X') + %EditC(C3case:'X');
                  q#Comm='Old Claimant Name: ' +  oldClmName;
                  q#cDat=%EditC(DateTo8(systemDate()):'X');
                  q#dcId=*Blanks;
                  q#pnam='W0070R';
                  q#secl='000';
                  q#user=q1user;
                  q#pgm=q1pgm;
                  q#fold=*Blanks;
                  q#difl=*Blanks;
                  q#iPgm=Q1pgm;
                  q#fl01=*Blanks;
                  q#fl02=*Blanks;
                  q#rmCd=CXrcNc;
                  q#seq#='00000';
                  q#sbyl='N';
                  q#Com2=*Blanks;
                  q#apid=*Blanks;
                  q#idoc=*Blanks;
                  q#ique=*Blanks;
                  q#imgf=*Blanks;
                  q#ipth=*Blanks;
                  q#pidx=*Blanks;
                  q#qpty=*Blanks;
                  q#dctp=*Blanks;
                  q#csid=*Blanks;
                  q#code=*Blanks;
                  WriteDarts(q#key:q#comm:q#cDat:q#dcId:q#pNam:q#secl:q#user:
                   q#pgm:q#fold:q#diFl:q#iPgm:q#fl01:q#fl02:q#rmCd:q#seq#:
                   q#sbyl:q#com2:q#apId:q#iDoc:q#iQue:q#imgF:q#iPth:q#pIdx:
                   q#qPty:q#dcTp:q#csId:q#code);
               EndIf;
               ReadE keyWMCMLB pcm0b;
            EndDo;
         EndIf;
         Chain(n) claimRrn pcm0p;

      /End-Free

     P UpdClmName      E
       //===================================================================
       // UpdReqAddrs - Updates Check Requests with name/address changes
       //===================================================================
     P UpdReqAddrs     B
     D UpdReqAddrs     PI
     D claimPtrIn                      *
     D claimInfo     e ds                  ExtName(wtClmDtl:rcdClmDt0)
     D                                     based(claimPtr)
     D claimPtr        s               *   inz
     C*
     C     keyWMCMLB     Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMss#
     C*
     C     keyWARPLC     Klist
     C                   Kfld                    C3co#
     C                   Kfld                    C3fnd
     C                   Kfld                    C3fYr
     C                   Kfld                    C3case

      /Free
         claimPtr=claimPtrIn;

         Chain(n) keyWMCMLB pcm0b;
         If %Found(WMCMLB);
            DoU %Eof(WMCMLB);
               Chain keyWARPLC prp0c;
               If %Found(WARPLC);
                  DoU %Eof(WARPLC);

                     // Insure that payment is for the address being changed
                     If (CDadr1=RPadr1 And CDadr2=RPadr2 And
                         CDcty=RPcty And CDst=RPst And CDZip=RPzip) Or
                        (CDadr1=RPadr1 And CDadr2=RPadr2 And
                         CDctry=RPctry And CDadr3=RPadr3);

                        // Now arrange previous name same as payment record
                        If CDmid<>' ';
                           xxCnam=%Trim(CDfsn) + ' ' + %Trim(CDmid) + ' ' +
                                  %Trim(CDlsn);
                        Else;
                           xxCnam=%Trim(CDfsn) + ' ' + %Trim(CDlsn);
                          //*** Beg Add *** 05/05/25 **************************
                          //*** Fix the payee name when the middle name contains
                          //*** double blanks if the payee name was written to t
                          //*** file prior to fix implemented by ticket 118852
                           If RPpNam = %Trim(CDfsn) + '  ' + %Trim(CDlsn);
                             RPpNam = xxCnam;
                           Endif;
                          //*** End Add *** 05/05/25 **************************
                        Endif;

                        // OK to update this payment
                        If RPpNam=xxCnam;
                           RPadr1=scClmAdr1;
                           RPadr2=scClmAdr2;
                           RPadr3=scProvince;
                           RPcty=scClmCty;
                           RPst=scClmSt;
                           RPzip=scClmZip;
                           RPctry=scCountry;
                           RPpNam=xxCnam;

                           RPuser=q1User;
                           RPpgm=q1pgm;
                           RPtime=TimeTo6(SystemTime());
                           RPdt=DateTo7(SystemDate());

                           Update prp0c;
                        EndIf;
                     EndIf;
                     ReadE keyWARPLC prp0c;
                  EndDo;
               EndIf;
               ReadE(n) keyWMCMLB pcm0b;
            EndDo;
         EndIf;

      /End-Free

     P UpdReqAddrs     E
       //===================================================================
       // WrtAdrHist - Write Address History File
       //===================================================================
     P WrtAdrHist      B
     D WrtAdrHist      PI
     D  beforeAfter                   1    Const

      /Free

       Clear pah0p;

       AHco#=CMco#;
       AHfnd=CMfnd;
       AHfYr=CMfYr;
       AHcase=CMcase;
       AHbaFl=beforeAfter;
       AHlsn=CDlsn;
       AHfsn=CDfsn;
       AHmid=CDmid;
       AHadr1=CDadr1;
       AHadr2=CDadr2;
       AHadr3=CDadr3;
       AHcty=CDcty;
       AHst=CDst;
       AHzip=CDzip;
       AHctry=CDctry;
       AHpgm=q1Pgm;
       AHuser=q1User;
       AHtime=TimeTo6(SystemTime());
       AHdt=DateTo7(SystemDate());

       Write pah0p;

       If beforeAfter='B';
          ahWritten=*On;
       Else;
          ahWritten=*Off;
       EndIf;

      /End-Free

     P WrtAdrHist      E
     P GetDiaryUr      B

     d GetDiaryUr      PI            10
     d  strDesc                       3
      *
     d strUr           s             10
      /free

         Chain strDesc pup01;

         If %found (WMUPL1);
            strUr = Upusr;
         Else;
            strUr = *blanks;
         Endif;

         return strUr;

      /end-free

     P GetDiaryUr      E
