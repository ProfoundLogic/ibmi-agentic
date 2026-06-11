     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')

    h** ======================================================================================== **
    h/TITLE W9111R - Import Claims data from external system, update Fiserv Claims files.
     h option(*nodebugio:*srcstmt)
    h* ========================================================================================= **
    h*  DESCRIPTION:  This program reads the import file CLIMPORT & edits, updates/write records  *
    h*                to varius claim files. It will also generate a summary report and an        *
    h*                error exception report.                                                     *
    h*                This program can process up to five record formats for each claim.          *
    h*                                                                                            *
    h*              EDITS * * * * * * * * EDITS * * * * * * * EDITS * * * * * *                  *
    h*                                                                                            *
    h*                Program checks input parm @@DNLD for "Y", if so then no file, so list       *
    h*                  the error and end program.                                                *
    h*                                                                                            *
    h*                If first record is not a record type of 01, is error, read until hit next   *
    h*                  01 record, bypass processing of all record types until hit next 01 record.*
    h*                                                                                            *
    h*                If Update type is not "A", "C" or "D", is an invalid update type            *
    h*                                                                                            *
    h*                If find error in any record type, remainder of record types for that clm#   *
    h*                  will also be edited.                                                      *
     h*                                                                                            *
     h*  NOTE: *IN90 controls the Writing and Updating of the Claim Records, Is set off if         *
     h*              Testing is being Done.                                                        *
     h*                                                                                            *
     h*  Called from:  W9111C                                                                      *
     h*                                                                                            *
     h*  Created: 12/08/05 by C:132 for project: 139WAI                                            *
    h** ======================================================================================== **
    h* Revisions:                                                                                 *
     H*                                                               *
     H* 10/23/25 C:342 Project: 119988/150WGB (SUNZ)                  *
     H*                Check MMI date separately as it will not       *
     H*                populate in CMmmi if error occurred for R2acz  *
     H*
     H* 05/08/25 C:342 Project: 150WGB (SUNZ)                         *
     H*                Indemnity Data Call Implementation             *
     H*
     H* 05/23/24 C:300 Support: 115634 (SUNZ)                         *
     H*                Update WMFNP Maintenance fields while updating *
     H*                Next Available Claim Number                    *
     H*
     H* 11/03/22 C:62  Project: WC2106
     H*                Add group condition to deductible reversal
     H*
     H* 07/07/21 C:62  Project: WC2781
     H*                If called from WICLAIMS don't:
     H*                -Don't turn on *inlr
     H*                -Don't open QSYSPRT and print to it
     H*                -Add errors to json via SPIJSON/AddErrorMessageJson
     H*                -If errors found return 'E'
     H*
     H* 05/27/21 C:62  Project: 150WFG
     H*                Populate state claim#.
     H*
     H* 09/28/20 C:62  Project: 89WCQ
     H*                Allow dulicate checks.
     H*
     H* 09/24/20 C:62  Project: 89WCQ
     H*                Condition the claim adjuster load.
     H*
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 04/29/20 C:62  Project: 88846(146)
     H*                Assign key fields in AWW.
     H*
     H* 01/17/20 C:62  Project: 86695(150)
     H*                Use different SS# assignment.
     H*
     H* 10/31/19 C:62  Project: 84233(150)
     H*                Correct transfer year assignment
     H*
     H* 09/27/19 C:62  Project: 83158(146)
     H*                Correct chain to pep00.
     H*
     H* 08/22/19 C:62  Project: 82434(150)
     H*                Auto assign ss# if sent as blank
     H*
     H* 08/12/19 C:62  Project: 150WAU
     H*                -Prevent duplicates
     H*                -Process payments if negative even if from/to not sent
     H*                -Fix amount if negative for duplicate checking.
     H*
     H* 06/07/19 C:180 Support: 80064                                 *
     H*                Fix update without proper chain.               *
     H*
     H* 05/30/19 C:62  Project: 150WAU
     H*                Populate CMACL#
     H*
     H* 03/06/19 C:53  Support: 77387
     H*                Various issues
     H*
     H* 02/04/19 C:62  Project: 136WHX
     H*                Verify class code is associated with employer
     H*
     H* 01/22/19 C:62  Project: 136WHX
     H*                -Populate CMOCDS
     H*                -Create AWW History
     H*
     H* 09/13/18 C:62  Project: 150WAU
     H*                -If check# is blank then use 0.
     H*                -If Nature/Part/Cause is 1 char add leading zero
     H*                -Add U to valid Gender
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 12/17/15 C:180 SUPPORT 32115                                  *
     H*          C:306 (Retrofit) - Prevent dup demographic records.  *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     H*  05/23/14 C:298  Priss# 121 : SY195
     H*                  Modified to correctly update UNCN path when there
     H*                  is no document in DART remark.                                            *
     H*
     H* 05/28/13  C:062  support: WC114 82381                         *
     H*                  Populate more fields in SMARP                *
     H*                                                               *
     H* 10/17/11  C:279  Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H* 06/13/11  C:062  support: 76998                               *
     H*                  Correct call of WA037R04                     *
     H*                                                               *
     H* 05/23/11  C:062  support: 71456                               *
     H*                  Allow update of claim demographic.           *
     H*                                                               *
     H* 02/02/11  C:246  support: 75676 - CLT 132                     *
     H*                  Added new parm to pgm WA037R04 to avoid record
     H*                  lock.                                        *
     H*                                                               *
     H* 06/28/10  C:251  Project: 132WOH                              *
     H*                  Duplicate check validation edit              *
     H*                                                               *
     H* 04/20/10  C:189  Project: 114WWA                              *
     H*                  Populate the Co Claim number to CMACL#       *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*
     h*  10/21/09 C:058  Project: Support Seq# 70333  Change addition of DARTs to include assign-
     h*       ment of the sequence #, based on last sequence number used for that same key in the
     h*       production file.  Record types 07 and 08; add logical SDCMLI to interrogate file.
     h*                                                                                            *
     h*  04/15/09 C:058  Project: Support Seq# 66926  (1) Change condition to ADD record from 'OR'
     h*       to 'AND haveClaimant = ON'         (2) Remove refreshing of Division from claim
     h*       input data; substitute zeros only if blank
     h*                                                                                            *
     h*  08/07/09 C:53   Project: 132WMN                                                           *
     h*                  Allow Adjustment Transactions to Claims Backfill                          *
     h*                  for CLT132                                                                *
     h*                                                                                            *
     h*  04/15/09 C:058  Project: 132WKW Seq# 66926  Condition execution in SR writeWMCDP using    *
     h*                     haveClaimant flag (already there, set, but not used). This separates   *
     h*                     the claimant and claim file process controls: previously governing of  *
     h*                     additions and updates was by the error02 flag for both files (WMCMP    *
     h*                     and WMCDP) and additions were unpredictable.                           *
     h*                                                                                            *
     h*  04/15/09 C:058  Project: 132WKW Seq# 66926  Free unexecutable code: save SS# to use as    *
     h*                     comparator to determine if SS# (not accident date) is being updated.   *
     h*                     This logic comes right after updating the file with the input data, so *
     h*                     they were never different, and add record process was never executed.  *
     h*                                                                                            *
     h*  04/08/09 C:058  Project: 132WKW Seq# 67761   Bypass use of WMRPP(carrier policy) division *
     h*                     number when 06 Rec division change is done. (File contains multiple    *
     h*                     division records with same policy number key: always gets first one,   *
     h*                     therefore usually gets division zero).                                 *
     h*                                                                                            *
     h*  10/24/08 C:220  Project: 132WLN                                                           *
     h*                  Expand Check # Field                                                      *
     h*                  To verify check no numeric or not                                         *
     h*                                                                                            *
     h*  10/23/08 C:220  Project: 132WLM                                                           *
     h*                  Add Group/Non-Group Check Type to Claims backfill                         *
     h*                                                                                            *
     h*  08/13/08 C:183  PRISS# 12: 81WZU                                                          *
     h*                  Changed the logic for PRISS reported, PRISS# 12.                          *
     h*                                                                                            *
     h*  08/06/08 C:058  Project: 132WKW Seq# 64308 64223                                          *
     h*           Change input header data to equal the claim master file value earlier in flow,   *
     h*             eliminating message that no new data was not found in 06 record.               *
     h*                                                                                            *
     h*                                                                                            *
     h*  07/23/08 C:136  Project: 81WZU                                                            *
     h*                  Modify for new entry parm. It will control whether any files get updated  *
     h*                  or written if ANY errors exist in the Claim Import file or not.           *
     h*                                                                                            *
     h*  07/14/08 C:136 Project: 132WKO PRISS 2                                                    *
     h*                 Expand count fields to 6,0 per client request. Changes not commented.      *
     h*                                                                                            *
     h*  06/04/08  C: 65   Project: 81WZF                                                          *
     h*                    Translate the 1 digit fraudalent code into the ncci 2 digit code.       *
     h*                                                                                            *
     h*  05/21/08  C: 15   Project: 132WKW                                                         *
     h*                    Added code to ignore the claim update flag (r1uptp) and check WMCMP to  *
     h*                    determine if the claim is an "add" or "update".                         *
     h*                                                                                            *
     h*  04/11/08  C: 190  Project: 132WKO                                                         *
     h*                   -Changed pgm to load the Claim Cackfill Acknowledgement File, when the   *
     h*                    last record('09' record) in CLIMPORT is processed.                      *
     h*                                                                                            *
     h*  03/26/08  C: 15   Project: 132WKW                                                         *
     h*                    Changed pgm to not prevent a new claim from being created if the edit   *
     h*                    that compares the claimant's name to an existing name with the same SS# *
     h*                    is envoked.  The error will still be listed on the error report.        *
     h*                                                                                            *
     h*  03/17/08  C: 15   Support: WC132/62209                                                    *
     H*                    Added code to load the "percent impairment" in record '02'.             *
     h*                                                                                            *
     h*  02/06/08  C: 15   Support: WC132/62209                                                    *
     h*                    Changed code to use pol# in record 06 correctly.                        *
     h*                                                                                            *
     h*  01/30/08  C: 15   Support: WC132/62112                                                    *
     h*                    Added code to load missing fields in SDCMP.                             *
     h*                                                                                            *
     h*  12/11/07  C: 15   Project: 81WZF                                                          *
     h*                     - Priss# 22 : Added PV1 condition to check edit.                       *
     h*                                                                                            *
     h*  11/08/07  C: 53   Project: 132WJK                                                         *
     h*                    Added code to import notes
     h*                    Use imported value for CMCASE when CLT132                               *
     h*                                                                                            *
     h*  09/20/07  C: 15   Project: 132WJK                                                         *
     h*                    Added code to load new fields from the import file.                     *
     h*                                                                                            *
     h*  08/01/07  C: 191  Project: 81WZF                                                          *
     h*                     - Priss# 12 : Duplicate check edit                                     *
     h*                     - Priss# 15 : Accident date update                                     *
     h*                                                                                            *
     h*  06/12/07  C: 191  Project: 81WZF                                                          *
     h*                     - Move to base                                                         *
     h*                     - Remove Vadum file ref, add CLIMPORT                                  *
     h*                     - Priss# 12 : Softcode reserve code validation                         *
     h*                                                                                            *
     h*  01/20/06  C: 132  Project: wa139 - 51037                                                  *
     h*                    Change code so if get a hit on chain to WMCM15 logical, use that SS#    *
     h*                    and Accident Date for the chain to WMCMLB for update, as per LK.        *
     h*                                                                                            *
    h** ======================================================================================== **
     h*                                                                                            *
     f* Incoming Flat File
     fclimport  if   e             disk    USROPN
      *
      * Routing file
     fsmorl     if   e           k disk
      *
      * Claim Transfer file
     fwactp     o    e             disk
      *
      * Claim Deduct Detail file  (co#/fnd/fyr/case)
     fwdddl     if   e           k disk
      *
      * Claim Payment Detail file  (co#/fnd/fyr/case/seq#)
      *** Beg Chg *** 05/08/25 ***********************************
     f*** wdpal     if a e           k disk
     fwdpal     if a e           k disk    InfDS(dbfPPAds)
      *** End Chg *** 05/08/25 ***********************************
      *
      * Claimnant Demographic file (SS#)
     fwmcdl     uf a e           k disk
      *
      * Claim Check View file  (co#/fnd/fyr/case/seq#)
     fwdrpl3    if a e           k disk             rename(prp01:prp03)
      *
      * Claim Status History file  (co#/fnd/fyr/case/efdt/time)
     fwmchl     if a e           k disk
      *
      * Claim Master file (co#/fnd/pds#)
     fwmcml15   if   e           k disk    RENAME(PCM01:PCM15)
      *
      * Claim Master file (co#/ss#/acdt)
     f*** Begin chg *** 05/12/08 ****************************************************
     f*******WMCMLB    uf a e           k disk
     fWMCMLB    if   e           k disk
     f*** End chg ***** 05/12/08 ****************************************************
      * Claim Master file (co#/fnd/fyr/case)
     f*** Begin chg *** 05/12/08 ****************************************************
     f*******WMCMl1    if   e           k disk    rename(pcm01:pcm02)
     fWMCMl1    uf a e           k disk    rename(pcm01:pcm02)
     f*** End chg ***** 05/12/08 ****************************************************
      *
      * Employer Master file (co#/fnd/emp#/div)
     fwmeml     if   e           k disk
     f*** Begin chg *** 02/04/19 ****************************************************
      * Employer Payroll file (co#/fnd/emp#/div/fyr/class)
     f*** Begin chg *** 03/06/19 ****************************************************
     f***wmepl     if   e           k disk    rename(pep01:pep00)
     fwmepl1    if   e           k disk    rename(pep01:pep00)
     f*** End chg *** 03/06/19 *******************************************  *********
     f*** End chg ***** 02/04/19 ****************************************************
      *
      * Employer Detail file (co#/fnd/emp#/div/pfdt)
     fwdell2    if   e           k disk
      *
      * Group Master file (co#/fnd)
     fwmfnl     uf   e           k disk
      *
      * Claim Legal Master file  (co#/fnd/fyr/case)
     fWMLLL     uf a e           k disk
      *
      * Reserve History file  (co#/fnd/fyr/case/efdt)
     fwmrhl     if a e           k disk
      *
      * Carrier/State Reporting (Co#/Cap#/Fnd)
     fWMRPl3    if   e           k disk
      *** Begin add *** 11/08/07 ****************************************************
     fsdcmle    uf a e           k disk             rename(pcm01:scm01)
     f                                              prefix(S)
     fsmarl1    uf a e           k disk
      *** End add *** 11/08/07 *******************************************  *********
      *** Begin add *** 04/11/08 ****************************************************
      *
      * Claim Backfill Acknowledgement File.
     fwzcbp     if a e           k disk
      *** End add *** 04/11/08 *******************************************  *********
      *
      *** Begin add *** 10/21/09 ****************************************************
      * DARTS notes file SDCMP by Key + Sequence# UNIQUE
     fsdcmli    if a e           k disk             rename(pcm01:scm02)
      *** End add *** 10/21/09 *******************************************  *********
     f*** Begin Add *** 01/22/19 *****************************************
     fwmawl     if a e           k Disk    Rename(paw01:paw00)
     fwdrbl     if   e           k Disk    ReName(prb01:prb00)
     f*** End   Add *** 01/22/19 *****************************************
      *
      * Loss Coverage Code file   (co#/tbid/key)
     fwtgtl     if   e           k disk
      *
      * Table files (co#/key)
     fwtabl     if   e           k disk    rename(ppa01:ppa02)
     f                                     ignore(pat01)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pbs01)
     f*******                              ignore(pb201)                        BR2 Codes
     f                                     ignore(pb701)
     f*******                              ignore(pca01)                        Cause Codes
     f                                     ignore(pcc01)
     f                                     ignore(pcn01)
     f                                     ignore(pcs01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f*******                              ignore(pna01)                        Nature Codes
     f                                     ignore(pnc01)
     f*******                              ignore(ppa01)                        Part Codes
     f*******                              ignore(ppc01)                        Pmt codes
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f*********                            ignore(prt01)                        Reserve Type Codes
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
      ****** Beg add *** 05/08/25 ******************************************************
      * Work history file
     fwmwhl     if a e           k Disk    Rename(pwh01:pwh00)
      * Employer detail file
     fwdell     if   e           k Disk    Rename(pel01:pel00)
      * Group Auxiliary File
     fwdf2l     if   e           k Disk    Rename(pf201:pf200)
      ****** End add *** 05/08/25 ******************************************************
      * Print files
      *
     f*** Begin Chg *** 07/07/21 *******************************************
     f**********qsysprt   o    f  132        printer oflind(*inoa)              error report
     f**********qsysprt1  o    f  132        printer oflind(*inob)              update report
     fqsysprt   o    f  132        printer oflind(*inoa) usropn                 error report
     fqsysprt1  o    f  132        printer oflind(*inob) usropn                 update report
     f*** End   Chg *** 07/07/21 *******************************************
      *=============================================================================================
      * Stand alone fields
      *=============================================================================================
     d @@que           s             10
     d @@rpdt          s              7
     d @@dnld          s              1
     D companyNm       s             60
     d recCount        s              7  0 inz(*zero)
      *** BEG CHG *** 07/14/08 *******************************************  *********
     d********* recOther        s              5  0 inz(*zero)
     d recOther        s              6  0 inz(*zero)
     d********* recCnt01        s              5  0 inz(*zero)
     d recCnt01        s              6  0 inz(*zero)
     d********* recCnt02        s              5  0 inz(*zero)
     d recCnt02        s              6  0 inz(*zero)
     d********* recCnt03        s              5  0 inz(*zero)
     d recCnt03        s              6  0 inz(*zero)
     d********* recCnt04        s              5  0 inz(*zero)
     d recCnt04        s              6  0 inz(*zero)
     d********* recCnt05        s              5  0 inz(*zero)
     d recCnt05        s              6  0 inz(*zero)
     d********* recCnt06        s              5  0 inz(*zero)
     d recCnt06        s              6  0 inz(*zero)
     d********* rcd01Cnt        s              5  0 inz(*zero)
     d rcd01Cnt        s              6  0 inz(*zero)
     d********* rcd02Cnt        s              5  0 inz(*zero)
     d rcd02Cnt        s              6  0 inz(*zero)
     d********* rcd03Cnt        s              5  0 inz(*zero)
     d rcd03Cnt        s              6  0 inz(*zero)
     d********* rcd04Cnt        s              5  0 inz(*zero)
     d rcd04Cnt        s              6  0 inz(*zero)
     d********* rcd05Cnt        s              5  0 inz(*zero)
     d rcd05Cnt        s              6  0 inz(*zero)
     d********* rcd06Cnt        s              5  0 inz(*zero)
     d rcd06Cnt        s              6  0 inz(*zero)
     d********* rcd01Err        s              5  0 inz(*zero)
     d rcd01Err        s              6  0 inz(*zero)
     d********* rcd02Err        s              5  0 inz(*zero)
     d rcd02Err        s              6  0 inz(*zero)
     d********* rcd03Err        s              5  0 inz(*zero)
     d rcd03Err        s              6  0 inz(*zero)
     d********* rcd04Err        s              5  0 inz(*zero)
     d rcd04Err        s              6  0 inz(*zero)
     d********* rcd05Err        s              5  0 inz(*zero)
     d rcd05Err        s              6  0 inz(*zero)
     d********* rcd06Err        s              5  0 inz(*zero)
     d rcd06Err        s              6  0 inz(*zero)
     d********* rcdOtErr        s              5  0 inz(*zero)
     d rcdOtErr        s              6  0 inz(*zero)
     d rcdErCnt        s              7  0 inz(*zero)
     d********* rcd01SKP        s              5  0 inz(*zero)
     d rcd01SKP        s              6  0 inz(*zero)
     d********* rcd02Skp        s              5  0 inz(*zero)
     d rcd02Skp        s              6  0 inz(*zero)
     d********* rcd03Skp        s              5  0 inz(*zero)
     d rcd03Skp        s              6  0 inz(*zero)
     d********* rcd04Skp        s              5  0 inz(*zero)
     d rcd04Skp        s              6  0 inz(*zero)
     d********* rcd05Skp        s              5  0 inz(*zero)
     d rcd05Skp        s              6  0 inz(*zero)
     d********* rcd06Skp        s              5  0 inz(*zero)
     d rcd06Skp        s              6  0 inz(*zero)
     d********* rcdOtSkp        s              5  0 inz(*zero)
     d rcdOtSkp        s              6  0 inz(*zero)
      *** END CHG *** 07/14/08 *******************************************  *********
     d rcdErSkp        s              7  0 inz(*zero)
     d rcdCount        s              7  0 inz(*zero)
     d recType         s              2    inz
      *** Begin add *** 11/08/07 ****************************************************
      *** Begin CHG *** 07/14/08 ****************************************************
     d********* recCnt07        s              5  0 inz(*zero)
     d recCnt07        s              6  0 inz(*zero)
     d********* rcd07Cnt        s              5  0 inz(*zero)
     d rcd07Cnt        s              6  0 inz(*zero)
     d********* rcd07Err        s              5  0 inz(*zero)
     d rcd07Err        s              6  0 inz(*zero)
     d********* rcd07Skp        s              5  0 inz(*zero)
     d rcd07Skp        s              6  0 inz(*zero)
     d rcd07           s              1n   inz(*off)
     d********* recCnt08        s              5  0 inz(*zero)
     d recCnt08        s              6  0 inz(*zero)
     d********* rcd08Cnt        s              5  0 inz(*zero)
     d rcd08Cnt        s              6  0 inz(*zero)
     d********* rcd08Err        s              5  0 inz(*zero)
     d rcd08Err        s              6  0 inz(*zero)
     d********* rcd08Skp        s              5  0 inz(*zero)
     d rcd08Skp        s              6  0 inz(*zero)
      *** End   CHG *** 07/14/08 ****************************************************
     d rcd08           s              1n   inz(*off)
     d error07         s              1n   inz(*off)
     d error08         s              1n   inz(*off)
     d h7cdat          s                   like(SCMCDAT)
     d h7trdt          s                   like(SCMTRDT)
     d h7dcdt          s                   like(SCMTRDT)
     d kdkey           s                   like(ARKEY)
     d kdseq#          s                   like(ARSEQ#)
     d kdasq#          s                   like(ARASQ#)
      *** End add *** 11/08/07 *******************************************  *********
     c*** Begin add *** 04/15/09 ********************************************************
     d savss#          s                   like(CMSS#)
     c*** End add ***** 04/15/09 ********************************************************
     c*** Begin add *** 02/02/11 ********************************************************
     d @@cmemp#        s                   like(CMEMP#)
     c*** End add ***** 02/02/11 ********************************************************
     d*** Begin Add *** 06/13/11 *******************************************
     d @@emp#          s                   like(CMEMP#)
     d*** End   Add *** 06/13/11 *******************************************
      *** Begin add *** 04/11/08 ****************************************************
      *** Begin Chg *** 07/14/08 ****************************************************
     d*recCnt09        s              5  0 inz(*zero)
     d recCnt09        s              6  0 inz(*zero)
     d*recCnt10        s              5  0 inz(*zero)
     d recCnt10        s              6  0 inz(*zero)
      *** End   Chg *** 07/14/08 ****************************************************
      *** End add *** 04/11/08 *******************************************  *********
     d*** Begin Add *** 09/13/18 ***********************************
     d strCheck#       s              7
     d*** End   Add *** 09/13/18 ***********************************
      *
     d userName        s             50
     d UPUSNM          s                   like(userName)
     d xxque           s                   like(@@que)
     d group#          s                   like(CMFND)
     d company         s                   like(CMCO#)
     d xxclm#          s                   like(r1clm#)
     d @@trtp          s              1    inz('C')
     d fnrrn           s              9  0
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d usaRptDt        s               d   datfmt(*usa)
     d rptDt8a         s              8
     d rptDt8n         s              8  0
     d cnxDt8a         s              8
     d cnxDt8n         s              8  0
     d isoPostDate     s               d   datfmt(*iso)
     d isotrDate       s               d   datfmt(*iso)
     d x7Date          s              7  0
     d x7transDate     s              7  0
     d x7acdt          s                   like(CMACDT)
     d zzdate          s              8  0
     d zztime          s              6  0 inz
     d sys_date        s               d   datfmt(*usa)
     d sys_time        s               t   timfmt(*hms:)
     d reportDate      s              7s 0
     d payAmtTot       s             13  2 inz(*zero)
     d rsvAmtTot       s             15  2 inz(*zero)
     d suRhTot         s             15  2 inz(*zero)
      *** beg add ***** 07/23/08 *******************************************************
     d writeRecords    s              1
     d upDate          s              1    inz('Y')
      *** end add ***** 07/23/08 *******************************************************
      *** beg add ***** 08/13/08 *******************************************************
     d loopCount       s              1  0 inz(0)
      *** end add ***** 08/13/08 *******************************************************

     d  suT01          s             11  2 inz
     d  suT02          s             11  2 inz
     d  suT03          s             11  2 inz
     d  suT04          s             11  2 inz
     d  suT05          s             11  2 inz
     d  suT06          s             11  2 inz
     d  suT07          s             11  2 inz
     d  suT08          s             11  2 inz
     d  suT09          s             11  2 inz
     d  suT10          s             11  2 inz
     d suTOT           s             11  2 inz(*zero)
      *
     d errCount        s              6  0 inz(*zero)
     d newPolicyNbr    s              1n   inz(*off)
     d badDATA         s              1n   inz(*off)
     d errorFlag       s              1n   inz(*off)
     d error01         s              1n   inz(*off)
     d error02         s              1n   inz(*off)
     d error03         s              1n   inz(*off)
     d error04         s              1n   inz(*off)
     d error05         s              1n   inz(*off)
     d error06         s              1n   inz(*off)
     c*** Begin add *** 03/26/08 ******************************************************
     d errcnam         s              1n   inz(*off)
     c*** End add ***** 03/26/08 ******************************************************
     d rcd01           s              1n   inz(*off)
     d rcd02           s              1n   inz(*off)
     d rcd03           s              1n   inz(*off)
     d rcd04           s              1n   inz(*off)
     d rcd05           s              1n   inz(*off)
     d rcd06           s              1n   inz(*off)
     d addClaim        s              1n   inz(*off)
     d chgClaim        s              1n   inz(*off)
     d delClaim        s              1n   inz(*off)
     d haveClaimant    s              1n   inz(*off)
     d xxfil#          s                   like(ORFIL#)
      *
     d kkco#           s                   like(CMCO#)
     d kkfnd           s                   like(CMFND)
     d kkemp#          s                   like(EMEMP#)
     d kkdiv           s                   like(EMDIV)
     d kkfyr           s                   like(CMFYR)
     d kkpds#          s                   like(CMPDS#)
     d kkcap#          s                   like(RPCAP#)
     d kkCASE          s                   like(CMCASE)
     d kkSEQ#          s                   like(PASEQ#)
     d kkacdt          s                   like(CMACDT)
     d kktbid          s              4
     d kkkey           s             10
     d xxdiv           s                   like(EMDIV)
     d xxpds#          s                   like(CMPDS#)
     d xxemp           s                   like(EMEMP#)
     d xxefdt          s                   like(rhefdt)
     d xxfnd           s                   like(EMFND)
     d xxfyr           s                   like(CMFYR)
     d xxCASE          s                   like(CMCASE) INZ(*zeros)
     d xxSEQ#          s                   like(PASEQ#)
     d xxcall          s              1    inz(' ')
     d xxamtb          s                   like(DDAMTB)
     D ORTITL1         s                   like(ORTITL)
     D ORTITL2         s                   like(ORTITL)
     d line            s            132
     d cymdRptDt       s              7  0
     d ss#flag         s               n   inz(*off)
     d kkss#           s                   like(CMSS#)
     d xxfeinc         s              9
     d xxmodc          s              3
     d xxtrdt          s                   like(patrdt)
     d xxpodt          s                   like(papodt)
     d xxfrdt          s                   like(pafrdt)
     d xxtodt          s                   like(patodt)
     d Up              c                   'ABCDEFGHIJKLMNOPQRS-
     d                                     TUVWXYZ'
     d Lo              c                   'abcdefghijklmnopqrs-
     d                                     tuvwxyz'
     d xr2lsn          s                   like(cdfsn)
     d xr2fsn          s                   like(cdfsn)
     d xcdlsn          s                   like(cdfsn)
     d xcdfsn          s                   like(cdfsn)
      *
     d*** Beg Add *** 06/28/10 *************************************
     d wamt            s                   like(r3amt)
     d weck#           s                   like(paeck#)
     d*** End Add *** 06/28/10 *************************************
      *=============================================================================================
      * constants
      *=============================================================================================
     d digits          c                   '0123456789'
     d title           c                   'Claims Import Error Report'
     d title1          c                   'Claims Import Summary Report'
     d OpnClaim        c                   'Import Open CLaim History Record'
     d ClsClaim        c                   'Import Closed Claim History Record'
     d onAddClm        c                   'Import Add with New Claim Record'
     d rsvHistory      c                   'Import Reserve Change History -
     d                                      Record'
      *** Beg Add *** 01/28/10 ****************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D*** Begin Add *** 07/07/21 *******************************************
     d* Check Program is on Call Stack
     d CheckCallStack  pr                  ExtPgm('SRCHKPGM')
     d   chrProgram                  10    Const
     d   chrOnStack                   1
     D*** End   Add *** 07/07/21 *******************************************
     D/copy SPrRtvBrnd
     D*** Begin Add *** 07/07/21 *******************************************
     D/copy SPrJson
     D/Include yajl_h
     D*** End   Add *** 07/07/21 *******************************************
      *** End Add *** 01/28/10 ****************************************
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
      //***  Beg   Add  ***  05/08/25  ************************************
     D/Copy sprmodule
     D/Copy wpridc
     D intRRN          S              9  0 inz

     D isNewIdc        s               n   inz(*Off)
     D isIdcTrnOk      s               n   inz(*Off)
     D qqCo#1          s              3  0
     D qqCar1          s              5
     D qqPolId1        s             18
     D qqPolEfDt1      s              8
     D qqClmId1        s             12
     D qqAcDt1         s              8
     D qqCo#2          s              3  0
     D qqCar2          s              5
     D qqPolId2        s             18
     D qqPolEfDt2      s              8
     D qqClmId2        s             12
     D qqAcDt2         s              8
     D qqFyr2          s                   LIKE(ELfyr) INZ
     D CTXjrst         s                   LIKE(CMjrst)
     D strCaP#         s             25A   inz
     D intPos          s              5  0 INZ
     D xxDate          s                   LIKE(WHdate)
      //***  End   Add  ***  05/08/25  ************************************
      *=============================================================================================
      * Arrays
      *=============================================================================================
      *** Begin chg *** 11/08/07 ****************************************************
     d*err*********    s             60    dim(35) ctdata perrcd(1)             error msg array
     d err             s             60    dim(40) ctdata perrcd(1)             error msg array
      *** End chg *** 11/08/07 *******************************************  *********
     d digitCheck      s              3  0 dim(4)
     d*** Begin Add *** 07/07/21 *******************************************
     d chrOnStack      s              1    Inz(' ')
     d jobEnding       s               n   Inz(*Off)
     d errorsFound     s               n   Inz(*Off)
     d*** End   Add *** 07/07/21 *******************************************
     d*******/Eject
      *=============================================================================================
      * Data structures
      *=============================================================================================
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

      *** Begin add *** 11/08/07 ****************************************************
     d                 ds
     d dskey                         40
     d  dsco#                         3  0 overlay(dskey)
     d  dsfnd                         3  0 overlay(dskey:4)
     d  dsfyr                         3  0 overlay(dskey:7)
     d  dscase                        7  0 overlay(dskey:10)
      *** End add *** 11/08/07 *******************************************  *********

     d errorFields     ds
     d  rec_type                      7
     d  policy#                      18
     d  ErrValue                     33
     d  grp#                          3
     d  emp#                          9
     d  div                           5
     d  errReason                    60

     d*** Beg   Add *** 05/08/25 *******************************************
     d dbfPPAds        DS
     d  dbPPArrn             397    400B 0
     d*** End   Add *** 05/08/25 *******************************************
      *
      * Incoming Record format Data Structures
     d                 ds                                                       X-REF Claim Numbers
     d  R1Data                      733                                         Record 01
     d  R1fnd                  3      5
     d  R1emp#                 6     14
     d  R1div                 15     19
     d  R1ss#                 20     28
     d  R1acdt                29     36
     d  R1clm#                37     76
      *** Begin add *** 11/08/07 ****************************************************
     d  R1clm7                40     46
      *** End add *** 11/08/07 *******************************************  *********
     d  R1cap#                77     96
     d  R1uptp                97     97
     d*** BEG CHG ****** 06/12/07 **********************************************
     d  R1fyr                 98    100
     d  R1case               101    107
     d****  R1fill                98    733
     d  R1fill               108    733
     d*** END CHG ****** 06/12/07 **********************************************

     d                 ds                                                       Clm info & reserves
      *** Begin chg *** 09/20/07 ****************************************************
     d******  R2Data                      733                                         Record 02
      *** Beg chg *** 05/08/25 ****************************************************
     d********R2Data                      800                                         Record 02
     d  R2Data                      900                                         Record 02
      *** End chg *** 05/08/25 ****************************************************
      *** End chg ***** 09/20/07 ****************************************************
     d  R2fnd                         3    overlay(R2Data:3)
     d  R2clm#                       40    overlay(R2Data:6)
     d  R2fsn                        15    overlay(R2Data:46)
     d  R2lsn                        30    overlay(R2Data:61)
     d  R2mid                         1    overlay(R2Data:91)
     d  R2adr1                       40    overlay(R2Data:92)
     d  R2adr2                       40    overlay(R2Data:132)
     d  R2cty                        25    overlay(R2Data:172)
     d  R2st                          2    overlay(R2Data:197)
     d  R2zip                         9    overlay(R2Data:199)
     d  R2ctry                        3    overlay(R2Data:208)
     d  R2adr3                       40    overlay(R2Data:211)
     d  R2sex                         1    overlay(R2Data:251)
     d  R2msts                        1    overlay(R2Data:252)
     d  R2dtob                        8    overlay(R2Data:253)
     d  R2ltfl                        1    overlay(R2Data:261)
     d  R2stat                        1    overlay(R2Data:262)
     d  R2acds                       60    overlay(R2Data:263)
     d  R2acd2                       60    overlay(R2Data:323)
     d  R2natr                        2    overlay(R2Data:383)
     d  R2part                        2    overlay(R2Data:385)
     d  R2caus                        2    overlay(R2Data:387)
     d  R2lscv                        2    overlay(R2Data:389)
     d  R2covt                        2    overlay(R2Data:391)
     d  R2jrst                        2    overlay(R2Data:393)
     d  R2rcvd                        8    overlay(R2Data:395)
     d  R2rpdt                        8    overlay(R2Data:403)
     d  R2dod                         8    overlay(R2Data:411)
     d  R2dept                       10    overlay(R2Data:419)
     d  R2mclm                        7    overlay(R2Data:429)
     d  R2clas                        5  0 overlay(R2Data:436)
     d  R2ocds                       40    overlay(R2Data:441)
     d  R2aww                         9  2 overlay(R2Data:481)
     d  R2hidt                        8    overlay(R2Data:490)
     d  R2dtlw                        8    overlay(R2Data:498)
     d  R2lump                        1    overlay(R2Data:506)
     d  R2mtst                        2    overlay(R2Data:507)
     d  R2frau                        1    overlay(R2Data:509)
     d  R2litg                        1    overlay(R2Data:510)
     d  R2cvfl                        1    overlay(R2Data:511)
     d  R2subr                        1    overlay(R2Data:512)
     d  R2dny                         1    overlay(R2Data:513)
     d  R2mctp                        2    overlay(R2Data:514)
     d  R2vorc                        1    overlay(R2Data:516)

     d  R2rt01                        2    overlay(R2Data:517)
     d  R2rr01                        9  2 overlay(R2Data:519)
     d  R2ar01                        9  2 overlay(R2Data:528)
     d  R2rt02                        2    overlay(R2Data:537)
     d  R2rr02                        9  2 overlay(R2Data:539)
     d  R2ar02                        9  2 overlay(R2Data:548)
     d  R2rt03                        2    overlay(R2Data:557)
     d  R2rr03                        9  2 overlay(R2Data:559)
     d  R2ar03                        9  2 overlay(R2Data:568)
     d  R2rt04                        2    overlay(R2Data:577)
     d  R2rr04                        9  2 overlay(R2Data:579)
     d  R2ar04                        9  2 overlay(R2Data:588)
     d  R2rt05                        2    overlay(R2Data:597)
     d  R2rr05                        9  2 overlay(R2Data:599)
     d  R2ar05                        9  2 overlay(R2Data:608)
     d  R2rt06                        2    overlay(R2Data:617)
     d  R2rr06                        9  2 overlay(R2Data:619)
     d  R2ar06                        9  2 overlay(R2Data:628)
     d  R2rt07                        2    overlay(R2Data:637)
     d  R2rr07                        9  2 overlay(R2Data:639)
     d  R2ar07                        9  2 overlay(R2Data:648)
     d  R2rt08                        2    overlay(R2Data:657)
     d  R2rr08                        9  2 overlay(R2Data:659)
     d  R2ar08                        9  2 overlay(R2Data:668)
     d  R2rt09                        2    overlay(R2Data:677)
     d  R2rr09                        9  2 overlay(R2Data:679)
     d  R2ar09                        9  2 overlay(R2Data:688)
     d  R2rt10                        2    overlay(R2Data:697)
     d  R2rr10                        9  2 overlay(R2Data:699)
     d  R2ar10                        9  2 overlay(R2Data:708)
     d  R2dedr                        9  2 overlay(R2Data:717)
     d  R2idt                         8    overlay(R2Data:726)
      *** Begin add *** 09/20/07 ****************************************************
     d  R2cata                        2    overlay(R2Data:734)
     d  R2lsrv                        2    overlay(R2Data:736)
     d  R2ncci                        2    overlay(R2Data:738)
     d  R2clex                        3    overlay(R2Data:740)
     d  R2supr                        3    overlay(R2Data:743)
     d  R2ctg1                       10    overlay(R2Data:746)
      *** End add ***** 09/20/07 ****************************************************
     d*** Begin add *** 03/17/08 ****************************************************
     d  R2ipct                        5    overlay(R2Data:756)
     d*** End add ***** 03/17/08 ****************************************************
     d*** Begin Add *** 05/27/21 *******************************************
     d  R2jrc#                        9    overlay(R2Data:761)
     d*** End   Add *** 05/27/21 *******************************************
     d*** Beg   Add *** 05/08/25 *******************************************
     d  R2#dep                        3  0 overlay(R2Data:770)
     d  R2IndClmCd                    1    overlay(R2Data:773)
     d  R2ests                        2    overlay(R2Data:774)
     d  R2mmi                         8  0 overlay(R2Data:776)
     d  R2wmth                        2    overlay(R2Data:784)
     d  R2loec                        5  2 overlay(R2Data:786)
     d  R2edis                        5  2 overlay(R2Data:791)
     d  R2mdex                        1    overlay(R2Data:796)
     d  R2tdbe                        1    overlay(R2Data:797)
     d  R2losscd                      2  0 overlay(R2Data:798)
     d  R2prww                        7  2 overlay(R2Data:800)
     d  R2AST                         2    overlay(R2Data:807)
     d  R2idcacd                      2    overlay(R2Data:809)
     d  R2rtnwrk                      8  0 overlay(R2Data:811)
     d  R2acz                         9    overlay(R2Data:819)
     d*** End   Add *** 05/08/25 *******************************************

     d                 ds                                                       Claim Payment Info
     d  R3Data                      733                                         Record 03
     d  R3fnd                         3    overlay(R3Data:3)
     d  R3clm#                       40    overlay(R3Data:6)
     d  R3trdt                        8    overlay(R3Data:46)
     d  R3type                        1    overlay(R3Data:54)
     d  R3recd                        2    overlay(R3Data:55)
     d  R3br2                         2    overlay(R3Data:57)
     d  R3chk#                        7  0 overlay(R3Data:59)
     d  R3amt                         9  2 overlay(R3Data:66)
     d  R3amts                        1    overlay(R3Data:75)
     d  R3pnam                       40    overlay(R3Data:76)
     d  R3podt                        8    overlay(R3Data:116)
     d  R3frdt                        8    overlay(R3Data:124)
     d  R3todt                        8    overlay(R3Data:132)
     d*** BEG CHG ****** 06/12/07 ******************************************************************
     d****  R3link                       10    overlay(R3Data:140)
     d  R3link                       12    overlay(R3Data:140)
     d*** BEG ADD ****** 10/23/08 ******************************************************************
     d  R3pgtp                        1    overlay(R3Data:152)
     d*** END ADD ****** 10/23/08 ******************************************************************
     d*** BEG ADD ****** 10/24/08 ******************************************************************
     d  R3eck#                       10    overlay(R3Data:153)
     d*** END ADD ****** 10/24/08 ******************************************************************
     d*** Beg   Add *** 05/08/25 *******************************************
     d  R3lmps                        1    overlay(R3Data:163)
     d*** End   Add *** 05/08/25 *******************************************
     d****  R3fill                      584    overlay(R3Data:150)
     d*** BEG CHG ****** 10/23/08 ******************************************************************
     d****  R3fill                      582    overlay(R3Data:152)
     d*** BEG CHG ****** 10/24/08 ******************************************************************
     d***  R3fill                      581    overlay(R3Data:151)
     d*** Beg   Chg *** 05/08/25 *******************************************
     d***  R3fill                      569    overlay(R3Data:163)
     d  R3fill                      570    overlay(R3Data:164)
     d*** End   Chg *** 05/08/25 *******************************************
     d*** END CHG ****** 10/24/08 ******************************************************************
     d*** END CHG ****** 10/23/08 ******************************************************************
     d*** END CHG ****** 06/12/07 ******************************************************************

     d                 ds                                                       Reserve History Chgs
     d  R4Data                      733                                         Record 04
     d  R4fnd                         3    overlay(R4Data:3)
     d  R4clm#                       40    overlay(R4Data:6)
     d  R4efdt                        8    overlay(R4Data:46)
     d  R4rt01                        2    overlay(R4Data:54)
     d  R4su01                        9  2 overlay(R4Data:56)
     d  R4sn01                        1    overlay(R4Data:65)
     d  R4rt02                        2    overlay(R4Data:66)
     d  R4su02                        9  2 overlay(R4Data:68)
     d  R4sn02                        1    overlay(R4Data:77)
     d  R4rt03                        2    overlay(R4Data:78)
     d  R4su03                        9  2 overlay(R4Data:80)
     d  R4sn03                        1    overlay(R4Data:89)
     d  R4rt04                        2    overlay(R4Data:90)
     d  R4su04                        9  2 overlay(R4Data:92)
     d  R4sn04                        1    overlay(R4Data:101)
     d  R4rt05                        2    overlay(R4Data:102)
     d  R4su05                        9  2 overlay(R4Data:104)
     d  R4sn05                        1    overlay(R4Data:113)
     d  R4rt06                        2    overlay(R4Data:114)
     d  R4su06                        9  2 overlay(R4Data:116)
     d  R4sn06                        1    overlay(R4Data:125)
     d  R4rt07                        2    overlay(R4Data:126)
     d  R4su07                        9  2 overlay(R4Data:128)
     d  R4sn07                        1    overlay(R4Data:137)
     d  R4rt08                        2    overlay(R4Data:138)
     d  R4su08                        9  2 overlay(R4Data:140)
     d  R4sn08                        1    overlay(R4Data:149)
     d  R4rt09                        2    overlay(R4Data:150)
     d  R4su09                        9  2 overlay(R4Data:152)
     d  R4sn09                        1    overlay(R4Data:161)
     d  R4rt10                        2    overlay(R4Data:162)
     d  R4su10                        9  2 overlay(R4Data:164)
     d  R4sn10                        1    overlay(R4Data:173)
     d  R4fill                      560    overlay(R4Data:174)

     d                 ds                                                       Claim Status Change
     d  R5Data                      733                                         Record 05
     d  R5fnd                         3    overlay(R5Data:3)
     d  R5clm#                       40    overlay(R5Data:6)
     d  R5ocst                        1    overlay(R5Data:46)
     d  R5efdt                        8    overlay(R5Data:47)
     d  R5fill                      679    overlay(R5Data:55)

     d                 ds                                                       Claim Transfer Chgs
     d  R6Data                      733                                         Record 06
     d  R6fnd                         3    overlay(R6Data:3)
     d  R6clm#                       40    overlay(R6Data:6)
     d  R6trdt                        8    overlay(R6Data:46)
     d  R6trtp                        1    overlay(R6Data:54)
     d*** BEG CHG ****** 06/12/07 ******************************************************************
     d*** Begin dlt *** 02/06/08 ********************************************************
     d*****  R6pol#                       20    overlay(R6Data:55)
     d*** End dlt ***** 02/06/08 ********************************************************
     d  R6grp#                        3    overlay(R6Data:55)
     d  R6emp#                        9    overlay(R6Data:58)
     d  R6div#                        5    overlay(R6Data:67)
     d  R6ss#                         9    overlay(R6Data:72)
     d*****  R6acdt                        8    overlay(R6Data:84)
     d  R6acdt                        8    overlay(R6Data:81)
     d*** END CHG ****** 06/12/07 ******************************************************************
     d*** Begin add *** 02/06/08 ********************************************************
     d  R6pol#                       20    overlay(R6Data:89)
     d*** End add ***** 02/06/08 ********************************************************
      *** Begin add *** 11/08/07 ****************************************************
     d                 ds                                                       Claim Transfer Chgs
     d  R7Data                      733                                         Record 06
     d  R7fnd                         3    overlay(R7Data:3)
     d  R7clm#                       40    overlay(R7Data:6)
     d  R7seq#                        5  0 overlay(R7Data:46)
     d  R7cdat                        8  0 overlay(R7Data:51)
     d  R7trdt                        8  0 overlay(R7Data:59)
     d  R7trtm                        6  0 overlay(R7Data:67)
     d  R7dcdt                        8  0 overlay(R7Data:73)
     d  R7difl                        1    overlay(R7Data:81)
     d  R7rmcd                        5    overlay(R7Data:82)
     d  R7comm                      275    overlay(R7Data:87)
     d  R7user                       10    overlay(R7Data:362)

     d                 ds                                                       Claim Transfer Chgs
     d  R8Data                      733                                         Record 06
     d  R8fnd                         3    overlay(R8Data:3)
     d  R8clm#                       40    overlay(R8Data:6)
     d  R8seq#                        5  0 overlay(R8Data:46)
     d  R8asq#                        5  0 overlay(R8Data:51)
     d  R8rmcd                        5    overlay(R8Data:56)
     d  R8cm75                       75    overlay(R8Data:61)
      *** End add *** 11/08/07 *******************************************  *********
      *** Begin add *** 04/11/08 ******************************************************
     d                 ds
     d  R9Data                      733                                         Record 09
     d  R9fid                        40    overlay(R9Data:3)
      *** BEG CHG *** 07/14/08 *******************************************  *********
     d********R901r                         3S 0 overlay(R9Data:43)
     d  R901r                         6S 0 overlay(R9Data:43)
     d********R902r                         3S 0 overlay(R9Data:46)
     d  R902r                         6S 0 overlay(R9Data:49)
     d********R903r                         3S 0 overlay(R9Data:49)
     d  R903r                         6S 0 overlay(R9Data:55)
     d********R904r                         3S 0 overlay(R9Data:52)
     d  R904r                         6S 0 overlay(R9Data:61)
     d********R905r                         3S 0 overlay(R9Data:55)
     d  R905r                         6S 0 overlay(R9Data:67)
     d********R906r                         3S 0 overlay(R9Data:58)
     d  R906r                         6S 0 overlay(R9Data:73)
     d********R907r                         3S 0 overlay(R9Data:61)
     d  R907r                         6S 0 overlay(R9Data:79)
     d********R908r                         3S 0 overlay(R9Data:64)
     d  R908r                         6S 0 overlay(R9Data:85)
     d**R909r                         3S 0 overlay(R9Data:47)
     d**R910r                         3S 0 overlay(R9Data:50)
     d********R9totr                        3S 0 overlay(R9Data:67)
     d  R9totr                        6S 0 overlay(R9Data:91)
     d********R9dtr                         8S 0 overlay(R9Data:70)
     d  R9dtr                         8S 0 overlay(R9Data:97)
      *** END CHG *** 07/14/08 *******************************************  *********
     d                 ds                  inz                                   Initialization
     d  xxCCYYMMDD                    8s 0                                         Date is CCYYMMDD
     d    xxCCYY                      4a   overlay(xxCCYYMMDD:1)                     Century, year
     d    xxMM                        2a   overlay(xxCCYYMMDD:5)                     Month
     d    xxDD                        2a   overlay(xxCCYYMMDD:7)                     Day
     d  xxEDATE                       8a                                           Edited date
      *** End add *** 04/11/08 *******************************************  *********
     d*** Beg add *** 03/06/19 *******************************************  *********
     d                 ds                  inz                                   Initialization
     d  xxSS#                         9                                            Date is CCYYMMDD
     d    xxSSA                       3    overlay(xxSS#:1)                          Century, year
     d    xxSSN                       6  0 overlay(xxSS#:4)                          Month
     d*** End add *** 03/06/19 *******************************************  *********
      ***** BEG ADD ***** 10/17/11 ***********************************
     d root            s             10
     d env             s              1
      ***** END ADD ***** 10/17/11 ***********************************
     d psds          esds                  extname(szq1p)

     d/copy sprdate
     d/copy sprcompany
     d/copy sprstring
     d/copy wprgroup
     d/copy wprusrprms
     d/Eject
      *=============================================================================================
      * KLISTS
      *=============================================================================================
      *
      * Claimant Demographic file by Soc Sec#
     c     cdky          klist
     c                   kfld                    kkss#
      *
      * Claims Master by SocSec#, Accident Date
     c     cmky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkss#
     c                   kfld                    kkacdt
      * Claims Master by Case
     c     cmky2         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
      * Claims Master by PDS#
     c     cmky15        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkpds#
      *
      * Claim Client Aux
     c     cxky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkcase
      *
      * Claim Status History
     c     chky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     c                   kfld                    xxefdt
      *
     c     ctky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
      * Deductible Detail file
     c     ddky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
      * Employer Master
     c     emky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
      *
      * Employer Detail
     c     elky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkacdt
      *
      * Group Master file
     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c     fnky2         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
      *
      * Loss Coverage table
     c     gtky          klist
     c                   kfld                    kkco#
     c                   kfld                    kktbid
     c                   kfld                    kkkey
      *
      * Claim Legal Master
     c     llky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
      * Rounting file
     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#
      * Claim Payments
     c     paky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     c                   kfld                    xxseq#
      *
     c     paky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
      *
      * Reserve History
     c     rhky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     c                   kfld                    xxefdt
      *
      * Carrier/State Reporting Master
     c     rpky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkCap#
      * Table file
     c     tbky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkkey

      *** Begin add *** 11/08/07 ****************************************************

     c     scmky         klist
     c                   kfld                    kdkey
     c                   kfld                    kdseq#

     c     arky          klist
     c                   kfld                    kdkey
     c                   kfld                    kdseq#
     c                   kfld                    kdasq#

      *** End   add *** 11/08/07 ****************************************************
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    @@que
     c                   parm                    @@rpdt
     c                   parm                    @@dnld
      *** beg add ***** 07/23/08 ********************************************************
     c                   parm                    @@upDate          1            updateIfErrorsExist
      *** end add ***** 07/23/08 ********************************************************

     c     datchk        plist
     c                   parm                    srdti
     c                   parm      *blanks       srffmt
     c                   parm      *blanks       srtfmt
     c                   parm                    srdto
     c                   parm                    srerr
      *
      * Change Claim From & To Parm List
     c     chgParm       plist
     c                   parm                    @@co#             3
     c                   parm                    @@ffnd            3
     c                   parm                    @@femp            9
     c                   parm                    @@fdiv            5
     c                   parm                    @@ffyr            3
     c                   parm                    @@fcas            7
     c                   parm                    @@tfnd            3
     c                   parm                    @@temp            9
     c                   parm                    @@tdiv            5
     c                   parm                    @@tfyr            3
     c                   parm                    @@tcas            7
     c                   parm                    @@trtp
      *
      * Delete Claim Parm List
     c     delParm       plist
     c                   parm                    @@co#             3
     c                   parm                    @@fnd             3
     c*** Begin Add *** 06/13/11 *******************************************
     c                   parm      cmemp#        @@emp#
     c*** End   Add *** 06/13/11 *******************************************
     c                   parm                    @@fyr             3
     c                   parm                    @@case            7
     c                   parm                    xxcall
      *
      *
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
      *
     c                   time                    dsdttm
     c                   eval      cymdRptDt = dtCYMD(dsdate)
     c                   z-add     dstime        zztime
      *
      *** Begin add *** 11/08/07 ****************************************************
     c                   call      'RTVCLTID'
     c                   parm                    cltid             3
      *** End   add *** 11/08/07 ****************************************************
       //*** Begin Add *** 07/07/21 ***********************************
       // Initialize job ending indicator
       jobEnding = *Off;

       // Was this called from WICLAIMS
       chrOnStack = ' ';
       CheckCallStack('WICLAIMS':chrOnStack);

       // If not called from WICLAIMS then open QSYSPRT
       If chrOnStack = '0';
          Open QSYSPRT;
          Open QSYSPRT1;
       EndIf;
       //*** End   Add *** 07/07/21 ***********************************
      ***** BEG ADD ***** 10/17/11 ***********************************
      * Get Root
     c                   call      'RTVDOCROOT'
     c                   parm                    root
      * Find IFS
     c                   call      'RTVDOCENV'
     c                   parm                    env
     c
      ***** END ADD ***** 10/17/11 ***********************************
      *** beg add ***** 07/23/08 ****************************************************
     c                   if        Q1PRMS > 3
     c                   eval      upDate = @@upDate
     c                   else
     c                   eval      upDate = 'Y'
     c                   endif
      *** end add ***** 07/23/08 ****************************************************
      *
      * *IN90 controls the ADD/UPDATE of Claims Records, Must be ON to do this.
      *
      *** beg del ***** 07/23/08 ****************************************************
     c************       move      *ON           *IN90                          Upd/Add Rcds
      *** end del ***** 07/23/08 ****************************************************
     c*
     c/Eject
      *=============================================================================================
      * Edit flat file
      *=============================================================================================
      *
      * Run through file the first time checking for errors. If error is found, except the line to
      * the error report and continue checking the file for other errors. Prevent the batch from
      * further processing.
      *
     c                   exsr      printSetup
      *
     c                   if        @@dnld = 'Y'
     c                   move      *on           *in20
     c                   goto      Term                                         End program
     c                   EndIf                                                  @@dnld = Y
      *
     c                   If        %Open(climport)
     c                   Close     climport
     c                   EndIf
     c                   Open      climport
      *** beg add ***** 07/23/08 *******************************************************
      *
      *   If the updateIfErrorsExist flag = 'N', run through twice, the first time to edit, the
      *   second time to update records if no errors exist; else, process as it always has which
      *   is to skip bad records and write good ones.
      *
      *** beg add ***** 08/13/08 *******************************************************
     c                   eval      loopcount = *zeros
      *** end add ***** 08/13/08 *******************************************************
     c                   eval      writeRecords = *blank
     c                   eval      *in90 = *off
     c                   if        upDate    <> 'N'
     c                   eval      writeRecords = 'Y'
     c                   endif
     c                   dou       writeRecords = 'Y'
     c                   select
      *** beg chg ***** 08/13/08 *******************************************************
     c*************      when      writeRecords = 'Y' and
     c*************                update     = 'N' and
     c*************                errCount > 0
     c                   when      update     = 'N' and
     c                             errCount > 0
     c                   leave
     c                   when      update    = 'N' and
     c                             errcount = 0 and
     c                             writeRecords <> 'Y' and
     c                             loopcount >= 1
     c                   eval      writeRecords = 'Y'
     c                   eval      *in90 = *on
      *** end chg ***** 08/13/08 *******************************************************
     c                   when      writeRecords = 'Y'
     c                   eval      *in90 = *on
     c                   endsl
      *** beg add ***** 08/13/08 *******************************************************
     c                   exsr      resetcounts
     c                   add       1             loopcount
      *** end add ***** 08/13/08 *******************************************************
     c     1             setll     pvf01
      *** end add ***** 07/23/08 *******************************************************
     c                   read      pvf01                                  60
     c                   if        *IN60 = *ON
     c                   goto      Term                                         End program
     c                   EndIf                                                  *in60 = ON
      *
      *
     c                   dow       *in60 = *off
      *
     c                   eval      recCount = recCount + 1
      *
     c     2             subst     VFData        recType

      *
     c                   eval      rcd01 = *OFF
     c                   eval      rcd02 = *OFF
     c                   eval      rcd03 = *OFF
     c                   eval      rcd04 = *OFF
     c                   eval      rcd05 = *OFF
     c                   eval      rcd06 = *OFF

     c                   select

      *  Process Record type is blank
     c                   when      recType = '  '
     c                   move(p)   err(4)        errReason
     c                   eval      rec_type = 'Rcrd 00'
     c                   eval      rcdOtErr = rcdOterr + 1
     c                   exsr      printError

      *  Process Record type 01
     c                   when      recType = '01'
     c                   eval      recCnt01 = recCnt01 + 1
     c
     c                   clear                   r1data
     c                   movel     VFData        r1data
      *
      * Reset Record, Error and Claim Type Indicators
     c                   eval      addClaim  = *OFF
     c                   eval      ChgClaim  = *OFF
     c                   eval      delClaim  = *OFF
      *
     c                   eval      error01 = *OFF
     c                   eval      error02 = *OFF
     c                   eval      error03 = *OFF
     c                   eval      error04 = *OFF
     c                   eval      error05 = *OFF
     c                   eval      error06 = *OFF
     c*** Begin add *** 03/26/08 ******************************************************
     c                   eval      errcnam = *off
     c*** End add ***** 03/26/08 ******************************************************
      *
     c                   eval      rcd01 = *ON
      *
      *  Save Claim Number for processing other record types
     c
     c                   If        R1Clm# <> xxclm#
     c                   eval      xxclm# = R1Clm#
     c                   EndIF
     c
      * Process Record type 01 if no errors
     c                   exsr      xrefEdit

      * Process Record type 02
     c                   when      recType = '02'
     c                   clear                   r2data
     c                   movel     VFDATA        R2data
     c                   eval      recCnt02 = recCnt02 + 1
     c                   eval      rcd02 = *ON
     c                   If        Error01 = *ON
     c                   eval      Rcd02Skp = Rcd02Skp + 1
     c                   Endif
     c                   If        R2Clm# = *blanks
     c                   eval      R2Clm# = xxclm#
     c                   endif
     c                   If        R2clm# = xxclm# AND Error01 = *OFF
     c                   exsr      ClaimEdit
     c                   EndIF                                                  IF <> xxClm#

      * Process Record type 03
     c                   when      recType = '03'
     c                   clear                   r3data
     c*** Begin Add *** 09/13/18 ***********************************
      /free
       If %Subst(VFData:59:7) = ' ';
          %Subst(VFData:59:7) = '0000000';
       Else;
          strCheck# = '0000000';
          EvalR strCheck# = %Xlate(' ':'0':%Trim(%Subst(VFData:59:7)));
          %Subst(VFData:59:7) = strCheck#;
       EndIf;
      /end-free
     c*** End   Add *** 09/13/18 ***********************************
     c                   movel     VFData        r3data
       //*** Begin Add *** 09/13/18 ***************************************
      /free
       // Reserve Codes
       Select;
       When R3reCd = '11';
          R3reCd = 'ME';
       When R3reCd = '12';
          R3reCd = 'CO';
       When R3reCd = '13';
          R3reCd = 'EX';
       When R3reCd = '14';
          R3reCd = 'RE';
       When R3reCd = '16';
          R3reCd = 'LE';
       EndSl;
      /end-free
       //*** End   Add *** 09/13/18 ***************************************
     c                   eval      recCnt03 = recCnt03 + 1
     c                   eval      rcd03 = *ON
     c                   If        Error01 = *ON
     c                   eval      Rcd03Skp = Rcd03Skp + 1
     c                   Endif
     c                   If        R3Clm# = *blanks
     c                   eval      R3Clm# = xxclm#
     c                   endif
     c                   If        R3clm# = xxclm# AND Error01 = *OFF
     c                   exsr      pymtEdit
     c                   EndIF                                                  IF <> xxClm#

      * Process Record type 04
     c                   when      recType = '04'
     c                   clear                   r4data
     c                   movel     VFData        r4data
     c                   eval      recCnt04 = recCnt04 + 1
     c                   eval      rcd04 = *ON
     c                   If        Error01 = *ON
     c                   eval      Rcd04Skp = Rcd04Skp + 1
     c                   Endif
     c                   If        R4Clm# = *blanks
     c                   eval      R4Clm# = xxclm#
     c                   endif
     c                   If        R4clm# = xxclm# AND Error01 = *OFF
     c                   exsr      RHistEdit
     c                   EndIF                                                  IF <> xxClm#

      * Process Record type 05
     c                   when      recType = '05'
     c                   clear                   r5data
     c                   movel     VFData        r5data
     c                   eval      recCnt05 = recCnt05 + 1
     c                   eval      rcd05 = *ON
     c                   If        Error01 = *ON
     c                   eval      Rcd05Skp = Rcd05Skp + 1
     c                   Endif
     c                   If        R5Clm# = *blanks
     c                   eval      R5Clm# = xxclm#
     c                   endif
     c                   If        R5clm# = xxclm# AND Error01 = *OFF
     c                   exsr      StatEdit
     c                   If        Error05
     c                   eval      Rcd05Err = Rcd05Err + 1
     c                   Endif
     c                   EndIF                                                  IF <> xxClm#

      * Process Record type 06
     c                   when      recType = '06'
     c                   clear                   r6data
     c                   movel     VFData        r6data
     c                   eval      recCnt06 = recCnt06 + 1
     c                   eval      rcd06 = *ON
     c                   If        Error01 = *ON
     c                   eval      Rcd06Skp = Rcd06Skp + 1
     c                   Endif
     c                   If        R6clm# = xxclm# AND Error01 = *OFF
     c                   exsr      ChngEdit
     c                   If        Error06
     c                   eval      Rcd06Err = Rcd06Err + 1
     c                   Endif
     c                   EndIF                                                  IF <> xxClm#
      *
      * Ensure that there are no detail records after the Rcd 06 record.
      *
     c*****              WHEN      Rcd06  = *on and recType <> '06'
     c*****              eval      recOther = recOther + 1
     c*****              eval      errorFlag = *on
     c*****              move(p)   err(9)        errReason
     c*****              MOVEL(P)  'RCRD'        rec_type
     c*****              Move      recType       rec_type
     c*****              eval      rcdOtErr = rcdOterr + 1
     c*****              exsr      printError

      *** Begin add *** 11/08/07 ****************************************************

      * Process Record type 07
     c                   when      recType = '07'
     c                   clear                   r7data
     c                   movel     VFData        r7data
     c                   eval      recCnt07 = recCnt07 + 1
     c                   eval      rcd07 = *ON
     c                   If        Error01 = *ON
     c                   eval      Rcd07Skp = Rcd07Skp + 1
     c                   Endif
     c                   If        R7Clm# = *blanks
     c                   eval      R7Clm# = xxclm#
     c                   endif
     c                   If        R7clm# = xxclm# AND Error01 = *OFF
     c                   exsr      DARTEdit1
     c                   If        Error07
     c                   eval      Rcd07Err = Rcd07Err + 1
     c                   Endif
     c                   EndIF                                                  IF <> xxClm#


      * Process Record type 08
     c                   when      recType = '08'
     c                   clear                   r8data
     c                   movel     VFData        r8data
     c                   eval      recCnt08 = recCnt08 + 1
     c                   eval      rcd08 = *ON
     c                   If        Error08 = *ON
     c                   eval      Rcd08Skp = Rcd08Skp + 1
     c                   Endif
     c                   If        R8Clm# = *blanks
     c                   eval      R8Clm# = xxclm#
     c                   endif
     c                   If        R8clm# = xxclm# AND Error01 = *OFF
     c                   exsr      DARTEdit2
     c                   If        Error08
     c                   eval      Rcd08Err = Rcd08Err + 1
     c                   Endif
     c                   EndIF                                                  IF <> xxClm#

      *** End add *** 11/08/07 *******************************************  *********
      *** Begin add *** 04/11/08 ****************************************************
      * Process Record type 09
     c                   when      recType = '09'
     c                   clear                   r9data
     c                   movel     VFData        r9data
     c                   eval      recCnt09 = recCnt09 + 1
     c                   exsr      writeAck
      *** End   add *** 04/11/08 ****************************************************

     c                   other
     c                   eval      recOther = recOther + 1
     c                   exsr      otherEdit

     c                   endsl

     c                   read      pvf01                                  60
     c                   enddo

      *** beg add ***** 07/23/08 *******************************************************
      *** beg del ***** 08/13/08 *******************************************************
     c***************    if        update    = 'N' and
     c***************              writeRecords <> 'Y'
     c***************    eval      writeRecords = 'Y'
     c***************    endif
      *** end del ***** 08/13/08 *******************************************************
     c                   enddo                                                  writeRecords
      *** end add ***** 07/23/08 *******************************************************

     c                   close     climport

      * ===================================================================== *
      *  Terminate Program
      * ===================================================================== *
      *
     c     Term          Tag
     c*** Begin Chg *** 07/07/21 **************************************
     c**********         eval      *inlr = *on
       jobEnding = *On;
       If chrOnStack = '0';
          *inLR = *On;
       EndIf;
     c*** End   Chg *** 07/07/21 **************************************
      *
     c     *cymd         move      sys_Date      reportDate
     c                   move      reportDate    @@Rpdt
      *
     c                   eval      rcdCount = rcd01Cnt + rcd02Cnt + rcd03Cnt +
     c                                        rcd04Cnt + rcd05Cnt + rcd06Cnt +
     c                                        recOther
      *
     c                   eval      rcdErSkp = rcd01Skp + rcd02Skp + rcd03Skp +
     c                                        rcd04Skp + rcd05Skp + rcd06Skp
      *
     c                   eval      rcdErCnt = rcd01Err + rcd02Err + rcd03Err +
     c                                        rcd04Err + rcd05Err + rcd06Err +
     c                                        rcdOtErr
      *
     c                   eval      suRhTot  = suT01 + suT02 + suT03 + suT04 +
     c                                        SuT05 + suT06 + suT07 + suT08 +
     c                                        SuT09 + suT10
      *
     c                   if        rcdCount = *zeros
     c                   move      *ON           *IN21
      *
     c                   else
      *
     c                   move      *OFF          *IN21
      *
     c                   EndIf
      *
      * Print totals on reports
     c                   exsr      printError                                   error report
     c/eject
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       //*** Begin Chg *** 07/07/21 ***********************************
       //***xxFil# = '0';
       //***Chain orKy POR01;
       //***If %found;
       //***   Except Route;
       //***Endif;
       //***xxFil# = '1';
       //***Chain orKy POR01;
       //***If %found;
       //***   Except Route1;
       //***Endif;
       If chrOnStack = '0';
          xxFil# = '0';
          Chain orKy POR01;
          If %found;
             Except Route;
          Endif;
          xxFil# = '1';
          Chain orKy POR01;
          If %found;
             Except Route1;
          Endif;
       Endif;
       //*** End   Chg *** 07/07/21 ***********************************
       //*** Begin Add *** 07/07/21 **********************************
       Select;
       When chrOnStack = '0';
          Close QSYSPRT;
          Close QSYSPRT1;
       When chrOnStack = '1';
          If Q1prms > 3 And errorsFound;
             @@upDate = 'E';
          EndIf;
          Return;
       EndSl;
       //*** End   Add *** 07/07/21 **********************************
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     c*** Begin Add *** 01/22/19 *******************************************
      *=============================================================================================
      * WriteWMAWP - Writes AWW History Record.                             pe 01
      *=============================================================================================
     c     WriteWMAWP    begsr

      /free
       AWco#  = CMco#;
       AWfnd  = CMfnd;
       AWfYr  = CMfYr;
       AWcase = CMcase;
       AWaww  = R2aww;
       AWcRt  = 0;
       AWefDt = DateTo7(SystemDate());

       // Calculate the rate
       SetGt  (CMco#:CMfnd:CMjrSt:CMacDt) prb00;
       ReadPe (CMco#:CMfnd:CMjrSt) prb00;
       If not %Eof(WDRBL);
          AWcRt = AWaww * RBcmpr / 100;
          If AWcRt < RBmnrt;
             AWcRt = RBmnrt;
          EndIf;
          If AWcRt > RBpacp;
             AWcRt = RBpacp;
          EndIf;
       EndIf;

       AWuser=q1User;
       AWpgm=q1Pgm;
       AWtime=TimeTo6(SystemTime());
       AWdt=DateTo7(SystemDate());
       Write paw00;

       CMcRt = AWcRt;

      /end-free
     c                   endsr                                                  xrefEdit
     c*** End   Add *** 01/22/19 *******************************************
      *=============================================================================================
      * X-RefEdit subroutine - edit X-ref and get matching claim record - type 01
      *=============================================================================================
     c     xrefEdit      begsr
      * Get WC key information
      *
     c                   MOVEL(P)  R1CAP#        kkCAP#
     c                   movel(p)  r1clm#        kkpds#
     c                   clear                   errorfields
     c                   movel(P)  r1clm#        policy#
     c                   move      r1fnd         grp#
     c                   move      r1emp#        emp#
     c                   move      r1div         div
      *
     c*** Begin add *** 05/21/08 ***************************************************
     c*  Determine if the claim exists and update the "update type" flag as needed.
     c*
     c                   If        CLTID = '132'
     c*
     c                   move      r1fnd         kkfnd
     c                   move      r1fyr         kkfyr
     c                   move      r1case        kkcase
      *
     c     cmky2         chain(n)  pcm02                              60
     c   60cmky15        chain     pcm15                              60

     c                   IF        *in60 = *off
     c                   eval      r1uptp = 'C'
     c*
     c*** Begin add *** 08/08/06 ****************************************************
     c*      Correct header data to reflect value from file
     c                   move      cmfnd         r1fnd
     c                   move      cmemp#        r1emp#
     c                   move      cmdiv         r1div
     c                   move      cmss#         r1ss#
     c     19000000      add       cmacdt        zzdate
     c                   move      zzdate        r1acdt
     c*** End add ***** 08/08/06 ****************************************************
     c*
     c                   else
     c                   eval      r1uptp = 'A'
     c                   endif
     c*
     c                   endif
     c*** End chg ***** 05/21/08 ***************************************************
     c*** Beg add *** 03/06/19 *******************************************  *********
     c*
     c*** Begin Chg *** 08/22/19 ******************************************
     c**********         If        CLTID = '136'
     c                   If        CLTID = '136' oR CLTID = '150'
     c*** End   Chg *** 08/22/19 ******************************************
     c*
     c                   IF        r1ss#='999999999' or r1ss#='         '
     c*** Beg add *** 05/30/19 *******************************************  *********
     c                             or r1ss#='219099999'
     c*** End add *** 05/30/19 *******************************************  *********
     c                   IF        r1uptp = 'C' or r1uptp = 'D'
     c                   move      r1fnd         kkfnd
     c                   move      r1fyr         kkfyr
     c                   move      r1case        kkcase
      *
     c     cmky2         chain(n)  pcm02                              60
     c   60cmky15        chain     pcm15                              60
     c                   IF        *in60 = *off
     c                   move      cmss#         r1ss#
     c                   endif
     c*
     c                   else
     c* add
     C                   If        cltID = '136'
     c     'SSN999999'   setgt     pcd01
     c                   Else
     c     '911999999'   setgt     pcd01
     c                   EndIf
     c                   readp(n)  pcd01                                6060
     c                   IF        *in60 = *off
     c                   move      cdss#         xxss#
     C                   If        cltID = '136'
     c                   IF        xxssa = 'SSN'
     c                   eval      xxssn = xxssn + 1
     c                   else
     c                   eval      xxssa = 'SSN'
     c                   z-add     1             xxssn
     c                   endif
     c                   Else
     c                   IF        xxssa = '911'
     c                   eval      xxssn = xxssn + 1
     c                   else
     c                   eval      xxssa = '911'
     c                   z-add     1             xxssn
     c                   endif
     c                   EndIf
     c                   move      xxss#         r1ss#
     c
     c                   endif
     c                   endif
     c*
     c                   endif
     c                   endif
     c*** End add *** 03/06/19 *******************************************  *********

     c                   DO
      *
     c                   IF        r1uptp  = 'A' or R1uptp = 'a'
     c                             or (r1fyr = *blanks and r1case = *blanks)
      *
     c                   IF        r1fnd <> *blanks and r1emp# <> *blanks
     c                   move      r1fnd         kkfnd
     c                   move      r1emp#        kkemp#
     c                   move      r1div         kkdiv
     c     emky          chain     pem01                              61
     c                   endif
      *
     c                   if        *in61
      *
     c                   if        R1CAP# = *blanks
     c                   move(p)   err(11)       errReason
     c                   eval      error01 = *on
     c                   eval      rec_type = 'Rcrd 01'
     c                   movel(p)  r1fnd         policy#
     c                   cat       r1emp#:0      policy#
     c                   cat       r1div:0       policy#
     c                   eval      Rcd01Err = Rcd01Err + 1
     c                   exsr      printError
     c                   leave
      *
     c                   else
      *
     c     rpky          chain     prp01                              60
     c                   IF        *IN60
     c                   move(p)   err(11)       errReason
     c                   eval      error01 = *on
     c                   eval      rec_type = 'Rcrd 01'
     c                   movel(p)  r1cap#        policy#
     c                   eval      Rcd01Err = Rcd01Err + 1
     c                   exsr      printError
     c                   leave
      *
     c                   else
      *
     c                   eval      kkfnd  =  rpfnd
     c                   eval      kkemp# =  rpemp#
     c                   eval      kkdiv  =  rpdiv
     c                   eval      kkfyr  =  rpfyr
     c     emky          chain     pem01                              61
     c                   if        *in61
     c                   movel(p)  err(12)       errReason
     c                   eval      error01 = *on
     c                   eval      rec_type = 'Rcrd 01'
     c                   movel(p)  r1fnd         policy#
     c                   cat       r1emp#:0      policy#
     c                   cat       r1div:0       policy#
     c                   eval      Rcd01Err = Rcd01Err + 1
     c                   exsr      printError
     c                   leave
     c                   endif
     c                   endif
     c                   endif
     c                   endif
      *
     c                   endif
      *
      *  If SS# is blank or the Accident Date is blank or invalid is an Error
      *
     c                   If        r1uptp  = 'A' or R1uptp = 'a'
     c                   IF        r1acdt = *blanks OR r1ss# = *blanks
     c                   eval      error01 = *on
     c                   eval      errorFlag = *on
     c                   movel(p)  r1clm#        policy#
     c                   move(p)   err(14)       errReason
     c                   eval      rec_type = 'Rcrd 01'
     c                   eval      Rcd01Err = Rcd01Err + 1
     c                   exsr      printError
     c                   leave
     c                   endif
      *
      *  Check Accident Date claim, check to see if exists using SS# and Accident Date
      *
     c                   move      r1acdt        zzdate
     c                   exsr      editDate
     c                   if        errorFlag = *on
     c                   eval      error01 = *on
     c                   movel(P)  err(15)       errReason
     c                   eval      rec_type = 'Rcrd 01'
     c                   eval      Rcd01Err = Rcd01Err + 1
     c                   exsr      printError
     c                   leave
     c                   else
     c                   z-add     x7date        kkacdt
     c                   endif
     c                   endif
     c
      *  Determine if Add, Change or Delete Claim
     c                   select
     c                   when      r1uptp  = 'A' or R1uptp = 'a'
     c                   eval      addClaim  = *ON
     c                   when      r1uptp  = 'C' or r1uptp = 'c'
     c                   eval      ChgClaim  = *ON
     c                   when      r1uptp  = 'D' or r1uptp = 'd'
     c                   eval      delClaim  = *ON
     c                   Other
     c                   move(p)   err(10)       errReason
     c                   eval      error01 = *ON
     c                   eval      rec_type = 'Rcrd 01'
     c                   eval      Rcd01Err = Rcd01Err + 1
     c                   exsr      printError
     c                   EndSL
      *
      *  If change or delete claim, check to see if exists.
      *
     c                   move      r1fnd         kkfnd
     c                   move      r1fyr         kkfyr
     c                   move      r1case        kkcase
     c                   move      r1ss#         kkss#
     c                   move      r1acdt        zzdate
     c                   exsr      editDate
     c                   z-add     x7date        kkacdt
      *
     c     cmky2         chain     pcm02                              60
     c   60cmky15        chain     pcm15                              60
     c*** Begin dlt *** 05/21/08 ***************************************************
     c***60cmky          chain     pcm01                              60
     c*** End dlt ***** 05/21/08 ***************************************************
      *
     c                   IF        *in60 = *off
     c                   eval      kkfnd  =  cmfnd
     c                   eval      kkemp# =  cmemp#
     c                   eval      kkdiv  =  cmdiv
     c                   eval      kkfyr  =  cmfyr
     c                   eval      kkcase =  cmcase
     c                   endif
      *
     c                   IF        *in60 = *on
     c                   IF        chgClaim OR delClaim
     c                   eval      errorFlag = *on
     c                   eval      error01 = *on
     c                   movel(p)  r1clm#        policy#
     c                   move(p)   err(16)       errReason
     c                   eval      rec_type = 'Rcrd 01'
     c                   eval      Rcd01Err = Rcd01Err + 1
     c                   exsr      printError
     c                   leave
     c                   endif                                                  ChgClaim
     c                   endif                                                  *in60 = *ON
      *
      ***** BEG ADD ***** 01/20/06 *************************************************
      * If got record, use the accident date and ss# for next chain to WMCMLB
     c                   IF        chgClaim OR delClaim
     c                   eval      kkss# = CMSS#
     c                   eval      kkacdt = CMACDT
     c                   EndIF                                                  ChgClaim
      ***** END ADD ***** 01/20/06 *************************************************
      *
      *  If Add claim if exists is an Error
      *
     c*** Begin chg *** 05/21/08 ***************************************************
     c***********        IF        addClaim AND *IN60 = *OFF
     c                   IF        addClaim
     c*** End chg ***** 05/21/08 ***************************************************
     c*** Begin add *** 05/21/08 ***************************************************
     c*** Begin Chg *** 08/12/19 *******************************************
     c*****cmky*         chain     pcm01                              60
     c     cmky15        chain     pcm15                              60
     c*** End   Chg *** 08/12/19 *******************************************
     c                   IF        *IN60 = *OFF
     c*** End add ***** 05/21/08 ***************************************************
     c*** Begin dlt *** 05/21/08 ***************************************************
     c*******            eval      errorFlag = *on
     c*******            eval      error01 = *on
     c*** End dlt ***** 05/21/08 ***************************************************
     c*** Begin Add *** 08/12/19 *******************************************
     c                   eval      errorFlag = *on
     c                   eval      error01 = *on
     c*** End   Add *** 08/12/19 *******************************************
     c                   movel(p)  r1clm#        policy#
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'r1ss#'       qqname           25
     C     r1ss#         parm      r1ss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     c                   movel(p)  r1ss#         errvalue
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'r1ss#'       qqname           25
     C     r1ss#         parm      r1ss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     c                   cat       '-':1         errvalue
     c                   cat       r1acdt:1      errvalue
     c                   move(p)   err(17)       errReason
     c                   eval      rec_type = 'Rcrd 01'
     c                   eval      Rcd01Err = Rcd01Err + 1
     c                   exsr      printError
     c                   leave
     c*** Begin add *** 05/21/08 ***************************************************
     c                   endif
     c*** End add ***** 05/21/08 ***************************************************
     c                   endif

     c                   leave
     c                   enddo                                                  %error
     c
     c                   if        NOT error01
     c                   clear                   errorfields
     c                   movel(P)  xxclm#        policy#
     c                   move      kkfnd         grp#
     c                   move      kkemp#        emp#
     c                   move      kkdiv         div
     c                   eval      rcd01Cnt = (rcd01Cnt + 1)
     c
     c
     c                   endif

     c                   endsr                                                  xrefEdit
     c******/eject
      *=============================================================================================
      * ClaimEdit subroutine - edit The Claim Info and reserves record - Type 02
      *=============================================================================================
     c     claimEdit     begsr
      *
      * Ensure that there are no detail records read before the Rcd 01 record.
      *
     c                   eval      rec_type = 'Rcrd 02'
      *
     c                   eval      policy# = xxclm#
       //*** Begin Add *** 09/13/18 ***************************************
      /free
       // Bucket 1
       Select;
       When R2rt01 = '11';
          R2rt01 = 'ME';
       When R2rt01 = '12';
          R2rt01 = 'CO';
       When R2rt01 = '13';
          R2rt01 = 'EX';
       When R2rt01 = '14';
          R2rt01 = 'RE';
       When R2rt01 = '16';
          R2rt01 = 'LE';
       EndSl;
       // Bucket 2
       Select;
       When R2rt02 = '11';
          R2rt02 = 'ME';
       When R2rt02 = '12';
          R2rt02 = 'CO';
       When R2rt02 = '13';
          R2rt02 = 'EX';
       When R2rt02 = '14';
          R2rt02 = 'RE';
       When R2rt02 = '16';
          R2rt02 = 'LE';
       EndSl;
       // Bucket 3
       Select;
       When R2rt03 = '11';
          R2rt03 = 'ME';
       When R2rt03 = '12';
          R2rt03 = 'CO';
       When R2rt03 = '13';
          R2rt03 = 'EX';
       When R2rt03 = '14';
          R2rt03 = 'RE';
       When R2rt03 = '16';
          R2rt03 = 'LE';
       EndSl;
       // Bucket 4
       Select;
       When R2rt04 = '11';
          R2rt04 = 'ME';
       When R2rt04 = '12';
          R2rt04 = 'CO';
       When R2rt04 = '13';
          R2rt04 = 'EX';
       When R2rt04 = '14';
          R2rt04 = 'RE';
       When R2rt04 = '16';
          R2rt04 = 'LE';
       EndSl;
       // Bucket 5
       Select;
       When R2rt05 = '11';
          R2rt05 = 'ME';
       When R2rt05 = '12';
          R2rt05 = 'CO';
       When R2rt05 = '13';
          R2rt05 = 'EX';
       When R2rt05 = '14';
          R2rt05 = 'RE';
       When R2rt05 = '16';
          R2rt05 = 'LE';
       EndSl;
      /end-free
       //*** End   Add *** 09/13/18 ***************************************
      *
      * When adding claim, clear the record formats first
     c                   If        addClaim
     c*** Begin chg *** 05/12/08 ****************************************************
     c*****************  clear                   pcm01
     c                   clear                   pcm02
     c*** End chg ***** 05/12/08 ****************************************************
     c                   clear                   pcd01
     c                   endif
      *
      * If Add, check that claim does not exist under the same GIC Policy Number
     c***                IF        addClaim
     c***  cmky15        chain     pcm15                              60
     c***                If        Not *in60
     c***                eval      error02 = *on
     c***                move(p)   err(9)        errReason
     c***                eval      Rcd02Err = Rcd02Err + 1
     c***                exsr      printError
     c***                EndIf
     c***                EndIf
      *
      * If Change Claim, Check for Claimant Demographic Record Exists
      *
     c                   move      *OFF          haveClaimant
     c****               eval      kkss# = R1ss#
     c     cdky          chain(n)  pcd01                              60
     c                   If        chgClaim and *IN60
     c                   eval      error02 = *on
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*MASK'       qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'r1ss#'       qqname           25
     C     r1ss#         parm      r1ss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     c                   movel(p)  r1ss#         errvalue
     C***  Begin Add  ***  06/09/20  *******************************
     C                   call      'SPIMASK'
     C                   parm      '*UNMASK'     qqmode           10
     C                   parm      'SSN'         qqtype           25
     C                   parm      'r1ss#'       qqname           25
     C     r1ss#         parm      r1ss#         qqval           100
     C***  End Add  ***  06/09/20  *******************************
     c                   move(p)   err(21)       errReason
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   EndIF
     c
      *
      * If Claimant Demographic record exists, verify that the name is the same.
      *
     c                   If        addClaim
     c     Lo:Up         xlate(p)  r2lsn         xr2lsn
     c     Lo:Up         xlate(p)  r2fsn         xr2fsn
     c     Lo:Up         xlate(p)  cdlsn         xcdlsn
     c     Lo:Up         xlate(p)  cdfsn         xcdfsn
     c                   If        not *in60 and xr2lsn <> xcdlsn
     c                             or not *in60 and xr2fsn <> xcdfsn
     c                   movel(p)  r2lsn         errValue
     c                   cat       ',':0         errvalue
     c                   cat       r2fsn:1       errvalue
     c*** Begin dlt *** 03/26/08 ******************************************************
     c**************     eval      error02 = *on
     c**************     eval      Rcd02Err = Rcd02Err + 1
     c*** End dlt ***** 03/26/08 ******************************************************
     c*** Begin add *** 03/26/08 ******************************************************
     c                   eval      errcnam = *on
     c*** End add ***** 03/26/08 ******************************************************
     c                   move(p)   err(34)       errReason
     c                   exsr      printError
     c                   EndIF
     c                   EndIF
     c
      *** Begin change 06/07/19 ***************************
     c***                If        chgClaim and *IN60 = *OFF
     c                   If        *IN60 = *OFF
      *** End   change 06/07/19 ***************************
     c                   move      *ON           haveClaimant
     c                   endif
     c*** Begin Add *** 02/04/19 ****************************************
      *
     c*** Beg add *** 03/06/19 *******************************************  *********
     c                   if        R1cap# <> *blanks And addClaim = *On
     c                   movel(p)  R1cap#        kkCAP#
     c     rpky          chain     prp01                              60
     c                   IF        *IN60=*off
     c                   eval      kkfyr  =  rpfyr
     c                   endif
     c                   endif
     c*** End add *** 03/06/19 *******************************************  *********
      * Check to see if class code is setup for the employer
      /free
      //*** Begin Chg *** 09/27/19 **********************************
      //*** Chain (kkCo#:kkFnd:kkEmp#:kkDiv:kkFyr:r2Clas) pep00;
       Chain (kkCo#:kkFnd:kkFyr:kkEmp#:kkDiv:r2Clas) pep00;
      //*** End   Chg *** 09/27/19 **********************************
      //*** Begin chg *** 03/06/19 **********************************       *********
      //*** If Not %Found(wmepl);
       If Not %Found(wmepl1);
      //*** End chg *** 03/06/19 **********************************         *********
          errValue  = 'Class Code-' + %EditC(r2Clas:'X');
          errReason = err(38);
          rcd02Err = rcd02Err + 1;
          ExSr PrintError;
       EndIf;
      /end-free
     c*** End   Add *** 02/04/19 ****************************************
      *
      * Verify Nature Code
     c                   IF        R2natr <> *blanks
     c*** Begin Chg *** 09/13/18 ****************************************
     c**********         movel(P)  r2natr        kkkey
      /free
       If %Len(%Trim(R2natr)) = 1;
          R2natr = '0' + %Trim(R2natr);
       EndIf;
       kkKey = R2natr;
      /End-free
     c*** End   Chg *** 09/13/18 ****************************************
     c     tbky          chain     pna01                              60
     c                   IF        *IN60
     c                   movel(P)  'Nature-'     errValue
     c                   cat       r2natr:0      errValue
     c                   move(p)   err(23)       errReason
     c*****              eval      error02 = *on
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   Endif                                                  *IN60 = *ON
     c                   EndIF                                                  R2natr <> Blanks
      *
      * Verify Part Code
     c                   IF        R2part <> *blanks
     c*** Begin Chg *** 09/13/18 ****************************************
     c**********         movel(P)  r2part        kkkey
      /free
       If %Len(%Trim(R2part)) = 1;
          R2part = '0' + %Trim(R2part);
       EndIf;
       kkKey = R2part;
      /End-free
     c*** End   Chg *** 09/13/18 ****************************************
     c     tbky          chain     ppa02                              60
     c                   IF        *IN60
     c                   movel(P)  'Part -'      errValue
     c                   cat       r2part:0      errValue
     c                   move(p)   err(23)       errReason
     c*****              eval      error02 = *on
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   Endif                                                  *IN60 = *ON
     c                   EndIF                                                  R2Part <> *blanks
      *
     c                   IF        R2caus <> *blanks
      * Verify Cause Code
     c*** Begin Chg *** 09/13/18 ****************************************
     c**********         movel(P)  r2caus        kkkey
      /free
       If %Len(%Trim(R2caus)) = 1;
          R2caus = '0' + %Trim(R2caus);
       EndIf;
       kkKey = R2caus;
      /End-free
     c*** End   Chg *** 09/13/18 ****************************************
     c     tbky          chain     pca01                              60
     c                   IF        *IN60
     c                   movel(P)  'Cause-'      errValue
     c                   cat       r2caus:0      errValue
     c*****              eval      error02 = *on
     c                   move(p)   err(23)       errReason
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   Endif                                                  *IN60 = *ON
     c                   EndIF                                                  R2Cause
      *
      * Verify Loss Coverage Code
     c                   If        r2lscv <> *blanks
     c                   move(p)   'LC'          kktbid
     c                   movel(P)  r2lscv        kkkey
     c     gtky          chain     pgt01                              60
     c                   IF        *IN60
     c     r2jrst        cat       'LC':0        kktbid
     c     gtky          chain     pgt01                              60
     c                   IF        *IN60
     c*****              eval      error02 = *on
     c                   movel(P)  'Loss CV-'    errValue
     c                   cat       r2lscv:0      errValue
     c                   move(p)   err(23)       errReason
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   Endif                                                  *IN60 = *ON
     c                   Endif                                                  *IN60 = *ON
     c                   endif
      *
     c                   IF        r2ltfl <> 'M' AND r2ltfl <> 'I' AND
     c                             r2ltfl <> 'R'
     c*****              eval      error02 = *on
     c                   movel(P)  'Lost Time-'  errValue
     c                   cat       r2ltfl:0      errValue
     c                   move(p)   err(25)       errReason
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   EndIF
      *
     c*** Begin Chg *** 09/13/18 **************************************
     c**********         IF        r2sex <> 'M' AND r2sex <> 'F' AND
     c**********                   r2sex  <> ' '
     c                   If        r2sex <> 'M' And R2sex <> 'F' And
     c                             r2sex <> 'U' And R2sex <> ' '
     c*** End   Chg *** 09/13/18 **************************************
     c                   movel(P)  'Gender-'     errValue
     c                   cat       r2sex:0       errValue
     c*****              eval      error02 = *on
     c                   move(p)   err(24)       errReason
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   EndIF
      *
     c                   IF        r2msts <> 'M' AND r2msts <> 'K' AND
     c                             r2msts <> 'S' AND r2msts <> 'U' AND
     c                             r2msts <> ' '
     c                   movel(P)  'Mar Sts-'    errValue
     c                   cat       r2msts:0      errValue
     c*****              eval      error02 = *on
     c                   move(p)   err(24)       errReason
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   EndIF
      *
      * Verify Reserve Code
     c                   movel(P)  r2rt01        kkkey
     c     tbky          chain     prt01                              60

     c                   if        NOT error02

      * Chain back to claims file when update.

     c                   if        chgClaim
     c*** Begin chg *** 05/12/008 ******************************************************************
     c**** cmky          chain     pcm01                              60
     c     cmky2         chain     pcm02                              60
     c*** End chg ***** 05/12/08 ****************************************************
      *** Begin Delete *** 06/07/19 ***************************************
     c***  cdky          chain     pcd01                              99
      *** End   Delete *** 06/07/19 ***************************************
     c                   endif

      *** Begin Add    *** 06/07/19 ***************************************
     c     cdky          chain     pcd01                              99
      *** End   Add    *** 06/07/19 ***************************************
      *  Load Claim fields
      *
     c                   eval      CMCO#  = kkco#
     c                   eval      CMFND  = kkfnd
     c                   eval      CMFYR  = kkfyr
     c                   eval      CMEMP# = kkemp#
     c                   eval      CMDIV  = kkdiv
     c                   eval      CMPDS# = R2CLM#
      *** Beg Add *** 04/20/10 ****************************************
     c*** Begin Chg *** 05/30/19 **************************************
     c**********         IF        cltid = '114'
     c                   IF        cltid = '114' Or cltid = '150'
     c*** End   Chg *** 05/30/19 **************************************
     c                   eval      CMACL# = R1CLM#
     c                   endif
      *** End Add *** 04/20/10 ****************************************
     c                   move      r1acdt        zzdate
     c                   exsr      editDate
     c                   z-add     x7date        cmacdt
     c                   eval      cmSS#  = kkSS#
     c                   eval      cmfsn  = r2fsn
     c                   eval      cmlsn  = r2lsn
     c                   eval      cmmid  = r2mid
     c                   eval      cmltfl = r2ltfl
     c                   eval      cmstat = r2stat
     c                   move(p)   r2acds        cmacds
     c                   move(P)   r2acd2        cmacd2
     c                   eval      cmnatr = r2natr
     c                   eval      cmpart = r2part
     c                   eval      cmcaus = r2caus
     c                   eval      cmlscv = r2lscv
     c                   eval      cmcovt = r2covt
     c                   eval      cmjrst = r2jrst
      //*** Beg add *** 05/08/25 ******************************************************
     c                   eval      ctxjrst = cmjrst
      //*** End add *** 05/08/25 ******************************************************
     c                   eval      cmdept = r2dept
     c**********         eval      cmclm# = r2cmcl
     c                   eval      cmclas = r2clas
     c                   z-add     r2aww         CMAWW
     c                   eval      cmcvfl = r2cvfl
     c                   eval      cmsubr = r2subr
     c                   eval      cmdny  = r2dny
     c                   eval      cmmctp = r2mctp
     c                   eval      cmvorc = r2vorc
      *
      *** Begin add *** 09/20/07 ****************************************************
     c                   testn                   r2cata               98
     c   98              move      r2cata        cmcata
     c                   eval      cmlsrv = r2lsrv
     c                   eval      cmncci = r2ncci
     c*** Begin Add *** 01/22/19 *********************************
     c                   eval      CMocDs = R2ocDs
     c*** End   Add *** 01/22/19 *********************************
     C*** Begin Add *** 09/24/20 *****************************************
     C                   If        cltID <> '089'
     C*** End   Add *** 09/24/20 *****************************************
     c                   eval      cmclex = r2clex
     C*** Begin Add *** 09/24/20 *****************************************
     C                   EndIf
     C*** End   Add *** 09/24/20 *****************************************
     c                   eval      cmsupr = r2supr
     c                   eval      cmctg1 = r2ctg1
      *** End add ***** 09/20/07 ****************************************************
     c*** Begin add *** 03/17/08 ****************************************************
     c                   testn                   r2ipct               9897
     c   98              move      r2ipct        cmipct
     c*** End add ***** 03/17/08 ****************************************************
     c*** Begin Add *** 05/27/21 *******************************************
       // Populate 3rd Claim #
       If cltID = '150';
          CMjrC# = R2jrC#;
       EndIf;
     c*** End   Add *** 05/27/21 *******************************************
      *
     c                   eval      cmrt01 = r2rt01
     c                   z-add     r2rr01        cmrr01
     c                   z-add     r2ar01        cmar01
     c                   eval      cmrt02 = r2rt02
     c                   z-add     r2rr02        cmrr02
     c                   z-add     r2ar02        cmar02
     c                   eval      cmrt03 = r2rt03
     c                   z-add     r2rr03        cmrr03
     c                   z-add     r2ar03        cmar03
     c                   eval      cmrt04 = r2rt04
     c                   z-add     r2rr04        cmrr04
     c                   z-add     r2ar04        cmar04
     c                   eval      cmrt05 = r2rt05
     c                   z-add     r2rr05        cmrr05
     c                   z-add     r2ar05        cmar05
     c                   eval      cmrt06 = r2rt06
     c                   z-add     r2rr06        cmrr06
     c                   z-add     r2ar06        cmar06
     c                   eval      cmrt07 = r2rt07
     c                   z-add     r2rr07        cmrr07
     c                   z-add     r2ar07        cmar07
     c                   eval      cmrt08 = r2rt08
     c                   z-add     r2rr08        cmrr08
     c                   z-add     r2ar08        cmar08
     c                   eval      cmrt09 = r2rt09
     c                   z-add     r2rr09        cmrr09
     c                   z-add     r2ar09        cmar09
     c                   eval      cmrt10 = r2rt10
     c                   z-add     r2rr10        cmrr10
     c                   z-add     r2ar10        cmar10
      *
      * Date Fields
     c                   move      R2dtob        zzdate
     c                   exsr      editDate
     c                   z-add     x7Date        CDdtob
     c                   move      R2rcvd        zzdate
     c                   exsr      editDate
     c                   z-add     x7Date        cmrcvd
     c                   move      R2rpdt        zzdate
     c                   exsr      editDate
     c                   z-add     x7Date        cmDTRP
     c                   move      R2DOD         zzdate
     c                   exsr      editDate
     c                   z-add     x7Date        cmdod
     c                   move      R2hidt        zzdate
     c                   exsr      editDate
     c                   z-add     x7Date        cmhidt
     c                   move      R2dtlw        zzdate
     c                   exsr      editDate
     c                   z-add     x7Date        cmdtlw
      * If R2Idt is Invalid use System Date.
     c                   move      R2idt         zzdate
     c                   exsr      editDate
     c                   IF        %error
     c                   move(p)   err(28)       errReason
     c                   eval      Rcd02Err = Rcd02Err + 1
     c                   exsr      printError
     c                   eval      x7Date = dtCYMD(dsdate)
     c                   EndIF
      //*** Beg add *** 05/08/25 ******************************************************
     c                   exsr      PopIDCfld
     c                   If        R2rtnwrk <> 0
     c                   exsr      WriteWMWHP
     c                   Endif
      //*** End add *** 05/08/25 ******************************************************
     c*
     c                   z-add     x7Date        cmidt
     c                   z-add     x7Date        cdidt
      * WMCDP fields
     c                   eval      cdSS#  = kkss#
     c*** Begin add *** 03/26/08 ******************************************************
     c*** Begin Del *** 05/23/11 *******************************************
     c**********         if        errcnam = *off
     c*** End   Del *** 05/23/11 *******************************************
     c*** End add ***** 03/26/08 ******************************************************
     c                   eval      cdfsn  = r2fsn
     c                   eval      cdlsn  = r2lsn
     c                   eval      cdmid  = r2mid
     c                   eval      cdadr1 = r2adr1
     c                   eval      cdadr2 = r2adr2
     c                   eval      cdcty  = r2cty
     c                   eval      cdst   = r2st
     c                   eval      cdctry = r2ctry
     c                   eval      cdadr3 = r2adr3
     c                   move      r2zip         cdzip
     c                   eval      cdsex  = r2sex
     c                   eval      cdmsts = r2msts
     c*** Begin add *** 03/26/08 ******************************************************
     c*** Begin Del *** 05/23/11 *******************************************
     c**********         endif
     c*** End   Del *** 05/23/11 *******************************************
     c*** End add ***** 03/26/08 ******************************************************
      *
     c                   eval      rcd02Cnt = (rcd02Cnt + 1)
     c                   exsr      writeWMCMP
      *
      *
     c                   exsr      writeWMCDP
      *
      *
      * Originally added to create Claim History Record, but will not use in future
      * because the VAUM1 file should contain these types of records.
     c***********        eval      chocst = r2stat
     c***********        movel(P)  onAddClm      chexpl
     c***********        exsr      writeWMCHP
      *
     c                   endif                                                  NOT error02
      *
     c                   endsr                                                  claimEdit
     c/Eject
      *=============================================================================================
      * pymtEdit subroutine - Edits claim payment records - Type 03
      *=============================================================================================
     c     pymtEdit      begsr
      *
      *
     c                   eval      error03 = *off
     c                   eval      rec_type = 'Rcrd 03'
      *
     c                   do
      *
      * Check dates for Invalid dates
      *
      * Verify Transaction Date
     c                   move      r3trdt        zzdate
     c                   exsr      editDate
     c                   if        errorFlag = *on
     c                   eval      error03 = *on
     c                   movel(P)  err(15)       errReason
     c                   movel     'Trns Dte'    errReason
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   else
     c                   z-add     x7date        xxtrdt
     c                   endif
      * Verify Post Date
     c*** Beg add *** 03/06/19 *******************************************  *********
     c                   if        r3podt='00000000' or r3podt=*blanks
     c                   move      r3trdt        r3podt
     c                   endif
     c*** End add *** 03/06/19 *******************************************  *********
     c                   move      r3PODT        zzdate
     c                   exsr      editDate
     c                   if        errorFlag = *on
     c*** Begin Add *** 08/12/19 ******************************************
     C                   If        cltID <> '150'
     c*** End   Add *** 08/12/19 ******************************************
     c                   eval      error03 = *on
     c*** Begin Add *** 08/12/19 ******************************************
     C                   EndIf
     c*** End   Add *** 08/12/19 ******************************************
     c                   movel(P)  err(15)       errReason
     c                   movel     'Post Dte'    errReason
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   else
     c                   z-add     x7date        xxpodt
     c                   endif
      * Verify From Date
     c                   move      r3frdt        zzdate
     c                   exsr      editDate
     c                   if        errorFlag = *on
     c*** Begin Add *** 08/12/19 ******************************************
     C                   If        cltID <> '150'
     c*** End   Add *** 08/12/19 ******************************************
     c                   eval      error03 = *on
     c*** Begin Add *** 08/12/19 ******************************************
     C                   EndIf
     c*** End   Add *** 08/12/19 ******************************************
     c                   movel(P)  err(15)       errReason
     c                   movel     'From Dte'    errReason
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   else
     c                   z-add     x7date        xxfrdt
     c                   endif
      * Verify To Date
     c                   move      r3todt        zzdate
     c                   exsr      editDate
     c                   if        errorFlag = *on
     c*** Begin Add *** 08/12/19 ******************************************
     C                   If        cltID <> '150'
     c*** End   Add *** 08/12/19 ******************************************
     c                   eval      error03 = *on
     c*** Begin Add *** 08/12/19 ******************************************
     C                   EndIf
     c*** End   Add *** 08/12/19 ******************************************
     c                   movel(P)  err(15)       errReason
     c                   movel     'To Date '    errReason
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   else
     c                   z-add     x7date        xxtodt
     c                   endif
      * Verify Transaction type
     c*** beg add *** 08/07/09 *************************************   ***
     c                   If        r3type = 'A' AND CLTID = '132'
     c                   else
     c*** end add *** 08/07/09 *************************************   ***
     c                   If        r3type <> 'P' AND r3type <> 'R' AND
     c                             r3type <> 'S' AND r3type <> 'V' AND
     c                             r3type <> 'Z'
     c                   eval      error03 = *on
     c                   movel(P)  err(20)       errReason
     c                   MOVEL(P)  'TRAN TP-'    errValue
     c                   cat       r3type:0      errValue
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   endif
     c*** beg add *** 08/07/09 *************************************   ***
     c                   endif
     c*** end add *** 08/07/09 *************************************   ***
      * Verify Reserve Code
     c*** beg chg ****** 06/12/07 ****************************************
     c********           If        r3recd <> '11' AND r3recd <> '12' AND
     c********                     r3recd <> '13' AND r3recd <> '14' AND
     c********                     r3recd <> '16'
     c* Validate reserve code from Reserve type code table
     c                   movel(P)  r3recd        kkkey
     c     tbky          chain     prt01
     c                   if        not %found
     c*** end chg ****** 06/12/07 ****************************************
     c                   eval      error03 = *on
     c                   movel(P)  err(20)       errReason
     c                   MOVEL(P)  'RSV CDE-'    errValue
     c                   cat       r3recd:0      errValue
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   endif
      *
      * Verify Payment Value 1 Code
     c                   movel(P)  r3br2         kkkey
     c     tbky          chain     pb201                              60
     c                   if        *IN60 AND r3br2 <> *BLANKS
     c                   eval      error03 = *on
     c                   movel(P)  err(20)       errReason
     c                   MOVEL(P)  'BR2 Val-'    errValue
     c                   cat       R3BR2:0       errValue
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   endif

     c******* BEG ADD **** 08/01/07 **************************************
      *
      *  Add an Edit to Prevent Duplicate Payments
      *
     c                   eval      xxfeinc = %subst(r3link:1:9)
     c                   eval      xxmodc  = %subst(r3link:10:3)
     c*
     c                   move      xxfeinc       xxfeinn           9 0
     c                   move      xxmodc        xxmodn            3 0

     c*** Begin Add *** 08/12/19 *************************************
     c                   eval      wamt = *zeros
     c                   if        r3amts = '-'
     c                   eval      wamt = r3amt * (-1)
     c                   else
     c                   eval      wamt = r3amt
     c                   endif
     c*** End   Add *** 08/12/19 *************************************
     c*
     c     paky1         chain     wdpal                              61
     c                   dow       *in61 = *off
     c*
     c*** beg add *** 08/07/09 *************************************   ***
     c*** Beg Chg *** 06/28/10 *************************************
     c**********         If        r3type = 'A' AND CLTID = '132'
     c                   If        CLTID = '132'
     c*** End Chg *** 06/28/10 *************************************
     c*** Beg Add *** 06/28/10 *************************************
     c*
     c** If Amount sign is negative multiply the amount by -1 for comparison
     c*
     c                   eval      wamt = *zeros
     c                   if        r3amts = '-'
     c                   eval      wamt = r3amt * (-1)
     c                   else
     c                   eval      wamt = r3amt
     c                   endif
     c*
     c                   Select
     c*
     c** If its a Group check
     c*
     c                   when      r3pgtp = 'G'
     c*
     c                   eval      weck# = *zeros
     c                   move      r3eck#        weck#
     c*
     c                   if        weck#  = paeck# and
     c                             r3chk# = pachk# and
     c                             pafein = xxfeinn and
     c                             pamod  = xxmodn and
     c                             pafrdt = xxfrdt and
     c                             patodt = xxtodt and
     c                             patype = r3type and
     c                             pabr2  = r3br2  and
     c                             paamt  = wamt
     c*
     c                   eval      error03 = *on
     c                   movel(p)  err(33)       errReason
     c                   movel(p)  R3chk#        errValue
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   leave
     c                   endif
     c*
     c** when a non group check
     c*
     c                   when      r3pgtp = ' '
     c                   if        r3chk# = pachk# and
     c                             pafein = xxfeinn and
     c                             pamod  = xxmodn and
     c                             pafrdt = xxfrdt and
     c                             patodt = xxtodt and
     c                             patype = r3type and
     c                             pabr2  = r3br2  and
     c                             paamt  = wamt
     c*
     c                   eval      error03 = *on
     c                   movel(p)  err(33)       errReason
     c                   movel(p)  R3chk#        errValue
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   leave
     c*
     c                   endif
     c*
     c                   endsl
     c*** End Add *** 06/28/10 *************************************
     c*
     c                   else
     c*** end add *** 08/07/09 *************************************   ***
     c                   if        r3chk# = pachk# and                          Duplicate Check
     c                             pafein = xxfeinn and
     c                             pamod  = xxmodn and
     c                             pafrdt = xxfrdt and
     c                             patodt = xxtodt and
     c                             patype = r3type and
     c*** Begin add *** 12/11/07 *****************************************
     c                             pabr2  = r3br2  and
     c*** End add ***** 12/11/07 *****************************************
     c*** Begin Chg *** 08/12/19 *************************************
     c**********                   paamt  = r3amt
     c                             paamt  = wamt
     c*** End   Chg *** 08/12/19 *************************************
     c*
     c*** Begin Add *** 08/12/19 ******************************************
     C                   If        cltID <> '150'
     c*** End   Add *** 08/12/19 ******************************************
     C*** Begin Add *** 09/28/20 ******************************************
     C                             And cltID <> '089'
     C*** End   Add *** 09/28/20 ******************************************
     c                   eval      error03 = *on
     c*** Begin Add *** 08/12/19 ******************************************
     C                   EndIf
     c*** End   Add *** 08/12/19 ******************************************
     c                   movel(p)  err(33)       errReason
     c                   movel(p)  R3chk#        errValue
     c                   eval      Rcd03Err = Rcd03Err + 1
     c                   exsr      printError
     c                   leave
     c                   endif                                                  Duplicate Check
     c*** beg add *** 08/07/09 *************************************   ***
     c                   endif
     c*** end add *** 08/07/09 *************************************   ***
     c     paky1         reade     wdpal                                  61
     c                   enddo                                                  *IN61
     c******* END ADD **** 08/01/07 ***************************************

     c                   If        error03 = *ON
     c                   leave
     c                   endif
     c
      *
      *  If No Errors, then write Payment Record
      *
     c                   move      *HiVal        xxseq#
     c     paky          setgt     ppa01
     c     paky1         readpe    ppa01                                  60
     c   60              z-add     *zeros        xxseq#
     c  n60              z-add     paseq#        xxseq#
     c                   add       1             xxseq#

     c                   clear                   ppa01
     c                   eval      PACO#  = KKCO#
     c                   eval      PAFND  = KKFND
     c                   eval      PAFYR  = KKFYR
     c                   eval      PACASE = kkCASE

     c                   eval      paseq# = xxseq#
     c*** Begin Chg *** 09/13/18 *******************************
     c**********         eval      pachk# = r3Chk#
     c                   Monitor
     c                   eval      pachk# = r3Chk#
     c                   On-Error
     C                   eval      pachk# = 0
     c                   EndMon
     c*** End   Chg *** 09/13/18 *******************************
     c                   eval      patrdt = xxtrdt
     c                   eval      papodt = xxpodt
     c                   eval      pafrdt = xxfrdt
     c                   eval      patodt = xxtodt
     c                   move      xxfeinn       pafein
     c                   move      xxmodn        pamod
      *
     c                   eval      paamt  = r3amt
     c                   If        r3AMTS = '-'
     c                   eval      paamt  = (paamt * -1)
     c                   endif

     c                   move      *blanks       kkkey
     c     tbky          setll     ppc01                              60
     c     kkco#         reade     ppc01                                  60
     c                   dow       *IN60 = *off
     c                   if        pctrtp = r3type and pcrecd = r3recd
     c                             or pctrtp = r3type and r3type = 'V'
     c                             or pctrtp = r3type and r3type = 'S'
     c                             or pctrtp = r3type and r3type = 'Z'
     c                   leave
     c                   endif
     c     kkco#         reade     ppc01                                  60
     c                   enddo

     c                   movel     pckey         patrcd
     c*******            eval      patype = pctrtp
     c                   eval      patype = r3type
     c                   eval      padesc = pcdesc
      *
     c                   If        patype = 'R'
     c                   eval      padesc = 'Recovery'
     c                   endif
      *
     c*********          eval      parecd = pcrecd
     c                   eval      parecd = r3recd
     c                   eval      paretp = rttype
     c                   move      r3br2         pabr2
      *
     c                   If        cmltfl = 'I'
     c                   eval      pabr7 = 'LT'
     c                   else
     c                   eval      pabr7 = 'NL'
     c                   endif
      *
     c                   eval      PAUSER = Q1USER
     c                   eval      PAPGM  = Q1PGM
     c                   eval      PATIME = dstime
     c                   eval      PADT   = dtCYMD(dsdate)
     c                   eval      PAIUSR = Q1USER
     c                   eval      PAIPGM = Q1PGM
     c                   eval      PAITIM = dstime
     c                   eval      PAIDT  = dtCYMD(dsdate)
      *
      * Total up all payment amounts.
     c                   eval      payAmtTot = payAmtTot + paamt
      *** beg add ****** 10/23/08 ****************************************
      *
      * Load Group/Non Group
      *
     c                   if        r3pgtp = 'G'
     c                   move      r3pgtp        papgtp
     c                   else
     c                   move      *blank        papgtp
     c                   endif
      *
      * Check 'Check number' numeric or not
      *
     c                   TestN                   R3ECK#               25
     c                   if        *in25 = *on
     c                   Move      R3ECK#        PAECK#
     c                   endif
      *** end add ****** 10/23/08 ****************************************
      *** Beg add ****** 05/08/25 ****************************************
         PAlmps = R3lmps;
      *** End add ****** 05/08/25 ****************************************

     c   90              write     pPA01
       //*** Beg Add *** 05/08/25 ******************************************
        //If isModEnabled('IDC') and PAretp = 'C';    //Write Idc Trans Log Rec
        If isModEnabled('IDC');
           intRRN = dbPPArrn;
           WriteTxLog(intRRN);
        Endif;
       //*** End Add *** 05/08/25 ******************************************

      * Create Check View Record
     c                   clear                   PRP03

     c                   eval      RPCO#  = KKCO#
     c                   eval      RPFND  = KKFND
     c                   eval      RPFYR  = KKFYR
     c                   eval      RPCASE = kkCASE
     c                   eval      RPUSER = Q1USER
     c                   eval      RPPGM  = Q1PGM
     c                   eval      RPTIME = dstime
     c                   eval      RPDT   = dtCYMD(dsdate)

     c                   eval      rptrdt = patrdt
     c                   eval      rpchk# = pachk#
     c                   eval      rppnam = r3pnam
     c                   eval      rpseq# = paseq#
     c                   eval      rpstat = 'A'

     c                   eval      rcd03Cnt = (rcd03Cnt + 1)
     c   90              write     PRP03

     c                   leave
     c                   enddo

     c                   endsr                                                  pymtEdit
     c/Eject
      *=============================================================================================
      * rhistEdit subroutine - Edits Reserve History Records - Type 04
      *=============================================================================================
     c     rhistEdit     begsr
      *
     c                   eval      error04 = *off
     c                   eval      rec_type = 'Rcrd 04'
      *
      * Verify Transaction Date
     c
     c                   MOVE      R4efdt        ZZDATE
     c                   exsr      editDate
     c                   if        errorFlag = *on
     c                   eval      error04 = *on
     c                   movel(P)  err(15)       errReason
     c                   movel     'Eff Date'    errReason
     c                   eval      Rcd04Err = Rcd04Err + 1
     c                   exsr      printError
     c                   eval      x7Date = dtCYMD(dsdate)
     c                   endif
     c                   z-add     x7date        rhefdt
      *
     c*** Beg add *** 09/13/18 *******************************************  *********
      /free
       // Convert the reserve Codes if needed
       // Bucket 1
       Select;
       When R4rt01 = '11';
          R4rt01 = 'ME';
       When R4rt01 = '12';
          R4rt01 = 'CO';
       When R4rt01 = '13';
          R4rt01 = 'EX';
       When R4rt01 = '14';
          R4rt01 = 'RE';
       When R4rt01 = '16';
          R4rt01 = 'LE';
       EndSl;
       // Bucket 2
       Select;
       When R4rt02 = '11';
          R4rt02 = 'ME';
       When R4rt02 = '12';
          R4rt02 = 'CO';
       When R4rt02 = '13';
          R4rt02 = 'EX';
       When R4rt02 = '14';
          R4rt02 = 'RE';
       When R4rt02 = '16';
          R4rt02 = 'LE';
       EndSl;
       // Bucket 3
       Select;
       When R4rt03 = '11';
          R4rt03 = 'ME';
       When R4rt03 = '12';
          R4rt03 = 'CO';
       When R4rt03 = '13';
          R4rt03 = 'EX';
       When R4rt03 = '14';
          R4rt03 = 'RE';
       When R4rt03 = '16';
          R4rt03 = 'LE';
       EndSl;
       // Bucket 4
       Select;
       When R4rt04 = '11';
          R4rt04 = 'ME';
       When R4rt04 = '12';
          R4rt04 = 'CO';
       When R4rt04 = '13';
          R4rt04 = 'EX';
       When R4rt04 = '14';
          R4rt04 = 'RE';
       When R4rt04 = '16';
          R4rt04 = 'LE';
       EndSl;
       // Bucket 5
       Select;
       When R4rt05 = '11';
          R4rt05 = 'ME';
       When R4rt05 = '12';
          R4rt05 = 'CO';
       When R4rt05 = '13';
          R4rt05 = 'EX';
       When R4rt05 = '14';
          R4rt05 = 'RE';
       When R4rt05 = '16';
          R4rt05 = 'LE';
       EndSl;
      /end-free
     c*** End add *** 09/13/18 *******************************************  *********
      * Load the reserve Codes
     c                   eval      rhrt01 = r4rt01
     c                   eval      rhrt02 = r4rt02
     c                   eval      rhrt03 = r4rt03
     c                   eval      rhrt04 = r4rt04
     c                   eval      rhrt05 = r4rt05
     c                   eval      rhrt06 = r4rt06
     c                   eval      rhrt07 = r4rt07
     c                   eval      rhrt08 = r4rt08
     c                   eval      rhrt09 = r4rt09
     c                   eval      rhrt10 = r4rt10
      *
      * Load the reserve amount changes
     c                   eval      rhsu01 = r4su01
     c                   eval      rhsu02 = r4su02
     c                   eval      rhsu03 = r4su03
     c                   eval      rhsu04 = r4su04
     c                   eval      rhsu05 = r4su05
     c                   eval      rhsu06 = r4su06
     c                   eval      rhsu07 = r4su07
     c                   eval      rhsu08 = r4su08
     c                   eval      rhsu09 = r4su09
     c                   eval      rhsu10 = r4su10
      *
     c                   IF        r4sn01 = '-'
     c                   eval      rhsu01 =  (rhsu01 * -1)
     c                   endif
     c                   IF        r4sn02 = '-'
     c                   eval      rhsu02 =  (rhsu02 * -1)
     c                   endif
     c                   IF        r4sn03 = '-'
     c                   eval      rhsu03 =  (rhsu03 * -1)
     c                   endif
     c                   IF        r4sn04 = '-'
     c                   eval      rhsu04 =  (rhsu04 * -1)
     c                   endif
     c                   IF        r4sn05 = '-'
     c                   eval      rhsu05 =  (rhsu05 * -1)
     c                   endif
     c                   IF        r4sn06 = '-'
     c                   eval      rhsu06 =  (rhsu06 * -1)
     c                   endif
     c                   IF        r4sn07 = '-'
     c                   eval      rhsu07 =  (rhsu07 * -1)
     c                   endif
     c                   IF        r4sn08 = '-'
     c                   eval      rhsu08 =  (rhsu08 * -1)
     c                   endif
     c                   IF        r4sn09 = '-'
     c                   eval      rhsu09 =  (rhsu09 * -1)
     c                   endif
     c                   IF        r4sn10 = '-'
     c                   eval      rhsu10 =  (rhsu10 * -1)
     c                   endif
      *
      * Total up all reserve amounts after apply the sign fields
     c                   eval      suTOT = suTOT + rhsu01
     c                   eval      suT01 = suT01 + rhsu01
     c                   eval      suTOT = suTOT + rhsu02
     c                   eval      suT02 = suT02 + rhsu02
     c                   eval      suTOT = suTOT + rhsu03
     c                   eval      suT03 = suT03 + rhsu03
     c                   eval      suTOT = suTOT + rhsu04
     c                   eval      suT04 = suT04 + rhsu04
     c                   eval      suTOT = suTOT + rhsu05
     c                   eval      suT05 = suT05 + rhsu05
     c                   eval      suTOT = suTOT + rhsu06
     c                   eval      suT06 = suT06 + rhsu06
     c                   eval      suTOT = suTOT + rhsu07
     c                   eval      suT07 = suT07 + rhsu07
     c                   eval      suTOT = suTOT + rhsu08
     c                   eval      suT08 = suT08 + rhsu08
     c                   eval      suTOT = suTOT + rhsu09
     c                   eval      suT09 = suT09 + rhsu09
     c                   eval      suTOT = suTOT + rhsu10
     c                   eval      suT10 = suT10 + rhsu10
      *
     c                   eval      zztime = zztime +1
     c                   eval      rhTIME = zztime
      *
     c                   eval      rhCO#  = kkco#
     c                   eval      rhFND  = kkfnd
     c                   eval      rhFYR  = kkfyr
     c                   eval      rhCASE = kkCASE
     c                   eval      rhCLST = CMSTAT
     c                   movel(p)  rsvHistory    rhexpl
      *
     c                   eval      RHUSER = Q1USER
     c                   eval      RHPGM  = Q1PGM
     c                   eval      RHTIME = dstime
     c                   eval      RHDT   = dtCYMD(dsdate)
     c                   eval      RHIUSR = Q1USER
     c                   eval      RHITIM = dstime
     c                   eval      RHIPGM = Q1PGM
     c                   eval      RHIDT  = dtCYMD(dsdate)
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************

     c                   IF        NOT error04
     c                   eval      rcd04Cnt = (rcd04Cnt + 1)
     c   90              write     prh01
     c                   endif

     c                   endsr                                                  rhistEdit
      *=============================================================================================
      * StatEdit subroutine - Status Change OPEN/CLOSE Claim Records - Type 05
      *=============================================================================================
     c     statEdit      begsr
      *
      * Get the case information to create change record.
      *
     c                   eval      error05 = *off
     c                   eval      rec_type = 'Rcrd 05'
      *
     c                   IF        r5clm#  <> xxclm#
     c                   eval      error05 = *on
     c                   move(p)   err(18)       errReason
     c                   eval      Rcd05Err = Rcd05Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      *
     c                   IF        R5OCST  <> 'O' and R5OCST <> 'C'
     c                   eval      error05 = *on
     c                   move(p)   err(19)       errReason
     c                   eval      Rcd05Err = Rcd05Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      *
     c                   IF        R5efdt  = *blanks
     c                   eval      error05 = *on
     c                   move(p)   err(19)       errReason
     c                   eval      Rcd05Err = Rcd05Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      *
     c                   move      r5efdt        zzdate
     c                   exsr      editDate
     c                   if        errorFlag
     c                   eval      error05 = *on
     c                   move(p)   err(19)       errReason
     c                   eval      Rcd05Err = Rcd05Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      *
     c                   IF        NOT error05
      *
     c                   clear                   pch01
      *
     c                   eval      chco#  = kkco#
     c                   eval      chfnd  = kkfnd
     c                   eval      chfyr  = kkfyr
     c                   eval      chcase = kkcase
     c                   eval      chocst = r5ocst
     c                   eval      CHEFDT = x7Date
      *
     c                   IF        r5ocst = 'O'
     c                   movel(P)  OpnClaim      chexpl
     c                   else
     c                   movel(P)  ClsClaim      chexpl
     c                   endif
      *
     c                   eval      rcd05Cnt = (rcd05Cnt + 1)
     c                   Exsr      writeWMCHP
     c                   endif                                                  NOT Error05
      *

     c                   endsr                                                  statEdit
      *=============================================================================================
      * chngEdit subroutine - Edits Change Claim or Delete Claim - Type 06
      *=============================================================================================
     c     chngEdit      begsr
      *
     c                   eval      error06 = *off
     c                   eval      rec_type = 'Rcrd 06'
      *
     c                   if        R6trtp  <> 'C' and R6trtp <> 'D' AND
     c                             R6trtp  <> 'c' and R6trtp <> 'd'
     c                   eval      error06 = *on
     c                   move(p)   R6trtp        errValue
     c                   move(p)   err(29)       errReason
     c                   exsr      printError
     c                   endif                                                  %error
      *
      * Check for same claim Number as record type 01
      *
     c                   IF        r6clm#  <> xxclm#
     c                   eval      error06 = *on
     c                   move(p)   err(18)       errReason
     c                   exsr      printError
     c                   endif                                                  %error
      *
     c                   if        R6trdt = *blanks
     c                   eval      error06 = *on
     c                   movel(P)  *blanks       errValue
     c                   move(p)   err(32)       errReason
     c                   exsr      printError
     c                   endif                                                  %error
      *
     c******* BEG ADD **** 08/01/07 ***************************************
      *
      *    Accident Date Update for 06 Record
      *
     c                   if        r6acdt = *blanks or r6acdt = *zeros          Accident Date
     c                   eval      r6acdt = r1acdt
     c                   endif                                                  Accident Date
     c******* END ADD **** 08/01/07 ***************************************
     c*** Begin add *** 02/06/08 ********************************************************
     c                   if        r6grp#  = *blanks or r6grp# = *zeros
     c                   eval      r6grp# = r1fnd
     c                   endif
     c                   if        r6pol# = *blanks or r6pol#  = *zeros
     c                   eval      r6pol# = r1cap#
     c                   endif
     c                   if        r6emp# = *blanks or r6emp#  = *zeros
     c                   eval      r6emp# = r1emp#
     c                   endif
     c*** Beg CHG ***** 10/12/09 ********************************************************
     c***************    if        r6div# = *blanks or r6div#  = *zeros
     c                   if        r6div# = *blanks
     c                   eval      r6div# = *zeros
     c*** End CHG ***** 10/12/09 ********************************************************
     c                   endif
     c                   if        r6ss# = *blanks or r6ss# = *zeros
     c                   eval      r6ss# = r1ss#
     c                   endif
      *
     c                   move      cmfnd         r1fnd
     c                   move      cmemp#        r1emp#
     c                   move      cmdiv         r1div
     c                   move      cmss#         r1ss#
     c     19000000      add       cmacdt        zzdate
     c                   move      zzdate        r1acdt
     c*** End add ***** 02/06/08 ********************************************************
      *
      *  Processing for a "change".
      *
     c                   if        R6trtp  = 'C' or R6trtp = 'c'
      *
     c                   IF        r6pol#  = r1cap#  AND r6ss# = r1ss#   AND
     c*** Begin add *** 02/06/08 ********************************************************
     c                             r6grp#  = r1fnd   AND r6emp# = r1emp#  AND
     c                             r6div#  = r1div   AND
     c*** End add ***** 02/06/08 ********************************************************
     c                             r6acdt  = r1acdt
     c                   eval      error06 = *on
     c                   move      *blanks       errValue
     c                   move(p)   err(31)       errReason
     c                   exsr      printError
     c                   endif
      *
     c                   movel(p)  r6pol#        kkcap#
     c     rpky          chain     prp01                              62
     c*** Begin add *** 04/08/09 ********************************************************
     c                   if        R6div# <> R1div
     c                   move      r6div#        rpdiv
     c                   endif
     c*** End add ***** 04/08/09 ********************************************************
     c                   if        *in62
     c*** Begin add *** 02/06/08 ********************************************************
     c                   move      r6grp#        kkfnd
     c                   move      r6emp#        kkemp#
     c                   move      r6div#        kkdiv
     c     emky          chain     pem01                              61
     c                   if        *in61
     c*** End add ***** 02/06/08 ********************************************************
     c                   eval      error06 = *on
     c                   movel(P)  r6pol#        errValue
     c                   move(p)   err(11)       errReason
     c                   exsr      printError
     c                   endif
     c*** Begin add *** 02/06/08 ********************************************************
     c                   else
     c                   move      rpfnd         kkfnd
     c                   move      rpemp#        kkemp#
     c                   move      rpdiv         kkdiv
     c     emky          chain     pem01                              61
     c                   endif
     c                   move      r6fnd         kkfnd
     c*** End add ***** 02/06/08 ********************************************************
     c                   endif
      *
     c                   move      r6acdt        zzdate
     c                   exsr      editDate
     c                   if        errorFlag
     c                   eval      error06 = *on
     c                   move      r6acdt        errValue
     c                   move(p)   err(15)       errReason
     c                   exsr      printError
     c                   endif                                                  %error
     c                   move      x7date        x7acdt
      *
     c                   if        error06
     c                   eval      Rcd06Err = Rcd06Err + 1
     c                   endif
      *
      * Do Claim Changes - If "C", call WA037R to change all records associated w/claim.
      *                  - IF "D", Call WA0070R04 to Delete claim records in all files.
      *
     c                   eval      cmco#  = kkco#
     c                   eval      cmfnd  = kkfnd
     c                   eval      cmfyr  = kkfyr
     c                   eval      cmcase = kkcase
     c
     c                   IF        NOT error06 and R6trtp = 'C'
     c                   eval      rcd06Cnt = (rcd06Cnt + 1)
     c   90              exsr      clmTrnsfr
     c                   endif

     c                   IF        NOT error06 and R6trtp = 'D'
     c                   eval      rcd06Cnt = (rcd06Cnt + 1)
     c                   move      cmco#         @@co#
     c                   move      cmfnd         @@fnd
      ****** Beg Add  02/02/11 ****************************************
     c                   z-add     cmemp#        @@cmemp#
      ****** End Add  02/02/11 ****************************************
     c                   move      cmfyr         @@fyr
     c                   move      cmcase        @@case
     c*** Begin Add *** 06/13/11 *******************************************
     c                   unlock    wmcml1
     c*** End   Add *** 06/13/11 *******************************************
     c   90              call      'WA037R04'    delParm
     c                   endif

     c                   endsr                                                  chngEdit
      *=============================================================================================
      * Claim Transfer subroutine.
      *=============================================================================================
     c     clmTrnsfr     begsr
     c*
     c*  If the SS# or accident date change, update the claim file and create a new
     c*  claimant demographics record, if one does not already exist.
     c*
     c                   if        r6ss# <> cmss# or x7acdt <> cmacdt
     c*
     c*** Begin chg *** 05/12/08 ****************************************************
     c**** cmky          chain     pcm01                              60
     c     cmky2         chain     pcm02                              60
     c*** End chg ***** 05/12/08 ****************************************************
     c                   if        not *in60
     c*
     c*** Begin add *** 04/15/09 ********************************************************
     c                   eval      savss# = cmss#
     c*** End add ***** 04/15/09 ********************************************************
     c                   eval      cmss#  = r6ss#
     c                   eval      cmacdt = x7acdt
     c*** Begin chg *** 05/12/08 ****************************************************
     c*********          update    pcm01
     c                   update    pcm02
     c*** End chg ***** 05/12/08 ****************************************************
     c*
     c*** Begin chg *** 04/15/09 ********************************************************
     c**********         if        r6ss# <> cmss#
     c                   if        savss# <> cmss#
     c*** End chg ***** 04/15/09 ********************************************************
     c     cdky          chain     pcd01                              60
     c                   eval      kkss#  = r6ss#
      /Free
        // Beg Add *** 12/17/15 *********************************************************
        // Beg Del *** 03/06/19 *********************************************************
        // feod WMCDL ;
        // End Del *** 03/06/19 *********************************************************
        // End Add *** 12/17/15 *********************************************************
      /End-free
     c     cdky          chain     pcd01                              60
     c                   if        *in60
     c                   eval      cdss#  = r6ss#
     c                   eval      cduser = Q1USER
     c                   eval      cdpgm  = Q1PGM
     c                   eval      cdtime = dstime
     c                   eval      cddt   = dtCYMD(dsdate)
     c                   eval      CDIUSR = Q1USER
     c                   eval      CDIPGM = Q1PGM
     c                   eval      CDITM  = dstime
     c                   eval      CDIDT  = dtCYMD(dsdate)
     c                   Write     pcd01
      /Free
        // Beg Add *** 12/17/15 *********************************************************
         feod WMCDL ;
        // End Add *** 12/17/15 *********************************************************
      /End-free
     c                   endif
     c                   endif
     c*
     c                   endif
     c*
     c                   endif
     c*
     c*  If the policy # changed or accident date changed, determine if the FYR needs to be
     c*  updated.
     c*
     c                   eval      xxfyr = cmfyr
     c                   if        emfnd <> cmfnd or x7acdt <> cmacdt
     c                             or ememp# <> cmemp# or emdiv <> cmdiv
     c                   eval      kkacdt = x7acdt
     c*** Begin Add *** 10/31/19 *******************************************
     c                   if        emfnd <> cmfnd
     c                   eval      kkfnd = emfnd
     c                   endif
     c*** End   Add *** 10/31/19 *******************************************
     c                   exsr      newFYR
     c                   endif
     c*
     c*  If the group # changed, retrieve a new case #.
     c*
     c                   eval      xxcase = cmcase
     c                   if        emfnd <> cmfnd
     c                   eval      kkfnd = emfnd
     c                   exsr      nextcase
     c                   endif
     c*
     c*  If the policy # changed, reverse the deductibles.
     c*
     c*** Begin Chg *** 11/03/22 *******************************************
     c**********         if        ememp# <> cmemp# or emdiv <> cmdiv
     c                   if        ememp# <> cmemp# or emdiv <> cmdiv or
     c                             emfnd  <> cmfnd
     c*** End   Chg *** 11/03/22 *******************************************
     c                   EXSR      RVDED
     c                   endif
     c*
     c                   if        emfnd <> cmfnd or ememp# <> cmemp#
     c                             or emdiv <> cmdiv or xxfyr <> cmfyr
     c                             or xxcase <> cmcase
     c*
     c                   move      cmco#         @@co#
     c                   move      cmfnd         @@ffnd
     c                   move      cmemp#        @@femp
     c                   move      cmdiv         @@fdiv
     c                   move      cmfyr         @@ffyr
     c                   move      cmcase        @@fcas
     c                   move      emfnd         @@tfnd
     c                   move      ememp#        @@temp
     c                   move      emdiv         @@tdiv
     c                   move      xxfyr         @@tfyr
     c                   move      xxcase        @@tcas
     c                   call      'WA037R'      chgParm
     c*
     c                   endif
     c*
     c*  Write a claim transfer record.
     c*
     c                   clear                   pct01
     c                   eval      ctco#  = kkco#
     c                   eval      ctfnd  = emfnd
     c                   eval      ctfyr  = xxfyr
     c                   eval      ctemp# = ememp#
     c                   eval      ctdiv  = emdiv
     c                   eval      ctcase = xxcase
     c                   eval      ctss#  = r6ss#
     c                   eval      ctclas = cmclas
     c                   eval      ctacdt = x7acdt
     c                   move      cmfnd         ctreq
     c                   eval      ctxfyr = cmfyr
     c                   eval      ctxemp = cmemp#
     c                   eval      ctxdiv = cmdiv
     c                   eval      ctxcas = cmcase
     c                   eval      ctxss# = cmss#
     c                   eval      ctxcls = cmclas
     c                   eval      ctxacd = cmacdt
     c                   eval      ctuser = q1user
     c                   eval      ctpgm  = q1pgm
     c                   eval      cttime = dstime
     c                   eval      ctdt   = dtCYMD(dsdate)
     c                   write     pct01
      //*** Beg add *** 05/08/25 ******************************************************
       qqPolId1 = %Editc(ctfnd:'X') + %Editc(ctxemp:'X') +
                  %Editc(ctxfyr:'X');

       qqPolId2 = %Editc(ctfnd:'X') + %Editc(ctemp#:'X') +
                  %Editc(ctfyr:'X');

       //WDELL/ELPFDT via ctco#/ctfnd/ctxemp/ctxdiv/ctxfyr
       chain (CTco#:CTfnd:CTxemp:CTxdiv:CTxfyr) pel00;
       If %found(wdell) and Elpfdt > *zero;
          qqPolEfdt1 = %Char(%date(ELpfdt:*cymd):*iso0);
       Else;
          qqPolEfdt1 = *Zeros;
       Endif;

       //WDELL/ELPFDT via ctco#/ctfnd/ctemp#/ctdiv/ctfyr
       chain (CTco#:CTfnd:CTemp#:CTdiv:CTfyr) pel00;
       If %found(wdell) and Elpfdt > *zero;
          qqPolEfdt2 = %Char(%date(ELpfdt:*cymd):*iso0);
       Else;
          qqPolEfdt2 = *zeros;
       Endif;

       //If Policy id's are not same / Policy effective dates are not same.
       If qqPolId1 <> qqPolId2 or qqPolEfDt1 <> qqPolEfDt2;
          isNewIdc = *on;
       Endif;

       // From and TO employer not same/ Division is not same.
       If ctxemp <> ctemp# or ctxdiv <> ctdiv;
          isNewIdc = *on;
       Endif;

       // From and To Accident is not same/ Case is not same.
       If ctxacd <> ctacdt or ctxcas <> ctcase;
          isNewIdc = *on;
       Endif;

       If isModEnabled('IDC') and isNewIdc = *on and
          isIdcState(CTco#:CTXjrst) = *on;

          // Load parameter for From Policy
          qqCo#1 = ctco#;
          //WDF2L/F2CAR# via ctco#/ctfnd
          Chain (CTco#:CTfnd) pf200;
          If %found(wdf2l);
             qqCar1 = %Editc(F2car#:'X');
          Endif;
          qqClmId1 = %Editc(CTxcas:'X');
          qqAcDt1  = %char(%date(%Editc(CTxacd:'X'):*cymd0):*iso0);

          // Load parameter for To Policy
          qqCo#2 = ctco#;
          //WDF2L/F2CAR# via ctco#/ctfnd
          Chain (CTco#:CTfnd) pf200;
          If %found(wdf2l);
             qqCar2 = %Editc(F2car#:'X');
          Endif;
          qqClmId2 = %Editc(CTcase:'X');
          qqAcDt2  = %char(%date(%Editc(CTacdt:'X'):*cymd0):*iso0);
          qqFyr2   = ctfyr;

          isIdcTrnOk =
          Transfer(qqCo#1:qqCar1:qqPolId1:qqPolEfdt1:qqClmId1:qqAcdt1:
                   qqCo#2:qqCar2:qqPolId2:qqPolEfdt2:qqClmId2:qqAcdt2:
                   qqFyr2);
       Endif;
      //*** End add *** 05/08/25 ******************************************************

     c                   endsr                                                  otherEdit

      *** Begin add *** 11/08/07 ****************************************************
      *=============================================================================================
      * dARTEdit1 subroutine - DARTS Claim Records - Type 07
      *=============================================================================================
     c     DARTEdit1     begsr
      *
      * Get the case information to create change record.
      *
     c                   eval      error07 = *off
     c                   eval      rec_type = 'Rcrd 07'
      *
     c                   IF        r7clm#  <> xxclm#
     c                   eval      error07 = *on
     c                   move(p)   err(18)       errReason
     c                   eval      Rcd07Err = Rcd07Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      * RMCD
     c                   IF        R7RMCD  = *blanks
     c                   eval      error07 = *on
     c                   move(p)   err(37)       errReason
     c                   eval      Rcd07Err = Rcd07Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      *
      *** Begin del *** 10/21/09 ****************************************************
      * SEQ# **********
     c*****************  IF        R7seq#  = *zeros
     c*****************  eval      error07 = *on
     c*****************  move(p)   err(36)       errReason
     c*****************  eval      Rcd07Err = Rcd07Err + 1
     c*****************  exsr      printError
     c*****************  endif                                                  %error
      *** End del *** 10/21/09 *******************************************  *********
      *
      * CDAT
     c                   IF        r7cdat  = *zeros
     c                   eval      error07 = *on
     c                   move(p)   err(35)       errReason
     c                   eval      Rcd07Err = Rcd07Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      *
     c                   move      r7cdat        zzdate
     c                   exsr      editDate
     c                   if        errorFlag
     c                   eval      error07 = *on
     c                   move(p)   err(19)       errReason
     c                   eval      Rcd07Err = Rcd07Err + 1
     c                   exsr      printError

     c                   else                                                   %error
     c                   eval      h7cdat = x7Date
     c                   endif                                                  %error
      *
      * DCDT
     c                   IF        R7dcdt  = *zeros
     c***                eval      error07 = *on
     c***                move(p)   err(35)       errReason
     c***                eval      Rcd07Err = Rcd07Err + 1
     c***                exsr      printError
     c***                endif                                                  %error
      ***
     c***                move      r7dcdt        zzdate
     c***                exsr      editDate
     c***                if        errorFlag
     c***                eval      error07 = *on
     c***                move(p)   err(19)       errReason
     c***                eval      Rcd07Err = Rcd07Err + 1
     c***                exsr      printError

     c                   else                                                   %error
     c                   eval      h7dcdt = x7Date
     c                   endif                                                  %error
      *
      * TRDT
     c                   IF        R7trdt  = *zeros
     c                   eval      error07 = *on
     c                   move(p)   err(35)       errReason
     c                   eval      Rcd07Err = Rcd07Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      *
     c                   move      r7trdt        zzdate
     c                   exsr      editDate
     c                   if        errorFlag
     c                   eval      error07 = *on
     c                   move(p)   err(35)       errReason
     c                   eval      Rcd07Err = Rcd07Err + 1
     c                   exsr      printError

     c                   else                                                   %error
     c                   eval      h7trdt = x7Date
     c                   endif                                                  %error
      *
     c                   IF        NOT error07
      *
      *
     c                   eval      rcd07Cnt = (rcd07Cnt + 1)
     c                   Exsr      writeSDCMP
     c                   endif                                                  NOT Error05
      *

     c                   endsr                                                  statEdit
      *=============================================================================================
      * DARTEdit2 subroutine - DARTS Claim Records - Type 08
      *=============================================================================================
     c     DARTEdit2     begsr
      *
      * Get the case information to create change record.
      *
     c                   eval      error08 = *off
     c                   eval      rec_type = 'Rcrd 08'
      *
     c                   IF        r8clm#  <> xxclm#
     c                   eval      error08 = *on
     c                   move(p)   err(18)       errReason
     c                   eval      Rcd08Err = Rcd08Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      * RMCD
     c                   IF        R8RMCD  = *blanks
     c                   eval      error08 = *on
     c                   move(p)   err(37)       errReason
     c                   eval      Rcd08Err = Rcd08Err + 1
     c                   exsr      printError
     c                   endif                                                  %error
      *
      *** Beg del *** 10/21/09 *******************************************  *********
      * SEQ#
     c************       IF        R8seq#  = *zeros
     c************       eval      error08 = *on
     c************       move(p)   err(36)       errReason
     c************       eval      Rcd08Err = Rcd08Err + 1
     c************       exsr      printError
     c************       endif                                                  %error
      *** End del *** 10/21/09 *******************************************  *********
      *
      *
     c                   IF        NOT error08
      *
     c                   eval      rcd08Cnt = (rcd08Cnt + 1)
     c                   Exsr      writeSMARP
     c                   endif                                                  NOT Error05
      *

     c                   endsr                                                  statEdit
      *** End add *** 11/08/07 *******************************************  *********
     C*===============================================================
     C* RVDED - This subroutine calls W8024R which determines if the old
     C*   employer was on a deductible plan and, if so, reverses the deduct
     C*   transactions.
     C*===============================================================
     C     rvded         begsr
     C*
     C                   clear                   xxamtb
     C*
     c                   eval      kkfnd = cmfnd
     C     ddky          chain     pdd01                              99
     C                   dow       not *in99
     C                   eval      xxamtb = xxamtb + ddamtb
     C     ddky          reade     pdd01                                  99
     C                   enddo
     C*
     c                   move      cmco#         @@co#
     c                   move      cmfnd         @@ffnd
     c                   move      cmemp#        @@femp
     c                   move      cmdiv         @@fdiv
     c                   move      cmfyr         @@ffyr
     c                   move      cmcase        @@fcas
     C                   MOVE      xxamtb        @@amtb
     C                   CALL      'W8024R'
     C                   PARM      *blank        @@call            1
     C                   PARM                    @@co#
     C                   PARM                    @@ffnd
     C                   PARM                    @@ffyr
     C                   PARM                    @@fcas
     C                   PARM                    @@femp
     C                   PARM                    @@fdiv
     C                   PARM      *zeros        @@temp
     C                   PARM      *zeros        @@tdiv
     C                   PARM                    @@amtb            9
     C                   PARM                    @@rtcd            1
     C*
     C                   endsr
      *=============================================================================================
      * otherEdit subroutine - prints bad record type to error report
      *=============================================================================================
     c     otherEdit     begsr

     c                   eval      errorFlag = *on
     c                   clear                   policy#
     c                   eval      rec_type = 'Invalid'
     c                   movel(p)  vfdata        errvalue
     c                   move(p)   err(4)        errReason
     c                   eval      rcdOtErr = rcdOterr + 1
     c                   exsr      printError

     c                   endsr                                                  otherEdit
     c****/Eject
      *=============================================================================================
      *  EditDATE subroutine - Takes incoming CCYYMMDD, checks for error, then converts it to
      *                        a seven position CYYMMDD format.
      *=============================================================================================
      *
     c     EditDATE      begsr
      *
     c                   eval      errorFlag = *off
     c                   eval      x7Date    = *zeros
      *
      * Edit date in Range
     c                   IF        zzdate > 19000101 AND zzdate < 20500101
      *
     c     *iso          test(de)                zzdate

     c                   IF        %error
     c                   eval      errorFlag = *on
     c                   z-add     *zeros        x7Date
     c                   ELSE
     c     *iso          move      zzdate        isoPostDate
     c     *cymd         move      isoPostDate   x7Date
     c                   EndIF                                                  %error

     c                   ELSE
     c                   eval      errorFlag = *on
     c                   movel(p)  zzdate        errValue
     c
     c                   EndIF

     c                   endsr                                                  EditDATE
      *=============================================================================================
      * printError subroutine - prints record to error report - also used to print "No Error Found"
      *=============================================================================================

     c     printError    begsr

     c                   select
      *
      * Skip printing the error report if this is the second time through which
      * will only happen if the updateIfErrorsExist flag = 'N'.  The errors were
      * printed the first time through when writeRecords <> 'Y'
      *
     c                   when      update = 'N' and
     c                             writeRecords = 'Y' and
     c*** Begin Chg *** 07/07/21 **************************************
     c**********                   *inlr <> *on
     c                             Not jobEnding
     c*** End   Chg *** 07/07/21 **************************************
     c                   when      *in20
     c                   movel(p)  err(1)        errReason
     c                   eval      rec_type = 'No File'
     c                   eval      errCount = errCount + 1
     c*** Begin Chg *** 07/07/21 **************************************
     c**********         except    detErr
     c**********         except    Det1
     c***oa*****         except    head
       If chrOnStack = '0';
          Except detErr;
          Except Det1;
          If *inOA = *On;
             Except head;
          EndIf;
       Else;
          errorsFound = *On;
          AddErrorMessageJson(errReason);
       EndIf;
     c*** End   Chg *** 07/07/21 **************************************

     c                   when      *in21
     c                   movel(p)  err(2)        errReason
     c                   eval      rec_type = 'No Rcds'
     c                   eval      errCount = errCount + 1
     c*** Begin Chg *** 07/07/21 **************************************
     c**********         except    detErr
     c**********         except    Det1
     c***oa*****         except    head
       If chrOnStack = '0';
          Except detErr;
          Except Det1;
          If *inOA = *On;
             Except head;
          EndIf;
       Else;
          errorsFound = *On;
          AddErrorMessageJson(errReason);
       EndIf;
     c*** End   Chg *** 07/07/21 **************************************

     c*** Begin Chg *** 07/07/21 **************************************
     c**********         when      *INLR AND errCount = *zeros
     c                   when      jobEnding And errCount = *zeros
     c*** End   Chg *** 07/07/21 **************************************
     c                   eval      rec_type = 'No Errs'
     c                   movel(p)  err(30)       errReason
     c*** Begin Chg *** 07/07/21 **************************************
     c**********         except    DetErr
     c***oa*****         except    head
     c**********         except    Det1
       If chrOnStack = '0';
          Except detErr;
          If *inOA = *On;
             Except head;
          EndIf;
          Except Det1;
       Else;
          errorsFound = *On;
          AddErrorMessageJson(errReason);
       EndIf;
     c*** End   Chg *** 07/07/21 **************************************

     c*** Begin Chg *** 07/07/21 **************************************
     c**********         when      *INLR AND errCount > *zeros
     c**********         except    DetTotl
     c**********         except    Det1
     c                   when      jobEnding AND errCount > *zeros
     c                   If        chrOnStack = '0'
     c                   except    DetTotl
     c                   except    Det1
     c                   EndIf
     c*** End   Chg *** 07/07/21 **************************************

     c                   other

     c*** Begin Chg *** 07/07/21 **************************************
     c**********         if        NOT *INLR
     c                   If        Not jobEnding
     c*** End   Chg *** 07/07/21 **************************************
     c                   eval      errorFlag = *off
     c                   eval      errCount = errCount + 1
     c*** Begin Chg *** 07/07/21 **************************************
     c***oa*****         except    head
     c**********         except    det
       If chrOnStack = '0';
          If *inOA = *On;
             Except head;
          EndIf;
          Except det;
       Else;
          errorsFound = *On;
          AddErrorMessageJson(errReason);
       EndIf;
     c*** End   Chg *** 07/07/21 **************************************
     c                   endif
      *
     c                   endsl
      *
     c                   endsr                                                  printError
     c/eject
      *=============================================================================================
      * writeWMCMP - Adds or Updates Claim Master file
      *=============================================================================================

     c     writeWMCMP    begsr

      * Check for Add or Update of Claim Record

     c                   eval      CMUSER = Q1USER
     c                   eval      CMPGM  = Q1PGM
     c                   eval      CMTIME = dstime
     c                   eval      CMDT   = dtCYMD(dsdate)

     c                   select

     c                   when      chgClaim
     c*** Begin Add *** 01/22/19 *********************************
     c                   If        R2aww <> CMaww And R2aww <> 0
     c                   Exsr      WriteWMAWP
     c                   EndIf
     c*** End   Add *** 01/22/19 *********************************

     c*** Begin chg *** 05/12/08 ****************************************************
     c***90****          update    pcm01
     c   90              update    pcm02
     c*** End chg ***** 05/12/08 ****************************************************

     c                   when      addClaim
      *
      * Each Record 01 - Get next available case Number for Group
     c                   exsr      newFYR
     c                   exsr      nextCase
     c                   z-add     kkco#         cmco#
     c                   z-add     kkfnd         cmfnd
     c                   z-add     xxfyr         cmfyr
     c                   z-add     xxcase        cmcase
     c*** Begin Add *** 01/22/19 *********************************
     c                   If        R2aww <> CMaww And R2aww <> 0
     c                   Exsr      WriteWMAWP
     c                   EndIf
     c*** End   Add *** 01/22/19 *********************************

     c                   eval      CMIUSR = Q1USER
     c                   eval      CMITM  = dstime
     c                   eval      CMIPGM = Q1PGM
     c
     c*** begin add ***  04/15/09  *****************************************
     c     cmidt         ifeq      *zero
     c                   eval      x7Date = dtCYMD(dsdate)
     c                   z-add     x7Date        cmidt
     c                   endif                                                  idt = 0
     c*** begin add ***  04/15/09  *****************************************
     c*** Begin chg *** 05/12/08 ****************************************************
     c***90****          write     pcm01
     c   90              write     pcm02
     c*** End chg ***** 05/12/08 ****************************************************

     c                   endsl

      * Now Add/Update Legal Master file
     c                   z-add     cmfyr         kkfyr
     c                   z-add     cmcase        KKCASE
      *
     c                   clear                   pll01
      *
     c     llky          chain     pll01                              60
     c                   eval      lllump = r2lump
     c                   eval      llmtst = r2mtst
     c*** begin chg ***  06/04/08  *****************************************
     c*************      eval      llfrau = r2frau
     c                   select
     c     r2frau        wheneq    '0'
     c                   eval      llfrau = '00'
     c     r2frau        wheneq    '1'
     c                   eval      llfrau = '01'
     c     r2frau        wheneq    '2'
     c                   eval      llfrau = '02'
     c                   other
     c                   eval      llfrau = '00'
     c                   endsl
     c*** end chg *****  06/04/08  *****************************************
     c                   eval      lllitg = r2litg
     c                   eval      LLUSER = Q1USER
     c                   eval      LLPGM  = Q1PGM
     c                   eval      LLTIME = dstime
     c                   eval      LLDT   = dtCYMD(dsdate)
     c***** Beg add *** 05/08/25 ******************************************************
     c                   eval      LLmdex = R2mdex
     c***** End add *** 05/08/25 ******************************************************
      *
     c                   IF        *IN60
     c                   eval      LLCO#  = kkco#
     c                   eval      LLFND  = kkfnd
     c                   eval      LLFYR  = kkfyr
     c                   eval      LLCASE = kkcase
      *
     c                   eval      LLIUSR = Q1USER
     c                   eval      LLIPGM = Q1PGM
     c                   eval      LLITM  = dstime
     c                   eval      LLIDT  = CMIDT
     c   90              write     pll01
      *
     c                   else
      *
     c   90              update    pll01
     c                   endif
      *

     c                   endsr                                                  writeWMCMP
      *=============================================================================================
      * writeWMCDP - Add/Updates Claimant Demographic file
      *=============================================================================================
      *
     c     writeWMCDP    begsr
      *
     c                   eval      CDUSER = Q1USER
     c                   eval      CDPGM  = Q1PGM
     c                   eval      CDTIME = dstime
     c                   eval      CDDT   = dtCYMD(dsdate)
      *
      /Free
        // Beg Add *** 12/17/15 *********************************************************
        //*** Begin Delete *** 06/07/19 **************************************************
        // feod WMCDL ;
        //*** End   Delete *** 06/07/19 **************************************************
        // End Add *** 12/17/15 *********************************************************
      /End-free
     c                   select
     c                   when      chgClaim
     c*** Begin add *** 04/15/09 ******************************************************
     c                             AND haveClaimant = *on
     c*** End add ***** 04/15/09 ******************************************************
     c*** Begin Add *** 05/23/11 *******************************************
     c                             or (addClaim and haveClaimant)
     c*** End   Add *** 05/23/11 *******************************************
     c   90              Update    pcd01
      *
     c                   when      addClaim
     c*** Begin add *** 04/15/09 ******************************************************
     c*** Begin chg *** 10/13/09 ******************************************************
     c*****************            OR haveClaimant = *off
     c                             AND haveClaimant = *off
     c*** End chg ***** 10/13/09 ******************************************************
     c     cdky          chain     pcd01                              63
     c     *in63         ifeq      *on
     c     cdidt         ifeq      *zero
     c                   eval      x7Date = dtCYMD(dsdate)
     c                   z-add     x7Date        cdidt
     c                   endif                                                  Not *IN60
     c*** End add ***** 04/15/09 ******************************************************
      *
     c                   eval      CDIUSR = Q1USER
     c                   eval      CDIPGM = Q1PGM
     c                   eval      CDITM  = dstime
      *
     c   90              Write     pcd01
      *
     c*** Begin Add *** 05/23/11 *******************************************
     c                   else
     c                   eval      cduser = Q1USER
     c                   eval      cdpgm  = Q1PGM
     c                   eval      cdtime = dstime
     c                   eval      cddt   = dtCYMD(dsdate)
     c                   eval      cdfsn  = r2fsn
     c                   eval      cdlsn  = r2lsn
     c                   eval      cdmid  = r2mid
     c                   eval      cdadr1 = r2adr1
     c                   eval      cdadr2 = r2adr2
     c                   eval      cdcty  = r2cty
     c                   eval      cdst   = r2st
     c                   eval      cdctry = r2ctry
     c                   eval      cdadr3 = r2adr3
     c                   move      r2zip         cdzip
     c                   eval      cdsex  = r2sex
     c                   eval      cdmsts = r2msts
     c   90              update    pcd01
     c*** End   Add *** 05/23/11 *******************************************
     c*** Beg add ***** 04/15/09 ******************************************************
     c                   endif                                                  Not *IN60
     c*** End add ***** 04/15/09 ******************************************************
      *
     c                   endsl                                                  Not *IN60

      /Free
        // Beg Add *** 12/17/15 *********************************************************
         feod WMCDL ;
        // End Add *** 12/17/15 *********************************************************
      /End-free
     c                   endsr                                                  writeWMCDP
      *=============================================================================================
      * writeWMCHP - Add/Updates Claimant Demographic file
      *=============================================================================================
     c     writeWMCHP    begsr
      *
     c                   eval      chco#  = kkco#
     c                   eval      chfnd  = kkfnd
     c                   eval      chfyr  = kkfyr
     c                   eval      chcase = cmcase
     c                   eval      CHUSER = Q1USER
     c                   eval      CHPGM  = Q1PGM
     c                   eval      zztime = zztime +1
     c                   eval      CHTIME = zztime
     c                   eval      CHDT   = dtCYMD(dsdate)
      *
     c   90              Write     pch01

     c                   endsr                                                  writeWMCHP
     c/Eject
      *** Beg add *** 11/08/07 *******************************************  *********
      *=============================================================================================
      * writeSDCMP - Add/Updates DARTS file
      *=============================================================================================
     c     writeSDCMP    begsr
     c********************************************
     c                   eval      dskey  = *blanks

     c                   eval      dsco#  = kkco#
     c                   eval      dsfnd  = kkfnd
     c                   eval      dsfyr  = kkfyr
     c                   eval      dscase = kkcase

     c                   eval      kdkey = dskey
     **** Beg add *** 10/21/09 *******************************************  *********
     c                   eval      kdseq# = *HiVal
     C*
     C*  Get the last SEQ# used.
     C*
     C     scmky         setgt     scm02
     C     kdkey         readpe    scm02                                  60
     C                   if        *in60 = *off
     C                             and cmkey = dskey
     C                   eval      cmseq# = cmseq# + 1
     C                   else
     C                   eval      cmseq# = 1
     C                   endif
     c                   eval      kdseq# = cmseq#
     c                   eval      r7seq# = cmseq#
      *** End add *** 10/21/09 *******************************************  *********
      *** Beg del *** 10/21/09 *******************************************  *********
     c**************     eval      kdseq# = r7seq#
     c**** scmky ***     chain     scm01                              99
     c** 99 ********     clear                   scm01
      *** End del *** 10/21/09 *******************************************  *********

     c                   eval      SCMUSER = r7USER
     c                   eval      SCMPGM  = Q1PGM
     c                   eval      SCMTIME = dstime
     c                   eval      SCMDT   = dtCYMD(dsdate)

     c                   eval      scmkey = dskey
     c                   eval      scmseq# = r7seq#
     c                   eval      scmcdat = h7cdat
     c                   eval      scmtrdt = h7trdt
     c                   eval      scmtrtm = r7trtm
     c                   eval      scmrmcd = r7rmcd
     c                   eval      scmcomm = r7comm
     c                   eval      scmdifl = r7difl

      *** Beg del *** 10/21/09 *******************************************  *********
     c****************   if        *in99 = *off
     c****************   update    scm01

     c**************     else
      *** End del *** 10/21/09 *******************************************  *********

     c                   eval      SCMIUSR = r7USER
     c                   eval      SCMIPGM = Q1PGM
     c                   eval      SCMIDT   = dtCYMD(dsdate)
     C*** Begin add *** 01/30/08 *********************************************************
     c                   eval      SCMDCID  = 'W0070C'
     c                   eval      SCMPNAM  = 'W0070R'
     c                   eval      SCMSTAT  = 'A'
     c                   eval      SCMFOLD  = SCMKEY
      ***** BEG ADD ***** 10/17/11 ***********************************
     c                   if        scmfold <> ' ' or scmdcid <> ' '
      ***** Beg Chg ***** 05/23/14 ***********************************
     c************       eval      scmuncn = '/' + %trim(root) + '/' +
     c************                           %trim(scmfold) + '/' +
     c************                           %trim(scmdcid)
      /Free
       scMunCn = ' ';
      /End-free
      ***** End Chg ***** 05/23/14 ***********************************
     c                   eval      scmifsp = env
     c                   else
     c                   eval      scmuncn = ' '
     c                   eval      scmifsp = ' '
     c                   endif
      ***** END ADD ***** 10/17/11 ***********************************
     c                   eval      SCMFL01  = 'Y'
     C*** End add ***** 01/30/08 *********************************************************

     c                   write     scm01

      *** Beg del *** 10/21/09 *******************************************  *********
     c*************      endif
      *** End del *** 10/21/09 *******************************************  *********

     c                   endsr                                                  writeSDCMP
      *
      *=============================================================================================
      * writeSMARP - Add/Updates DARTS file
      *=============================================================================================
     c     writeSMARP    begsr

     c                   eval      dskey  = *blanks

     c                   eval      dsco#  = kkco#
     c                   eval      dsfnd  = kkfnd
     c                   eval      dsfyr  = kkfyr
     c                   eval      dscase = kkcase

     c                   eval      kdkey = dskey
      *** Beg chg *** 10/21/09 *******************************************  *********
     c*************      eval      kdseq# = r8seq#
     c                   eval      kdseq# = R7seq#
      *** End chg *** 10/21/09 *******************************************  *********
     c                   eval      kdasq# = r8asq#
     c     arky          chain     par01                              99
     c   99              clear                   par01

     c                   eval      ARUSER = Q1USER
     c                   eval      ARPGM  = Q1PGM
     c                   eval      ARTIME = dstime
     c                   eval      ARDT   = dtCYMD(dsdate)

     c                   eval      arkey = dskey
      *** Beg chg *** 10/21/09 *******************************************  *********
     c*************      eval      arseq# = r8seq#
     c                   eval      arseq# = R7seq#
      *** End chg *** 10/21/09 *******************************************  *********
     c                   eval      arasq# = r8asq#
     c                   eval      arrmcd = r8rmcd
     c                   eval      arcm75 = r8cm75
      *** Beg Add *** 10/21/09 *******************************************  *********
     c                   eval      arcdat = scmcdat
     c                   eval      artrdt = scmtrdt
     c                   eval      artrtm = scmtrtm
     c                   If        r8rmcd = ' '
     c                   eval      arrmcd = scmrmcd
     c                   EndIf
      *** End Add *** 10/21/09 *******************************************  *********

     c                   if        *in99 = *off
     c                   update    par01

     c                   else

     c                   eval      ARIUSR = Q1USER
     c                   eval      ARIDT   = dtCYMD(dsdate)
     C*** Begin add *** 01/30/08 *********************************************************
     c                   eval      ARPNAM  = 'W0070R'
     c                   eval      ARSTAT  = 'A'
     C*** End add ***** 01/30/08 *********************************************************

     c                   write     par01
     c                   endif

     c                   endsr                                                  writeWMCDP
      *
      *** End add *** 11/08/07 *******************************************  *********
     c/Eject
      *=============================================================================================
      * nextCase - Get Next Case Number for Add, when Group Number Changes
      *=============================================================================================
     c     nextCase      begsr
      *
      * If not 1st time and group changed or *INLR, update Next Available Case Nubmer
      *
      * First UPdate old group Next Avail Case Nbr
      *
     c     fnky2         chain     pfn01                              61
     c                   If        NOT *IN61
     c                   z-add     fnnacs        xxcase
     c                   add       1             fnnacs
      *** Beg Add ***** 05/23/24 ***********************
      /free
        FNpgm  = Q1pgm;
        FNuser = Q1user;
        FNtime = Timeto6(SystemTime());
        FNdt   = Dateto7(SystemDate());
      /end-free
      *** End Add ***** 05/23/24 ***********************
     c                   update    pfn01
     c                   feod      wmfnl
     c                   EndIf                                                  NOT *IN61
     c
      *** Beg add *** 11/08/07 *******************************************  *********
     c                   If        addClaim AND CLTID = '132'
     c                   If        R1clm7 <> *blanks or R1clm7 <> '0000000'
     c                   move      R1clm7        xxcase
     c                   EndIf                                                  NOT *IN61
     c                   EndIf                                                  NOT *IN61
      *** End add *** 11/08/07 *******************************************  *********
      *
     c                   endsr                                                  nextCase
      *=============================================================================================
      * newFYR - Get Next Case Number for Add, when Group Number Changes
      *=============================================================================================
     c     newFYR        begsr
      *
      * Next, calculate the claim year.
      *
     c     elky          setgt     pel01
     c     emky          readpe    pel01                                  61
     c                   If        NOT *IN61
     c                   z-add     elfyr         xxfyr
     c                   else
     c                   movel     kkacdt        xxfyr
     c                   EndIf                                                  NOT *IN61

     c                   endsr                                                  newFYR
      *
      *=============================================================================================
      * printSetup subroutine - Gets heading info, setup for Reports.
      *=============================================================================================
     c     printSetup    begsr
     *
     * Get user name if applicable.
     *
     c*** Beg del *** 03/06/19 *******************************************  *********
     c*****************  eval      *inoa = *on
     c*** End del *** 03/06/19 *******************************************  *********
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

     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     c                   eval      xxfil#   = '0'
     c     orky          chain     por01                              60

     c                   if        ORTITL = *blank
     c                   eval      ORTITL = title
     c                   else
     c                   eval      ORTITL = leftJstify(ORTITL)
     c                   endif
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route
      ***** End Del ***** 08/26/14 ************************************
     c                   eval      ORTITL = centerFld(ORTITL)
     c*** Begin Chg *** 07/07/21 **************************************
     c**********         except    head
     c                   If        chrOnStack = '0'
     c                   except    head
     c                   EndIf
     c*** End   Chg *** 07/07/21 **************************************

     c                   clear                   ORTITL
     c                   eval      xxfil#   = '1'
     c     orky          chain     por01                              60
      *
     * Get the distribution information for the update report.
     *
     c                   if        ORTITL = *blank
     c                   eval      ORTITL1 = title1
     c                   else
     c                   eval      ORTITL1 = ORTITL
     c                   endif

     C                   eval      ORTITL1 = leftJstify(ORTITL1)
      ***** Beg Del ***** 08/26/14 ************************************
     c**********         except    route1
      ***** End Del ***** 08/26/14 ************************************
     c                   eval      ORTITL1 = centerFld(ORTITL1)

     c*** Begin Chg *** 07/07/21 **************************************
     c**********         except    head1
     c                   If        chrOnStack = '0'
     c                   except    head1
     c                   EndIf
     c*** End   Chg *** 07/07/21 **************************************
     *
     c                   endsr                                                  printSetup
     c*** Begin add *** 04/11/08 ******************************************************
      *=============================================================================================
      * writeAck - Load Acknowledgement File.
      *=============================================================================================
     c     writeAck      begsr
     c                   eval      Cbfid  = R9fid                               FILE ID
     c                   eval      Cb01r  = R901r                               NUM OF 01 REC REPORT
     c                   eval      Cb02r  = R902r                               NUM OF 02 REC REPORT
     c                   eval      Cb03r  = R903r                               NUM OF 03 REC REPORT
     c                   eval      Cb04r  = R904r                               NUM OF 04 REC REPORT
     c                   eval      Cb05r  = R905r                               NUM OF 05 REC REPORT
     c                   eval      Cb06r  = R906r                               NUM OF 06 REC REPORT
     c                   eval      Cb07r  = R907r                               NUM OF 07 REC REPORT
     c                   eval      Cb08r  = R908r                               NUM OF 08 REC REPORT
     c**                 eval      Cb09r  = R909r                               NUM OF 09 REC REPORT
     c**                 eval      Cb10r  = R910r                               NUM OF 10 REC REPORT
     c                   eval      Cb09r  = *zeros                              NUM OF 09 REC REPORT
     c                   eval      Cb10r  = *zeros                              NUM OF 10 REC REPORT
     c                   eval(R)   Cbtotr = R9totr                              TOT NUM OF REC REPOR
     c                   eval      Cbdtr  = R9dtr                               DATE REPORTED
     c                   eval      Cb01P  = Reccnt01                            NUM OF 01 REC PROCES
     c                   eval      Cb02P  = Reccnt02                            NUM OF 02 REC PROCES
     c                   eval      Cb03P  = Reccnt03                            NUM OF 03 REC PROCES
     c                   eval      Cb04P  = Reccnt04                            NUM OF 04 REC PROCES
     c                   eval      Cb05P  = Reccnt05                            NUM OF 05 REC PROCES
     c                   eval      Cb06P  = Reccnt06                            NUM OF 06 REC PROCES
     c                   eval      Cb07P  = Reccnt07                            NUM OF 07 REC PROCES
     c                   eval      Cb08P  = Reccnt08                            NUM OF 08 REC PROCES
     c                   eval      Cb09P  = Reccnt09                            NUM OF 09 REC PROCES
     c                   eval      Cb10P  = *zeros                              NUM OF 10 REC PROCES
     c                   eval      Cbtotp = RecCount                            TOT NUM OF REC REPOR
     c                   move      *date         xxCCYYMMDD
     c                   eval      xxEDATE=xxMM + xxDD + xxCCYY
     c                   MOVE      xxEDATE       CBDTP
     c                   write     pcb01                                        WRITE WZCBP
     c                   endsr                                                  writeAck
      *** End   add *** 04/11/08 ****************************************************
      *** Beg add ***** 08/13/08 ******************************************************
      *=============================================================================================
      * resetcounts - Reset Counts
      *=============================================================================================
     c     resetCounts   begsr
     c                   eval      recCount = *ZEROS
     c                   eval      payAmtTot= *ZEROS
      *
     c                   eval      recCnt01 = *ZEROS
     c                   eval      recCnt02 = *ZEROS
     c                   eval      recCnt03 = *ZEROS
     c                   eval      recCnt04 = *ZEROS
     c                   eval      recCnt05 = *ZEROS
     c                   eval      recCnt06 = *ZEROS
     c                   eval      recCnt07 = *ZEROS
      *
     c                   eval      rcd01cnt = *ZEROS
     c                   eval      rcd02cnt = *ZEROS
     c                   eval      rcd03cnt = *ZEROS
     c                   eval      rcd04cnt = *ZEROS
     c                   eval      rcd05cnt = *ZEROS
     c                   eval      rcd06cnt = *ZEROS
     c                   eval      rcd07cnt = *ZEROS
     c                   eval      rcd08cnt = *ZEROS
     c                   eval      recother = *ZEROS
      *
     c                   eval      rcd01skp = *ZEROS
     c                   eval      rcd02skp = *ZEROS
     c                   eval      rcd03skp = *ZEROS
     c                   eval      rcd04skp = *ZEROS
     c                   eval      rcd05skp = *ZEROS
     c                   eval      rcd06skp = *ZEROS
     c                   eval      rcd07skp = *ZEROS
     c                   eval      rcd08skp = *ZEROS
     c                   eval      rcdOtskp = *ZEROS
     c                   eval      rcdErskp = *ZEROS
      *
     c                   eval      rcd01Err = *ZEROS
     c                   eval      rcd02Err = *ZEROS
     c                   eval      rcd03Err = *ZEROS
     c                   eval      rcd04Err = *ZEROS
     c                   eval      rcd05Err = *ZEROS
     c                   eval      rcd06Err = *ZEROS
     c                   eval      rcd07Err = *ZEROS
     c                   eval      rcd08Err = *ZEROS
     c                   eval      rcdOtErr = *ZEROS
     c                   eval      rcdErCnt = *ZEROS
      *
     c                   eval      suT01 = *zeros
     c                   eval      suT02 = *zeros
     c                   eval      suT03 = *zeros
     c                   eval      suT04 = *zeros
     c                   eval      suT05 = *zeros
     c                   eval      suT06 = *zeros
     c                   eval      suT07 = *zeros
     c                   eval      suT08 = *zeros
     c                   eval      suT09 = *zeros
     c                   eval      suT10 = *zeros
     c                   endsr                                                  resetCounts
      *** End add ***** 08/13/08 ****************************************************
      //*** Beg add *** 05/08/25 ******************************************************
      *================================================================
      * PopIdcFld - Populate Newly added IDC fields to wmcmp
      *================================================================
       Begsr PopIdcFld;
        CMclas     =  R2clas;
        CM#dep     =  R2#dep;
        CMIndClmCd =  R2IndClmCd;
        CMests     =  R2ests;
        CMwmth     =  R2wmth;
        CMloec     =  R2loec;
        CMpedis    =  R2edis;
        CMtdbe     =  R2tdbe;
        CMlosscd   =  R2losscd;
        CMprww     =  R2prww;
        CMast      =  R2ast;
        CMidcacs   =  R2idcacd;
        Monitor;
         CMacz1    =  %Dec(%Subst(R2acz:1:5):5:0);
         CMacz2    =  %Dec(%Subst(R2acz:6:4):4:0);
         //*** Beg Del *** 10/23/25 ******************************************************
         //*** CMmmi     =  dtCYMD(R2mmi);
         //*** End Del *** 10/23/25 ******************************************************
        On-Error;
        EndMon;
        //*** Beg add *** 10/23/25 ******************************************************
        Monitor;
         CMmmi     =  dtCYMD(R2mmi);
        On-Error;
         CMmmi     =  *zeros;
        EndMon;
        //*** End add *** 10/23/25 ******************************************************
       Endsr;

      *================================================================
      * WriteWMWHP - Write to wmwhp file
      *================================================================
       Begsr WriteWMWHP;
        Monitor;
         xxDate = dtCYMD(R2rtnwrk);
        On-Error;
         xxDate = *zeros;
        Endmon;
        chain (CMco#:CMfnd:CMfyr:CMcase:xxDate:'R') pwh00;
        If not %found(wmwhl);
           clear pwh00;
           WHco#   =  CMco#;
           WHfnd   =  CMfnd;
           WHfyr   =  CMfyr;
           WHcase  =  CMcase;
           WHdate  =  xxDate;
           WHwkst  =  'R';
           WHstat  =  'A';
           WHuser  =  Q1user;
           WHpgm   =  Q1pgm;
           WHtime  =  TimeTo6(SystemTime());
           WHdt    =  DateTo7(SystemDate());
           WHldwk  =  *zeros;
           WHiusr  =  Q1user;
           WHipgm  =  Q1pgm;
           WHitim  =  TimeTo6(SystemTime());
           WHidt   =  DateTo7(SystemDate());
           WHrtwq  =  *blanks;
           WHcdlw  =  *zeros;
           WHcdek  =  *zeros;
           WHcdak  =  *zeros;
           WHlrse  =  *blanks;
           Write pwh00;
        Endif;
       Endsr;

      //*** End add *** 05/08/25 ******************************************************
     c/eject
      *=============================================================================================
      * Output files
      *=============================================================================================
     o/copy scopyrout

     oqsysprt   e            route          1                                   error report
     o          e            route          1 52
     o                                              'Run Time & date'
     o                       Dsdttm           +   1 '  :  :  &  /  /    '

     o*** Beg Chg *****  01/28/10  ************************************
     o***       e            route          1 55
     o***                                           '(C) Copyright Fiserv ISD'
     o***                                     +   0 ', Inc.,'
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
     o                       companyNm          100
     o                       sys_date           132
     o          e            head           2
     o                       q1pgm
     o                       ORTITL             100
     o                                          127 'PAGE:'
     o                       page          z    132
     o          e            head           0
     o                                            4 'Type'
     o                                           14 'Policy #'
     o                                           36 'Err Value'
     o                                           74 'Error Reason'
     o                                          132 'Rcd #'
     o          e            head           2
     o                       line               132
     o          e            detErr         1
     o                       rec_type            07
     o                       errReason          100
     o                                           37 '************************'
     o                                          130 '************************'
     o          e            det            1
     o                       recType             03
     o                       policy#             24
     o                       errValue       B    60
     o                       errReason          122
     o                       recCount      z    132
     o          e            dettotl     1  1
     o                                           40 '*** Total Errors listed on-
     o                                               Report ***'
     o                       errCount      1  +   1
     o/copy scopyrout1

     oqsysprt1  e            route1         1                                   successful upload

     o          e            route1         1 52
     o                                              'Run Time & Date:'
     o                       Dsdttm           +   2 '  :  :  &  /  /    '
     o*** Beg Chg *****  01/28/10  ************************************
     o***       e            route1         1 55
     o***                                           '(C) Copyright Fiserv ISD'
     o***                                     +   0 ', Inc.,'
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
     o                       sys_date           132
     o          e            head1          2
     o                       q1pgm
     o                       ORTITL1             88
     o                                          127 'PAGE:'
     o                       page1         z    132
     o          e            head1          1
     o                                           20 'Nbr of'
     o                                           30 'Nbr of'
     o                                           40 'Nbr of'
     o                                           50 'Nbr of'
     o                                           60 'Nbr of'
     o                                           70 'Nbr of'
      *** Beg add *** 11/08/07 *******************************************  *********
     o                                           80 'Nbr of'
     o                                           90 'Nbr of'
      *** End add *** 11/08/07 *******************************************  *********
      *** Beg chg *** 11/08/07 *******************************************  *********
     o*********                                  81 'Nbr Othr'
     o*********                                  94 'Total of'
     o                                          101 'Nbr Othr'
     o                                          114 'Total of'
      *** End chg *** 11/08/07 *******************************************  *********
     o          e            head1          1
     o                                           10 'Run Date'
     o                                           20 'Rcd 01'
     o                                           30 'Rcd 02'
     o                                           40 'Rcd 03'
     o                                           50 'Rcd 04'
     o                                           60 'Rcd 05'
     o                                           70 'Rcd 06'
      *** Beg add *** 11/08/07 *******************************************  *********
     o                                           80 'Rcd 07'
     o                                           90 'Rcd 08'
      *** End add *** 11/08/07 *******************************************  *********
      *** Beg chg *** 11/08/07 *******************************************  *********
     o**********                                 81 'Rcds/Errs'
     o**********                                 94 'All Rcds'
     o                                          101 'Rcds/Errs'
     o                                          114 'All Rcds'
      *** End chg *** 11/08/07 *******************************************  *********
     o          e            head1          2
     o                       line               132
     o          e            det1           2
     o                       DSDATE        Y     12
     o                       recCnt01      1     20
     o                       recCnt02      1     30
     o                       recCnt03      1     40
     o                       recCnt04      1     50
     o                       recCnt05      1     60
     o                       recCnt06      1     70
      *** Beg add *** 11/08/07 *******************************************  *********
     o                       recCnt07      1     80
     o                       recCnt08      1     90
      *** End add *** 11/08/07 *******************************************  *********
      *** Beg chg *** 11/08/07 *******************************************  *********
     o****                   recOther      1     80
     o****                   recCount      1     94
     o                       recOther      1    100
     o                       recCount      1    114
      *** End chg *** 11/08/07 *******************************************  *********
     o                                        +   2 'Tot records Read'
     o          e            det1           2
     o                 N90                       11 'NO UPDATES'
     o                  90                       11 'ADD/UPDATE'
     o                       rcd01Cnt      1     20
     o                       rcd02Cnt      1     30
     o                       rcd03Cnt      1     40
     o                       rcd04Cnt      1     50
     o                       rcd05Cnt      1     60
     o                       rcd06Cnt      1     70
      *** Beg add *** 11/08/07 *******************************************  *********
     o                       rcd07Cnt      1     80
     o                       rcd08Cnt      1     90
      *** End add *** 11/08/07 *******************************************  *********
      *** Beg chg *** 11/08/07 *******************************************  *********
     o****                   recOther      1     80
     o****                   rcdCount      1     94
     o                       recOther      1    100
     o                       rcdCount      1    114
      *** End chg *** 11/08/07 *******************************************  *********
     o                                        +   2 'Tot rcds Add/Upd'
     o          e            det1           2
     o                                           12 'Errors '
     o                       rcd01Err      1     20
     o                       rcd02Err      1     30
     o                       rcd03Err      1     40
     o                       rcd04Err      1     50
     o                       rcd05Err      1     60
     o                       rcd06Err      1     70
      *** Beg add *** 11/08/07 *******************************************  *********
     o                       rcd07Err      1     80
     o                       rcd08Err      1     90
      *** End add *** 11/08/07 *******************************************  *********
      *** Beg chg *** 11/08/07 *******************************************  *********
     o****                   rcdOtErr      1     80
     o****                   rcdErCnt      1     94
     o                       rcdOtErr      1    100
     o                       rcdErCnt      1    114
      *** End chg *** 11/08/07 *******************************************  *********
     o                                        +   2 'Tot Err by Type'
     o          e            det1           2
     o                                           12 'Skipped'
     o                       rcd01Skp      1     20
     o                       rcd02Skp      1     30
     o                       rcd03Skp      1     40
     o                       rcd04Skp      1     50
     o                       rcd05Skp      1     60
     o                       rcd06Skp      1     70
      *** Beg add *** 11/08/07 *******************************************  *********
     o                       rcd07Skp      1     80
     o                       rcd08Skp      1     90
      *** End add *** 11/08/07 *******************************************  *********
      *** Beg chg *** 11/08/07 *******************************************  *********
     o****                   rcdOtSkp      1     70
     o****                   rcdErSkp      1     94
     o                       rcdOtSkp      1    100
     o                       rcdErSkp      1    114
      *** End chg *** 11/08/07 *******************************************  *********
     o                                        +   2 'Tot Rcds Skipped'
     o          e            det1           2
     o                       payAmtTot     1     20
     o                                        +   2 'Pay Records-Total Amount'
     o                       suTot         1     80
     o                                        +   2 'Rsv History Rcds-Total Amt'
     o          e            det1           1
     o                       suT01         1     20
     o                                        +   2 '11'
     o                                        +   2 'Rsv History 01 Total Amt'
     o          e            det1           1
     o                       suT02         1     20
     o                                        +   2 '13'
     o                                        +   2 'Rsv History 02 Total Amt'
     o          e            det1           1
     o                       suT03         1     20
     o                                        +   2 '12'
     o                                        +   2 'Rsv History 03 Total Amt'
     o          e            det1           1
     o                       suT04         1     20
     o                                        +   2 '16'
     o                                        +   2 'Rsv History 04 Total Amt'
     o          e            det1           1
     o                       suT05         1     20
     o                                        +   2 '14'
     o                                        +   2 'Rsv History 05 Total Amt'
     o          e            det1           1
     o                       suT06         1     20
     o                                        +   2 '  '
     o                                        +   2 'Rsv History 06 Total Amt'
     o          e            det1           1
     o                       suT07         1     20
     o                                        +   2 '  '
     o                                        +   2 'Rsv History 07 Total Amt'
     o          e            det1           1
     o                       suT08         1     20
     o                                        +   2 '  '
     o                                        +   2 'Rsv History 08 Total Amt'
     o          e            det1           1
     o                       suT09         1     20
     o                                        +   2 '  '
     o                                        +   2 'Rsv History 09 Total Amt'
     o          e            det1           1
     o                       suT10         1     20
     o                                        +   2 '  '
     o                                        +   2 'Rsv History 10 Total Amt'
     o          e            det1           1
     o                       suRhTot       1     20
     o                                        +   2 '  '
     o                                        +   2 'Total Rsv Hist Amt Added'
** Error messages                                           | <-- End of Error Msg
No import file was found.                                        01
There are NO records in the import file.                         02
There were NO valid record types in the import file.             03
Record type invalid, must be '01' through '08'                   04
No Record type of '01' for other record types.                   05
No matching policy for the claim number.                         06
Claim marked as an Update, but no matching claim exists.         07
Claim marked as a Delete, but no matching claim exists.          08
Claim marked as a Add, but a matching claim exists.              09
Record 01 - Update Type is not 'A', 'C' or 'D'                   10 ---
No match on Policy #.                                            11
Policy does not exist for incoming claim#.                       12
No claim exists for Record 01 - Chg or DELETE Claim.             13
SS# or Accident Date is blank, claim is not processed.           14
Accident date is not a valid date.                               15
Claim does not exist for Change claim or Delete claim.           16
Claim already exists for Add - w/same SS# and Accident Date      17
Claim # does not match the claim # in the 01 record.             18
Invalid claim status code or effective date.                     19
Invalid transaction type, reserve code or payment value 1.       20
No claimant demographic record for claim update process.         21
Incomplete claimant name.                                        22
Invalid Nature, Part, Cause or Loss Coverage code.               23
Invalid Gender (not M/F) or Marital Status (not M/K/S/U).        24
Invalid Lost Time Flag (must be I, M, or R).                     25
No policy # in the 01 record.  Claim not processed.              26
Policy # not found.  Claim was processed.                        27
Invalid date claim entered.  Defaulted to system date.           28
Invalid transaction type in 06 record.                           29
CLIMPORT file successfully Processed with NO Errors *********    30
No new Policy#, SS# or Accident Date in 06 record.               31
Transaction Date is blank in 06 record.                          32
Duplicate Check#                                                 33
A matching SS# was found with a different name.                  34
Invalid date                                                     35
Invalid sequence # on a DARTS record                             36
Invalid code on a DARTS record                                   37
Payroll class code not setup for this employer.                  38
Not Used                                                         39
Not Used                                                         40
