     h Copyright('(c) 2025 Sapiens. All Rights Reserved.')
     h**********************************************************************
     h/TITLE WPICLMERR - Profound Claim Error Processing
     h/DEFINE   PROFOUNDUI
     h nomain
     h DFTNAME(WPICLMERR)
     h option(*nodebugio: *srcstmt)
     h**********************************************************************
     h*
     h*  Description:  This Program Has Claim Edit Procedures.
     h*
     h*  Called from: Pround UI
     h*  Calls to: RTVCLTID
     h*    RTVCLTID
     h*
     h*  Created on 6/20/2012 by c:62 for Project WC2115
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h*  04/08/25 C:333  Support: 118812 (Project: WC2998)                 *
     h*                 - Checking MSA Period for both Structured & LumpSum*
     h*                                                                    *
     h*  03/06/25 C:333  Project WC2998                                    *
     h*                  Changes to CMS Settlement Reporting.              *
     h*                                                                    *
     h*  01/14/25 C:340  Project WC2995                                    *
     h*                  Recompile Only;                                   *
     h*                                                                    *
     h*  01/07/25 C:342  Support: 118194 Prescient                         *
     h*                 - Added edit for Location when it is not effective *
     h*                  at the DOI.                                       *
     h*                                                                    *
     h*  04/10/24 C:079 Ticket: 146/115349                                 *
     h*                 - Add claimdtl pointer to ErrorCheck3              *
     h*                                                                    *
     H*  03/22/24 C:333 Project: WC2826                                    *
     H*                 - Recompiled as DSPF WTCLMDTL changed.             *
     h*                                                                    *
     h*  01/30/24 C:344 Support: 114844 (Prescient)                        *
     h*                 - Remove verification of Date of Disability for    *
     h*                 the State of Pennsylvania 'PA' only for            *
     h*                 client = Prescient (146).                          *
     h*                                                                    *
     h*  04/10/23 C:333 Support: 108889 (UBIC)                             *
     h*                 - Recompiled as DSPF WTCLMDTL changed.             *
     h*                                                                    *
     h*  09/06/22 C:79  Project: WC2823                                    *
     h*                 Correct WV Date Error logic                        *
     h*                                                                    *
     h*  08/05/20 C:65  Project: WC2756                                    *
     h*                 Display error if payroll class is marked as        *
     h*                 excluded from claim assignment.                    *
     h*                                                                    *
     h*  01/15/20 C:053 Support: 86448                                     *
     h*                 Bypass Init Act RTW edit based on Jurisdiction ST  *
     h*                                                                    *
     h*  08/19/19 C:079 Project: WC2604                                    *
     h*                 Error handling for foreign location                *
     h*                                                                    *
     h*  06/17/19 C:180 Support: 71987                                     *
     h*                 Recompiled only                                    *
     h*                                                                    *
     h*  04/10/19 C:062 Project: 152WAB                                    *
     h*                 Apply CSI mods for S1 Medical.                     *
     h*                                                                    *
     h*  05/30/18 C:079 Project: WC2515                                    *
     h*                 Remove fields for Claims EDI 3.1                   *
     h*                                                                    *
     h*  05/30/18 C:180 Project: WC2515                                    *
     h*                 Validate Acquired Indemntity Trough date           *
     h*                                                                    *
     h*  02/14/17 C:212 Project: 90MHY                                     *
     h*                 Recompile over changed WTCLMDTL                    *
     h*                                                                    *
     h*  01/31/17 C:306 Project: WC2410                                    *
     h*                 Recompile over changed WTCLMDTL                    *
     h*                                                                    *
     h*  05/25/16 C:310 Support: 41713                                     *
     h*                 Correct handling of an edit on ORM date field.     *
     h*                                                                    *
     h*  05/24/16 C:62  Support: 41403(133)                                *
     h*                 Don't require reporting level is status set to O.  *
     h*                                                                    *
     h*  05/24/16 C:309 Support: PHW41670                                  *
     h*                 Recompile only                                     *
     h*                                                                    *
     h*  05/09/16 C:300 Project: 114WXV                                    *
     h*                 Add Cell Number to Claimant Demographics.          *
     h*
     h*  03/15/16 C:306 Support: PHW40321                            *
     h*                 Corrected the edit message on CMS Date of in.*
     h*                 cident, when it enters as blank.             *
     h*                                                              *
     h*  02/08/16 C:306 Project: WC2403                              *
     h*                 CMS Reporting Edits                          *
     h*                                                              *
     h*  12/11/15 C:53  Project: C133WPK
     h*                 Add errors for 2 new fields to screen for 133 only
     h*
     h*  12/02/15 C:212 Project: 25MUD                               *
     h*                 Recompile over changed WTCLMDTL              *
     h*                 Add error processing for new filed added in
     h*                 WTCLMDTL - rcdClmDtl0 for prior Claim#
     h*
     h*  07/02/15 C:306 Support: SEQ86962 / 31805                    *
     h*                 Added the email validation check on legal &  *
     h*                 Management tab.                              *
     h*
     h*  06/17/15 C:306 Support: PHW35163                            *
     h*                 Modified to not to show edit  message , if   *
     h*                 user selects the country and province zip    *
     h*                 code,instead fo address 2 fields on claim    *
     h*                 edit.                                        *
     h*
     h*  06/10/15 C:136 Project: 114WLF
     h*                 Refine claim edits so that they are index
     h*                 specific when changing reserves
     h*
     h*  04/17/15 C:300 Support: PHW34297                            *
     h*                 Remove "Actual RTW date edits (WC2067)"      *
     h*                 for Medical Only claims.                     *
     h*  12/23/14 C:062 Support: WC133 87540                         *
     h*                 Correct work release edits.                  *
     h*
     h*  12/01/14 C:279 Project: 114WLF                              *
     h*                 Added procedure EdtRsvChgs.                  *
     h*
     h*  11/13/14 C:298 Redmine: 4768                                *
     h*                 Modified to add edit for fields Adjuster     *
     h*                 and Supervisor.                              *
     h*
     h*  08/29/14 C:290 Support: 86558                               *
     h*                 Modified the CMS DOI edit, so that the CMS   *
     h*                 Incident date can be of prior to the DOI.    *
     h*                                                              *
     h*  08/13/14 C:279 Project: WC2067                              *
     h*                 Modified to add edit for Actual RTW date.    *
     h*                                                              *
     h*  08/04/14 C:283 Support: WC146 84781                         *
     h*                 Disable/Enable the Accident Location fields  *
     h*                 as per the Employer Premises Flag.           *
     h*
     h*  07/22/14 C:298 Support: WC099 85969                         *
     h*                 Error handling for inactive staff codes      *
     h*                 if value of field has not changed.           *
     h*
     h*  07/21/14 C:283 Support: WA045 86221                         *
     h*                 Correct the Mandatory error message for      *
     h*                 scPayClass and handle the visibility of      *
     h*                 asterisk's for scPayClass and scNCCI.        *
     h*
     h*  07/04/14 C:290 Support: WC114 85446                         *
     h*                 Changed the MCO Fein field to numeric and    *
     h*                 modified the MCO edits accordingly to accept *
     h*                 the blank and all zeros as valid.            *
     h*
     h*  06/17/14 C:279 Redmine: 4321
     h*                 Initialize error indicator for quick diary.  *
     h*                                                              *
     h*  06/12/14 C:291 Redmine: 4175
     h*                 Add Phone number check for Concurrent        *
     h*                 Employer phone number                        *
     h*                                                              *
     h*  03/04/14 C:180 Support: WC114 84947                         *
     h*                          Validate MCO                        *
     h*
     h*                                                              *
     h*  02/26/14 C:282 Project: 129WSC #Priss  25                   *
     h*                 Added the Edit for 'Apportionment Effective  *
     h*                 Date'.                                       *
     h*
     h*  02/13/14 C:180 Project WC2234 - PRISS 027                   *
     h*                 Validate EDI fields.                         *
     h*                                                              *
     h*  02/06/14 C:295 Support: WC102 82059                         *
     h*                  Allow entry of 'Full Salary Continued'      *
     h*                  on 'medical only' claims                    *
     h*                  modify edit to allow for                    *
     h*                  Loss Time = 'Medical' or 'Indeminity'       *
     h*                                                              *
     h*  12/10/13 C:062 Support: WC147 83560
     h*                 Refresh wmcmp after updating aww.
     h*
     h*  12/09/13 C:180 Support WC114 84236                          *
     h*                 ORM date in Medical tab allowed only         *
     h*                 if ORM indicator is checked                  *
     h*                                                              *
     h*  11/25/13 C:282 Project: 129WSC                              *
     h*                 Condition Edit for 'Apportionment Effective  *
     h*                 Date' for client 129 only.
     h*                                                              *
     h*  11/12/13 C:279 Project: SY266 Redmine# 2107                 *
     h*                 Modified Edit condition for Employer's       *
     h*                 premises and Accident Location Narrative.    *
     h*                                                              *
     h*  11/12/13W C:062 Support: WC146 84144
     h*                 Don't allow the lost time to be changed to R
     h*                 if there are reserves and/or payments
     h*
     h*  10/01/13 C:053 Project: 25WSS
     h*                 Retrofit for client.
     h*
     h*  09/26/13 C:279 Support: WC147 83614
     h*                 Conditioned Edit for 'Method of determining  *
     h*                 pre-inj wage' based on Claim/DARTS setup.    *
     h*
     h*  09/23/13 C:062 Support: WC114 83507
     h*                 Recompile only
     h*
     h*  08/29/13  C:279 Project:  WC2213                             *
     h*                  Conditioned Edit for AWW required based on   *
     h*                  Claim/DARTS setup.                           *
     h*
     H*  07/30/13  C:180 SUPPORT CI114 82954
     H*                 Validate accident description for invalid     *
     H*                 characters.                                   *
     H*
   H*  07/29/13  C:295 Support: WC133 82963                         *
   H*                  ORM date on the medical benefit screen       *
   H*                  should allow to enter future date.           *
     h*
     H* 06/21/13  C:65  PROJECT: SY266 Redmine #3034                  *
     H*                 Set the county selection criteria if the state*
     H*                 is populated.                                 *
     h*
     h* 02/20/13  C:282  Project:  147WAO
     h*                  Add validation for new field Per for FL State
     h*                  Information.
     h*
     h* 01/09/13  C:284  Project:  146WFQ
     h*                  Add validation for new field scFeet and scInch
     h*
     H* 07/27/11  C:190 PROJECT 114WZT                                *
     H*                 Made "Loss recovery type" required on         *
     H*                 particular condition.                         *
     H*                                                               *
     h**********************************************************************
     fwmcmp     if   e             Disk    ReName(pcm01:pcm0p)
     fwtgil     if   e           k Disk    ReName(pgi01:pgi00)
     fwtgtl     if   e           k Disk    ReName(pgt01:pgt00)
     fwdf1l     if   e           k Disk    ReName(pf101:pf100)
     fwmfnl     if   e           k Disk    ReName(pfn01:pfn00)
     fwmcxl     if   e           k Disk    ReName(pcx01:pcx00)
     fwmcll     if   e           k Disk
     fwmwhl     if   e           k disk    rename(pwh01:pwh00)
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwtabl     if   e           k disk    rename(prt01:prt00)
     Fsmcol     if   e           k disk    rename(pco01:pco0s)
     Fwmprl1    if   e           k disk    rename(ppr01:ppr01)
      ***** Beg Add *** 01/07/25 *******************************************
     Fwmall     if   e           k disk    rename(pal01:pal00)
      ***** End Add *** 01/07/25 *******************************************
     D xxClass         s              5  0

     d* Proto-types

      * calculate remaining reserve limit for user
     d ClcRemRsvLim    pr                  extpgm('SRRSVL')
     d  ##co                               like(CMco#)
     d  ##grp                              like(CMfnd)
     d  ##yr                               like(CMfyr)
     d  ##case                             like(CMcase)
     d  ##rsvCode                          like(CMrt01)
     d  ##limit                      11  2

      * Get the total Request Amounts
     d GetRequestAmt   pr             9  2
     d outRsvCd                       2
     d outClmRrn#                     9  0 const options(*nopass)

    d* Edit Control Record Api
     d RtvEdiRls       pr                  ExtPgm('WA076R')
     d intCO##                        3  0
     d strJRST                        2
     d strTCD                         3
     d intFnd#                        3  0
     d strRtncd                       1
     d strEdif                        3
     d strElt                         1
     d intEltd                        3  0
     d strFroi                        2
     d intSdue                        3  0
     d strRls                         1
     d intCsdt                        7  0
     d strEadc                        5
     d strHcpr                        5
     d strFnrc                        5
     d strHced                        1

     d* Error Check for A1 State Info
     d ErrorCheckA1    pr              n
       //*** Beg Add *** 09/06/22 *********************************************
     d statePtrInA1                    *
       //*** End Add *** 09/06/22 *********************************************
     d statePtrIn                      *
       //*** Beg Add *** 09/06/22 *********************************************
     d froiStateIn                    2
       //*** End Add *** 09/06/22 *********************************************

     d* Error Check for AL State Info
     d ErrorCheckAL    pr              n
     d statePtrIn                      *

     d* Error Check for CA State Info
     d ErrorCheckCA    pr              n
     d statePtrIn                      *

     d* Error Check for DE State Info
     d ErrorCheckDE    pr              n
     d statePtrIn                      *

     d* Error Check for FL State Info
     d ErrorCheckFL    pr              n
     d statePtrIn                      *

     d* Error Check for GA State Info
     d ErrorCheckGA    pr              n
     d statePtrIn                      *

     d* Error Check for LA State Info
     d ErrorCheckLA    pr              n
     d statePtrIn                      *

     d* Error Check for MA State Info
     d ErrorCheckMA    pr              n
     d statePtrIn                      *

     d* Error Check for NC State Info
     d ErrorCheckNC    pr              n
     d statePtrIn                      *

     d* Error Check for NY State Info
     d ErrorCheckNY    pr              n
     d statePtrIn                      *

     d* Error Check for PA State Info
     d ErrorCheckPA    pr              n
     d statePtrIn                      *

     d* Error Check for WV State Info
     d ErrorCheckWV    pr              n
     d statePtrIn                      *

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

     d* General Prototypes
     d/copy wPrClmErr
     d/copy wPrClmErrM
     d/copy sPrDate
     d/copy wPrClmPop
     d/copy sPrGenErr
     d/copy wPrEmplr
     d/copy wPrUsrPrms
       //*** Beg Add *** 09/06/22 **********************************************
     d/copy wPrClaims
       //*** End Add *** 09/06/22 **********************************************

     D valueIn         s             80
     D staffCdIn       s              3
     D valueIn1        s             80
     D numValueIn      s             15  2
     D dateValueIn     s              8  0
     D genDate         s              8  0
     D errorMsg        s             50
     D errorMsgId      s             10
     D outCltID        s              3
     D kkTbId          s                   like(GTtbId)
     D kkKey           s                   like(GTkey)
     d intCO##         s              3  0
     d strJRST         s              2
     d strTCD          s              3
     d intFnd#         s              3  0
     d strRtncd        s              1
     d strEdif         s              3
     d strElt          s              1
     d intEltd         s              3  0
     d strFroi         s              2
     d intSdue         s              3  0
     d strRls          s              1
     d intCsdt         s              7  0
     d strEadc         s              5
     d strHcpr         s              5
     d strFnrc         s              5
     d strHced         s              1
     D ##type          s              1
     D numbers         s             10    inz('0123456789')
     D position        s              2  0 inz(*zero)
     D totalIncurred   s             15  2

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

     d  rsvCodeDs      ds
     d  CMrt01
     d  CMrt02
     d  CMrt03
     d  CMrt04
     d  CMrt05
     d  CMrt06
     d  CMrt07
     d  CMrt08
     d  CMrt09
     d  CMrt10
     d  rsvCode                       2    overlay(rsvCodeDs) dim(10)

     D fids01        e ds                  ExtName(szq2p)
     D psds          esds                  ExtName(szq1p)

     C     keyWTGIL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkTbId

     C     keyWTGTL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    kkTbId
     C                   Kfld                    kkKey

     C     keyWMFNL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd

     C     keyWDF1L      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMfYr
       //===================================================================
       // Procedure Definition Section
       //===================================================================
       //===================================================================
       // ErrorCheck0 - Validate Claim Information screen fields.
       //===================================================================
     P ErrorCheck0     B                   Export
     D ErrorCheck0     PI              n
     D claimRrnIn                     9  0
     D claimPtrIn                      *
     D claimInfo     e ds                  ExtName(wtClmDtl:rcdClmDt0)
     D                                     based(claimPtr)
     D claimRrn        s              9  0
     D claimPtr        s               *   inz
     D error           s               n
     D i               s              3  0
     d occupationPos   s              3  0
     d*  Occupation Desc Const
     d conocd          C                   Const('ABCDEFGHIJKLMNOPQRS-
     d                                     TUVWXYZ1234567890 abcdefgh-
     d                                     ijklmnopqrstuvwxyz')

     C     keyWMCLL      Klist
     C                   Kfld                    CMco#
     C                   Kfld                    CMfnd
     C                   Kfld                    CMemp#
     C                   Kfld                    CMdiv
     C                   Kfld                    scCampus
      /Free

       GetClientID(outCltID);
       claimPtr=claimPtrIn;
       claimRrn=claimRrnIn;

       Chain claimRrn pcm0p;
       Chain (CMco#:CMfnd) pcx00;
       totalIncurred = %Xfoot(rsvRemain) + %Xfoot(rsvPaid);

       If scClmSt <> '  ';
          scClmCnSel='SUBSTR(CNKEY,1,2)=''' + scClmSt + '''';
       Endif;

       // Claim Information =========================================

       // Claimant Last Name Is Required
       valueIn=scLsN;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgLsN=errorMsg;
          erLsN=*On;
          error=*On;
       Else;
          mgLsN=*Blanks;
          erLsN=*Off;
       Endif;

       // Claimant First Name Is Required
       valueIn=scFsN;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgFsN=errorMsg;
          erFsN=*On;
          error=*On;
       Else;
          mgFsN=*Blanks;
          erFsN=*Off;
       Endif;

       // Claimant Address 1 Is Required
       valueIn=scClmAdr1;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgClmAdr1=errorMsg;
          erClmAdr1=*On;
          error=*On;
       Else;
          mgClmAdr1=*Blanks;
          erClmAdr1=*Off;
       Endif;

       // Claimant City Is Required
       If Sccountry = ' ' And Scprovince = ' ';
       valueIn=scClmCty;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgClmCty=errorMsg;
          erClmCty=*On;
          error=*On;
       Else;
          mgClmCty=*Blanks;
          erClmCty=*Off;
       Endif;
       Else;
          mgClmCty=*Blanks;
          erClmCty=*Off;
       Endif;

       // Claimant State Is Required
       If Sccountry = ' ' And Scprovince = ' ';
       valueIn=scClmSt;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgClmSt=errorMsg;
          erClmSt=*On;
          error=*On;
       Else;
          mgClmSt=*Blanks;
          erClmSt=*Off;
       Endif;
       Else;
          mgClmSt=*Blanks;
          erClmSt=*Off;
       Endif;

       // Claimant Zip Is Required
       If Sccountry = ' ' And Scprovince = ' ';
       numValueIn=scClmZip;
       If ChkNotZero(numValueIn:errorMsg:errorMsgId)=*On;
          mgClmZip=errorMsg;
          erClmZip=*On;
          error=*On;
       Else;
          mgClmZip=*Blanks;
          erClmZip=*Off;
       Endif;
       Else;
          mgClmZip=*Blanks;
          erClmZip=*Off;
       Endif;

       // Devereux Cause code is Required
       If devGroup=*On;
          valueIn=scDevCauCd;
          If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
             mgDevCauCd=errorMsg;
             erDevCauCd=*On;
             error=*On;
          Else;
             mgDevCauCd=*Blanks;
             erDevCauCd=*Off;
          Endif;
          // Devereux Restraint Type is Required if Cause Code=01
          devGroup2=*Off;
          mgDevRstTp=*Blanks;
          erDevRstTp=*Off;
          If scDevCauCd='01';
             devGroup2=*On;
             valueIn=scDevRstTp;
             If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
                mgDevRstTp=errorMsg;
                erDevRstTp=*On;
                error=*On;
             Endif;
          EndIf;
       EndIf;

       // Validate Claim Home phone#
       ##type = 'P';
       erClmPhn# = Check_Phone#(scClmPhn#:##type:mgClmPhn#);
       If erClmPhn# = *on;
          error = *on;
       Endif;

       // Validate Claim Work Phone#
       erClmWrk# = Check_Phone#(scClmWrk#:##type:mgClmWrk#);
       If erClmWrk# = *on;
          error = *on;
       Endif;

       // Validate Claim Mobile Phone#
       erClmCel# = Check_Phone#(scClmCel#:##type:mgClmCel#);
       If erClmCel# = *on;
          error = *on;
       Endif;

       // Birth Date Must Be valid If entered
       mgBirthDat=*Blanks;
       erBirthDat=*Off;
       If scBirthDat<>0 Or rqBirthDat=*On;
          dateValueIn=scBirthDat;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgBirthDat=errorMsg;
             erBirthDat=*On;
             error=*On;
          EndIf;
       EndIf;

       // Birth Date Must Less than Accident Date
       If scBirthDat<>0;
          dateValueIn=scBirthDat;
          If ChkGtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgBirthDat=errorMsg;
             erBirthDat=*On;
             error=*On;
          EndIf;
       EndIf;

       // Hire Date Must Be valid If entered
       mgHireDate=*Blanks;
       erHireDate=*Off;
       If scHireDate<>0 Or rqHireDate=*On;
          dateValueIn=scHireDate;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgHireDate=errorMsg;
             erHireDate=*On;
             error=*On;
          EndIf;
       EndIf;

       // Only allow Alpha and Blanks
       erOccDesc=*Off;
       mgOccDesc=' ';
       // Removed edit for Client CSI under redmine# 2067.
       If outCltID <> '114';
          If scOccDesc<>*Blanks;
             For i = 1 to %len(scOccDesc);
                occupationPos = 0;
                occupationPos = %Scan(%subst(scOccdesc:i:1):conocd);
                If occupationPos = 0;
                   mgOccDesc='Field must only contain a space, spaces or valid'+
                            ' alphabetic charcters.';
                   erOccDesc=*On;
                   error=*On;
                   Leave;
                EndIf;
             Endfor;
          EndIf;
       EndIf;

       // Hire Date Must Less than Accident Date
       If scHireDate<>0;
          dateValueIn=scHireDate;
          If ChkGtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgHireDate=errorMsg;
             erHireDate=*On;
             error=*On;
          Else;
             If ChkFutrDate(dateValueIn:errorMsg:errorMsgId)=*On;
                mgHireDate=errorMsg;
                erHireDate=*On;
                error=*On;
             EndIf;
          EndIf;
       EndIf;


       // Validate EDI Week type
       erWeekTyp=*Off;
       mgWeekTyp=' ';
       Select;
          When scWWTC = 'S' and (scWDSSu + scWDSM + scWDST + scWDSW +
                                 scWDSR + scWDSF + scWDSSa) <> 'NSSSSSN'
                            and (scWDSSu + scWDSM + scWDST + scWDSW +
                                 scWDSR + scWDSF + scWDSSa) <> 'NNNNNNN';
               mgWeekTyp = 'Standard week must be Monday through Friday.';
               erWeekTyp = *On;
               error=*On;
          When scWWTC = 'F' and (scWDSSu + scWDSM + scWDST + scWDSW +
                                 scWDSR + scWDSF + scWDSSa) =  'NNNNNNN';
               mgWeekTyp = 'Specify days for the fixed week.';
               erWeekTyp = *On;
               error=*On;
          When scWWTC = *BLANKS and (scWDSSu = 'S' or scWDSM = 'S' or
                                     scWDST  = 'S' or scWDSW = 'S' or
                                     scWDSR  = 'S' or scWDSF = 'S' or
                                     scWDSSa = 'S');
               mgWeekTyp = 'Specify week type or clear work schedule.';
               erWeekTyp = *On;
               error=*On;
          Endsl;

       // Accident Description Is Required
       valueIn=scAccDesc1;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgAccDesc1=errorMsg;
          erAccDesc1=*On;
          error=*On;
       Else;
       // Check for invalid characters
           If ChkBadChar(valueIn:errorMsg:errorMsgId)=*On;
              mgAccDesc1=errorMsg;
              erAccDesc1=*On;
              error=*On;
           Else;
              mgAccDesc1=*Blanks;
              erAccDesc1=*Off;
           Endif;
       Endif;

       // Check for invalid characters
       valueIn=scAccDesc2;
           If ChkBadChar(valueIn:errorMsg:errorMsgId)=*On;
              mgAccDesc2=errorMsg;
              erAccDesc2=*On;
              error=*On;
           Else;
              mgAccDesc2=*Blanks;
              erAccDesc2=*Off;
           Endif;
       // Gender Is Required
       valueIn=scGender;
       mgGender=*Blanks;
       erGender=*Off;
       If rqGender=*On;
          If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
             mgGender=errorMsg;
             erGender=*On;
             error=*On;
          EndIf;
       Endif;

       // Validate Height: Inch should be between 0 to 12.

       If scInch <> ' ';
          If not (%int(scInch) >= 0 And %int(scInch) <= 12);
             erInch = '1';
             error=*On;
          Else;
             erInch = '0';
          Endif;
       Endif;

       //*** Beg Add ***** 01/07/25 ***************************************
       erLocation = *off;
       mgLocation = *blanks;
       If scLocation <> ' ';
          Chain (CMco#:CMfnd:CMemp#:CMdiv:scLocation) pal00;
          If %found(wmall);
             If ALefdt > CMacdt and (ALdldt=0 or (ALdldt<>0 and ALdldt>=
                CMacdt)) and CMacdt <> 0;
                mgLocation = 'Location is not effective as of Incident Date';
                erLocation = *on;
                error = *On;
             Endif;
          Endif;
       Endif;
       //*** End Add ***** 01/07/25 ***************************************

       // Campus code is Required for CLT025
       If (outCltID = '025');
          valueIn=scCampus;
          If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
             mgCampus=errorMsg;
             erCampus=*On;
             error=*On;
          Else;
              mgCampus=*Blanks;
              erCampus=*Off;
           If (scCampus <> svCampus);
              Chain keyWMCLL pCL01;
              If %Found(WMCLL);
               scPlcName=CLdesc;
               scPlcAddr=CLaddr;
               scPlcCity=CLccty;
               scPlcSt=CLcst;
               scPlcZip=CLczip;
              Endif;
              svCampus=scCampus;
           Endif;
       Endif;
       Endif;
       // If On Premises is Yes then the address is required
       mgPlcName=*Blanks;
       erPlcName=*Off;
       mgPlcAddr=*Blanks;
       erPlcAddr=*Off;
       mgPlcCity=*Blanks;
       erPlcCity=*Off;
       mgPlcSt=*Blanks;
       erPlcSt=*Off;
       mgPlcZip=*Blanks;
       erPlcZip=*Off;
       mgNarrativ = *Blanks;
       erNarrativ = *Off;
       mgAsLc = *Blanks;
       erAsLc = *Off;

       // Check to see if group is excluded from edi
       Chain keyWMFNL pfn00;

       If scPlcZip > 0;
          valueIn=scPlcName;
          If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
             mgPlcName=errorMsg;
             erPlcName=*On;
             error=*On;
          Endif;
          valueIn=scPlcAddr;
          If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
             mgPlcAddr=errorMsg;
             erPlcAddr=*On;
             error=*On;
          Endif;
          valueIn=scPlcCity;
          If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
             mgPlcCity=errorMsg;
             erPlcCity=*On;
             error=*On;
          Endif;
          valueIn=scPlcSt;
          If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On and
             scAsLc = *blanks;
             mgPlcSt=errorMsg;
             erPlcSt=*On;
             error=*On;
          Endif;
          If scPlcSt <> *blanks and scAsLc <> *blanks;
             mgAsLc='Foreign location and Accident State cannot both +
                     be entered';
             erAsLc=*On;
             error=*On;
          Endif;
          numValueIn=scPlcZip;
          If ChkNotZero(numValueIn:errorMsg:errorMsgId)=*On;
             mgPlcZip=errorMsg;
             erPlcZip=*On;
             error=*On;
          Endif;
       Else;
          If scPremises <> 'Y' and FNedi = ' ' and scNarrativ = ' ';
             mgPlcZip = 'The street address or Accident Location Narrative is +
              required.';
             mgNarrativ = 'The street address or Accident Location Narrative +
              is required.';
             erPlcZip = *On;
             erNarrativ = *On;
             error = *On;
          Endif;
       Endif;
       If scPremises = 'Y' Or dspMode0 = *On;
          indEmpPre = *On;
       Else;
          indEmpPre = *Off;
       Endif;

       // Accident county not allowed if out of state
       mgPlcCnty=*Blanks;
       erPlcCnty=*Off;
       If scJurState='GA' and scPlcSt<>'GA' and scPlcCnty<>*Blanks;
       // mgPlcCnty='County not allowed, if out of state.';
       EndIf;

       // Jurisdiction State is Required
       valueIn=scJurState;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgJurState=errorMsg;
          erJurState=*On;
          error=*On;
       Else;
          mgJurState=*Blanks;
          erJurState=*Off;
       Endif;

       // TPA Office # is Required
       valueIn=scTpa#;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgTpa#=errorMsg;
          erTpa#=*On;
          error=*On;
       Else;
          mgTpa#=*Blanks;
          erTpa#=*Off;
       Endif;

       // Prior Claim# should contain valid characters
       erPriClm = *Off;
       mgPriClm = *blanks;
       // Add edit for Client TASB only
       If outCltID = '025';
          If %check(conocd:scPrClm#) <> 0;
             mgPriClm = 'Invalid characters in data';
             erPriClm = *on;
             error=*On;
          Else;
             mgPriClm = *Blanks;
             erPriClm = *Off;
          Endif;
       EndIf;

       // Payroll is Required is the group says it is.
       mgPayClass=*Blanks;
       erPayClass=*Off;
       indPayClas = *Off;
       Chain keyWDF1L pf100;
       If %Found(WDF1L) And F1rlxC<>'Y' And
                        (scPayClass='00000  ' or scPayClass=*Blanks);
          indPayClas = *On;
          mgPayClass='Payroll Class Code is required.';
          erPayClass=*On;
          error=*On;
       EndIf;
       Monitor;
          xxclass=%Dec(%SubSt(scPayClass : 1 : 5) : 5 : 0);
       On-Error 105;
          xxclass = 0;
       EndMon;
       Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
       Setgt (CMco#:ELrtcd:xxClass:CMacdt) ppr01;
       Readpe (CMco#:ELrtcd:xxClass) ppr01;
       If erPayClass = *off and not %eof and PRExClm = 'Y';
          indPayClas = *On;
          mgPayClass='Payroll class cannot be selected for assignment.';
          erPayClass=*On;
          error=*On;
       Endif;

       // Report to Employer Date Must Be valid If entered
       mgRptEmp=*Blanks;
       erRptEmp=*Off;
       dateValueIn=scRptEmp;
       If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
          mgRptEmp=errorMsg;
          erRptEmp=*On;
          error=*On;
       Else;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgRptEmp=errorMsg;
             erRptEmp=*On;
             error=*On;
          EndIf;
       EndIf;

       // Category 2 field can not be blank for CSI specific groups

       mgCata2=*Blanks;
       erCata2=*Off;
       If  (CMfnd= 350 or CMfnd= 385 or CMfnd= 400  or CMfnd= 410
           or CMfnd= 420 or CMfnd= 430 or CMFnd=440 or CMfnd=450
           or CMfnd=460) and outCltID = '114';
       valueIn=scCata2C;
          If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
             mgCata2=errorMsg;
             erCata2=*On;
             error=*On;
          EndIf;
       EndIf;

       erCarCdInd = *off;
       If (CMfnd= 440 or CMfnd= 460) and outCltID = '114';
          If scCmps = ' ';
             erCarCdInd = *On;
             error = *On;
          EndIf;
       EndIf;

       // FROI receive date Must Be valid If entered
       mgRecDate=*Blanks;
       erRecDate=*Off;
       dateValueIn=scRecDate;
       If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
          mgRecDate=errorMsg;
          erRecDate=*On;
          error=*On;
       Else;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgRecDate=errorMsg;
             erRecDate=*On;
             error=*On;
          Else;
             If ChkFutrDate(dateValueIn:errorMsg:errorMsgId)=*On;
                mgRecDate=errorMsg;
                erRecDate=*On;
                error=*On;
             EndIf;
          EndIf;
       EndIf;

       // Claimant contact Date Must Be valid If entered
       mgConDate=*Blanks;
       erConDate=*Off;
       If scConDate<>0;
          dateValueIn=scConDate;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgConDate=errorMsg;
             erConDate=*On;
             error=*On;
          Else;
             If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
                mgConDate=errorMsg;
                erConDate=*On;
                error=*On;
             Else;
                If ChkFutrDate(dateValueIn:errorMsg:errorMsgId)=*On;
                   mgConDate=errorMsg;
                   erConDate=*On;
                   error=*On;
                EndIf;
             EndIf;
          EndIf;
       EndIf;

       // Days per Week must be 0-7
       mgDays=' ';
       erDays=*Off;
       If scDays<0 or scDays>7;
          mgDays='Invalid number of days per week. Should be 0-7.';
          erDays=*On;
       EndIf;

       // Hours per Day must be 0-24
       mgHours=' ';
       erHours=*Off;
       If scHours<0 or scHours>24;
          mgHours='Invalid number of hours per day. Should be 0-24.';
          erHours=*On;
       EndIf;

       // Nature is Required
       valueIn=scNature;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgNature=errorMsg;
          erNature=*On;
          error=*On;
       Else;
          mgNature=*Blanks;
          erNature=*Off;
       Endif;

       // Body PartPart is Required
       valueIn=scPart;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgPart=errorMsg;
          erPart=*On;
          error=*On;
       Else;
          mgPart=*Blanks;
          erPart=*Off;
       Endif;

       // Cause is Required
       valueIn=scCause;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgCause=errorMsg;
          erCause=*On;
          error=*On;
       Else;
          mgCause=*Blanks;
          erCause=*Off;
       Endif;

       // NCCI Code is Required
       valueIn=scNcci;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          If scLostTime<>'R';
             mgNcci=errorMsg;
             erNcci=*On;
             error=*On;
             indNCCI = *On;
          EndIf;
       Else;
          // Check to see if it matches the lost time flag
          indNCCI = *Off;
          kkTbId=CMjrSt + 'NC';
          Chain keyWTGIL pgi00;
          If Not %Found(WTGIL);
             kkTbId='  NC';
          EndIf;

          If scLostTime<>'R';
             kkKey=scNcci;
             Chain keyWTGTL pgt00;
             If %Found(WTGTL) and ((GTfg01='M' And scLostTime<>'M') Or
                                   (GTfg01=' ' And scLostTime<>'I'));
                mgNcci='The NCCI code must be the same type' +
                         ' as the Lost Time flag.';
                erNcci=*On;
                error=*On;
             Else;
                mgNcci=*Blanks;
                erNcci=*Off;
             EndIf;
          EndIf;
       Endif;

       // Coverage Code is Required
       valueIn=scCovCode;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgCovCode=errorMsg;
          erCovCode=*On;
          error=*On;
       Else;
          mgCovCode=*Blanks;
          erCovCode=*Off;
       Endif;

       // Lost Time is Required
       valueIn=scLostTime;
       If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
          mgLostTime=errorMsg;
          erLostTime=*On;
          error=*On;
       Else;
          mgLostTime=*Blanks;
          erLostTime=*Off;
       Endif;

       // Don't allow lost time to be changed to R if there are any Reserves or
       If CMltFl<>'R'  And scLostTime='R' And totalIncurred<>0;
          mgLostTime='No payment/ reserves can be on ' +
             'a record only claim.';
          erLostTime=*On;
          error=*On;
       Else;
          mgLostTime=*Blanks;
          erLostTime=*Off;
       EndIf;

       // If flag is set then Loss Recovery type is required
       If CXlsrv = 'Y' And scLostTime = 'I' And scRecType = ' ';
          mgRecType='Loss Recovery Time must be selected';
          erRecType=*On;
          error=*On;
       Else;
          mgRecType=*Blanks;
          erRecType=*Off;
       Endif;

       // Carrier Code is required for CSI if group is 440 or 460

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free

     P ErrorCheck0     E
       //===================================================================
       // ErrorCheck1 - Validate Management Information screen fields.
       //===================================================================
     P ErrorCheck1     B                   Export
     D ErrorCheck1     PI              n
     D claimRrnIn                     9  0
     D mgmtPtrIn                       *
     D mode                           1
     D mgmtInfo      e ds                  ExtName(wtClmDtl:rcdClmDt1)
     D                                     based(mgmtPtr)
     D claimRrn        s              9  0
     D mgmtPtr         s               *   inz
     D error           s               n
     D denyReason      s            150
      /Free

       GetClientID(outCltID);
       claimRrn=claimRrnIn;
       mgmtPtr=mgmtPtrIn;

       Chain claimRrn pcm0p;

       // Management Information =========================================

       // Claim Aduster Is Required
       staffCdIn=scClmAdj;
       If scClmAdj <> svClmAdj or scClmAdj = *Blanks;
          If InvalidStaffCd(staffCdIn:errorMsg:errorMsgId)=*On;
             mgClmAdj=errorMsg;
             erClmAdj=*On;
             error=*On;
          Endif;
       Else;
          mgClmAdj=*Blanks;
          erClmAdj=*Off;
       Endif;

       // Supervisor Is Required
       staffCdIn=scSuperv;
       If scSuperv <> svSuperv or scSuperv = *Blanks;
          If InvalidStaffCd(staffCdIn:errorMsg:errorMsgId)=*On;
             mgSuperv=errorMsg;
             erSuperv=*On;
             error=*On;
          Endif;
       Else;
          mgSuperv=*Blanks;
          erSuperv=*Off;
       Endif;


       // Case Nurse is required for CSI, if changed
       staffCdIn=scCsNurse;
       If (InvalidStaffCd(staffCdIn:errorMsg:errorMsgId)=*On)
        and scCsNurse <> svCsNurse
        and (outCltID = '114' Or outCltID = '152');
          mgCsNurse=errorMsg;
          erCsNurse=*On;
          error=*On;
       Else;
          mgCsNurse=*Blanks;
          erCsNurse=*Off;
       Endif;

       If outCltID  = '133' and mode='C';
          If scDCIsts<>*blanks And scDCIsts <> 'O' and scValLvl=0;
             mgValLvl='If DCI Status entered, Valuation Level is required';
             erValLvl=*On;
          Else;
             erValLvl=*Off;
          EndIf;
          If scDCIsts=*blanks and scValLvl<>0;
             mgDCISts='If Valuation Level entered, DCI Status is required';
             erDCISts=*On;
          Else;
             erDCISts=*Off;
          EndIf;
       EndIf;

       // Full Denial Effective Date Must be valid if entered
       mgFdEffDt=*Blanks;
       erFdEffDt=*Off;
       If scFdEffDt<>0;
          dateValueIn=scFdEffDt;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgFdEffDt=errorMsg;
             erFdEffDt=*On;
             error=*On;
          EndIf;
       EndIf;

       // Full Denial Rescission Date Must be valid if entered
       mgFdResDt=*Blanks;
       erFdResDt=*Off;
       If scFdResDt<>0;
          dateValueIn=scFdResDt;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgFdResDt=errorMsg;
             erFdResDt=*On;
             error=*On;
          EndIf;
       EndIf;


       // Acquired Indemnity Through Date must be valid if entered.
        mgAcInTrDt=' ';
        erAcInTrDt=*Off;
        If scAcInTrDt>0;
           If DtCheck(scAcInTrDt) = 0;
              mgAcInTrDt='Acquired Indemnity Date is invalid.';
              erAcInTrDt=*On;
              error=*On;
           EndIf;
        EndIf;

       // Partial Denial Effective Date must be valid if entered.
        mgParDenDt=' ';
        erParDenDt=*Off;
        If scParDenDt>0;
           If DtCheck(scParDenDt) = 0;
              mgParDenDt='Partial Denial Effective Date is invalid.';
              erParDenDt=*On;
              error=*On;
           EndIf;
        EndIf;

       // Denial Field Edits
       intCo## = CMco#;
       strJrst = CMjrSt;
       strTcd  = *blank;

       RtvEdiRls(intCo##:strJrst:strTcd:intFnd#:strRtncd:
                   strEdif:strElt:intEltd:strFroi:intSdue:
                   strRls:intCsdt:strEadc:strHcpr:strFnrc:
                   strHced);

       // Full Denial Effective date is mandatory if Claim denied flag is
       // changed from ' ' or 'N' to 'Y'.

       If (outCltID = '133' or strRls = '3') And mode='C';

          If (scDenied = 'Y' AND CMdny = ' ')
          Or (scDenied = 'Y' AND CMdny = 'N');
             If scFdEffDt = *zero;
                mgFdEffDt='Invalid Date';
                erFdEffDt=*On;
                error=*On;
             Endif;
          Endif;

       // Full Denial Rescission Date is mandatory if Claim denied flag is
       // changed from 'Y' to 'N' or ' '.

          If (scDenied = 'N' AND CMdny = 'Y') Or
             (scDenied = ' ' AND CMdny = 'Y');
             If scFdResDt = *zero;
                mgFdResDt='Invalid Date';
                erFdResDt=*On;
                error=*On;
             Endif;
          EndIf;

       // At least one Full Denial Reason should be selected if Claim denied fla
       // changed from ' ' or 'N' to 'Y'.

          mgFdReas1=' ';
          erFdReas1=*Off;
          If (scDenied = 'Y' AND CMdny = ' ')
          Or (scDenied = 'Y' AND CMdny = 'N');
             If (scFdReas1 = *blanks and scFdReas2 = *blanks and
                 scFdReas3 = *blanks and scFdReas4 = *blanks and
                 scFdReas5 = *blanks);
                mgFdReas1='Must not be blank.';
                erFdReas1=*On;
                error=*On;
             Endif;
          Endif;

       // Something must be placed or changed in the Denial Reason Narrative if
       // Claim Denied flag changed from ' ' or 'N' to 'Y' or
       // Claim Denied flag changed from 'Y' to 'N or ' '.

          mgDenReas1=' ';
          erDenReas1=*Off;
          If (scDenied = 'Y' AND CMdny = ' ') Or
             (scDenied = 'Y' AND CMdny = 'N') Or
             (scDenied = 'N' AND CMdny = 'Y') Or
             (scDenied = ' ' AND CMdny = 'Y');
             denyReason=scDenReas1+scDenReas2+scDenReas3;
             If (denyReason= *blanks Or denyReason = CMdnyN);
                mgDenReas1='Narrative mut be entered or changed.';
                erDenReas1=*On;
                error=*On;
             Endif;
          EndIf;

       EndIf;

       // Validate Responsible party phone#
       ##type = 'P';
       erRspPhn# = Check_Phone#(scRspPhn#:##type:mgRspPhn#);
       If erRspPhn# = *on;
          error = *on;
       Endif;

       // Validate Insurance company Phone#
       erInsPhn# = Check_Phone#(scInsPhn#:##type:mgInsPhn#);
       If erInsPhn# = *on;
          error = *on;
       Endif;

       // Validate Responsible party EmailId
       erRspEmAd = *Off;
       If %len(%trim(scRspEmAd)) > 0;
          erRspEmAd = Check_Email(scRspEmAd:mgRspEmAd);
          If erRspEmAd = *On;
             error = *on;
          EndIf;
       EndIf;

       // Validate Insurance Company EmailId
       erInsEmAd = *off;
       If %len(%trim(scInsEmAd)) > 0;
          erInsEmAd = Check_Email(scInsEmAd:mgInsEmAd);
          If erInsEmAd = *On;
             error = *on;
          EndIf;
       EndIf;


       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheck1     E
       //===================================================================
       // ErrorCheck2 - Validate the Medical Tab
       //===================================================================
     P ErrorCheck2     B                   Export
     D ErrorCheck2     PI              n
     D claimRrnIn                     9  0
     D medicalPtrIn                    *
     D medicalInfo   e ds                  ExtName(wtClmDtl:rcdClmDt2)
     D                                     based(medicalPtr)
     D claimRrn        s              9  0
     D medicalPtr      s               *   inz
     D error           s               n
      /Free

       claimRrn=claimRrnIn;
       medicalPtr=medicalPtrIn;

       Chain claimRrn pcm0p;

       // Medical Information    =========================================

       // Surgery Date must be Valid if entered.
       mgSurgDate=' ';
       erSurgDate=*Off;
       If scSurgDate<>*Zeros And dtCheck(scSurgDate)=0;
          mgSurgDate='Invalid Date.';
          erSurgDate=*On;
          error=*On;
       Else;
          dateValueIn=scSurgDate;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgSurgDate=errorMsg;
             erSurgDate=*On;
             error=*On;
          EndIf;
       Endif;

       // Rehab Date must be Valid if entered.
       mgRehabDt=' ';
       erRehabDt=*Off;
       If scRehabDt<>*Zeros And dtCheck(scRehabDt)=0;
          mgRehabDt='Invalid Date.';
          erRehabDt=*On;
          error=*On;
       Else;
          dateValueIn=scRehabDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgRehabDt=errorMsg;
             erRehabDt=*On;
             error=*On;
          EndIf;
       Endif;

       // Anticipated MMI Date must be Valid if entered.
       mgAntMmi=' ';
       erAntMmi=*Off;
       If scAntMmi<>*Zeros And dtCheck(scAntMmi)=0;
          mgAntMmi='Invalid Date.';
          erAntMmi=*On;
          error=*On;
       Else;
          dateValueIn=scAntMmi;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgAntMmi=errorMsg;
             erAntMmi=*On;
             error=*On;
          EndIf;
       Endif;

       // CMS Date must be Valid if entered.
       mgCmsDate=' ';
       erCmsDate=*Off;
       If scCmsDate<>*Zeros And dtCheck(scCmsDate)=0;
          mgCmsDate='Invalid Date.';
          erCmsDate=*On;
          error=*On;
       Else;
          dateValueIn=scCmsDate;
             If ChkFutrDate(dateValueIn:errorMsg:errorMsgId)=*On;
                mgCmsDate=errorMsg;
                erCmsDate=*On;
                error=*On;
             EndIf;
          Chain (CMco#) pco0s;
          If %found(smcol) and COrcdi = 'Y';
             If dtCYMD(scCmsDate) < CMacDt And scCmsDate<>*Zeros;
                mgCmsDate='CMS Date of Incident to be >= '+
                          'Claim Date of Injury.';
                erCmsDate=*On;
                error=*On;
             EndIf;
          Endif;
       Endif;

       // Released to Work Date must be Valid if entered.

       // Final MMI Date must be Valid if entered.
       mgFinMmi=' ';
       erFinMmi=*Off;
       If scFinMmi<>*Zeros And dtCheck(scFinMmi)=0;
          mgFinMmi='Invalid Date.';
          erFinMmi=*On;
          error=*On;
       Else;
          dateValueIn=scFinMmi;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgFinMmi=errorMsg;
             erFinMmi=*On;
             error=*On;
          EndIf;
       Endif;

       // Death Date must be Valid if entered.
       mgDeathDt=' ';
       erDeathDt=*Off;
       dateValueIn=scDeathDt;
       If scDeathDt<>*Zeros And dtCheck(dateValueIn)=0;
          mgDeathDt='Invalid Date.';
          erDeathDt=*On;
          error=*On;
       Else;
          scDeathDt=dtCheck(dateValueIn);
          If ChkFutrDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgDeathDt=errorMsg;
             erDeathDt=*On;
             error=*On;
          Else;
             If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
                mgDeathDt=errorMsg;
                erDeathDt=*On;
                error=*On;
             EndIf;
          EndIf;
       Endif;

       // ORM Date must be Valid if entered.
       mgOrmDate=' ';
       erOrmDate=*Off;
       If scOrmDate<>*Zeros And dtCheck(scOrmDate)=0;
          mgOrmDate='Invalid Date.';
          erOrmDate=*On;
          error=*On;
       Else;
          dateValueIn=scOrmDate;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgOrmDate=errorMsg;
             erOrmDate=*On;
             error=*On;
          Else;
             If scOrmDate<>*Zeros and (scOrmind = 'N' or scOrmind = ' ');
                mgOrmDate='Date requires ORM indicator.';
                erOrmDate=*On;
                error=*On;
                EndIf;
          EndIf;
          // Check ORM termination date.
          If  scOrmDate <> *zeros;
             If  dtCYMD(scOrmDate) <  dtCYMD(scCmsDate) and scOrmind = 'Y';
                 mgOrmDate='ORM Termination Date to be >= ' +
                           'to the CMS Date of Incident';
                 erOrmDate=*On;
                 error=*On;
             EndIf;
          EndIf;

       Endif;

       // Year Last Exposed Must be valid year.
       mgYrLstExp=' ';
       erYrLstExp=*Off;
       genDate=01010000 + scYrLstExp;
       If scYrLstExp<>*Zeros And (dtCheck(genDate)=0 Or scYrLstExp<1900);
          mgYrLstExp='Invalid Year.';
          erYrLstExp=*On;
          error=*On;
       Endif;

       // Validate phone#
       ##type = 'P';
       erHosPhn# = Check_Phone#(scHosPhn#:##type:mgHosPhn#);
       If erHosPhn# = *on;
          error = *on;
       Endif;

       // Released to Apportionment Effective Date must be Valid if entered.
       If outCltID = '129';
          mgApprDt=' ';
          erApprDt=*Off;
          If scApprDt<>*Zeros And dtCheck(scApprDt)=0;
             mgApprDt='Invalid Date.';
             erApprDt=*On;
             error=*On;
          Endif;

          // % Apportionment is required if Apportionment Effective Date is Ente
          mgPctAppr = ' ';
          erPctAppr = *Off;
          If scApprDt <> *Zeros and scPctAppr = 0;
             mgPctAppr = '% Apportionment is required if Apportionment +
                           Effective Date is Entered.';
             erPctAppr= *On;
             error=*On;
          EndIf;

          // Apportionment Effective Date is required when % Apportionment is En
          mgPctApEdt = ' ';
          erPctApEdt = *Off;
          If scApprDt = *Zeros and scPctAppr <> 0;
             mgPctApEdt = 'Apportionment Effective Date is required +
                           when % Apportionment is Entered.';
             erPctApEdt = *On;
             error=*On;
          EndIf;

       EndIf;
       // Validate MCO                                                      Ente

          mgMcoCode = *blanks;
          mgMcoName = *blanks;
          mgMcoFein = *blanks;
          erMcoCode= *Off;
          erMcoName= *Off;
          erMcoFein= *Off;

          Select;
            When scMcoCode = *blanks;
                 If (scMcoName > *blanks or scMcoFein > *Zeros);
                     mgMcoCode = 'Invalid MCO code.';
                     erMcoCode= *On;
                     error=*On;
                 Endif;

            When scMcoCode = '00';
                 scMcoName = *Blanks;
                 scMcoFein = *Zeros;

            When scMcoName = *Blanks and scMcoFein = *Zeros;
                 mgMcoName = 'Invalid MCO Name.';
                 erMcoName= *On;
                 mgMcoFein = 'Invalid MCO Fein.';
                 erMcoFein= *On;
                 error=*On;
       // Check if valid MCO FEIN entered (This will not probably execute
       // because the MCO fein is having search ahead).

            When scMcoFein <> *Zeros;
            position = %check(numbers:%editc(scMcoFein:'X'));
              If position > 0;
                 mgMcoFein = 'MCO Fein can only conatain numbers 0 to 9';
                 erMcoFein= *On;
                 error = *On;
              EndIf;
            Endsl;
       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheck2     E
       //===================================================================
       // ErrorCheck3 - Validate the Benefit Screen Fields
       //===================================================================
     P ErrorCheck3     B                   Export
     D ErrorCheck3     PI              n
     D claimRrnIn                     9  0
     D benefitPtrIn                    *
     D*** Beg Add *** 04/10/24 *********************************************
     D claimPtrIn                      *
     D claimInfo     e ds                  ExtName(wtClmDtl:rcdClmDt0)
     D                                     based(claimPtr)
     D strLostTime                         Like(CMltFl)
     D*** End Add *** 04/10/24 *********************************************
     D benefitInfo   e ds                  ExtName(wtClmDtl:rcdClmDt3)
     D                                     based(benefitPtr)
     D awwRrn          s              9  0 inz(0)
     D claimRrn        s              9  0
     D benefitPtr      s               *   inz
     D*** Beg Add *** 04/10/24 *********************************************
     D claimPtr        s               *   inz
     D*** End Add *** 04/10/24 *********************************************
     D error           s               n   Inz(*Off)
     D cancel          s               n   Inz(*Off)
     D inAddMode       s               n   Inz(*On)
     D inEdtMode       s               n   Inz(*Off)
      /Free

       claimRrn=claimRrnIn;
       benefitPtr=benefitPtrIn;
       //*** Beg Add *** 04/10/24 ******************************************
       If %Parms > 2;
          claimPtr=claimPtrIn;
       Endif;
       //*** End Add *** 04/10/24 ******************************************

       Chain claimRrn pcm0p;
       Chain (CMco#:CMfnd) pcx00;

       // Benefits Edits         =========================================

       // Full Salary can't be checked if lost time is not I
       mgFullSal='  ';
       erFullSal=*Off;
       If scFullSal='Y' and CMltFl<>'I' and CMltFl<>'M' ;
          mgFullSal='Full Salary can only be checked if lost time is Indemnity +
                                  or Medical.';
          erFullSal=*On;
          error=*On;
       Else;
          // Full Salary is Required If Full thru date is entered
          If scFullSal<>'Y' and scPaidDt<>0;
             mgFullSal='Full Salary must be checked.';
             erFullSal=*On;
             error=*On;
          EndIf;
       EndIf;

       // Amount Paid and Week/Days are mutually inclusive.
       mgAmtPaid='  ';
       mgWeekPaid='  ';
       mgDaysPaid='  ';
       erAmtPaid=*Off;
       erWeekPaid=*Off;
       erDaysPaid=*Off;
       If (scAmtPaid<>*Zeros And (scWeekPaid=0 And scDaysPaid=0)) Or
          (scAmtPaid=*Zeros And (scWeekPaid<>0 Or scDaysPaid<>0));
          mgAmtPaid='Amount Paid and Weeks/Days Paid are mutually inclusive.';
          erAmtPaid=*On;
          mgWeekPaid='Amount Paid and Weeks/Days Paid are mutually inclusive.';
          erWeekPaid=*On;
          mgDaysPaid='Amount Paid and Weeks/Days Paid are mutually inclusive.';
          erDaysPaid=*On;
          error=*On;
       Endif;

       // Date Last Worked must be Valid if entered.
       erLstWkDt=*Off;
       If scLstWkDt<>*Zeros And dtCheck(scLstWkDt)=0;
          mgLstWkDt='Invalid Date.';
          erLstWkDt=*On;
          error=*On;
       Else;
          dateValueIn=scLstWkDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgLstWkDt=errorMsg;
             erLstWkDt=*On;
             error=*On;
          Else;
             If ChkFutrDate(dateValueIn:errorMsg:errorMsgId)=*On;
                mgLstWkDt=errorMsg;
                erLstWkDt=*On;
                error=*On;
             EndIf;
          EndIf;
       Endif;

       // Disability Notice Date must be valid if entered.
       erDisNotDt=*Off;
       If scDisNotDt<>*Zeros And dtCheck(scDisNotDt)=0;
          mgDisNotDt='Invalid Date.';
          erDisNotDt=*On;
          error=*On;
       Endif;

       // Date of Disability Began must be Valid if entered.

       // Date of Disability must be Valid if entered.
       erDisDt=*Off;
       //*** Beg Add *** 01/30/24 *** PHW114844 **********************
       GetClientID(outCltID);
       //*** End Add *** 01/30/24 *** PHW114844 **********************
       If (scDisDt<>*Zeros And dtCheck(scDisDt)=0) Or
       //*** Beg Chg *** 01/30/24 *** PHW114844 **********************
       //***(CMjrSt='PA' And CMltFl='I' And scDisDt=0);
          (outCltID<>'146' And CMjrSt='PA' And CMltFl='I' And
          scDisDt=0);
       //*** End Chg *** 01/30/24 *** PHW114844 **********************
          mgDisDt='Invalid Date.';
          erDisDt=*On;
          error=*On;
       Else;
          dateValueIn=scDisDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgDisDt=errorMsg;
             erDisDt=*On;
             error=*On;
          Else;
             If ChkFutrDate(dateValueIn:errorMsg:errorMsgId)=*On;
                mgDisDt=errorMsg;
                erDisDt=*On;
                error=*On;
             EndIf;
          EndIf;
       Endif;

       // Date Notified of LT must be Valid if entered.
       erDtNotLt=*Off;
       If scDtNotLt<>*Zeros And dtCheck(scDtNotLt)=0;
          mgDtNotLt='Invalid Date.';
          erDtNotLt=*On;
          error=*On;
       Else;
          dateValueIn=scDtNotLt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgDtNotLt=errorMsg;
             erDtNotLt=*On;
             error=*On;
          EndIf;
       Endif;

       // Initial Date of LT must be Valid if entered.
       erInitDtLt=*Off;
       If scInitDtLt<>*Zeros And dtCheck(scInitDtLt)=0;
          mgInitDtLt='Invalid Date.';
          erInitDtLt=*On;
          error=*On;
       Endif;

       // Current Date Last Day Worked must be Valid if entered.

       // Suspension Effective date must contain valid date if Entered
       erSusEfDt=*Off;
       If scSusEfDt<>*Zeros And dtCheck(scSusEfDt)=0;
          mgSusEfDt='Invalid Date.';
          erSusEfDt=*On;
          error=*On;
       Endif;

       // Estimated RTW must be Valid if entered.
       erEstRtw=*Off;
       If scEstRtw<>*Zeros And dtCheck(scEstRtw)=0;
          mgEstRtw='Invalid Date.';
          erEstRtw=*On;
          error=*On;
       Else;
          dateValueIn=scEstRtw;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgEstRtw=errorMsg;
             erEstRtw=*On;
             error=*On;
          EndIf;
       Endif;

       // Released to Work Date must be Valid if entered.
       erRelWork=*Off;
       If scRelWork<>*Zeros And dtCheck(scRelWork)=0;
          mgRelWork='Invalid Date.';
          erRelWork=*On;
          error=*On;
       Else;
          dateValueIn=scRelWork;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgRelWork=errorMsg;
             erRelWork=*On;
             error=*On;
          EndIf;
       Endif;

       // Actual RTW must be Valid if entered.
       erActRtw=*Off;
       If scActRtw<>*Zeros And dtCheck(scActRtw)=0;
          mgActRtw='Invalid Date.';
          erActRtw=*On;
          error=*On;
       Else;
          dateValueIn=scActRtw;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgActRtw=errorMsg;
             erActRtw=*On;
             error=*On;
          EndIf;
       Endif;

       If CMltfl <> 'M';
       If CMjrst <> 'FL';
       If erActRtw = *Off and erDisDt = *Off and scActRTW > 0 and scDisDt = 0;
          mgActRtw = 'Date of Disability must be entered.';
          erActRtw = *On;
          error    = *On;
       EndIf;

       If erActRtw = *Off and erDisDt = *Off and scActRTW > 0 and
        dtCYMD(scActRTW) < dtCYMD(scDisDt);
          mgActRtw = 'Actual RTW must be greater than Date of Disability.';
          erActRtw = *On;
          error    = *On;
       EndIf;
       EndIf;

       Chain (CMco#:CMfnd:CMfyr:CMcase) pwh00;
       If %found and erActRtw = *Off and erDisDt = *Off and
        ((dtCYMD(scActRTW) > WHdate) or (WHdate > 0 and scActRTW = 0));
          mgActRtw = 'Date must be prior to first Work History Date.';
          erActRtw = *On;
          error    = *On;
       EndIf;
       Endif;

       // Paid thru Date must be Valid if entered.
       mgPaidDt=*Blanks;
       erPaidDt=*Off;
       If scPaidDt<>*Zeros;
          dateValueIn=scPaidDt;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgPaidDt=errorMsg;
             erPaidDt=*On;
             error=*On;
          EndIf;
       Endif;

       // If AWW is Zero and Claim is Indemnity then Pop-Up Window
       //*** Beg Chg *** 04/10/24 *****************************************
       //***If CMaww=0 And CMltFl='I' And error=*Off;
       If %Parms > 2;
          strLostTime = scLostTime;
       Else;
          strLostTime = CMltFl;
       Endif;
       If CMaww=0 And strLostTime = 'I' And error=*Off;
       //*** End Chg *** 04/10/24 *****************************************
          Chain keyWMFNL pcx00;
          If %found and (CXawwr = 'Y' or CXawwr = ' ');
          cancel=editAddAww(claimRrn:awwRrn:inAddMode:inEdtMode);
          If cancel=*On;
             error=*On;
          Endif;
          Endif;
          Chain claimRrn pcm0p;
       Endif;
       If CXlsrv = 'Y' And CMltfl = 'I' And scMthWage = ' ' and CXwmth = 'Y';
         mgMthWage = 'This field cannot be blanks';
         erMthWage= *On;
         error=*On;
       Else;
         mgMthWage = ' ';
         erMthWage= *Off;
       Endif;

       // Validate Concurrent employer1 phone#
       ##type = 'P';
       erConEmpP1 = Check_Phone#(scConEmpP1:##type:mgConEmpP1);
       If erConEmpP1 = *on;
          error = *on;
       Endif;
       // Validate Concurrent employer2 phone#
       ##type = 'P';
       erConEmpP2 = Check_Phone#(scConEmpP2:##type:mgConEmpP2);
       If erConEmpP2 = *on;
          error = *on;
       Endif;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheck3     E
       //===================================================================
       // ErrorCheck4 - Validate the Legal Information
       //===================================================================
     P ErrorCheck4     B                   Export
     D ErrorCheck4     PI              n
     D claimRrnIn                     9  0
     D legalPtrIn                      *
     D legalInfo     e ds                  ExtName(wtClmDtl:rcdClmDt4)
     D                                     based(legalPtr)
     D claimRrn        s              9  0
     D legalPtr        s               *   Inz
     D error           s               n   Inz(*Off)
      /Free

       claimRrn=claimRrnIn;
       legalPtr=legalPtrIn;

       Chain claimRrn pcm0p;

       // Fein/Mod must be entered if assigned date entered
       mgDefFein=' ';
       erDefFein=*Off;
       mgDefName=' ';
       erDefName=*Off;
       // Fein Must be entered if Assign date entered
       If scDefAttDt<>0 And scDefFein='000000000000' And scDefName=' ';
          mgDefFein='Must enter Fein/Mod or Name since assign date entered.';
          erDefFein=*On;
          mgDefName='Must enter Fein/Mod or Name since assign date entered.';
          erDefName=*On;
          error=*On;
       Endif;

       // Assign Date Must be valid if entered.
       mgDefAttDt=' ';
       erDefAttDt=*Off;
       If scDefAttDt<>*Zeros And dtCheck(scDefAttDt)=0;
          mgDefAttDt='Invalid Date.';
          erDefAttDt=*On;
          error=*On;
       Else;
          // Must be greater than accident date
          dateValueIn=scDefAttDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgDefAttDt=errorMsg;
             erDefAttDt=*On;
             error=*On;
          EndIf;
       Endif;

       // Fein/Mod must be entered if representative date entered
       mgPlaFein=' ';
       erPlaFein=*Off;
       mgPlaName=' ';
       erPlaName=*Off;
       If scPlaRepDt<>0 And scPlaFein='000000000000' And scPlaName=' ';
          mgPlaFein='Must enter Fein/Mod or Name since assign date entered.';
          erPlaFein=*On;
          mgPlaName='Must enter Fein/Mod or Name since assign date entered.';
          erPlaName=*On;
          error=*On;
       Endif;

       // Date of Representation Must be valid if entered.
       mgPlaRepDt=' ';
       erPlaRepDt=*Off;
       If scPlaRepDt<>*Zeros And dtCheck(scPlaRepDt)=0;
          mgPlaRepDt='Invalid Date';
          erPlaRepDt=*On;
          error=*On;
       Else;
          // Must be greater than accident date
          dateValueIn=scPlaRepDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgPlaRepDt=errorMsg;
             erPlaRepDt=*On;
             error=*On;
          EndIf;
       Endif;

       // Fein/Mod must be entered if defense phone number entered
       mgDefPhn#=' ';
       erDefPhn#=*Off;
       If scDefPhn#<>0 And scDefFein='000000000000' And scDefName=' ';
          mgDefPhn#='Must enter Fein/Mod or Name since Defense Attorney ' +
           'Phone Number has been entered.';
          erDefPhn#=*On;
          error=*On;
       Endif;

       // Fein/Mod must be entered if plaintif phone number entered
       mgPlaPhn#=' ';
       erPlaPhn#=*Off;
       If scPlaPhn#<>0 And scPlaFein='000000000000' And scPlaName=' ';
          mgPlaPhn#='Must enter Fein/Mod or Name since Plaintif Attorney ' +
           'Phone Number has been entered.';
          erPlaPhn#=*On;
          error=*On;
       Endif;

       // Settlement Date Must be valid if entered.
       mgSettDt=' ';
       erSettDt=*Off;
       If scSettDt<>*Zeros And dtCheck(scSettDt)=0;
          mgSettDt='Invalid Date';
          erSettDt=*On;
          error=*On;
       Else;
          // Must be greater than accident date
          dateValueIn=scSettDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgSettDt=errorMsg;
             erSettDt=*On;
             error=*On;
          EndIf;
       Endif;

       // Indemnity Settlement Mail Date Must be valid if entered.
       mgSetIndDt=' ';
       erSetIndDt=*Off;
       If scSetIndDt<>*Zeros And dtCheck(scSetIndDt)=0;
          mgSetIndDt='Invalid Date';
          erSetIndDt=*On;
          error=*On;
       Else;
          // Must be greater than accident date
          dateValueIn=scSetIndDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgSetIndDt=errorMsg;
             erSetIndDt=*On;
             error=*On;
          EndIf;
       Endif;

       // Medical Settlement Mail Date Must be valid if entered.
       mgSetMedDt=' ';
       erSetMedDt=*Off;
       If scSetMedDt<>*Zeros And dtCheck(scSetMedDt)=0;
          mgSetMedDt='Invalid Date';
          erSetMedDt=*On;
          error=*On;
       Else;
          // Must be greater than accident date
          dateValueIn=scSetMedDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgSetMedDt=errorMsg;
             erSetMedDt=*On;
             error=*On;
          EndIf;
       Endif;

       mgSuitDate=' ';
       erSuitDate=*Off;
       // Suit Date Must be valid if entered.
       If scSuitDate<>*Zeros And dtCheck(scSuitDate)=0;
          mgSuitDate='Invalid Date';
          erSuitDate=*On;
          error=*On;
       Else;
          // Must be greater than accident date
          dateValueIn=scSuitDate;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgSuitDate=errorMsg;
             erSuitDate=*On;
             error=*On;
          EndIf;
       Endif;

       mgDelayDt=' ';
       erDelayDt=*Off;
       // Delay Date Must be valid if entered.
       If scDelayDt<>*Zeros And dtCheck(scDelayDt)=0;
          mgDelayDt='Invalid Date';
          erDelayDt=*On;
          error=*On;
       Else;
          // Must be greater than accident date
          dateValueIn=scDelayDt;
          If ChkLtAccDate(dateValueIn:CMacDt:errorMsg:errorMsgId)=*On;
             mgDelayDt=errorMsg;
             erDelayDt=*On;
             error=*On;
          EndIf;
       Endif;

       // Validate defense attorney phone#
       ##type = 'P';
       If erDefPhn# = *off;
          erDefPhn# = Check_Phone#(scDefPhn#:##type:mgDefPhn#);
          If erDefPhn# = *on;
             error = *on;
           Endif;
        Endif;

       // Validate plaintiff attorney phone#
       ##type = 'P';
       If erPlaPhn# = *off;
          erPlaPhn# = Check_Phone#(scPlaPhn#:##type:mgPlaPhn#);
          If erPlaPhn# = *on;
             error = *on;
          Endif;
       Endif;

       // Validate defense attorney EmailId
       erDefemad = *Off;
       If %Len(%trim(scDefEmAd)) > 0;
          erDefemad = Check_Email(scDefEmAd:mgDefemad);
          If erDefemad = *On;
             error = *on;
          EndIf;
       EndIf;

       // Validate Plaintiff Attorney EmailId
       erPlaEmAd = *Off;
       If %Len(%trim(scPlaEmAd)) > 0;
          erPlaEmAd = Check_Email(scPlaEmAd:mgPlaEmAd);
          If erPlaEmAd = *On;
             error = *on;
          EndIf;
       EndIf;

       //*** Beg Add ***** 03/06/25 ****************************************
       mgLumpSum = '';
       erLumpSum = *Off;
       mgMSAAmt = '';
       erMSAAmt = *Off;
       mgMSAPrd = '';
       erMSAPrd = *Off;
       mgStruct = '';
       erStruct = *Off;
       mgInDepAmt = '';
       erInDepAmt = *Off;
       mgADepAmt = '';
       erADepAmt = *Off;

       If scStruct = 'Y';
         // Required fields when structured is checked
          If scMSAAmt = *Zeros and scSettAmt > 0;
             mgMSAAmt = 'Field required.';
             erMSAAmt = *On;
             error = *on;
          Endif;

          //*** Beg Del ***** 04/08/25 *********************
          //***If scMSAPrd = *Zeros and scMSAAmt > 0;
          //***   mgMSAPrd = 'Field required.';
          //***   erMSAPrd = *On;
          //***   error = *on;
          //***Endif;
          //*** End Del ***** 04/08/25 *********************

          If scInDepAmt = *Zeros;
             mgInDepAmt = 'Field required.';
             erInDepAmt = *On;
             error = *On;
          Endif;

          If scADepAmt = *Zeros;
             mgADepAmt = 'Field required.';
             erADepAmt = *On;
             error = *On;
          Endif;

       Endif;
       //*** Beg Add ***** 04/08/25 ************************
       If scStruct = 'Y' or scLumpSum = 'Y';
          If scMSAPrd = *Zeros and scMSAAmt > 0;
             mgMSAPrd = 'Field required.';
             erMSAPrd = *On;
             error = *on;
          Endif;
       Endif;
       //*** End Add ***** 04/08/25 ************************
       //*** End Add ***** 03/06/25 ****************************************

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheck4     E
       //===================================================================
       // ErrorCheck5 - Validate the State Info Fields
       //===================================================================
     P ErrorCheck5     B                   Export
     D ErrorCheck5     PI              n
     D claimRrnIn                     9  0
     d*** Beg Add *** 09/06/22 ************************************************
     D statePtrInA1                    *
     d*** End Add *** 09/06/22 ************************************************
     D statePtrIn                      *
     D stateIn                        2
     D error           s               n
     D claimRrn        s              9  0
     D statePtr        s               *   inz
     D state           s              2
       //*** Beg Add *** 09/06/22 *********************************************
     D statePtrA1      s               *   inz
     D froiState       s              2
       //*** End Add *** 09/06/22 *********************************************
      /Free

       claimRrn=claimRrnIn;
       //*** Beg Chg *** 09/06/22 *********************************************
       //***statePtr=statePtrInA1;
       statePtrA1=statePtrInA1;
       statePtr=statePtrIn;
       //*** End Chg *** 09/06/22 *********************************************
       state=StateIn;

       Chain claimRrn pcm0p;
       //*** Beg Add *** 09/06/22 *********************************************
       //***froiState = getFroiState(CMco#:CMjrSt);
       //*** End Add *** 09/06/22 *********************************************

       //*** Beg Chg *** 09/06/22 *********************************************
       //***Select;
       //***When state='A1';
       //***   error=ErrorCheckA1(statePtr);
       //***When state='AL';
       //***   error=ErrorCheckAL(statePtr);
       //***When state='CA';
       //***   error=ErrorCheckCA(statePtr);
       //***When state='DE';
       //***   error=ErrorCheckDE(statePtr);
       //***When state='FL';
       //***   error=ErrorCheckFL(statePtr);
       //***When state='GA';
       //***   error=ErrorCheckGA(statePtr);
       //***When state='LA';
       //***   error=ErrorCheckLA(statePtr);
       //***When state='MA';
       //***   error=ErrorCheckMA(statePtr);
       //***When state='NC';
       //***   error=ErrorCheckNC(statePtr);
       //***When state='NY';
       //***   error=ErrorCheckNY(statePtr);
       //***When state='PA';
       //***   error=ErrorCheckPA(statePtr);
       //***When state='WV';
       //***   error=ErrorCheckWV(statePtr);
       //***EndSl;
       error=ErrorCheckA1(statePtrA1:statePtr:state);
       //*** End Chg *** 09/06/22 *********************************************

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheck5     E
       //===================================================================
       // ErrorCheck6 - Validate the Diary Information
       //===================================================================
     P ErrorCheck6     B                   Export
     D ErrorCheck6     PI              n
     D claimRrnIn                     9  0
     D diaryPtrIn                      *
     D diaryInfo     e ds                  ExtName(wtClmDtl:rcdClmDt6)
     D                                     based(diaryPtr)
     D claimRrn        s              9  0
     D diaryPtr        s               *   inz
     D error           s               n
      /Free

       claimRrn=claimRrnIn;
       diaryPtr=diaryPtrIn;

       Chain claimRrn pcm0p;

       // clear error indicators
       erMedDate  = *Off;
       erMedRmCd  = *Off;
       erMedNote  = *Off;
       erCompDate = *Off;
       erCompRmCd = *Off;
       erCompNote = *Off;
       erFormDate = *Off;
       erFormRmCd = *Off;
       erFormNote = *Off;
       erSuprDate = *Off;
       erSuprRmCd = *Off;
       erSuprNote = *Off;
       erUnitDate = *Off;
       erUnitRmCd = *Off;
       erUnitNote = *Off;

       // Medical Date Must be Valid if entered.
       If scMedDate<>0 and dtCheck(scMedDate)=0;
          mgMedDate='Invalid Date';
          erMedDate=*On;
          error=*On;
       Else;
          scMedDate=dtCheck(scMedDate);
       EndIf;

       // Medical Remark Must be Valid if Date Entered.
       If scMedDate<>*Zeros And scMedRmCd=*Blanks;
          mgMedRmCd='Remark Code must be Entered.';
          erMedRmCd=*On;
          error=*On;
       EndIf;

       // Medical Notes Must be Valid if entered.
       If scMedDate<>*Zeros And scMedNote=*Blanks;
          mgMedNote='Notes must be Entered.';
          erMedNote=*On;
          error=*On;
       EndIf;

       // Compensation Date Must be Valid if entered.
       If scCompDate<>0 and dtCheck(scCompDate)=0;
          mgCompDate='Invalid Date';
          erCompDate=*On;
          error=*On;
       Else;
          scCompDate=dtCheck(scCompDate);
       EndIf;

       // Compensation Remark Must be Valid if Date Entered.
       If scCompDate<>*Zeros And scCompRmCd=*Blanks;
          mgCompRmCd='Remark Code must be Entered.';
          erCompRmCd=*On;
          error=*On;
       EndIf;

       // Compensation Notes Must be Valid if entered.
       If scCompDate<>*Zeros And scCompNote=*Blanks;
          mgCompNote='Notes must be Entered.';
          erCompNote=*On;
          error=*On;
       EndIf;

       // State Form Date Must be Valid if entered.
       If scFormDate<>0 and dtCheck(scFormDate)=0;
          mgFormDate='Invalid Date';
          erFormDate=*On;
          error=*On;
       Else;
          scFormDate=dtCheck(scFormDate);
       EndIf;

       // State Form Remark Must be Valid if Date Entered.
       If scFormDate<>*Zeros And scFormRmCd=*Blanks;
          mgFormRmCd='Remark Code must be Entered.';
          erFormRmCd=*On;
          error=*On;
       EndIf;

       // State Form Notes Must be Valid if entered.
       If scFormDate<>*Zeros And scFormNote=*Blanks;
          mgFormNote='Notes must be Entered.';
          erFormNote=*On;
          error=*On;
       EndIf;

       // Supervisor Date Must be Valid if entered.
       If scSuprDate<>0 and dtCheck(scSuprDate)=0;
          mgSuprDate='Invalid Date';
          erSuprDate=*On;
          error=*On;
       Else;
          scSuprDate=dtCheck(scSuprDate);
       EndIf;

       // Supervisor Remark Must be Valid if Date Entered.
       If scSuprDate<>*Zeros And scSuprRmCd=*Blanks;
          mgSuprRmCd='Remark Code must be Entered.';
          erSuprRmCd=*On;
          error=*On;
       EndIf;

       // Supervisor Notes Must be Valid if entered.
       If scSuprDate<>*Zeros And scSuprNote=*Blanks;
          mgSuprNote='Notes must be Entered.';
          erSuprNote=*On;
          error=*On;
       EndIf;

       // Unit Manager Date Must be Valid if entered.
       If scUnitDate<>0 and dtCheck(scUnitDate)=0;
          mgUnitDate='Invalid Date';
          erUnitDate=*On;
          error=*On;
       Else;
          scUnitDate=dtCheck(scUnitDate);
       EndIf;

       // Unit Manager Remark Must be Valid if Date Entered.
       If scUnitDate<>*Zeros And scUnitRmCd=*Blanks;
          mgUnitRmCd='Remark Code must be Entered.';
          erUnitRmCd=*On;
          error=*On;
       EndIf;

       // Unit Manager Notes Must be Valid if entered.
       If scUnitDate<>*Zeros And scUnitNote=*Blanks;
          mgUnitNote='Notes must be Entered.';
          erUnitNote=*On;
          error=*On;
       EndIf;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheck6     E
       //===================================================================
       // ErrorCheckA1 - Validate the State Info Fields - A1
       //===================================================================
     P ErrorCheckA1    B
     D ErrorCheckA1    PI              n
       //*** Beg Add *** 09/06/22 *********************************************
     D statePtrInA1                    *
       //*** End Add *** 09/06/22 *********************************************
     D statePtrIn                      *
       //*** Beg Add *** 09/06/22 *********************************************
     D FroiStateIn                    2
       //*** End Add *** 09/06/22 *********************************************
     D error           s               n

     D a1Info        e ds                  ExtName(wtClmDtl:rcdClmDtA1)
     D                                     based(statePtr)
     D statePtr        s               *   inz
       //*** Beg Add *** 09/06/22 *********************************************
     D intWitnPhn      s             10  0
     D errorState      s               n
       //*** End Add *** 09/06/22 *********************************************

       //*** Beg Chg *** 09/06/22 *********************************************
       //***statePtr=statePtrIn;
       statePtr=statePtrInA1;
       //*** End Chg *** 09/06/22 *********************************************

       // Validate Preparer phone#
       ##type = 'P';
       erPrepPhn = Check_Phone#(scPrepPhn:##type:mgPrepPhn);
       If erPrepPhn = *on;
          error = *on;
       Endif;
       //*** Beg Add *** 09/06/22 *********************************
       // Edits for shared state fields on A1 screen and state screens
       Select;
          // Alabama Specific Edit on A1 Screen
          When froiStateIn = 'AL';
            // AL State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckAL(statePtr);
          // California Specific Edit on A1 Screen
          When froiStateIn = 'CA';
            // CA State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckCA(statePtr);
          // Delaware Specific Edit on A1 Screen
          When froiStateIn = 'DE';
            // DE State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckDE(statePtr);
          // Florida Specific Edit on A1 Screen
          When froiStateIn = 'FL';
            // FL State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckFL(statePtr);
          // Georgia Specific Edit on A1 Screen
          When froiStateIn = 'GA';
            // GA State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckGA(statePtr);
          // Massachusetts York Specific Edit on A1 Screen
          When froiStateIn = 'MA';
            // Desciption Is Required
            valueIn=scEquip1A1;
            If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
               mgEqip1A1=errorMsg;
               erEqip1A1=*On;
               error=*On;
            Else;
               mgEqip1A1=*Blanks;
               erEqip1A1=*Off;
            Endif;

            // How Injury Occurred is Required
            valueIn=scEvents1;
            If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
               mgEvents1=errorMsg;
               erEvents1=*On;
               error=*On;
            Else;
               mgEvents1=*Blanks;
               erEvents1=*Off;
            Endif;

            // Preparer Name Is Required
            valueIn=scPreparer;
            If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
               mgPreparer=errorMsg;
               erPreparer=*On;
               error=*On;
            Else;
               mgPreparer=*Blanks;
               erPreparer=*Off;
            Endif;

            // Preparer Title Is Required
            valueIn=scPrepTitl;
            If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
               mgPrepTitl=errorMsg;
               erPrepTitl=*On;
               error=*On;
            Else;
               mgPrepTitl=*Blanks;
               erPrepTitl=*Off;
            Endif;

            // Prep Date Is Required
            mgPrepDt=*Blanks;
            erPrepDt=*Off;
            dateValueIn=scDtPrep;
            If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
               mgPrepDt=errorMsg;
               erPrepDt=*On;
               error=*On;
            EndIf;

            // MA State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckMA(statePtr);
          // North Carolina Specific Edit on A1 Screen
          When froiStateIn = 'NC';
            // NC State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckNC(statePtr);

          // New York Specific Edit on A1 Screen
          When froiStateIn = 'NY';

            // How Injury Occurred is Required
            valueIn=scEvents1;
            If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
               mgEvents1=errorMsg;
               erEvents1=*On;
               error=*On;
            Else;
               mgEvents1=*Blanks;
               erEvents1=*Off;
            Endif;

            // Preparer Name Is Required
            valueIn=scPreparer;
            If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
               mgPreparer=errorMsg;
               erPreparer=*On;
               error=*On;
            Else;
               mgPreparer=*Blanks;
               erPreparer=*Off;
            Endif;

            // Preparer Title Is Required
            valueIn=scPrepTitl;
            If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
               mgPrepTitl=errorMsg;
               erPrepTitl=*On;
               error=*On;
            Else;
               mgPrepTitl=*Blanks;
               erPrepTitl=*Off;
            Endif;

            // Prep Phone Is Required
            numValueIn=scPrepPhn;
            If ChkNotZero(numValueIn:errorMsg:errorMsgId)=*On;
               mgPrepPhn=errorMsg;
               erPrepPhn=*On;
               error=*On;
            Else;
               mgPrepPhn=*Blanks;
               erPrepPhn=*Off;
            Endif;

            // Validate Preparer phone#
            ##type = 'P';
            If erPrepPhn = *off;
               erPrepPhn = Check_Phone#(scPrepPhn:##type:mgPrepPhn);
               If erPrepPhn = *on;
                  error = *on;
               Endif;
            Endif;

            // NY State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckNY(statePtr);

          // Pennsylvania Specific Edit on A1 Screen
          When froiStateIn = 'PA';
            // Validate Preparer phone#
            ##type = 'P';
            If erPrepPhn = *off;
               erPrepPhn = Check_Phone#(scPrepPhn:##type:mgPrepPhn);
               If erPrepPhn = *on;
                  error = *on;
               Endif;
            Endif;

             // Validate Witness phone#
             ##type = 'P';
             Monitor;
             intWitnPhn = %Dec(scWitnA12:10:0);
             On-error;
                intWitnPhn = 0;
             Endmon;
             erWitnPhn = Check_Phone#(intWitnPhn:##type:mgWitnPhn);
             If erWitnPhn = *on;
                error = *on;
             Endif;

             // PA State specific screen edits
             statePtr = StatePtrIn;
             errorState = ErrorCheckPA(statePtr);
          // West Virginia specific Edit on A1 Screen
          When froiStateIn = 'WV';
            // WV State specific screen edits
            statePtr = StatePtrIn;
            errorState = ErrorCheckWV(statePtr);
       Endsl;
       //*** End Add *** 09/06/22 *********************************

       //*** Beg Chg *** 09/06/22 *********************************************
       //***If error=*on;
       If error=*on or errorState = *on;
       //*** End Chg *** 09/06/22 *********************************************
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckA1    E
       //===================================================================
       // ErrorCheckAL - Validate the State Info Fields - AL
       //===================================================================
     P ErrorCheckAL    B
     D ErrorCheckAL    PI              n
     D statePtrIn                      *
     D error           s               n

     D alInfo        e ds                  ExtName(wtClmDtl:rcdClmDtAL)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckAL    E
       //===================================================================
       // ErrorCheckCA - Validate the State Info Fields - CA
       //===================================================================
     P ErrorCheckCA    B
     D ErrorCheckCA    PI              n
     D statePtrIn                      *
     D error           s               n

     D caInfo        e ds                  ExtName(wtClmDtl:rcdClmDtCA)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;


       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckCA    E
       //===================================================================
       // ErrorCheckDE - Validate the State Info Fields - DE
       //===================================================================
     P ErrorCheckDE    B
     D ErrorCheckDE    PI              n
     D statePtrIn                      *
     D error           s               n

     D deInfo        e ds                  ExtName(wtClmDtl:rcdClmDtDE)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckDE    E
       //===================================================================
       // ErrorCheckFL - Validate the State Info Fields - FL
       //===================================================================
     P ErrorCheckFL    B
     D ErrorCheckFL    PI              n
     D statePtrIn                      *
     D error           s               n

     D flInfo        e ds                  ExtName(wtClmDtl:rcdClmDtFL)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       // Blanks value not allowed if Rate of Pay > 0.
       erPer=*Off;
       If scRtPay > 0 And scPer = ' ';
          erPer=*On;
          error=*On;
       EndIf;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckFL    E
       //===================================================================
       // ErrorCheckGA - Validate the State Info Fields - GA
       //===================================================================
     P ErrorCheckGA    B
     D ErrorCheckGA    PI              n
     D statePtrIn                      *
     D error           s               n

     D gaInfo        e ds                  ExtName(wtClmDtl:rcdClmDtGA)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       // Pay From Date Must Be valid If entered
       mgPayFrom=*Blanks;
       erPayFrom=*Off;
       If scPayFrom<>0;
          dateValueIn=scPayFrom;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgPayFrom=errorMsg;
             erPayFrom=*On;
             error=*On;
          EndIf;
       EndIf;

       // Until Date Must Be valid If entered
       mgUntlDate=*Blanks;
       erUntlDate=*Off;
       If scUntlDate<>0;
          dateValueIn=scUntlDate;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgUntlDate=errorMsg;
             erUntlDate=*On;
             error=*On;
          EndIf;
       EndIf;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckGA    E
       //===================================================================
       // ErrorCheckLA - Validate the State Info Fields - LA
       //===================================================================
     P ErrorCheckLA    B
     D ErrorCheckLA    PI              n
     D statePtrIn                      *
     D error           s               n

     D laInfo        e ds                  ExtName(wtClmDtl:rcdClmDtLA)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckLA    E
       //===================================================================
       // ErrorCheckMA - Validate the State Info Fields - MA
       //===================================================================
     P ErrorCheckMA    B
     D ErrorCheckMA    PI              n
     D statePtrIn                      *
     D error           s               n

     D maInfo        e ds                  ExtName(wtClmDtl:rcdClmDtMA)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       // 5th Day Date Must Be valid If entered
       mg5thDay=*Blanks;
       er5thDay=*Off;
       If sc5thDay<>0;
          dateValueIn=sc5thDay;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mg5thDay=errorMsg;
             er5thDay=*On;
             error=*On;
          EndIf;
       EndIf;

       // Date Reported Must Be valid If entered
       mgWorkRel=*Blanks;
       erWorkRel=*Off;
       If scWorkRel<>0;
          dateValueIn=scWorkRel;
          If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
             mgWorkRel=errorMsg;
             erWorkRel=*On;
             error=*On;
          EndIf;
       EndIf;

       // Desciption Is Required
       //*** Beg Del *** 09/06/22 *********************************
       //***valueIn=scInjOcc1;
       //***If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
       //***   mgInjOcc1=errorMsg;
       //***   erInjOcc1=*On;
       //***   error=*On;
       //***Else;
       //***   mgInjOcc1=*Blanks;
       //***   erInjOcc1=*Off;
       //***Endif;

       // Preparer Name Is Required
       //***valueIn=scPrepMA;
       //***If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
       //***   mgPrepMA=errorMsg;
       //***   erPrepMA=*On;
       //***   error=*On;
       //***Else;
       //***   mgPrepMA=*Blanks;
       //***   erPrepMA=*Off;
       //***Endif;

       // Preparer Title Is Required
       //***valueIn=scPrpTtlMA;
       //***If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
       //***   mgPrpTtlMA=errorMsg;
       //***   erPrpTtlMA=*On;
       //***   error=*On;
       //***Else;
       //***   mgPrpTtlMA=*Blanks;
       //***   erPrpTtlMA=*Off;
       //***Endif;

       // Prep Date Is Required
       //***mgPrepDtMA=*Blanks;
       //***erPrepDtMA=*Off;
       //***dateValueIn=scPrepDtMA;
       //***If ChkDate(dateValueIn:errorMsg:errorMsgId)=*On;
       //***   mgPrepDtMA=errorMsg;
       //***   erPrepDtMA=*On;
       //***   error=*On;
       //***EndIf;
       //*** End Del *** 09/06/22 *********************************

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckMA    E
       //===================================================================
       // ErrorCheckNC - Validate the State Info Fields - NC
       //===================================================================
     P ErrorCheckNC    B
     D ErrorCheckNC    PI              n
     D statePtrIn                      *
     D error           s               n

     D ncInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNC)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckNC    E
       //===================================================================
       // ErrorCheckNY - Validate the State Info Fields - NY
       //===================================================================
     P ErrorCheckNY    B
     D ErrorCheckNY    PI              n
     D statePtrIn                      *
     D error           s               n

     D nyInfo        e ds                  ExtName(wtClmDtl:rcdClmDtNY)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       //*** Beg Del *** 09/06/22 *********************************************
       // How did the accident occur is required
       //***mgHowOcc1=*Blanks;
       //***erHowOcc1=*Off;
       //***valueIn=scHowOcc1;
       //***If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
       //***   mgHowOcc1=errorMsg;
       //***   erHowOcc1=*On;
       //***   error=*On;
       //***Endif;

       // Preparer Name is required
       //***mgPrepNY=*Blanks;
       //***erPrepNY=*Off;
       //***valueIn=scPrepNY;
       //***If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
       //***   mgPrepNY=errorMsg;
       //***   erPrepNY=*On;
       //***   error=*On;
       //***Endif;

       // Preparer Title is required
       //***mgPrpTtlNY=*Blanks;
       //***erPrpTtlNY=*Off;
       //***valueIn=scPrpTtlNY;
       //***If ChkNotBlank(valueIn:errorMsg:errorMsgId)=*On;
       //***   mgPrpTtlNY=errorMsg;
       //***   erPrpTtlNY=*On;
       //***   error=*On;
       //***Endif;

       // Preparer Phone Is Required
       //***mgPrpPhnNY=*Blanks;
       //***erPrpPhnNY=*Off;
       //***numValueIn=scPrpPhnNY;
       //***If ChkNotZero(numValueIn:errorMsg:errorMsgId)=*On;
       //***   mgPrpPhnNY=errorMsg;
       //***   erPrpPhnNY=*On;
       //***   error=*On;
       //***Endif;

       // Validate Preparer phone#
       //***##type = 'P';
       //***If erPrpPhnNY = *off;
       //***   erPrpPhnNY = Check_Phone#(scPrpPhnNY:##type:mgPrpPhnNY);
       //***   If erPrpPhnNY = *on;
       //***      error = *on;
       //***   Endif;
       //***Endif;
       //*** End Del *** 09/06/22 *********************************************

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckNY    E
       //===================================================================
       // ErrorCheckPA - Validate the State Info Fields - PA
       //===================================================================
     P ErrorCheckPA    B
     D ErrorCheckPA    PI              n
     D statePtrIn                      *
     D error           s               n

     D paInfo        e ds                  ExtName(wtClmDtl:rcdClmDtPA)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       // Validate Contact phone#
       ##type = 'P';
       erContPhn# = Check_Phone#(scContPhn#:##type:mgContPhn#);
       If erContPhn# = *on;
          error = *on;
       Endif;

       // Validate phone# for person completing the form
       //*** Beg Del *** 09/06/22 ***********************************
       //***erPrpPhnPA = Check_Phone#(scPrpPhnPA:##type:mgPrpPhnPA);
       //***If erPrpPhnPA = *on;
       //***   error = *on;
       //***Endif;

       // Validate Witness phone#
       //***erWitnPhn = Check_Phone#(scWitnPhn:##type:mgWitnPhn);
       //***If erWitnPhn = *on;
       //***   error = *on;
       //***Endif;
       //*** End Del *** 09/06/22 ***********************************

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckPA    E
       //===================================================================
       // ErrorCheckWV - Validate the State Info Fields - WV
       //===================================================================
     P ErrorCheckWV    B
     D ErrorCheckWV    PI              n
     D statePtrIn                      *
     D error           s               n   Inz(*Off)

     D wvInfo        e ds                  ExtName(wtClmDtl:rcdClmDtWV)
     D                                     based(statePtr)
     D statePtr        s               *   inz
      /Free

       statePtr=statePtrIn;

       //Date of Report must be valid if entered
       erDt2=*Off;
       //*** Beg Chg *** 09/06/22 ******************************************
       //***If scDt2 <> 0 And dtCheck(scDt2)=0;
       If scDt2 <> 0;
          If dtCheck(scDt2) = 0;
       //*** End Chg *** 09/06/22 ******************************************
             erDt2=*On;
             error=*On;
       //*** Beg Add *** 09/06/22 ******************************************
          Endif;
       //*** End Add *** 09/06/22 ******************************************
       EndIf;

       If error=*on;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-Free
     P ErrorCheckWV    E
       //********* beg add ********* 12/01/14 ******************************
       //===================================================================
       // EdtRsvChgs - Validate the reserve code and reserve amount change
       //===================================================================
     p EdtRsvChgs      b                   export
     d EdtRsvChgs      pi
     d inRrnWMCMP                     9  0
     d inArrRsvCod                    2    dim(10)
     d inArrRsvChg                    9s 2 dim(10)
     d inArrRtnErr                    1    dim(45)

     d intCount        s              3  0 inz
     d intIndex        s              3  0 inz
     d intAmtLmt       s             11  2 inz
     d intRqPy         s              9  2 inz
     d intElouts       s             11  2

     d                 ds
     d  remRsvCuml             1     90  2 dim(10)

      /free

       clear inArrRtnErr;
       clear remRsvCuml;
       Chain inRrnWMCMP pcm0p;

       // Retrieve Claim Darts Record
       Chain (CMco#:CMfnd) pcx00;

       // Edit Reserve changes
       for intCount = 1 to %Elem(inArrRsvCod);

          // No need to process these edits if no code
          If inArrRsvCod(intCount) <> *blanks;

             // get reserve type
             kkkey = inArrRsvCod(intCount);
             Chain (CMco#:kkkey) prt00;

             // Remaining Reserves Cannot be Less Than 0
             intIndex = %lookup(inArrRsvCod(intCount):rsvCode);

             If CXrChg = 'Y' And intIndex <> 0;
                remRsvCuml(intCount) = inArrRsvChg(intCount) -
                 (rsvRemain(intIndex) + rsvPaid(intIndex));
             Else;
                remRsvCuml(intCount) = inArrRsvChg(intCount);
             Endif;

             // Comp Reserves can only be entered on an indemnity claim.
             If RTtype = 'C' and CMltFl <> 'I' and remRsvCuml(intCount) <> 0;
                inArrRtnErr(1) = '1';
             Endif;

             // Is Medical Excluded?
             If RtvMedExcl(CMco#:CMfnd:CMemp#:CMdiv:CMfYr) = 'Y' and
              RTtype = 'M' and remRsvCuml(intCount) <> 0;
                inArrRtnErr(2) = '1';
             Endif;

             Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel00;
             If %found(WDELL);
                intELouts = ELoutS + %Xfoot(remRsvCuml);
                If intELoutS >= 9999999.99;
                   inArrRtnErr(4) = '1';
                Endif;
             Endif;

             If (intIndex <> 0 and (rsvRemain(intIndex) + remRsvCuml(intCount))
              < 0) or (intIndex = 0 and remRsvCuml(intCount) < 0);
                inArrRtnErr(intCount + 4) = '1';
             Endif;

             // If rsv change is negative make sure remaining rsv
             // will be enough to cover any requests for payments
             If remRsvCuml(intCount) < 0;
                intRqPy = getRequestAmt(inArrRsvCod(intCount):inRrnWMCMP);

                If ((intIndex <> 0 and (intRqPy > (rsvRemain(intIndex) +
                 remRsvCuml(intCount)))) or (intIndex = 0 and
                 intRqPy > remRsvCuml(intCount))) and intRqPy <> *zero;
                   inArrRtnErr(intCount + 24) = '1';
                Endif;
             Endif;

             // Do not allow the user to exceed their reserve authority limit
             If remRsvCuml(intCount) > 0;
                intAmtLmt = 0;
                ClcRemRsvLim(CMco#:CMfnd:CMfyr:CMcase:inArrRsvCod(intCount):
                 intAmtLmt);

                // For cumulative edits consider previous screen amounts
                // in the total.
                If remRsvCuml(intCount)>intAmtLmt and getRsvLimType(q1user)='I';
                   inArrRtnErr(intCount + 14) = '1';
                Endif;
             Endif;
          Endif;
       Endfor;

      /end-free
     p EdtRsvChgs      e
