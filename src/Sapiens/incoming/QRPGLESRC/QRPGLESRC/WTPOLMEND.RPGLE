     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLMEND - Policy Mid-Term Endorsement
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 05/23/12                                         *
      *  Programmer: C:191                                            *
      *  Project   : WC2116                                           *
      *===============================================================*
      *  Revisions:                                                   *
     H*                                                               *
     H* 12/04/24 C:291 Support: 118102                                *
     H*                Revise index to the Cancellation Detail file   *
     H*                to extract current and renewal policy years.   *
     H*                                                               *
     H* 10/15/24 C:320 Project: WC2984                                *
     H*                Remove Edit for A/R Flag to be removed for     *
     H*                Delete Division.                               *
     H*                                                               *
     H* 08/28/24 C:291 Support: 117541 (AmWins)                       *
     H*                Fixed wdcxl10 Record lock issue.               *
     H*                                                               *
     H* 08/27/24 C:291 Support: 117511 (AmWins)                       *
     H*                Clear Pending/Processing flag in wdcxp for     *
     H*                Renewal Term.                                  *
     H*                                                               *
     H* 07/19/24 C:333 Support: 115927                                *
     H*                Pop-up Window Displayed on CX Policy for       *
     H*                Mid-term Add of Division.                      *
     H*
     H* 07/05/24 C:340 Support: 115866                                *
     H*                Reset Flag - Change Chain to ReadE Loop to     *
     H*                             reset all Flags for Div/Year.     *
     H*                                                               *
     H* 02/26/24 C:340 Support: WC2956                                *
     H*                Reset Flag - Cancellation/Reinstatement Process*
     H*                             Control                           *
     H*
     H* 08/03/23  C:340 Project: WC2670
     H*                 Remove call for Scriptura OutputWindow for
     H*                 Revised Bills
     H*
     H* 02/28/23 C:65  Support: 107413 (Internal)
     H*                Change claim security edit from old SWCPF field
     H*                to admin/darts control field.
     H*
     H* 10/21/22 C:291 Support: 103978
     H*                Increase Maximum choices on Division dropdown.
     H*                Recompile only.
     H*
     H* 01/25/22 C:136 Support: 98291
     H*                Correct logic for invoking edit to prevent
     H*                adding division to policy that is cx'd and
     H*                audited.
     H*
     H* 08/13/21 C:65  Project: WC2278
     H*                Add RRN to OutputWindow procedure to allow email
     H*                addresses to populate when called from single requests.
      *                                                               *
      * 05/19/21 C:318 Support 94493                                  *
      *                Invoke edit to prevent selection of Waiver of  *
      *                Subrog Change if waiver detail doesnt exist    *
      *                                                               *
      * 05/18/21 C:295 Support: 94788 (SUNZ)                          *
      *                Added Deletion of division edit if A/R batch   *
      *                exists.                                        *
      *                Also added new mesage W400076 in WMESGG MSGF   *
      *                                                               *
      * 02/02/21 C:136 Project: WC2757                                *
      *                Default description to waiver detail for       *
      *                blanket waivers.                               *
      *                Correct logic for state specific waivers.      *
      *                 Allow Add/Change/Delete                       *
      *                Prevent duplicate blanket waivers              *
      *                                                               *
      * 03/16/20 C:212 Project wc2447                                 *
      *                For confirmation pop up, change call to        *
      *                PopOneBtnAgain. No DSPF changes                *
      *                                                               *
      * 03/16/20 C:180 Project wc2700                                 *
      *                Allow non-renewal cancellation override        *
      *   !!!!!!!!!!!! Do not EBF this code without the rest !!!!!!!!!!!!!!!
      *   !!!!!!!!!!!! of the project. !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      *                                                               *
      * 02/12/20 C:136 Support: 80310                                 *
      *                Populate LDA with waiver form name always.     *
      *                                                               *
      * 01/23/20 C:136 Project: WC2187                                *
      *                Replace algorithm for locking policies. Lock   *
      *                policy for all premium bearing endorsements.   *
      *||||||| Include all objects of this project with any EBF ||||||||||||||
      *                                                               *
      *  01/20/20 C:298 Project: WC2703                               *
      *                 Added java script Onchange event on Sort      *
      *                 field.                                        *
      *                                                               *
      *  12/10/19 C:33  Support: 84805 (remove 86213 from 08/01/14)   *
      *                 Replaced the message id WC30397 with WC30317  *
      *                                                               *
      *  10/09/19 C:298 Project: WC2685                               *
      *                 Mid-Term Add of Division on Cancelled Policy. *
      *                                                               *
      *  05/31/19 C:291 Project: WC2584                               *
      *                 Pass WDELP rrn and QQMod2 = ' ' to State      *
      *                 specific midterm endorsement program.         *
      *                 Add State specific Waiver form to LDA.        *
      *                                                               *
      *  03/20/19 C:180 Support: 77956                                *
      *                 Display file changes only - Increase division *
      *                 dropdown choices to 1500.                     *
      *                                                               *
      *  10/24/18 C:267 Support: 50562                                *
      *                 Delete all records in workfile WXDA1P.        *
      *                 This is used for Division ADD - to pass new   *
      *                 division# to STLETTER for darts remarks.      *
      *                                                               *
      *  10/02/18 C:079 Support: 136/64264                            *
      *                 Reset error indicator when division changes   *
      *                                                               *
      *  09/24/18 C:267 Project: WC2584                               *
      *                 Add procedure isPenRec to check existance of  *
      *                 record in WMENP. Used along side State Specifi*
      *                 endorsement form.                             *
      *                                                               *
      *  09/19/18 C:291 Project: PHW72916                             *
      *                 Allow Deletion of division on a cancelled     *
      *                 Division.                                     *
      *                                                               *
      *  08/29/18 C:291 Project: PHW72916                             *
      *                 Allow Addition of division on mid-term        *
      *                 endorsement to the State reporting division   *
      *                 only.                                         *
      *                                                               *
      *  07/27/18 C:267 Project: WC2584                               *
      *                 Check State Specific Midterm Endorsement form *
      *                 table for possible specific state form number.*
      *                 Use the state form to get additional entry    *
      *                 program name from endorsement form table.     *
      *                                                               *
      *  06/06/18 C:TXO Project: PHW64264                             *
      *                 Allow Deletion of division on mid-term        *
      *                 endorsement if it passes the edit             *
      *                                                               *
      *  10/13/17 C:65  Project: 150WBU                               *
      *                 Do not enforce A/R summary edit for client    *
      *                 150 as they will not allow individual audits. *
      *                                                               *
      *  12/01/16 C:136 Project: SY371                                *
      *                 Implement Scriptura processing                *
      *                                                               *
      *  11/10/16 C:136 Project: SY371                                *
      *                 Use new endorsement extended description      *
      *                 field                                         *
      *                                                               *
      *  11/28/14 C:282 Support: WA089 87207                          *
      *                 Only the selected division record will be     *
      *                 read from employer detail file in programStart*
      *                 procedure instead of reading all division     *
      *                 record for an employer.                       *
      *                                                               *
      *  08/01/14 C:282 Support: WA099 86213                          *
      *                 Replaced the message id 'WC30317' with new    *
      *                 message id 'WC30397'.                         *
      *                                                               *
    *  03/10/14 C:290 Support: WA099 83258                          *
    *                 Added edits on midterm endorsement process so *
    *                 that no endorsements should be allowed to be  *
    *                 effective on/after the cancellation date,with *
    *                 the exception of Rescind Deletion of Division,*
    *                 which would be allowed to be effective on the *
    *                 cancellation date (not after) on a Deleted    *
    *                 Division. Added a new messageid WC40157 in    *
    *                 Message file(WMESGG).                         *
    *                                                               *
    *  10/22/13 C:136 Support: WA045 83208                          *
    *                 Ensure rrn of WDELP being processed is being  *
    *                 passed to midterm endorsement program to      *
    *                 ensure that reportable changes don't invoke   *
    *                 the 'Changes Not Reported' window.            *
    *                                                               *
      *  09/24/13 C:283 Support: WA146 83260                          *
      *                 Initialize the Array Data Structure AR before *
      *                 using.                                        *
    *                                                               *
    *  09/03/13 C:191 Redmine: 2478                                 *
    *                 Remove Year/Policy Effective Date             *
    *                 Range Field.                                  *
    *                                                               *
    *  06/12/13 C:291 Redmine: 2970                                 *
    *                 Turn next Button off before calling Mailing   *
    *                 Addresss Change Endorsement screen.           *
    *  05/09/13 C:279 Redmine: 2219 CSI                             *
    *                 Modified to display warning message if Policy *
    *                 is pending in cancellation batch.             *
    *                                                               *
    *  04/11/13 C:191 Redmine: 2711 SCS                             *
    *                 Change cancellation message to include non-   *
    *                 reportable change                             *
    * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
    * ------------------------------------------------------------- *
    * This module is entry module to program WTPOLMEND. In addition *
    * WPIMIDEND & WTPOLLMST modules are also bound to this program  *
    *===============================================================*

     fWTPOLMEND cf   e             workstn
     f                                     HANDLER('PROFOUNDUI(HANDLER)')

     fsdcmla    if   e           k disk    rename(pcm01:pcm0a)
     f                                     prefix(c3:2)
     fssylf     if   e           k disk    include(swc01)
     f                                     include(ssy01)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwdell     if   e           k disk    rename(pel01:pel00)
     fwdell1    if   e           k disk
     fwdell2    if   e           k disk    rename(pel01:pel02)
      *** beg add ***** 10/22/13 ***********************************************
     f                                     infds(el2fds)
      *** beg add ***** 10/22/13 ***********************************************
     fwmcmlc    if   e           k disk    rename(pcm01:pcm0c)
     fwmeal     if   e           k disk    rename(pea01:pea00)
     fwmemp     if   e             disk    rename(pem01:pem0p)
     fwmgcl     if   e           k disk    rename(pgc01:pgc00)
     fwmmul6    uf a e           k disk    rename(pmu01:pmu06)
     fwmmula    if   e           k disk    rename(pmu01:pmu0a)
     fwmrpl     if   e           k disk    rename(prp01:prp00)
     fwmshl1    if   e           k disk
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fwtmyl     if   e           k disk    rename(pmy01:pmy00)
     f                                     infds(MYfds)
     f****** Beg add ******* 07/27/18 **************************
     fwtm2l     if   e           k disk    rename(pm201:pm200)
     fwtenl     if   e           k disk    rename(pen01:pen0t)
     f                                     prefix(wt)
     f****** End add ******* 07/27/18 **************************
      *** beg del ***** 01/23/20 *******************************
     f***wxril1    if a e           k disk    rename(pri01:pri01)
      *** end del ***** 01/23/20 *******************************
     f****** beg add ******* 05/09/13 **************************
     fwxcnl     if   e           k disk    rename(pcn01:pcn00)
     f****** beg add ******* 05/09/13 **************************
     FWAEAL1    iF   E           K DISK
      *** beg add ***** 12/01/16 **************************************
     fwmenl3    if   e           k disk    rename(pen01:pen03)
      *** beg add ***** 10/24/18 ***********************************************
      *** beg add ***** 02/02/21 **************************************
     fwmwvl9    if   e           k disk    rename(pwv01:pwv09)
      *** beg add ***** 02/02/21 ***********************************************
     fwxda1p    uf a e             disk    rename(pda01:pda0p)
     fwxda1l    if   e           k disk
     f                                     infds(da1fds)
      ***** Beg Add ***** 10/09/19 *****************************
     fwdehl6    if   e           k disk    rename(peh01:peh06)
      ***** End Add ***** 10/09/19 *****************************
      ***** Beg Add ***** 05/19/21 *****************************
     fwmwvl     if   e           k disk
      ***** End Add ***** 05/19/21 *****************************
      ***** Beg Add ***** 05/18/21 *****************************
     fwdeal5    if   e           k disk    rename(pea01:pea05)
      ***** End Add ***** 05/18/21 *****************************
      ***** begin add *** 02/28/23 *****************************
     fwmexl     if   e           k disk    rename(pex01:pex00)
      ***** end add ***** 02/28/23 *****************************
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
     FWDCXL10   UF   E           K Disk
      *** End Add *** 02/26/24 *** WC2956 **********************************
      *** beg add ***** 10/24/18 ***********************************************
     d/copy sPrLetter
     d/copy sPrXML
      *** beg add ***** 01/23/20 ***************************
     d/copy sPrLock
     d ix              s              1  0
     d iy              s              1  0
     d lck             s             10    Dim(6) CtData Perrcd(1)
     d pgm             ds
     d  MYpgm1
     d  MYpgm2
     d  MYpgm3
     d  MYpgm4
     d  MYpgm5
     d  pgmArr                             Dim(5) Like(MYpgm1) Overlay(pgm)
      *** end add ***** 01/23/20 ***************************
     d isUseScript     s              1n   inz
     d processType     c                   const('Mid-Term Endorsement')
     d xxProcType      s            100
     d xxSubmit        s              1n
     d xxMass          s              1
      *** end add ***** 12/01/16 **************************************
     D*** Beg Add *** 08/03/23 *** WC2670 **************************
     d isUseOutWindow  S              1n   Inz
     D*** End Add *** 08/03/23 *** WC2670 **************************

     d/copy sPrDate
     d/copy sPrGenPop
     d/copy wPrAgent
     d/copy wPrCarrier
     d/copy wPrEmplr
     d/copy wPrGroup
     d/copy wPrMidEnd
     d/copy wPrProduce
      *** beg add ***** 02/02/21 **************************
      /Copy wPrEndorse
      *** end add ***** 02/02/21 **************************

     d*Entry
     d MidEndEntry     pr                  extpgm('WTPOLMEND')
     d  @@rrn                         9
     d  @@mode                        1
     d  @@next                       10
     d MidEndEntry     pi
     d  @@rrn                         9
     d  @@mode                        1
     d  @@next                       10

     d psds          esds                  extname(SZQ1P)

     d clientId        s              3
     d doLoad          s               n   inz(*On)
     d flgProcess      s               n   inz(*off)
     d flgType         s              1
     d intCO#          s                   like(emco#)
     d intFND          s                   like(emfnd)
     d intLVL          s              3  0
     d isError         s               n
     d isCancelled     s               n   inz
     d isCarr#Req      s               n   inz
     d strCONM         s             40
     d strFNAM         s             40
     d strOutQ         s             10
     d xxCancDate      s              7  0 inz
     d xxDEL           s              1    inz
     d xxEMrrn         s              9  0 inz
     D xxendt          s                   like(mydt)
     D xxeedt          s              8  0
     d xxMode          s              1    inz
     d xxMYrrn         s              9  0 inz
     d xxRrn           s              9  0 inz
     d xxRIrtcd        s               n   inz
     d stateRptDiv     s              3  0 inz
      *** Beg add ***** 07/27/18 **********************************
     d intMYSort       s                   like(MYSORT)
      *** End add ***** 07/27/18 **********************************
      ***** Beg Add ***** 10/09/19 ********************************
     d intStRptDiv     s              5  0 inz
     d intPolCxDt      s              7  0 inz
     d intDivFrstYr    s              3  0 inz
     d intPrYrExpDt    s              7  0 inz
      ***** End Add ***** 10/09/19 ********************************
      *** Begin Add *** 03/16/20 ****************************************
     d outMode         s              3    Inz('BIL')
      *** End   Add *** 03/16/20 ****************************************
      *
      *** beg add ***** 10/22/13 **********************************
     d el2fds          ds
     d  binEl2rrn            397    400b 0
     d intEl2rrn       s              9  0
      *** end add ***** 10/22/13 **********************************
      *
      *** beg add ***** 10/24/18 **********************************
     d da1fds          ds
     d  binda1rrn            397    400b 0
      *** end add ***** 10/24/18 **********************************
      *
     D MYfds           ds
     D  MYrrn                397    400b 0

     d SVLDA           DS          1024
     d  LDEND#                 1      5  0
     d  LDENDA                 1      5
     d  LDMYRR                 6     14  0
     d  LDEEDT                15     22  0
     d  LDFYR                 23     25  0
     d  LDPYRL                26     26  0
      *** Beg Add ***** 05/31/19 *****************
     d  ldStSpcfFlg           40     40
     d  ldWaiverFrm           41     55
      *** End Add ***** 05/31/19 *****************
     d*** beg add ***** 12/01/16 **********************************************
     d xxDart                         1    overlay(svlda:251)
     d xxRMCD                         5    overlay(svlda:252)
     d xxCopy                         1    overlay(svlda:257)
     d xxPrint                        1    overlay(svlda:258)
     d xxPrinter                     10    overlay(svlda:259)
     d xxTray                        50    overlay(svlda:269)
     d xx#CopiesA                     3    overlay(svlda:319)
     d xxEmail                        1    overlay(svlda:322)
     d xxToEmail                    200    overlay(svlda:323)
     d xxSubject                    150    overlay(svlda:523)
     d xxBody                       350    overlay(svlda:673)
     d*** end add ***** 12/01/16 **********************************************
      *** beg add ***** 02/02/21 **************************
     d chrAddPgm       s             10
     d chrPrtPgm       s             10
     d chrState        s              2    Inz('TX')
     d chrWvBlkPrjDs   s            500
     d chrWvrForm      s             15
      *** end add ***** 02/02/21 **************************
      *** Beg Add *** 02/26/24 *** WC2956 **********************************
     d strMsg          S             50A
      *** End Add *** 02/26/24 *** WC2956 **********************************

      *** Beg Chg ***** 07/19/24 *************************************
     d*conErrTtl       c                   const('*** Error ***')
     d conErrTtl       c                   const('Edit Message')
      *** End Chg ***** 07/19/24 *************************************
     d conInfTtl       c                   const('Information')
     d conCnf          c                   const('Confirmation')
     d conMsg          c                   const('Cancel date entered is invali-
     d                                     d. A claim exists that is either ope-
     d                                     n or closed with prior payments made-
     d                                      against the claim. The cancel date -
     d                                     is invalid since the claim has an -
     d                                     accident date that is greater than o-
     d                                     r equal to the cancel date. Override-
     d                                      error message?')
     d conMsg1         c                   const('An active DARTS Remark exists-
     d                                      for this employer. Override error m-
     d                                     essage?')
      *** Beg Chg ***** 07/19/24 *************************************
     d*conMsg2         c                   const('Employer is pending cancellat-
     d*                                    ion. Override error message?')
     d conMsg2         c                   const('This policy is currently unde-
     d                                     r cancellation. Do you wish to conti-
     d                                     nue?')
      *** End Chg ***** 07/19/24 *************************************
     d conMsg3         c                   const('An exit (or Cancel) has been -
     d                                     issued since attempting this endorse-
     d                                     ment. This endorsement will not be p-
     d                                     rinted or processed. Any data added -
     d                                     (or changed) will continue to exist.-
     d                                     Continue to process endorsement?')
     d conMsg4         c                   const('A division cannot be added at-
     d                                      this time. Employer master record i-
     d                                     n use by')
     d conMsgDone      c                   const('Endorsement print job submitt-
     d                                     ed')
     d*** Beg chg ****** 04/11/13 **********************************************
     d********* conMsgNtDn      c                   const('Endorsement print job
     d*************                                     celled or abended')
     d conMsgNtDn      c                   const('Endorsement has been cancelle-
     d                                     d.')
     d conMsgNoChg     c                   const('Endorsement changes made are -
     d                                     not reportable via WCPOLS. The data -
     d                                     change will take place, but no endor-
     d                                     sement form will be generated.')
     d*** End chg ****** 04/11/13 **********************************************
     d*** beg add ****** 05/09/13 **********************************************
     d conMsg5         c                   const('Employer has already been sel-
     d                                     ected for cancellation by Mass Cance-
     d                                     llation process. Override error mess-
     d                                     age?')
     d*** end add ****** 05/09/13 **********************************************

     d* Proto-types

     d* Get company information API
     d GetCompany      pr                  extpgm('WA001R')
     d   intCO#                            like(emco#)
     d   intFND                            like(emfnd)
     d   strCONM                     40
     d   strFNAM                     40
     d   intLVL                       3  0
     d   flgType                      1
     d   strOutQ                     10

     d* Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d* Program Start
     d ProgramStart    pr
     d ProcessMe                       n

     d* Main Processing
     d Main            pr
      *
     d* Process mid-term endorsement
     d*** Beg chg ****** 04/11/13 **********************************************
     d************ ProcessMidEnd   pr              n
     d ProcessMidEnd   pr             1
     d*** End chg ****** 04/11/13 **********************************************
      *
     d* Call Mid-end procs
     d CallMidEndProc  pr              n
     d  inPGM                        10
     d  inRsp                          n
      *
     d* Endorsement processing procedure
     d ProcMidEnd      pr              n   extproc(ptrProcMidEnd)
     d  inEMrrn                       9  0
     d  inELrrn                       9  0
     d  inMYrrn                       9  0
     d  inEEDT                        8  0
      *
     d ptrProcMidEnd   s               *   procptr

     d* Load Selection fields
     d LoadSelection   pr

     d* Load Detail fields
     d LoadDetail      pr

     d* Validate Endorsement entry
     d ValidEndOpt     pr              n

     c*
     c     *DTAARA       DEFINE    *LDA          SVLDA
      /free

       // Relative Record # for employer detail.
       If %parms >= 1;
         xxRrn = %int(@@rrn);
       Else;
         xxRrn = 0;
       Endif;

       // Program Mode.
       If %parms >= 2;
         xxMode = @@Mode;
       Else;
         xxMode = 'C';
       Endif;

       GetClientID(clientID);

       ProgramStart(flgProcess);

       If flgProcess;
         Main();
       Endif;


       *InLr = *On;
      /end-free

       //***********************************************************************
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     D ProgramStart    PI
     D ProcessMe                       n
      /free

       //*** Beg Add *** 08/03/23 *** WC2670 **************************
       In svLDA;
       Clear svLDA;
       Out svLDA;
       //*** End Add *** 08/03/23 *** WC2670 **************************

       // Chain to Employer Detail for Program.
       Chain xxRrn PEL0P;

       // Process only if valid employer detail record found.
       If not %found;
         processme = *Off;
         Return;
       Endif;

       xxEmRRN = GetEmpRRN(ELemp# :ELdiv :ELfnd :ELco#);
       scCo#  = ELco#;
       scFnd  = ELfnd;
       scEmp# = ELemp#;
       scDiv  = ELdiv;
       scSelDiv  = ELdiv;
       scFyr  = ELfyr;
       Chain xxEmRRN pem0p;

       GetCompany(intCO#:intFND:strCONM:strFNAM:intLVL:flgType:strOutq);

       xxCancDate = GetCxDate(scCo#:scFnd:scEmp#:scDiv);
       If xxCancDate > 0;
         isCancelled = *On;
       Else;
         isCancelled = *Off;
       Endif;

       isCarr#Req = *Off;
       Chain (scCo#:scFnd) pgc00;
       If %Found(wmgcl) And GCcrfl = 'Y';
          Setll (scCo#:scFnd:scfyr:scEmp#:scdiv) pel01;
         DoU %Eof(wdell1);
           Reade (scCo#:scFnd:scfyr:scEmp#:scdiv) pel01;
           If %Eof(wdell1);
             Leave;
           EndiF;
           Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) prp00;
           If Not %Found(wmrpl) Or RPcap# = *Blanks;
             isCarr#Req = *On;
             Leave;
           Endif;
         Enddo;
       Endif;

         Clear SCDivSel;
         SCDivSel = 'EMco# = ' + %Editc(ELco#:'X') + ' AND EMfnd = '
          + %Editc(ELfnd:'X') + ' AND EMemp# = ' + %Editc(ELemp#:'X');
         Clear SCMySel;
         SCMySel = 'MYco# = ' + %editc(ELco#:'X') + ' AND MYfnd = '
          + %editc(ELfnd:'X') + ' AND MYstat = ''A''';

       // Delete workfile WXDA1P - used to store the newly added division
         ExSr DltDA1P;

         isUseScript = UseScriptura(processType);

         ProcessMe = *on;
         Return;

       //==================================================================
       // DLTDA1P -  Delete WXDA1P records
       //==================================================================
          BegSR  DLTDA1P;
            SetLl *LOval pda01;
            Read pda01;
            Dow not %eof(WXDA1L);
                chain binda1rrn pda0p;
                If %found(WXDA1P);
                   delete pda0p;
                EndIf;
                Read pda01;
            EndDo;
          EndSr;
       //==================================================================
       //*** end add ***** 10/24/18 *************************************

      /end-free
     P ProgramStart    E

       //***********************************************************************
       //===================================================================
       // Main - Main processing
       //===================================================================
     P Main            B
     D Main            PI

     D flgPrint        s               n   inz
     D processStatus   s              1    inz
      /free

       //*  Process until exit button is pressed.

       DoU ExitNow = *On;

         If doLoad;
           //  Load detail panel
           LoadDetail();
         Endif;

         ExFmt rcdMidEnd;

       //* Menu Choices

         Select;
         When BtnCancel = *On;
           ExitNow = *On;
         When BtnNext = *On;
           If ValidEndOpt();
              BtnNext = *Off;
             //Process endorsement here
            processStatus = ProcessMidEnd();
            Select;
            When processStatus = '1';
               PopOneBtnAgain(conMsgDone:conInfTtl:'MidEndSubmit');
            When processStatus = '0';
               PopOneBtn(conMsgNtDn:conInfTtl);
            When processStatus = 'X';
               PopOneBtn(conMsgNoChg:conInfTtl);
            EndSl;
             ExitNow = *On;
           Endif;
         Other;
           Select;
           When scSelDiv <> scDiv;
            //Division selection changed from drop down
             isError = *Off;
             erMidEnd = *Off;
            //*** beg add ***** 02/02/21 ************************
             erBlkWvr = *Off;
            //*** end add ***** 02/02/21 ************************
             scDiv = scSelDiv;
             xxRRN = GetEmpYrRR(0 :scEmp# :scDiv :scFnd :scCo# :' ');
             Chain xxRrn PEL0P;
             scFyr = ELfyr;
             xxEmRRN = GetEmpRRN(scEmp# :scDiv :scFnd :scCo#);
             Chain xxEmRRN pem0p;
             doLoad = *On;
           Other;
             isError = Not ValidEndOpt();
           EndSl;
         EndSl;

       Enddo;

      /end-free
     P Main            E

       //===================================================================
       // Process mid-term endorsement
       //===================================================================
     p ProcessMidEnd   b
     d ProcessMidEnd   pi             1
      *
     d KKEND#          s                   like(MUEND#)
     d XXEND#          s                   like(MUEND#)
     d xxRtcd          s              1    inz('0')
     d xxRsp           s               n
     d xxPen           s               n
     d xxF312          s              1
     d strAltPgm       s             10a
     d xxJRSt          s              2a
     d strForm         s             10a   inz(*hival)
     d strWvForm       s             15a   inz(' ')
     D DELETD          C                   CONST('Endorsement not -
     D                                     completed')
      *
     C     ENKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    XXEND#
      *
     C     ELKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C     ELKYA         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXENDT
      *
     C     MUKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    ELFYR
     C                   KFLD                    SCEMP#
     C*
     C     MUKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    ELFYR
     C                   KFLD                    SCEMP#
     C                   KFLD                    KKEND#
     C*
B4   C     MYPGM1        IFNE      *BLANK
     C     MYPGM2        ORNE      *BLANK
     C     MYPGM3        ORNE      *BLANK
     C     MYPGM4        ORNE      *BLANK
     C     MYPGM5        ORNE      *BLANK
     C*
     C*  PROCESS FOR ALL FUND YEARS ENDORSEMENT DATE THRU CURRENT DATE
      *
     C     ELKYA         SETGT     PEL02
     C     ELKY          READPE    PEL02                                  60
B5   C     *IN60         DOWEQ     *OFF
      /free
       intEl2rrn = %int(binEl2Rrn);
      /end-free
      *
      * Allow an endorsement effective date equal to the policy
      * expiration date only if XXDEL = '1'. See prior comments
      * for XXDEL.
      *
B6   C     XXDEL         IFEQ      '0'
     C     XXENDT        ANDGE     ELPFDT
     C     XXENDT        ANDLT     ELPTDT
     C     XXDEL         OREQ      '1'
     C     XXENDT        ANDGE     ELPFDT
     C     XXENDT        ANDLE     ELPTDT
     C*
     C*  GET CURRENT ENDORSEMENT # FOR FYR BEING PROCESSED
     C*
     C                   MOVE      *HIVAL        KKEND#
     C     MUKY1         SETGT     PMU06
     C     MUKY          READPE(N) PMU06                                  60
B7   C     *IN60         IFEQ      *ON
     C                   Z-ADD     *ZERO         MUEND#
E7   C                   ENDIF
     C*
     C*  LOAD ENDORSEMENT NUMBER TO LOCAL DATA AREA POSITION 1 THRU 5
     C*
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     SVLDA         SVLDAO          250
     C     MUEND#        ADD       1             XXEND#
     C                   MOVE      XXEND#        LDEND#
     C                   MOVE      MYrrn         LDMYRR
     C                   MOVE      SCEEDT        LDEEDT
     C                   MOVE      ELFYR         LDFYR
     C                   OUT       *DTAARA
      *** beg del ***** 02/02/21 ***************************
     C**  Check state specific Midterm Endorsement form table WTM2L
      */free
       //strAltPgm = ' ';
       //xxJRSt = GetRatingState(scCo#:scFnd:scEmp#:scDiv:ELfyr);
       //
       //intMYSORT = %int(scMidEnd);
       //Chain (scCo#:scFnd:intMYSORT:xxJRSt) pm200;
       //If %Found(WTM2L);
       //   ReadPe (scCo#:scFnd) pen0t;
       //   Dow not %eof(WTENL);
       //      If %subst(wtENform:1:8) = %subst(M2form:1:8);
       //         If wtENedDt > xxEnDt;
       //            ReadPe (scCo#:scFnd) pen0t;
       //            iter;
       //         EndIf;
       //         strAltPgm = wtENpgm3;
       //         //*** Beg Add ***** 05/31/19 *******************
       //         If %subst(wtENForm:9:1) <> ' ';
       //            strWvForm = %subst(wtENForm:1:8)+ '.' +
       //             %subst(wtENForm:9:1);
       //         Else;
       //            strWvForm = wtENForm;
       //         Endif;
       //         //*** End Add ***** 05/31/19 *******************
       //         Leave;
       //      EndIf;
       //      ReadPe (scCo#:scFnd) pen0t;
       //   EndDo;
       //EndIf;
      */end-free
      *** end del ***** 02/02/21 ***************************
     C*
     C*  CREATE AN EMPLOYER ENDORSEMENT RECORD
     C*
     C                   MOVE      ELCO#         MUCO#
     C                   MOVE      ELFND         MUFND
     C                   MOVE      ELEMP#        MUEMP#
     C                   MOVE      ELDIV         MUDIV
     C                   MOVE      ELFYR         MUFYR
     C                   MOVE      XXEND#        MUEND#
     C                   MOVE      *ZERO         MUSEQ#
     C                   MOVE      *BLANKS       MUFORM
     C                   MOVEL     'MID-YEAR'    MUFORM
     C                   MOVEL     *BLANKS       MUENST
     C                   MOVEL     MYDESC        MUDESC
     C                   MOVEL     MYDESC        MUDSC2
     C                   MOVE      XXENDT        MUEEDT
     C                   MOVEL     'I'           MUEXFL
     C                   MOVEL     'M'           MUMNAU
     C                   MOVEL     'N'           MUPRNT
     C                   MOVEL     'A'           MUSTAT
     C                   MOVEL     Q1USER        MUUSER
     C                   MOVEL     Q1PGM         MUPGM
     C                   EVAL      MUTIME=TimeTo6(SystemTime())
     C                   EVAL      MUDT=DateTo7(SystemDate())
     C                   MOVEL     Q1USER        MUIUSR
     C                   MOVEL     Q1PGM         MUIPGM
     C                   EVAL      MUIDT = MUDT
      *** beg chg ***** 02/02/21 *******************************
     C***                   If        strAltPgm <> ' '
     C***                   MOVEL     strAltPgm     MUPPGM
     C***                   Else
     C***                   MOVEL     MYPGM1        MUPPGM
     C***                   EndIf
     C                   MOVEL     MYPGM1        MUPPGM
      *** end chg ***** 02/02/21 *******************************
     C*
     C                   WRITE     PMU06
     C                   Eval      xxRsp = *On

      *** beg chg ***** 02/02/21 *******************************
     C***                   If        strAltPgm <> ' '
     C***                   MOVE      ELCO#         QQCO#             3
     C***                   MOVE      ELFND         QQFND             3
     C***                   MOVE      ELEMP#        QQEMP#            9
     C***                   MOVE      ELDIV         QQDIV             5
     C***                   MOVE      ELFYR         QQFYR             3
     C***                   MOVE      '3'           QQmode            1
     C***                   MOVE      intEl2rrn     QQrrn             9 0
     C***                   MOVE      ' '           QQmod2            1
      ***/Free
       //****  in *Lock *Dtaara;
       //****  ldStSpcfFlg = 'Y';
       //****  ldWaiverFrm = strWvForm;
       //****  out *Dtaara;
      ***/End-Free
     C***                   CALL      strAltPgm
     C***                   PARM                    QQCO#             3
     C***                   PARM                    QQFND             3
     C***                   PARM                    QQEMP#            9
     C***                   PARM                    QQDIV             5
     C***                   PARM                    QQFYR             3
     C***                   PARM                    QQmode            1
     C***                   PARM                    QQrrn             9 0
     C***                   PARM                    QQmod2            1
      ***
     C***                   Eval      xxRsp = *On
     C***                   Else
      *** Beg Add ***** 02/12/20 *******************************
      ***/Free
       ////// // Put form in LDA for W0815R to filter waiver detail with
       //////  If myPgm1 = 'W0051R24'
       //////   Or myPgm1 = 'W0051R25'
       //////   Or myPgm1 = 'W0051R26';
       //////     In *Lock *Dtaara;
       //////     ldStSpcfFlg = ' ';
       //////     ldWaiverFrm = strWvForm;
       //////     Out *Dtaara;
       //////  Endif;
      ***/End-Free
     C***                   Eval      xxRsp = CallMidEndProc(MYpgm1:xxRsp)
     C***                   Eval      xxRsp = CallMidEndProc(MYpgm2:xxRsp)
     C***                   Eval      xxRsp = CallMidEndProc(MYpgm3:xxRsp)
     C***                   Eval      xxRsp = CallMidEndProc(MYpgm4:xxRsp)
     C***                   Eval      xxRsp = CallMidEndProc(MYpgm5:xxRsp)
      /Free
       xxRsp = CallMidEndProc(MYpgm1:xxRsp);
       xxRsp = CallMidEndProc(MYpgm2:xxRsp);
       xxRsp = CallMidEndProc(MYpgm3:xxRsp);
       xxRsp = CallMidEndProc(MYpgm4:xxRsp);
       xxRsp = CallMidEndProc(MYpgm5:xxRsp);
      /End-Free
      *** end chg ***** 02/02/21 *******************************
      *
     C                   If        xxRsp = *Off
     C* Cancel was pressed
     C                   EVAL      xxRtcd = '0'
     C                   Else
     C* Set default to no reportable change, this will be set to '1' if reportab
     C* in next section
     C                   EVAL      xxRtcd = 'X'
     C                   Endif
     C*  IF WMENP ENDORSEMENT REQUEST RECORDS WERE CREATED
     C*    THEN SUBMIT THE PRINT PORTION OF THE ENDORSEMENT PROCESS
     C                   Eval      xxPen = GetPenRec(xxEND#)
     C*
      *** beg del ***** 02/02/21 *******************************
     C**  For State Specific Endorsement programs, call this routine
     C**    instead to check WMENP record creation.
      */free
       //If strAltPgm <> ' ';
       //   xxPen = isPenRec(ELco#:ELfnd:ELemp#:ELdiv:ELfyr:q1jobn:XXEND#);
       //EndIf;
      */end-free
      *** end del ***** 02/02/21 *******************************
     C*
     C     xxPen         IFEQ      *ON
      *
     C                   MOVE      ELCO#         QQCO#             3
     C                   MOVE      ELFND         QQFND             3
     C                   MOVE      ELEMP#        QQEMP#            9
     C                   MOVE      ELDIV         QQDIV             5
     C                   MOVE      ELFYR         QQFYR             3
     C                   MOVE      Q1JOBN        QQJOBN            6
     C                   MOVE      XXEND#        QQEND#            5
     C                   MOVEL     strOutQ       QQOUTQ           10
     C*
     C*  DETERMINE IF F3=EXIT OR F12=CANCEL WAS PRESSED BEFORE
     C*  THE ENDORSEMENT COULD BE COMPLETED
     C*
     C     xxRsp         IFEQ      *Off
     C                   Eval      xxF312 = PopTwoBtn(conMsg3:conCnf:'Yes':'No')
     C     xxF312        IFEQ      *On
     C                   MOVE      *ON           xxRsp
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRSP         IFNE      *Off

      *** Beg Add *** 02/26/24 *** WC2956 **********************************
       // Clear Pending/Processing Flag.
       If MYpgm1 = 'W0051R20' Or MYpgm2 = 'W0051R20' Or MYpgm3 = 'W0051R20'
          Or MYpgm4 = 'W0051R20' Or MYpgm5 = 'W0051R20' Or
          MYpgm1 = 'W0051R40' Or MYpgm2 = 'W0051R40' Or MYpgm3 = 'W0051R40'
          Or MYpgm4 = 'W0051R40' Or MYpgm5 = 'W0051R40';

      *** Beg Chg *** 07/05/24 *** 115866 **********************************
          // Chain (scCo#:scFnd:scEmp#:scDiv:scFyr) WDCXL10;
          // If %Found;
          //    CXpdPr = ' ';
          //    Update Pcx01;
          // EndIf;
          //*** Beg Chg *** 12/04/24 *** 118102 ****************************
          //***SetLL (scCo#:scFnd:scEmp#:scDiv:scFyr) WDCXL10;
          //***ReadE (scCo#:scFnd:scEmp#:scDiv:scFyr) WDCXL10;
          SetLL (ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr) WDCXL10;
          ReadE (ELCo#:ELFnd:ELEmp#:ELDiv) WDCXL10;
          //*** End Chg *** 12/04/24 *** 118102 ****************************
          Dow Not %EOF;
             If CXpdPr <> ' ';
                CXpdPr = ' ';
                Update Pcx01;
             EndIF;
          //*** Beg Chg *** 12/04/24 *** 118102 ****************************
          //***ReadE (scCo#:scFnd:scEmp#:scDiv:scFyr) WDCXL10;
          ReadE (ELCo#:ELFnd:ELEmp#:ELDiv) WDCXL10;
          //*** End Chg *** 12/04/24 *** 118102 ****************************
          EndDo;
      *** End Chg *** 07/05/24 *** 115866 **********************************
          //*** Beg Add ***** 08/27/24 *************************************
          //Clear Pending/Processing Flag on Renewal years
          Setgt (scCo#:scFnd:scEmp#:scDiv:scFyr) WDCXL10;
          Reade (scCo#:scFnd:scEmp#:scDiv) WDCXL10;
          Dow Not %Eof;
             If CXpdPr <> ' ';
                CXpdPr = ' ';
                Update Pcx01;
             EndIF;
             Reade (scCo#:scFnd:scEmp#:scDiv) WDCXL10;
          EndDo;
          //*** End Add ***** 08/27/24 *************************************
       EndIf;
      *** End Add *** 02/26/24 *** WC2956 **********************************

      /Free
       If isUseScript;
       //*** Beg Add *** 08/03/23 *** WC2670 **************************
          isUseOutWindow = *on;
       Endif;
       Select;
       // Revised Bill
       When MYpgm1 = 'W0051R21' or MYpgm2 = 'W00051R21'
        or MYpgm3 = 'W0051R21' or MYpgm4 = 'W00051R21' or MYpgm5 = 'W00051R21';
          isUseOutWindow = *off;
       Endsl;
       If isUseOutWindow;
       //*** End Add *** 08/03/23 *** WC2670 **************************
          xxProcType = processType;
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:q1jobn:xxend#) pen03;
          In *Lock *dtaara;
          If ENrmcd <> *blanks;
             xxDart = 'Y';
             xxRmCd = ENrmcd;
          Else;
             xxDart = 'N';
             xxRmCd = *blanks;
          Endif;
          Out *dtaara;
          //*** begin chg ***  08/13/21  ***********************************
          //*****xxSubmit = OutputWindow(xxProcType:xxMass:'Y');
          If intEl2Rrn <> 0;
             xxSubmit = OutputWindow(xxProcType:xxMass:'Y':' ':intEl2Rrn);
          Else;
             xxSubmit = OutputWindow(xxProcType:xxMass:'Y');
          Endif;
          //*** end chg *****  08/13/21  ***********************************
       Endif;
      /End-free

     C                   CALL      'W7050C1'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQJOBN            6
     C                   PARM                    QQEND#            5
     C                   PARM                    QQOUTQ           10
     C                   PARM                    scSuppSch

     C                   EVAL      xxRtcd = *On
     C                   ELSE
     C                   EXSR      DLTRCD
      ** Call this so that WMENP records won't be left hanging
      ** if the midterm endorsement was cancelled.
     C                   CALL      'W7052R1'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQFYR
     C                   PARM                    QQJOBN
     C                   PARM                    QQEND#
      *
     C                   ENDIF
     C                   ELSE
     C                   EXSR      DLTRCD
     C                   ENDIF
     C*  REPLACE ORIGINAL VALUE OF *LDA
      *
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     SVLDAO        SVLDA
     C                   OUT       *DTAARA
     C                   LEAVE
E6   C                   ENDIF
      *
     C     ELKY          READPE    PEL02                                  60
E5   C                   ENDDO
     C*
E4   C                   ENDIF
     C*
     C*  RELEASE EMP WHEN ADDITION OF A DIVISION IS COMPLETED.
     C*
      /Free
       For ix = 1 to %Elem(pgmArr);
          iy = %LookUp(pgmArr(ix):lck);
          If iy > 0;
             ClearPolicyLock(scCo#:scFnd:scEmp#);
             Leave;
          Endif;
       Endfor;
      /End-Free
      *
     C                   Return    xxRtcd

     C*====================================================================
     C* DLTRCD - Delete WMMUP record
     C*====================================================================
     C     DLTRCD        BEGSR
      *
     C                   Z-ADD     XXEND#        KKEND#
     C     MUKY          SETGT     PMU06
     C     MUKY          READPE    PMU06                                  60
      *
     C     *IN60         IFEQ      *OFF
     C     MUEND#        ANDEQ     KKEND#
     C     MUKY1         DELETE    PMU06                              60
      *
     C                   ELSE
      *
     C     MUKY1         CHAIN     PMU06                              60
     C                   MOVEL(P)  DELETD        MUDESC
     C                   MOVEL(P)  DELETD        MUDSC2
     C                   MOVE      'X'           MUSTAT
     C  N60              UPDATE    PMU06
     C                   ENDIF
     C*
     C                   ENDSR
     p ProcessMidEnd   e
       //===================================================================
       // Call Mid-term endorsement procedures
       //===================================================================
     P CallMidEndProc  B
     D CallMidEndProc  pi              n
     D  inPgm                        10
     D  inRsp                          n
     D*
     D xxRtcd          s               n
      /free
        xxRtcd = inRsp;
        If inPgm <> *Blanks And xxRtcd = *On;
          ptrProcMidEnd = GetProcPtr(inPgm);
          If ptrProcMidEnd <> *null;
            xxMYrrn = MYrrn;
            If ProcMidEnd(xxEMrrn:intEl2rrn:xxMYrrn:scEEDT);
              xxRtcd = *On;
            Else;
              xxRtcd = *Off;
            Endif;
          Endif;
        Endif;
        Return xxRtcd;
      /end-free
     P CallMidEndProc  E
       //===================================================================
       // Load Policy Summary - Load the policy summary fields.
       //===================================================================
     P LoadDetail      B
     D LoadDetail      PI
     D*
     D  xxAgt#         s              5  0 inz
     D  xxFein         s              9  0 inz
     D  xxAHrrn        s              9  0 inz
     D  agentName      s             40    inz
     D  ProdName       s             50    inz
     D  agentMod       s              3  0 inz
     D  sysdt7         s              7  0 inz
     D  AHco#          s              3  0 inz
     D  AHfnd          s              3  0 inz
     D  AHagt#         s              5  0 inz
     D  AHagrc         s              5  0 inz
     D  AHprrc         s              5  0 inz
     D  AHprod         s              5  0 inz

      /free

       SCgroup = %Editc(scFnd:'X') + ' - ' + GetGrpName(scFnd);
       scEmpDBA = GetEmpFullName(scEmp#:scDiv:xxEMrrn:scFnd:scCo#:'DBA');
       scEmpFed = GetEmpFullName(scEmp#:scDiv:xxEMrrn:scFnd:scCo#:'FED');

       sysDt7 = DateTo7(SystemDate);
       xxAgt# = GetEmpAgt(scCO#:scFND:scEMP#:
                 scDIV:scFYR:sysDt7:agentName:xxAHrrn);
       If xxAgt# <> 0;
          scAgtName = %editc(xxAgt# : 'X') + ' - ' + agentName;
          GetAgtInfo1(xxAHrrn:AHco#:AHfnd:AHagt#:AHagrc:AHprrc:AHprod);
          xxFEIN = GetAgtFein(scCo#:scFnd:xxAgt#:agentMod);
          ProdName = GetProdNm(xxFEIN:agentMod:AHprod);
          scProdName = %editc(AHProd : 'X') + ' - ' + ProdName;
       Endif;


       scBillSts = GetBillsts(xxRrn);
       stateRptDiv = GetStRptDv(scCO#:scFND:scEMP#);

      /end-free
     P LoadDetail      E

       //===================================================================
       // Validate Mid-Term endorsement options
       //===================================================================
     P ValidEndOpt     b
     D ValidEndOpt     pi              n
     D*
     D flgOvr          s              1    inz
     D flgOv           s              1    inz
     D xxErr           s               n   inz
     D xxMsg           s            256    inz
     D xxTtl           s             50    inz
     D xxRsp           s               n   inz
     D xxDate          s              7  0 inz
     D xxacdt          s                   like(mydt)
     D xxFyrs          s                   like(elfyr)
     d strJobInfo      s             28
     d strProcess      s             10
     d strProcess25    s             25
     d strUser         s             50
       //*** beg add ***** 01/25/22 **************************
       Dcl-S isNotEligForOvr Ind;
       //*** end add ***** 01/25/22 **************************
      *
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
      *
     D                 DS
     D  AR                     1     90  2 INZ
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     C*
     C     CMKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXACDT
     C*
     C     CMKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C*
     C     ELKY          KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
      *
     C     ELKYA         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV
     C                   KFLD                    XXENDT
      *
     C     ELKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYRS
     C*
     C     SHKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    XXFYRS
     C                   KFLD                    KKTYPE            1

     C     keyWXCNL      KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCEMP#
     C                   KFLD                    SCDIV

      /free
       isError = *Off;
       erMidEnd = *Off;
       erEEDT = *Off;
       If scMidEnd = *blanks;
         mgMidEnd = 'WCR2222';
         erMidEnd = *On;
         isError = *On;
         Return not isError;
       Endif;

       intMYSORT = %int(scMidEnd);
       Chain (scCo#:scFnd:intMYSORT) pmy00;
       If not %Found(wtmyl);
         mgMidEnd = 'WCR2222';
         erMidEnd = *On;
         isError = *On;
         Return not isError;
       Endif;

      *** Beg Add *** 02/26/24 *** WC2956 **********************************
       // Can't run Deletion of Devisions.
       If MYpgm1 = 'W0051R20' Or MYpgm2 = 'W0051R20' Or MYpgm3 = 'W0051R20'
          Or MYpgm4 = 'W0051R20' Or MYpgm5 = 'W0051R20' Or
          MYpgm1 = 'W0051R40' Or MYpgm2 = 'W0051R40' Or MYpgm3 = 'W0051R40'
          Or MYpgm4 = 'W0051R40' Or MYpgm5 = 'W0051R40';

          SetLL (scCo#:scFnd:scEmp#) WDCXL10;
          //*** Beg Chg ***** 08/28/24 ************************
          //***ReadE (scCo#:scFnd:scEmp#) WDCXL10;
          Reade(n) (scCo#:scFnd:scEmp#) WDCXL10;
          //*** End Chg ***** 08/28/24 ************************
          Dow Not %EOF;
             If CXpdPr = 'Y';
                If MYpgm1 = 'W0051R20' Or MYpgm2 = 'W0051R20' Or
                   MYpgm3 = 'W0051R20' Or MYpgm4 = 'W0051R20' Or
                   MYpgm5 = 'W0051R20' ;
                   StrMsg = 'Deletion of Divisions cannot Proceed!';
                Else;
                   StrMsg = 'Recission of Divisions cannot Proceed!';
                EndIf;
                PopOneBtn('This action cannot be taken because there +
                 is a prior Cancellation or Reinstatement action that has not +
                 processed. If no jobs are running or waiting to run, Please +
                 contact support to resolve this situation.':StrMsg:'OK');
                Return not isError;
                Leave;
             EndIf;
             //*** Beg Chg ***** 08/28/24 ************************
             //***ReadE (scCo#:scFnd:scEmp#) WDCXL10;
             Reade(n) (scCo#:scFnd:scEmp#) WDCXL10;
             //*** End Chg ***** 08/28/24 ************************
          EndDo;
       Endif;

      *** End Add *** 02/26/24 *** WC2956 **********************************

       If isCancelled;
        // Can't run revised billing on cx'd policy.
         If MYpgm1 = 'W0051R21' Or MYpgm2 = 'W0051R21' Or MYpgm3 = 'W0051R21'
          Or MYpgm4 = 'W0051R21' Or MYpgm5 = 'W0051R21';
           mgMidEnd = 'WC30135';
           erMidEnd = *On;
           isError = *On;
         Endif;
       //*** beg chg ***** 01/25/22 ********************
       // ADD DIVISION CANNOT BE RUN ON CANCELLED POLICY
       //  If (MYpgm1 = 'W0051R19' Or MYpg3m2 = 'W0051R19' Or MYpgm3 = 'W0051R19
       //   Or MYpgm4 = 'W0051R19' Or MYpgm5 = 'W0051R19')
       //   and EMstrp <> 'Y';
       //     mgMidEnd = 'WC30317';
       //    erMidEnd = *On;
       //    isError = *On;
       //  Endif;
       //   If xxcancDate <> 0 And
       //    OvrRenewalCx(outMode:scCo#:scFnd:
       //    scEmp#:scDiv:scFyr:xxCancDate) = *On;
       //      erMidEnd = *Off;
       //      isError = *Off;
       //      xxCancDate = 0;
       //   Endif;
          Exsr ChkForAddDiv;
          If xxCancDate <> 0 And
           isNotEligForOvr = *Off And
           OvrRenewalCx(outMode:scCo#:scFnd:
           scEmp#:scDiv:scFyr:xxCancDate) = *On;
             erMidEnd = *Off;
             isError = *Off;
             xxCancDate = 0;
          Endif;
       //*** end chg ***** 01/25/22 ********************

       Else;
       //Allow "Rescind Division Deletion" for CX divisions only
         If MYpgm1 = 'W0051R40' Or MYpgm2 = 'W0051R40' Or MYpgm3 = 'W0051R40'
          Or MYpgm4 = 'W0051R40' Or MYpgm5 = 'W0051R40';
           mgMidEnd = 'WC32385';
           erMidEnd = *On;
           isError = *On;
         Endif;
       Endif;

       // Check if carrier # required.
       If isCarr#Req;
         If MYpgm1 = 'W0051R21' Or MYpgm2 = 'W0051R21' Or MYpgm3 = 'W0051R21'
          Or MYpgm4 = 'W0051R21' Or MYpgm5 = 'W0051R21';
           mgMidEnd = 'WC30318';
           erMidEnd = *On;
           isError = *On;
         Endif;
       Endif;

      *** Beg Del ***  10/15/24  ***  WC2984  ************************
      *** If ELarsm = 'Y' And EMstrp <> 'Y' and clientID <> '150';
      ***   If MYpgm1 = 'W0051R20' Or MYpgm2 = 'W0051R20' Or MYpgm3 = 'W0051R20'
      ***    Or MYpgm4 = 'W0051R20' Or MYpgm5 = 'W0051R20';
      ***    Setll (scCo#:scFnd:scEmp#:stateRptDiv:scFyr) pea00;
      ***    DoU %Eof(wmeal);
      ***      Reade (scCo#:scFnd:scEmp#:stateRptDiv:scFyr) pea00;
      ***      If %Eof(wmeal);
      ***        Leave;
      ***      Endif;
      ***      If EAarty = 'H' Or EAarty = 'F' Or EAarty = 'B' Or
      ***       EAarty = 'A' Or EAarty = 'D';
      ***        mgMidEnd = 'WC30335';
      ***        erMidEnd = *On;
      ***        isError = *On;
      ***      Endif;
      ***    Enddo;
      ***   Endif;
      *** Endif;
      *** End Del ***  10/15/24  ***  WC2984  ************************

      ***** Beg Add ***** 05/19/21 *****************************
      //Invoke edit to prevent selection of Waiver of Subrogate
      //Change if waiver detail doesnt exist
       If MYpgm1 = 'W0051R25' Or MYpgm2 = 'W0051R25' Or MYpgm3 = 'W0051R25'
        Or MYpgm4 = 'W0051R25' Or MYpgm5 = 'W0051R25';
          Chain (scCo#:scFnd:scFyr:scEmp#:scDiv) pwv01;
          If Not %Found(wmwvl);
             mgMidEnd = 'WCR1703';
             erMidEnd = *On;
             isError = *On;
          Endif;
       Endif;
      ***** End Add ***** 05/19/21 *****************************
       Exsr SlcTrd;

       Return not isError;
      /end-free
       //*** beg add ***** 01/25/22 ************************
       //---------------------------------------------------
       Begsr OvrPendingCx;
       //---------------------------------------------------
        If isNotEligForOvr = *On;
           LeaveSr;
        Endif;
        xxEnDt = dtCYMD(scEEDt);
        If xxCancDate > xxEnDt
         And xxCancDate <> *ZEROS;
           xxTtl = conErrttl;
           xxMsg = conMsg2;
           If PopTwoBtn(xxMsg:xxTtl:'Yes':'No');
              flgOvr = 'Y';
           Else;
              isError = *On;
           Endif;
        Endif;
       Endsr;
       //*** end add ***** 01/25/22 ************************
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C     EAKY1         KLIST
     C                   KFLD                    SCCO#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCEMP#
     C*
     C*** begin add ***  02/28/23  ************************************
           Chain(n) (scCo#:scFnd) pex00;
     C*** end add *****  02/28/23  ************************************
     C*  Check and see if policy page was issued for state reporting division
      *
     C*  VALIDATIONS FOR DELETION OF A DIVISION
     C*
B01  C     MYPGM1        IFEQ      'W0051R20'
     C     MYPGM2        OREQ      'W0051R20'
     C     MYPGM3        OREQ      'W0051R20'
     C     MYPGM4        OREQ      'W0051R20'
     C     MYPGM5        OREQ      'W0051R20'
      *
     C     EMSTRP        IFEQ      'Y'
     C                   EVAL      mgMidEnd = 'WCR3145'
     C                   EVAL      erMidEnd = *On
     C                   EVAL      isError = *On
     C                   ENDIF
     C*
     C*  EXECUTE THIS CODE ONLY IF DARTS IS INSTALLED
     C*
     C                   MOVEL     'SY'          XXSYK             2
     C*
     C     XXSYK         CHAIN     SSY01                              60
     C*
B04  C     *IN60         IFEQ      *OFF
     C     SYSCIF        ANDEQ     'Y'
     C*
     C*  CHECK FOR ACTIVE DARTS REMARKS
     C*    ESTABLISH KEY
     C                   CLEAR                   QCKEY
     C*
     C                   MOVE      SCCO#         QCCO#
     C                   MOVE      SCFND         QCFND
     C                   MOVE      SCEMP#        QCEMP#
     C                   MOVE      SCDIV         QCDIV
     C*
     C*  CHAIN TO DARTS REMARKS DETAIL FILE
     C*
     C     QCKEY         CHAIN     PCM0A                              60
     C*
B05  C     *IN60         DOWEQ     *OFF
     C*
B06  C     C3DIFL        IFEQ      'Y'
     C     C3STAT        ANDNE     'I'
     C*
     C                   MOVEL     'N'           flgov
     C*
     C                   EVAL      xxTtl = conErrTtl
     C                   EVAL      xxMsg = conMsg1
     C                   EVAL      xxRsp = PopThreeBtn(xxMsg:xxTtl)
     C*
     C                   IF        xxRsp = *On
     C                   EVAL      flgov = 'Y'
     C                   ENDIF
B07  C     flgov         IFNE      'Y'
     C                   EVAL      mgMidEnd = 'WC30011'
     C                   EVAL      erMidEnd = *On
     C                   EVAL      isError = *On
E07  C                   ENDIF
     C*
     C                   LEAVE
     C*
E06  C                   ENDIF
     C*
     C     QCKEY         READE     PCM0A                                  60
     C*
E05  C                   ENDDO
     C*
E04  C                   ENDIF
     C*
      //****** 05/18/21 ********** Beg Add *************************************
      /Free
       // Check if A/R transfer is processed for Employer.
           Setll (scCo#:scFnd:scEmp#:scDiv) pea05;
           Reade (scCo#:scFnd:scEmp#:scDiv) pea05;
           Dow not %eof(wdeal5);
             If Eastat <> 'T';
                mgMidEnd = 'W400076';
                erMidEnd = *On;
                isError = *On;
             Endif;
           Reade (scCo#:scFnd:scEmp#:scDiv) pea05;
           Enddo;
      /end-free
      //****** 05/18/21 ********** End Add *************************************
E01  C                   ENDIF
     C*
     C*  ENDORSEMENT DATE MUST BE ENTERED
     C*
B3   C     SCEEDT        IFEQ      *ZERO
     C                   EVAL      mgEEDT = 'WCR0500'
     C                   EVAL      erEEDT = *On
     C                   EVAL      isError = *On
     C                   LEAVESR
X3   C                   ENDIF
     C*
     C*  ENDORSEMENT DATE MUST BE VALID
     C*
     C                   EVAL      xxEEDT = dtCheck(scEEDT)
B4   C                   IF        xxEEDT = 0
     C                   EVAL      mgEEDT = 'WCR0500'
     C                   EVAL      erEEDT = *On
     C                   EVAL      isError = *On
     C                   ELSE
     C                   EVAL      scEEDT = xxEEDT
E4   C                   ENDIF
     C*
     C*  ENDORSEMENT DATE MUST BE WITHIN POLICY DATES
     C*
     C* Allow an endorsement effective date equal to the policy
     C* expiration date if processing a Deletion of Division
     C*
     C                   MOVE      '0'           *IN92
     C                   MOVE      '0'           XXDEL
      *
      *  The following endorsemets sets XXDEL = to '1'
      *     W0051R20 is Deletion of Division
      *     W0051R40 is Recission of Deletion of Division
      *     W0051R65 is Deletion of Location/Entity
      *
     C     MYPGM1        IFEQ      'W0051R20'
     C     MYPGM2        OREQ      'W0051R20'
     C     MYPGM3        OREQ      'W0051R20'
     C     MYPGM4        OREQ      'W0051R20'
     C     MYPGM5        OREQ      'W0051R20'
     C*
     C     MYPGM1        OREQ      'W0051R65'
     C     MYPGM2        OREQ      'W0051R65'
     C     MYPGM3        OREQ      'W0051R65'
     C     MYPGM4        OREQ      'W0051R65'
     C     MYPGM5        OREQ      'W0051R65'
     C*
     C     MYPGM1        OREQ      'W0051R40'
     C     MYPGM2        OREQ      'W0051R40'
     C     MYPGM3        OREQ      'W0051R40'
     C     MYPGM4        OREQ      'W0051R40'
     C     MYPGM5        OREQ      'W0051R40'
     C                   MOVE      '1'           *IN92
     C                   MOVE      '1'           XXDEL
     C                   ENDIF
      *
     C* Allow an endorsement effective date equal to the policy
     C* expiration date if xxdel = '1' only
      *
     C                   EVAL      xxENDT = dtCYMD(scEEDT)
     C                   EVAL      xxErr = *On
      *
     C     ELKYA         SETGT     PEL02
     C     ELKY          READPE    PEL02                                  60
     C     *IN60         DOWEQ     '0'
     C     XXDEL         IFEQ      '0'
     C     XXENDT        ANDGE     ELPFDT
     C     XXENDT        ANDLT     ELPTDT
     C     XXDEL         OREQ      '1'
     C     XXENDT        ANDGE     ELPFDT
     C     XXENDT        ANDLE     ELPTDT
     C                   Z-ADD     ELFYR         XXFYRS
     C                   EVAL      xxErr = *Off
     C                   LEAVE
     C                   ENDIF
     C     ELKY          READPE    PEL02                                  60
     C                   ENDDO
     C                   IF        xxErr = *On
     C                   EVAL      mgEEDT = 'WCR0930'
     C                   EVAL      erEEDT = *On
     C                   EVAL      isError = *On
     C                   ENDIF
     C*
     C* Only perform policy page edit if WCPOLS is installed
     C*
     C                   MOVEL     'WC'          XXSYK
     C     XXSYK         CHAIN     SWC01                              60
     C     WCFL03        IFEQ      'Y'
      ** Check policy year of endorsement effective date for the
      ** submission of a policy page.
     C     isError       IFEQ      *OFF
      *
     C                   MOVE      'P'           KKTYPE
     C     SHKY1         CHAIN     PSH01                              60
     C     *IN60         IFEQ      *OFF
     C     SHSTAT        ANDGT     '2'
     C                   ELSE
     C                   EVAL      mgEEDT = 'WCR3148'
     C                   EVAL      erEEDT = *On
     C                   EVAL      isError = *On
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C     MYPGM1        IFEQ      'W0051R21'
     C     MYPGM2        OREQ      'W0051R21'
     C     MYPGM3        OREQ      'W0051R21'
     C     MYPGM4        OREQ      'W0051R21'
     C     MYPGM5        OREQ      'W0051R21'
     C*  DO NOT ALLOW MIDTERM REVISED BILLING CHANGE ON AN
     C*  AUDITED ACCOUNT
     C     ELKY2         CHAIN     PEL00                              60
     C     *IN60         IFEQ      '0'
     C     ELFAF         IFEQ      'A'
     C                   EVAL      mgEEDT = 'WCR1110'
     C                   EVAL      erEEDT = *On
     C                   EVAL      isError = *On
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*  If processing Deletion of Division and there are any claims
     C*  that exist with an accident date greater than or equal to the
     C*  endorsement date, flag an error message due to backposting
     C*  cancelation date.  Allow
     C*
     C     MYPGM1        IFEQ      'W0051R20'
     C     MYPGM2        OREQ      'W0051R20'
     C     MYPGM3        OREQ      'W0051R20'
     C     MYPGM4        OREQ      'W0051R20'
     C     MYPGM5        OREQ      'W0051R20'
     C*
     C                   Z-ADD     XXENDT        XXACDT
     C*
     C     CMKY          SETLL     PCM0C
     C     CMKY1         READE     PCM0C                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     OVRFLG        ANDNE     'Y'
     C*
     C                   XFOOT     AR            XXPMTS           13 2
     C*
     C     CMSTAT        IFNE      'C'
     C     CMSTAT        OREQ      'C'
     C     XXPMTS        ANDNE     *ZEROS
     C*
     C*  Allow override on error msg only if user has proper authority
     C*
     C     Q1USER        CHAIN     PUP00                              60
     C*** begin chg ***  02/28/23  ************************************
     C*****UPLVL         IFGE      WCSL03
     C     UPLVL         IFGE      EXOVRL
     C*** end chg *****  02/28/23  ************************************
     C*
     C                   MOVE      'N'           flgOvr
     C                   EVAL      xxTtl = conErrttl
     C                   EVAL      xxMsg = conMsg
     C                   IF        PopTwoBtn(xxMsg:xxTtl:'Yes':'No')
     C                   EVAL      flgOvr = 'Y'
     C                   ENDIF
     C*
     C     flgOvr        IFEQ      'Y'
     C*
     C                   MOVE      'Y'           OVRFLG            1
     C*
     C                   ELSE
     C*
     C                   MOVE      'N'           OVRFLG
     C                   EVAL      mgEEDT = 'WCR0623'
     C                   EVAL      erEEDT = *On
     C                   EVAL      isError = *On
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   MOVE      'N'           OVRFLG
     C                   EVAL      mgEEDT = 'WCR0623'
     C                   EVAL      erEEDT = *On
     C                   EVAL      isError = *On
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CMKY1         READE     PCM0C                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDIF
      *
      * Do not allow endorsement to be issued after the cancellation
      * date. If the Employer is pending cancellation, display a
      * window with a warning message that the account is pending cancellation
      *
      *** beg chg ***** 01/25/22 **************************
     C***                   EVAL      xxENDT = dtCYMD(scEEDT)
     C***     xxCancDate    IFGT      XXENDT
     C***     xxCancDate    ANDNE     *ZEROS
     C***                   EVAL      xxTtl = conErrttl
     C***                   EVAL      xxMsg = conMsg2
     C***                   IF        PopTwoBtn(xxMsg:xxTtl:'Yes':'No')
     C***                   EVAL      flgOvr = 'Y'
     C***                   ELSE
     C***                   EVAL      isError = *On
     C***                   ENDIF
     C***                   ENDIF
       Exsr OvrPendingCx;
      *** end chg ***** 01/25/22 **************************
      *
      /free
       If xxENDT >= xxCancDate AND xxCancDate <> *zeros;
       If (xxENDT = xxCancDate);
          //***  Allow only "Rescind Division Deletion" on cancellation date
          If (Mypgm1 = 'W0051R40' OR MYpgm2 = 'W0051R40' OR
             MYpgm3 = 'W0051R40' Or MYpgm4 = 'W0051R40' OR
             MYpgm5 = 'W0051R40');

             // Pgm will execute normally...

          Else;
             mgEEDT = 'WC40157';
             erEEDT = *On;
             isError = *On;
          Endif;
       Else;
          mgEEDT = 'WC31388';
          erEEDT = *On;
          isError = *On;
       Endif;
      /end-free
     C                   ENDIF
      *
       //*** beg del ***** 01/25/22 *********************
      */Free
       ////WC2685 - Prevent adding of division if any year being added to the
       //// division is cancelled midterm and audited on the state reporting div
       //
       //If not(isError);
       //   intStRptDiv = GetStRptDv(scCo#:scFnd:scEmp#);
       //   intPolCxDt = GetCxDate(scCo#:scFnd:scEmp#:intStRptDiv);
       //   If intPolCxDt <> 0;
       //      intDivFrstYr = RtvPolYr(scCo#:scFnd:scEmp#:intStRptDiv:xxENDT);
       ////Find last audit record for each year in state reporting division
       //      Setll (scCo#:scFnd:scEmp#:intStRptDiv:intDivFrstYr) peh06;
       //      Dow not(%eof(wdehl6));
       //         Reade (scCo#:scFnd:scEmp#:intStRptDiv) peh06;
       //         intPrYrExpDt = 0;
       //         Setgt (EHco#:EHfnd:EHemp#:EHdiv:EHfyr:*Hival) peh06;
       //         Readpe (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) peh06;
       //         If EHfaf = 'A';
       //            intPrYrExpDt = EHptdt;
       //         Endif;
       //         Setgt (EHco#:EHfnd:EHemp#:EHdiv:EHfyr) peh06;
       //         Reade (EHco#:EHfnd:EHemp#:EHdiv) peh06;
       //         If not(%eof) and intPrYrExpDt <> 0 and
       //            intPrYrExpDt <> EHpfdt;
       //            mgEEDT = 'WC40172';
       //            erEEDT = *On;
       //            isError = *On;
       //            Leave;
       //         Endif;
       //      Enddo;
       //   Endif;
       //Endif;
      */End-free
       //*** end del ***** 01/25/22 *********************

      /Free
       If isError = *Off;
          For ix = 1 to %Elem(pgmArr);
             iy = %LookUp(pgmArr(ix):lck);
             If iy > 0;
                Exsr ChkRIU;
                Leave;
             Endif;
          Endfor;
       Endif;
      /End-Free
     C*
     C     MYPGM1        IFEQ      'W0051R21'
     C     MYPGM2        OREQ      'W0051R21'
     C     MYPGM3        OREQ      'W0051R21'
     C     MYPGM4        OREQ      'W0051R21'
     C     MYPGM5        OREQ      'W0051R21'
     C
     C     keyWXCNL      CHAIN     PCN00
     C                   IF        %found(WXCNL)
     C                   MOVE      'N'           flgOvr
     C                   EVAL      xxTtl = conErrttl
     C                   EVAL      xxMsg = conMsg5
     C                   IF        PopTwoBtn(xxMsg:xxTtl:'Yes':'No')
     C                   EVAL      flgOvr = 'Y'
     C                   ENDIF
     C*
     C     flgOvr        IFEQ      'Y'
     C                   MOVE      'Y'           OVRFLG            1
     C                   ELSE
     C                   MOVE      'N'           OVRFLG
     C                   EVAL      mgMidEnd = 'WCR1143'
     C                   EVAL      erMidEnd = *On
     C                   EVAL      isError = *On
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDSR
      /Free
       //*** beg add ***** 01/25/22 ***********************
       //---------------------------------------------
       Begsr ChkForAddDiv;
       //---------------------------------------------
       // Don't allow if policy year is cx'd and audit
       // update bill has been run
        If isError;
           LeaveSr;
        EndIf;
        If MYpgm1 <> 'W0051R19';
           LeaveSr;
        Endif;
        intStRptDiv = GetStRptDv(scCo#:scFnd:scEmp#);
        Setgt (scCo#:scFnd:scEmp#:intStRptDiv:scFyr:*Hival) peh06;
        Dou %Eof(WDEHL6);
           Readpe (scCo#:scFnd:scEmp#:intStRptDiv:scFyr) peh06;
           If %Eof;
              Leave;
           EndIf;
           If EHfaf = 'A';
              mgEEDT = 'WC40172';
              erEEDT = *On;
              isError = *On;
              isNotEligForOvr = *On;
              Leave;
           Endif;
        Enddo;
       Endsr;
       //*** end add ***** 01/25/22 ***********************
       //====================================================
       Begsr ChkRIU; // Check if policy is locked elsewhere.
       //====================================================
        If GetPolicyLockStatus(scCo#:scFnd:scEmp#:strJobInfo:
         strProcess:strUser) = *On;
           Select;
           When strProcess = 'MIDTERMEND';
              strProcess25 = 'Midterm Endorsement';
           When strProcess = 'ADDIVISION';
              strProcess25 = 'Add of Division';
           When strProcess = 'REVBILLING';
              strProcess25 = 'Revised Billing';
           Endsl;
           xxMsg = 'This operation is available when ' + %Trim(strProcess25) +
            ' Job(' + %Trim(strJobInfo) + ') has completed for ' +
            %Trim(strUser) + '.';
           xxTtl = '*** Policy Locked! ***';
           xxRsp = PopOneBtn(xxMsg:xxTtl);
           isError = *On;
        Else;
           SetPolicyLock(scCo#:scFnd:scEmp#:'MIDTERMEND');
        Endif;
       Endsr;
      /End-Free
     P ValidEndOpt     E
** LCK
W0051R19
W0051R20
W0051R24
W0051R25
W0051R26
W0051R40
