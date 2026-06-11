     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H**********************************************************************
     H/TITLE WTPOLQTFR - Quote Transfer
     H DFTNAME(WTPOLQTFR)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 04/01/13                                         *
      *  Programmer: C:282                                            *
      *  Project   : 147WAK                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      * 05/30/24 C:62  Project: WC2663(115236)                        *
      *                Add popup window to inform that invoice not    *
      *                paid.                                          *
      *                                                               *
      * 12/07/23 C:65  Support: Phw#105479 (CoveRisk)                 *
      *                Check if state reporting division exists in    *
      *                live environment for allowing transfer.        *
      *                                                               *
      * 12/07/22 C:65  Support: Phw#105479 (CoveRisk)                 *
      *                Change the Chk_WAQTP procedure to skip the     *
      *                edit check when called from LoadAgencyInfo as  *
      *                it shouldn't be editing yet at that point.     *
      *                                                               *
      * 11/10/22 C:344 Support: PHW103923                             *
      *                Add quote status type 'N' to the valid list of *
      *                types that allow transfer to live.             *
      *                                                               *
      * 11/03/21 C:136 Support: 99173                                 *
      *                Use RRN for WDELL instead of WDELLQ in the     *
      *                waiver edit.                                   *
      *                                                               *
      * 05/04/21 C:136 Project: WC1986                                *
      *                Add validation for EFT in Quote.               *
      *                                                               *
      * 02/04/21 C:33  Project: SY406 #94388                          *
      *                Use an appropriate rrnWDELLQ value             *
      *                                                               *
      * 10/28/19 C:136 Project: SY406                                 *
      *                Add edit to prevent transfer of waiver shell   *
      *                records written for Connect premium calc.      *
      *                                                               *
      * 05/11/18 C:291 Project: 150WCR                                *
      *                Do not send policy to error report if policy   *
      *                is overridden for Master/MCP policy assignment.*
      *                                                               *
      * 02/08/18 C:33  Support: 64192                                 *
      *               -Account for same employer number in quote &    *
      *                live.                                          *
      *                                                               *
      * 08/10/17 C:062 Project: 150WAP                                *
      *                Add combined peo processing                    *
      *                                                               *
      * 12/27/16 C:136 Support: 44876                                 *
      *                Don't edit against the live agent info         *
      *                for divisions added to renewal quotes          *
      *                                                               *
      * 01/07/16 C:65  Project: WC2401                                *
      *                Change the coding done under WC2254 to call an *
    *                API instead of directly calling the various    *
      *                programs for windows here.                     *
      *                                                               *
      *    09/17/15 C:302 Project: WC2254                             *
      *                   Recompile Only                              *
      *                                                               *
      * 08/27/15 C:33  36152                                          *
      *                -Submit immediate transfers.                   *
      *                -Update the finalize window.                   *
      *                -Update the manual assign window.              *
      *                -Update Transfer Type edit.                    *
      *                                                               *
      * 08/26/14 C:307 Project: SY354                                 *
      *                -Implement Routing Page Changes.               *
      *                                                               *
      * 05/15/15 C:308 Support: 34729                                 *
      *                Modified to use OUTQ from user profile setup.  *
      *                                                               *
      * 11/27/14 C:295 Redmine: 4700                                  *
      *                Change the activation group to *caller from ILE*
      *                                                               *
      * 11/10/14 C:136 Project: 114QPR Priss 11                       *
      *                Allow Agent Review quote type to transfer      *
      *                                                               *
      * 07/28/14 C:295 Project: 114WMD                                *
      *                Compilation only as the changes are in         *
      *                Action Panel                                   *
      *                                                               *
      *  06/05/14 C:180 WC099 Support: 83384                          *
      *                 - Default Transfer all divisions check box    *
      *                   to checked when policy is multi-division.   *
      *                                                               *
      *                 - Add edit when state reporting division is   *
      *                   missing.                                    *
      *                                                               *
      *  04/28/14A C:302 WA099 Support: 84814                         *
      *                 Default the trasfer all divisions flag to     *
      *                 "Y" when multi-div.                           *
      *                                                               *
      *  04/28/14 C:283 WA099 Support: 84926                          *
      *                 SIC Code Required edit for Entity/Location    *
      *                 when transferring a quote to Live.            *
      *                                                               *
      *  04/07/14 C:283 WA114 Support: 84956                          *
      *                 Added an edit to enable "Use Agency Default   *
      *                 Rate" check box if Agent's Default rate is    *
      *                 different from the Agent Rate in the Policy.  *
      *                                                               *
      *  03/18/14 C:290 WA025 Support: 84978                          *
      *                 Corrected the grammar of the error message    *
      *                 appearing when the employer is already        *
      *                 cancelled in Live.                            *
      *                                                               *
      *  02/25/14 C:65  Project:  Redmine #3991 (V14R1 Testing)       *
      *                 Correct read statement on loading agency grid *
      *                 - it was causing infinite loop and blowing    *
      *                 up the grid on too many records.              *
      *                                                               *
      *  02/06/14 C:136 SY336 Priss #28                               *
      *                 Disable qte agent vs liv agent if             *
      *                 'Use Quote Agency' flag is set to 'Y'         *
      *                                                               *
      *  02/05/14 C:65  SY336 Priss#53                                *
      *                 If cancel is clicked then pass no program     *
      *                 back out to close up window.  Remove call to  *
      *                 Policy Summary entirely as it was not needed. *
      *                                                               *
      *  02/04/14 C:190 SY336 Priss#28.                               *
      *                 Show live agency info in agency grid if       *
      *                 different from qte agency                     *
      *                                                               *
      *  09/03/13 C:189 Redmine: 3015                                 *
      *                 Use include instead of ignore for action      *
      *                 panel record format                           *
      *                                                               *
      *  09/03/13 C:282 Project: 114WJH Priss# 31                     *
      *                 Corrected the Rate in Agency Information      *
      *                 Grid.                                         *
      *                 Added the new checkbox "Use Agency Rate       *
      *                 Entered in Quote"                             *
      *                                                               *
      *  08/20/13 C:282 Project: 114WJH Priss# 27                     *
      *                 In Work w Group/Admin Darts if Quote Transfer *
      *                 to Live Transfer type is set to default to    *
      *                 Immediate and 'Transfer Quote to Live Batch'  *
      *                 is set then we select Transfer Quote the      *
      *                 screen should present with the 'Batch'        *
      *                 as the default.                               *
      *                                                               *
      *  08/13/13 C:282 Project: 114WJH Priss# 14                     *
      *                 Remove the handling 'if default is Immediate  *
      *                 and security setup is done for Batch only,    *
      *                 than Immediate option should be disabled.'    *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*
     fwtpolqtfr cf   e             workstn extfile('WTPOLQTFR')
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f                                     usropn
     f                                     Sfile(sflBillDiv:rrnBillDiv)
     f                                     Sfile(sflAgnInfo:rrnAgnInfo)
     f                                     Sfile(sflTErr:rrnTErr)
     fwtactpnl  cf   e             workstn
     f                                     Handler('PROFOUNDUI(HANDLER)')
     f                                     usropn
     f                                     Include(rcdPolPnl)
     f                                     Include(rcdQtePnl)
     fwdell     if   e           k disk    rename(pel01:pel00)
     f                                     infds(dbfds1)
     fwmemlq    if   e           k disk    rename(pem01:pem00q)
     f                                     infds(dbfds2)
     fwmall     if   e           k disk    rename(pal01:pal00)
     f                                     infDs(dbfds3)
     fwmahl     if   e           k disk    rename(pah01:pah00)
     f                                     infDs(dbfds4)
     fwdellq    if   e           k disk    rename(pel01:pel00q)
     f                                     infds(dbfds5)
     fwdelp     if   e             disk    rename(pel01:pel0p)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fwmaal1    if   e           k disk    rename(paa01:paa01)
     fwmarl     if   e           k disk    rename(par01:par00)
     fwaqtl7    if   e           k disk    rename(pqt01:pqt07)
     fwtqsl1    if   e           k disk    rename(pqs01:pqs01)
     fwmgcl     if   e           k disk    rename(pgc01:pgc00)
     fwmeml     if   e           k disk    rename(pem01:pem00) usropn
     fwmahlq    if   e           k disk    rename(pah01:pah00q) usropn
     fsmorl     if   e           k disk    rename(por01:por00) usropn
     fwmapl     if   e           k disk    rename(pap01:pap00)
      * Logical to check for assigned Emp# is already in Quote Transfer File
     fwaqol1    if   e           k disk    rename(pqo01:pqo02) prefix(QO2:2)
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fsmcol     if   e           k disk    rename(pco01:pco00)
     fwmeml3    if   e           k disk    usropn
     fwdrrl1    if   e           k disk
     fwtgtl     if   e           k disk
     fwtabl     if   e           k disk
     fsdofl1    if   e           k disk
     fwaqol     uf a e           k disk
     f*** Begin Add *** 08/10/17 ************************************
     Fwmrsl     if   e           k disk    Rename(prs01:prs00)
     f*** End   Add *** 08/10/17 ************************************
      *** Beg Add ***** 10/28/19 ************************************
     fwmwvl     if   e           k disk    Rename(pwv01:pwv00)
      *** End Add ***** 10/28/19 ************************************
     fqsysprt   o    f  132        printer oflind(*inof)       usropn

      /include sCopyPolPr
      /copy sPrRunCmd
      /copy SPrRtvBrnd
     d*** Begin Add *** 05/30/24 *******************************************
      /copy SPrModule
      /copy wPrPmtSvc
     d*** End   Add *** 05/30/24 *******************************************

     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  rrnwdell             397    400b 0
     d dbfds2          ds
     d  rrnwmemlq            397    400b 0
     d dbfds3          ds
     d rrnwmall              397    400b 0
     d dbfds4          ds
     d rrnwmahl              397    400b 0
     d dbfds5          ds
     d rrnwdellq             397    400b 0
      *
      * Standalone
     d @@rrn           s              9
     d @@mode          s              1
     d @@next          s             10
     d ##Err           s              1
     d isErr           s              1
     d xxRISK          s              9    inz
     d quoteMode       s              1    inz(' ')
     d clientID        s              3    inz('000')
     d outCltID        s              3
     d ##patlen        s              3  0 inz(40)
     d ##strlen        s              3  0 inz(100)
     d ##start         s              3  0 inz(1)
     d userLevel       s              3    inz
     d userType        s                   inz like(ofty01)
     d flgRes          s              1    inz
     d flgofEnv        s              1    inz
     d svActTrfgrp     s              1    inz
     d isAuthority     s              1
     d isAuthImd       s              1
     d isAuthBat       s              1
     d*** Begin Add *** 05/30/24 ****************************************
     d isInvCloud      s               n   Inz(*Off)
     d isGroupIncluded...
     d                 s               n   Inz(*Off)
     d paidInvCnt      s              5  0 Inz(0)
     d*** End   Add *** 05/30/24 ****************************************
      *
     d ##key           s             10
     d ##name          s            100
     d ##pdsc          s            100
     d scqte           s            100
     d scqte1          s             50
     d scqte2          s             50
     d ##scnres        s              3  0
     d xxType          s              1
     d xxQue           s                   like(UPOUTQ)
     d xxPrt           s                   like(ORFIL#)
     d xxPgm           s                   like(ORpnam)
     d xxSt            s              2
     d xxRRN           s              9  0
     d svrrnTErr       s              5  0
     d xxKey           s             10
     d xxRate          s              3  0
     d mode            s              1
     d edit_mask       s             10
     d cmd             s            255
     d lib             s             10
     d modLib          s             10
     d srdti           s              8  0
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d inELrrn         s              9
     d inMode          s              1
     d inNext          s             10
     d inTabInd        s              5
     d inQte           s              1
     d inQTrfRrn       s              9
     d charCo          s              3
     d charDate        s              8
     d charQue         s             10
     d charEmp         s              9
     d charFnd         s              3
     d charUserAuth    s            256
     d charELrrn       s              9
     d AgtDftRt        s              3  0
     d xxed            s             15
     d xxedn           s             80
     d scQTrnfCf       s            100
     d scAgnQteErr     s            256
     d***  Begin Add  ***  08/27/15  **************************************
     d strBtnMsg       s            256
     d strBtnTitle     s             50
     d strBtnResponse  s              1
     d***  End   Add  ***  08/27/15  **************************************
      *
     d errAgt#         s               n
     d openWMAHLQ      s               n
     d process         s               n   inz(*off)
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d qteMode         s               n   inz(*off)
     d***  Begin Add  ***  02/08/18  ***********************************
     d isRenewal       s              1n
     d***  End   Add  ***  02/08/18  ***********************************
     d isValid         s               n   inz(*off)
     d isMultDivFlg    s               n   inz(*off)
     d isSubErr        s               n   inz(*off)
     d xxResponse      s               n   inz(*off)
     d xxRespErr       s               n   inz(*off)
     d isMassRun       s               n
     d usa_date_n      s             10
     d usa_date        s               d   datfmt(*usa)
      *
     d ##data          s             80
     d prtCoNm         s             40
     d ##leng          s              3  0
     d LIBNAM          s             10
     d zzdiv           s                   like(EMdiv)
     d zzemp#          s                   like(EMemp#)
     d zzname          s                   like(EMname)
     d ##co            s                   like(ELco#)
     d ##fnd           s                   like(ELfnd)
     d ##emp           s                   like(ELemp#)
     d ##div           s                   like(ELdiv)
     d ##fyr           s                   like(ELfyr)
     d xxRRST          s                   like(RRst)
     d saveFyr         s                   like(ELfyr)
     d scco#           s                   like(ELco#)
     d scfnd           s                   like(ELfnd)
     d scfyr           s                   like(ELfyr)
     d scemp#          s                   like(ELemp#)
     d scdiv           s                   like(ELdiv)
     d saveEffDt       s                   like(ELpfdt)
     d xsysdt          s                   like(APcxdt)
     d liveAHAGT#      s                   like(AHagt#)
     d liveahrt#       s                   like(AHrt#)
     d liveAHEFDT      s                   like(AHefdt)
     d currAHEFDT      s                   like(AHefdt)
     d xxtbid          s                   like(GTtbid)
     d empAgt#         s                   like(QTagt#)
     d empRT#          s                   like(AHrt#)
     d prtEmpNm        s             32
     d prtEmpFlg       s                   like(*in60)
     d prtOpened       s                   like(*in60)
     d prtTitle        s                   like(ORTITL)
     d CUrrrt#         s                   like(AHrt#)
     d xxerror         s                   like(##err)
     d intEXQTIL       s                   like(EXQTIL)
     d intEXQTBL       s                   like(EXQTBL)
     d charEXQTIT      s                   like(EXQTIT)
     d charEXQTBT      s                   like(EXQTBT)
      // Begin Add 06/05/14
     D StatRptDiv      S               N   INZ(*OFF)
      // End   Add 06/05/14
     D*** begin add ***  12/07/23  ****************************************
     D liveSTRPDiv     S               N   INZ(*OFF)
     D*** end add *****  12/07/23  ****************************************
     d brandName       s             80    inz

     d QCA             s             55    DIM(6) CTDATA PERRCD(1)              CMD EXEC/QCAEXEC
      *** Beg Add ***** 10/28/19 *******************************
      *** Beg Chg ***** 11/03/21 *******************************
     d***strRrnWDELLQ    s              9
     d strRrnWDELL     s              9
      *** End Chg ***** 11/03/21 *******************************
     d strPopUpMsg     s            256
     d hasWaiverError  s               n
      *** End Add ***** 10/28/19 *******************************
      *
      * Constant Variable
     d Cancelled       c                   'This Policy or Billing Division -
     d                                     has been cancelled in the Live -
     d                                     environment.'
     d Address         c                   const('A complete mailing or -
     d                                     physical address not entered.')
     d County          c                   const('The County code not -
     d                                     entered.')
     d Region          c                   const('The region number not -
     d                                     entered.')
     d Unemployment    c                   const('The Unemployment # is -
     d                                     flagged as required and has not -
     d                                     been entered.')
     d Carrier         c                   const('The next available carrier # -
     d                                     is not within the valid range.')
     d Naics_code      c                   const('The NAICS Code is flagged as -
     d                                     required for this Rating State and -
     d                                     has not been entered')
     d Sic_code        c                   const('An SIC code is flagged -
     d                                     as a required entry and is -
     d                                     missing')
     d Fed_id_#        c                   const('The Federal Identification -
     d                                     Number is flagged as required -
     d                                     and has not been entered.')
     d CustomerID      c                   const('A Customer ID has not -
     d                                     entered')
     d EFT_online      c                   const('There is no valid EFT -
     d                                     setup for Online Self Reporting -
     d                                     processing.')
     d Type_busi       c                   const('Type of business not -
     d                                     valid for ')
     d Bill_sched      c                   const('The selected Payment Plan -
     d                                     not valid for Policy Year ')
     d State_risk      c                   'The State Risk ID# is flagged as -
     d                                     required and has not been entered.'
     d Ncci_risk       c                   'NCCI Risk ID # is flagged as -
     d                                     required and has not been entered.'
     d Agt_rate        c                   const('The Agency rate has not -
     d                                     entered for Policy year ')
     d QuoteType       c                   'The Agency quote status record -
     d                                     has an invalid type or was not -
     d                                     found.'
     d Producer        c                   const('The Producer is not valid -
     d                                     for year ')
     d Prod_nmbr       c                   const('Producer ')
     d Prod_canc       c                   const('is cancelled, as of date ')
     d Tzeros          c                   const('000000000')
     d Getlib          c                   CONST('RTVQTELIB')
     d xfer            c                   const('will be transferred')
     d xemp1           c                   const('Employer')
     d xemp2           c                   const('from quote employer')
     d Title           c                   'Mass Transfer Exception Report'
      // Begin Add 06/05/14
     d StateDiv        c                   'The quote selected for transfer -
     d                                      does not have a State Reporting -
     d                                      Division set.'
      // End   Add 06/05/14
      //*** beg add ***** 05/04/21 ************
       Dcl-C invQteEftRcd 'The quote EFT record cannot be effective -
        on or before a live EFT record.';
       Dcl-S chrDiv Char(5);
       Dcl-S chrFyr Char(3);
       Dcl-S chrErr Char(1);
       Dcl-S chrMode Char(1);
       Dcl-S chrData Char(100);
       Dcl-S chrPayType Char(1);
       Dcl-S chrEnv Char(1);
       Dcl-Pr SrOnline ExtPgm('SRONLI');
        co# Char(3);
        fnd Char(3);
        emp# Char(9);
        div Char(5);
        fyr Char(3);
        efdt Char(8);
        mode Char(1);
        err Char(1);
        data Char(100);
        payType Char(1);
        env Char(1);
       End-Pr;
      //*** end add ***** 05/04/21 ************
      *
      * Data Structure
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
      *
      *--------------------------------------------------------------------
      * Protype Declaration
      *--------------------------------------------------------------------
      * Check employer for complete entries
     d Chk_Emplr       pr

      * Check WMAHP records for complete entries
     d Chk_Wmahp       pr

      * Check WMALP records for complete entries
     d Chk_Wmalp       pr

      * Check cancellation status of live employer
     d Chk_Cx          pr

      * Check quote status of employer
     d Chk_Waqtp       pr
     d*** begin add ***  12/07/22  *************************************
     d inCheck                         n   const
     d*** end add *****  12/07/22  *************************************

      * Check WDELP records for complete entries
     d Chk_Wdelp       pr

      * Check Agent Default Rate processing
     d ChkAgtDftRt     pr

      * Check Call Stack
     d CheckCallStack  pr                  extPgm('SRCHKPGM')
     d pgmInStack                    10    const
     d inStack                        1n

      * Change mode, if required
     d CheckDivCount   pr

      * Security
     d CheckSecurity   pr                  EXTPGM('SRCCSC')
     d inUTYP                         1    const
     d inULVL                         3    const
     d inCTYP                         1    const
     d inCLVL                         3    const
     d inEXEC                         1    const
     d outRes                         1
     d inCTP2                         1    const
     d inCTP3                         1    const
     d inCTP4                         1    const
     d inCTP5                         1    const
     d inCTP6                         1    const

      * Concatenate addresses into a single field
     d ConcatAddr      pr           125
     d inAddress1                    40    const
     d inAddress2                    40    const
     d inCity                        25    const
     d inState                        2    const
     d inZipCode                      9  0 const

      * Clear Subfiles
     d ClrSubFiles     pr

      * Get cancellation date
     d Cxdate          pr             7  0

      * Error with the record
     d Errckw          pr

      * Error with the record
     d Error           pr

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

      * Get the agent default rate
     d GetAgtDftRt     pr                  like(AADRT#)

      * Gets the record from the RRN passed in
     d Get_Record      pr

      * Load Selection fields
     d LoadSelection   pr

      * Load policy contact grid
     d LoadBillDiv     pr

      * Load agency contact information
     d LoadAgencyInfo  pr

      * Gets records for Quote Transfer Error
     d Load_Sfl        pr

      * Load Quote Transfer Error
     d LoadTransError  pr

      * Main Processing
     d Main            pr

      * Mass Run
     d MassRun         pr              n

      * Printing Report
     d Print           pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      * Process other options
     d ProcErrMsg      pr
      *
     d ProcErrMsgEdt   pr                  extpgm('WTPOLQTE')
     d inELrrn                        9
     d inMode                         1
     d inNext                        10
     d inTabInd                       5
     d inQte                          1
      *
     d ProcPolTrack    pr                  extpgm('WTPOLTRACK')
     d inELrrn                        9
     d inMode                         1
     d inNext                        10
     d inTabInd                       5
     d inQte                          1
      *
     d ProcPolSum      pr                  extpgm('WTPOLSUM')
     d inELrrn                        9
     d inMode                         1
     d inNext                        10
     d inTabInd                       5
     d inQte                          1
      *
      * Routing for QSYSPRT
     d Routing         pr

      * Validate Employer
     d Validate        pr

      * Process Submit
     d Submit          pr

      * Process Transfer
     d Trnsfr          pr

      * Retrive Environment Library
     d RtvEnvLib       pr                  extPgm('RTVENVLIB')
     d requestType                   10    const
     d libName                       10
     d modLibName                    10

      * QuoteTransfer
     D***  Begin Chg  ***  08/27/15  *************************************
     d********** QuoteTransfer   pr                  EXTPGM('W8700C')
     d QuoteTransfer   pr                  EXTPGM('W8700C1')
     D***  End   Chg  ***  08/27/15  *************************************
     d charCo                         3
     d charDate                       8
     d charQue                       10
     d charFnd                        3
     d charEmp                        9
      *
     d/copy sPrAtrium
     d/copy sPrDate
     d/copy sPrGenPop
     d/copy sPrString
     d/copy sPrGenErr

     d/copy wPrAgent
     d/copy wPrProduce
     d/copy wPrGroup
     d/copy wPrRateSet
     d/copy wPrEmpLr
     d/copy wPrCarrier
     d/copy wPrAR
     d/copy wPrUsrPrms
     d/copy wPrLoc
     d/copy wPrClaims

      * Key List
      * Employer Master File Key List
     c     keyWMEMLQ     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div
      *
     c     key1WMEMLQ    klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
      *
     c     keyWMEML3     klist
     c                   kfld                    EMco#
     c                   kfld                    EMemp#
     c                   kfld                    EMdiv
     c                   kfld                    EMfnd
      *
     c     keyWMEML      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    scAssTEmp
      *
      * Agent rate file
     c     keyWMARL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    xxRate
     c                   kfld                    AHefdt
      *
     c     key1WMARL     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    xxRate
      *
      * Agt Quote System File
     c     keyWAQTL7     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    scAgnDiv
     c                   kfld                    quoteMode
     c                   kfld                    AHagt#
      *
     c     key2WAQTL7    klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    EMemp#
     c                   kfld                    EMdiv
     c                   kfld                    quoteMode
     c                   kfld                    empAgt#
      *
     c     key3WAQTL7    klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    EMemp#
     c                   kfld                    EMdiv
     c                   kfld                    quoteMode
      *
      * Emp quote Transfer file
     c     keyWAQOL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div
      *
      *  Quote Transfer File - Logical for existing Assigned Emp#
     c     keyWAQOL1     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    scAssTEmp
     c                   kfld                    ##div
      *
     c     key1WAQOL     klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
      *
      * Admin/Darts Control Definition File
     c     keyWMEXL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
      *
     * Stat Rpt/Rating File
     c     keyWDRRL1     klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    xxRRST
      *
      * Grp Carrier Information
     c     keyWMGCL      klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
      *
      * System Table File
     c     keyWTGTL      klist
     c                   kfld                    EMco#
     c                   kfld                    xxtbid
     c                   kfld                    xxkey
     *
      * Address Location File
     c     keyWMALL      klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    EMemp#
     c                   kfld                    EMdiv
      *
      * Agent history Master File
     c     keyWmahl      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
      *
     c     key1Wmahl     klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    EMemp#
     c                   kfld                    EMdiv
     c                   kfld                    ELfyr
      *
     c     key2Wmahl     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    ##div
      *
     c     keyWmahlq     klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    EMemp#
     c                   kfld                    EMdiv

     c     key2Wmahlq    klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    EMemp#
     *
      * Billing Scheduled code (WTBSP)
     c     keyWTABL      klist
     c                   kfld                    EMco#
     c                   kfld                    xxkey
      *
     * Agent Producer File
     c     keyWMAPL      klist
     c                   kfld                    EMco#
     c                   kfld                    AAfein
     c                   kfld                    AAmod
     c                   kfld                    AHprod
      *
      * Employer Detail File Key List
     c     keyWDELL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    kkdiv
      *
     c     key1WDELL     klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
     c                   kfld                    ##emp
     c                   kfld                    kkdiv
     c                   kfld                    ##fyr#            3 0
      *
     c     key2WDELL     klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    EMemp#
     c                   kfld                    EMdiv
      *
      * Agent Assignment Master File
     c     keyWMAAL1     klist
     c                   kfld                    EMco#
     c                   kfld                    EMfnd
     c                   kfld                    AHagt#
      *
     c     keyWMAAL2     klist
     c                   kfld                    AHCO#
     c                   kfld                    AHFND
     c                   kfld                    AHagt#
     *
     *  Group master file
     c     keyWMFNL      klist
     c                   kfld                    ##co
     c                   kfld                    ##fnd
      *
      * System Output Routing Maintenance
     c     keySMORL      klist
     c                   kfld                    xxpgm
     c                   kfld                    xxPrt
      *
      * User Parameters File
     c     keyWMUPL      klist
     c                   kfld                    q1user
      *
      * Entry Parameter List
     c     *Entry        Plist
     c                   Parm                    @@rrn                          Wdelp
     c                   Parm                    @@mode                         Mode 'C'
     c                   Parm                    @@next                         Calling Program Name
     c                   Parm                    ##err                          Return Error Info
      *
      /Free

       // Main program processing
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Else;
          xxRrn = 3386;
       Endif;

       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
       Endif;

       // Quote Environment
       qteMode = *On;
       quoteMode = 'Q';

       // Retriev Live Library
          LIBNAM = RtvQuoteLib('*LIV ');

       // Retrieve Client Id
       GetClientID(ClientID);

       // Get System Date and Time
       dsdate = DateTo8(SystemDate());
       dstime = TimeTo6(SystemTime());

       prtEmpFlg = *on;
       clear Elfyr;

       // Check whether the process is Mass Run
       isMassRun = MassRun;


       // If Process in not Mass Run than read the display file
       If mode <> 'M';
          Open wtpolqtfr;
          Open wtactpnl;
       EndIf;

       // Override to live Agent History File
        RtvEnvLib('*OTHER':lib:modLib);
        cmd = 'OVRDBF FILE(WMAHLQ) TOFILE('
                + %trim(lib) + '/WMAHLQ)';
        RunCmd(cmd:%len(cmd));
        If openWMAHLQ = *Off;
           Open wmahlq;
           openWMAHLQ = *On;
        Endif;

       //***  Begin Add  ***  08/27/15  ***********************************
       inquiry_B = *off;
       inquiry_I = *off;
       //***  End   Add  ***  08/27/15  ***********************************

        If mode = 'M';
           // Chain to Employer Detail for Program.
           Chain xxRrn pel0p;
           If %Found (wdelp);
              ##co    = ELco#;
              ##Fnd   = ELfnd;
              ##Emp   = ELemp#;
              ##Div   = ELdiv;
              ##Fyr   = ELfyr;
              Validate();
              ##Err = isErr;
           EndIf;
           Return;
        Else;
           Select;
           When mode = 'D';
              dspMode    = *On;
              dspVisb    = *Off;
              inquiry_I  = *On;
              inquiry_B  = *On;
              scTrfAlDIn = *On;
              scQteAgnIn = *On;
              scDAgnRtIn = *On;
           When mode = 'A';
              addMode    = *On;
              dspVisb    = *On;
           When mode = 'C';
              chgMode   = *On;
              dspVisb   = *On;
           Endsl;

           scEnv = quoteMode;
           scActMode = mode;
           scActCltId=ClientID;

           ProgramStart(process);

           // Check for Single or Multiple Division.
           CheckDivCount();

           // Transfer all division checkbox is disabled in case of single division.
           If isMultDivFlg = *Off;
              scTrfAlDIn = *On;
           Else;
              If mode <> 'D';
                scTrfAlDiv = 'Y';
                 scTrfAlDIn = *Off;
              EndIf;
           // Begin Add 06/05/14
                 scTrfAlDiv = 'Y';
           // End   Add 06/05/14
           EndIf;

           If process;
              Main();
           Endif;

       // Calling the Quote Summary on Cancel and Policy Tracking on Agency Quote Status Error
           inMode    = 'C';
           inNext    = ' ';
           inTabInd  = '00000';
           inQte     = 'Q';

           Select;
           EndSl;

           // Unlock WMECP;
       *inof = *on;
       If %open(Qsysprt);
          Open Smorl;
          xxprt = '0';
          Chain keySMORL smorl;
          If %found;
             Except Route;
          Endif;
          Close smorl;
       Endif;
           *InLr = *On;

       EndIf;                                                                  // mode = 'M'

      /End-Free
     *====================================================================
     o/copy scopyrout
     oqsysprt   e            prtHeader      1  6
     o                       q1pgm               10
     o                       prtCoNm             81
     o                       q1user             132
     o          e            prtHeader      2
     o                       dsDate              10 '  /  /    '
     o                       prtTitle            91
     o                                          126 'Page'
     o                       page          4    132
     o          e            prtHeader      0
     o                                            9 ' EMPLOYER'
     o                                          + 1 '  DIV'
     o                                          + 1 ' YR'
     o                                          + 1 'EMPLOYER NAME'
     o                                           78 'REASONS FOR NON-TRANSFER'
     o                                          +00 '/EXCEPTIONS'
     o          e            prtEmplr    1  0
     o                       EMEMP#        z      9
     o                       EMDIV         z    + 1
     o                       ELFYR         z    + 1
     o                       prtEmpNm           + 1
     o          e            prtDetail   1  1
     o                       scQTrfE            132
       //*** beg add ***** 05/04/21  **************************
       //===========================================
       Dcl-Proc Chk_Eft;
       //===========================================
       Dcl-S intRecCount Packed(3:0);
       Exec Sql
        Select Count(*)
        Into :intRecCount
        From WMEFP a
        Where Exists(
         Select * From WMEFP b
         Where a.EFco# = b.EFco#
          And a.EFfnd  = b.EFfnd
          And a.EFemp# = b.EFemp#
          And a.EFdiv  = b.EFdiv
          And b.EFEnvFlg = ' '
          And a.EFefdt <= b.EFefdt
          And b.EFefdt <> b.EFexdt)
         And a.EFco#  = :EMco#
         And a.EFfnd  = :EMfnd
         And a.EFemp# = :EMemp#
         And a.EFdiv  = :EMDiv
         And a.EFEnvFlg = 'Q'
         And a.EFefdt <> a.EFexdt;
       If intRecCount > 0;
          scQTrfE = invQteEftRcd;
          scQTEtoltp = scQTrfE;
          scIndex = 1;
          Error();
       Endif;
       End-Proc;
       //*** end add ***** 05/04/21  **************************
     *====================================================================
     * Chk_Emplr - Check employer for complete entries
     *====================================================================
     p Chk_Emplr       B
     d Chk_Emplr       Pi

      /Free
       Get_Record();

       Load_Sfl();

       If xxerror = *on And mode = 'M';
          isErr = '2';
       EndIf;

      /End-Free
     p Chk_Emplr       E
     *====================================================================
     * Chk_Wmahp - Check WMAHP records for complete entries
     *====================================================================
     p Chk_Wmahp       B
     d Chk_Wmahp       Pi
    
     c     key1Wmahl     chain     pah00                              99

     c                   eval      errAgt# = *off

     c                   dow       *in99 = *off

     c                   if        AHSTAT <> 'I'

      * To check if all divison or slected division have same agent in Quote and Live
      * than 'Use Quote Agency After Transfer Live' will be disabled.

      *** BEG CHG ***** 12/27/16 ****************************
     c***********        If        liveAHAGT# = *Zero Or
     c***********                  liveAHAGT# = empAgt#
     c                   If        liveAHAGT# = *Zero  And
     c                             EMrdfl     = *Blank Or
     c                             liveAHAGT# = empAgt#
      *** END CHG ***** 12/27/16 ****************************
     c                   eval      scQteAgnIn = *On
     c                   EndIf

      * To check if all divison or slected division have same agent in Quote and Live
      * than 'Use Default Agency Rate from Agency Assignmnet' will be disabled.

      *** BEG CHG ***** 12/27/16 ****************************
     c***********        If        liveAHAGT# = *Zero Or
     c***********                  liveAHRT#  = empRT#
     c                   If        liveAHAGT# = *Zero  And
     c                             EMrdfl     = *Blank Or
     c                             liveAHRT#  = empRT#
      *** END CHG ***** 12/27/16 ****************************
     c                   eval      scDAgnRtIn = *On
     c                   EndIf

      * Check for difference between Test and Live Agent
     c                   Select
     c                   When      liveAHAGT# <> *zero
     c                              and liveAHAGT# <> empAgt#
     c                              and liveAHEFDT <= currAHEFDT
     c                              and not errAgt#
      *
     c                   If        scQteAgn = ' ' and not isMassRun
     c                   eval      scQTrfE = 'Quote agent '
     c                                     + %trim(%editc(empAgt#:'Z'))
     c                                     + ' is different from Live agent '
     c                                     + %trim(%editc(liveAHAGT#:'Z'))
     c                                     + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 7
     c                   callp     error
     c                   Endif
      *
     c                   If        isMassRun
     c                   eval      scQTrfE = 'Quote agent '
     c                                     + %trim(%editc(empAgt#:'Z'))
     c                                     + ' is different from Live agent '
     c                                     + %trim(%editc(liveAHAGT#:'Z'))
     c                                     + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 7
     c                   callp     error
     c                   Endif
     c                   eval      errAgt# = *on
      *
      * Check for difference between Test and Live Agent Rate
      *
     c                   When      liveAHAGT# <> *zero
     c                              and liveAHRT# <> empRT#
     c                              and liveAHEFDT <= currAHEFDT
     c                              and not errAgt#
      *
     c                   If        scQteAgn = 'Y' and not isMassRun
     c                   eval      scQTrfE = 'Quote agent rate '
     c                                     + %trim(%editc(empRT#:'Z'))
     c                                + ' is different from Live agent rate '
     c                                    + %trim(%editc(liveAHRT#:'Z'))
     c                                    + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 7
     c                   callp     error
     c                   Endif
      *
     c                   If        isMassRun
     c                   eval      scQTrfE = 'Quote agent rate '
     c                                     + %trim(%editc(empRT#:'Z'))
     c                                + ' is different from Live agent rate '
     c                                    + %trim(%editc(liveAHRT#:'Z'))
     c                                    + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 7
     c                   callp     error
     c                   Endif
      *
     c                   eval      errAgt# = *on
     c                   EndSl

     c     *cymd         move      AHEFDT        usa_date
     c                   move      usa_date      usa_date_n

     c                   if        AHRT# = *zero
     c                   eval      scQTrfE = agt_rate +
     c                                       %trim((%editc(ELFYR:'Z'))) + ','
     c                   eval      scQTrfE = %trim(scQTrfE) + ' agent '
     c                             + %trim((%editc(AHAGT#:'Z')))
     c                             + ' & effective date ' + usa_date_n + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 7
     c                   callp     error
     c                   endif
      *
     c     keyWMAAL1     chain     paa01                              99
     c  n99keyWMAPL      chain     pap00                              99
     c                   if        *in99 = *on
     c                   eval      scQTrfE = producer +
     c                                       %trim((%editc(ELFYR:'Z'))) + ','
     c                   eval      scQTrfE = %trim(scQTrfE) + ' agent '
     c                             + %trim((%editc(AHAGT#:'Z')))
     c                             + ' & effective date ' + usa_date_n + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 7
     c                   callp     error
     c                   endif

      * If found Producer w/Cancel Date, Check it against system date
     c                   if        *in99 = *OFF and APCXDT <> *zeros
     c                   time                    dsdttm
     c                   eval      srdti = dsdate
      /Free
       xsysdt = dtCYMD(srdti);
      /End-Free
     c     *cymd         move      APCXDT        usa_date
     c                   move      usa_date      usa_date_n
     c                   if        (apcxdt > *zeros) AND (apcxdt <= xsysdt)
     c                   eval      scQTrfE = prod_nmbr
     c                             + %trim((%editc(APPROD:'Z'))) + ' '
     c                             + prod_canc + usa_date_n + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 7
     c                   callp     error
     c                   endif
     c                   endif

     c                   endif

     c     key1Wmahl     reade     pah00                                  99

     c                   enddo

     p Chk_Wmahp       E
     *====================================================================
     * Chk_Wmalp   Check WMALP records for complete entries
     *====================================================================
     p Chk_Wmalp       b
     d Chk_Wmalp       pi

     c     keyWMALL      chain     pal00                              99
     c                   dow       *in99 = *off

     c                   eval      xxtbid = 'BUSI'
     c                   eval      xxkey  = ALTPOB
     c     keyWTGTL      chain     pgt01                              60
     c                   if        ALELTP = 'E' and *in60 = *on
     c                   eval      scQTrfE = type_busi + 'entity ' + ALELID
     c                              + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 8
     c                   callp     error
     c                   endif
      *
      * Ensure ALNAIC is populated if Ratings and Statistics for state
      * requires it
     c                   select
     c                   when      ALPST <> *blanks
     c                   eval      xxRRST = ALPST
     c                   when      ALMST <> *blanks
     c                   eval      xxRRST = ALMST
     c                   endsl

     c     keyWDRRL1     chain     prr01
     c                   if        %found

     c                   if        RRNAIC = 'Y' and
     c                             ALNAIC = *blanks
     c                   move      ALELID        xxELID           10 0
     c                   eval      scQTrfE = naics_code +
     c                             ' Entity/Location ID ' +
     c                             %trim(%editc(xxELID : 'Z')) + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 8
     c                   callp     error
     c                   endif

     c                   endif
      /Free
       If ALSIC = *Blanks and EXsicr = 'Y';
          xxELID = %Int(ALelid);
          scQTrfE = sic_code + ' for +
                     location ID ' +
                      %trim(%editc(xxELID : 'Z')) + '.';
          scQTEtoltp = scQTrfE;
          scIndex = 8;
          Callp error();
       Endif;
      /End-Free

     c     keyWMALL      reade     pal00                                  99
     c                   enddo

     p Chk_Wmalp       E
     *====================================================================
     * Chk_Cx - Check cancellation status of live employer
     *====================================================================
     p Chk_Cx          b
     d Chk_Cx          pi

     c                   if        CxDate <> 0
     c                   eval      scQTrfE = cancelled
     c                   eval      xxError = *on
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   eval      dspVisb  = *Off
     c                   callp     error
     c                   eval      dspVisb  = *On
     c                   endif

     p Chk_Cx          E
     *====================================================================
     * Chk_Waqtp - Check quote status of employer
     *====================================================================
     p Chk_Waqtp       B
     d Chk_Waqtp       Pi
     d*** begin add ***  12/07/22  *************************************
     d inCheck                         n   const
     d*** end add *****  12/07/22  *************************************

      * Get live agent
     c                   clear                   liveAHAGT#
     c                   clear                   liveAHRT#
     c                   clear                   liveAHEFDT

     C***  Begin Add  ***  02/08/18  ************************************
     C                   If        isRenewal
     C***  End   Add  ***  02/08/18  ************************************

     c     keyWmahlq     setgt     pah00q
     c                   dou       %eof or AHSTAT <> 'I'
     c     keyWmahlq     readpe    pah00q
     c                   enddo

     c                   select
     c                   when      not %eof and AHAGRC <> *zero
     c                   eval      liveAHAGT# = AHAGRC
     c                   eval      liveAHRT# = AHRTRC
     c                   eval      liveAHEFDT = AHEFDT
     c                   when      not %eof
     c                   eval      liveAHAGT# = AHAGT#
     c                   eval      liveAHRT# = AHRT#
     c                   eval      liveAHEFDT = AHEFDT
     c                   endsl

     C***  Begin Add  ***  02/08/18  ************************************
     C                   Endif
     C***  End   Add  ***  02/08/18  ************************************

     c                   eval      empAgt# = *zero
     c                   eval      empRT# = *zero
     c                   eval      currRt# = *zero
     c                   clear                   currAHEFDT

     c     keyWmahlq     setgt     pah00

     c                   dou       %eof or AHSTAT <> 'I'
     c     keyWmahlq     readpe    pah00
     c                   enddo

     c                   if        not %eof
     c                   eval      empAgt# = AHAGT#
     c                   eval      empRT#   = AHRT#
     c                   eval      currRt# = AHRT#
     c                   eval      currAHEFDT = AHEFDT
     c                   endif

     c     key2WAQTL7    setgt     waqtl7
     c     key3WAQTL7    readpe    waqtl7

     c                   eval      xxType = upper(qttype)

     c                   select
     c                   when      %eof or QTAGT# <> empAgt#
     c                   eval      xxTYPE = *blanks
     c                   when      xxTYPE = 'T' and QTXFER <> *blanks
     c                   eval      xxTYPE = upper(QTXFER)
     c                   endsl
     c*** begin add ***  12/07/22  *************************************
     c                   If        inCheck
     c*** end add *****  12/07/22  *************************************
     c
      *** Begin Add ****** 11/10/22 *** PHW103923 *************************
      * When the Client not CoveRisk, check for valid Quote Status Types
     c                   if        ClientID <> '129'
     c                             and xxTYPE <> 'P'
      *** End   Add ****** 11/10/22 *** PHW103923 *************************
      *** Begin Change *** 11/10/22 *** PHW103923 *************************
     c****               if            xxTYPE <> 'P'
      *** End   Change *** 11/10/22 *** PHW103923 *************************
     c                             and xxTYPE <> 'Q'
     c                             and xxTYPE <> 'B'
     c                             and xxTYPE <> 'M'
     c                             and xxTYPE <> 'F'
     c                             and xxTYPE <> 'G'
     c                             and xxTYPE <> 'U'
     c                             and xxTYPE <> 'Y'
      *** Begin Add *** 11/10/22 *** PHW103923 *************************
      * When the Client is CoveRisk, check for valid Quote Status Types
     c                             or ClientID = '129'
     c                             and xxTYPE <> 'P'
     c                             and xxTYPE <> 'Q'
     c                             and xxTYPE <> 'B'
     c                             and xxTYPE <> 'M'
     c                             and xxTYPE <> 'F'
     c                             and xxTYPE <> 'G'
     c                             and xxTYPE <> 'U'
     c                             and xxTYPE <> 'Y'
     c                             and xxTYPE <> 'N'
      *** End   Add *** 11/10/22 *** PHW103923 *************************
     c                   eval      scQTrfE = quoteType
     c                   eval      xxerror = *on
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scCallPgm  = 'WTPOLTRACK'
     c                   callp     error
     c                   endif
     c*** begin add ***  12/07/22  *************************************
     c                   Endif                                                  If inCheck
     c*** end add *****  12/07/22  *************************************

     p Chk_Waqtp       E
     *====================================================================
     * Chk_Wdelp - Check WDELP records for complete entries
     *====================================================================
     p Chk_Wdelp       B
     d Chk_Wdelp       Pi

     d*** Begin Add *** 08/10/17 ***********************************
     d rateState       s              2    Inz
     d errorMsg        s            100    Inz
     d*** End   Add *** 08/10/17 ***********************************

     c     key2WDELL     chain     pel00                              99
     c                   dow       *in99 = *off

     c                   eval      prtEmpFlg = *on

     c                   movel(p)  ELBSCD        xxkey

     c     keyWTABL      chain     pbs01                              99

     c                   if        *in99 = *on
     c                   eval      scQTrfE = bill_sched
     c                                       + %trim((%editc(ELFYR:'Z'))) + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 2
     c                   callp     error
     c                   else
       //*** beg add ***** 05/04/21 ************************
       // Make sure S/R Online billing plan has online payment setup
       charCo = %Editc(ELco#:'X');
       charFnd = %Editc(ELfnd:'X');
       charEmp = %Editc(ELemp#:'X');
       chrDiv = %Editc(ELdiv:'X');
       chrFyr = %Editc(ELfyr:'X');
       charDate = %Char(%Date():*Usa0);
       chrMode = '1';
       chrErr = *Blanks;
       chrData = *Blanks;
       chrPayType = *Blanks;
       chrEnv = 'Q';
       If BSonli = 'Y';
          SrOnline(charCo:charFnd:charEmp:chrDiv:chrFyr:
           charDate:chrMode:chrErr:chrData:chrPayType:chrEnv);
          If chrErr = '1';
             scQTrfE = 'S/R Online billing plan ' + %EditC(ELbscd:'X') +
              ' requires online payment setup.';
             scindex = 1;
             callp Error();
          Endif;
       Endif;
       //*** end add ***** 05/04/21 ************************

     c                   endif
      *
     c                   if        exerw1 = 'Y' and elffd1 = ' '
     c                   eval      scQTrfE = exehd1 + ' not entered'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 5
     c                   callp     error
     c                   endif
      *
     c                   if        exerw2 = 'Y' and elffd2 = ' '
     c                   eval      scQTrfE = exehd2 + ' not entered'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 5
     c                   callp     error
     c                   endif
     c*** Begin Add *** 08/10/17 ***********************************
      /free
       // Check to see if there are errors with PEO if renewal
       //*** Begin Chg *** 05/11/18 **************************************
       //If ELnwRw = 'R';
       If ELnwRw = 'R' and
           EMovrMP <> 'Y';                 //Not overriden for Master/MCP Policy assignment
          //*** End Chg *** 05/11/18 **************************************
          // Get the rating State
          rateState = ' ';
          Chain (ELco#:ELrtCd) prs00;
          If %Found(wmrsl);
             rateState = RSjrSt;
          EndIf;

          // Determine if there are any PEO errors
          errorMsg = EdtPeoPolType(ELco#:ELfnd:ELemp#:ELdiv:rateState:ELpfDt);

          // If error message returned then display error
          If errorMsg <> ' ';
             scQTrfE    = errorMsg;
             scQTEtoltp = scQTrfE;
             scIndex    = 2;
             error();
          EndIf;
       EndIf;
      /end-free
     c*** End   Add *** 08/10/17 ***********************************
      *
      *** Beg Add ***** 10/28/19 ***********************************
      /Free
       Setll (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pwv00;
       Dou %Eof(WMWVL);
          Reade (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pwv00;
          If %Eof;
             Leave;
          Endif;
          //*** Beg Chg ***** 11/03/21 *********************
          //*** strRrnWDELLQ = %EditC(rrnWdellQ:'X');
          //*** If WVname = 'Portal Waiver';
          //***    scURL = CreateURL('W1054RA':'Waiver Endorsements':strRrnWDELLQ);
          //***    scQTrfE    = 'Waivers must be edited before the quote ' +
          //***     'transfer can be processed.';
          //***    scQTEtoltp = scQTrfE;
          //***    hasWaiverError = *On;
          //***    error();
          //***    Leave;
          //*** Endif;
          strRrnWDELL  = %EditC(rrnWdell :'X');
          If WVname = 'Portal Waiver';
             scURL = CreateURL('W1054RA':'Waiver Endorsements':strRrnWDELL);
             scQTrfE    = 'Waivers must be edited before the quote ' +
              'transfer can be processed.';
             scQTEtoltp = scQTrfE;
             hasWaiverError = *On;
             error();
             Leave;
          Endif;
          //*** End Chg ***** 11/03/21 *********************
       Enddo;
      /End-Free
      *** End Add ***** 10/28/19 ***********************************
      *
     c                   callp     Chk_WMAHP
      *
     c     key2WDELL     reade     pel00                                  99
      *
     c                   enddo

     p Chk_Wdelp       E
     *====================================================================
     * ChkAgtDftRt - Check Agent Default Rate processing
     *====================================================================
     p ChkAgtDftRt     B
     d ChkAgtDftRt     Pi
      *
     c                   eval      agtDftRt = GetAgtDftRt
      *
     c                   If        EXAAFL = 'Y' and empAgt# <> 0
     c                              and agtDftRt <> *zero
     c                              and agtDftRt <> currRt#
     c                              and scQteAgnRt = ' '

     c                   select

     c                   when      not isMassRun
     c                   If        scDflAgnRt = ' '
     c                   eval      scQTrfE  = 'Quote agent rate not equal ' +
     c                                        'to agent default rate.'
     c                   eval      scDAgnRtIn = *Off
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 7
     c                   callp     error
     c                   endif
     c                   eval      QOrafl = scDflAgnRt

     C                   when      isMassRun
     c                   eval      scQTrfE  = 'Quote agent rate not equal ' +
     c                                        'to agent default rate.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scDAgnRtIn = *Off
     c                   callp     error

     c                   endsl

     c                   endif

     pChkAgtDftRt      e
      //==================================================================
      // CheckDivCount - Employer having single or multiple division
      //==================================================================
     p CheckDivCount   B
     d CheckDivCount   Pi

      /Free
       isMultDivFlg = *Off;
       Select;
       When DivCount(##co:##fnd:##emp) > 1;
          isMultDivFlg = *On;
       Endsl;
      /End-free

     pCheckDivCount    e
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     p ClrSubFiles     B
     d ClrSubFiles     PI
      /Free

          rrnBillDiv = 0;
          rrnAgnInfo = 0;
          rrnTErr    = 0;
          clrBillDiv = *On;
          clrAgnInfo = *On;
          clrTErr    = *On;
          dspBillSub = *Off;
          dspAgnSub  = *Off;
          dspTErrSub = *Off;
          dspBillCtl = *Off;
          dspAgnCtl  = *Off;
          dspTErrCtl = *Off;
          Write rcdQteTfr;

      /End-Free
     p ClrSubfiles     E
      *====================================================================
      * Concatenate addresses into a single field
      *====================================================================
     pConcatAddr       b
     dConcatAddr       pi           125
     d inAddress1                    40    const
     d inAddress2                    40    const
     d inCity                        25    const
     d inState                        2    const
     d inZipCode                      9  0 const

     d outAddress      s            125
     d outZipCode      s             10
      /Free
       If inAddress1 = *blank;
          Return *blanks;
       Endif;
       outZipCode = %editc(inZipCode:'X');
       Select;
       When %subst(outZipCode:6:4) = '0000';
          outZipCode = %subst(outZipCode:1:5);
       Other;
          outZipCode = %subst(outZipCode:1:5) + '-' + %subst(outZipCode:6:4);
       Endsl;
       outAddress = %trim(inAddress1);
       If inAddress2 <> *blank;
          outAddress = %trim(outAddress) + ', ' + %trim(inAddress2);
       Endif;
       outAddress = %trim(outAddress) + ', ' + %trim(inCity) + ', ' +
        %trim(inState) + '  ' + %trim(outZipCode);
       Return outAddress;
      /End-Free

     pConcatAddr       e
     *====================================================================
     * CxDate    - Get cancellation date
     *====================================================================
     pCxDate           b                   export
     dCxDate           PI             7  0

     d cx_date         s              7  0

     c                   clear                   cx_date

     c                   select
     c***  Begin Add  ***  02/08/18  ************************************
     c                   When      not isRenewal
     c***  End   Add  ***  02/08/18  ************************************
     c                   when      EMEFD5 <> *zero
     c                   eval      cx_date = EMCAD5
     c                   when      EMEFD4 <> *zero
     c                   eval      cx_date = EMCAD4
     c                   when      EMEFD3 <> *zero
     c                   eval      cx_date = EMCAD3
     c                   when      EMEFD2 <> *zero
     c                   eval      cx_date = EMCAD2
     c                   when      EMEFD1 <> *zero
     c                   eval      cx_date = EMCAD1
     c                   endsl
     c                   return    cx_date

     pCxDate           e
     *====================================================================
     * ERRCKW - Subroutine For Error Checking WDWEMP#
     *====================================================================
     perrckw           b
    derrckw           pi
    
     *  The Employer Number Cannot be equal to zero
     *
     c  n90              if        scAssTEmp <= *zero
     c                   eval      ErrTEmpIn1 = *on
     c                   eval      *in90 = *on
     c                   endif                                                scAssTEmp <=*ZERO
     *
     c                   if        *in90 = *off
     c                   MOVE      *BLANKS       CMDST1
    c     LIBNAM        IFNE      *BLANKS
    c                   MOVEL(P)  QCA(4)        CMDST1
    c                   CAT(P)    LIBNAM:0      CMDST1
    c                   CAT(P)    QCA(5):0      CMDST1
    c                   Z-ADD     55            LENGTH
    c                   CALL      'QCMDEXC'
    c                   PARM                    CMDST1
    c                   PARM                    LENGTH
    c                   OPEN      WMEML
     c     keyWMEML      chain     pem00                              60
     c                   endif
      *
     c                   if        *in90 = *off
     c                             and *in60 = *off
     c                   eval      ErrTEmpIn2   = *on
     c                   eval      *in90 = *on
     c                   endif
    c                   MOVEL(P)  QCA(6)        CMDST1
    c                   Z-ADD     55            LENGTH
    c                   CALL      'QCMDEXC'
    c                   PARM                    CMDST1           55
    c                   PARM                    LENGTH           15 5
    c                   CLOSE     WMEML
    c                   ENDIF
     *
     c                   if        *in90 = *off
     c     keyWAQOL1     chain     pqo02                              60
     c                   endif
     c                   if        *in90 = *off
     c                             and *in60 = *off
     c                   eval      ErrTEmpIn3 = *on
     c                   eval      *in90 = *on
     c                   endif

     perrckw           e
     *====================================================================
     * ERROR - Error with the record
     *====================================================================
     p Error           b
     d Error           pi

     c                   select
     c                   when      mode <> 'M'
     c                   eval      rrnTErr = rrnTErr  + 1
     c                   write     sflTErr
     c                   when      mode = 'M'
     c                   callp     Print
     c                   endsl
     c                   eval      xxerror = *on

     p Error           e
     *====================================================================
     *====================================================================
     * GetAgtDftRt - Get the agent default rate
     *====================================================================
     pGetAgtDftRt      b                   export
     dGetAgtDftRt      pi                  like(AADRT#)

     c     keyWMAAL3     klist
     c                   kfld                    EMCO#
     c                   kfld                    EMFND
     c                   kfld                    empAgt#

     c                   clear                   AADRT#
     c     keyWMAAL3     chain     wmaal1
     c                   return    AADRT#

     pGetAgtDftRt      e
     *====================================================================
     * GET_RECORD - Gets the record from the RRN passed in
     *====================================================================
     p Get_Record      B
     d Get_Record      Pi
    
     C*** begin add ***  12/07/23  ****************************************
     C                   Move      *off          liveSTRPDiv
     C*
     C* Determine if employer is in live environment and has state rpt div.
     C*
    c     LIBNAM        IFNE      *BLANKS
     c                   MOVE      *BLANKS       CMDST1
    c                   MOVEL(P)  QCA(4)        CMDST1
    c                   CAT(P)    LIBNAM:0      CMDST1
    c                   CAT(P)    QCA(5):0      CMDST1
    c                   Z-ADD     55            LENGTH
    c                   CALL      'QCMDEXC'
    c                   PARM                    CMDST1
    c                   PARM                    LENGTH
    c                   OPEN      WMEML
      *
     c     key1WMEMLQ    Chain     pem00                              60
     C                   Dow       *in60 = *off
     C                   If        EMStrp = 'Y'
     C                   Move      *on           liveSTRPDiv
     C                   Leave
     C                   Endif
     C     key1WMEMLQ    Reade     pem00                                  60
     C                   Enddo
      *
    c                   MOVEL(P)  QCA(6)        CMDST1
    c                   Z-ADD     55            LENGTH
    c                   CALL      'QCMDEXC'
    c                   PARM                    CMDST1           55
    c                   PARM                    LENGTH           15 5
    c                   CLOSE     WMEML
     C                   Endif                                                  LIBNAM <> ' '
     C*** end add *****  12/07/23  ****************************************
     c     keyWMEMLQ     chain     pem00q                             99
     c  n99EMco#         chain     pco00                              99
     c                   if        *in99 = *on
     c                   callp     RtvBrnd('I':brandName)

     c                   eval      ##data   = brandname
     c                   else
     c                   eval      ##data = CONAME
     c                   endif
     c                   movel     ##data        prtCoNm

     c     keyWMEXL      chain     pex00                              60
     c                   select
     c                   when      EMpst <> *blanks
     c                   eval      xxRRST = EMpst
     c                   other
     c                   eval      xxRRST = EMmst
     c                   endsl
     c                   clear                   RRnaic
     c                   clear                   RRurfl
     c     keyWDRRL1     chain     prr01
     c                   clear                   GCfrcn
     c                   clear                   GCtocn
     c     keyWMGCL      chain     pgc00
    
      /Free
       //***  Begin Add  ***  02/04/21  ****************
       Clear rrnWDELLQ;
       //***  End   Add  ***  02/04/21  ****************
       If not isMassRun;
          scTrfErDiv = EMdiv;
          scTrfDBANm = EMname + %subst(EMnam2:1:10);
          scTrfFedNm = EMfdnm + %subst(EMfdn2:1:10);
          Setgt (EMCo#:EMFnd:EMEmp#:EMDiv) pel00q;
          Readpe (EMCo#:EMFnd:EMEmp#:EMDiv) pel00q;
          If Not %Eof(Wdellq);
             scQTrfRrn = rrnWdellq;
          EndIf;
       EndIf;
     /End-Free
    
     p Get_Record      E
       //===================================================================
       // Load Selection - Load the criteria fields for FYR/DIV drop down boxes.
       //===================================================================
     p LoadSelection   B
     d LoadSelection   PI

     dfyrLine          s             31
     ddivLine          s             49
     dfirst            s               n   inz(*on)
      /free
       xxSt = RatingSt(ELco#:ELrtcd);

       clear fyrChoices;
       fyrChoices = 'ELco# = ' + %char(ELco#) + ' AND ELfnd = '
        + %char(ELfnd) + ' AND ELemp# = ' + %char(ELemp#)
        + ' AND ELdiv = ' + %char(kkdiv);

       clear divChoices;
       divChoices = 'EMco# = ' + %char(ELco#) + ' AND EMfnd = '
        + %char(ELfnd) + ' AND EMemp# = ' + %char(ELemp#);

      /end-free
     p LoadSelection   E
       //===================================================================
       // Billing Divisions
       //===================================================================
     p LoadBillDiv     B
     d LoadBillDiv     PI
      /free

       // clear the subfile.
       clrBillDiv = *Off;
       dspBillSub = *On;
       dspBillCtl = *On;
       // Begin add 06/05/14
         StatRptDiv = *OFF;
       // End add   06/05/14

       // Read the file WMEMLQ for all division if Transfer All Division is 'Y'
       // else read the selected division record.

       If scTrfAlDiv = ' ';
          Setll keyWMEMLQ PEM00q;
          Reade keyWMEMLQ PEM00q;
       Else;
          Setll key1WMEMLQ PEM00q;
          Reade key1WMEMLQ PEM00q;
       EndIf;

       DoW Not %EOF(WMEMLQ);
          Clear sflBillDiv;
          scBilDiv   = EMdiv;
          scBilDbaNm = EMname + %subst(EMnam2:1:10);
          scBilFedNm = EMfdnm + %subst(EMfdn2:1:10);
          scBilIncDt = (dtMDYY(GetEmIncDt(EMco#:EMfnd:EMemp#:EMdiv)));
          scCxDate   = (dtMDYY(
           GetCxDate(EMco#:EMfnd:EMemp#:EMdiv)));
          scBilPhyAd = ConcatAddr(EMpad1:EMpad2:EMpcty:
           EMpst:EMpzip);
          scBilMalAd = ConcatAddr(EMmad1:EMmad2:EMmcty:
           EMmst:EMmzip);
          scBilFedId = EMfein;
          scBilRptSt = GetRatingState(EMco#:EMfnd:EMemp#:EMdiv:##fyr);
          If EMstrp = 'Y';
             scBilStRpD = 'Yes';
       // Begin add 06/05/14
             StatRptDiv = *ON;
       // End   add 06/05/14
          Else;
             scBilStRpD = ' ';
          Endif;

          rrnBillDiv =  rrnBillDiv + 1;
          Write sflBillDiv;

       If scTrfAlDiv = ' ';
             Reade keyWMEMLQ PEM00q;
          Else;
             Reade key1WMEMLQ PEM00q;
          EndIf;

       EndDo;

       rrnBillDiv = 1;

      /end-free
     p LoadBillDiv     E
       //===================================================================
       // Load Agency Grid
       //===================================================================
     p LoadAgencyInfo  B
     d LoadAgencyInfo  PI
      *
     d agentName       s             40
     d prodName        s             40
     d agtFein         s              9  0
     d AgtMod          s              3  0
     d xxAGrr          s              9  0
     d xxAArr          s              9  0
     d xxAPrr          s              9  0
      /free


       // clear the subfile.
       clrAgnInfo = *Off;
       dspAgnSub  = *On;
       dspAgnCtl  = *On;

       // Read the file WMAHL for all division if Transfer All Division is 'Y'
       // else read the selected division record.

       If scTrfAlDiv = ' ';
          Setll key2WMAHL pah00;
          Reade key2WMAHL pah00;
       Else;
          Setll keyWMAHL pah00;
          Reade keyWMAHL pah00;
       EndIf;

       Dow not %EOF(wmahl);

         Exsr AgencyGrid;
          If scTrfAlDiv = ' ';
             Reade key2WMAHL pah00;
          Else;
             Reade keyWMAHL pah00;
          EndIf;
       Enddo;

       //*** begin chg ***  12/07/22  *************************************
       //*****Chk_Waqtp();
       Chk_Waqtp(*off);
       //*** end chg *****  12/07/22  *************************************
       If liveAHAGT# <> empAgt#;
       // Read the file WMAHLQ for all division if Transfer All Division is 'Y'
       // else read the selected division record.

          If scTrfAlDiv = ' ';
             Setll keyWmahlq pah00q;
             Reade keyWmahlq pah00q;
          Else;
             Setll key2Wmahlq pah00q;
             Reade key2Wmahlq pah00q;
          EndIf;

          Dow not %EOF(wmahlq);
            quoteMode = 'W';
            Exsr AgencyGrid;
            quoteMode = 'Q';
             If scTrfAlDiv = ' ';
                Reade keyWmahlq pah00q;
             Else;
                Reade key2Wmahlq pah00q;
             EndIf;
          Enddo;
       EndIf;

       //  Position exclude grid at top at all times.
       rrnAgnInfo = 1;

       //===================================================================
       // Write Agency Informaion
       //===================================================================
       BegSr AgencyGrid;

          scAgnDiv   =  AHdiv;
          scAgnEffDt =  dtMDYY(AHEfdt);

          Clear agentName;
          agentName  = GetAgtName(AHAgt#:xxAGrr:xxAArr:AHFnd:AHCo#);
          scAgnNm    =  %editc(AHAgt# : 'X') + ' - ' + agentName;

          Clear prodName;
          agtFein    = GetAgtFein(AHCo#:AHFnd:AHAgt#:agtMod);
          ProdName   = GetProdNm(agtFein:agtMod:AHProd:xxAPrr:AHCo#);
          scAgnProd  =  %editc(AHProd : 'X') + ' - ' + prodName;

          Clear scAgnRate;
          xxrate =  AHrt#;
          Setgt keyWMARL par00;
          Readpe key1WMARL par00;
          Dow not %EOF(wmarl) and ARstat <> 'A';
             Readpe key1WMARL par00;
          Enddo;
          scAgnRate  =  ARnwrt;

          Chain keyWMAAL2 paa01;
          If %Found(wmaal1);
            scAgnDfRt = AAdrt#;
          EndIf;

          If AHStat = 'A';
             scAgnSts = 'Active';
          Else;
             scAgnSts = 'Inactive';
          Endif;

          Clear scAgnQSts;
          Setgt  keyWAQTL7 PQT07;
          Readpe keyWAQTL7 PQT07;
          If not %eof;
             If QTtype = 'T' and QTxFer <> *blanks;
                QTtype = QTxFer;
             Endif;
             Clear QSused;
             Clear QSdesc;
             Chain (##co:QTType) pqs01;
             If QSused <> *blank;
                Chain (scCo#:QSused) pqs01;
             Endif;
            scAgnQSts = QSdesc;
            If QTstat = 'Q';
               scAgnQtLv = 'Quote';
            Else;
               scAgnQtLv = 'Live';
            Endif;

          Endif;

          rrnAgnInfo =  rrnAgnInfo + 1;
          Write sflAgnInfo;
       Endsr;
      /end-free
     p LoadAgencyInfo  E
     *====================================================================
     * Load_Sfl - Gets records for subfile
     *====================================================================
     p Load_Sfl        b
     d Load_Sfl        pi
    
     c                   eval      xxerror = *off
     C***  Begin Add  ***  02/08/18  ************************************
     C                   Eval      isRenewal = EMrqFl = 'Y'
     C*
     C                   If        isRenewal
     C***  End   Add  ***  02/08/18  ************************************
      *
      *  Do not check master level records for accounts that are renewed
      *
     c                   MOVE      *BLANKS       CMDST1
     c     LIBNAM        IFNE      *BLANKS
     c                   MOVEL(P)  QCA(1)        CMDST1
     c                   CAT(P)    LIBNAM:0      CMDST1
     c                   CAT(P)    QCA(2):0      CMDST1
     c                   Z-ADD     55            LENGTH
     c                   CALL      'QCMDEXC'
     c                   PARM                    CMDST1           55
     c                   PARM                    LENGTH           15 5
     c                   OPEN      WMEML3
      *
     c     keyWMEML3     CHAIN     WMEML3                             70
      *
     c                   MOVEL(P)  QCA(3)        CMDST1
     c                   Z-ADD     55            LENGTH
     c                   CALL      'QCMDEXC'
     c                   PARM                    CMDST1           55
     c                   PARM                    LENGTH           15 5
     c                   CLOSE     WMEML3
     c                   ENDIF
     c*
     C***  Begin Chg  ***  02/08/18  ************************************
     c**********         if        *IN70  =  '1'
     c                   Endif
     c*
     c                   If        not isRenewal
     C***  End   Chg  ***  02/08/18  ************************************
     *
     c                   select
     c                   when      EMMPFL = 'P'
     c                              and (EMPAD1 = *blank or EMPCTY = *blank
     c                                or EMPST  = *blank or EMPZIP = *zeros)
     c                   eval      xxerror = *on
     c                   when      EMMPFL = 'M'
     c                              and (EMMAD1 = *blank or EMMCTY = *blank
     c                                or EMMST  = *blank or EMMZIP = *zeros)
     c                   eval      xxerror = *on
     c                   when      EMMPFL <> 'P' and EMMPFL <> 'M'
     c                   eval      xxerror = *on
     c                   endsl
     *
     c                   if        xxerror = *on
     c                   eval      scQTrfE = address
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
     *
     c                   if        EMFEIN = *zeros and EXFIFG = 'Y'
     c                   eval      scQTrfE = fed_id_#
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
     *
     c                   select
     c                   when      ClientID = '081'
     c                   if        EMCUST = *blanks and EXFIFG = 'Y'
     c                   eval      scQTrfE = customerID
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
     c                   endsl
     *
     c                   if        EMSIC = *blanks and EXSICR = 'Y'
     c                   eval      scQTrfE = sic_code + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
      *
      * Check the State Risk ID# and NCCI Risk ID# if flags are set for
      * these to be required.
      *
     c                   movel(p)  emsemp        XXRISK
     c                   If        emsemp = *blanks AND (EXSRRB = 'Y' OR
     c                             RRSRRB= 'Y') OR
     c                             XXRISK = TZEROS AND (EXSRRB = 'Y'  OR
     c                             RRSRRB= 'Y')
     c                   EVAL      scQTrfE = State_Risk
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   Endif
      *
     c                   movel(p)  emrsk#        XXRISK
     c                   If        EMRSK# = *zeros and (EXNRRB = 'Y' OR
     c                             RRNRRB='Y') OR
     c                             xxrisk = TZEROS and (EXNRRB = 'Y' OR
     c                             RRNRRB='Y')
     c                   EVAL      scQTrfE = NCCI_Risk
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   Endif
     *
     c                   eval      xxtbid = 'BUSI'
     c                   eval      xxkey  = EMTPOB
     c     keyWTGTL      chain     pgt01                              60
      *
     c                   if        *in60 = *on
     c                   eval      scQTrfE = type_busi + 'employer.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
     *
     c                   if        EMCNTY = *blanks
     c                   eval      scQTrfE = county
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
      *
     c                   if        EMRGN# = *zeros
     c                   eval      scQTrfE = region
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
      *
     c                   if        RRURFL = 'Y'
     c                              and EMUE# = *blanks
     c                   eval      scQTrfE = unemployment
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
      *
     c                   if        RRNAIC = 'Y'
     c                              and EMNAIC = *blanks
     c                   eval      scQTrfE = naics_code + '.'
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 1
     c                   callp     error
     c                   endif
      *
     c                   if        GCFRCN <> *zero and GCTOCN <> *zero
     c     key1WAQOL     setll     waqol
     c                   dou       %eof
     c     key1WAQOL     reade(n)  waqol
     c                   eval      GCNACN = GCNACN + 1
     c                   enddo
     c     keyWAQOL      chain     waqol
      *
     c                   if        GCNACN < GCFRCN
     c                              or GCNACN > GCTOCN
     c                   eval      scQTrfE = carrier
     c                   eval      scQTEtoltp = scQTrfE
     c                   eval      scIndex   = 5
     c                   callp     error
     c                   endif
     c                   endif

     c                   callp     Chk_Wmalp

     c                   endif

     c*** begin chg ***  12/07/22  *************************************
     c*****              callp     Chk_Waqtp
       Chk_Waqtp(*on);
     c*** end chg *****  12/07/22  *************************************

     c                   callp     Chk_Wdelp

     c                   callp     Chk_Cx

      *** beg add ***** 05/04/21 **************************
       Chk_Eft();
      *** end add ***** 05/04/21 **************************

     p Load_Sfl        E
       //===================================================================
       // Load Quote Transfer Error
       //===================================================================
     p LoadTransError  B
     d LoadTransError  PI
      /Free

       // Clear the subfile.
       clrTErr    = *Off;
       dspTErrSub = *On;
       dspTErrCtl = *On;

       // Load Error Grid for all division if Transfer All Division is 'Y' else
       // load error only for selected division.

       If scTrfAlDiv = ' ';
          Validate();
       Else;
         Setll key1WMEMLQ pem00q;
         Reade key1WMEMLQ pem00q;
         Dow Not %Eof(Wmemlq);
            ##div = Emdiv;
            Validate();
         Reade key1WMEMLQ pem00q;
         EndDo;
       EndIf;
       // Begin Add 06/05/14
       //*** begin chg ***  12/07/23  ****************************************
       //*****If IsMultDivFlg and not StatRptDiv;
       If IsMultDivFlg and not StatRptDiv and not liveSTRPDiv;
       //*** end chg *****  12/07/23  ****************************************
          scQTrfE     = StateDiv;
          scQTEtoltp  = scQTrfE;
          scIndex     = 1;
          error();
       Endif;
       // End Add 06/05/14
       svrrnTErr = rrnTErr;
       rrnTErr = 1;
      /End-Free
     p LoadTransError  E
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            B
     d Main            PI

      /free

       // Load action bar
       LoadActionBar();

       // Process until exit button is pressed.
       Dou ExitNow = *On or xxResponse = *On or xxRespErr = *On or
           btnCancel = *On;

          // Check if new division or policy year has been selected.
          If kkDiv <> ##Div;
             SetGt key1WDELL PEL00;
             Readpe keyWDELL PEL00;
             If not %eof(WDELL);
                kkfyr = ELFyr;
             Endif;
             xxRRN = rrnwdell;
             LoadActionBar();
             LoadSelection();
          Endif;

          If kkFyr <> ##Fyr or kkDiv <> ##Div;
             ##fyr = kkFyr;
             ##div = kkDiv;
          Endif;

          // Clear Sub-files
          ClrSubfiles();

          // Load All Billing Division
          LoadBillDiv();

          // Load Agency Contact Information
          LoadAgencyInfo();

          // Load Quote Transfer Error
          LoadTransError();

          // Defaulted value on Transfer Type radio button read the file WMEXL

          Chain keyWMEXL pex00;
          If %Found(WMEXL);
          // Immediate level
             intEXqtil  = EXqtil;
             charEXqtit = EXqtit;
          // Batch Level
             intEXqtbl  = EXqtbl;
             charEXqtbt = EXqtbt;

             If EXtrfl = 'I';
                scTrfgrp = 'I';
             ElseIf EXtrfl = 'B';
                scTrfgrp = 'B';
             EndIf;

          EndIf;

          // Check User Security Level/Type
          flgofEnv = 'Q';

          // Get user sec Level/Type
          userLevel = %Editc(GetUsrLvl(Q1user:userType):'X');

         // Check user authority for 'Transfer Quote To Live Immediate'
          CheckSecurity(userType:userLevel:charEXqtit:%Editc(intEXqtil:'X'):
                        '1':isAuthImd:' ':' ':' ':' ':' ');

         // If User has no authority than 'Transfer Quote To Live Immediate' will
         // be disbaled.
          If isAuthImd <> '0';
             inquiry_I = *On;
          Endif;

         // Check user authority for 'Transfer Quote To Batch'
          CheckSecurity(userType:userLevel:charEXqtbt:%Editc(intEXqtbl:'X'):
                        '1':isAuthBat:' ':' ':' ':' ':' ');

         // If User has no authority than 'Transfer Quote To Live Batch' will
         // be disbaled.
          If isAuthBat <> '0';
             inquiry_B = *On;
          Endif;

         // In group setup if default is Batch and security setup is done for
         // Immediate only, than Batch option should be disabled.
          If scTrfgrp = 'B';
             If (intEXqtbl = 0) And  (intEXqtil <> 0);
             inquiry_B = *On;
             EndIf;
          EndIf;

         // In Work w Group/Admin Darts if Quote Transfer to Live Transfer type is
         // set to default to Immediate and 'Transfer Quote to Live Batch' is set
         // then we select Transfer Quote the screen should present with the 'Batch'
         // as the default.
            If scTrfgrp = 'I';
               If (intEXqtbl <> 0);
               scTrfgrp = 'B';
               EndIf;
            EndIf;

          // Process policy summary screen.
          Exsr ExfmtScreens;

         // Call the Quote Review Program on Edits of Quote Transfer Error Grid
          ProcErrMsg();

         Select;

        //*** Beg  Add ***** 10/28/19 ***************
         When hasWaiverError = *On And btnCancel = *Off And btnRefresh = *Off
          and btnSubmit = *Off;
            hasWaiverError = *Off;
            Clear scURL;
            strPopUpMsg = 'Waivers created in Connect must be edited before ' +
             'transferring to live. Press OK after editing to refresh this ' +
             'screen.';
            btnRefresh = PopOneBtn(strPopUpMsg:'Waivers Error':'OK');
        //*** End Add ***** 10/28/19 ***************

         When btnSubmit = *On;
            btnSubmit = *Off;
            Submit();

         When btnCancel = *On;
         @@next = *blanks;


         When xxRespErr = *On;
         @@next = 'WTPOLTRACK';

       //*** begin add ***  01/07/16  ****************************************
         When btnShowWin = *on;
            PopupWindow(txtShowWin:scActRRN:'PS':txtWinData);
            btnShowWin = *off;
       //*** end add *****  01/07/16  ****************************************

         // do nothing - catch tab click event
         When tabClick = *on;
            tabClick = *Off;

         When btnRefresh = *On;
            btnRefresh =*Off;

          // Menu Choices
          When scSelOpt <> *blanks;
            @@next = scSelOpt;
            Leave;

          Other;

          //* Popup Windows
          Popup();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 1050;
          APnlHeight = TabHeight + 50;
          Exsr WriteAction;
          Write rcdTabPan;
          Exfmt rcdQteTfr;
          Exsr ReadAction;
          Read rcdTabPan;
       EndSl;

       Endsr;

       //===================================================================
       // Write action panel;
       //===================================================================
       BegSr WriteAction;

       If qteMode;
          Write rcdQtePnl;
       Else;
          Write rcdPolPnl;
       Endif;

       Endsr;
       //===================================================================
       // Read action panel;
       //===================================================================
       BegSr ReadAction;

       If qteMode;
          Read rcdQtePnl;
       Else;
          Read rcdPolPnl;
       Endif;

       Endsr;

      /end-free
     p Main            E
      *====================================================================
      * MassRun - Determine if this is a mass run
      *====================================================================
     pMassRun          b                   export
     dMassRun          pi             1n

     d inStack         s              1n

     c                   callp     CheckCallStack('W8718R':inStack)

     c                   return    inStack

     pMassRun          e
     *====================================================================
     * Print - Write records for printing the exception report
     *====================================================================
    p Print           B
    d Print           PI
     *
     c                   if        prtOpened = *off
     c                   open      qsysprt
     c                   callp     Routing
     c                   eval      prtOpened = *on
     c                   except    prtHeader
     c                   endif
      *
     c                   if        *inOF = *on
     c                   except    prtHeader
     c                   eval      *inOF = *off
     c                   endif
      *
     c                   if        prtEmpFlg = *on
     c                   eval      prtEmpFlg = *off
     c                   eval      prtEmpNm = EMNAME
     c                   except    prtEmplr
     c                   endif
      *
     c                   except    prtDetail
      *
     p Print           E
     *====================================================================
     * Routing -  Processing routing for QSYSPRT
     *====================================================================
    p Routing         B
    d Routing         PI
      *
     c     keyWMUPL      chain     wmupl
      *
     c                   if        %found(wmupl)
     c                   eval      xxQue = UPOUTQ
     c                   eval      *in09 = *on
     c                   else
     c                   eval      *in08 = *on
     c                   endif

     c                   open      smorl
     c                   eval      xxPgm = 'W8718R'
     c                   eval      xxPrt = '0'
     c     keySMORL      chain     smorl
      *
     c                   if        ORTITL = *blanks
     c                   eval      ORTITL = title
     c                   endif
      *
     c                   eval      ORTITL = leftJstify(ORTITL)
      *
     c                   if        %found(smorl)
     c                   endif
      *
     c                   close     smorl
      *
     c                   eval      prtTitle = centerFld(ORTITL:%size(prtTitle))
      *
     p Routing         E
      *=====================================================================
      * ProgramStart - Initial Program tasks
      *=====================================================================
     p ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // Chain to Employer Detail for Program.
       Chain xxRrn PEL0P;
       scActRrn=xxRrn;

       // Process only if valid employer detail record found.
       If not %found;
          processme = *Off;
          Return;
       Endif;

       TabIndex = 0;
       APnlHeight = 1100;

       ##co    = ELco#;
       ##Fnd   = ELfnd;
       ##Emp   = ELemp#;
       ##Div   = ELdiv;
       ##Fyr   = ELfyr;
       saveFyr = ELfyr;
       saveEffDt = ELpfdt;
       ##fyr#  = *hival;

       kkDiv = ELdiv;
       kkFyr = ELfyr;

       //*** Begin Add *** 05/30/24 ****************************************
       // Let's see if Invoice Cloud is Installed
       isInvCloud    = isModEnabled('Invoice Cloud');

       // Let's get the Invoice Cloud Header
       isGroupIncluded = ICGroupOpt(ELco#:ELfnd);
       //*** End   Add *** 05/30/24 ****************************************

       LoadSelection();

       processMe = *on;
       Return;

      /end-free
     p ProgramStart    E
      *=====================================================================
      * ProcErrMsg - Process Other Options
      *=====================================================================
     p ProcErrMsg      B
     d ProcErrMsg      PI
      /free

        ReadC sflTErr;

       // processing for Edit mode
        Select;
       //*** Beg Add ***** 10/28/19 *************************
        When scTErrEdt = *On and hasWaiverError = *On;
           scTErrEdt = *Off;
       //*** End Add ***** 10/28/19 *************************
        When scTErrEdt = *On and scCallPgm = 'WTPOLTRACK';
           scTErrEdt = *Off;
           charELrrn   = %EditC(scQTrfRrn:'X');
           scAgnQteErr = 'The Agency Quote Status has an invalid type' +
                          ' for transfer or was not found. Please update' +
                          ' Policy Tracking/Quote Status information in order' +
                          ' to proceed with Quote Transfer for all Billing' +
                          ' Division in the quote being transferred.';
           xxRespErr = PopOneBtn(scAgnQteErr:'Agency Quote Status Error':'OK');
        When scTErrEdt = *on;
           scTErrEdt = *off;
           inELrrn   = %EditC(scQTrfRrn:'X');
           inMode    =  'C';
           inNext    = ' ' ;
           inTabInd  = %EditC(scIndex:'X');
           inQte     = 'Q';
           procErrMsgEdt(inELrrn:inMode:inNext:inTabInd:inQte);
        EndSl;

      /end-free
     p ProcErrMsg      E
     *====================================================================
     * VALIDATE - Subroutine for checking if emp has already been requested
     *====================================================================
    p Validate        B
    d Validate        PI

     c     keyWAQOL      chain(n)  pqo01                              99
     c                   eval      isErr = *in99
      *
     c                   if        *in99 = *on
     c                   callp     Chk_Emplr
     c                   callp     ChkAgtDftRt
     c                   endif
      *
     c                   if        xxError = *off and isMassRun
     c                   callp     ChkAgtDftRt
     c                   endif

     p Validate        E
      *===================================================================
      * Submit - Immediate Transfer or Batch Transfer
      *===================================================================
     p Submit          B
     d Submit          Pi

      /Free

       isSubErr = *Off;

       // Edit to ensure that Immediate or Batch Transfer is On
       //***  Begin Add  ***  08/27/15  ***********************************
       msTrfGrp = *blanks;
       erTrfGrp = *off;
       //***  End   Add  ***  08/27/15  ***********************************
       If scTrfgrp = ' ' And isSubErr = *Off;
          //***  Begin Chg  ***  08/27/15  ********************************
          //PopOneBtn('Immediate or Batch Transfer Button must +
          //be selected.':'Error':'OK');
          //isSubErr = *On;
          strBtnMsg   = 'Transfer Type must be selected.';
          strBtnTitle = 'Error';
          strBtnResponse = PopThreeBtn(strBtnMsg:strBtnTitle:'Cancel':
           'Immediate':'Batch');
          Select;
          When strBtnResponse = '1';
           //msTrfGrp = 'Transfer Type must be selected.';
           //erTrfGrp = *on;
             isSubErr = *on;
          When strBtnResponse = '0';
             scTrfGrp = 'I';
          When strBtnResponse = ' ';
             scTrfGrp = 'B';
          Endsl;
          //***  End   Chg  ***  08/27/15  ********************************
       EndIf;

       // Edit to ensure no Quote Transfer Error exsit in Grid.
       If svrrnTErr > 0 And isSubErr = *Off;
          PopOneBtn('Quote Transfer Error Exist so not allowed '+
          'to Quote Transfer.':'Error':'OK');
          isSubErr = *On;
       EndIf;

       // Check user authority for Immediate Quote Transfer
       If isSubErr = *Off;
          If scTrfgrp = 'I';
             CheckSecurity(userType:userLevel:charEXqtit:%Editc(intEXqtil:'X'):
                           '1':isAuthority:' ':' ':' ':' ':' ');
             charUserAuth = Q1user + 'is not authorized for Immediate ' +
                             'Quote Transfer.';
          ElseIf scTrfgrp = 'B';
             CheckSecurity(userType:userLevel:charEXqtbt:%Editc(intEXqtbl:'X'):
                           '1':isAuthority:' ':' ':' ':' ':' ');
             charUserAuth = Q1user + 'is not authorized for Batch ' +
                             'Quote Transfer.';
          EndIf;

          If isAuthority <> '0';
              PopOneBtn(charUserAuth:'Error':'OK');
             isSubErr = *On;
          Endif;
       EndIf;

       //*** Begin Add *** 05/30/24 ****************************************
       //
       // Initiate popup if there is an unpaid invoice.
       If isInvCloud And isGroupIncluded;
          // Check to see if invoice has been paid or not
          Exec Sql
             Select count(*)
             Into :paidInvCnt
             From WDEAP_HLD
             Where EAco#= :ELco# And EAfnd= :ELfnd And
                   EAemp#= :ELemp# And EAdiv= :ELdiv And
                   EAeaTp='IP';

          If paidInvCnt = 0;
             strBtnMsg   = 'Please note that the employer''s name will ' +
                           'be removed from Invoice Cloud as the quote ' +
                           'payment was not submitted.  Please run the ' +
                           'estimated billing to create the live policy # ' +
                           'and invoices at Invoice Cloud.';

             strBtnTitle = 'Error';
             strBtnResponse = PopTwoBtn(strBtnMsg:strBtnTitle:
               'Submit':'Cancel');
             Select;
             When strBtnResponse = *Off;
                isSubErr = *On;
             When strBtnResponse = *On;
             EndSL;
          EndIf;
       EndIf;
       //*** End   Add *** 05/30/24 ****************************************

       If isSubErr = *Off;

       // Quote Transfer Confirmation window - Immediate
          If scTrfgrp = 'I';
             scQTrnfCf = 'Quote ' + %Editc(##fnd:'X') + '/' +
                         %Editc(##emp:'X') + ' will be transferred ' +
                         'to Live Immediately.';
       // Quote Transfer Confirmation window - Batch
          ElseIf scTrfgrp = 'B';
             scQTrnfCf = 'Quote ' + %Editc(##fnd:'X') + '/' +
                         %Editc(##emp:'X') + ' will be transferred ' +
                         'to Live from Transfer Pending to Live or Daily Batch';
          EndIf;

        //***  Begin Chg  ***  08/27/15  **********************************
        //xxResponse = PopTwoBtn(scQTrnfCf:'Error':'OK':'CANCEL');
          xxResponse = PopTwoBtn(scQTrnfCf:'Finalize Transfer':'OK':'Cancel');
        //***  End   Chg  ***  08/27/15  **********************************

          If xxResponse = *On and scTrfgrp = 'I';

         // Transfer all division is checked
            If scTrfAlDiv = ' ';
               Trnsfr();
            Else;
               Setll key1WMEMLQ pem00q;
               Reade key1WMEMLQ pem00q;
               Dow Not %Eof(Wmemlq);
                  ##div = Emdiv;
                  Trnsfr();
               Reade key1WMEMLQ pem00q;
               EndDo;
            EndIf;

             charCo    = %EditC(##Co:'X');
             charDate  = %EditC((Dateto8(systemdate)):'X');
             charQue = GetUsrOutq(q1User);
             charFnd   = %EditC(##Fnd:'X');
             charEmp   = %EditC(##Emp:'X');
             QuoteTransfer(charCo:charDate:charQue:charFnd:charEmp);

          ElseIf xxResponse = *On And scTrfgrp = 'B';
         // Transfer all division is checked
            If scTrfAlDiv = ' ';
               Trnsfr();
            Else;
               Setll key1WMEMLQ pem00q;
               Reade key1WMEMLQ pem00q;
               Dow Not %Eof(Wmemlq);
                  ##div = Emdiv;
                  Trnsfr();
               Reade key1WMEMLQ pem00q;
               EndDo;
            EndIf;

          ElseIf xxResponse = *Off;

          EndIf;

       EndIf;

      /End-free

     pSubmit           e
     *====================================================================
     * TRNSFR - SUBROUTINE FOR ADDING THE EMPLOYER TO BE TRANSFERED
     *====================================================================
     p trnsfr          B
     d trnsfr          Pi
    
     c     keyWMFNL      chain     pfn00                              60
     c     keyWMEMLQ     chain     pem00q

      /Free
       // ChkAgtDftRt();
      /End-Free

     c                   select

      *  If "Renew to Quote" load the current emp# to the 'Active Emp#'
     c                   when      EMRQFL = 'Y'

     c                   eval      QOTEMP = ##emp
    
     *  IF AUTO ASSIGN FLAG IS NOT 'Y', PROMPT FOR EMPLOYER #
    
     c                   when      fnaute <> 'Y'
    
     c                   eval      scAssGroup = ##fnd
     c                   eval      scAssQtEmp = ##emp
     c                   eval      scAssQtDiv = ##div
     c                   eval      scAssTDiv  = ##div
     c                   clear                   scAssTEmp
    
     c                   move      *On           *In90
     c                   dou       (*In90 = *Off)
     c                             Or BtnAssCan = *On
     c                   move      *off          *in90
     c                   Exfmt     RcdEmpNmAs
     c                   callp     Errckw
     c                   enddo
    
     c                   if        BtnAssCan = *On
     c                   move      *On           *inlr
     c                   return
     c                   endif                                                  *INKL=*ON
    
     c                   call      'WA005R'
     c                   parm      scAssTEmp     zzemp#
     c                   parm      scAssQtDiv    zzdiv
     c                   parm                    zzname
     c                   parm                    xxedn
    
     c                   movel(p)  xxedn         xxed
     c     xemp1         cat(p)    xxed:1        scqte1
     c                   cat       xfer:1        scqte1
     c                   movel(p)  scqte1        ##data
    c
     c                   call      'SRCENTER'
     c                   parm                    ##data
     c                   parm      50            ##leng
     c                   movel(p)  ##data        scqte1
     *
     c                   call      'WA005R'
     c                   parm      scAssQtEmp    zzemp#
     c                   parm      scAssQtDiv    zzdiv
     c                   parm                    zzname
     c                   parm                    xxedn
     *
     c                   movel(p)  xxedn         xxed
     c     xemp2         cat(p)    xxed:1        scqte2
     c                   movel(p)  scqte2        ##data
     *
     c                   call      'SRCENTER'
     c                   parm                    ##data
     c                   parm      50            ##leng
     c                   movel(p)  ##data        scqte2
    
      /Free
       scqte = scqte1 + scqte2;
       //***  Begin Chg  ***  08/27/15  **********************************
       //PopOneBtn(scqte:'Auto Assign Quote Number Transfer':'OK');
       PopOneBtn(scqte:'Manual Assign Quote Number Transfer':'OK');
       //***  End   Chg  ***  08/27/15  **********************************
      /End-Free
     c                   eval      qotemp = scAssTEmp
     c                   endsl                                                  FNAUTE <> Y
     *
     c                   eval      qoco#  = ##co
     c                   eval      qofnd  = ##fnd
     c                   eval      qoemp# = ##emp
     c                   eval      qodiv  = ##div
     c                   movel     q1user        qouser
     c                   movel     q1pgm         qopgm
     *
     c                   time                    dsdttm
     c                   eval      srdti = dsdate
     c                   movel     dstime        qotime
      /Free
       qodt   = dtCYMD(srdti);
    
       // Check Employer is already assigned in Quote Transfer File on Batch Transfer.
       // If transferring the same Employer in Immediate then then we need to
       // update the same Employer in Quote Transfer File,
      // Else write the new record.
       // Note : For Not Auto Assign Employer we skip the update process because
       // that is already handle in Errckw().
    
      Chain keyWAQOL pqo01;
      If %Found(Waqol) And scAssTEmp = 0;
         If scTrfgrp = 'I';
            QOitfr = 'Y';
         Else;
            QOitfr = ' ';
          EndIf;
          qodt   = dtCYMD(srdti);
          qotime = dstime;
         Update pqo01;
    
      Else;
         If scTrfgrp = 'I';
            QOitfr = 'Y';
         Else;
            QOitfr = ' ';
          EndIf;
          Unlock waqol;
          If scAssTEmp <> 0;
             qotemp = scAssTEmp;
          EndIf;
         Write pqo01;
      Endif;
    
      /End-Free
     ptrnsfr           e
     *====================================================================
      * utility procedure definitions
      /COPY SCOPYPOLPI
**  QCAEXEC TABLE
OVRDBF FILE(WMEML3) TOFILE(
/WMEML3)
DLTOVR FILE(WMEML3)
OVRDBF FILE(WMEML) TOFILE(
/WMEML)
DLTOVR FILE(WMEML)
