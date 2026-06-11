     hCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h/Title W8393R - PEO Policy Import
     h Option(*nodebugio:*srcstmt)
     h BndDir('WTBNDDIR')
      //*************************************************************************
      //  Description:  This program reads the flat file WXPMP, edits the records
      //                error report or a successful upload verication report and
      //                classification report. This program can process one header
      //                record in each flat file only.
      //
      //  Called from:  W8393C - Override
      //  Calls      :  None
      //*************************************************************************
      //  Created on 04/12/2005 by c:62 for Project 135WCD
      //*************************************************************************
      // Revisions:
      //
      // 10/16/25 C:65 Support: Phw#119562 (Amwins)
      //               Populate transaction date/time when deletion
      //               occurs on an additional location.
      //
      // 06/18/25 C:291 Support: 119238
      //                Clear Report as Peo field for Additional divisions add.
      //
      // 04/28/25 C:300 Support: 118916, Project: WC2965
      //                Changes as per review comments
      //
      // 03/31/25 C:344 Support: 118686 (SCS)
      //                Retrieve 'Bill On Effective Date Flag' based on the
      //                new method for getting the state reporting record.
      //
      // 01/09/25 C:344 Support: 118289 (SCS); Xref : 118183
      //                PEO Policy Import - Division Cancellations
      //
      // 12/12/24 C:333 Support: 118183; Xref : 109150
      //                PEO Policy Import - Division Cancellations
      //
      // 12/03/24 C:342 Support:117908 (SUNZ)
      //                1.Add processing to load correct emp# in wdcxp and wdxhl
      //                file to avoid looping issue over the same emp# and div.
      //                This looping issue occurred when MCP PEO policy with
      //                new emp# and div 0 is added to system.
      //                2.Add edit when MCP policy eff date >= PEO Master Cx Date
      //                2.Added No 72 CTDATA at bottom for error message.
      //
      // 08/05/24 C:65  Project: WC2951
      //                Add processing to default the experience rating in WDELP.
      //                Default report to NCCI as 'Y' for '1' or '2' exp rating.
      //
      // 05/23/24 C:300 Support: 115634 (SUNZ)
      //                Update WMFNP Maintenance fields while updating
      //                Next Available Employer Number
      //
      // 04/29/24 C:340 Project: WC2965
      //                Add processing to allow email address to be imported and
      //                edited for population when required.
      //                Add Error Message # 71 on CT Array.
      //
      // 01/02/24 C:344 Support: 114748 (Vital Services)
      //                Change condition to include the status of SIC Code.
      //
      // 12/18/23 C:291 Support: 113826
      //                Assign WAQTP/QTtype same as State Reporting Division QTType.
      //
      // 10/05/23 C:333 Project: 150WFZ
      //                PEO Import Enhancement
      //
      // 09/18/23 C:342 Support: 113918/153
      //                update the WRITEWMAHP procedure to use the earliest
      //                ELPFDT on the policy.
      //
      // 09/15/23 C:291 Support: 113826
      //                Add Cancel Detail/History records to new added Division
      //                when PEO policy is under non-Renewal cancellation.
      //
      // 09/01/23 C:340 Ticket: 111666 (Vital)
      //                Clear Expense Constant carried from PEO
      //
      // 01/06/23 C:65  Ticket: 105837 (Sunz)
      //                Clear the renew to quote flag on imported policies as they
      //                are new business and not renewed to quote.
      //
      // 07/22/22 C:062 Ticket: 100473(150)
      //                Use eff_date instead of system date for endorsements
      //
      // 05/20/22 C:136 Ticket: 102001(150)
      //                Clear ELFAF.
      //
      // 02/04/22 C:136 Support: 97947
      //                MCP's should  be flagged as new business
      //
      // 11/15/21 C:136 Project: 150WFM
      //                Add quote processing
      //
      // 10/16/21 C:136 Project: 150WFJ
      //                Update employer link when emp and div populated in
      //                spreadsheet.
      //
      // 08/24/21 C:291 Project: WC2812
      //                Add DefaultRatingFactors procedure
      //
      // 04/20/21 C:136 Project: 150WEX (Removed and replaced by WC2812)
      //                Do not let PEO factors carry over to MCP policy
      //
      // 04/07/20 C:295 Support: 86489
      //                Add an edit that ensures PEO policy upload not to default
      //                non-specific address flag.
      //
      // 09/20/19 C:298 Support: 78713
      //                Add an edit that ensures PEO policy upload
      //                should not allow a CX date outside of the policy range.
      //
      // 07/15/19 C:136 Support: 80900
      //                Fix edit that ensures mailing address state HAS
      //                to match the rating state.
      //
      // 09/27/18 C:136 Support: 73571
      //                Initialize dlt_dt7 to prevent carrying values from
      //                previous record and throwing an edit on a transaction
      //                that isn't being deleted.
      //
      // 07/03/18 C:136 Support: 71746
      //                Correct logic editing policies with delete dates
      //
      // 07/13/18 C:291 Project: 148WAV
      //                Default deviations on add.
      //
      // 05/14/18 C:136 Support: 70313
      //                Default Exp Rating Flag to '2' for Sunz
      //
      // 04/03/18 C:33  Project: 150WCF
      //                Large Deductible default changes
      //
      // 03/06/18 C:136 Support: 64681
      //                Correct writeWMALP so that locations are written to
      //                correct division
      //
      // 02/21/18 C:136 Project: WC2588
      //                Clear quote transfer date
      //
      // 02/06/18 C:136 Project: 150WAD
      //                Add logic to write excluded states records
      //
      // 01/17/18 C:136 Project: 150WCJ
      //                Default PEO Named Insured from WDRRP.RRNMINDFT
      //                for MCP policies
      //
      // 01/10/18 C:136 Support: 63616
      //               -Populate increased limits effective date
      //
      // 08/02/17 C:136 Support: 150WAM
      //                Sunz customizations
      //                 Add Customer ID
      //                 Add Carrier #
      //                 Adjust various edits
      //                 Allow re-use of link code
      //                 Add ability to process midterm deletion of division
      //                  or location
      //
      // 06/27/17 C:136 Support: 46720
      //                A new division can't have a different ARD than the
      //                state reporting division.
      //
      // 06/14/17 C:136 Support: 47681
      //               -Write to LDA so that downstream processes
      //                know that PEO midterm adds are being generated
      //                by the import instead of Quote Entry
      //               -Discontinue use of eRunType. Midterm endorsements
      //                will always be generated if policy page has
      //                been run
      //
      // 05/12/17 C:136 Project: 148WAC
      //                Allow locations to be added to MCP policies
      //
      // 02/20/17 C:136 Project: 148WAC (AmWins PEO Customization)
      //               -Added fields required in Location Master to
      //                process
      //
      // 02/01/17 C:136 Project: WC2479
      //               -Add functionality to:
      //                - write New Business record
      //                - use Effective Date field in spreadsheet
      //               -Convert to free form
      //
      // 08/05/15 C:307 Project: SY354
      //                -Adding 'Route To:'
      //
      // 08/26/14 C:307 Project: SY354
      //                -Implement Routing Page Changes.
      //
      // 07/20/10 C:220 Project: WC1928
      //                - Changed the Employer Master Office Number
      //                loading logic.
      //
      // 01/28/10 C:249 Project: SY215
      //          C:189 Retrieve brand Name
      //                                                               *
      // 07/16/07 C:062 SUPPORT: WA135 59125
      //                Do not populate ELILEX
      //
      // 05/10/07 C:136 SUPPORT: WA135 58323
      //                Correct eff dates for Inc Limits and Ded Discount
      //
      // 03/21/07 C:183 Project: 135WHS
      //                Add processing to call W0051C31 for each division
      //                loaded.
      //
      // 12/06/06 C:61  Project: WC1444
      //                Call WA200R to create "Carrier Policy #" WMRPP record
      //
      // 08/01/06 C:33  Project: 135WFD    WA135/52667
      //              - Write the emp# to WAQTP.
      //              - Select the appropriate policy year for member imports.
      //
      // 02/24/06 C:168 Project: 135WFD
      //              - If the Effective Date in file WXPMP is not empty
      //                (signaling an 'A' Run Type) then get this efective
      //                date and move it to WMEMP.EMEFD1 and WDELP.ELPFDT
      //              - For client 135 don't validate the 'type of bulding'
      //              - Change the report heading according to the 'Run Type'
      //
      // 11/03/05 C:136 Support
      //                Replace EMPEOL with EMLINK.
      //
      // 08/20/05 c:62 Project 135WCD
      //               Change the default on the increased limits code.
      //*************************************************************************
     fwxpmp     if   e             disk    infds(dbWxpmp) rename(ppm01:ppm0p)
      // PMelid, Select PMrect = 'D'
     fwxpml     if   e           k disk    rename(ppm01:ppm00)
     fwxpml1    if   e           k disk    rename(ppm01:ppm01)
     fsmorl     if   e           k disk
     fstabl     if   e           k disk
     fwaqtp     o    e           k disk
     fwmahp     o    e           k disk
      *** beg chg ***** 11/15/21 ****************
     f***wmahl     if   e           k disk    rename(pah01:pah02)
     fwmahl     if   e           k disk    rename(pah01:pah02) usropn
      *** end chg ***** 11/15/21 ****************
     fwmall     uf a e           k disk    rename(pal01:pal00)
     fwmcmlc    if   e           k disk    rename(pcm01:pcm0c)
     fwmczl     if   e           k disk
     fwdelp     o    e           k disk    rename(pel01:pel0p)
      *** Beg Add ***** 08/24/21 ***************************************
     f                                     infds(elfds)
      *** End Add ***** 08/24/21 ***************************************
      *** beg chg ***** 11/15/21 ****************
     f***wdell1    if   e           k disk    rename(pel01:pel01)
     f***wdell2    if   e           k disk    rename(pel01:pel02)
     fwdell1    if   e           k disk    rename(pel01:pel01) usropn
     fwdell2    if   e           k disk    rename(pel01:pel02) usropn
      *** end chg ***** 11/15/21 ****************
     fwdldl1    uf a e           k disk    rename(pld01:pld01)
     fwdrrl     if   e           k disk    rename(prr01:prr00)
     fwdvhp     o    e           k disk
     fwdvhl     if   e           k disk    rename(pvh01:pvh02)
     fwdxsl     if a e           k disk    rename(pxs01:pxs00)
     fwmemp     o    e             disk
      *** beg chg ***** 10/16/21 **************************
     f***wmeml     if   e           k disk    rename(pem01:pem02)
      *** beg chg ***** 11/15/21 ****************
     f***wmeml     uf   e           k disk    rename(pem01:pem02)
      *** end chg ***** 10/16/21 **************************
     f***wmeml18   if   e           k disk    rename(pem01:pem18)
     fwmeml     uf   e           k disk    rename(pem01:pem02) usropn
     fwmeml18   if   e           k disk    rename(pem01:pem18) usropn
      *** end chg ***** 11/15/21 ****************
     fwmemlq    if   e           k disk    prefix(q) rename(pem01:pem0q)
     fwmexl     if   e           k disk    rename(pex01:pex00)
     fwmfnl     uf   e           k disk
     fwmgcl     if   e           k disk    rename(pgc01:pgc00)
     fwmldl3    if a e           k disk    rename(pld01:pld03)
     fwmldl2    if   e           k disk    rename(pld01:pld02)
     fwmnbp     o    e           k disk    rename(pnb01:pnb0p)
     fwmrpl     if a e           k disk    rename(prp01:prp00)
     fwmrsl     if   e           k disk
     fwmshl1    if   e           k disk
     fwtabl     if   e           k disk    include(pbs01) rename(pbs01:pbs00)
     fwtddl2    if   e           k disk    rename(pdd01:pdd02)
     fwtgtl     if   e           k disk
     fwtlml3    if   e           k disk    rename(plm01:plm03)
     fwttbl     if   e           k disk
      ***** Beg Add ***** 09/15/23 ***************************
     fwdcxl     if a e           k disk    rename(pcx01:pcx00)
     fwdxhl     if a e           k disk    rename(pxh01:pxh00)
      ***** End Add ***** 09/15/23 ***************************
      ***** Beg Add ***** 03/31/25 ***************************
     fwdell     if   e           k disk    rename(pel01:pel03)
     f                                     prefix(C_)
      ***** End Add ***** 03/31/25 ***************************
     fqsysprt   o    f  210        printer oflind(*inoa)
     fqsysprt1  o    f  105        printer oflind(*inob)
      //=========================================================================
      // Prototypes
      //=========================================================================
      *Entry
     d W8393R          pr
     d  eFnd                          3
     d  eQue                         10
     d  eUpld                         1
     d  eRpdt                         7
     d  eRunType                      1
     d W8393R          pi
     d  eFnd                          3
     d  eQue                         10
     d  eUpld                         1
     d  eRpdt                         7
     d  eRunType                      1
     d AutoCrtLoc      pr
     d BadDataCheck    pr
     d CheckDuplicate  pr              n
     d DetailEdit      pr
     d ErrorCheckData  pr
     d ErrorCheckFile  pr
     d FutClaimsExist  pr              n
     d  locID                        10a
     d GetCarrCode     pr                  like(GCdcar)
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
     d GetCounty       pr             2a
     d  state                         2a
     d  zipCode                       5s 0 const
     d GetDivision     pr              n
     d  retCode                       1s 0 Options(*Nopass)
     d GetNextEmp#     pr             9s 0
     d GetNextDiv      pr             5s 0
     d   employer#                    9p 0
     d GetNumOfHead    pr             6s 0
     d GetNumOfTrail   pr             6s 0
     d GetPeoType      pr             1a
     d GetRateSetCode  pr             3a
     d  rateState                     2a
     d  effDate7                      7s 0
     d GetRecordCnt    pr             9s 0
     d InvalidChars    pr              n
     d  valueToCheck                 11a   const
     d  nameOfField                  25a   const
     d  isReqired                      n   const
     d LocationExists  pr              n
     d  isUpdateTrans                  n   const
     d PolPgNotRun     pr              n
     d PrintError      pr
     d  errorNumber                   3s 0 value
     d  errorFlag                      n   value
     d PrintUpload     pr
     d ProcessWXPMP    pr
     d ProgramStart    pr
       //*** beg add ***** 11/15/21 **************
       Dcl-Pr RtvEnv ExtPgm('RTVENV');
        rtnEnv Char(4);
       End-Pr;
       Dcl-Pr RtvQteLib ExtPgm('RTVQTELIB');
        rtnEnv Char(4) Const;
        rtnLiveLib Char(10);
       End-Pr;
       Dcl-Pr QcmdExc ExtPgm('QCMDEXC');
        command Char(255) Const;
        length Packed(15:5) Const;
       End-Pr;
       //*** end add ***** 11/15/21 **************
     d SbmMidTermAdd   pr                  Extpgm('W0051C31')
     d  #1co#                         3    Const
     d  #1fnd                         3    Const
     d  #1emp#                        9    Const
     d  #1div                         5    Const
     d  #1endt                        8    Const
     d  #1locId                      10    Const
     d SbmMidTermDlt   pr                  Extpgm('W0051C32')
     d  #1co#                         3    Const
     d  #1fnd                         3    Const
     d  #1emp#                        9    Const
     d  #1div                         5    Const
     d  #1endt                        8    Const
     d  #1locId                      10    Const
     d SetBadData      pr
     d  prBadData                      n   Const
     d UpdateWmalp     pr
     d WA200R          pr                  Extpgm('WA200R')
     d  co#                           3  0 Const
     d  fnd                           3  0 Const
     d  emp#                          9  0 Const
     d  div                           5  0 Const
     d  fyr                           3  0 Const
     d  pgm                          10    Const
     d WrtMidTermAdd   pr
     d  DivOrLocFlag                  1    Const
     d WrtMidTermDlt   pr
     d  DivOrLocFlag                  1    Const
     d WriteQUOTE      pr
     d WriteWDELP      pr
     d WriteWDLDP      pr
     d WriteWDVHP      pr
     d WriteWDXSP      pr
     d WriteWMAHP      pr
     d WriteWMALP      pr
     d WriteWMLDP      pr
     d WriteWMEMP      pr
     d WriteWMNBP      pr
     d WriteWMRPP      pr
     d/copy SPrRtvBrnd
     d/copy SprDate
     d/copy SprCompany
     d/copy SprString
     d/copy WprGroup
     d/copy wPrEmplr
     d/copy WprUsrPrms
      //=========================================================================
      // Stand alone fields
      //=========================================================================
     d charRowMsg      s             60
     d charDivMsg      s             60
     d charLocMsg      s             60
     d clientID        s              3    inz(*blanks)
     d companyNm       s             60
     d empCount        s              6  0 inz(*zero)
     d errorCount      s              6  0 inz(*zero)
     d intRowCount     s              6  0 inz(*zero)
     d intLocCount     s              6  0 inz(*zero)
     d intDivCount     s              6  0 inz(*zero)
     d message         s             60
     d recCount        s              6  0 inz(*zero)
     d recType         s              1
     d userName        s             50
     d headerCount     s              6  0 inz(*zero)
     d trailerCount    s              6  0 inz(*zero)
     d badData         s              1n   inz(*off)
     d headerFirst     s              1n   inz(*off)
     d trailerLast     s              1n   inz(*off)
     d errorFlag       s              1n   inz(*off)
     d hasError        s              1n   inz(*off)
     d sys_date        s               d   datfmt(*usa)
     d sysDateCymd     s              7s 0
     d sysDateUsa      s              8s 0
     d sys_time        s               t   timfmt(*hms:)
     d sysTimeHms      s              6s 0
     d reportDate      s              7s 0
     d company         s                   like(ELCO#)
     d kkco#           s                   like(ELCO#)
     d kkfnd           s                   like(ELFND)
     d kkemp#          s                   like(ELEMP#)
     d kkdiv           s                   like(ELDIV)
     d kkfyr           s                   like(ELFYR)
     d kkrtcd          s                   like(ELRTCD)
     d kkefdt          s                   like(ELPFDT)
     d kkpfdt          s                   like(ELPFDT)
     d kkst            s                   like(CZST)
     d kkzip           s                   like(CZZIP)
     d k1emp#          s                   like(ELEMP#)
     d k1div           s                   like(ELDIV)
     d xxque           s                   like(eQue)
     d group#          s                   like(ELFND)
     d ORTITL1         s                   like(ORTITL)
     d ORTITL2         s                   like(ORTITL)
     d xxfil#          s                   like(ORFIL#)
     d UPUSNM          s                   like(userName)
     d kkkey           s                   like(stkey)
     d kktbid          s                   like(gttbid)
     d kktbcd          s                   like(tbtbcd)
     d kkpad1          s                   like(empad1)
     d kkpcty          s                   like(empcty)
     d kkpst           s                   like(empst)
     d isoEfDt         s               d   datfmt(*iso)
     d ann_Dt7         s              7s 0
     d dlt_Dt7         s              7s 0
     d dlt_Dt8         s              8s 0
     d eff_Dt7         s              7s 0
     d*** Begin Add *** 07/22/22 *******************************************
     d eff_Dt8         s              8s 0
     d*** End   Add *** 07/22/22 *******************************************
     d*** Beg   Add *** 09/18/23 *******************************************
     d intDiv          s                   like(eldiv)
     d intFrDt         s                   like(elpfdt)
     d*** End   Add *** 09/18/23 *******************************************
     d line            s            199
     d line1           s            100
     d loc_Dt7         s              7  0
     d recordType      s              7
     d errReason       s             60
     d rateStError     s               n
     d isRequired      s               n
     d isNotRequired   s               n   Inz(*On)
     d intRowNumber    s              7  0
     d svArSm          s                   like(ELarsm)
       Dcl-S indPeoNotInQuote Ind;
       Dcl-S indPeoNotInLive Ind;
       Dcl-S isQteImp Ind;
       Dcl-S strCommand Char(255);
       Dcl-S strEnv Char(4) Inz('*LIV');
       Dcl-S strLiveLib Char(10);
       //*** End Add ***** 11/15/21 **************
      //*** Beg Add ***** 12/18/23 ********************************
       Dcl-S  strStRptQTtype Like(QTtype);
      //*** End Add ***** 12/18/23 ********************************
      *** Beg Add ***  04/29/24  *** WC2965 *************************
       Dcl-S EmailErr Ind;
       Dcl-S NoStrpEmail Ind;
     d xxdate          s              7s 0
      *** End Add ***  04/29/24  *** WC2965 *************************
      **** Beg Add **** 03/31/25 *******************************************
     d inEmp#          s                   like(ELEMP#)
     d inDiv           s                   like(ELDIV)
      **** End Add **** 03/31/25 *******************************************
      //=========================================================================
      // Constants
      //=========================================================================
     d title           c                   'PEO Master Upload Error Report'
     d title1          c                   'PEO Master Upload Report'
     d mtitle          c                   'Additional PEO Member Error Report'
     d mtitle1         c                   'Additional PEO Member Upload Report'
     d message1        c                   '*** No errors to report. ***'
     d message2        c                   '*** No records exist in WXPMP ***'
     d message3        c                   '*** Blank record ***'
     d message4        c                   '*** Header/trailer is missing ***'
     d message5        c                   '*** Duplicate records ***'
     d message6        c                   '*** Multiple headers ***'
     d message7        c                   '*** Multiple trailers ***'
     d message8        c                   '*** No header record ***'
     d message9        c                   '*** No trailer record ***'
     d message10       c                   '*** No match on record count ***'
     d#brnd1           s             80    inz
     d#brnd2           s             80    inz
      //=========================================================================
      // Arrays
      //=========================================================================
      *** Beg Chg ***  04/29/24  *** WC2965 *************************
     d*** err             s             60    dim(70) ctdata perrcd(1)
      *** Beg Chg ***  12/03/24  *** WC2965 *************************
     d*** err             s             60    dim(71) ctdata perrcd(1)
     d err             s             60    dim(72) ctdata perrcd(1)
      *** End Chg ***  12/03/24  *** WC2965 *************************
      *** Beg Chg ***  04/29/24  *** WC2965 *************************
      *** Beg Add ***** 08/24/21 ***************************************
     d errRatingDft    s             60    dim(10) ctdata perrcd(1)
     d strHeading      s             60
      *** End Add ***** 08/24/21 ***************************************
      //=========================================================================
      // Data structures
      //=========================================================================
     d svlda           ds                  dtaara(*lda)
     d  ldPeoImpFlg                   1    overlay(svlda:29)
     d dbWxpmp         ds
     d  rrnWxpmp             397    400b 0
      *** Beg Add ***** 08/24/21 *****************************************
     d elFds           ds
     d  rrnWdelp             397    400b 0
     d dsNewWDELP      ds                  likerec(pel01:*input) inz
      *** beg chg ***** 11/15/21 *****************************************
     d*** dsPeoWDELP      ds                  likerec(pel01:*input) inz
     d dsPeoWDELP      ds                  likerec(pel02:*input) inz
      *** end chg ***** 11/15/21 *****************************************
      *** End Add ***** 08/24/21 *****************************************
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)

     d header          ds
     d  Head_Rec_Type                 1    overlay(header)
     d  Rec_Count                     9    overlay(header:2)

     dDetail           ds
     d Det_rec_Type                   1    overlay(detail)
     d Det_Rec_Count                  9    overlay(detail:2)
     d Peo_Link_Code                 15    overlay(detail:11)
     d Rating_State                   2    overlay(detail:26)
     d Dba_Name                     100    overlay(detail:28)
     d Fed_Name                     100    overlay(detail:128)
     d M_Addr_Line_1                 40    overlay(detail:228)
     d M_Addr_Line_2                 40    overlay(detail:268)
     d M_Addr_City                   25    overlay(detail:308)
     d M_Addr_State                   2    overlay(detail:333)
     d M_Addr_Zip                     9    overlay(detail:335)
     d M_Addr_Zip5                    5    overlay(M_Addr_Zip)
     d P_Addr_Line_1                 40    overlay(detail:344)
     d P_Addr_Line_2                 40    overlay(detail:384)
     d P_Addr_City                   25    overlay(detail:424)
     d P_Addr_State                   2    overlay(detail:449)
     d P_Addr_Zip                     9    overlay(detail:451)
     d P_Addr_Zip5                    5    overlay(P_Addr_Zip)
     d Phone                         10    overlay(detail:460)
     d Fax                           10    overlay(detail:470)
     d Fed_Tax_ID                     9    overlay(detail:480)
     d Type_of_Bus                    1    overlay(detail:489)
     d SIC_Code                       6    overlay(detail:490)
     d Type_of_Bld                    3    overlay(detail:496)
     d Eff_Date                       8    overlay(detail:499)
     d Pol_Ann_Date                   8    overlay(detail:507)
     d NAICS_Code                    10    overlay(detail:515)
     d #OfEmployees                   7    overlay(detail:525)
     d DbaPerNmFlg                    1    overlay(detail:532)
     d FedPerNmFlg                    1    overlay(detail:533)
     d Emp_LSID                       1    overlay(detail:534)
     d State_Unemp#                  20    overlay(detail:535)
     d State_RiskID                  15    overlay(detail:555)
     d NCCI_RiskID                   15    overlay(detail:570)
     d IncLimitCode                   2    overlay(detail:585)
     d SchedDbCr                      5    overlay(detail:587)
     d Dev1Factor                     5    overlay(detail:592)
     d Dev2Factor                     5    overlay(detail:597)
     d ExpModFactor                   5    overlay(detail:602)
     d Loc_ID                        10    overlay(detail:607)
     d Loc_Year                       3    overlay(detail:617)
     d Loc_YrTtlPay                  13    overlay(detail:620)
     d Loc_Bldg_Yr                    4    overlay(detail:633)
     d Loc_#OfStories                 3    overlay(detail:637)
     d Loc_Peak_Emp                   7    overlay(detail:640)
     d Exp_Rate_Flg                   1    overlay(detail:647)
     d Carr_Pol#                     25    overlay(detail:648)
     d Customer_ID                   20    overlay(detail:673)
     d Deletion_Date                  8    overlay(detail:693)
      *** beg add ***** 10/16/21 *****************************
     d Employer#                      9    overlay(detail:701)
      *** Chg Add ***  04/29/24  *** WC2965 *************************
     d*** Division#                      5    overlay(detail:706)
     d Division#                      5    overlay(detail:710)
      *** Chg End ***  04/29/24  *** WC2965 *************************
      *** end add ***** 10/16/21 *****************************
      *** Beg Add ***** 10/05/23 *****************************
     d OvrEmployer#                   9    overlay(detail:715)
      *** End Add ***** 10/05/23 *****************************
      *** Beg Add ***  04/29/24  *** WC2965 *************************
     d EmailAddress                  60    overlay(detail:724)
      *** End Add ***  04/29/24  *** WC2965 *************************

     d trailer         ds
     d  Trail_Rec_Typ                 1    overlay(trailer)

     d pmdata        e ds                  extname(wxpmp)
     d psds          esds                  extname(szq1p)
      *** Beg Add ***  12/03/24  ************************************
     d cxDate          s                   like(emcad1)
      *** End Add ***  12/03/24  ************************************
      //========================================================
      // KLISTS
      //========================================================
     c     ahky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr

     c     keyWDELL2     klist
     c                   kfld                    kkCo#
     c                   kfld                    kkFnd
     c                   kfld                    kkEmp#
     c                   kfld                    kkDiv
     c                   kfld                    kkPfDt

     c     key1WDELL2    klist
     c                   kfld                    kkCo#
     c                   kfld                    kkFnd
     c                   kfld                    kkEmp#
     c                   kfld                    kkDiv

     c     emky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv

     c     emky2         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    k1emp#
     c                   kfld                    k1div

     c     emky3         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    k1emp#

     c     fnky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     gtky          klist
     c                   kfld                    kkco#
     c                   kfld                    kktbid
     c                   kfld                    kkkey

     c     orky          klist
     c                   kfld                    q1pgm
     c                   kfld                    xxfil#

     c     stky          klist
     c                   kfld                    kkkey

     c     tbky          klist
     c                   kfld                    kkco#
     c                   kfld                    kktbcd

     c     vhky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
      //========================================================
      // Main Processing
      //========================================================
      /Free

       ProgramStart();
       ErrorCheckFile();

       If errorCount = *zero;
          message = message1;
          printError(0:*off);
          processWXPMP();
          printUpload();
          eUpld = 'Y';
          eRpDt = %Editc(sysDateCymd:'X');
       Else;
          eUpld = 'N';
       Endif;

       If hasError;
          xxfil# = '0';
          Chain orky por01;
          If %Found;
             Except route;
          Endif;
       Else;
          xxfil# = '1';
          Chain orky por01;
          If %Found;
             Except route1;
          Endif;
       Endif;

       *Inlr = *on;

      /End-Free
      *==========================================================
      * Output files
      *==========================================================
     o/copy scopyrout

     oqsysprt   e            route          1

     O          e            route          1 45
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     //
     // Report Heading
     //
     o          e            head           1 06
     o                       sys_time          +002
     o                       companyNm          135
     o                       sys_date           201
     o          e            head           2
     o                       q1pgm             +002
     o                       ORTITL             135
     o                                          195 'PAGE:'
     o                       page          z    201
     o          e            head           0
     o                                         +002 'Type   '
     o                                         +002 'Row    '
     o                                         +002 'Link Code      '
     o                                         +002 'Loc ID    '
     o                                         +002 'State'
     o                                         +002 'Error reason'
     o          e            head           2
     o                       line              +002
     o          e            det            1
     o               20      recordType        +002
     o               20      intRowNumber      +002
     o               20      PMpeol            +002
     o               20      PMelid            +002
     o               20      PMrst             +002
     o               20      errReason         +005
     o              n20      message            174
     o/copy scopyrout1

     oqsysprt1  e            route1         1

     o          e            route1         1 59
     o                       #brnd1            +002
     o          e            route1         1
     o                       #brnd2            +002
     //
     // Report Heading
     //
     o          e            head1          1 06
     o                       sys_time          +002
     o                       companyNm           88
     o                       sys_date           105
     o          e            head1          2
     o                       q1pgm             +002
     o                       ORTITL1             88
     o                                          100 'PAGE:'
     o                       page1         z    105
     o          e            head1          2
     o                       line1             +002
     o          e            det1           1
     o                       charRowMsg        +005
     o          e            det1           1
     o                       charDivMsg        +005
     o          e            det1           1
     o                       charLocMsg        +005
      //----------------------------------------------------------------
      // AutoCrtLoc - Auto create location record
      //----------------------------------------------------------------
     p AutoCrtLoc      b
     d AutoCrtLoc      pi

      /Free
       // Return if not flagged in groups to auto create
       If FNaclr <> 'L';
          Return;
       Endif;
       ALco#  = EMco#;
       ALfnd  = EMfnd;
       ALemp# = EMemp#;
       ALdiv  = EMdiv;
       ALelid = '0000000001';
       ALeltp = FNaclr;
       ALefdt = Emefd1;
       ALname = EMname;
       ALnam2 = EMnam2;
       ALfdnm = EMfdnm;
       ALfdn2 = EMfdn2;
       ALmad1 = EMmad1;
       ALmad2 = EMmad2;
       ALmcty = EMmcty;
       ALmst  = EMmst;
       ALmzip = EMmzip;
       ALpad1 = EMpad1;
       ALpad2 = EMpad2;
       ALpcty = EMpcty;
       ALpst  = EMpst;
       ALpzip = EMpzip;
       ALtpob = EMtpob;
       ALfein = EMfein;
       ALarea = EMarea;
       ALphn# = EMphn#;
       ALfara = EMfaxa;
       ALfax# = EMfax#;
       ALcnty = EMcnty;
       ALsemp = EMsemp;
       ALsic  = EMsic;
       ALnaic = EMnaic;
       ALue#  = EMue#;
       ALnaob = EMnaob;
       ALshft = EMshft;
       ALnamp = EMnamp;
       ALfdnp = EMfdnp;
       ALncci = 'N';
       ALstat = 'A';
       ALIPgm = Q1PGM;
       ALPgm  = Q1PGM;
       ALIUsr = Q1USER;
       ALUser = Q1USER;
       ALITm  = sysTimeHms;
       ALTime = ALitm;
       ALIDt  = sysDateCymd;
       ALDt   = ALidt;
       Write pal00;
       Feod Wmall;
       Loc_Year = %Editc(ELfyr:'X');
       WriteWDLDP();
      /End-free

     p AutoCrtLoc      e
      //------------------------------------------------------------------------*
      // BadDataCheck - checks for bad data and passes back inidicator
      //------------------------------------------------------------------------*
     p BadDataCheck    b
     d BadDataCheck    pi

      /Free
       // Check for bad numers, record count and invalid type
       Select;
       When PMRecT = 'H';
          isRequired = *On;
          SetBadData(InvalidChars(PMrCnt:'Record Count':isRequired));
          If badData = *On;
             Return;
          Endif;
          If %Int(PMrCnt) <> getRecordCnt;
             badData = *On;
             message = 'Incorrect record count';
             PrintError(0:*off);
          Endif;
       When PMrecT = 'D' And PMemp# <> *Blanks;
          isRequired = *On;
          SetBadData(InvalidChars(PMemp#:'Employer#':isRequired));
          SetBadData(InvalidChars(PMdiv:'Division#':isRequired));
       When PMrecT = 'D' and PMexdt = *Blanks;
          isRequired = *On;
          isNotRequired = *Off;
          SetBadData(InvalidChars(PMmZip:'M ZipCode':isNotRequired));
          SetBadData(InvalidChars(PMpZip:'P ZipCode':isNotRequired));
          SetBadData(InvalidChars(PMphon:'Phone #':isNotRequired));
          SetBadData(InvalidChars(PMfax :'Fax #':isNotRequired));
          SetBadData(InvalidChars(PMfein:'Fed Tax ID':isRequired));
          SetBadData(InvalidChars(PM#ofe:'# Of Employees':isRequired));
          SetBadData(InvalidChars(PMandt:'Ann Rating Date':isNotRequired));
          SetBadData(InvalidChars(PMdbcr:'Debit/Credit Factor':isNotRequired));
          SetBadData(InvalidChars(PMasp2:'Dev 1 Factor':isNotRequired));
          SetBadData(InvalidChars(PMdev2:'Dev 2 Factor':isNotRequired));
          SetBadData(InvalidChars(PMexm1:'Exp Mod Factor':isNotRequired));
          SetBadData(InvalidChars(PMexdt:'Deletion Date':isNotRequired));
          If PMelid = *blanks;
             isRequired = *Off;
          Else;
             isRequired = *On;
          Endif;
          SetBadData(InvalidChars(PMelyr:'Location Year':isRequired));
          SetBadData(InvalidChars(PMtpyr:'Loc Year Total Payroll':
           isNotRequired));
          SetBadData(InvalidChars(PMBuildYr:'Bld Construction Year'
           :isNotRequired));
          SetBadData(InvalidChars(PM#Stories:'Bld # Of Stories'
           :isNotRequired));
          SetBadData(InvalidChars(PMPeakEmp:'Peak # Employees'
           :isNotRequired));
       When PMrecT = 'D' and PMexdt <> *Blanks;
          isRequired = *On;
          SetBadData(InvalidChars(PMefdt:'Eff Date':isRequired));
          SetBadData(InvalidChars(PMexdt:'Deletion Date':isRequired));
       When PMrecT = 'T';
       Other;
          badDATA = *on;
          PrintError(2:*on);
       Endsl;
      /End-Free

     p BadDataCheck    e
      //------------------------------------------------------------------------*
      // CheckDuplicate - checks for duplicates
      //------------------------------------------------------------------------*
     p CheckDuplicate  B
     d CheckDuplicate  PI              n

     d elid            s             10a
     d peol            s             15a
     d rst             s              2a

      /Free
       //*** beg chg ***** 10/16/21 **************
       //*** Exec Sql
       //***  Declare Project Cursor For
       //***  Select PMelid, PMpeol, PMrst
       //***  From Wxpmp
       //***  Where PMrect = 'D'
       //***  Group By PMelid, PMpeol, PMrst
       //***  Having Count(*) > 1
       //***  Order By PMelid, PMpeol, PMrst;
       Exec Sql
        Declare Project Cursor For
        Select PMelid, PMpeol, PMrst
        From Wxpmp
        Where PMrect = 'D' And PMemp# = ' '
        Group By PMelid, PMpeol, PMrst
        Having Count(*) > 1
        Order By PMelid, PMpeol, PMrst;
       //*** end chg ***** 10/16/21 **************
       Exec Sql Open Project;
       Exec Sql Fetch Next From Project Into :elid, :peol, :rst;
       Dow sqlStt = '00000';
          Select;
          When Sqlstt <> '00000';
          When elid = *blanks;
             message = 'Duplicate records for link ' +
              %trim(peol) + '/' + rst + '.';
             PrintError(0:*Off);
          When elid <> *blanks;
             message = 'Duplicate records for link ' +
              %trim(peol) + '/' + rst + ' for '  + ' location '
              + %trim(elid) + '.';
             PrintError(0:*Off);
          Endsl;
          Exec Sql Fetch Next From Project Into :elid, :peol, :rst;
       Enddo;
       Exec Sql Close Project;
       If message <> *blanks;
          Return *On;
       Else;
          Return *Off;
       Endif;
      /End-Free

     p CheckDuplicate  E
      //----------------------------------------------------------------
      // detailEdit - edit detail record
      //----------------------------------------------------------------
     p DetailEdit      b
     d DetailEdit      pi
     d dec52           s              5  2
     d dec54           s              5  4
     d flgPeoType      s              1a
     d indAnnDtErr     s               n
     d indDbCrErr      s               n
     d isLocation      s               n
     d isPolicy        s               n
     d retCode         s              1s 0

      /Free
       //*** beg add ***** 11/15/21 **************
       //
       // Deletion date must be blank for quote import.
       //
       If Deletion_Date <> *blanks And isQteImp = *On;
          PrintError(68:*On);
          Return; // No reason to continue looking for errors
       Endif;
       //*** end add ***** 11/15/21 **************
       //
       // Effective Date must be valid
       //
       Monitor;
          eff_Dt7 = %dec(%char(%date(Eff_Date:*iso0):*cymd0):7:0);
          //*** Begin Add *** 07/22/22 ************************************
          eff_Dt8 = dtMDYY(eff_Dt7);
          //*** End   Add *** 07/22/22 ************************************
       On-Error;
          PrintError(27:*on);
          Return;  // No reason to continue looking for errors
       EndMon;
       //
       // Validate deletion date
       //
       If Deletion_Date <> *blanks;
          Monitor;
             dlt_Dt7 = %dec(%char(%date(Deletion_Date:*iso0):*cymd0):7:0);
          On-Error;
             PrintError(53:*on);
             Return; // Can't proceed with an invalid delete date
          EndMon;
       Else;
          dlt_dt7 = 0;
       Endif;
       Select;
       When dlt_Dt7 = 0;
       When dlt_Dt7 < eff_Dt7;
          PrintError(29:*On);
       Endsl;
       //
       // PEO link code must be populated
       //
       If PEO_Link_Code = *blanks;
          PrintError(3:*on);
       Endif;
       //
       // Rating State must be valid state
       //
       rateStError = *off;
       kkkey = Rating_State;
       Chain stky pst01;
       If Not %Found;
          rateStError = *on;
          PrintError(4:*on);
       Endif;
       //
       // Can only delete divisions in master group
       //
       Select;
       When dlt_Dt7 = 0;
       When rateStError = *On;
          Return;
       When PMelid <> *Blanks;
       When FNpeof = 'M';
          PrintError(55:*On);
          Return;
       When FNpeof = 'C';
          GetRateSetCode(Rating_State:dlt_Dt7);
          If RRpeof <> 'S';
             PrintError(55:*On);
             Return;
          Endif;
       Endsl;
       //*** Beg Add ***** 12/03/24 *********************************
       // If MCP policy having effective date >= to Cx Date of PEO master.
       If dlt_Dt7 = 0 and PMovrEmp# = *Blanks and PMelid = *blanks
          and FNpeof = 'C';
          GetRateSetCode(Rating_State:eff_Dt7);
          If RRpeof = 'M' or RRpeof = 'S';
             cxDate = GetCxDate(kkco#:kkfnd:kkemp#:kkdiv);
             If eff_Dt7 >= cxDate and cxDate <> *zeros;
                PrintError(72:*On);
                Return;
             Endif;
          Endif;
       Endif;
       //*** End Add ***** 12/03/24 *********************************
       //*** Beg Add ***** 10/05/23 *********************************
       If PMovrEmp# <> *Blanks;
          Chain(n) (kkCo#:kkFnd:%Int(PMovrEmp#)) pem0q;
          If Not(%Found);
             PrintError(69:*On);    //** if invalid employer
             Return;
          Endif;
       Endif;
       If PMovrEmp# <> *Blanks;
          kkEmp# = %Int(PMovrEmp#);
       Endif;
       //*** End Add ***** 10/05/23 *********************************
       //
       // Ensure policy can be located for delete transaction
       // Ensure it is not cancelled already.
       // Ensure location exists before allowing delete
       // Ensure that the state reporting division isn't cancelled
       // Ensure the division doesn't have summarized A/R
       // Ensure claims with future accident dates do not exist on
       //  division or location
       // No more edits after this if delete date is populated.
       //
       isPolicy = GetDivision(retCode);
       //*** beg add ***** 11/15/21 ***********************
       // Look for policy in live for quote import if not found
       // in quote for edit 20. Don't look if Loc_ID <> *blank.
       Select;
       When Not(isQteImp);
       When Loc_ID <> *Blanks;
       When isPolicy;
       Other;
          UserFilesClose();
          UserFilesOvrToLive();
          UserFilesOpen();
          isPolicy = GetDivision();
          UserFilesClose();
          UserFilesDltOvr();
          UserFilesOpen();
       EndSl;
       //*** end add ***** 11/15/21 ***********************
       Select;
       When dlt_Dt7 = 0;
       When Not(isPolicy);
          PrintError(57:*On);
          Return;
       When retCode = 1;
          PrintError(58:*On);
          Return;
       Other;
          If PolPgNotRun();
             PrintError(64:*On);
          Endif;
          If EMdiv = 0 and PMelid = *Blanks;
             PrintError(59:*On);
          Endif;
       //***** Beg Chg *** 12/12/24 ***************************
          //***If svArSm = 'Y' and PMelid = *Blanks;
       //***** Beg Chg *** 01/09/25 ***************************
          //***If svArSm = 'Y' and PMelid = *Blanks and clientID <> '150';
          If svArSm = 'Y' and PMelid = *Blanks and clientID <> '150' and
           clientID <> '146';
       //***** End Chg *** 01/09/25 ***************************
       //***** End Chg *** 12/12/24 ***************************
             PrintError(60:*On);
          Endif;
          If FutClaimsExist(PMelid);
             PrintError(61:*On);
          Endif;
          If EMcad1 <> 0;
             PrintError(62:*On);
          Endif;
          If PMelid <> *Blanks;
             isLocation = LocationExists(*Off);
             Select;
             When Not(isLocation);
                 PrintError(63:*On);
             When ALdldt <> 0;
                 PrintError(65:*On);
             Endsl;
          Endif;
          Return;
       Endsl;
       //
       // Employer name can't be blank
       //
       If Dba_Name = *blank;
          PrintError(5:*on);
       Endif;
       //
       // Location ID cannot be zero
       // The location year is required if location ID is populated
       // The member can't have a division that has been previously
       //  written to the Rating_State.
       // The location must have a division that has been
       //  previously written to the Rating_State. Look in
       //  import file before invoking error.
       //
       Select;
       When Loc_ID = *zeros;
          PrintError(43:*on);
       When Loc_ID <> *blanks and (Loc_Year = *blanks or Loc_Year = *zeros);
          PrintError(15:*on);
       When Loc_ID = *blanks and isPolicy;
          PrintError(20:*on);
       When Loc_ID <> *blanks and Not(isPolicy);
          Chain (*blank:PMpeol:PMrst) ppm01;
          If Not(%Found);
             PrintError(42:*On);
          Endif;
       Endsl;
       //
       // Mailing address 1 and Physical Addres
       //  can't be blank
       //
       If M_Addr_Line_1 = *blank and P_Addr_Line_1 = *blank;
          PrintError(7:*on);
       Endif;
       //
       // Verify mailing address if provided
       //
       If M_Addr_Line_1 <> *blank;
          //
          // Mailing city can't be blank
          //
          If M_Addr_City = *Blank;
             PrintError(49:*on);
          Endif;
          //
          // Mailing address state must be valid
          //
          kkkey = M_Addr_State;
          Chain stky pst01;
          If Not %Found;
             PrintError(9:*on);
          Endif;
          //
          // Mailing address zip is required
          //
          Select;
          When M_Addr_Zip = *blank;
             PrintError(8:*on);
          When %Int(M_Addr_Zip) = 0;
             PrintError(10:*on);
          Endsl;
       Endif;

       If P_Addr_Line_1 <> *blank;
          //
          // Physical city can't be blank
          //
          If P_Addr_City = *Blank;
             PrintError(50:*on);
          Endif;
          //
          // Physical address state must be valid
          //
          kkkey = P_Addr_State;
          Chain stky pst01;
          If Not %Found;
             PrintError(13:*on);
          Endif;
          //
          // Physical address zip is required
          //
          Select;
          When P_Addr_Zip = *blank;
             PrintError(12:*on);
          When %Int(P_Addr_Zip) = 0;
             PrintError(14:*on);
          Endsl;
       Endif;
       //
       // FEIN can't be zero
       //
       If %Int(Fed_Tax_ID) = 0;
          PrintError(16:*on);
       Endif;
       //
       // Type of business is required and must be valid
       //
       kktbid = 'BUSI';
       kkkey = Type_of_Bus;
       Chain gtky pgt01;
       If Not %Found;
          PrintError(17:*on);
       Endif;
       //
       // There must be a county for the physical address
       //
       Select;
       When P_Addr_Zip5 = *blanks;
       Other;
          If GetCounty(P_Addr_State:%Int(P_Addr_Zip5)) = *blanks;
             PrintError(21:*on);
          Endif;
       Endsl;
       //
       // There must be a county for the mailing address
       //
       Select;
       When M_Addr_Zip5 = *blanks;
       Other;
          If GetCounty(M_Addr_State:%Int(M_Addr_Zip5)) = *blanks;
             PrintError(31:*on);
          Endif;
       Endsl;
       //
       // Number of employees > 0
       //
       If %Int(#OfEmployees) <= 0;
          PrintError(28:*on);
       Endif;
       //
       // The name flags must be Y or blank
       //
       If DbaPerNmFlg = *blank or DbaPerNmFlg = 'Y';
       Else;
          PrintError(48:*on);
       Endif;
       If FedPerNmFlg = *blank or FedPerNmFlg = 'Y';
       Else;
          PrintError(48:*on);
       Endif;
       //
       // Employee leasing ID must valid if populated.
       //
       Select;
       When Loc_ID <> *blanks;
       When Emp_LSID = *blanks;
       Other;
          kktbid = 'NREL';
          kkkey = Emp_LSID;
          Chain gtky pgt01;
          If Not(%Found);
             PrintError(54:*on);
          Endif;
       Endsl;
       Select;
       When Loc_ID <> *blanks;
       When Exp_Rate_Flg = *blanks;
       Other;
          kktbid = 'EXRT';
          kkkey = Exp_Rate_Flg;
          Chain gtky pgt01;
          If Not(%Found);
             PrintError(56:*on);
          Endif;
       Endsl;
       //
       // Anniversary rating date must be valid
       //
       indAnnDtErr = *Off;
       Select;
       When Loc_ID <> *blanks;
       When Pol_Ann_Date = *blanks or Pol_Ann_Date = *zeros;
       Other;
          Monitor;
             ann_Dt7 = %dec(%char(%date(Pol_Ann_Date:*iso0):*cymd0):7:0);
          On-Error;
             PrintError(32:*on);
             indAnnDtErr = *On;
          EndMon;
          // ARD must be <= PFDT
          If eff_Dt7 <= ann_Dt7 and
           indAnnDtErr = *Off;
             PrintError(33:*on);
          Endif;
       Endsl;
       //*** beg chg ***** 11/15/21 *************
       //kkPfDt = eff_Dt7;
       //Setgt keyWDELL2 pel02;
       //Readpe key1WDELL2 pel02;
       //If %Eof or ELptdt < eff_Dt7;
       //   PrintError(23:*on);
       //   Return;
       //Endif;
       //kkfyr = ELfyr;
       //Chain ahky pah02;
       //If Not %Found;
       //   PrintError(24:*on);
       //Endif;
       //
       // The PEO must have a WDELP and WMAHP record
       Select;
       When isQteImp = *On;
         // Look in quote first
          indPeoNotInQuote = *Off;
          indPeoNotInLive  = *Off;
          kkPfDt = eff_Dt7;
          Setgt keyWDELL2 pel02;
          Readpe key1WDELL2 pel02;
          If %Eof or ELptdt < eff_Dt7;
             indPeoNotInQuote = *On;
          Else;
             kkfyr = ELfyr;
             Chain ahky pah02;
             If Not %Found;
                PrintError(24:*on);
             Endif;
          Endif;
         // Override to live if not found in quote
          If indPeoNotInQuote = *On;
             UserFilesClose();
             UserFilesOvrToLive();
             UserFilesOpen();
             kkPfDt = eff_Dt7;
             Setgt keyWDELL2 pel02;
             Readpe key1WDELL2 pel02;
             If %Eof or ELptdt < eff_Dt7;
                indPeoNotInLive = *On;
             Else;
                kkfyr = ELfyr;
                Chain ahky pah02;
                If Not %Found;
                   PrintError(24:*on);
                Endif;
             Endif;
             UserFilesClose();
             UserFilesDltOvr();
             UserFilesOpen();
          Endif;
          If indPeoNotInQuote = *On
           And indPeoNotInLive = *On;
             PrintError(23:*on);
             Return;
          EndIf;
       Other;
          kkPfDt = eff_Dt7;
          Setgt keyWDELL2 pel02;
          Readpe key1WDELL2 pel02;
          If %Eof or ELptdt < eff_Dt7;
             PrintError(23:*on);
             Return;
          Else;
             kkfyr = ELfyr;
             Chain ahky pah02;
             If Not %Found;
                PrintError(24:*on);
             Endif;
          Endif;
       Endsl;
       //*** end chg ***** 11/15/21 *************
       //
       // Increased Limits is required if master policy has one
       //
       Select;
       When Loc_ID <> *Blanks;
       //*** beg add ***** 04/20/21 *************
       When GetPeoType = 'M';
       //*** end add ***** 04/20/21 *************
       When InclimitCode = *blanks and ELilcd <> *blanks;
          PrintError(6:*on);
       Endsl;
       //
       // Validate Increased Limits
       //
       If InclimitCode <> *blanks;
          Setgt  (kkCo#:kkFnd:Rating_State:IncLimitCode:eff_Dt7) plm03;
          Readpe (kkCo#:kkFnd:Rating_State:IncLimitCode) plm03;
          If %Eof;
             Callp PrintError(34:*on);
          Endif;
       Endif;
       //
       // Validate rate set code set up
       //
       If Not(rateStError) and GetRateSetCode(Rating_State:eff_Dt7) = *blanks;
          Callp PrintError(22:*on);
          Return; // No reason to keep checking
       Endif;
       //
       // Verify that a PEO type is assigned to the policy
       //
       flgPeoType = GetPeoType();
       If flgPeoType <> 'S' and flgPeoType <> 'M';
          PrintError(66:*on);
       Endif;
       //
       // The ARD must be the same across all divisions.
       // Currently, only 'S' type PEO's can have multiple
       // divisions.
       //
       Select;
       When Loc_ID <> *blanks;
       When flgPeoType <> 'S';
       When ELandt <> ann_Dt7;
          PrintError(52:*on);
       Endsl;
       //
       // Address state must equal rating state
       //
       Select;
       When P_Addr_State <> *blanks and P_Addr_State <> Rating_State;
          PrintError(51:*On);
       When M_Addr_State <> *blanks and M_Addr_State <> Rating_State
        and P_Addr_State = *Blanks;
          PrintError(30:*On);
       Endsl;
       //
       // SIC code must be valid
       //
       Select;
       //*** Beg Chg ***** 01/02/2024 **** PHW114748 **********************
       //***When EXsicr <> 'Y';
       When EXsicr <> 'Y' and SIC_Code = *blanks;
       //*** End Chg ***** 01/02/2024 **** PHW114748 ***********************
       When SIC_Code = *blanks and EXsicr = 'Y';
          PrintError(45:*On);
       Other;
          kktbid = 'SIC';
          kkkey = SIC_Code;
          chain gtky pgt01;
          If Not %Found;
             PrintError(18:*on);
          Endif;
       Endsl;
       //
       // NAIC code must be valid
       //
       Select;
       When RRnaic <> 'Y';
       When NAICS_Code = *blanks and RRnaic = 'Y';
          PrintError(46:*On);
       Other;
          kktbid = 'NAIC';
          kkkey = NAICS_Code;
          Chain gtky pgt01;
          If Not %Found;
             PrintError(26:*on);
          Endif;
       Endsl;
       //
       // State Unemployment may be required
       //
       Select;
       When State_Unemp# = *blanks and RRurfl = 'Y';
          PrintError(47:*On);
       Endsl;
       //
       // Validate Sched Debit/Credit
       //
       indDbCrErr = *Off;
       If SchedDbCr <> *blanks;
          If RRcrcd = 0 and RRdbcd = 0;
             Callp PrintError(35:*on);
          Endif;
          Monitor;
             dec54 = %Dec(SchedDbCr:5:4);
          On-Error;
             Callp PrintError(36:*on);
             indDbCrErr = *On;
          Endmon;
          Select;
          When indDbCrErr = *On;
          Other;
             If dec54 < RRcmax or
              (dec54 > RRdmax and RRdmax <> 0);
                Callp PrintError(11:*on);
             Endif;
          Endsl;
       Endif;
       //
       // Validate Dev 1
       //
       If Dev1Factor <> *blanks;
          If RRdv1c = 0;
             Callp PrintError(37:*on);
          Endif;
          Monitor;
             dec52 = %Dec(Dev1Factor:5:2);
          On-Error;
             Callp PrintError(38:*on);
          Endmon;
       Endif;
       //
       // Validate Dev 2
       //
       If Dev2Factor <> *blanks;
          If RRdv2c = 0;
             Callp PrintError(39:*on);
          Endif;
          Monitor;
             dec52 = %Dec(Dev2Factor:5:2);
          On-Error;
             Callp PrintError(40:*on);
          Endmon;
       Endif;
       //
       // Validate Exp Mod Factor
       //
       If ExpModFactor <> *blanks;
          Monitor;
             dec54 = %Dec(ExpModFactor:5:4);
          On-Error;
             Callp PrintError(41:*on);
          Endmon;
       Endif;
       //*** Beg Add ***** 10/05/23 *********************************
       kkEmp# = 1;
       kkDiv = 0;
       //*** End Add ***** 10/05/23 *********************************
       //*** Beg Add ***  04/29/24  *** WC2965 *************************

       EmailErr = *Off;

       If PMovrEmp# <> *Blanks;
          kkEmp# = %Int(PMovrEmp#);
       Endif;

       // Check if State Reporting Div have Email
       NoStrpEmail = *Off;
       SetLL (kkCo#:kkFnd:kkEmp#:0) PEM02;
       ReadE(n) (kkCo#:kkFnd:kkEmp#) PEM02;
       Dow Not %EOF;
           If EMStrp = 'Y' and EMMail = ' ';
              NoStrpEmail = *On;
              Leave;
           EndIf;
           ReadE(n) (kkCo#:kkFnd:kkEmp#) PEM02;
       EndDo;

       xxDate = %int(%Char(%Date() : *CYMD0));

       //*** Beg Chg ***** 04/28/25 ***********************
       // If EMailAddress = ' ';
       If EMailAddress = ' ' and NoStrpEmail = *On;
       //*** End Chg ***** 04/28/25 ***********************

          If M_Addr_State <> ' ' ;
             SetGT (kkCo#:kkFnd:M_Addr_State:xxDate) WDRRL;
             ReadPE (kkCo#:kkFnd:M_Addr_State) WDRRL;
             If Not %EOF and RREmail = 'Y';
                EmailErr = *On;
             EndIf;
          EndIf;
          //*** Beg Chg ***** 04/28/25 ***********************
          // If M_Addr_State <> ' ' ;
          If P_Addr_State <> ' ' ;
          //*** End Chg ***** 04/28/25 ***********************
             SetGT (kkCo#:kkFnd:P_Addr_State:xxDate) WDRRL;
             ReadPE (kkCo#:kkFnd:P_Addr_State) WDRRL;
             If Not %EOF and RREmail = 'Y';
                EmailErr = *On;
             EndIf;
          EndIf;

          If EmailErr = *On;
             PrintError(71:*On);
          EndIf;

       EndIf;

       //*** End Add ***  04/29/24  *** WC2965 *************************
      /End-Free

     p DetailEdit      e
       //*** beg add ***** 10/16/21 ***********************
      //---------------------------------------------------
       Dcl-Proc EmpDivEdit;
      //---------------------------------------------------
        Chain(n) (kkCo#:kkFnd:%Int(PMemp#):%Int(PMdiv)) pem02;
        If Not(%Found);
           Callp PrintError(67:*On);
        Endif;
        //*** Beg Add ***** 10/05/23 ********************************
        If PMemp# <> *Blanks and PMovrEmp# <> *Blanks;
           PrintError(70:*On);
           Return;
        Endif;
        //*** End Add ***** 10/05/23 ********************************
       End-Proc;
       //*** end add ***** 10/16/21 ***********************
      //------------------------------------------------------------------------*
      // ErrorCheckData - error checks fields
      //------------------------------------------------------------------------*
     p ErrorCheckData  B
     d ErrorCheckData  PI
      /Free
       Select;
       When PMRecT = 'H';
          intRowCount += 1;
       When PMRecT = 'D';
          intRowCount += 1;
          Select;
          When PMelid <> *Blanks;
             intLocCount += 1;
          Other;
             intDivCount += 1;
          Endsl;
          detail = PMdata;
       //*** beg chg **** 10/16/21 ****************
       //    DetailEdit();
          If PMemp# = *Blanks;
             DetailEdit();
          Else;
             EmpDivEdit();
          Endif;
       //*** end chg **** 10/16/21 ****************
       When PMRecT = 'T';
          intRowCount += 1;
       Endsl;
      /End-Free
     p ErrorCheckData  E
      //------------------------------------------------------------------------*
      // ErrorCheckFile - Reads thru WXPMP and looks for errors
      //------------------------------------------------------------------------*
     p ErrorCheckFile  B
     d ErrorCheckFile  PI

      /Free
       //
       // Check for empty file
       //
       Setll 1 ppm0p;
       Read ppm0p;
       If %eof;
          message = message2;
          PrintError(0:*off);
          Return;
       Endif;
       //
       // Check for the number of headers
       //
       headerCount = getNumOfHead;
       Select;
       When headerCount = 0;
          message = message8;
          PrintError(0:*off);
          Return;
       When headerCount > 1;
          message = message6;
          printError(0:*off);
          Return;
       Endsl;
       //
       // Check for the number of trailers
       //
       trailerCount = getNumOfTrail;
       Select;
       When trailerCount = 0;
          message = message9;
          PrintError(0:*off);
          Return;
       When trailerCount > 1;
          message = message7;
          PrintError(0:*off);
          Return;
       Endsl;
       //
       // Is the header the first
       //
       Setll 1 ppm0p;
       Read ppm0p;
       If not %eof;
          recType = PMRecT;
          If recType = 'H';
             headerFirst = *on;
          Endif;
       Endif;
       //
       // Is the trailer last
       //
       Setgt *hival ppm0p;
       Readp ppm0p;
       If not %eof;
          recType = PMRecT;
          If recType = 'T';
             trailerLast = *on;
          Endif;
       Endif;

       If headerFirst = *off or
              trailerLast = *off;
          message = message4;
          PrintError(0:*off);
          Return;
       Endif;
       //
       // Check for duplicates
       //
       badData = CheckDuplicate;
       If badData;
          Return;
       Endif;
       //
       // Run through file the first time checking for errors. If error is found,
       // Print to the error report and continue checking the file for other errors.
       // Prevent further processing.
       //
       Setll 1 ppm0p;
       Read ppm0p;
       Dow Not(%Eof);
          BadDataCheck();
          If not badData;
             ErrorCheckData();
          Else;
             badData = *off;
          Endif;
       Read ppm0p;
       Enddo;
      /End-Free

     p ErrorCheckFile  E
      //----------------------------------------------------------------
      // Returns *On if location exists
      //----------------------------------------------------------------
     p FutClaimsExist  b
     d FutClaimsExist  pi              n
     d  locID                        10a

      /Free
       Setll (kkCo#:kkfnd:EMemp#:EMdiv:dlt_dt7) pcm0c;
       Reade (kkCo#:kkfnd:EMemp#:EMdiv) pcm0c;
       Dow Not(%Eof);
       If locID = CMctg1;
          Return *On;
       Endif;
       Reade (kkCo#:kkfnd:EMemp#:EMdiv:dlt_dt7) pcm0c;
       Enddo;
       Return *Off;
      /End-Free

     p FutClaimsExist  e
      //----------------------------------------------------------------
      // Returns carrier code
      //----------------------------------------------------------------
     p GetCarrCode     b
     d GetCarrCode     pi                  like(GCdcar)

      /Free
       Chain (kkCo#:kkFnd) pgc00;
       If %found;
          Return GCdcar;
       Else;
          Return *blanks;
       Endif;
      /End-Free

     p GetCarrCode     e
      //----------------------------------------------------------------
      // GetCounty  - Retrieve the county
      //----------------------------------------------------------------
     p GetCounty       B
     d GetCounty       PI             2a
     d  state                         2a
     d  zipCode                       5s 0 const

      /Free
       Chain (kkCo#:state:zipCode) pcz01;
       If %found;
          Return CZcnty;
       Else;
          Return *blanks;
       Endif;
      /End-Free

     p GetCounty       E
      //----------------------------------------------------------------
      // GetDivision - Return *On if division found for rating state
      //----------------------------------------------------------------
     p GetDivision     b
     d GetDivision     pi              n
     d  retCode                       1s 0 Options(*NoPass)
     d kkfyr           s                   like(ELfyr)
     d indDivFound     s               n
     d svEmp#          s                   like(EMemp#)
     d svDiv           s                   like(EMdiv)
     d svFein          s                   like(EMfein)

      /Free
       svArSm = *Blank;
       If Loc_ID = *blanks or Deletion_Date <> *Blanks;
       // Get year from PEO Master
          kkPfDt = eff_Dt7;
          Setgt  keyWDELL2  pel02;
          Readpe key1WDELL2 pel02;
          kkFyr = ELfyr;
       Else;
       // Use location year
          kkFyr = %Int(Loc_Year);
       Endif;

       // Look for rating state = Rating_State
       Setll (kkCo#:kkFnd:Peo_Link_Code) pem18;
       Reade (kkCo#:kkFnd:Peo_Link_Code) pem18;
       Dow Not(%Eof);
       //*** Beg Add ***** 10/05/23 ********************************
          If OvrEmployer# <> *blanks and EMemp# <> %Int(OvrEmployer#);
            Reade (kkCo#:kkFnd:Peo_Link_Code) pem18;
            Iter;
          Endif;
       //*** End Add ***** 10/05/23 ********************************
          If EMcad1 <> 0 and EMcad1 <= eff_Dt7;
             Reade (kkCo#:kkFnd:Peo_Link_Code) pem18;
             Iter;
          Endif;
          Chain (kkCo#:kkFnd:kkFyr:EMemp#:EMdiv) pel01;
          If Not(%Found);
             Reade (kkCo#:kkFnd:Peo_Link_Code) pem18;
             Iter;
          Endif;
          Chain (kkCo#:ELrtcd) prs01;
          If RSjrst = Rating_State and indDivFound = *Off;
             indDivFound = *On;
             svEmp# = EMemp#;
             svDiv  = EMdiv;
             svFein = EMfein;
             svArSm = ELarsm;
             Reade (kkCo#:kkFnd:Peo_Link_Code) pem18;
             Iter;
          Endif;
          If RSjrst = Rating_State and indDivFound = *On
           and %Parms >= 1;
             retCode = 1;  // More than one active policy exists for link code/rating state
          Endif;
       Reade (kkCo#:kkFnd:Peo_Link_Code) pem18;
       Enddo;
       // Just need these 3 values to write WMALP record
       If indDivFound;
          EMemp# = svEmp#;
          EMdiv  = svDiv;
          EMfein = svFein;
       Endif;
       Return indDivFound;
      /End-Free

     p GetDivision     e
      //----------------------------------------------------------------
      // getNextDiv - Gets the next available division
      //----------------------------------------------------------------
     p GetNextDiv      B
     d GetNextDiv      PI             5s 0
     d  employer#                     9p 0

      /Free
       k1emp# = employer#;
       k1div = *hival;
       Setgt emky2 pem0q;
       Readpe emky3 pem0q;
       If %Eof;
          Return 0;
       Else;
          Return qEMdiv + 1;
       Endif;
      /End-Free

     p GetNextDiv      E
      //----------------------------------------------------------------
      // getNextEmp# - Gets the next available employer #
      //----------------------------------------------------------------
     p GetNextEmp#     B
     d getNextEmp#     PI             9s 0

      /Free
       //*** beg chg ***** 11/15/21 ***************
       //Chain fnky pfn01;
       //If %Found;
       //   FNnaen = FNnaen + 1;
       //   Update pfn01;
       //   Fnnaen = FNnaen - 1;
       //   Return FNnaen;
       //Endif;
       Chain fnKy pfn01;
       Select;
       When isQteImp;
          FNnaqn = FNnaqn + 1;
          //*** Beg Add ***** 05/23/24 ***********************
          FNpgm  = Q1pgm;
          FNuser = Q1user;
          FNtime = Timeto6(SystemTime());
          FNdt   = Dateto7(SystemDate());
          //*** End Add ***** 05/23/24 ***********************
          Update pfn01;
          Fnnaqn = FNnaqn - 1;
          Return FNnaqn;
       Other;
          FNnaen = FNnaen + 1;
          //*** Beg Add ***** 05/23/24 ***********************
          FNpgm  = Q1pgm;
          FNuser = Q1user;
          FNtime = Timeto6(SystemTime());
          FNdt   = Dateto7(SystemDate());
          //*** End Add ***** 05/23/24 ***********************
          Update pfn01;
          Fnnaen = FNnaen - 1;
          Return FNnaen;
       EndSl;
       //*** end chg ***** 11/15/21 ***************
      /End-Free

     p GetNextEmp#     E
      //------------------------------------------------------------------------*
      // getNumOfHead   - gets the number of headers
      //------------------------------------------------------------------------*
     p getNumOfHead    B
     d getNumOfHead    PI             6s 0

     d hCount          s              6s 0 inz(0)

      /Free
       Exec Sql
        Declare Project2 Cursor For
        Select Count(*)
        From Wxpmp
        Where Pmrect = 'H';
       Exec Sql
        Open Project2;
       If Sqlstt = '00000';
          Exec Sql
           Fetch Next
           From Project2
           Into  :hCount;
       Endif;
       Exec Sql
        Close Project2;
       Return hCount;
      /End-Free

     p GetNumOfHead    E
      //------------------------------------------------------------------------*
      // getNumOfTrail  - gets the number of trailers
      //------------------------------------------------------------------------*
     p GetNumOfTrail   B
     d GetNumOfTrail   PI             6s 0
     d tCount          s              6s 0 inz(0)

      /Free
       Exec Sql
        Declare Project3 Cursor For
        Select Count(*)
        From Wxpmp
        Where Pmrect = 'T';
       Exec Sql
        Open Project3;
       If Sqlstt = '00000';
          Exec Sql
           Fetch Next
           From Project3
           Into  :Tcount;
       Endif;
       Exec Sql
        Close Project3;
       Return tCount;
      /End-Free

     p GetNumOfTrail   e
      //------------------------------------------------------------------------
      // Return PEO Type
      //------------------------------------------------------------------------
     p GetPeoType      b
     d GetPeoType      pi             1a

      /Free
       Select;
       When FNPeof <> 'C';
          Return FNpeof;
       When FNPeof = 'C';
          Return RRpeof;
       Endsl;
      /End-Free

     p GetPeoType      e
      //------------------------------------------------------------------------*
      // getRateSetCode - Returns default rate set code from WDRRP
      //------------------------------------------------------------------------*
     p GetRateSetCode  b
     d GetRateSetCode  pi             3a
     d  rateSt                        2a
     d  effDate7                      7s 0

      /Free
       Setgt  (kkCo#:kkFnd:rateSt:effDate7) prr00;
       Readpe (kkCo#:kkFnd:rateSt) prr00;
       If %Eof;
          Return *blanks;
       Else;
          Return RRdrat;
       Endif;
      /End-Free

     p GetRateSetCode  e
      //------------------------------------------------------------------------*
      // getRecordCnt  - gets the number of records
      //------------------------------------------------------------------------*
     p GetRecordCnt    B
     d GetRecordCnt    PI             9s 0

     d rCount          s              9s 0 inz(0)

      /Free
       Exec Sql
        Declare Project4 Cursor For
        Select Count(*)
        From Wxpmp;
       Exec Sql
        Open Project4;
       If Sqlstt = '00000';
       Exec Sql
        Fetch Next
        From Project4
        Into  :rCount;
       Endif;
       Exec Sql
        Close Project4;
       Return rCount;
      /End-Free

     p GetRecordCnt    E
      //----------------------------------------------------------------
      // InvalidChars - look for character data in numeric field
      //----------------------------------------------------------------
     p InvalidChars    b
     d InvalidChars    pi              n
     d  valueToCheck                 11    const
     d  nameOfField                  25    const
     d  isRequired                     n   const
     d  aNumber        s             11s 0

      /Free
       Monitor;
          aNumber = %Int(valueToCheck);
       On-Error;
          Select;
          When not(isRequired) and valueToCheck = *blanks;
          Other;
             aNumber = %Int(rrnWxpmp) + 1;
             message = 'Row ' + %Char(aNumber) + ' - ' +
              %trim(nameOfField) + ' has character data.';
             PrintError(0:*Off);
             Return *On;
          Endsl;
       EndMon;
       Return *Off;
      /End-Free

     p InvalidChars    e
      //----------------------------------------------------------------
      // Returns *On if location exists
      //----------------------------------------------------------------
     p LocationExists  b
     d LocationExists  pi              n
     d  isUpdateTrans                  n   const

      /Free
       If GetDivision();
          If isUpdateTrans;
             Chain (kkCo#:kkFnd:EMemp#:EMdiv:Loc_ID) pal00;
             Return %Found;
          Else;
             Chain(n) (kkCo#:kkFnd:EMemp#:EMdiv:Loc_ID) pal00;
             Return %Found;
          Endif;
       Else;
          Return *Off;
       Endif;
      /End-Free

     p LocationExists  e
      //------------------------------------------------------------------------*
      // PolPgNotRun - Checks status of policy page before submitting midterm end
      //------------------------------------------------------------------------*
     p PolPgNotRun     b
     d PolPgNotRun     pi              n
      /Free
       Chain (ELco#:ELfnd:ELemp#:ELfyr:'P') psh01;
       If %Found and SHstat > '2';
          Return *Off;
       Else;
          Return *On;
       Endif;
      /End-Free
     p PolPgNotRun     e
      //------------------------------------------------------------------------*
      // printError - Prints errors to a report
      //------------------------------------------------------------------------*
     p PrintError      B
     d PrintError      PI
     d  errorNumber                   3s 0 value
     d  errorFlag                      n   value

      /Free
       If errorNumber <> 0;
          errReason = err(errorNumber);
       Endif;

       *in20 = *off;
       If errorFlag = *on;
          *in20 = *on;
       Endif;

       If eQue = *blanks;
          *In08 = *On;
       Else;
          *In09 = *On;
       Endif;

       xxque    = eQue;
       userName = getUsrName(q1user);
       UPusnm   = userName;
       line = *all'_';

       If not hasError;
          Clear ORTITL;
          xxfil#   = '0';
          Chain orky por01;

          If ORtitl = *blank;
             If eRunType = 'A';
                ORtitl = mtitle;
             Else;
                ORtitl = title;
             Endif;
          Endif;

          ORtitl = centerFld(ORtitl);
          hasError = *on;
          Except head;
       Endif;

       If *inoa = *on;
          *inoa = *off;
          Except head;
       Endif;

       Select;
       When PMRecT = 'H';
          recordType = 'Header';
       When PMRecT = 'D';
          recordType = 'Detail';
       When PMRecT = 'T';
          recordType = 'Trailer';
       Other;
          recordType = 'Invalid';
       Endsl;

       intRowNumber = %Int(rrnWxpmp) + 1;
       Except det;

       If message <> message1 or errorNumber <> 0;
          errorCount = errorCount + 1;
       Endif;
      /End-Free

     p PrintError      e
      //----------------------------------------------------------------=========
      // printUpload - prints successful upload report
      //----------------------------------------------------------------=========
     p printUpload     B
     d printUpload     PI

      /Free
       If eQue = *blanks;
          *In08 = *On;
       Else;
          *In09 = *On;
       Endif;

       xxque    = eQue;
       userName = getUsrName(q1user);
       UPUSNM   = userName;
       line1 = *all'_';
       clear ORTITL;
       xxfil#   = '1';
       Chain orky por01;

       If ORtitl = *blank;
          If eRunType = 'A';
             ORtitl1 = mtitle1;
          Else;
             ORtitl1 = title1;
          Endif;
       Else;
          ORtitl1 = ORtitl;
       Endif;

       ORtitl1 = centerFld(ORtitl1);
       Except head1;
       charRowMsg = %Trim(%Editc(intRowCount:'3')) + ' rows processed';
       charDivMsg = %Trim(%Editc(intDivCount:'3')) +
        ' division/s added to Group ' + %Char(FNfnd);
       charLocMsg = %Trim(%Editc(intLocCount:'3')) +
        ' location/s added to Group ' + %Char(FNfnd);
       Except det1;
      /End-Free

     p printUpload     E
      //------------------------------------------------------------------------*
      // ProcessWXPMP
      //------------------------------------------------------------------------*
     p ProcessWXPMP    b
     d ProcessWXPMP    pi

      /Free
       Read ppm00;
       Dow Not(%Eof);
          detail = pmData;
          Select;
        //*** beg add ***** 10/16/21 **********************
          When PMemp# <> *Blanks;
             UpdateWMEMP();
        //*** end add ***** 10/16/21 **********************
          When Deletion_Date <> *Blanks;
          //*** Beg Add ***** 10/05/23 ******************************
             If PMovrEmp# <> *blanks;
                kkEmp# = %Int(PMovrEmp#);
             Endif;
          //*** End Add ***** 10/05/23 ******************************
             If PMelid = *blanks;
                GetDivision();
                WrtMidTermDlt('D');
             Else;
                UpdateWmalp();
                WrtMidTermDlt('L');
             Endif;
          Other;
             If PMelid = *blanks;
                WriteWMEMP();
                AutoCrtLoc();
                WriteWMNBP();
                WriteWDELP();
                WriteWDXSP();
                //*** Beg Del ***** 08/24/21 **********************************
                //WriteWMLDP();
                //*** End Del ***** 08/24/21 **********************************
                WriteWMRPP();
                WriteWMAHP();
                WriteWDVHP();
                //*** Beg Add ***** 09/15/23 **********************************
                WriteWDCXP();
                //*** End Add ***** 09/15/23 **********************************
                WriteQUOTE();
                WrtMidTermAdd('D');
             Else;
                WriteWMALP();
                WriteWDLDP();
                WrtMidTermAdd('L');
             Endif;
          Endsl;
       Read ppm00;
       Enddo;
      /End-Free

     p ProcessWXPMP    e
      //------------------------------------------------------------------------*
      // programStart - Initialization
      //------------------------------------------------------------------------*
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       GetClientID(clientID);
       //*** beg add ***** 11/15/21 *************
       UserFilesOpen();
       RtvEnv(strEnv);
       If strEnv = '*QTE';
          isQteImp = *On;
          RtvQteLib('*LIV':strLiveLib);
       EndIf;
       //*** end add ***** 11/15/21 *************
       sys_Date = %date();
       sys_Time = %time();
       sysDateCymd = %Dec(sys_Date:*cymd);
       sysDateUsa  = %Dec(sys_Date:*usa);
       sysTimeHms  = %Dec(sys_Time:*hms);
       kkco# = UserCo#(q1user);
       companyNm = GetCompNam(kkco#);
       companyNm = CenterFld(companyNm);
       kkfnd  = %Dec(efnd:3:0);
       kkemp# = 1;
       kkdiv  = 0;
       Chain(n) fnky pfn01;
       Chain    fnky pex00;
       RtvBrnd('@':#Brnd1:#brnd2);
       // Set LDA so that W0051R30 knows it's an import
       In *Lock *Dtaara;
       ldPeoImpFlg = 'Y';
       Out *Dtaara;
      /End-Free

     p ProgramStart    e
      //----------------------------------------------------------------
      // SetBadData - sets badData *on if not already *on
      //----------------------------------------------------------------
     p SetBadData      b
     d SetBadData      pi
     d  prBadData                      n   const

      /Free
       If prBadData = *On;
          badData = *On;
       Endif;
      /End-Free

     p SetBadData      e
      //----------------------------------------------------------------
      // Update Locations for delete transaction
      //----------------------------------------------------------------
     p UpdateWmalp     b
     d UpdateWmalp     pi

      /Free
       If LocationExists(*On);
       //*** begin add ***  10/16/25  ******************************************
       //* populate deletion transaction date/time.
          If ALDldt = 0;
             ALTrddt = sysDateCymd;
             ALTrdtm = sysTimeHms;
          Endif;
       //*** end add *****  10/16/25  ******************************************
          ALdldt = %dec(%char(%date(Deletion_Date:*iso0):*cymd0):7:0);
          ALdlrs = 'Expired from PEO Policy Import on ' + %Char(Sys_Date:*USA/);
          ALUser = Q1user;
          ALPgm  = Q1pgm;
          ALDt   = sysDateCymd;
          ALTime = sysTimeHms;
          Update pal00;
       Endif;
      /End-Free

     p UpdateWmalp     e
      //*** beg add ***** 10/16/21 ************************
      //----------------------------------------------------
       Dcl-Proc UpdateWMEMP;
      //----------------------------------------------------
        Chain (kkCo#:kkFnd:%Int(PMemp#):%Int(PMdiv)) pem02;
        EMlink = PMpeol;
        EMuser = Q1user;
        EMpgm  = Q1pgm;
        EMdt   = sysDateCymd;
        EMtime = sysTimeHms;
        Update pem02;
       End-Proc;
      //*** end add ***** 10/16/21 ************************
      //*** beg add ***** 11/15/21 ************************
      //===================================================
      // Close user open files
      //===================================================
       Dcl-Proc UserFilesClose;
        If %Open(WDELL1);
           Close WDELL1;
        EndIf;
        If %Open(WDELL2);
           Close WDELL2;
        EndIf;
        If %Open(WMAHL);
           Close WMAHL;
        EndIf;
        If %Open(WMEML);
           Close WMEML;
        EndIf;
        If %Open(WMEML18);
           Close WMEML18;
        EndIf;
       End-Proc;
      //===================================================
      // Delete overrides on user open files
      //===================================================
       Dcl-Proc UserFilesDltOvr;
        strCommand = 'DLTOVR FILE(WDELL1)';
        Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
        strCommand = 'DLTOVR FILE(WDELL2)';
        Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
        strCommand = 'DLTOVR FILE(WMAHL)';
        Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
        strCommand = 'DLTOVR FILE(WMEML)';
        Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
        strCommand = 'DLTOVR FILE(WMEML18)';
        Callp QCMDEXC(%Trim(strCommand):%Len(strCommand));
       End-Proc;
       //========================================
       // Open user open files
       //========================================
       Dcl-Proc UserFilesOpen;
        If Not %Open(WDELL1);
           Open WDELL1;
        EndIf;
        If Not %Open(WDELL2);
           Open WDELL2;
        EndIf;
        If Not %Open(WMAHL);
           Open WMAHL;
        EndIf;
        If Not %Open(WMEML);
           Open WMEML;
        EndIf;
        If Not %Open(WMEML18);
           Open WMEML18;
        EndIf;
       End-Proc;
       //========================================
       // Override user open files to live
       //========================================
       Dcl-Proc UserFilesOvrToLive;
        strCommand = 'OVRDBF FILE(WMAHL) TOFILE(' +
         %Trim(strLiveLib) + '/WMAHL)';
        Callp QCmdExc(%Trim(strCommand):%Len(strCommand));
        strCommand = 'OVRDBF FILE(WMEML) TOFILE(' +
         %Trim(strLiveLib) + '/WMEML)';
        Callp QCmdExc(%Trim(strCommand):%Len(strCommand));
        strCommand = 'OVRDBF FILE(WMEML18) TOFILE(' +
         %Trim(strLiveLib) + '/WMEML18)';
        Callp QCmdExc(%Trim(strCommand):%Len(strCommand));
        strCommand = 'OVRDBF FILE(WDELL1) TOFILE(' +
         %Trim(strLiveLib) + '/WDELL1)';
        Callp QCmdExc(%Trim(strCommand):%Len(strCommand));
        strCommand = 'OVRDBF FILE(WDELL2) TOFILE(' +
         %Trim(strLiveLib) + '/WDELL2)';
        Callp QCmdExc(%Trim(strCommand):%Len(strCommand));
       End-Proc;
      //*** end add ***** 11/15/21 ************************
      //*** Beg Add ***** 09/15/23 ************************
       //------------------------------------------------------------
       // WriteWDCXP - Writes to Cancellation Detail File
       //------------------------------------------------------------
       Dcl-Proc WriteWDCXP;
        Dcl-S svReq# Packed(5:0);
        //*** Beg Add ***** 12/03/24 ************************
        If ELemp# = KKEmp# and ELdiv = KKDiv;
           Return;
        Endif;
        If FNpeof = 'C';
           GetRateSetCode(Rating_State:eff_Dt7);
           If RRpeof = 'M';
              Return;
           Endif;
        Endif;
        If FNpeof = 'M';
           Return;
        Endif;
        //*** End Add ***** 12/03/24 ************************
        //Copy Cncellation detail records of the State reporting division (Emp# 1, Div 0) to
        // new added Division.
        Setgt  (ELCo#:ELFnd:ELFyr:KKEmp#:KKDiv:*Hival) pcx00;
        Readpe (ELCo#:ELFnd:ELFyr:KKEmp#:KKDiv) pcx00;
        If Not %Eof(wdcxl) and CXrein <> 'Y';        //Cancel detail record found and Not Reinstaed
           svReq# = CXreq#;
           CXreq# = 1;
           CXdiv  = ELdiv;
           //*** Beg Add ***** 12/03/24 ************************
           CXemp# = ELemp#;
           //*** End Add ***** 12/03/24 ************************
           CXuser = Q1user;
           CXpgm  = Q1pgm;
           CXdt   = sysDateCymd;
           CXtime = sysTimeHms;
           CXidt = CXdt;
           CXitm = CXtime;
           CXiusr = CXuser;
           CXipgm = CXpgm;
           Write pcx00;

           //Copy Cncellation history records of the State reporting
           //division to new added Division.
           Setll (ELCo#:ELFnd:ELFyr:KKEmp#:KKDiv:svReq#) pxh00;
           Reade (ELCo#:ELFnd:ELFyr:KKEmp#:KKDiv:svReq#) pxh00;
           Dow Not %Eof(wdxhl);
              XHdiv  = ELdiv;
              //*** Beg Add ***** 12/03/24 ************************
              XHemp# = ELemp#;
              //*** End Add ***** 12/03/24 ************************
              XHreq# = CXreq#;
              XHuser = Q1user;
              XHpgm  = Q1pgm;
              XHdt   = sysDateCymd;
              XHtime = sysTimeHms;
              Write pxh00;
              Reade (ELCo#:ELFnd:ELFyr:KKEmp#:KKDiv:svReq#) pxh00;
           Enddo;
        Endif;
       End-Proc;
      //*** End Add ***** 09/15/23 ************************
      //----------------------------------------------------------------
      // writeWDELP - writes to Policy Year Detail
      //----------------------------------------------------------------
     p WriteWDELP      b
     d WriteWDELP      pi

      *** Beg Add *** 08/24/21 ******************************************
     d inEntryType     s             30a
     d dsEntryType     ds                  Qualified
     d entryType                     20a
     d rateCode                       3a
     d billPlan                       3p 0
      *** End Add *** 08/24/21 ******************************************

      /Free
       ELemp# = EMemp#;
       ELdiv  = EMdiv;
       ELpfdt = eff_Dt7;
       If Pol_Ann_Date <> *Blanks;
          ELandt = %dec(%char(%date(Pol_Ann_Date:*iso0):*cymd0):7:0);
       Else;
          ELandt = 0;
       Endif;
       ELrtcd = GetRateSetCode(Rating_State:eff_Dt7);
       If RRbscd <> *Zero;
          ELbscd = RRbscd;
       EndIf;
       ELsrpr = 'P';
       //*** begin add ***  08/05/24  ******************************
       Select;
       When Exp_Rate_Flg <> *Blank;
          ELExrt = Exp_Rate_Flg;
       When clientID = '150';
          ELExrt = '2';
       Other;
          ELExrt = '3';
       Endsl;
       If ELExrt = '1' or ELExrt = '2';
          ELNcci = 'Y';
       Endif;
       //*** end add *****  08/05/24  ******************************
       ELtypb = Type_of_Bld;
       ELilcd = IncLimitCode;
       If ELilcd <> *Blanks;
           ELilef = Eff_Dt7;
       Endif;
       If SchedDbCr = *blanks or (RRcrcd = 0 and RRdbcd = 0);
          ELsdef = 0;
          ELdbcr = 0;
       Else;
          ELsdef = Eff_Dt7;
          ELdbcr = %Dec(SchedDbCr:5:4);
       Endif;
       Clear ELdvef;
       Clear ELasp2;
       If RRdv1c <> 0;
          Select;
          When RRdv1chg = 'Y' and Dev1Factor <> *blanks;
             ELasp2 = %Dec(Dev1Factor:5:2);
             ELdvef = Eff_Dt7;
          When RRdv1dft <> 0;
             ELasp2 = RRdv1dft;
             ELdvef = Eff_Dt7;
          Endsl;
       Endif;
       Clear ELdev2;
       Clear ELd2ef;
       If RRdv2c <> 0;
          Select;
          When RRdv2chg = 'Y' and Dev2Factor <> *blanks;
             ELdev2 = %Dec(Dev2Factor:5:2);
             ELd2ef = Eff_Dt7;
          When RRdv2dft <> 0;
             ELdev2 = RRdv2dft;
             ELd2ef = Eff_Dt7;
          Endsl;
       Endif;
       If ExpModFactor = *blanks;
          ELexm1 = 0;
          ELemd1 = 0;
       Else;
          ELexm1 = %Dec(ExpModFactor:5:4);
          ELemd1 = Eff_Dt7;
       Endif;
       If #OfEmployees = *blanks;
          ELempy = 0;
       Else;
          ELempy = %Int(#OfEmployees);
       Endif;
      //*** beg add ***** 02/04/22 ***************
       If ELemp# <> 1;
          ELnwrw = 'N';
       Endif;
      //*** end add ***** 02/04/22 ***************
      //*** begin add ***  01/06/23  *************************************
       Clear ELRqfl;
      //*** end add *****  01/06/23  *************************************
      //*** beg chg ***** 08/24/21 ***************
       // Clear waiver info if not set up in WDRRP
       //If RRwvfl = *blank;
       //   Clear ELwvtp;
       //   Clear ELwvpc;
       //   Clear ELfchg;
       //   Clear ELwmin;
       //   Clear ELwvfl;
       //Endif;
       // Clear stock discount if not set up in WDRRP
       //If RRdspr = *blanks;
       //   Clear ELdisp;
       //   Clear ELdisc;
       //Endif;
       // Clear contractor's credit if not set up in WDRRP
       //If RRcccd = *zeros;
       //   Clear ELasp1;
       //   Clear ELcref;
       //Endif;
       // Clear subject premium credit 1a if not set up
       //If RR1acd = *zeros;
       //   Clear ELaspm;
       //   Clear ELmcef;
       //Endif;
       // Clear subject premium credit 1b if not set up
       //If RR1bcd = *zeros;
       //   Clear ELasps;
       //   Clear ELscef;
       //Endif;
       // Clear deductible code if not set up
       //Setgt  (ELco#:ELfnd:Rating_State:ELdcde:ELpfdt) pdd02;
       //Readpe (ELco#:ELfnd:Rating_State:ELdcde) pdd02;
       //Dow Not(%Eof) and DDstat = 'I';
       //Readpe (ELco#:ELfnd:Rating_State:ELdcde) pdd02;
       //Enddo;
       //If %Eof;
       //   Clear ELdcde;
       //   Clear ELdcef;
       //Endif;
       // Clear voluntary labor charge if not set up
       //If RRvlcc = 0;
       //   Clear ELvlca;
       //Endif;
      //*** Beg Add *** 09/01/23 *** PHW111666 ***************************
       // Clear premiums carried from PEO
       Clear ELprtd;
      //*** End Add *** 09/01/23 *** PHW111666 ***************************
       // Clear premiums carried from PEO
       Clear ELemnp;
       Clear ELEMNP;
       Clear ELAMNP;
       Clear ELMAPR;
       Clear ELAMPR;
       Clear ELESBJ;
       Clear ELASBJ;
       Clear ELSTPR;
       Clear ELASPR;
       Clear ELEMPM;
       Clear ELAMPM;
       Clear ELENMP;
       Clear ELANMP;
       Clear ELANPR;
       Clear ELAAPR;
       Clear ELINMP;
       Clear ELTXPR;
       Clear ELTXDU;
       Clear ELEDC1;
       Clear ELADC1;
       Clear ELEDC2;
       Clear ELADC2;
       Clear ELEDV2;
       Clear ELADV2;
       Clear ELEDC3;
       Clear ELADC3;
       Clear ELEXP1;
       Clear ELEXP2;
       Clear ELEXP3;
       Clear ELAXP1;
       Clear ELAXP2;
       Clear ELAXP3;
       Clear ELEIN1;
       Clear ELAIN1;
       Clear ELEIN2;
       Clear ELAIN2;
       Clear ELEDCT;
       Clear ELADCT;
       Clear ELEDCM;
       Clear ELADCM;
       Clear ELEDCS;
       Clear ELADCS;
       Clear ELEDCR;
       Clear ELADCR;
       Clear ELEDC ;
       Clear ELADC ;
       Clear ELEMIN;
       Clear ELAMIN;
       Clear ELSTAM;
       Clear ELRETN;
       Clear ELEFRS;
       Clear ELAFRS;
       Clear EL1APR;
       Clear EL2APR;
       Clear ELSBFE;
       Clear ELETCA;
       Clear ELATCA;
       Clear EL1FPR;
       Clear EL2FPR;
       Clear EL1AAM;
       Clear EL2AAM;
       Clear ELMFAM;
       Clear ELSFAM;
       Clear EL3APR;
       Clear EL3FPR;
       Clear EL3AAM;
       Clear ELETER;
       Clear ELATER;
       Clear ELRPE1;
       Clear ELRPA1;
       Clear ELRPE2;
       Clear ELRPA2;
       Clear ELRPE3;
       Clear ELRPA3;
       Clear EL4APR;
       Clear EL4FPR;
       Clear EL4AAM;
       Clear EL5APR;
       Clear EL5FPR;
       Clear EL5AAM;
       Clear ELEDD2;
       Clear ELADD2;
       Clear ELEDTR;
       Clear ELADTR;
       Clear EL6APR;
       Clear EL6FPR;
       Clear EL6AAM;
       Clear EL7APR;
       Clear EL7FPR;
       Clear EL7AAM;
       //Clear Rating Factors
       Clear ELmanP;
       Clear ELfFl2;
       Clear ELcMul;
       Clear ELcxBl;
       Clear ELdCde;
       Clear ELdCd2;
       Clear ELdCEf;
       Clear ELdCEx;
       Clear EL2DEf;
       Clear EL2DEx;
       Clear ELd1S2;
       Clear ELd1D2;
       Clear ELd1X2;
       Clear ELstFc;
       Clear ELdev2F;
       Clear ELd2S2;
       Clear ELd2D2;
       Clear ELd2X2;
       Clear ELmcCd;
       Clear ELmcGp;
       Clear ELaspM;
       Clear ELmcEf;
       Clear ELmcEx;
       Clear ELaspS;
       Clear ELscEf;
       Clear ELscEx;
       Clear ELasp1;
       Clear ELcrEf;
       Clear ELcrEx;
       Clear ELprom;
       Clear ELexM2;
       Clear ELeMD2;
       Clear ELexM3;
       Clear ELeMD3;
       Clear ELaRp1;
       Clear ELaRp2;
       Clear ELaRp3;
       Clear ELexMd1Nf;
       Clear ELexMd2Nf;
       Clear ELexMd3Nf;
       Clear ELasp3;
       Clear ELdisc;
       Clear ELdisP;
       Clear ELmFee;
       Clear ELsFMn;
       Clear ELsFPc;
       Clear ELwvTp;
       Clear ELwvPc;
       Clear ELfChg;
       Clear ELwMin;
       Clear ELwMax;
       Clear ELwvFl;
       Clear ELarCp;
       Clear ELarCa;
       Clear ELarEf;
       Clear ELarEx;
       Clear ELvLCa;
      //*** end chg ***** 08/24/21 ***************
       //*** Begin Add *** 05/20/22 ****************************************
       Clear ELfaf;
       //*** End   Add *** 05/20/22 ****************************************
       ELuser = Q1user;
       ELpgm  = Q1pgm;
       ELdt   = sysDateCymd;
       Eltime = sysTimeHms;
       ELiusr = ELuser;
       ELipgm = ELpgm;
       ELidt  = ELdt;
       ELitm  = ELtime;
       Clear ELqtfr;
       //*** Beg Add *** 03/31/25 ******************************************
       // Extract the 'Bill on Effective Date flag'
       If EMemp# = 1 or EMemp# <> 1 and EMdiv = 0;
          inEmp# = 1;
          inDiv = 0;
       Else;
          inEmp# = EMemp#;
          inDiv = GetStRptDv(EMco#:EMfnd:EMemp#);
       Endif;
       Chain (EMco#:EMfnd:inEmp#:inDiv:ELfyr) pel03;
       If %Found(WDELL);
          ELblfl = C_ELblfl;
       Endif;
       //*** End Add *** 03/31/25 ******************************************
       Write pel0p;
       Feod Wdelp;
       //*** Beg Add *** 08/24/21 ******************************************
       //*** beg chg ***** 11/15/21 **************
       //If ELemp# = 1;
       //   dsEntryType.entryType = 'IMP Add Division';
       //Else;
       //   dsEntryType.entryType = 'IMP Add Division';
       //Endif;
       Select;
       When isQteImp = *On And Elemp# = 1;
          dsEntryType.entryType = 'IMP Add Qte Div';
       When isQteImp = *On And ELemp# <> 1;
          dsEntryType.entryType = 'IMP Add Qte Policy';
       When isQteImp = *Off And Elemp# = 1;
          dsEntryType.entryType = 'IMP Add Division';
       When isQteImp = *Off And ELemp# <> 1;
          dsEntryType.entryType = 'IMP Add Division';
       EndSl;
       //*** end chg ***** 11/15/21 **************
       dsEntryType.rateCode = ELrtcd;
       dsEntryType.billPlan = ELbscd;
       inEntryType = dsEntryType;
       DefaultRatingFactors(rrnWDELP:inEntryType);

       If FNpeof <> *Blank;
          Chain (Elco#:ELfnd:ELfyr:ELemp#:ELdiv) pel01 dsNewWDELP;
       //*** beg del ***** 11/15/21 **************
       // Chain (Elco#:ELfnd:ELfyr:1:0) pel01 dsPeoWDELP;
       //*** end del ***** 11/15/21 **************
       Endif;
       //Deductible Discount
       If dsPeoWDELP.ELdcde <> *Blank and dsNewWDELP.ELdcde = *Blank;
          strHeading = errRatingDft(1);
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Stock Discount
       If (dsPeoWDELP.ELasp3 <> *Zero and dsNewWDELP.ELasp3 = *Zero) or
        (dsPeoWDELP.ELdisc <> *Zero and dsNewWDELP.ELdisc = *Zero);
          If RRdshd <> *Blank;
             strHeading = RRdshd;
          Else;
             strHeading = %trim(errRatingDft(2));
          Endif;
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Safety Fee Percent/Minimum Amount
       If (dsPeoWDELP.ELSfpc <> *Zero and dsNewWDELP.ELSfpc = *Zero) or
        (dsPeoWDELP.ELSfmn <> *Zero and dsNewWDELP.ELSfmn = *Zero);
          If RRsfhd <> *Blank;
             strHeading = RRsfhd;
          Else;
             strHeading = %trim(errRatingDft(3));
          Endif;
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Waiver Surcharge
       If (dsPeoWDELP.ELwvtp <> *Blank and dsNewWDELP.ELwvtp = *Blank) or
        (dsPeoWDELP.ELwvpc <> *Zero  and dsNewWDELP.ELwvpc = *Zero) or
        (dsPeoWDELP.ELfchg <> *Zero  and dsNewWDELP.ELfchg = *Zero) or
        (dsPeoWDELP.ELwmin <> *Zero  and dsNewWDELP.ELwmin = *Zero) or
        (dsPeoWDELP.ELwmax <> *Zero  and dsNewWDELP.ELwmax = *Zero) or
        (dsPeoWDELP.ELwvfl <> *Blank and dsNewWDELP.ELwvfl = *Blank);
        strHeading = %trim(errRatingDft(4));
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Subj prem credit 1A
       If dsPeoWDELP.ELaspM <> *Zero and dsNewWDELP.ELaspM = *Zero;
          If RR1ahd <> *Blank;
             strHeading = RR1ahd;
          Else;
             strHeading = %trim(errRatingDft(5));
          Endif;
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Subj prem credit 1B
       If dsPeoWDELP.ELaspS <> *Zero  and dsNewWDELP.ELaspS = *Zero;
          If RR1bhd <> *Blank;
             strHeading = RR1bhd;
          Else;
             strHeading = %trim(errRatingDft(6));
          Endif;
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Voluntary Labor Chrg
       If dsPeoWDELP.ELvLCa <> *Zero and dsNewWDELP.ELvLCa = *Zero;
          If RRvlch <> *Blank;
             strHeading = RRvlch;
          Else;
             strHeading = %trim(errRatingDft(7));
          Endif;
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Contractor Credit
       If dsPeoWDELP.ELasp1 <> *Zero and dsNewWDELP.ELasp1 = *Zero;
          If RRcchd <> *Blank;
             strHeading = RRcchd;
          Else;
             strHeading = %trim(errRatingDft(8));
          Endif;
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Facultative Reinsurance
       If (dsPeoWDELP.ELarCp <> *Zero and dsNewWDELP.ELarCp = *Zero) or
        (dsPeoWDELP.ELarCa <> *Zero and dsNewWDELP.ELarCa = *Zero);
          strHeading = %trim(errRatingDft(9));
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //Stabilization Factor
       If dsPeoWDELP.ELstFc <> *Zero and dsNewWDELP.ELstFc = *Zero;
          strHeading = %trim(errRatingDft(10));
          message = 'INFO ONLY: ' +  %trim(strHeading) +
           ' did not carry over from the PEO.';
           PrintError(0:*off);
       Endif;
       //*** End Add *** 08/24/21 ******************************************
      /End-Free

     p WriteWDELP      e
      //----------------------------------------------------------------
      // writeWDLDP - Write/Update Location Year Info
      //----------------------------------------------------------------
     p WriteWDLDP      b
     d WriteWDLDP      pi

      /Free
       Chain (ALco#:ALfnd:ALemp#:ALdiv:%Int(Loc_Year):ALelid) pld01;
       LDco#  = ALco#;
       LDFnd  = ALfnd;
       LDEmp# = ALemp#;
       LDDiv  = ALdiv;
       LDeltp = ALeltp;
       LDelid = ALelid;
       LDfyr  = %Int(Loc_Year);
       LDempy = %Int(#OfEmployees);
       LDtypb = Type_of_Bld;
       If Loc_YrTtlPay = *blanks;
          LDtpyr = 0;
       Else;
          LDtpyr = %Int(Loc_YrTtlPay);
       Endif;
       If Loc_Bldg_Yr = *blanks;
          LDbuildYr = 0;
       Else;
          LDbuildYr  = %Int(Loc_Bldg_Yr);
       Endif;
       If Loc_#OfStories = *blanks;
          LD#Stories = 0;
       Else;
          LD#Stories = %Int(Loc_#OfStories);
       Endif;
       If Loc_Peak_Emp = *blanks;
          LDpeakEmp = 0;
       Else;
          LDpeakEmp  = %Int(Loc_Peak_Emp);
       Endif;
       LDpgm  = Q1pgm;
       LDuser = Q1user;
       LDdt   = sysDateCymd;
       LDtime = sysTimeHms;
       If %Found(Wdldl1);
          Update pld01;
       Else;
          LDipgm = Q1pgm;
          LDiusr = Q1user;
          LDidt  = sysDateCymd;
          LDitm  = sysTimeHms;
          Write pld01;
          Feod  Wdldl1;
       Endif;
      /End-Free

     p WriteWDLDP      e
      //----------------------------------------------------------------
      // writeWDVHP - writes to Vendor History
      //----------------------------------------------------------------
     p WriteWDVHP      b
     d WriteWDVHP      pi

      /Free
       kkfyr = ELfyr;
       Chain vhky pvh02;
       If %Found;
          VHemp# = EMemp#;
          VHdiv  = EMdiv;
          VHuser = Q1user;
          VHpgm  = Q1pgm;
          VHdt   = sysDateCymd;
          VHtime = sysTimeHms;
          VHiusr = VHuser;
          VHipgm = VHpgm;
          VHidt  = VHdt;
          VHitim = VHtime;
          Write pvh01;
       Endif;
      /End-Free

     p WriteWDVHP      e
      //*** beg add ***** 02/06/18 **************
      //----------------------------------------------------------------
      // writeWDXSP - writes to Excluded States
      //----------------------------------------------------------------
     p WriteWDXSP      b
     d WriteWDXSP      pi

      /Free
       Chain (ELco#:ELfnd:ELemp#:ELfyr) pxs00;
       If %Found;
          Return;
       Endif;
       Setll (ELco#:ELfnd:0:0) pxs00;
       Reade (ELco#:ELfnd:0:0) pxs00;
       Dow Not %Eof;
          XSEmp# = ELEmp#;
          XSFyr  = ELFyr;
          XSIPgm = Q1PGM;
          XSIUsr = Q1USER;
          XSIDt  = sysDateCymd;
          XSPgm  = Q1PGM;
          XSUser = Q1USER;
          XSTime = sysTimeHms;
          XSDt   = XSIDt;
          Write pxs00;
       Reade (ELco#:ELfnd:0:0) pxs00;
       Enddo;
      /End-Free

     p WriteWDXSP      e
      //*** end add ***** 02/06/18 **************
      //----------------------------------------------------------------
      // writeWMAHP - writes to WMAHP
      //----------------------------------------------------------------
     p WriteWMAHP      b
     d WriteWMAHP      pi

      /Free
       kkfyr  = ELfyr;
       Chain ahky pah02;
       AHemp# = EMemp#;
       AHdiv  = EMdiv;
       AHefdt = sysDateCymd;
       //***  Beg Chg  **** 09/18/23 *******************************
       //***  If ELpfdt < AHefdt;
          //***  AHefdt = ELpfdt;
       //***  Endif;
       intDiv = GetStRptDv(EMCo#:EMFnd:EMEmp#);
       intFrDt = RtvPolFrDt(EMCo#:EMFnd:EMEmp#:intDiv:kkfyr);
       If intFrDt < AHefdt;
          AHefdt = intFrDt;
       Endif;
       //***  End Chg  **** 09/18/23 *******************************
       AHstat = 'A';
       AHuser = q1user;
       AHpgm  = q1pgm;
       AHdt   = sysDateCymd;
       AHtime = sysTimeHms;
       AHiusr = AHuser;
       AHipgm = AHpgm;
       AHidt  = AHdt;
       AHitm  = AHtime;
       Write pah01;
      /End-Free

     p WriteWMAHP      e
      //---------------------------------------------------------------------
      // writeQUOTE - writes 'W'ritten record to Quote Tracking
      //---------------------------------------------------------------------
     p WriteQUOTE      b
     d WriteQUOTE      pi

      /Free
       Clear pqt01;
       QTco#  = EMco#;
       QTfnd  = EMfnd;
       QTagt# = AHagt#;
       QTemp# = EMemp#;
       QTdiv  = EMdiv;
       QTchap = EMchap;
       QTname = EMname;
       QTefdt = EMefd1;
       //*** beg chg ***** 11/15/21 ***********************
       //QTtype = 'W';
       //QTstat = 'W';
       Select;
       When isQteImp;
          QTstat = 'Q';
          QTtype = 'P';
       Other;
          QTstat = 'W';
          QTtype = 'W';
       EndSl;
       //*** end chg ***** 11/15/21 ***********************
       //*** Beg Add ***** 12/18/23 ***************************
       strStRptQTtype =
        GetQuoteSts(EMco#:EMfnd:EMemp#:0:ELFyr:QTstat);
       If strStRptQTtype <> *Blank;
          QTtype = strStRptQTtype;
       Endif;
       //*** End Add ***** 12/18/23 ***************************
       QTwkdt = sysDateCymd;
       QTnbdt = sysDateCymd;
       QTordt = sysDateCymd;
       QTidt  = sysDateCymd;
       QTiusr = Q1user;
       QTipgm = Q1pgm;
       QTitm  = sysTimeHms;
       QTdt   = QTidt;
       QTtime = QTitm;
       QTpgm  = QTipgm;
       QTuser = QTiusr;
       Write pqt01;
      /End-Free

     p WriteQUOTE      e
      //----------------------------------------------------------------
      // WriteWMALP - Write location record
      //----------------------------------------------------------------
     p WriteWMALP      b
     d WriteWMALP      pi

      /Free
       GetDivision();
       Chain (kkCo#:kkFnd:EMemp#:EMdiv:Loc_ID) pal00;
       ALco#  = kkCo#;
       ALfnd  = kkFnd;
       ALemp# = Ememp#;
       ALdiv  = EMdiv;
       ALelid = Loc_ID;
       ALfein = %Int(Fed_Tax_ID);
       If ALfein = EMfein;
          ALeltp = 'L';
       Else;
          ALeltp = 'E';
       Endif;
       ALefdt = %Dec(%Char(%Date(Eff_Date:*Iso0):*Cymd0):7:0);
       ALname = %subst(Dba_Name:1:40);
       ALnam2 = %subst(Dba_Name:41:60);
       ALfdNm = %subst(Fed_Name:1:40);
       ALfdN2 = %subst(Fed_Name:41:60);
       ALmad1 = M_Addr_Line_1;
       ALmad2 = M_Addr_Line_2;
       ALmcty = M_Addr_City;
       ALmst  = M_Addr_State;
       Select;
       When M_Addr_Zip = *blanks;
       Other;
          ALmzip = %Int(M_Addr_Zip);
       Endsl;
       ALpad1 = P_Addr_Line_1;
       ALpad2 = P_Addr_Line_2;
       ALpcty = P_Addr_City;
       ALpst  = P_Addr_State;
       Select;
       When P_Addr_Zip = *blanks;
       Other;
          ALpzip = %Int(P_Addr_Zip);
       Endsl;
       ALtpob = Type_of_Bus;
       If Phone = *blanks;
          ALarea = 0;
          ALphn# = 0;
       Else;
          EvalR Phone = %TrimR(Phone);
          ALarea = %Int(%Subst(Phone:1:3));
          ALphn# = %Int(%Subst(Phone:4:7));
       Endif;
       If Fax = *blanks;
          ALfara = 0;
          ALfax# = 0;
       Else;
          EvalR Fax = %TrimR(Fax);
          ALfara = %Int(%Subst(Fax:1:3));
          ALfax# = %Int(%Subst(Fax:4:7));
       Endif;
       Select;
       When P_Addr_Zip5 = *blanks;
          ALcnty = GetCounty(M_Addr_State:%Int(M_Addr_Zip5));
       Other;
          ALcnty = GetCounty(P_Addr_State:%Int(P_Addr_Zip5));
       Endsl;
       ALsemp = State_RiskID;
       ALsic  = Sic_Code;
       ALnaic = Naics_Code;
       ALue#  = State_Unemp#;
       ALnamp = DBAPerNmFlg;
       ALfdnp = FedPerNmFlg;
       ALncci = 'Y';
       ALstat = 'A';
       ALUser = Q1user;
       ALPgm  = Q1pgm;
       ALDt   = sysDateCymd;
       ALTime = sysTimeHms;
      *** Beg Add ***  04/29/24  *** WC2965 *************************
       ALMail = EmailAddress;
      *** End Add ***  04/29/24  *** WC2965 *************************
       If %Found(Wmall);
          Update pal00;
       Else;
          ALIUsr = Q1user;
          ALIPgm = Q1pgm;
          ALIDt  = sysDateCymd;
          ALITm  = sysTimeHms;
          Write pal00;
       Endif;
       Feod Wmall;
      /End-free

     p WriteWMALP      e
      //----------------------------------------------------------------
      // Writes Large Deductible Info (08/24/21 - Replaced by DefaultRatingFactors)
      //----------------------------------------------------------------
     p WriteWMLDP      b

     d isUseDsDefault  s              1n
     d strBsKey        s             10a
     d dsWMLDL2        ds                  likerec(pld02:*input) inz
      /Free
       // Make sure policy is a large deductible type before writing
       strBsKey = %Editc(ELbscd:'X');
       Chain (ELco#:strBsKey) pbs00;
       If BSlpgm <> 'L';
          Return;
       Endif;
       Reset dsWMLDL2;
       Select;
       When FNpeoF = *blank;
          Chain (ELco#:ELfnd:ELfyr:ELemp#:GetStRptDv(ELco#:ELfnd:ELemp#))
           pld02 dsWMLDL2;
       Other;
          Chain (ELco#:ELfnd:ELfyr:1:0) pld02 dsWMLDL2;
       Endsl;
       isUseDsDefault = %found(wmldl2);
       // Default from Rating and Stat (Emp# = 0)
       Setgt  (ELco#:ELfnd:0:0:0:Rating_State:ELpfdt) pld03;
       Readpe (ELco#:ELfnd:0:0:0:Rating_State) pld03;
       If Not(%Eof);
          LDemp# = ELemp#;
          LDdiv  = ELdiv;
          LDfyr  = ELfyr;
          LDefd1 = ELpfdt;
          If isUseDsDefault;
             LDamP1  = dsWMLDL2.LDamP1;
             LDamP2  = dsWMLDL2.LDamP2;
             LDamG1  = dsWMLDL2.LDamG1;
             LDamG2  = dsWMLDL2.LDamG2;
             LDvara1 = dsWMLDL2.LDvara1;
             LDvara2 = dsWMLDL2.LDvara2;
             LDadP1  = dsWMLDL2.LDadP1;
             LDadP2  = dsWMLDL2.LDadP2;
             LDdes1  = dsWMLDL2.LDdes1;
             LDdes2  = dsWMLDL2.LDdes2;
          Endif;
          LDidt  = sysDateCymd;
          LDiusr = Q1user;
          LDipgm = Q1pgm;
          LDitm  = sysTimeHms;
          LDdt   = LDidt;
          LDtime = LDitm;
          LDpgm  = LDipgm;
          LDuser = LDiusr;
          Write pld03;
          Return;
       Endif;
       // Default from master policy if Rating and Stat doesn't exist
       Chain (ELco#:ELfnd:1:0:ELfyr) pld03;
       If %Found;
          LDemp# = ELemp#;
          LDdiv  = ELdiv;
          LDfyr  = ELfyr;
          LDefd1 = ELpfdt;
          LDidt  = sysDateCymd;
          LDiusr = Q1user;
          LDipgm = Q1pgm;
          LDitm  = sysTimeHms;
          LDdt   = LDidt;
          LDtime = LDitm;
          LDpgm  = LDipgm;
          LDuser = LDiusr;
          Write pld03;
          Return;
       Endif;
      /End-Free

     p WriteWMLDP      e
      //----------------------------------------------------------------
      // WriteWMEMP - writes to Employer Master
      //----------------------------------------------------------------
     p WriteWMEMP      b
     d WriteWMEMP      pi
     d  intNxtEmp      s                   like(EMemp#)
     d  intNxtDiv      s                   like(EMdiv)
     d  charPeoType    s                   like(FNpeof)

      /Free
       eff_Dt7 = %Dec(%Char(%Date(Eff_Date:*Iso0):*Cymd0):7:0);
       //*** Begin Add *** 07/22/22 ************************************
       eff_Dt8 = dtMDYY(eff_Dt7);
       //*** End   Add *** 07/22/22 ************************************
       GetRateSetCode(Rating_State:eff_Dt7);
       charPeoType = GetPeoType();
       //*** beg chg ***** 11/15/21 *************
       //Select;
       //When charPeoType = 'M';
       //   intNxtEmp = getNextEmp#;
       //   intNxtDiv = 0;
       //When charPeoType = 'S';
       //   intNxtEmp = 1;
       //   intNxtDiv = getNextDiv(intNxtEmp);
       //Endsl;
       Select;
       When isQteImp
        And charPeoType = 'S'
        And indPeoNotInQuote = *On;   // PEO is not in quote env
          intNxtEmp = GetNextEmp#;
          intNxtDiv = 0;
       When charPeoType = 'M';
          //*** Beg Chg ***** 10/05/23 ******************************
          //***intNxtEmp = GetNextEmp#;
          //***intNxtDiv = 0;
          If PMovrEmp# <> *blank;
             intNxtEmp = %Int(PMovrEmp#);
             kkEmp# = intNxtEmp;
             intNxtDiv = getNextDiv(intNxtEmp);
          Else;
             intNxtEmp = GetNextEmp#;
             intNxtDiv = 0;
          Endif;
          //*** End Chg ***** 10/05/23 ******************************
       When charPeoType = 'S';
          intNxtEmp = 1;
          intNxtDiv = GetNextDiv(intNxtEmp);
       Endsl;
       //*** end chg ***** 11/15/21 *************

       // Copy PEO Master Policy and overwrite with import values
       //*** beg chg ***** 10/16/21 **************
       //*** Chain emky1 pem02;
       //*** beg chg ***** 11/15/21 **************
       //Chain(n) emky1 pem02;
       //*** end chg ***** 10/16/21 **************
       //Setgt (EMco#:EMfnd:EMemp#:EMdiv:Eff_Dt7) pel02;
       //Readpe (EMco#:EMfnd:EMemp#:EMdiv) pel02;

       // indPeoNotInQuote is *On only if running quote
       // import and PEO is not in quote env.
       If indPeoNotInQuote = *On;
          UserFilesClose();
          UserFilesOvrToLive();
          UserFilesOpen();
       Endif;
       Chain(n) emky1 pem02;
       Setgt (EMco#:EMfnd:EMemp#:EMdiv:Eff_Dt7) pel02;
       Readpe (EMco#:EMfnd:EMemp#:EMdiv) pel02 dsPeoWDELP;
       If indPeoNotInQuote = *On;
          UserFilesClose();
          UserFilesDltOvr();
          UserFilesOpen();
       Endif;
       //*** end chg ***** 11/15/21 **************

       EMemp# = intNxtEmp;
       EMdiv  = intNxtDiv;
       EMefd1 = eff_Dt7;
       EMname = %Subst(Dba_Name:1:40);
       EMNam2 = %Subst(Dba_Name:41:60);
       EMFdNm = %Subst(Fed_Name:1:40);
       EMFdN2 = %Subst(Fed_Name:41:60);
       EMmad1 = M_Addr_Line_1;
       EMmad2 = M_Addr_Line_2;
       EMmcty = M_Addr_City;
       EMmst  = M_Addr_State;
       Clear EMnonm ;
       CLear EMnonp ;
       If M_Addr_Zip = *blank;
          EMmzip = 0;
       Else;
          EMmzip = %Int(M_Addr_Zip);
       Endif;
       EMpad1 = P_Addr_Line_1;
       EMpad2 = P_Addr_Line_2;
       EMpcty = P_Addr_City;
       EMpst  = P_Addr_State;
       If P_Addr_Zip = *blank;
          EMpzip = 0;
       Else;
          EMpzip = %Int(P_Addr_Zip);
       Endif;
       // Set state reporting division flag
       If EMdiv = 0;
          EMstrp = 'Y';
       Else;
          EMstrp = 'N';
       Endif;
       // Right adjust in case the spreadsheet
       //  suppresses leading zeros
       If Fed_Tax_ID = *blanks;
          EMfein = 0;
       Else;
          EvalR Fed_Tax_ID = %TrimR(Fed_Tax_ID);
          EMfein = %Int(Fed_Tax_ID);
       Endif;
       Select;
       When P_Addr_Zip5 = *blanks;
          EMcnty   = GetCounty(M_Addr_State:%Int(M_Addr_Zip5));
       Other;
          EMcnty   = GetCounty(P_Addr_State:%Int(P_Addr_Zip5));
       Endsl;
       EMmpfl   = 'M';
       If EMpad1 <> *blanks and EMmad1 = *blanks;
          EMmpfl   = 'P';
       Endif;
       EMwals   = 'N';
       EMvip    = 'N';
       EMspac   = 'N';
       EMtpob   = Type_of_Bus;
       EMsic    = SIC_Code;
       EMNaic   = NAICS_Code;
       EMlink   = PEO_Link_Code;
       EMnamp   = DBAPerNmFlg;
       EMfdnp   = FedPerNmFlg;
       EMnaic   = Naics_Code;
       EMue#    = State_Unemp#;
       EMsemp   = State_RiskID;
       EMrsk#   = Ncci_RiskID;
       EMstat   = 'A';
       Select;
       When Emp_Lsid <> *blanks;
          Emlsid = Emp_Lsid;
       When RRlsid <> *Blank;
          EMlsid = RRlsid;
       When charPeoType = 'M';
          Emlsid = '4';
       When charPeoType = 'S';
          Emlsid = '2';
       Endsl;
       Select;
       When Exp_Rate_Flg <> *Blank;
          EMexrt = Exp_Rate_Flg;
       When clientID = '150';
          EMexrt = '2';
       When charPeoType = 'M';
          EMexrt = '3';
       When charPeoType = 'S';
          EMexrt = '1';
       Endsl;
       If Phone = *blanks;
          EMarea = 0;
          EMphn# = 0;
       Else;
          EvalR Phone = %TrimR(Phone);
          EMarea = %Int(%Subst(Phone:1:3));
          EMphn# = %Int(%Subst(Phone:4:7));
       Endif;
       If Fax = *blanks;
          EMfaxa = 0;
          EMfax# = 0;
       Else;
          EvalR Fax = %TrimR(Fax);
          EMfaxa = %Int(%Subst(Fax:1:3));
          EMfax# = %Int(%Subst(Fax:4:7));
       Endif;
       EMcust = Customer_ID;
       EMuser = Q1user;
       EMpgm  = Q1pgm;
       EMdt   = sysDateCymd;
       EMtime = sysTimeHms;
       EMiusr = EMuser;
       EMipgm = EMpgm;
       EMidt  = EMdt;
       EMitim = EMtime;
       If charPeoType = 'M';
          EMPeoNmIn = RRNmInDft;
       Endif;
       Clear EMqtfr;
       //*** Beg Add ***** 06/18/25 ***********************
       Clear EMRPeo;
       //*** End Add ***** 06/18/25 ***********************
      *** Beg Add ***  04/29/24  *** WC2965 *************************
       //*** Beg Add ***** 04/28/25 ***********************
       EMMail = EmailAddress;
       //*** End Add ***** 04/28/25 ***********************
       If EMStrp = 'Y' and EMMail = ' ';
          Chain (kkCo#:kkFnd) WMFNL;
           If %Found and FNEmail <> ' ';
              EMMail = FNEmail;
           EndIf;
       //*** Beg Del ***** 04/28/25 ***********************
       // Else;
       //    EMMail = EmailAddress;
       //*** End Del ***** 04/28/25 ***********************
       EndIf;
      *** End Add ***  04/29/24  *** WC2965 *************************
       Write pem01;
       Feod Wmemp;
      /End-Free

     p WriteWMEMP      e
      //----------------------------------------------------------------
      // writeWMNBP - Write new business record
      //----------------------------------------------------------------
     p WriteWMNBP      b
     d WriteWMNBP      pi

      /Free
       // Only write record for state reporting division
       If EMdiv <> 0;
          Return;
       Endif;
       Clear pnb0p;
       NBCo#  = EMco#;
       NBFnd  = EMfnd;
       NBEmp# = EMemp#;
       NBRcdt = sysDateCymd;
       NBEndt = NBRcdt;
       NBIPgm = Q1pgm;
       NBIUsr = Q1user;
       NBITm  = sysTimeHms;
       NBIDt  = sysDateCymd;
       NBuser = Q1user;
       NBpgm  = Q1pgm;
       NBTime = NBitm;
       NBDT   = NBidt;
       Write pnb0p;
      /End-free

     p WriteWMNBP      e
      //----------------------------------------------------------------
      // writeWMRPP - writes Carrier Reporting file
      //----------------------------------------------------------------
     p WriteWMRPP      b
     d WriteWMRPP      pi

      /Free
       // Write a new carrier record for the
       //  state reporting division of an MCP group
       //  if set up in groups to do so
       If ELemp# <> 1 and ELdiv = 0;
          If Carr_Pol# = *Blanks;
             Callp WA200R (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:Q1pgm);
             Return;
          Else;
             RPco#  = ELco#;
             RPfnd  = ELfnd;
             RPemp# = ELemp#;
             RPdiv  = ELdiv;
             RPfyr  = ELfyr;
             RPcarr = GetCarrCode();
             RPcap# = Carr_Pol#;
             RPisdt = sysDateCymd;
             RPipgm = Q1pgm;
             RPiusr = Q1user;
             RPidt  = sysDateCymd;
             RPitm  = sysTimeHms;
             RPpgm  = RPipgm;
             RPuser = RPiusr;
             RPdt   = RPidt;
             RPtime = RPitm;
             Write prp00;
          Endif;
          Return;
       Endif;
       // Copy the carrier record from the state
       //  reporting divison to the new member
       Chain (ELco#:ELfnd:ELemp#:kkdiv:ELfyr) prp00;
       If %found;
          RPdiv  = ELdiv;
          RPipgm = Q1pgm;
          RPiusr = Q1user;
          RPidt  = sysDateCymd;
          RPitm  = sysTimeHms;
          RPpgm  = RPipgm;
          RPuser = RPiusr;
          RPdt   = RPidt;
          RPtime = RPitm;
          Write prp00;
       Endif;
      /End-free

     p WriteWMRPP      e
      //---------------------------------------------------------------------
      // Write Mid Term Add Endorsement
      //---------------------------------------------------------------------
     p WrtMidTermAdd   b
     d WrtMidTermAdd   pi
     d  divOrLocFlag                  1    Const

      /Free
       Select;
       When PolPgNotRun();
          Return;
       When divOrLocFlag = 'D';
          //*** Begin Chg *** 07/22/22 *************************************
          //***SbmMidTermAdd(%Editc(EMco#:'X'):%Editc(EMfnd:'X'):
          //*** %Editc(EMemp#:'X'):%Editc(EMdiv:'X'):
          //*** %Editc(sysDateUsa:'X'):*blanks);
          SbmMidTermAdd(%Editc(EMco#:'X'):%Editc(EMfnd:'X'):
           %Editc(EMemp#:'X'):%Editc(EMdiv:'X'):
           %Editc(eff_Dt8:'X'):*blanks);
          //*** End   Chg *** 07/22/22 *************************************
       When divOrLocFlag = 'L';
          //*** Begin Chg *** 07/22/22 *************************************
          //***SbmMidTermAdd(%Editc(ALco#:'X'):%Editc(ALfnd:'X'):
          //*** %Editc(ALemp#:'X'):%Editc(ALdiv:'X'):
          //*** %Editc(sysDateUsa:'X'):ALelid);
          SbmMidTermAdd(%Editc(ALco#:'X'):%Editc(ALfnd:'X'):
           %Editc(ALemp#:'X'):%Editc(ALdiv:'X'):
           %Editc(eff_Dt8:'X'):ALelid);
          //*** End   Chg *** 07/22/22 *************************************
       Endsl;
      /End-Free

     p WrtMidTermAdd   e
      //---------------------------------------------------------------------
      // Write Mid Term Add Endorsement
      //---------------------------------------------------------------------
     p WrtMidTermDlt   b
     d WrtMidTermDlt   pi
     d  divOrLocFlag                  1    Const

      /Free
       dlt_Dt8 = %dec(%char(%date(Deletion_Date:*iso0):*usa0):8:0);
       Select;
       When PolPgNotRun();
          Return;
       When divOrLocFlag = 'D';
          SbmMidTermDlt(%Editc(EMco#:'X'):%Editc(EMfnd:'X'):
           %Editc(EMemp#:'X'):%Editc(EMdiv:'X'):
           %Editc(dlt_Dt8:'X'):*blanks);
       When divOrLocFlag = 'L';
          SbmMidTermDlt(%Editc(ALco#:'X'):%Editc(ALfnd:'X'):
           %Editc(ALemp#:'X'):%Editc(ALdiv:'X'):
           %Editc(dlt_Dt8:'X'):ALelid);
       Endsl;
      /End-Free

     p WrtMidTermDlt   e

** Error messages
Numeric fields have character information (bad data).                          1
Record type is invalid. It must be a 'H','D', or 'T'.                          2
PEO link code is blank.                                                        3
Invalid Rating State.                                                          4
Employer name is blank.                                                        5
Increased limits is required by set up for master policy.                      6
No address has been provided.                                                  7
Mailing zipcode is required.                                                   8
Mailing Address State is invalid.                                              9
Mailing Address Zip code is zero.                                              10
Debit Credit Factor exceeds min/max for rating state.                          11
Physical zipcode is required.                                                  12
Physical Address State is invalid.                                             13
Physical Address Zip code is zero.                                             14
Location year is required and must be valid.                                   15
FEIN is zero.                                                                  16
Type of Business is invalid or missing.                                        17
SIC code is invalid.                                                           18
Type of Building is invalid.                                                   19
This member is already written in this state.                                  20
Physical address zipcode is invalid.                                           21
Default rate set code not set up in Rating and Statistics.                     22
No Detail record for employer 1.                                               23
No Agent history for employer 1.                                               24
No Vendor history for employer 1.                                              25
NAICS code is invalid.                                                         26
Effective date is not a valid date.                                            27
Number of employees must be greater than zero.                                 28
Cancellation date can not precede the effective date.                          29
The mailing address state and rating state must match.                         30
Mailing address zipcode is invalid.                                            31
Anniversary rating date is not a valid date.                                   32
Anniversary date must be prior to policy effective date.                       33
Increased limits code is invalid.                                              34
Statistics record not set up for Schedule Debit/Credit.                        35
Schedule Debit/Credit must be a 5,4 numeric value.                             36
Statistics record not set up for Deviation 1 Discount.                         37
Deviation 1 Discount must be a 5,2 numeric value.                              38
Statistics record not set up for Deviation 2 Discount.                         39
Deviation 2 Discount must be a 5,2 numeric value.                              40
Exp Mod Factor must be a 5,4 numeric value.                                    41
No division/year exists to add location to.                                    42
Location ID cannot be zero.                                                    43
Building construction year must be YYYY.                                       44
SIC code is required.                                                          45
NAICS code is required.                                                        46
State Unemployment # is required.                                              47
Personal name flag must be 'Y' or 'N'.                                         48
Mailing city can't be blank.                                                   49
Physical city can't be blank.                                                  50
The physical address state and rating state must match.                        51
All divisions must have the same anniversary rate date.                        52
Deletion date is not a valid date.                                             53
Leasing ID is invalid.                                                         54
Cancellation can only be processed for master PEO's.                           55
Experience rating flag is invalid.                                             56
Cancellation not permitted. Policy can't be found.                             57
Cancellation not permitted. Multiple policies exist for link/state.            58
Cancellation not permitted on state reporting division.                        59
Cancellation not permitted on summarized division.                             60
Cancellation not permitted. Future claims exist.                               61
Division is previously cancelled.                                              62
Location can't be located for delete transaction.                              63
Cancellation not permitted if policy page hasn't been run.                     64
Location has been previously deleted.                                          65
Rating state is not set up for a PEO type.                                     66
Invalid employer#/division#.                                                   67
Deletion date must be blank for quote import.                                  68
Invalid Override Employer.                                                     69
Employer# and Override Employer# cannot both be populated.                     70
The state being imported requires an email address to be populated.            71
MCP/Master Policy eff Date greater than the CX date of PEO Master              72
