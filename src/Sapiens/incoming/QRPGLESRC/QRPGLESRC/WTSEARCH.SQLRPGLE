     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define WTSEARCH
    H*********************************************************************
     H/title WTSEARCH - Searches (Employer/Claim/Agent)
    H*********************************************************************
     H* This program performs a search on employers, claims or agents.  It
     H*  will return the RRN for the record selected.
     H*
     H* Entry parameters:
     H*  - Mode: 'E'=Employer search
     H*          'Q'=Employer search (quote only)
     H*          'L'=Employer search (live only)
     H*          'C'=Claim search
     H*          'A'=Agent search
     H*          '*'=All searches (default, if not passed)
     H*  - Call: '2'=Allows for selection of a record
     H*  - RRN: Used to return rrn of select value
     H*          WMEMP, WMCMP or WMAAP
     H*  - Criteria: Data structure used to pass in selection criteria to
     H*               eliminate the need for the criteria screen.
     H*              * * * Use WTSEARCHDS copybook for field definitions.
     H*              * * * Mode must be passed as needed search.
     H*
     H* Created: 02/15/12   C:33   Project: SY266
    H*********************************************************************
     H* Revisions:
     H*
     H* 10/24/25 C:079 Support: 99-119830
     H*                Speed up response when filtering on Carrier Policy#
     H*
     H* 01/03/24 C:300 Project: WC2676
     H*                For Employer Number Search, do not return
     H*                WMEMP rrn, if WDELP record not found
     H*                DSPF: Added scEmpsRRN in SflEmpRslt to save
     H*                 999999999 in case WDELP record not found
     H*
     H* 12/15/23 C:342 Project: WC2676
     H*                WC2676  : Return Funds Process / CS Standards
     H*                Hide Select button when called from WTRFCHK
     H*
     H* 11/06/23 C:333 Project: WC2926
     H*                Policy copy and search enhancements.
     H*                DSPF changes:
     H*                 WTSEARCH - Employer and division range.
     H*
     H* 06/16/20 C:33  Support: 89563
     H*                Add Refresh button
     H*
     H* 06/09/20 C:33  Project: SY420
     H*                Mask SSN
     H*
     H* 12/27/19 C:318 Project: 84648
     H*                Corrected Name population if size is greater than 50
     H*
     H* 02/25/19 C:53  Project: 126WFQ
     H*                Add State Claim # to search screen
     H*
     H* 01/30/19 C:33  Project: SY406
     H*               -Add 'Portal' to search criteria
     H*               -Rename WCMS
     H*
     H* 12/18/18 C:33  Project: SY406
     H*                Use correct environment flag for GetFQLockSts
     H*
     H* 04/02/18 C:33  Project: 150WCF
     H*                Display employer link code for all but 089
     H*
     H* 03/16/17 C:65  Project: 148WAC
     H*                Add employer link code to policy filter screen and
     H*                display in customer ID spot for client 148.
     H*
     H* 01/30/17 C:136 Phaseware: 45256
     H*                Modify OnLoad event of rcdCrit
     H*                so that the fields would only be
     H*                conditioned if Tab 0 was selected.
     H*
     H* 04/25/16 C:62  Project WC2419
     H*                Change Total Incurred to Net Total Incurred
     H*
     H* 11/02/15 C:33  38137
     H*                Update the default search type.
     H*
     H* 09/18/15 C:33  Project: SY360
     H*                Include link for iDARTS in result grid(s).
     H*
     H* 08/31/15 C:33  35971
     H*                Pass the policy envrionment to GetEmpAgt
     H*
     H* 07/16/15 C:306 PHW35967
     H*                Fixed the searching in case of "Begins with".
     H*
     H* 04/02/15 C:33  PHW 34035
     H*                Make sure main group restriction is accounted for.
     H*
     H* 01/26/15 C:062 Project 114WGU
     H*                Pass Quote Type back.
     H*
     H* 12/29/14 C:283 Redmine 4809
     H*                Create the INQ Link for Employer if it is locked by FQ application.
     H*
     H* 12/24/14 C:295 Project: WC2244
     H*                Modified the search criteria to allow search by "Begins with"
     H*                and contains both.
     H*
     H* 06/12/14 C:062 Redmine 4323 (CoveRisk)
     H*                - GrpExpired - Retrieve Expired group
     H*                - If the Group is expired then it will open up in display mode.
     H*
     H* 04/29/14 C:242 Support WC045 85455
     H*                Add Claim Security by file WMCUP in Search
     H*
     H* 04/09/14 C:062 Support: IM114 85273
     H*                Refine logic to show prompt screen instead of results
     H*
     H* 04/05/14 C:279 Project: WC2197
     H*                Modify to consider user authority for edit/inquiry
     H*                mode (project re-implemented).
     H*
     H* 03/27/14 C:290 Support: WA114 85151
     H*                Modified sqlWhereSize field length so that the search
     H*                query will form correctly.
     H*
     H* 03/19/14 C:295 Project: 89WAX
     H*                Modified to add Customer ID as a search criteria on
     H*                policy search screen.
     H*
     H* 02/21/14 C:190 Redmine# 3974
     H*                Pulled out changes of WC2197.
     H*
     H* 02/13/14 C:267 Redmine: 3768
     H*                Modify Quote Type drop down menu options
     H*                by excluding 'Transfer IN' and 'Transfer OUT'
     H*                selection records from WTQSP file.
     H*
     H* 02/12/14 C:191 Support 84736
     H*                Modify to return correct results when using Rating
     H*                State as a search criteria on policy search screen.
     H*
     H* 11/11/13 C:062 Support WC114 84161
     H*                Add 114WIH code.
     H*
     H* 10/09/13 C:295 Support 82970
     H*                - GrpExpired - Retrieve Expired group
     H*                - If the Group is expired then it will open up in display mode.
    H*
     H* 09/13/13 C:062 Support: WC114 83467
     H*                Use date routine instead of bif date function.
     H*
     H* 07/26/13 C:279 Project: WC2197
     H*                Modify to consider user authority for edit/inquiry
     H*                mode.
     H*
     H* 07/11/13 C:189 Redmine: 3363
     H*                Change Claim injury date to DATE data type
     H*                from character field for sorting by date.
     H*
     H* 06/11/13 C:267 Redmine: 1637 (Haifa Kealy)
     H*                Change Policy cancel date to DATE data type
     H*                from numeric field for sorting by date.
     H*                Same for Inception date field.
     H*
     H* 04/12/13 C:33  Redmine: 2644
     H*                Change New vs. Renewal qualifier.
     H*
     H* 03/25/13 C:279 Redmine: 2577
     H*                Modify to not access PDSQTE items if Claims only.
    H*********************************************************************
     FWTSEARCH  CF   E             WORKSTN Handler('PROFOUNDUI(HANDLER)')
     F                                     sfile(sflResults:scSflRRN)
     F                                     sfile(sflAgtRslt:scSflAgtRR)
     F                                     sfile(sflClmRslt:scSflClmRR)
     F                                     sfile(sflClmRsl1:scSflClmRR)
     F                                     sfile(sflEmpRslt:scSflEmpRR)
     Fwmemp     if   e             disk    rename(pem01:pem0p)
     Fwmcmp     if   e             disk    rename(pcm01:pcm0p)
     Fwmaap     if   e             disk    rename(paa01:paa0p)
     Fwmeml     if   e           k disk    rename(pem01:pem00)
     Fwmemlq    if   e           k disk    rename(pem01:pem0q)  usropn
     Fwdell     if   e           k disk    rename(pel01:pel00)
     F                                     infds(ELfds)
     Fwdellq    if   e           k disk    rename(pel01:pel00q) usropn
     F                                     infds(ELfdsQ)
     Fwdell2    if   e           k disk    rename(pel01:pel02)
     Fwdell2q   if   e           k disk    rename(pel01:pel02q) usropn
     Fwaqtl7    if   e           k disk    rename(pqt01:pqt07)
     Fwtqsl1    if   e           k disk
     Fwmrsl     if   e           k disk    rename(prs01:prs00)
     Fsdacl     if   e           k disk    rename(pac01:pac00)
     Fwmxrl     if   e           k disk    rename(pxr01:pxr00)
     Fwmcul     if   e           k disk    rename(pcu01:pcu00)
     /*** Beg Add ***** 11/06/23 ********************************
     Fsmupl     if   e           k disk
     /*** End Add ***** 11/06/23 ********************************
     D*
     D psDs          esds                  extName(SZQ1P)
     D ELfds           ds
     D  ELrrn                397    400b 0
     D ELfdsQ          ds
     D  ELrrnQ               397    400b 0
     D*
     D/copy sCopyDsPhx
     D/copy sprGenPop
     D strSqlCursor    s           5000
     D strRestrGrpSQL  s           4000
     D xxMode          s                   like(@@Mode) inz('*')
     D xxCall          s                   like(@@Call) inz
     D xxUserCo#       s                   like(EMco#)
     D xxUserGrp       s                   like(EMfnd)
     D xxUserEmp#      s                   like(EMemp#)
     D xxUserDiv       s                   like(EMdiv)
     D xxReason        s             65    inz(*blanks)
     D xxyear          s              3  3 inz(*zeros)
     D xxEnv           s              1
     D nullDate        s               d   inz
     D intDftTab       s              1  0 inz
     D intMonitorSts   s              5  0 inz
     D isSearchMode    s              1n   inz
     D isSelectMode    s              1n   inz
     D isInquiryOnly   s              1n   inz
     D isUseGenResult  s              1n   inz
     D isFilter        s              1n   inz
     D isExitPoint     s              1n   inz
     D isEmpSearch     s              1n   inz
     D isClmSearch     s              1n   inz
     D isAgtSearch     s              1n   inz
     D isEmpSelected   s              1n   inz
     D isClmSelected   s              1n   inz
     D isAgtSelected   s              1n   inz
     D isResultsOnly   s              1n   inz
     D isQualified     s              1n   inz
     D isGrpRestr      s              1n   inz
     D isNoGrpRestr    s              1n   inz
     D isMultGrpRestr  s              1n   inz
     D isSnglGrpRestr  s              1n   inz
     D isEmpRestr      s              1n   inz
     D isNoEmpRestr    s              1n   inz
     D isMultEmpRestr  s              1n   inz
     D isSnglEmpRestr  s              1n   inz
     D isDivRestr      s              1n   inz
     D isNoDivRestr    s              1n   inz
     D is1QteSts       s              1n   inz
     D isClmOnly       s               n
     D isAuthEr        s               n
     D isPolUpd        s               n
     D isQteUpd        s               n
     D isClmUpd        s               n
     D isPolInq        s               n
     D isQteInq        s               n
     D isClmInq        s               n
     D inUTYP          s              1
     D inULVL          s              3
     D inCTP1          s              1
     D inCLV1          s              3
     D inEXEC          s              1
     D outRes          s              1
     D inCTP2          s              1
     D inCTP3          s              1
     D inCTP4          s              1
     D inCTP5          s              1
     D inCTP6          s              1
     D inCLV2          s              3
     D inCLV3          s              3
     D inCLV4          s              3
     D inCLV5          s              3
     D inCLV6          s              3
     D inCTP7          s              1
     D inCTP8          s              1
     D inCTP9          s              1
     D inCTP0          s              1
     D inCLV7          s              3
     D inCLV8          s              3
     D inCLV9          s              3
     D inCLV0          s              3
     D keySDACL        s             10
     D svMode          s              1
     D flgUsrGrpRestr  s              1
     D flgUsrEmpRestr  s              1
     D flgUsrDivRestr  s              1
     D sqlWhereSize    s            600
     D #ofQuoteTypes   c                   50
     D aryEmpQteTp     s              1    dim(#ofQuoteTypes)
     D aryGroups       s              3  0 dim(999)
     D outCltID        s              3
     D strDartsKey     s             40
     D charKeyEnv      s              1
     D*** Beg Add *** 10/24/25 ************************************************
     D sqlJoinSize     s            200
     D*** End Add *** 10/24/25 ************************************************

     D/copy WTSearchDs

     D**xxCriteria       ds                  likeDS(dsSrchCrit) inz
     DnullCriteria     ds                  likeDS(dsSrchCrit) inz

     DdartsKeyEmp      ds
     D ELco#                   1      3  0
     D ELfnd                   4      6  0
     D ELemp#                  7     15  0
     D ELdiv                  16     20  0
     D keyEnv                 40     40
     DdartsKeyClm      ds
     D CMco#                   1      3  0
     D CMfnd                   4      6  0
     D CMfyr                   7      9  0
     D CMcase                 10     16  0

     D*entry
     DWTSearchEntry    pr                  ExtPgm('WTSEARCH')
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)
     DWTSearchEntry    pi
     D @@mode                         1
     D @@call                         1
     D @@rrn                          9
     D @@criteria                          likeds(dsSrchCrit)

     DCheckSecurity    pr                  EXTPGM('SRCCSC')
     D inUTYP                         1    const
     D inULVL                         3    const
     D inCTP1                         1    const
     D inCLV1                         3    const
     D inEXEC                         1    const
     D outRes                         1
     D inCTP2                         1    const
     D inCTP3                         1    const
     D inCTP4                         1    const
     D inCTP5                         1    const
     D inCTP6                         1    const
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

     DClearSubFiles    pr
     DClrAgtCriteria   pr
     DClrClmCriteria   pr
     DClrEmpCriteria   pr
     DClrQckCriteria   pr
     DConcatAddr       pr           125
     D inAddress1                    40    const
     D inAddress2                    40    const
     D inCity                        25    const
     D inState                        2    const
     D inZipCode                      9  0 const
     DCreateCursors    pr
     DCreateSqlWhere   pr
     D sqlWhere                                  like(sqlWhereSize)
     D sqlConditions                       const like(sqlWhereSize)
     D sqlConditional                 3    const options(*nopass)
     DCriteriaError    pr             1n
     DDefineSearch     pr
     DDftSrchType      pr             1
     DDisplaySubFile   pr
     DGetEmpAccess     pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     DGetGrpAccess     pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     DInqUpdAgt        pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inAgt#                         5  0 const
     DInqUpdClm        pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const
     D inCase                         7  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     DInqUpdEmp        pr             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     DLoadSFL          pr
     DPgmStr           pr
     DProcess          pr             1n
     DProcessResults   pr             1n
     DQualifiedAgt     pr              n
     D inCo#                          3s 0 const
     D inFnd                          3s 0 const
     D inAgt#                         5s 0 const
     DQualifiedClm     pr              n
     D inCo#                          3s 0 const
     D inFnd                          3s 0 const
     D inFyr                          3s 0 const
     D inCase                         7s 0 const
     D inEmp#                         9s 0 const
     D inDiv                          5s 0 const
     DQualifiedEmp     pr              n
     D inCo#                          3s 0 const
     D inFnd                          3s 0 const
     D inEmp#                         9s 0 const
     D inDiv                          5s 0 const
     D inEnv                          1    const options(*nopass)
     DRecordSelected   pr             1n
     DSearchAgt        pr
     DSearchClm        pr
     DSearchEmp        pr
     DSrchGroupAuth    pr             1n
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     DSrchUserAuth     pr              n
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inYear                         3  0 const
     D inCase                         7  0 const
     DshowPrompt       s               n
     D WtSearchDa      ds                  dtaara('WTSEARCHDA')
     D  prompt                 1      1
     d GetClientID     pr                  extpgm('RTVCLTID')
     d outCltID                       3

       //*** Beg Add ***** 12/15/23 ******************************
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1
     d Callpgm         s              1
       //*** End Add ***** 12/15/23 ******************************

     D/copy sPrDate
     D/copy sPrModule
     D/copy sPrString
     D/copy sPrSecure
      ***  Begin Add  ***  06/09/20  *******************
     D/copy sPrMask
      ***  End   Add  ***  06/09/20  *******************
     D/copy wPrUsrPrms
     D/copy sPrAtrium
     D/copy wPrGroup
     D/copy wPrEmplr
     D/copy wPrCarrier
     D/copy wPrAgent
     D/copy sPrTable

      /Free

       If %parms >= 1 and @@Mode <> *blank;
          xxMode = @@Mode;
       Endif;

       If %parms >= 2;
          xxCall = @@Call;
       Endif;

       If %parms >= 4;
          dsSrchCrit = @@Criteria;
       Endif;

       PgmStr();

       If isAuthEr = *off;
       Monitor;
       If dsSrchCrit <> nullCriteria And showPrompt = *Off;
          isResultsOnly = *on;
          DefineSearch();
          ProcessResults();
          isExitPoint = not isFilter;
          isResultsOnly = not isFilter;
       Endif;
       Dow not isExitPoint;
          isExitPoint = Process();
       Enddo;
       On-error;
          intMonitorSts = %status;
          scMonErr = 'Error status code: ' + %editc(intMonitorSts:'X');
          Exfmt rcdError;
       Endmon;
       Endif;

       *inLr = *On;
       Return;

      /End-Free

     PClearSubFiles    b
     P*====================================================================
     P* Clear all sub-files
     P*====================================================================
     DClearSubFiles    pi

      /Free
       scSflRRN   = 0;
       scSflAgtRR = 0;
       scSflClmRR = 0;
       scSflEmpRR = 0;
       scSflDsp = *off;
       scSflDspCt = *off;
       scClrSfl = *on;
       Write rcdResult;
       Write rcdAgtRslt;
       Write rcdClmRslt;
       Write rcdClmRsl1;
       Write rcdEmpRslt;
       scClrSfl = *off;
      /End-Free

     PClearSubFiles    e

     PClrAgtCriteria   b
     P*====================================================================
     P* Clear agent criteria
     P*====================================================================
     DClrAgtCriteria   pi

      /Free
       Clear scAgtName;
       Clear scAgtGrp;
       Clear scAgt#;
       Clear scAgtGrpSt;
      /End-Free

     PClrAgtCriteria   e

     PClrClmCriteria   b
     P*====================================================================
     P* Clear claim criteria
     P*====================================================================
     DClrClmCriteria   pi

      /Free
       If not isGrpRestr;
          Clear scClmGrp;
       Endif;
       Clear scClmYr;
       Clear scClmCase;
       Clear scClmSS#;
       Clear scClmAccDt;
       Clear scClmLnam;
       Clear scClmFnam;
       If not isEmpRestr;
          Clear scClmEmp#;
       Endif;
       If not isDivRestr;
          Clear scClmDiv;
       Endif;
       Clear scClmLoc;
       Clear scClmEmpNm;
       Clear scClmStat;
       Clear scClmUser;
       Clear scClmLtFl;
       Clear scClmJrSt;
       Clear scClmSCL#;
      /End-Free

     PClrClmCriteria   e

     PClrEmpCriteria   b
     P*====================================================================
     P* Clear employer criteria
     P*====================================================================
     DClrEmpCriteria   pi

      /Free
       chEmpFQ = %trim(chEmpFQ) + ',' +
        'FastQuote';
       chEmpFQ = %trim(chEmpFQ) + ',' +
        GetModuleProperty('Portal Module':'Name');
       chEmpFQ = %trim(chEmpFQ) + ',' +
        'PolicyGO';
       cvEmpFQ = ',F,P,W';
       Clear scEmpName;
       If not isGrpRestr;
          Clear scEmpGrp;
       Endif;
       If not isEmpRestr;
          Clear scEmpEmp#;
       Endif;
       If not isDivRestr;
          Clear scEmpDiv;
       Endif;
       Clear scEmpAgt#;
       Clear scEmpAgtNm;
       Clear scEmpCarr;
       Clear scEmpFein;
       Clear scEmpSt#;
       Clear scEmpAddr;
       Clear scEmpFQ;
       Clear scEmpNB;
       Clear scEmpFrDt;
       Clear scEmpToDt;
       Clear scEmpQteTp;
       Clear scRateSt;
       Clear scEmpUnder;
       Clear scEmpPhone;
       If not is1QteSts;
          Clear scEmpQteSt;
       Endif;
       Clear scCustId;
       Clear scEmpLink;
      /End-Free

     PClrEmpCriteria   e

     PClrQckCriteria   b
     P*====================================================================
     P* Clear quick search criteria
     P*====================================================================
     DClrQckCriteria   pi

      /Free
       Clear scSrchName;
       Clear scClmLNam0;
       Clear scClmFNam0;
       //*** Beg Chg ***** 11/06/23 ***************************************
       //***scSrchType = DftSrchType;
       If UPdstab <> 'Q';
          scSrchType = DftSrchType;
       Endif;
       //*** End Chg ***** 11/06/23 ***************************************
      /End-Free

     PClrQckCriteria   e

     PConcatAddr       b
     P*====================================================================
     P* Concatenate addresses into a single field
     P*====================================================================
     DConcatAddr       pi           125
     D inAddress1                    40    const
     D inAddress2                    40    const
     D inCity                        25    const
     D inState                        2    const
     D inZipCode                      9  0 const

     D outAddress      s            125
     D outZipCode      s             10
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
     PConcatAddr       e

     PCreateCursors    b
     P*====================================================================
     P* Create cursors for SQL
     P*====================================================================
     DCreateCursors    pi
      /Free
       Select;
       When isEmpSelected = *on;
          SearchEmp();
       When isClmSelected = *on;
          SearchClm();
       When isAgtSelected = *on;
          SearchAgt();
       Endsl;
      /End-Free
     PCreateCursors    e

     PCreateSqlWhere   b
     P*====================================================================
     P* Create where selections for SQL statements
     P*====================================================================
     DCreateSqlWhere   pi
     D sqlWhere                                  like(sqlWhereSize)
     D sqlConditions                       const like(sqlWhereSize)
     D sqlConditional                 3    const options(*nopass)

     D strConditional  s              3    inz('and')

      /Free
       If %parms >= 3 and sqlConditional <> *blanks;
          strConditional = sqlConditional;
       Endif;

       If sqlConditions <> *blanks;

          Select;
          When %subst(sqlWhere:1:5) = 'Where';
             sqlWhere = %trim(sqlWhere) + ' ' + %trim(sqlConditional);
          Other;
             sqlWhere = %trim(sqlWhere) + 'Where';
          Endsl;

          sqlWhere = %trim(sqlWhere) + ' ' + sqlConditions;

       Endif;

      /End-Free
     PCreateSqlWhere   e

     PCriteriaError    b
     P*====================================================================
     P* Load the results SFL using SQL
     P*====================================================================
     DCriteriaError    pi             1n

     DisError          s              1n   inz
     Di1               s              3  0

      /Free

       Exsr ClearErrors;

       // Search is not valid
       Select;
       //Tab 0
       When scSrchTab = 0 and (scSrchType = *blank or scSrchType = '*');
          isError = *on;
          msSrchType = 'No search selection made.';
          erSrchType = *on;
       When scSrchTab = 0 and scSrchCrit = *blank;
          isError = *on;
          msContType = 'No search selection made for Name Contains/Begin with.';
          erContType = *on;
       //Tab 1
       When scSrchTab = 1 and not isEmpSearch;
          isError = *on;
          msSearch = 'Policy search not valid in this mode.';
          erSearch = *on;
       When scSrchTab = 1 and not SrchGroupAuth(xxUserCo#:scEmpGrp);
          isError = *on;
          msEmpGrp = 'Not authorized to group.';
          erEmpGrp = *on;
       When scSrchTab = 1 and scSrchCrt2 = *blank;
          isError = *on;
          msContType = 'No search selection made for Name Contains/Begin with.';
          erContType = *on;
       //Tab 2
       When scSrchTab = 2 and not isClmSearch;
          isError = *on;
          msSearch = 'Claim search not valid in this mode.';
          erSearch = *on;
       When scSrchTab = 2 and not SrchGroupAuth(xxUserCo#:scClmGrp);
          isError = *on;
          msClmGrp = 'Not authorized to group.';
          erClmGrp = *on;
       When scSrchTab = 2 and scSrchCrt3 = *blank;
          isError = *on;
          msContType = 'No search selection made for Name Contains/Begin with.';
          erContType = *on;
       //Tab 3
       When scSrchTab = 3 and not isAgtSearch;
          isError = *on;
          msSearch = 'Agent search not valid in this mode.';
          erSearch = *on;
       When scSrchTab = 3 and not SrchGroupAuth(xxUserCo#:scAgtGrp);
          isError = *on;
          msAgtGrp = 'Not authorized to group.';
          erAgtGrp = *on;
       When scSrchTab = 3 and scSrchCrit = *blank ;
          isError = *on;
          msContType = 'No search selection made for Name Contains/Begin with.';
          erContType = *on;
       Endsl;

       //*** Beg Add ***** 11/06/23 *********************************
       // Employer Number Range
       If (scEmpLoRng <> *zeros and scEmpUpRng <> *zeros)
        and scEmpUpRng < scEmpLoRng;
          isError = *on;
          erEmpRange = *on;
       Endif;
       If (scDivLoRng <> *blank and scDivUpRng <> *blank)
        and scDivUpRng < scDivLoRng;
          isError = *on;
          erDivRange = *on;
       Endif;
       //*** End Add ***** 11/06/23 *********************************
       // Policy Date Range
       If scEmpFrDt <> *zero or scEmpToDt <> *zero;
          If scEmpFrDt <> *zero and DtCheck(scEmpFrDt) = *zero;
             isError = *on;
             msEmpFrDt = 'WCR0500';
             erEmpFrDt = *on;
          Else;
             scEmpFrDt = DtCheck(scEmpFrDt);
          Endif;
          If scEmpToDt <> *zero and DtCheck(scEmpToDt) = *zero;
             isError = *on;
             msEmpToDt = 'WCR0500';
             erEmpToDt = *on;
          Else;
             scEmpToDt = DtCheck(scEmpToDt);
          Endif;
          If not isError and CvtToDate(scEmpToDt) < CvtToDate(scEmpFrDt);
             isError = *on;
             msEmpFrDt = 'WCR0529';
             erEmpFrDt = *on;
             msEmpToDt = 'WCR0529';
             erEmpToDt = *on;
          Endif;
       Endif;

       // Claim Accident Date
       If scClmAccDt <> *zeros and DtCheck(scClmAccDt) = *zero;
          isError = *on;
          msClmAccDt = 'WCR0500';
          erClmAccDt = *on;
       Else;
          scClmAccDt = DtCheck(scClmAccDt);
       Endif;

       If scEmpQteTp <> *blanks;
          Exsr LoadAryEmpQteTp;
       Endif;

       Return isError;

       // Clear all error indicators
       Begsr ClearErrors;

       Clear erEmpFrDt;
       Clear erEmpToDt;
       Clear erClmAccDt;
       Clear erAgtGrp;
       Clear erEmpGrp;
       Clear erClmGrp;
       //*** Beg Add ***** 11/06/23 *********************************
       Clear erEmpRange;
       Clear erDivRange;
       //*** End Add ***** 11/06/23 *********************************

       Endsr;

       // Load Quote Type Array from comma separated screan value
       Begsr LoadAryEmpQteTp;

       For i1 = 1 to #ofQuoteTypes;
          aryEmpQteTp(i1) = %subst(scEmpQteTp:i1*2-1:1);
          If aryEmpQteTp(i1) = *blank;
             aryEmpQteTp(i1) = '¢';
          Endif;
       Endfor;

       Endsr;

      /End-Free
     PCriteriaError    e

     PDefineSearch     b
     P*====================================================================
     P* Define search selected
     P*====================================================================
     DDefineSearch     pi
      /Free
       isAgtSelected = *off;
       isClmSelected = *off;
       isEmpSelected = *off;
       Select;
       When isResultsOnly and isEmpSearch;
          isEmpSelected = *on;
          Exsr ExtractEmpCrit;
       When isResultsOnly and isClmSearch;
          isClmSelected = *on;
          Exsr ExtractClmCrit;
       When isResultsOnly and isAgtSearch;
          isAgtSelected = *on;
          Exsr ExtractAgtCrit;
       When scSrchTab = 0 and scSrchType = 'E';
          isEmpSelected = *on;
          scEmpName = scSrchName;
          scSrchCrt2 = scSrchCrit;
       When scSrchTab = 0 and scSrchType = 'C';
          isClmSelected = *on;
          //scClmName = scSrchName;
          scClmLNam = scClmLNam0;
          scClmFNam = scClmFNam0;
          scSrchCrt3 = scSrchCrit;
       When scSrchTab = 0 and scSrchType = 'A';
          isAgtSelected = *on;
          scAgtName = scSrchName;
       When scSrchTab = 1;
          isEmpSelected = *on;
          scSrchType = 'E';
          scSrchName = scEmpName;
             scSrchCrit = scSrchCrt2;
       When scSrchTab = 2;
          isClmSelected = *on;
          scSrchType = 'C';
          scClmLNam0 = scClmLNam;
          scClmFNam0 = scClmFNam;
             scSrchCrit = scSrchCrt3;
       When scSrchTab = 3;
          isAgtSelected = *on;
          scSrchType = 'A';
          scSrchName = scAgtName;
       Endsl;

       Select;
       When isEmpSelected;
          scCSVName = 'Employer Search';
       When isClmSelected;
          scCSVName = 'Claim Search';
       When isAgtSelected;
          scCSVName = 'Agent Search';
       Endsl;

       // get live records only if Claims only
       If isClmOnly = *on;
          scEmpQteSt = 'L';
       Endif;

       // Extract agent criteria from the entry parms
       Begsr ExtractAgtCrit;
          scAgtGrp   = dsAgtGrp;
          scAgtName  = dsAgtName;
          scAgt#     = dsAgt#;
          scAgtProdN = dsAgtProdN;
          scAgtProd# = dsAgtProd#;
          scAgtStat  = dsAgtStat;
          scAgtGrpSt = dsAgtGrpSt;
          isGrpRstr = scAgtGrp <> *zero;
       Endsr;

       // Extract claim criteria from the entry parms
       Begsr ExtractClmCrit;
          scClmGrp   = dsClmGrp;
          scClmYr    = dsClmYr;
          scClmCase  = dsClmCase;
          scClmSS#   = dsClmSS#;
          scClmAccDt = dsClmAccDt;
          scClmLNam  = dsClmLName;
          scClmFNam  = dsClmFName;
          scClmEmp#  = dsClmEmp#;
          scClmDiv   = dsClmDiv;
          scClmLoc   = dsClmLoc;
          scClmEmpNm = dsClmEmpNm;
          scClmStat  = dsClmStat;
          scClmUser  = dsClmUser;
          scClmLtFl  = dsClmLtFl;
          scClmJrSt  = dsClmJrSt;
          scClmSCL#  = dsClmSCL#;
          isGrpRstr = scClmGrp <> *zero;
       Endsr;

       // Extract employer criteria from the entry parms
       Begsr ExtractEmpCrit;
          scEmpName  = dsEmpName;
          scEmpGrp   = dsEmpGrp;
          scEmpEmp#  = dsEmpEmp#;
          scEmpDiv   = dsEmpDiv;
          scEmpAgt#  = dsEmpAgt#;
          scEmpAgtNm = dsEmpAgtNm;
          scEmpCarr  = dsEmpCarr;
          scEmpFein  = dsEmpFein;
          scEmpSt#   = dsEmpSt#;
          scEmpAddr  = dsEmpAddr;
          scEmpFQ    = dsEmpFQ;
          scEmpNB    = dsEmpNB;
          scEmpFrDt  = dsEmpFrDt;
          scEmpToDt  = dsEmpToDt;
          scEmpQteTp = dsEmpQteTp;
          scRateSt   = dsRateSt;
          scEmpUnder = dsEmpUnder;
          scEmpQteSt = dsEmpQteSt;
          scCustid = dsCustid;
          scEmpLink = dsEmpLink;
          isGrpRstr = scEmpGrp <> *zero;
       Endsr;
      /End-Free
     PDefineSearch     e

     PDftSrchType      b
     P*====================================================================
     P* Return the default search type
     P*====================================================================
     DDftSrchType      pi             1

      /Free
       Select;
       When xxMode = '*';
          Return *blank;
       When isEmpSearch;
          Return 'E';
       When isClmSearch;
          Return 'C';
       When isAgtSearch;
          Return 'A';
       Endsl;

      /End-free

     PDftSrchType      e

     PGetEmpAccess     b
     P*====================================================================
     P* Return the employer access flag
     P*====================================================================
     DGetEmpAccess     pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     DgrpAccess        s              1
     DempAccess        s              1
      /Free

       Select;
       When isSelectMode;
          Return 'D';
       When isGrpRestr and isNoEmpRestr;
          Return 'C';
       When isNoGrpRestr and isNoEmpRestr;
          Return 'C';
       Endsl;

       Callp GroupAuth(inCo#:inFnd:q1User:grpAccess);

       Select;
       When isEmpRestr;
          empAccess = 'A';
       When isNoEmpRestr;
          empAccess = 'A';
       When isSnglEmpRestr or isMultEmpRestr;
          Callp EmpAuth(inCo#:inFnd:inEmp#:q1User:empAccess);
       Endsl;

       Select;
       When grpAccess = 'I' or empAccess = 'I';
          Return 'D';
       When grpAccess = 'A' and empAccess = 'A';
          Return 'C';
       Endsl;

       Return 'D';

      /End-free
     PGetEmpAccess     e

     PGetGrpAccess     b
     P*====================================================================
     P* Return the group access flag
     P*====================================================================
     DGetGrpAccess     pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const

     DoutAccess        s              1
      /Free

       Select;
       When isSelectMode;
          Return 'D';
       When isGrpRestr;
          Return 'C';
       When isNoGrpRestr;
          Return 'C';
       Endsl;

       Callp GroupAuth(inCo#:inFnd:q1User:outAccess);

       Select;
       When outAccess = 'I';
          Return 'D';
       When outAccess = 'A';
          Return 'C';
       Endsl;

       Return 'D';

      /End-free
     PGetGrpAccess     e

     PInqUpdAgt        b
     P*====================================================================
     P* Return the Inquiry or Update call flag for the agent
     P*====================================================================
     DInqUpdAgt        pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inAgt#                         5  0 const

     DgrpAccess        s              1
      /Free

       grpAccess = GetGrpAccess(inCo#:inFnd);

       Select;
       When isInquiryOnly;
          Return 'D';
       Other;
          Return grpAccess;
       Endsl;

      /End-free
     PInqUpdAgt        e

     PInqUpdClm        b
     P*====================================================================
     P* Return the Inquiry or Update call flag for the claim
     P*====================================================================
     DInqUpdClm        pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inFyr                          3  0 const
     D inCase                         7  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     DgrpAccess        s              1
     DempAccess        s              1
     DgrpExpired       s              1n   Inz
      /Free

       grpAccess = GetGrpAccess(inCo#:inFnd);
       empAccess = GetEmpAccess(inCo#:inFnd:inEmp#:inDiv);
       grpExpired = ExpiredGrp (inCo#:inFnd);

       Select;
       When isInquiryOnly;
          Return 'D';
       When grpAccess = 'D' or empAccess = 'D';
          Return 'D';
       When grpExpired = *On ;
          Return 'D';
       Other;
          Return 'C';
       Endsl;

      /End-free
     PInqUpdClm        e

     PInqUpdEmp        b
     P*====================================================================
     P* Return the Inquiry or Update call flag for the employer
     P*====================================================================
     DInqUpdEmp        pi             1
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const

     DgrpAccess        s              1
     DempAccess        s              1
     DGrpExpired       s              1n   Inz
      /Free

       grpAccess = GetGrpAccess(inCo#:inFnd);
       empAccess = GetEmpAccess(inCo#:inFnd:inEmp#:inDiv);
       GrpExpired = ExpiredGrp (inCo#:inFnd);

       Select;
       When isInquiryOnly;
          Return 'D';
       When grpAccess = 'D' or empAccess = 'D';
          Return 'D';
       When GrpExpired = *ON ;
          Return 'D';
       Other;
          Return 'C';
       Endsl;

      /End-free
     PInqUpdEmp        e

     PDisplaySubFile   b
     P*====================================================================
     P* Display appropriate result subfile
     P*====================================================================
     DDisplaySubFile   pi
      /Free
       If isUseGenResult;
          isAgtSelected = *off;
          isClmSelected = *off;
          isEmpSelected = *off;
       Endif;

       scSflRRN   = 0;
       scSflAgtRR = 0;
       scSflClmRR = 0;
       scSflEmpRR = 0;
       scSflDsp = *on;
       scSflDspCt = *on;
       If scResultNm = *blanks;
          scResultNm = '<No name available>';
       Endif;
       Select;
       When isEmpSelected;
          Exfmt rcdEmpRslt;
       When isClmSelected;
       If outCltID= '126';
          Exfmt rcdClmRsl1;
       Else;
          Exfmt rcdClmRslt;
       Endif;
       When isAgtSelected;
          Exfmt rcdAgtRslt;
       Other;
          Exfmt rcdResult;
       Endsl;

       Return;

      /End-Free
     PDisplaySubFile   e

     PLoadSFL          b
     P*====================================================================
     P* Load the results SFL using SQL
     P*====================================================================
     DLoadSFL          pi

     DstrCommand       s            255

     DQCMDEXC          pr                  ExtPgm('QCMDEXC')
     D command                             const like(strCommand)
     D length                        15  5 const

     Drrn              s              9  0
     Dstr9999Msg       s            256
     Dxx9999rsp        s               n

     d agtDS           ds                  inz
     d  agtName                      40
     d  agtCo#                        3s 0
     d  agtFnd                        3s 0
     d  agtAgt#                       5s 0
     d  agtRRN                        9s 0

     d clmDS           ds                  inz
     d  clmLastName                  40
     d  clmFirstName                 60
     d  clmMiddleInit                40
     d  clmSS#                        9
     d  clmState                      2
     d  clmDtOfInjury                 7s 0
     d  clmLostTime                   1
     d  clmStatus                     1
     d  clmDenied                     1
     d  clmControvert                 1
     d  clmAdjustor                   3
     d  clmRemain01                   9s 2
     d  clmRemain02                   9s 2
     d  clmRemain03                   9s 2
     d  clmRemain04                   9s 2
     d  clmRemain05                   9s 2
     d  clmRemain06                   9s 2
     d  clmRemain07                   9s 2
     d  clmRemain08                   9s 2
     d  clmRemain09                   9s 2
     d  clmRemain10                   9s 2
     d  clmPayment01                  9s 2
     d  clmPayment02                  9s 2
     d  clmPayment03                  9s 2
     d  clmPayment04                  9s 2
     d  clmPayment05                  9s 2
     d  clmPayment06                  9s 2
     d  clmPayment07                  9s 2
     d  clmPayment08                  9s 2
     d  clmPayment09                  9s 2
     d  clmPayment10                  9s 2
     d  clmEmp#                       9s 0
     d  clmDiv                        5s 0
     d  clmCo#                        3s 0
     d  clmFnd                        3s 0
     d  clmFyr                        3s 0
     d  clmCase                       7s 0
     d  clmRRN                        9s 0
     d  clmACL#                      25

     d empDS           ds                  inz
     d  empName                      40
     d  empName2                     60
     d  empFedName                   40
     d  empFedName2                  60
     d  empMailAddr1                 40
     d  empMailAddr2                 40
     d  empMailCity                  25
     d  empMailSt                     2
     d  empMailZip                    9s 0
     d  empPhysAddr1                 40
     d  empPhysAddr2                 40
     d  empPhysCity                  25
     d  empPhysSt                     2
     d  empPhysZip                    9s 0
     d  empCo#                        3s 0
     d  empFnd                        3s 0
     d  empEmp#                       9s 0
     d  empDiv                        5s 0
     d  empCust                      20
     d  empLink                      15
     d  empRRN                        9s 0
     d  empEnv                        1


      /Free
       Select;
       When isEmpSelected and isClmOnly = *off;
          strCommand = 'OVRDBF FILE(WDELLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WDELLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WDELLQ;
          clear strCommand;
          strCommand = 'OVRDBF FILE(WDELL2Q) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WDELL2Q)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WDELL2Q;
          clear strCommand;
          strCommand = 'OVRDBF FILE(WMEMLQ) TOFILE(' +
           %trim(RtvQuoteLib('*QTE')) + '/WMEMLQ)';
          Callp QCMDEXC(strCommand:%len(strCommand));
          Open WMEMLQ;
       Endsl;

       Exec SQL Prepare sql1 From :strSqlCursor;
       Exec SQL Declare cursor1 Cursor For sql1;
       Exec SQL Open cursor1;
       Dou SQLSTT <> '00000';
          Select;
          When isEmpSelected;
             Exsr EmpSql;
             isQualified = QualifiedEmp(empCo#:empFnd:empEmp#:empDiv:empEnv);
          When isClmSelected;
             Exsr ClmSql;
             isQualified = QualifiedClm(clmCo#:clmFnd:clmFyr:clmCase:
              clmEmp#:clmDiv);
          When isAgtSelected;
             Exsr AgtSql;
             isQualified = QualifiedAgt(agtCo#:agtFnd:agtAgt#);
          Endsl;
          Select;
          When SQLSTT <> '00000';
             isQualified = *off;
          Endsl;
          If isQualified;
             scSflRRN   += 1;
             scSflAgtRR += 1;
             scSflClmRR += 1;
             scSflEmpRR += 1;
             Write SflResults;
             Write SflAgtRslt;
             Write SflClmRslt;
             Write SflClmRsl1;
             Write SflEmpRslt;
          Endif;
          If scSflRRn >=9999 Or scSflAgtRR >=9999 Or
           scSflClmRR >=9999 Or scSflEmpRR >=9999;
            str9999msg = 'Your search returned too many records. ' +
             'Click on Results to see first 9999 records Or, click Filter ' +
             'to narrow down your search results';
            isFilter = PopTwoBtn(str9999msg:'Search':'Filter':'Results');
            Leave;
          Endif;
       Enddo;
       Exec SQL Close cursor1;

       Select;
       When isEmpSelected;
          Close WDELLQ;
          Close WDELL2Q;
          Close WMEMLQ;
       Endsl;

       //==================================================================;
       Begsr AgtSql;

       hdgResults = 'Agent Search Results - Generic';

       Exec SQL Fetch next From cursor1 into :agtDS;

       scResultNm = %trim(agtName);
       scResult#  = %editc(agtFnd:'X') + '-' + %editc(agtAgt#:'X');
       scRRN = %Editc(agtRRN:'X');
       scInqUpd = InqUpdAgt(xxUserCo#:agtFnd:agtAgt#);

       scURL = CreateURL('STDRVPGM':scResultNm:scRRN:scInqUpd:
        'STDEFAULT');

       Endsr;

       //==================================================================;
       Begsr ClmSql;

       hdgResults = 'Claim Search Results - Generic';

       If outCltID= '126';
          Colhdg = ',Name,Social Security #,Jurisdiction State,Number,Date of'+
                   ' Injury,Lost Time Status,Net Total Incurred,Status,' +
                   'State Claim #,Adjuster,';
       Else;
       If dsClmSS# <> *blanks;
          Colhdg = ',Name,Social Security #,Jurisdiction State,Number,Date of'+
                   ' Injury,Lost Time Status,Net Total Incurred,Status,' +
                   'Claim Indicator,SCL#,';
       Else;
          Colhdg = ',Name,Social Security #,Jurisdiction State,Number,Date of'+
                   ' Injury,Lost Time Status,Net Total Incurred,Status,' +
                   'Claim Indicator,Adjuster,';
       Endif;
       Endif;

       Exec SQL Fetch next From cursor1 into :clmDS;

       scResultNm = %trim(clmLastName) + ', ' + %trim(clmFirstName) +
        ' ' + %trim(clmMiddleInit);
       scRRN = %Editc(clmRRN:'X');
       scInqUpd = InqUpdClm(xxUserCo#:clmFnd:clmFyr:clmCase:clmEmp#:clmDiv);
       dspInqSel = *off;
       clear scInqryURL;

       If isClmUpd = *on;
          If scInQUpd = 'C';
             dspInqSel = *on;
             scResultNm = %subst(scResultNm:1:40);
             scInqryURL = CreateURL('STDRVPGM':scResultNm:scRRN:'D':
              'WTCLMSUM');
          Endif;
       Else;
          scInQUpd = 'D';
       Endif;
       Clear scDartURL ;

       scURL = CreateURL('STDRVPGM':scResultNm:scRRN:scInqUpd:
        'WTCLMSUM');

       scClmEmp = GetEmpName(clmEmp#:clmDiv:rrn:clmFnd:clmCo#);
       //***  Begin Chg  ***  06/09/20  **************
       //scSocSec# = DisplaySS#(clmSS#);
       scSocSec# = DisplaySS#(MaskData('SSN':'clmSS#':clmSS#));
       //***  End   Chg  ***  06/09/20  **************
       scClmState = clmState;
       scResult#  = %editc(clmFnd:'X') + '-' + %editc(clmFyr:'X') + '-' +
        %editc(clmCase:'X');
       scDtOfInj = CvtToDate(clmDtOfInjury);
       Select;
       When clmLostTime = 'I';
          scLostTime = 'Indemnity';
       When clmLostTime = 'M';
          scLostTime = 'Medical';
       When clmLostTime = 'R';
          scLostTime = 'Record only';
       Other;
          scLostTime = 'Undefined';
       Endsl;
       scTotalInc = clmRemain01 + clmRemain02 + clmRemain03 + clmRemain04
        + clmRemain05 + clmRemain06 + clmRemain07 + clmRemain08
        + clmRemain09 + clmRemain10
        + clmPayment01 + clmPayment02 + clmPayment03 + clmPayment04
        + clmPayment05 + clmPayment06 + clmPayment07 + clmPayment08
        + clmPayment09 + clmPayment10;
       Select;
       When clmStatus = 'O';
          scClmSts = 'Open';
       When clmStatus = 'C';
          scClmSts = 'Closed';
       Other;
          scClmSts = 'Undefined';
       Endsl;
       denyContr=*Off;
       If outCltID= '126';
          scStClm# = clmACL#;
       Else;
       Select;
       When clmDenied = 'Y';
          scClmInd = 'Denied';
          denyContr=*On;
       When clmControvert = 'Y';
          scClmInd = 'Controverted';
          denyContr=*On;
       Other;
          scClmInd = *blanks;
       Endsl;
       Endif;
       If dsClmSS# <> *Blanks;
          scClmAdj = clmACL#;
       Else;
          scClmAdj = GetUsrName(GetUserId(clmAdjustor));
       Endif;
       strDartsKey = %editc(clmCo#:'X') + %editc(clmFnd:'X') +
        %editc(clmFyr:'X') + %editc(clmCase:'X');
       scDartURL  = %subst('iDARTS - ' + scResultNm:1:50) +
        CreateGenUrl('/iDartsWeb/profound.do':
        'user=' + %Trim(q1User):
        '&keyProgram=W0070R':
        '&key=' + strDartsKey:
        '&action=listing':
        '&mode=remarks');

       Endsr;

       //==================================================================;
       Begsr EmpSql;

       hdgResults = 'Employer Search Results - Generic';

       Exec SQL Fetch next From cursor1 into :empDS;

       If empName = *blanks and empFedName = *blanks;
          empName = '<No name available>';
       Endif;
       scResult#  = %editc(empFnd:'X') + '-' + %editc(empEmp#:'X') + '-' +
        %editc(empDiv:'X');
       scRRN = %Editc(empRRN:'X');
       //*** Beg Add ***** 01/03/24 ******************************
       scEmpsRRN = empRRN;
       //*** End Add ***** 01/03/24 ******************************
       scInqUpd = InqUpdEmp(xxUserCo#:empFnd:empEmp#:empDiv);
       If GetFQLockSts(empCo#:empFnd:empEmp#:empDiv:empEnv) = *On
           And scInqUpd <> 'D';
          scResultNm = 'INQ - ' + empName + %subst(empName2:1:10);
       Else;
          scResultNm = empName + %subst(empName2:1:10);
       Endif;

       Clear scCust;
       Select;
       When outCltID= '089';
          If empCust <> *blanks;
             scCust = 'CustId: ' + empCust;
          Endif;
       When empLink <> ' ';
          scCust = 'EmpLink: ' + empLink;
       Other;
          Clear scCust;
       Endsl;

       Clear ELfyr;
       Clear xxEnv;
       xxEnv = empEnv;
       dspInqSlEm = *off;
       clear scInqURLEm;
       Clear scDartURL ;
       Clear charKeyEnv;

       Select;
       When empEnv = 'Q';
          Clear ELrrnQ;
          Setgt (empCo#:empFnd:empEmp#:empDiv) pel00q;
          Readpe (empCo#:empFnd:empEmp#:empDiv) pel00q;
          If not %eof;
             scRRN = %Editc(ELrrnQ:'X');
             //*** Beg Add ***** 01/03/24 ******************************
             scEmpsRRN = ELrrnQ;
             //*** End Add ***** 01/03/24 ******************************
             If isQteUpd = *on;
                If scInQUpd = 'C';
                   dspInqSlEm = *on;
                   scInqURLEm = CreateQteURL('STDRVPGM':scResultNm:scRRN:'D':
                   'WTPOLSUM':'00000':'Q');
                Endif;
             Else;
                scInQUpd = 'D';
             Endif;
             scURL = CreateQteURL('STDRVPGM':scResultNm:scRRN:scInqUpd:
              'WTPOLSUM':'00000':'Q');
          Else;
             If scInqUpd = 'D';
                scInqUpd = 'P';
             Else;
                scInqUpd = 'Y';
             Endif;
             scRRN = %Editc(empRRN:'X');
             //*** Beg Add ***** 01/03/24 ******************************
             scEmpsRRN = empRRN;
             //*** End Add ***** 01/03/24 ******************************
             If isQteUpd = *on;
                If scInQUpd = 'Y';
                   dspInqSlEm = *on;
                   scInqURLEm = CreateQteURL('STDRVPGM':scResultNm:scRRN:'P':
                   'WTPOLSUM':'00000':'Q');
                Endif;
             Else;
                scInQUpd = 'P';
             Endif;
             scURL = CreateQteURL('STDRVPGM':scResultNm:scRRN:scInqUpd:
              'WTPOLSUM':'00000':'Q');
          Endif;
          charKeyEnv  = 'Q';
          Chain (empCo#:empFnd:empEmp#:empDiv) pem0q;
       Other;
          Clear ELrrn;
          Setgt (empCo#:empFnd:empEmp#:empDiv) pel00;
          Readpe (empCo#:empFnd:empEmp#:empDiv) pel00;
          If not %eof;
             scRRN = %Editc(ELrrn:'X');
             //*** Beg Add ***** 01/03/24 ******************************
             scEmpsRRN = ELrrn;
             //*** End Add ***** 01/03/24 ******************************
             If isPolUpd = *on;
                If scInQUpd = 'C';
                   dspInqSlEm = *on;
                   scInqURLEm = CreateURL('STDRVPGM':scResultNm:scRRN:'D':
                    'WTPOLSUM');
                Endif;
             Else;
                scInQUpd = 'D';
             Endif;
             scURL = CreateURL('STDRVPGM':scResultNm:scRRN:scInqUpd:
              'WTPOLSUM');
          Else;
             If scInqUpd = 'D';
                scInqUpd = 'P';
             Else;
                scInqUpd = 'Y';
             Endif;
             scRRN = %Editc(empRRN:'X');
             //*** Beg Add ***** 01/03/24 ******************************
             // If WDELP record not found for the selected Employer
             // do not return WMEMP RRN to Serach Employer()
             scEmpsRRN = 999999999;
             //*** End Add ***** 01/03/24 ******************************
             If isPolUpd = *on;
                If scInQUpd = 'Y';
                   dspInqSlEm = *on;
                   scInqURLEm = CreateURL('STDRVPGM':scResultNm:scRRN:'P':
                    'WTPOLSUM');
                Endif;
             Else;
                scInQUpd = 'P';
             Endif;
             scURL = CreateURL('STDRVPGM':scResultNm:scRRN:scInqUpd:
              'WTPOLSUM');
          Endif;
          Chain (empCo#:empFnd:empEmp#:empDiv) pem00;
       Endsl;

       scDBANm  = empName + %subst(empName2:1:10);
       scDBANm2 = %subst(empName2:11:50);
       scFedNm  = empFedName + %subst(empFedName2:1:10);
       scFedNm2 = %subst(empFedName2:11:50);
       scMailAddr = ConcatAddr(empMailAddr1:empMailAddr2:empMailCity:
        empMailSt:empMailZip);
       scPhysAddr = ConcatAddr(empPhysAddr1:empPhysAddr2:empPhysCity:
        empPhysSt:empPhysZip);
       scRatingSt = GetRatingState(empCo#:empFnd:empEmp#:empDiv:xxYear:empEnv);
       ScCxDt = CvtToDate(
        GetCxDate(empCo#:empFnd:empEmp#:empDiv:xxReason:empEnv));
       scInceptDt = CvtToDate(EMefd1);
       scEmpStat = GetQuoteSts(empCo#:empFnd:empEmp#:empDiv:ELfyr:empEnv);
       scEmpSts = GetQuoteStsDesc(empCo#:empFnd:empEmp#:empDiv:ELfyr:empEnv);
       scEmpEnv = empEnv;
       strDartsKey = %editc(empCo#:'X') + %editc(empFnd:'X') +
        %editc(empEmp#:'X') + %editc(empDiv:'X');
       %subst(strDartsKey:40:1) = charKeyEnv;
       scDartURL  = %subst('iDARTS - ' + scDBANm:1:50) +
        CreateGenUrl('/iDartsWeb/profound.do':
        'user=' + %Trim(q1User):
        '&keyProgram=W0050R':
        '&key=' + strDartsKey:
        '&action=listing':
        '&mode=remarks');

       Endsr;
      /End-Free

     PLoadSFL          e

     PPgmStr           b
     P*====================================================================
     P* Program Start - Initialize program requirements
     P*====================================================================
     DPgmStr           pi

     Di1               s              5  0
     Dgrp              s                   like(EMfnd)
      /Free

       isAuthEr = *off;
       isPolUpd = *off;
       isQteUpd = *off;
       isClmUpd = *off;
       isPolInq = *off;
       isQteInq = *off;
       isClmInq = *off;

       // Get user sec level/type
       inULVL = %Editc(GetUsrLvl(Q1user:inUTYP):'X');

       // Claim Update
       keySDACL = 'ClmUpdAut';
       Exsr CheckAuth;
       If outRes = '0';
          isClmUpd = *on;
       Endif;

       // Policy Update
       keySDACL = 'PolUpdAut';
       Exsr CheckAuth;
       If outRes = '0';
          isPolUpd = *on;
       Endif;

       // Quote Update
       keySDACL = 'QteUpdAut';
       Exsr CheckAuth;
       If outRes = '0';
          isQteUpd = *on;
       Endif;

       // Claim Inquiry
       keySDACL = 'ClmInqAut';
       Exsr CheckAuth;
       If outRes = '0';
          isClmInq = *on;
       Endif;

       // Policy Inquiry
       keySDACL = 'PolInqAut';
       Exsr CheckAuth;
       If outRes = '0';
          isPolInq = *on;
       Endif;

       // Quote Inquiry
       keySDACL = 'QteInqAut';
       Exsr CheckAuth;
       If outRes = '0';
          isQteInq = *on;
       Endif;

       If xxMode = '*';
          If isClmUpd = *off and isClmInq = *off;
             xxMode = 'E';
          Endif;

          If xxMode = '*'  and isPolUpd = *off and isPolInq = *off and
           isQteUpd = *off and isQteInq = *off;
             xxMode = 'C';
          Endif;
       Endif;

       svMode = xxMode;
       If xxMode = '*' or xxMode = 'E';
          If isPolUpd = *off and isPolInq = *off and (isQteUpd = *on or
           isQteInq = *on);
             svMode = 'Q';
          ElseIf isQteUpd = *off and isQteInq = *off and (isPolUpd = *on or
           isPolInq = *on);
             svMode = 'L';
          Endif;
       Endif;

       If xxMode = 'E';
          xxMode = svMode;
       Endif;

       If (xxMode = 'Q' and isQteUpd= *off and isQteInq = *off) or
        (xxMode = 'L' and isPolUpd= *off and isPolInq = *off)   or
        (xxMode = 'C' and isClmUpd= *off and isClmInq = *off)   or
        (xxMode = 'E' and isPolUpd = *off and isPolInq = *off and
        isQteUpd = *off and isQteInq = *off);
          isAuthEr = *on;
          PopOneBtn('You do not have required authority to perform +
           this action.':'Information');
          Return;
       Endif;

       isEmpSearch = (xxMode = 'E' or xxMode = 'Q' or xxMode = 'L'
        or xxMode = '*');
       isClmSearch = (xxMode = 'C' or xxMode = '*');
       isAgtSearch = (xxMode = 'A' or xxMode = '*');
       scEmpSrch = isEmpSearch;
       scClmSrch = isClmSearch;
       scTabNames = 'Quick Search, Policy Search, Claim Search';
       is1QteSts = (svMode = 'Q' or svMode = 'L');
       sc1QteSts = is1QteSts;

       isInquiryOnly = xxCall = '1';
       isSelectMode = xxCall = '2';
       isSearchMode = not isSelectMode;
       isUseGenResult = xxCall = '*';
       scSlctMode = isSelectMode;
       scSrchMode = isSearchMode;

       //*** Beg Add ***** 12/15/23 ******************************
       callpgm = *off;
       ChkStack('WTRFCHK':CallPgm);
       If callpgm = *on;
          clear scSlctMode;
       Endif;
       //*** End Add ***** 12/15/23 ******************************
       If is1QteSts;
          scEmpQteSt = svMode;
       Endif;
       scSrchType = DftSrchType;

       Select;
       When scSlctMode = *off;
          intDftTab = 0;
          //*** Beg Add ***** 11/06/23 ******************************
          Chain(n) (q1user) pup01;
          If UPDSTAB = 'P';
             intDftTab = 1;
          Endif;
          If UPDSTAB = 'C';
             intDftTab = 2;
          Endif;
          If UPDSTAB = 'Q';
             If UPQDFLT = 'P';
                scSrchType = 'E';
             Endif;
             If UPQDFLT = 'C';
                scSrchType = 'C';
             Endif;
          Endif;
          //*** End Add ***** 11/06/23 ******************************
       When isEmpSearch;
          intDftTab = 1;
       When isClmSearch;
          intDftTab = 2;
       When isAgtSearch;
          intDftTab = 3;
       Other;
          scSrchType = *blanks;
       Endsl;

       scSrchTab = intDftTab;

       // Determine if fields are protected;
       scClmLeft = 95;
       If not isEmpSearch;
          dspAtrEmp = prtNormal;
          scClmLeft = 20;
       Endif;
       If not isClmSearch;
          dspAtrClm = prtNormal;
       Endif;
       If not isAgtSearch;
          dspAtrAgt = prtNormal;
       Endif;

       ClrQckCriteria();
       ClrEmpCriteria();
       ClrClmCriteria();
       ClrAgtCriteria();

       xxUserCo#  = UserCo#(q1User);
       xxUserGrp  = UserGrp(q1User);
       xxUserEmp# = UserEmp#(q1User);
       xxUserDiv  = UserDiv(q1User);
       scQteTypSl = 'QSstat = ''A'' and QSco# = ' +  %editc(xxUserCo#:'X')
        + ' AND QSKEY <> ''X'' AND QSKEY <> ''T''';

       Exsr ChkGroupRestr;
       Exsr ChkEmployerRestr;

       isClmOnly = ClaimsOnly();

       dspEmpQtSt = *on;
       If isClmOnly = *on or xxMode = 'C';
          dspEmpQtSt = *off;
       Endif;
       // To retrieve the client id
          GetClientID(outCltID);
       showPrompt = *On;
       Monitor;
          // Try to Read WTSEARCHDA data Area
          In WtSearchDa;
          // If data area has been accessed and the value is not
          // Y then don't go to prompt screen
          If prompt <> 'Y';
             showPrompt = *Off;
          EndIf;
          // See if there are values being passed and if so no prompt
          // Claims
          If dsClmYr<>0 Or dsClmCase<>0 Or dsClmSS#<>' ' Or dsClmAccDt<>0 Or
             dsClmLName<>' ' Or dsClmFName<>' ' Or dsClmEmp#<>0 Or
             (dsClmDiv<>' ' And dsClmDiv<>'00000') Or dsClmLoc<>' ' Or
             dsClmEmpNm<>' ' Or dsClmStat<>' ' Or dsClmLtFl<>' ' Or
             dsClmJrSt<>' ' Or dsClmSCL#<>' ';
             showPrompt=*Off;
          EndIf;
          // Employer
          If dsEmpName<>' ' Or dsEmpEmp#<>0 Or dsEmpAgt#<>0 Or dsEmpAgtNm<>' '
             Or dsEmpCarr<>' ' Or dsEmpFein<>0 Or dsEmpSt#<>' ' Or
             (dsEmpDiv<>' ' And dsEmpDiv<>'00000') Or dsEmpAddr<>' ' Or
             dsEmpFQ<>' ' Or dsEmpNB<>' ' Or dsEmpFrDt<>0 Or
             dsEmpToDt<>0 Or dsEmpQteTp<>' ' Or dsRateSt<>' ' Or
             dsEmpUnder<>' ' Or dsEmpQteSt<>' ';
             showPrompt=*Off;
          EndIf;
          // Agent
          If dsAgtName<>' ' Or dsAgt#<>0 Or dsAgtProdN<>' ' Or dsAgtProd#<>0
             Or dsAgtStat<>' ' Or dsAgtGrpSt<>' ';
             showPrompt=*Off;
          EndIf;
       On-Error;
          // If data area can't be reached then don't go to prompt
          showPrompt = *Off;
       EndMon;
       Select;
       When dsClmGrp <> *zero;
          scClmGrp = dsClmGrp;
          isGrpRstr = *On;
       When dsEmpGrp <> *zero;
          scEmpGrp = dsEmpGrp;
          isGrpRstr = *On;
       When dsAgtGrp <> *zero;
          scAgtGrp = dsAgtGrp;
          isGrpRstr = *On;
       Endsl;

        scSrchCrit = 'C';
        scSrchCrt2 = 'C';
        scSrchCrt3 = 'C';

       // Check employer restrictions
       Begsr ChkEmployerRestr;

       flgUsrEmpRestr = UsrEmpRstFlg(q1User);
       isEmpRestr     = flgUsrEmpRestr =  'Y';
       isNoEmpRestr   = flgUsrEmpRestr =  'N';
       isSnglEmpRestr = flgUsrEmpRestr =  'S';
       isMultEmpRestr = flgUsrEmpRestr =  'M';
       isEmpRstr = isEmpRestr;

       Select;
       When isEmpRestr;
          scClmEmp# = xxUserEmp#;
          scEmpEmp# = xxUserEmp#;
          isGrpRestr = *on;
          isGrpRstr = *on;
          scClmGrp  = xxUserGrp;
          scEmpGrp  = xxUserGrp;
       Endsl;

       flgUsrDivRestr = UsrDivRstFlg(q1User);
       isDivRestr     = flgUsrDivRestr =  'Y';
       isNoDivRestr   = flgUsrDivRestr =  'N';
       isDivRstr = isDivRestr;

       Select;
       When isDivRestr;
          scClmDiv  = %editc(xxUserDiv:'3');
          scEmpDiv  = %editc(xxUserDiv:'3');
       Endsl;

       Endsr;

       // Check Authority
       Begsr CheckAuth;
       Chain keySDACL pac00;
       CheckSecurity(inUTYP:inULVL:ACstp1:%Editc(ACslv1:'X'):'1':outRes
        :ACstp2:ACstp3:ACstp4:ACstp5:ACstp6:%Editc(ACslv2:'X')
        :%Editc(ACslv3:'X'):%Editc(ACslv4:'X'):%Editc(ACslv5:'X')
        :%Editc(ACslv6:'X'):ACstp7:ACstp8:ACstp9:ACstp0:%Editc(ACslv7:'X')
        :%Editc(ACslv8:'X'):%Editc(ACslv9:'X'):%Editc(ACslv0:'X'));
       Endsr;

       // Check group restrictions
       Begsr ChkGroupRestr;

       flgUsrGrpRestr = UsrRstFlg(q1User);
       isGrpRestr     = flgUsrGrpRestr =  'Y';
       isNoGrpRestr   = flgUsrGrpRestr =  'N' or flgUsrGrpRestr = ' ';
       isSnglGrpRestr = flgUsrGrpRestr =  'S';
       isMultGrpRestr = flgUsrGrpRestr =  'M';
       If isGrpRstr <> *on;
          isGrpRstr = isGrpRestr or isEmpRestr;
       EndIf;
       Clear strRestrGrpSQL;

       Select;
       When isMultGrpRestr or isSnglGrpRestr;
          Exsr LoadAryGroups;
       When isGrpRestr;
          scAgtGrp = xxUserGrp;
          scClmGrp = xxUserGrp;
          scEmpGrp = xxUserGrp;
       Endsl;

       Endsr;

       // Load Restricted Groups Array
       Begsr LoadAryGroups;

       For i1 = 1 to 999;
          grp = i1;
          If GroupAuth(xxUserCo#:grp) = *on;
             aryGroups(i1) = grp;
             If strRestrGrpSQL <> *blanks;
                strRestrGrpSQL = %trim(strRestrGrpSQL) + ',';
             Endif;
             strRestrGrpSQL = %trim(strRestrGrpSQL) + %editc(grp:'X');
          Endif;
       Endfor;

       Endsr;

      /End-Free
     PPgmStr           e

     PProcess          b
     P*====================================================================
     P* Program Processing - The exit point will be returned
     P*====================================================================
     DProcess          pi             1n

     DisError          s              1n
      /Free

       isFilter = *Off;
       Dou not isError;
          Exfmt rcdCrit;
          If scExit = *on;
             Return *on;
          Endif;
          isError = CriteriaError;
       Enddo;

       DefineSearch();

       return ProcessResults();

      /End-Free
     PProcess          e

     PProcessResults   b
     P*====================================================================
     P* Program Processing - The exit point will be returned
     P*====================================================================
     DProcessResults   pi             1n
      /Free
       ClearSubFiles();
       CreateCursors();

       LoadSFL();
       If isFilter =  *On;
         Select;
         When (scSrchTab = 0 and scSrchType = 'E');
          scSrchTab = 1;
         When (scSrchTab = 0 and scSrchType = 'C');
          scSrchTab = 2;
         When (scSrchTab = 0 and scSrchType = 'A');
          scSrchTab = 2;
         EndSl;
         Return *off;
       Endif;

       DisplaySubFile();
       isFilter = *off;

       Select;
       When scNewSrch = *on;
          Clear scSrchName;
          Clear scClmLNam0;
          Clear scClmFNam0;
          Clear scSrchType;
          ClrQckCriteria();
          ClrEmpCriteria();
          ClrClmCriteria();
          ClrAgtCriteria();
          scSrchTab = intDftTab;
          Return *off;
       When scExit = *on;
          Return *on;
       When scSlctMode = *on and RecordSelected();
          Return *on;
       //***  Begin Add  ***  06/16/20  ***************
       When btnRefresh = *on;
          btnRefresh = *off;
          Return ProcessResults();
       //***  End   Add  ***  06/16/20  ***************
       When btnEmpFltr = *on;
          btnEmpFltr = *Off;
          isFilter = *on;
          scSrchTab = 1;
          Return *off;
       When btnClmFltr = *on;
          btnClmFltr = *Off;
          isFilter = *on;
          scSrchTab = 2;
          Return *off;
       When btnAgtFltr = *on;
          btnAgtFltr = *Off;
          isFilter = *on;
          scSrchTab = 2;
          Return *off;
       Other;
          Return *off;
       Endsl;

      /End-Free
     PProcessResults   e

     PQualifiedAgt     b
     P*====================================================================
     P* Qualify agent selections
     P*====================================================================
     DQualifiedAgt     pi              n
     D inCo#                          3s 0 const
     D inFnd                          3s 0 const
     D inAgt#                         5s 0 const
      /Free

       Return *on;

      /End-Free
     PQualifiedAgt     e

     PQualifiedClm     b
     P*====================================================================
     P* Qualify claim selections
     P*====================================================================
     DQualifiedClm     pi              n
     D inCo#                          3s 0 const
     D inFnd                          3s 0 const
     D inFyr                          3s 0 const
     D inCase                         7s 0 const
     D inEmp#                         9s 0 const
     D inDiv                          5s 0 const

     D rrn             s              9  0
     D xxDBAName       s            100
     D xxFedName       s            100
     D result          s              3  0
     D u1pdsc          s            100
     D u2pdsc          s            100

      /Free
       xxDBAName = GetEmpFullName(inEmp#:inDiv:rrn:inFnd:inCo#:'DBA');
       xxFedName = GetEmpFullName(inEmp#:inDiv:rrn:inFnd:inCo#:'FED');

       If EmpAuth(inCo#:inFnd:inEmp#:q1User) = *off;
          Return *off;
       Endif;

       If scClmEmpNm <> *blanks;
          If not StringFound(%trim(xxDBAName):%trim(scClmEmpNm):'1')
           and not StringFound(%trim(xxFedName):%trim(scClmEmpNm):'1');
            Return *off;
          Endif;
       Endif;

       // Determine if this user has access to claim
       If Not SrchUserAuth(inCo#:inFnd:inFyr:inCase);
          Return *Off;
       EndIf;

       Return *on;

      /End-Free
     PQualifiedClm     e

     PQualifiedEmp     b
     P*====================================================================
     P* Qualify employer selections
     P*====================================================================
     DQualifiedEmp     pi              n
     D inCo#                          3s 0 const
     D inFnd                          3s 0 const
     D inEmp#                         9s 0 const
     D inDiv                          5s 0 const
     D inEnv                          1    const options(*nopass)

     D xxAgt#          s              5  0
     D xxAgentName     s             40
     D x7FrDate        s              7  0
     D x7ToDate        s              7  0
     D rrnAA           s              9  0
     D rrnAG           s              9  0
     D rrnAH           s              9  0
     D outEffDt        s              7  0 inz
     D outEnv          s              1    inz
     D outYear         s              3  0 inz
     D xxQteSts        s              1
      /Free

       If %parms >= 5;
          outEnv = inEnv;
       Endif;

       xxAgt# = GetEmpAgt(inCo#:inFnd:inEmp#:inDiv:outYear:outEffDt:
        xxAgentName:rrnAH:rrnAA:rrnAG:outEnv);
       x7FrDate = DtCYMD(scEmpFrDt);
       x7ToDate = DtCYMD(scEmpToDt);

       If EmpAuth(inCo#:inFnd:inEmp#:q1User) = *off;
          Return *off;
       Endif;

       If scEmpAgt# <> *zero
        and scEmpAgt# <> xxAgt#;
          Return *off;
       Endif;

       If scEmpAgtNm <> *blanks;
          If not StringFound(xxAgentName:scEmpAgtNm:'1');
            Return *off;
          Endif;
       Endif;

       If scEmpCarr <> *blanks;
          Setgt (inCo#:inFnd:inEmp#:inDiv) pel00;
          Dou %eof;
             Readpe (inCo#:inFnd:inEmp#:inDiv) pel00;
             //*** Beg Chg *** 10/24/25 **************************************
             //***If scEmpCarr = GetCarrPol(inCo#:inFnd:inEmp#:inDiv:ELfYr);
             If Upper(scEmpCarr) = Upper(GetCarrPol(inCo#:inFnd:inEmp#:inDiv:
              ELfYr));
             //*** End Chg *** 10/24/25 **************************************
                Leave;
             Endif;
          Enddo;
          If %eof;
             Return *off;
          Endif;
       Endif;

       If scEmpToDt <> *zeros;
          Select;
          When xxEnv = 'Q';
             Setgt (inCo#:inFnd:inEmp#:inDiv:x7ToDate) pel02q;
             Readpe (inCo#:inFnd:inEmp#:inDiv) pel02q;
          Other;
             Setgt (inCo#:inFnd:inEmp#:inDiv:x7ToDate) pel02;
             Readpe (inCo#:inFnd:inEmp#:inDiv) pel02;
          Endsl;
          If %eof or ELpFDt < x7FrDate;
             Return *off;
          Endif;
       Endif;

       If scEmpNB <> *blanks
        and scEmpNB <> ELnwRw;
          Return *off;
       Endif;

       If scEmpQteTp <> *blanks
        and %Lookup(scEmpStat:aryEmpQteTp) = 0;
          Return *off;
       Endif;

       If scRateSt <> *blanks
        and Upper(scRateSt) <> Upper(scRatingSt);
          Return *off;
       Endif;

       If scEmpQteSt <> *blanks
        and scEmpQteSt <> xxEnv;
          Return *off;
       Endif;

       Return *on;

      /End-Free
     PQualifiedEmp     e

     PRecordSelected   b
     P*====================================================================
     P* Determine if a record was selected and return the RRN to calling pgm
     P*====================================================================
     DRecordSelected   pi             1n
      /Free
       Select;
       When isEmpSelected;
          ReadC sflEmpRslt;
       When isClmSelected;
          If outCltID= '126';
             ReadC sflClmRsl1;
          Else;
             ReadC sflClmRslt;
          Endif;
       When isAgtSelected;
          ReadC sflAgtRslt;
       Other;
          ReadC sflResults;
       Endsl;
       Select;
       When not %eof and btnSelect = *on;
          @@rrn = scRRN;
          If @@Mode = 'E';
             @@Mode = scEmpEnv;
             //*** Beg Add ***** 01/03/24 ******************************
             // If WDELP record not found for the selected Employer
             // do not return WMEMP RRN to Serach Employer()
             If scEmpsRRN = 999999999;
                @@rrn = '999999999';
             Endif;
             //*** End Add ***** 01/03/24 ******************************
          EndIf;
          return *on;
       Other;
          return *off;
       Endsl;
      /End-Free
     PRecordSelected   e

     PSearchAgt        b
     P*====================================================================
     P* Use the criteria to search for agents
     P*====================================================================
     DSearchAgt        pi
     DstrSqlWhere      s                   like(sqlWhereSize) inz
     DsrchConditional  s              3    inz('and')
     DstrConditions    s                   like(sqlWhereSize)
      /Free

       // Load SQL statement;
       strSqlCursor = 'select AAname, +
        AAco#, AAfnd, AAagt#, +
        RRN(WMAAP) from WMAAP';

       If xxUserCo# <> *zero;
          strConditions = ' ' +
           'AAco# = ' + %editc(xxUserCo#:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       Select;
       When scAgtGrp <> *zero and (scSrchTab  = 3 or isGrpRestr or
        isResultsOnly);
          strConditions = ' ' +
           'AAfnd = ' + %editc(scAgtGrp:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       When strRestrGrpSQL <> *blank;
          strConditions = ' ' +
           'AAfnd in (' + %trim(strRestrGrpSQL) + ')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endsl;
       If scAgtName <> *blank;
          strConditions = ' ' +
           '(Upper(AAname) like Upper(''%'+%trim(scAgtName)+'%''))';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scAgt# <> *zero;
          strConditions = ' ' +
           'AAagt# = ' + %editc(scAgt#:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scAgtProdN <> *blank;
       // strConditions = ' ' +
       //  '(Upper(AAname) like Upper(''%'+%trim(scAgtProdN)+'%''))';
       // CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scAgtProd# <> *zero;
       // strConditions = ' ' +
       //  'AAagt# = ' + %editc(scAgtProd#:'X');
       // CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scAgtStat <> *blanks;
       // strConditions = ' ' +
       //  'AGstat = ''' + %trim(scAgtStat) + '''';
       // CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scAgtGrpSt <> *blanks;
          strConditions = ' ' +
           'AAstat = ''' + %trim(scAgtGrpSt) + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;

       strSqlCursor = %trim(strSqlCursor) + ' ' + %trim(strSqlWhere) +
        ' order by Upper(AAname)';

      /End-Free
     PSearchAgt        e

     PSearchClm        b
     P*====================================================================
     P* Use the criteria to search for claims
     P*====================================================================
     DSearchClm        pi
     DstrSqlWhere      s                   like(sqlWhereSize) inz
     DsrchConditional  s              3    inz('and')
     DstrConditions    s                   like(sqlWhereSize)
     DstrClmLName      s                   like(scClmLNam)
     DstrClmFName      s                   like(scClmFNam)

      /Free
       // Load SQL statement;
       strSqlCursor = 'select CMlsN, CMfsN, CMmid, CMss#, CMjrSt, CMacDt, +
        CMltFl, CMstat, CMdny,  CMcvFl, CMclEx, +
        CMrr01, CMrr02, CMrr03, CMrr04, CMrr05, +
        CMrr06, CMrr07, CMrr08, CMrr09, CMrr10, +
        CMar01, CMar02, CMar03, CMar04, CMar05, +
        CMar06, CMar07, CMar08, CMar09, CMar10, +
        CMemp#, CMdiv, +
        CMco#, CMfnd, CMfyr, CMcase, +
        RRN(WMCMP), CMacl# from WMCMP';

       If xxUserCo# <> *zero;
          strConditions = ' ' +
           'CMco# = ' + %editc(xxUserCo#:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       Select;
       When scClmGrp <> *zero and (scSrchTab  = 2 or isGrpRestr or
        isResultsOnly);
          strConditions = ' ' +
           'CMfnd = ' + %editc(scClmGrp:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       When strRestrGrpSQL <> *blank;
          strConditions = ' ' +
           'CMfnd in (' + %trim(strRestrGrpSQL) + ')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endsl;
       If scClmYr <> *zero;
          strConditions = ' ' +
           'CMfyr = ' + %editc(scClmYr:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmCase <> *zero;
          strConditions = ' ' +
           'CMcase = ' + %editc(scClmCase:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmSS# <> *blanks;
          strConditions = ' ' +
           'CMss# = ''' + scClmSS# + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmAccDt <> *zero;
          strConditions = ' ' +
           'CMacDt = ' + %editc(DtCYMD(scClmAccDt):'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmLNam <> *blank;
          strClmLName = ChkApost(%Trim(scClmLNam));
          If scSrchCrit ='C' or scSrchCrt3 = 'C';
          strConditions = ' ' +
           'Upper(CMlsN) like Upper(''%'+%trim(strClmLName)+'%'')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
          Else;
          strConditions = ' ' +
           'Upper(CMlsN) like Upper('''+%trim(strClmLName)+'%'')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
          Endif;
       Endif;
       If scClmFNam <> *blank;
          strClmFName = ChkApost(%Trim(scClmFNam));
          If scSrchCrit ='C' or scSrchCrt3 = 'C';
          strConditions = ' ' +
           'Upper(CMfsN) like Upper(''%'+%trim(strClmFName)+'%'')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
          Else;
             strConditions = ' ' +
              'Upper(CMfsN) like Upper('''+%trim(strClmFName)+'%'')';
             CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
          Endif;
       Endif;
       If scClmEmpNm <> *blank;
       // Handled in QualifiedClm;
       Endif;
       If scClmEmp# <> *zero and (scSrchTab  = 2 or isEmpRestr);
          strConditions = ' ' +
           'CMemp# = ' + %editc(scClmEmp#:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmDiv <> *blanks and (scSrchTab  = 2 or isDivRestr);
          strConditions = ' ' +
           'CMdiv = ' + scClmDiv;
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmLoc <> *blanks;
          strConditions = ' ' +
           'CMctg1 = ''' + %trim(scClmLoc) + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmStat <> *blanks;
          strConditions = ' ' +
           'CMstat = ''' + %trim(scClmStat) + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmUser <> *blanks;
          strConditions = ' ' +
           '(CMclEx = ''' + %trim(scClmUser) + ''' or' +
           ' CMsupr = ''' + %trim(scClmUser) + ''' or' +
           ' CMaMan = ''' + %trim(scClmUser) + ''' or' +
           ' CMuMan = ''' + %trim(scClmUser) + ''' or' +
           ' CMnrse = ''' + %trim(scClmUser) + ''' or' +
           ' CMfeAd = ''' + %trim(scClmUser) + ''' or' +
           ' CMsdfA = ''' + %trim(scClmUser) + ''' or' +
           ' CMeSup = ''' + %trim(scClmUser) + ''')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmLtFl <> *blanks;
          strConditions = ' ' +
           'CMltFl = ''' + %trim(scClmLtFl) + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmJrSt <> *blanks;
          strConditions = ' ' +
           'CMjrSt = Upper('''+%trim(scClmJrSt)+''')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scClmSCL# <> *blanks;
          strConditions = ' ' +
           'CMaCl# = Upper('''+%trim(scClmSCL#)+''')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;

       strSqlCursor = %trim(strSqlCursor) + ' ' + %trim(strSqlWhere) +
        ' order by Upper(CMlsN), Upper(CMfsN), Upper(CMmid)';

      /End-Free
     PSearchClm        e

     PSearchEmp        b
     P*====================================================================
     P* Use the criteria to search for employers
     P*====================================================================
     DSearchEmp        pi

     DstrSqlWhere      s                   like(sqlWhereSize) inz
     DsrchConditional  s              3    inz('and')
     DstrConditions    s                   like(sqlWhereSize)
     DstrLivLibrary    s             10
     DstrQteLibrary    s             10
     DstrEmpCursor     s                   like(strSqlCursor)
     DstrLivCursor     s                   like(strSqlCursor)
     DstrQteCursor     s                   like(strSqlCursor)
     DstrEmpName       s             +5    like(scEmpName)
     DisUseBoth        s              1n   inz('1')
     D*** Beg Add *** 10/24/25 *************************************************
     DstrSqlJoin       s                   like(sqlJoinSize) inz
     D*** End Add *** 10/24/25 *************************************************

      /Free
       strLivLibrary = RtvQuoteLib('*LIV');
       strQteLibrary = RtvQuoteLib('*QTE');

       // Load SQL statement;
       strEmpCursor = 'select +
        EMname, EMnam2, EMfdNm, EMfdN2, +
        EMmAd1, EMmAd2, EMmCty, EMmSt, EMmZip, +
        EMpAd1, EMpAd2, EMpCty, EMpSt, EMpZip, +
        EMco#, EMfnd, EMemp#, EMdiv, EMcust, +
        EMLink, +
        RRN(WMEMP),';
       strSqlCursor = %trim(strEmpCursor) + ' ' +
        '''*'' from WMEMP';
       strLivCursor = %trim(strEmpCursor) + ' ' +
        '''L'' from ' + %trim(strLivLibrary) + '/WMEMP';
       strQteCursor = %trim(strEmpCursor) + ' ' +
        '''Q'' from ' + %trim(strQteLibrary) + '/WMEMP';

       If scEmpName <> *blank;
          strEmpName = ChkApost(%Trim(scEmpName));
          If scSrchCrit ='C' or scSrchCrt2 = 'C';
             strConditions = ' ' +
              '(Upper(EMname) like Upper(''%'+%trim(strEmpName)+'%'') or' +
              ' Upper(EMnam2) like Upper(''%'+%trim(strEmpName)+'%'') or' +
              ' Upper(EMfdNm) like Upper(''%'+%trim(strEmpName)+'%'') or' +
              ' Upper(EMfdN2) like Upper(''%'+%trim(strEmpName)+'%''))';
             CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
          Else;

          // In case of "Begins with", no need to check the values in
          // second part fields.

             strConditions = ' ' +
              '(Upper(EMname) like Upper('''+%trim(strEmpName)+'%'')  or' +
              ' Upper(EMfdNm) like Upper('''+%trim(strEmpName)+'%''))';
             //*** Beg Add *** 10/24/25 ***************************************
             // If the Employer Name is blank and Carrier Policy Number is
             // not blank then we dont want to select all employers.  This
             // was causing the Carrier Pol Search to have very slow respons
             If scEmpCarr = *blanks;
             //*** End Add *** 10/24/25 ***************************************
                CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
             //*** Beg Add *** 10/24/25 ***************************************
             Endif;
             //*** End Add *** 10/24/25 ***************************************
          Endif;
       Endif;
       If xxUserCo# <> *zero;
          strConditions = ' ' +
           'EMco# = ' + %editc(xxUserCo#:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       Select;
       When scEmpGrp <> *zero and (scSrchTab  = 1 or isGrpRestr or
        isResultsOnly);
          strConditions = ' ' +
           'EMfnd = ' + %editc(scEmpGrp:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       When strRestrGrpSQL <> *blank;
          strConditions = ' ' +
           'EMfnd in (' + %trim(strRestrGrpSQL) + ')';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endsl;
       If scEmpEmp# <> *zero and (scSrchTab  = 1 or isEmpRestr);
          strConditions = ' ' +
           'EMemp# = ' + %editc(scEmpEmp#:'X');
            CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       //*** Beg Add ***** 11/06/23 **********************************
       Else;
          If (scSrchTab  = 1 or isEmpRestr);
             If scEmpLoRng <> *Zero;
                strConditions = ' ' +
                 'EMemp# >= ' + %editc(scEmpLoRng:'X');
                  CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
             Endif;
             If scEmpUpRng <> *Zero;
                strConditions = ' ' +
                 'EMemp# <= ' + %editc(scEmpUpRng:'X');
                  CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
             Endif;
          Endif;
       //*** End Add ***** 11/06/23 **********************************
       Endif;
       If scEmpDiv <> *blanks and (scSrchTab  = 1 or isDivRestr);
          strConditions = ' ' +
           'EMdiv = ' + scEmpDiv;
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       //*** Beg Add ***** 11/06/23 **********************************
       Else;
          If (scSrchTab  = 1 or isDivRestr);
             If scDivLoRng <> *blanks;
                strConditions = ' ' +
                 'EMdiv >= ' + scDivLoRng;
                CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
             Endif;
             If scDivUpRng <> *blanks;
                strConditions = ' ' +
                 'EMdiv <= ' + scDivUpRng;
                CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
             Endif;
          Endif;
       //*** End Add ***** 11/06/23 **********************************
       Endif;
       If scEmpAgt# <> *zero;
       // Handled in QualifiedEmp;
       Endif;
       If scEmpAgtNm <> *blanks;
       // Handled in QualifiedEmp;
       Endif;
       If scEmpCarr <> *blanks;
       // Handled in QualifiedEmp;
       Endif;
       If scEmpFein <> *zero;
          strConditions = ' ' +
           'EMfein = ' + %editc(scEmpFein:'X');
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scEmpSt# <> *blank;
          strConditions = ' ' +
           'EMsEmp = ' + scEmpSt#;
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scEmpAddr <> *blank;
          strConditions = ' ' +
           '(Upper(EMmAd1) like Upper(''%'+%trim(scEmpAddr)+'%'') or' +
           ' Upper(EMmAd2) like Upper(''%'+%trim(scEmpAddr)+'%'') or' +
           ' Upper(EMpAd1) like Upper(''%'+%trim(scEmpAddr)+'%'') or' +
           ' Upper(EMpAd2) like Upper(''%'+%trim(scEmpAddr)+'%''))';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scEmpPhone <> *zero;
          strConditions = ' ' +
           'EMphn# like ''%' + %trim(%char(scEmpPhone)) + '%''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scCustId <> *blanks;
          strConditions = ' ' +
              ' trim(upper(EMcust)) = ''' + %trim(upper(sccustId)) + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scEmpLink <> *blanks;
          strConditions = ' ' +
              ' trim(upper(EMLink)) = ''' + %trim(upper(scEmpLink)) + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scEmpFQ <> *blanks;
          Select;
          When scEmpFQ = 'F';
             strConditions = ' ' +
              'EMiUsr = ''WCEXPRESS''';
          When scEmpFQ = 'P';
             strConditions = ' ' +
              'EMiUsr = ''CSPORTAL''';
          Other;
             strConditions = ' ' +
              'EMiUsr <> ''CSPORTAL'' and ' +
              'EMiUsr <> ''WCEXPRESS''';
          Endsl;
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       If scEmpNB <> *blanks;
       // Handled in QualifiedEmp;
       Endif;
       If scEmpFrDt <> *zeros;
       // Handled in QualifiedEmp;
       Endif;
       If scEmpQteTp <> *blanks;
       // Handled in QualifiedEmp;
       Endif;
       If scRateSt <> *blanks;
       // Handled in QualifiedEmp;
       Endif;
       If scEmpUnder <> *blanks;
          strConditions = ' ' +
           'EMuWtr = ''' + scEmpUnder + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;

       //*** Beg Add *** 10/24/25 ***************************************
       // This logic will Join WMRPP to WMEMP where the Carrier Policy
       // Number is selected which helps to speed up response time.
       If scEmpCarr <> *blanks;
          strSqlJoin = ' ' +
           'Join WMRPP on EMfnd = RPfnd and EMemp# = RPemp# and EMdiv = RPdiv';
          strQteCursor = %Trim(strQteCursor) + strSqlJoin;
          strLivCursor = %Trim(strLivCursor) + strsqlJoin;
          strConditions = ' ' +
           'Upper(RPcap#) = ' + '''' + %Trim(Upper(scEmpCarr)) + '''';
          CreateSqlWhere(strSqlWhere:strConditions:srchConditional);
       Endif;
       //*** End Add *** 10/24/25 ***************************************

       Select;
       When scEmpQteSt = 'Q';
          strSqlCursor = %trim(strQteCursor) + ' ' + %trim(strSqlWhere) +
           ' order by Upper(EMname)';
       When scEmpQteSt = 'L';
          strSqlCursor = %trim(strLivCursor) + ' ' + %trim(strSqlWhere) +
           ' order by Upper(EMname)';
       When isSelectMode;
          strSqlCursor = %trim(strSqlCursor) + ' ' + %trim(strSqlWhere) +
           ' order by Upper(EMname)';
       When isUseBoth;
          strSqlCursor =   %trim(strLivCursor) + ' ' + %trim(strSqlWhere) +
           ' union all ' + %trim(strQteCursor) + ' ' + %trim(strSqlWhere) +
           ' order by EMname';
       Other;
          strSqlCursor = %trim(strLivCursor) + ' ' + %trim(strSqlWhere) +
           ' order by Upper(EMname)';
       Endsl;

      /End-Free
     PSearchEmp        e



    P*====================================================================
    P* SrchGroupAuth - Determine if user is authorized to the group
    P*====================================================================
     PSrchGroupAuth    b
    P*
     DSrchGroupAuth    pi             1n
     D inCo#                          3  0 const
     D inGroup                        3  0 const

      /Free
       Select;
       When inGroup = *zero;
          Return *on;
       Endsl;

       Return GroupAuth(inCo#:inGroup);

      /End-free
     PSrchGroupAuth    e
    P*====================================================================
    P* SrchUserAuth - Determine if user is authorized to the claim
    P*====================================================================
     PSrchUserAuth     b
    P*
     DSrchUserAuth     pi              n
     D inCo#                          3  0 const
     D inGroup                        3  0 const
     D inYear                         3  0 const
     D inCase                         7  0 const
     D ClaimSecurity   pr                  ExtPgm('WA067R')
     D ##Co#                          3
     D ##Group                        3
     D ##Year                         3
     D ##Case                         7
     D ##User                        10
     D ##Rtcd                         1

     D ##Co#           s              3
     D ##Group         s              3
     D ##Year          s              3
     D ##Case          s              7
     D ##User          s             10
     D ##Rtcd          s              1

     C     keyWMXRL      Klist
     C                   Kfld                    inCO#
     C                   Kfld                    q1User
     C                   Kfld                    inGroup
     C                   Kfld                    inYear
     C                   Kfld                    inCase

      /Free
       Chain keyWMXRL pxr00;
       If %Found(WMXRL) And XRstat<>'I';
          // Doesn't have authority
          Return *Off;
       Else;
          ##Co#   = %editc(inCo#:'X');
          ##Group = %editc(inGroup:'X');
          ##Year  = %editc(inYear:'X');
          ##Case  = %editc(inCase:'X');
          ##User  = Q1User;
          ##Rtcd  = ' ';
          ClaimSecurity(##Co#:##Group:##Year:##Case:##User:##Rtcd);
          If ##Rtcd = '2';
             Return *Off;
          Else;
             Return *On;
          Endif;
       EndIf;

      /End-free
     PSrchUserAuth     e
