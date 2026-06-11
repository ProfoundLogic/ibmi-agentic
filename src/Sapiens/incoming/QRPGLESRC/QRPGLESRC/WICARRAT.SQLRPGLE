     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP('CONNECT')
     h option(*nodebugio: *srcstmt)
     H BNDDIR('WTBNDDIR':'YAJL') DECEDIT('0.')
     H*====================================================================
     H* Create on 09/05/2018 by c:62 for Project SY406
     H*====================================================================
     H* Description: Retrieves Carrier Rating Information
     H*
     H*====================================================================
     H* inParms -
     H*   group#
     H*   ratingState
     H*   effDate
     H*====================================================================
     H* Revisions:
     H*
    H*  12/09/21  C:33  Support: 99529                                   *
    H*                  Trim rating json (no comments)                   *
     H*====================================================================
     fststl3    if   e           k disk    rename(pst01:pst03)
     fwdf1l1    if   e           k disk    rename(pf101:pf101)
     fwdrrl     if   e           k disk    rename(prr01:prr00)
     fwiratddtl uf   e           k disk    rename(pdt01:pdt00)
     fwiratddtl1if   e           k disk    rename(pdt01:pdt01)
     fwiratdftl if   e           k disk    rename(pdf01:pdf00)
     fwiratdftl1if   e           k disk    rename(pdf01:pdf01)
     fwmupl     if   e           k disk    rename(pup01:pup00)
     fwtddl2    if   e           k disk    rename(pdd01:pdd02)
     fwtgtl     if   e           k disk    rename(pgt01:pgt00)
     fwtlml1    if   e           k disk    rename(plm01:plm01)

     D PSDS          ESDS                  extname(SZQ1P)

     D* this size needs to change
     D array           s               a   len(32767)
     d inJson          s               a   len(2000000) varying
     D json            s               a   len(8500000) varying inz
     D*jsont           s               a   len(8500000) varying
     D*jsonSub         s               a   len(8500000) varying

     d maxSize         c                   999

     d decCount        s              3  0
     d decMsg#         s             10i 0
     d mstComma        s              1
     d dtlComma        s              1
     d comma           s              1
     d dateError       s               n   Inz(*Off)
     d effDate7        s                   Like(STdt)
     d effDate8        s              8  0
     d errMsgArray     s            500    dim(maxSize) varying inz
     d group#          s              3  0
     d groupError      s               n   Inz(*Off)
     d hasRateHdg      s               n
     d intWaivCnt      s              5  0 Inz
     D i1              s             10i 0 inz
     d isUseEmployerInfo...
     d                 s              1n
     d stateKey        s                   Like(STkey)
     d strAuthority    s             10a
     d strEffDate      s              8
     d i               s              2  0 Inz(0)
     d intYesCount     s              3  0
     d isShowEmptyRate...
     d                 s              1n   inz(*on)
     d strYesOrNo      s              3

     D errMsg          s          99999a   Varying Inz(' ')
     D errMsg500       s            500a   Varying Inz(' ')
     D sqlStmtStr1     s            200a
     D sqlStmtStr2     s            200a
     D sqlStmtStr      s            400a
     D moreRows        s               n   Inz(*On)
     D reserveCodes    s            200a
     d rrn             s              9  0
     D clientID        s              3    inz('000')

     D docNode         s                   like(yajl_val)
     D key             s             50a   varying
     D list            s                   like(yajl_val)
     D node            s                   like(yajl_val)
     D val             s                   like(yajl_val)

     D result          ds                  Qualified
     D  group#                        3a
     D  rateState                     2a
     D  effDate                       8a

     DdsEmpInfo        ds                  qualified inz
     D employer                       9  0
     D division                       5  0
     D year                           3  0

     ddsWDELP        e ds                  extname(WDELP) Qualified Inz

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrJSON
      /copy sPrSecure
      /copy sPrString
      /copy sPrSql
      /copy wPrGroup

      /include yajl_h

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      *entry
     DRequestPgm       pr                  ExtPgm('WICARRAT')
     D                              256
     D                               10
     D                                 a   len(8500000) varying
     D                                     options(*varsize)
     D                                 a   len(8500000) varying
     D                               10

     DRequestPgm       pi
     D inRequest                    256
     D inUser                        10
     D inParms                         a   len(8500000) varying
     D                                     options(*varsize)
     D outJSON                         a   len(8500000) varying
     D ioRtCd                        10

      /Free
       Exec Sql Set Option Commit = *None;

       Exsr PgmStart;

       Clear json;
       Select;
       When inRequest = 'GetCarrierRating';
          Exsr GetParms;
          ExSr BldReqInfo;
          If ioRtCd <> 'Error';
             Exsr BldCarRate;
          Endif;
          ProcessErrorMessages();
       EndSl;
       outJSON = json;
       *Inlr = *On;
       //=================================================================
       // Build Carrier Rating Information                                  quests
       //=================================================================
       Begsr BldCarRate;

        If isUseEmployerInfo;
           Reset dsWDELP;
           Exec sql
            select * into :dsWDELP
             from WDELP
             where ELco# = :UPco#
              and ELfnd = :group#
              and ELemp# = :dsEmpInfo.employer
              and ELdiv = :dsEmpInfo.division
              and ELfyr = :dsEmpInfo.year;
           isUseEmployerInfo = IsValidFetch(sqlstt);
        Endif;

        Reset intWaivCnt;
        If dsWDELP.ELwvtp = 'S';
           Exec Sql
            Select Count(*)
            Into :intWaivCnt
            From WMWVP
            Where WVco# = :dsWDELP.ELco#
             And WVfnd  = :dsWDELP.ELfnd
             And WVemp# = :dsWDELP.ELemp#
             And WVdiv  = :dsWDELP.ELdiv
             And WVfyr  = :dsWDELP.ELfyr;
        EndIf;

        // Write a Std Limits record
        WriteStdLimits();

        Setll (UPco#:'CRDF') pgt00;
        Dou %Eof(WTGTL);
           Reade (UPco#:'CRDF') pgt00;
           If %Eof;
              Leave;
           Endif;
           If FactorHasNoDefault();
              Iter;
           Endif;
           If DefaultNotSetUp();
              Iter;
           Endif;
           dtlComma = *Blank;
           BuildRateJSON();
           mstComma = ',';
        Enddo;

        If hasRateHdg = *On;
           json += ']';
        Endif;

       Endsr;

       //=================================================================
       // Build Request Information
       //=================================================================
       Begsr BldReqInfo;
          If isUseEmployerInfo;
             Leavesr;
          Endif;

          json += '"requestParms" : {' +
           %Trim(BuildJsonNode('group#':result.group#)) + ',' +
           %Trim(BuildJsonNode('ratingState':result.rateState)) + ',' +
           %Trim(BuildJsonNode('effDate':result.effDate:'text')) +
           '}';

       Endsr;
       //=================================================================
       // Break parms out from inParms and validate
       //=================================================================
       Begsr GetParms;

        // Load the json tree
        //inJson = inParms;
        docNode = yajl_string_load_tree(inParms : errMsg500);
        If errMsg500 = ' ';

           // Load the data structure with the parameter values
           // from the tree
           node               = YAJL_object_find(docNode: 'group#');
           result.group#      = YAJL_get_string(node);
           node               = YAJL_object_find(docNode: 'ratingState'   );
           result.rateState   = YAJL_get_string(node);
           node               = YAJL_object_find(docNode: 'effDate');
           result.effDate     = YAJL_get_string(node);

           Reset i1;
           Reset dsEmpInfo;
           Dow YAJL_object_loop(docNode:i1:key:val);
              Monitor;
                 Select;
                 When key = 'employer';
                    dsEmpInfo.employer = %int(YAJL_get_string(val));
                 When key = 'division';
                    dsEmpInfo.division = %int(YAJL_get_string(val));
                 When key = 'year';
                    dsEmpInfo.year = %int(YAJL_get_string(val));
                 When key = 'showEmptyRate';
                    isShowEmptyRate = YAJL_get_string(val);
                 Endsl;
              On-error;
              Endmon;
           Enddo;
           isUseEmployerInfo = dsEmpInfo <> *zeros;

           // Free up the space by removing the tree
           yajl_tree_free(docNode);

           //======= Validate the incoming parameters ====================

           // Group must be passed in and be valid
           If result.group# = ' ';
              AddErrorMessage('"No group passed in"');
              groupError= *On;
           Else;
              Monitor;
                 group# = %Dec(result.group#:3:0);
              On-Error;
                 AddErrorMessage('"Invalid Numeric Data - Group"');
                 groupError= *On;
              EndMon;

              // No errors with group data
              If Not groupError;
                 // Is this a valid group
                 If ValidGrp(group#:rrn:UPco#) = *Off;
                    AddErrorMessage('"Invalid Group #"');
                 Else;
                    // Does the user have authority for this group?
                    If Groupauth(UPco#:group#:inUser) = *Off;
                       AddErrorMessage(
                        '"User does not have authority to group."');
                    EndIf;
                 EndIf;
              EndIf;
           EndIf;

           // State must be passed in and be valid
           If result.rateState = ' ';
              AddErrorMessage('"No state passed in."');
           Else;
              stateKey = result.rateState;
              Chain stateKey pst03;
              If Not %Found(ststl3);
                 AddErrorMessage('"Invalid State"');
              EndIf;
           EndIf;

           // Effective date must be passed in and be valid
           If result.effDate = ' ';
              effDate8 = DateTo8(systemDate());
              effDate7 = dtCYMD(effDate8);
           Else;
              Monitor;
                 effDate8 = %Dec(result.effDate:8:0);
              On-Error;
                 AddErrorMessage('"Invalid Numeric Data - Effective Date."');
                 dateError= *On;
              EndMon;

              // Is the date valid?
              If Not dateError;
                 effDate7 = dtCYMD(effDate8);
                 If effDate7 = 0;
                    AddErrorMessage('"Invalid Effective Date."');
                 EndIf;
              EndIf;
           EndIf;

        Else;
           AddErrorMessage('"' + %Trim(errMsg500) + '"');
           LeaveSr;
        EndIf;

        // Make sure Carrier Rating Default and Rating and Statistics
        //  are set up as of the group/state/effective date
        //  passed into the program.

        Setgt (UPco#:group#:stateKey:effDate7) prr00;
        Readpe (UPco#:group#:stateKey) prr00;
        If %Eof;
           AddErrorMessage('"Rating and Statistics is' +
            ' not set up for the group/state/effective date."');
           Leavesr;
        Endif;

        Chain (RRco#:RRfnd:RRst:RRefdt) pdf00;
        If Not(%Found);
           AddErrorMessage('"Carrier rating default set up' +
            ' does not exist for the group/state/effective date."');
           Leavesr;
        Endif;

       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
        strAuthority = ioRtCd;
        Clear ioRtCd;
        Chain (inUser) pup00;
        GetClientID(clientID);
       Endsr;

      /End-free

      *=========================================================
      * Add error messages to array
      *=========================================================
     pAddErrorMessage...
     p                 b
     dAddErrorMessage...
     d                 pi
     d inMessage                    500    varying const
      /Free
       decMsg# += 1;
       errMsgArray(decMsg#) = inMessage;
       ioRtCd = 'Error';
      /End-free
     pAddErrorMessage...
     p                 e

       //====================================================
       // Build Rating JSON
       //====================================================
     pBuildRateJSON    b
     dBuildRateJSON    pi

     ddecPolEntry      s             13  4
     ddecTblEntry      s             13  4
     ddoesMatch        s               n
     disNumeric        s               n
     dsavDftFlg        s                   like(DTdftFlg)
     dstrEntry         s                   like(DTentry)
     dstrTypeOfField   s             25    varying

      /Free
       strTypeOfField = CheckForFldType();
       If hasRateHdg = *Off;
          hasRateHdg = *On;
          json += ',"ratingFactors":[';
       Endif;
       json += mstComma + '{' +
        BuildJsonNode('key':DFkey) + ',' +
        BuildJsonNode('headingDisplayed':%trim(GetRateDescription())) + ',' +
        BuildJsonNode('required':DFrequire) + ',' +
        %Trim(BuildJsonNode('typeOfField':strTypeOfField)) + ',' +
        %Trim(BuildJsonNode('agentAllowedEntry':DFalwagt)) + ',' +
        '"detail":[';
       Setll (UPco#:group#:stateKey:DFefdt:DFkey) pdt00;
       Dou %Eof(WIRATDDTL);
          Reade(n) (UPco#:group#:stateKey:DFefdt:DFkey) pdt00;
          Select;
          When strTypeOfField = 'Text Box';
             DTeTyp = GetEntryType();
             DTdftFlg = 'Yes';
             Clear DTentry;
             Clear DTdesc;
             Clear DTwvType;
             Clear DTprmlvl;
             Clear DTmin;
             Clear DTmax;
          When %eof;
             Iter;
          Endsl;
          // Iter if processing Inc Limits or Ded and code is not active
          //  as of date passed in.
          If CodeNotValid();
             Iter;
          EndIf;
          If isUseEmployerInfo;
             // Update DT* feilds with EL* values based on the rating
             Exsr GetEmpValues;
          Endif;
          If not isShowEmptyRate;
             If DTentry = *blanks or DTentry = *zeros;
                Iter;
             Endif;
          Endif;
          json += dtlComma +
           '{' +
           BuildJsonNode('typeOfEntry':%trim(DTetyp)) + ',' +
           BuildJsonNode('entry':%trim(DTentry)) + ',' +
           BuildJsonNode('defaultFlag':%trim(DTdftflg));
          If DTdesc <> *Blanks;
             json += ',' +
              BuildJsonNode('description':%trim(DTdesc));
          Endif;
          If DTwvType <> *Blanks;
             json += ',' +
              BuildJsonNode('type':%trim(DTwvtype));
          Endif;
          If DTwvType = 'Specific';
             json += ',' +
              BuildJsonNode('count':%Editc(intWaivCnt:'X'));
          Endif;
          json += '}';
          dtlComma = ',';
       Enddo;
       json += ']}';

       //=================================================================
       // Load employer (EL*) values into DT* fields based on rating
       //=================================================================
       Begsr GetEmpValues;
        savDftFlg = DTdftFlg;
        Clear DTdftflg;
        Clear strEntry;
        Clear decPolEntry;
        Clear decTblEntry;
        isNumeric = *On;
        doesMatch = *Off;
        Select;
        When GTkey = 'CONCREDIT';
           strEntry = %editc(dsWDELP.ELasp1:'3');
        When GTkey = 'DEV1';
           strEntry = %editc(dsWDELP.ELasP2:'3');
        When GTkey = 'DEV2';
           strEntry = %editc(dsWDELP.ELdev2F:'3');
        When GTkey = 'REINSUR ';
           strEntry = %editc(dsWDELP.ELaRCP:'3');
        When GTkey = 'SAFETYFEE';
           strEntry = %editc(dsWDELP.ELsFPc:'3');
        When GTkey = 'SCHDEBCRED';
           If dsWDELP.ELdbCr <> 0;
              strEntry = %Editc(%Dec(dsWDELP.ELdbCr*100:5:2):'P');
           Else;
              strEntry = *Zero;
           Endif;
        When GTkey = 'SUBJECT1A';
           strEntry = %editc(dsWDELP.ELasPm:'3');
        When GTkey = 'SUBJECT1B';
           strEntry = %editc(dsWDELP.ELasPs:'3');
        When GTkey = 'VOLUNTARY';
           strEntry = %editc(dsWDELP.ELvLCA:'3');
        When GTkey = 'WAIVER';
           Select;
           When dsWDELP.ELwvPc <> 0;
              strEntry = %editc(dsWDELP.ELwvPc:'3');
           When dsWDELP.ELfchg <> 0;
              strEntry = %editc(dsWDELP.ELfchg:'3');
           Endsl;
        When GTkey = 'EXPMOD';
           strEntry = %editc(dsWDELP.ELexM1:'3');
        When GTkey = 'MEMBERFEE';
           strEntry = %editc(dsWDELP.ELmFee:'3');
        When GTkey = 'INCLIMIT';
           strEntry = dsWDELP.ELiLCd;
           If strEntry = *Blanks;
              strEntry = 'Standard Limits';
           Endif;
        When GTkey = 'DEDUCT';
           strEntry = dsWDELP.ELdCde;
        When GTkey = 'PRMDISC';
           Select;
           When dsWDELP.ELasp3 <> *Zero;
              strEntry = %editc(dsWDELP.ELasp3:'3');
           When dsWDELP.ELdisc <> *Blanks;
              strEntry = dsWDELP.ELdisc;
           Endsl;
        Endsl;
        Monitor;
           decPolEntry = %Dec(strEntry:13:4);
        On-Error;
           isNumeric = *Off;
        EndMon;
        If isNumeric = *On And decPolEntry = 0;
           strEntry = *Zero;
        Endif;
        Select;
        When isNumeric = *On;
           Monitor;
              decTblEntry = %Dec(Dtentry:13:4);
           On-Error;
           EndMon;
           If decPolEntry = decTblEntry;
              doesMatch = *On;
           Endif;
        Other;
           If strEntry = DTentry;
              doesMatch = *On;
           Endif;
        EndSl;
        Select;
        When doesMatch = *On And strEntry <> *Zero
         And strEntry <> *Blanks;
           DTdftFlg = 'Yes';
        When strTypeOfField = 'Text Box';
           DTentry = strEntry;
           DTdftFlg = 'Yes';
        When strEntry = *Zero Or strEntry = *Blanks;
           DTdftFlg = savDftFlg;
        EndSl;
       Endsr;
      /End-Free
     pBuildRateJSON    e
      //====================================================
      // Check if Ded or Inc Limit Code in Default Set Up is
      //  still active as of effective passed into program
      //====================================================
     pCodeNotValid     b
     dCodeNotValid     pi              n
     d dedCode         s              3a
     d limCode         s             10a
      /Free
       Select;
       When DFkey = 'DEDUCT';
          dedCode = DTentry;
          Setgt (UPco#:group#:stateKey:dedCode:effDate7) pdd02;
          Dou %Eof(WTDDL2);
             Readpe (UPco#:group#:stateKey:dedCode) pdd02;
             If %Eof;
                Leave;
             EndIf;
             If DDstat = 'A';
                Return *Off;
             Endif;
          Enddo;
          Return *On;
       When DFkey = 'INCLIMIT' And DTpgm <> 'WICARRAT';
          limCode = DTentry;
          Setgt (UPco#:group#:stateKey:limCode:effDate7) plm01;
          Dou %Eof(WTLML1);
             Readpe (UPco#:group#:stateKey:limCode) plm01;
             If %Eof;
                Leave;
             EndIf;
             If LMstat = 'A';
                Return *Off;
             Endif;
          Enddo;
          Return *On;
       Other;
          Return *Off;
       EndSl;
      /End-Free
     pCodeNotValid     e
      //====================================================
      // Count detail records and populate field type accordingly
      //====================================================
     pCheckForFldType  b
     dCheckForFldType  pi            10a   varying
      /Free
       Exec SQL
        Select Count(*)
         Into :decCount
         From WIRATDDT
         Where DTco# = :DFco# And DTfnd = :DFfnd
          And DTst = :DFst And DTkey = :DFkey And
          DTefdt = :DFefdt;
       Select;
       When decCount > 1;
          Return 'Drop Down';
       When decCount = 1;
          Return 'Check Box';
       Other;
          Return 'Text Box';
       EndSl;
      /End-Free
     pCheckForFldType  e
      //====================================================
      // Check if Any Ded Codes in Default Set Up are
      //  still active
      //====================================================
     pDedCodesNotSetUp...
     p                 b
     dDedCodesNotSetUp...
     d                 pi              n
     d code            s              3a
      /Free
       If GTkey <> 'DEDUCT';
          Return *Off;
       Endif;
       // Use eff date of most recent deductible default
       //  record found in FactorHasNoDefault to loop
       //  through detail set up.
       Setll (UPco#:group#:stateKey:GTkey:DFefdt) pdt01;
       Dou %Eof(WIRATDDTL);
          Reade (UPco#:group#:stateKey:GTkey:DFefdt) pdt01;
          If %Eof;
             Leave;
          Endif;
          code = DTentry;
       // Return *Off if an active record exists as of the
       //  effective date passed into the program
          Setgt (UPco#:group#:stateKey:code:effDate7) pdd02;
          Dou %Eof(WTDDL2);
             Readpe (UPco#:group#:stateKey:code) pdd02;
             If %Eof;
                Leave;
             EndIf;
             If DDstat = 'A';
                Return *Off;
             Endif;
          Enddo;
       Enddo;
       // No active codes exist in detail default setup
       Return *On;
      /End-Free
     pDedCodesNotSetUp...
     p                 e
      //====================================================
      // Check if Factor is set up in Rating and Stat
      //====================================================
     pDefaultNotSetUp...
     p                 b
     dDefaultNotSetUp...
     d                 pi              n
      /Free
       // WDRRP was read in GetParms subroutine
       Select;
       When GTkey = 'CONCREDIT';
          If RRcccd = *Zero;
             Return *On;
          EndIf;
       When GTkey = 'DEV1';
          If RRdv1c = *Zero;
             Return *On;
          Endif;
       When GTkey = 'DEV2';
          If  RRdv2c = *Zero;
             Return *On;
          Endif;
       When GTkey = 'REINSUR ';
          If RRarcc = *Zero;
             Return *On;
          Endif;
       When GTkey = 'SAFETYFEE';
          If RRsfpc = *Zero;
             Return *On;
          EndIf;
       When GTkey = 'SCHDEBCRED';
          If RRcrcd = *Zero Or RRdbcd = *Zero;
             Return *On;
          Endif;
       When GTkey = 'SUBJECT1A';
          If RR1acd = *Zero;
             Return *On;
          Endif;
       When GTkey = 'SUBJECT1B';
          If RR1bcd = *Zero;
             Return *On;
          Endif;
       When GTkey = 'VOLUNTARY';
          If RRvlcc = *Zero;
             Return *On;
          Endif;
       When GTkey = 'WAIVER';
          If RRwvcd = *Zero;
             Return *On;
          Endif;
       When GTkey = 'EXPMOD';
          If RReccd = *Zero;
             Return *On;
          Endif;
       When GTkey = 'MEMBERFEE';
          // Look at WDF1P for Membership Fee
          If MemberFeeNotSetUp();
             Return *On;
          Endif;
       When GTkey = 'INCLIMIT';
          If IncLimCodesNotSetUp();
             Return *On;
          Endif;
       When GTkey = 'DEDUCT';
          If DedCodesNotSetUp();
             Return *On;
          Endif;
       When GTkey = 'PRMDISC';
       EndSl;
       Return *Off;
      /End-Free
     pDefaultNotSetUp...
     p                 e
      //====================================================
      // Check if Factor has a default record
      //====================================================
     pFactorHasNoDefault...
     p                 b
     dFactorHasNoDefault...
     d                 pi              n
      /Free
       Chain (RRco#:RRfnd:RRst:GTkey:RRefdt) pdf01;
       Return Not(%Found);
      /End-Free
     pFactorHasNoDefault...
     p                 e
      //====================================================
      // Returns entry type from Generic Table
      //====================================================
     pGetEntryType     b
     dGetEntryType     pi            10
      /Free
       Chain (DFco#:'CRDF':DFkey) WTGTL;
       If %Found;
          Return GTcf01;
       EndIf;
       Return 'string';
      /End-Free
     pGetEntryType     e
      /Free
       //=====================================================
       // Returns optional heading from WDRRP else WTGTP
       //=====================================================
       Dcl-Proc GetRateDescription;
       Dcl-Pi *N Char(40);
       End-Pi;
       Select;
       When DFkey = 'CONCREDIT' And RRcchd <> *Blanks;
          Return RRcchd;
       When DFkey = 'DEV1' And RRdv1h <> *Blanks;
          Return RRdv1h;
       When DFkey = 'DEV2' And RRdv2h <> *Blanks;
          Return RRdv2h;
       When DFkey = 'EXPMOD';
          If RRmrrd <> *Zero Or RRmrrn <> *Zero Or RRmrrc <> *Zero;
             Return 'Merit Rating';
          Endif;
       When DFkey = 'SAFETYFEE' And RRsfhd <> *Blanks;
          Return RRsfhd;
       When DFkey = 'SCHCREDIT' And RRcrhd <> *Blanks;
          Return RRcrhd;
       When DFkey = 'SCHDEBIT' And RRdbhd <> *Blanks;
          Return RRdbhd;
       When DFkey = 'SUBJECT1A' And RR1ahd <> *Blanks;
          Return RR1ahd;
       When DFkey = 'SUBJECT1B' And RR1bhd <> *Blanks;
          Return RR1bhd;
       When DFkey = 'VOLUNTARY' And RRvlch <> *Blanks;
          Return RRvlch;
       EndSl;
       Chain (DFco#:'CRDF':DFkey) pgt00;
       Return GTdesc;
       End-Proc;
      /End-Free
      //====================================================
      // Check if any Inc Limits Codes in Default Set Up are
      //  still active
      //====================================================
     pIncLimCodesNotSetUp...
     p                 b
     dIncLimCodesNotSetUp...
     d                 pi              n
     d code            s             10a
      /Free
       If GTkey <> 'INCLIMIT';
          Return *Off;
       Endif;
       // Use eff date of most recent Inc Limits default
       //  record found in FactorHasNoDefault to loop
       //  through detail set up.
       Setll (UPco#:group#:stateKey:GTkey:DFefdt) pdt01;
       Dou %Eof(WIRATDDTL);
          Reade (UPco#:group#:stateKey:GTkey:DFefdt) pdt01;
          If %Eof;
             Leave;
          Endif;
          If DTpgm = 'WICARRAT';
             Return *Off;
          Endif;

          code = DTentry;
       // Return *Off if an active record exists as of the
       //  effective date passed into the program
          Setgt (UPco#:group#:stateKey:code:effDate7) plm01;
          Dou %Eof(WTLML1);
             Readpe (UPco#:group#:stateKey:code) plm01;
             If %Eof;
                Leave;
             EndIf;
             If LMstat = 'A';
                Return *Off;
             Endif;
          Enddo;
       Enddo;
       // No active codes exist in detail default setup
       Return *On;
      /End-Free
     pIncLimCodesNotSetUp...
     p                 e
      //====================================================
      // Check Group Detail for Membership Fee Set Up
      //====================================================
     pMemberFeeNotSetUp...
     p                 b
     dMemberFeeNotSetUp...
     d                 pi              n
      /Free
       If GTkey <> 'MEMBERFEE';
          Return *Off;
       Endif;
       Setgt (UPco#:group#:effDate7) pf101;
       Readpe (UPco#:group#) pf101;
       If F1mfee = 0;
          Return *On;
       Else;
          Return *Off;
       Endif;
      /End-Free
     pMemberFeeNotSetUp...
     p                 e

      *=========================================================
      * Process/report error messages
      *=========================================================
     pProcessErrorMessages...
     p                 b

     Di1               s             10i 0
      /Free
       If decMsg# <= 0;
          Return;
       Endif;
       json += ',"errorMsg": [';
       For i1 = 1 to decMsg#;
          json += errMsgArray(i1) + ',';
       Endfor;
       json = %scanrpl(',':']':json:%len(json)-1);
      /End-free
     pProcessErrorMessages...
     p                 e

      //====================================================
      // Write a temporary std limits records for Inc Limits
      //====================================================
     pWriteStdLimits   b
     dstrStdLimits     s             20a
      /Free
       If RRacc = 0 And RRdis = 0 And RRagg = 0;
          Return;
       Endif;
       // Default Standard Limits to Yes if no other
       //  increased limit records are set to Yes
       Exec Sql
        Select count(*)
        Into :intYesCount
        From WIRATDDT
        Where DTco# = :RRco#
         And DTfnd = :RRfnd
         And DTst = :RRst
         And DTefdt = :RRefdt
         And DTkey = 'INCLIMIT'
         And DTentry <> 'Standard Limits'
         And DTdftFlg = 'Yes';
       If intYesCount > 0;
          strYesOrNo = 'No';
       Else;
          strYesOrNo = 'Yes';
       Endif;
       Chain (RRco#:RRfnd:RRst:'INCLIMIT':RRefdt) pdf01;
       If Not(%Found);
          Exec Sql
           Insert Into WIRATDFT
            (DFco#,
             DFfnd,
             DFst,
             DFkey,
             DFefdt,
             DFdesc,
             DFpgm)
           Values
            (:RRco#,
             :RRfnd,
             :RRst,
             'INCLIMIT',
             :RRefdt,
             'Increased Limits',
             :Q1pgm);
       Endif;

       Chain (RRco#:RRfnd:RRst:RRefdt:'INCLIMIT':'Standard Limits') pdt00;
       If Not(%Found);
          Select;
          When RRemlb <> *Blanks;
             strStdLimits = RRemlb;
          Other;
             strStdLimits =
              %Trim(%Editc(RRacc:'4')) + '/' +
              %Trim(%Editc(RRagg:'4')) + '/' +
              %Trim(%Editc(RRdis:'4'));
          EndSl;
          Exec Sql
           Insert Into WIRATDDT
           (DTco#,
            DTfnd,
            DTst,
            DTkey,
            DTefdt,
            DTdesc,
            DTpgm,
            DTetyp,
            DTentry,
            DTdftflg)
           Values
           (:RRco#,
            :RRfnd,
            :RRst,
            'INCLIMIT',
            :RRefdt,
            :strStdLimits,
            :Q1pgm,
            'Code',
            'Standard Limits',
            :strYesOrNo);
       Else;
            DTdftFlg = strYesOrNo;
            Update pdt00;
       Endif;
      /End-Free
     pWriteStdLimits   e



