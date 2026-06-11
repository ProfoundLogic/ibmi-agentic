     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     h DFTACTGRP(*no) ACTGRP(*CALLER)
     h option(*nodebugio: *srcstmt)
     H bnddir('WTBNDDIR':'YAJL')
     H*====================================================================
     H* Revisions:
     H*
     H*  09/09/22 C:062 Ticket: 103577
     H*                 Populate receive date.
     H*
     H*  08/29/22 C:340 Project: WC2823
     H*                 - Update GetFRO & SaveFROI to add Witness
     H*
     H*  07/20/22 C:333 Project: WC2824
     H*                 - FROI Intake CS Standards
     H*
     H*  05/23/22 C:33  -Check live if HOLD not found
     H*
     H*  10/26/21 C:062 Ticket: 98702(146)
     H*                 -Populate fields for hospital phone
     H*                 -Translate the wage period
     H*
     H*  09/24/21 C:33  Seq: 98327
     H*                 Add more diagnostic information to the save error
     H*                  message.
     H*
     H*  08/04/21 C:180 Support 97167
     H*                 - Optionally check for lock on file WHFRP.
     H*
     H*  07/29/21 C:180 Support 97009
     H*                 - Remove edit for Initial treatment, Activity engaged.
     H*                 - Do not require fund to be associated with
     H*                   Webservice/connect when processing FROI import.
     H*
     H*  06/09/20 C:298 Project: SY420
     H*                 - Mask SSN
     H*====================================================================
     FWSFFL     IF   E           K DISK    Rename(pff01:pff00)
     FXMLPREL1  UF   E           K DISK

     D* this size needs to change
     Darray            s               a   len(32767)
     Djson             s               a   len(8500000) varying
     DjsonSub          s               a   len(8500000) varying
     dclientID         s              3a
     dintCo#           s              3  0
     dstrCo#           s              3a
     dstrSql           s           5000    varying inz

     D PSDS          ESDS                  extname(SZQ1P)

     d decAreaCode     s              3  0
     d decPhone        s              7  0
     d decZip          s              9  0
     d strAuthority    s             10a
     d strErrorMsg     s            500a   varying
     d strCity         s            100a
     d strLine1        s            256a
     d strLine2        s            256a
     d strState        s              2a
     d strType         s             25a   varying
     d decCo#          s              3  0 inz

     d i0              s             10i 0 inz
     d i1              s             10i 0 inz
     d i2              s             10i 0 inz
     d i3              s             10i 0 inz
     d i4              s             10i 0 inz
     d inJson          s               a   Len(2000000) varying
     d jsonExtract350  s            350a   varying
     d jsonExtract600  s            600a   varying
     d key             s             50a   varying
     d list            s                   Like(yajl_val)
     d node0           s                   Like(yajl_val)
     d node1           s                   Like(yajl_val)
     d node2           s                   Like(yajl_val)
     d node3           s                   Like(yajl_val)
     d node4           s                   Like(yajl_val)
     d val             s                   Like(yajl_val)

     d sysDate         s               d   datfmt(*iso)
     d sysDate7        s              7  0
     d sysTime6        s              6  0
       Dcl-s isFroiImport ind inz(*off);

     ddsWHFRPin      e ds                  extname(WHFRP) qualified inz
     ddsWHFRPout     e ds                  extname(WHFRP) qualified inz

     d* Generic data structures
     ddsAddress        ds                  inz qualified
     d line1                        256    varying
     d line2                        256    varying
     d city                         100    varying
     d state                          2
     d zip                            9  0

     ddsBasicInfo      ds                  inz qualified
     d number                        25    varying
     d fein                           9  0
     d name                         200    varying

     ddsKeyInfo        ds                  inz qualified
     D co#                            3
     D ss#                            9
     D accDt                          8
     D initDt                         8
     D initTime                       6

     ddsPersonalName   ds                  inz qualified
     d last                         100    varying
     d first                        100    varying
     d middle                       100    varying
     d full                         300    varying

     ddsPhone          ds                  inz qualified
     d areaCode                       3  0
     d phone                          7  0
     d type                          25    varying

     ddsCodeInfo       ds                  inz qualified
     d code                          25    varying
     d description                  100    varying
     d otherDesc                    100    varying

     d* Specific data structures
     ddsAccidentInfo   ds                  inz qualified
     d date                           8
     d time                           6
     d address                             likeds(dsAddress)

     ddsAgentInfo      ds                  inz qualified
     d basicInfo                           likeds(dsBasicInfo)

     ddsClaimInfo      ds                  inz qualified
     d year                           3
     d case#                         25    varying
     d jurisdictionSt                 2

     ddsClaimantInfo   ds                  inz qualified
     d ssn                            9
     d name                                likeds(dsPersonalName)
     d dob                            8
     d dod                            8
     d address                             likeds(dsAddress)
     d phone1                              likeds(dsPhone)
     d phoneWork                           likeds(dsPhone)
     d email                        250    varying
     d maritalStatus                  1
     d dependents#                    3  0
     d stateOfHire                    2    varying
     d gender                         1
     d occupationDesc               100    varying
     d employmentSts                       likeds(dsCodeInfo)

     ddsCompanyInfo    ds                  inz qualified
     d basicInfo                           likeds(dsBasicInfo)
     d address                             likeds(dsAddress)
     d phone1                              likeds(dsPhone)
     d fax                                 likeds(dsPhone)

     ddsContactInfo    ds                  inz qualified
     d name                                likeds(dsPersonalName)
     d phone1                              likeds(dsPhone)

     ddsDates          ds                  inz qualified
     d lastWorked                     8
     d reported                       8
     d disability                     8
     d returnToWork                   8
     d reportedAsWork                 8
     d adminNotified                  8
     d providedClmFrm                 8
     d hire                           8
     d preperation                    8
     d lastFullDayPd                  8
     d recieved                       8
     d fullSalary                     8
     d fifthDay                       8
     d eightDay                       8

     ddsEmployerInfo   ds                  inz qualified
     d basicInfo                           likeds(dsBasicInfo)
     d division                       5  0
     d policy#                       40    varying
     d carrierPolicy#                40    varying
     d dbaPersonal                    1
     d dbaName                      250    varying
     d fedPersonal                    1
     d fedName                      250    varying
     d addressMail                         likeds(dsAddress)
     d addressPhys                         likeds(dsAddress)
     d location                            likeds(dsLocationInfo)
     d phone1                              likeds(dsPhone)
     d email                        250    varying
     d sic                                 likeds(dsCodeInfo)
     d contact                      100    varying
     d selfInsured                    1
     d natureOfBusi                 100    varying
     d typeOfEmployer               100    varying
     d uiAccount#                    40    varying
     d stateRiskID                   40    varying

     ddsGroupInfo      ds                  inz qualified
     d basicInfo                           likeds(dsBasicInfo)
     d carrierCode                   25    varying
     d addressMail                         likeds(dsAddress)
     d addressPhys                         likeds(dsAddress)
     d phone1                              likeds(dsPhone)
     d altName                      100    varying
     d altAddress                          likeds(dsAddress)

     ddsHospitalInfo   ds                  inz qualified
     d name                         100    varying
     d address                             likeds(dsAddress)
     d phone1                              likeds(dsPhone)
     d extraInfo                    100    varying

     ddsLocationInfo   ds                  inz qualified
     d basicInfo                           likeds(dsBasicInfo)
     d addressMail                         likeds(dsAddress)
     d addressPhys                         likeds(dsAddress)
     d county                        25    varying

     ddsPhysicianInfo  ds                  inz qualified
     d name                         100    varying
     d address                             likeds(dsAddress)
     d phone1                              likeds(dsPhone)
     d extraInfo                    100    varying

     ddsPreparerInfo   ds                  inz qualified
     d name                         100    varying
     d title                        100    varying
     d phone1                              likeds(dsPhone)
     d extension                     10    varying
     d date                           8

     ddsRelative       ds                  inz qualified
     d name                         100    varying
     d relationship                 100    varying
     d address                             likeds(dsAddress)
     d phone1                              likeds(dsPhone)

     ddsThirdParty     ds                  inz qualified
     d name                         100    varying
     d contact                      100    varying
     d address                             likeds(dsAddress)
     d phone1                              likeds(dsPhone)

     ddsWageInfo       ds                  inz qualified
     d period                        25
     d fullPaid                      25
     d avgWeekly                     25
     d avgWeeklyEst                   1
     d fullSalary                     1
     d daysOverAWeek                  9
     d beginTime                      6
     d atSame                         1
     d preInjWeekly                  25
     d postInjWeekly                 25
     d perHour                       25
     d perDay                        25
     d weeks52Prior                  25

     ddsWitness        ds                  inz qualified
     d name                                likeds(dsPersonalName)
     d phone1                              likeds(dsPhone)

      /copy sPrAtrium
      /copy sPrDate
      /copy sPrFiles
      /copy sPrJson
      /copy sPrModule
      /copy sPrSecure
      /copy sPrSql
      /copy sPrString
      /copy sPrXml
      /copy wPrAgent
      /copy wPrCarrier
      /copy wPrEmplr
      /copy wPrGroup
      /copy wPrTable
      /copy wPrUsrPrms
      /include yajl_h
      /copy sPrMask

     dGetClientID      pr                  ExtPgm('RTVCLTID')
     d clientID                       3a

      *entry
     DRequestPgm       pr                  ExtPgm('WIFROI')
     D                              256
     D                               10
     D                                 a   len(8500000) varying
     D                                 a   len(8500000) varying
     D                               10
     DRequestPgm       pi
     D inRequest                    256
     D inUser                        10
     D inParms                         a   len(8500000) varying
     D outJSON                         a   len(8500000) varying
     D ioRtnCode                     10

      /Free
       Exec Sql Set Option Commit = *None;
       Exsr PgmStart;
       If isFroiImport = *Off;
          CreateAliasSQL('WHFRP_HLD':'WHFRP':'HOLD':*on);
          CreateAliasSQL('WHFRP_HST':'WHFRP':'HISTORY':*on);
       Else;
          CreateAliasSQL('WHFRP_HLD':'WHFRP':'HOLD':*on:*off);
          CreateAliasSQL('WHFRP_HST':'WHFRP':'HISTORY':*on:*off);
       Endif;

       Select;
       When inRequest = 'DeleteFROI';
          DeleteFROI();
       When inRequest = 'GetFROI';
          GetFROI();
       When inRequest = 'PrintFROI';
          PrintFROI();
       When inRequest = 'SaveFROI';
          SaveFROI();
       When inRequest = 'SubmitFROI';
          SubmitFROI();
       When inRequest = 'ListFROI';
          ListFROI();
       When inRequest = 'ListFROIHistory';
          ListFROIHistory();
       Other;
          Exsr BuildStub;
       EndSl;
       outJSON = json;
       *inlr = *on;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        BuildGiantJSON(json:'stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed.');
       Endsr;

       //--------------------------------------------------------
       Begsr PgmStart;
       //--------------------------------------------------------
       //*** Beg Chg *** 07/20/22 *******************************//
        //***If ioRtnCode = 'FROIimport';
        If ioRtnCode = 'FROIimport' Or ioRtnCode = 'GOFroiPrnt';
       //*** End Chg *** 07/20/22 *******************************//
           isFroiImport = *On;
           strAuthority = *blanks;
        Else;
           isFroiImport = *Off;
           strAuthority = ioRtnCode;
        Endif;
        Clear ioRtnCode;
        intCo# = UserCo#(inUser);
        strCo# = %Editc(intCo#:'X');
        GetClientID(clientID);
        sysDate  = %Date();
        sysDate7 = %Dec(sysDate:*Cymd);
        sysTime6 = %Dec(%Time():*hms);
       Endsr;
      /End-free

      //=============================================================
      // Convert dates
      //=============================================================
     PDateConvertFROI  b
     DDateConvertFROI  pi             8
     D inDate                         8    const
     D inCvt                          4    const varying
     D inReturnZero                   1n   const options(*nopass)
     DoutDate          s              8    inz
     DstrNullReturn    s              8    varying inz
      /Free
       If %parms >= 3 and inReturnZero;
          strNullReturn = *zeros;
       Endif;
       Monitor;
          Select;
          When inDate = *blanks;
             Return strNullReturn;
          When Upper(inCvt) = 'YYMD';
             outDate = %char(DtYYMD(%int(inDate)));
          When Upper(inCvt) = 'MDYY';
             outDate = %editc(DtMDYY2(%int(inDate)):'X');
          Endsl;
       On-error;
          outDate = strNullReturn;
       Endmon;
       Return outDate;
      /End-free
     PDateConvertFROI  e

      //=============================================================
      // Delete FROI
      //=============================================================
     PDeleteFROI       b

     DdsIn             ds                  likeds(dsKeyInfo) inz

     d errMsg          s          99999a   Varying Inz(' ')
     d dt7Accident     s              7  0 inz
     d dt8Accident     s              8  0 inz
     d dt7Initial      s              7  0 inz
     d dt8Initial      s              8  0 inz
     d acdtYYMD        s              8    inz
     d initYYMD        s              8    inz
     d initTime        s              6  0 inz
     d badData         s               n   Inz(*Off)

      /Free
       Exsr GetParms;
       If ioRtnCode <> 'Error';
          Exsr BldReturnInfo;
       EndIf;
       Exsr BldErrorsInfo;

       //=================================================================
       // Build Error Message Response                                      quests
       //=================================================================
       Begsr BldErrorsInfo;

          json = %scanrpl(',':'':json:%len(json)-1);
          // Finish by generating the errors
          json += ProcessErrorMessagesJson('*PRE');

       Endsr;

       //=================================================================
       // Build Request Information                                         quests
       //=================================================================
       Begsr BldReqInfo;

          json = '"requestParms" : {' +
                 BuildJsonNode('company':dsIn.co#) +
                 ', ' +
                 BuildJsonNode('claimantSSN':dsIn.ss#)  +
                 ', ' +
                 BuildJsonNode('accidentDate':dsIn.accDt) +
                 ', ' +
                 BuildJsonNode('initialDate':dsIn.initDt) +
                 '} , ';

       Endsr;

       //=================================================================
       // Build Return Information                                          quests
       //=================================================================
       Begsr BldReturnInfo;

          // DeleteFroi and create Response
          ExSr DeleteFroi;

       Endsr;

       //=================================================================
       // Delete Froi and send response                                     quests
       //=================================================================
       Begsr DeleteFroi;

        Exec sql
         select * into :dsWHFRPout
          from WHFRP
          Where FRco = :decCo# And FRss = :dsIn.ss# and FRacDt = :acdtYYMD
           and FRiDt = :initYYMD and FRiTm = :dsIn.initTime;

          // Now delete from the ement
          Exec SQL Delete from WHFRP_HLD
           Where FRco = :decCo# And FRss = :dsIn.ss# and FRacDt = :acdtYYMD
            and FRiDt = :initYYMD and FRiTm = :dsIn.initTime;

          If sqlSTT <> '00000';
           AddErrorMessageJson('FROI not deleted.  Unable to access record.':
             ioRtnCode);
          Else;
           LogFROI(dsWHFRPout.FRCo:dsWHFRPout.FRFnd:dsWHFRPout.FREmp:
            dsWHFRPout.FRDiv:dsWHFRPout.FRSS:dsWHFRPout.FRAcdt:
            dsWHFRPout.FRdtrp:inUser:'FROI Deleted':'FROI has been deleted ' +
            ' by user ' + %trim(inUser) + '.');
          EndIf;

       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;

        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson(%Trim(strErrorMsg):ioRtnCode);
           Leavesr;
        Endif;

        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'keyInfo';
              Reset i1;
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'company';
                    dsIn.co# = YAJL_get_string(val);
                 When key = 'claimantSSN';
                    dsIn.ss# = YAJL_get_string(val);
                 When key = 'accidentDate';
                    dsIn.accDt = YAJL_get_string(val);
                 When key = 'initialDate';
                    dsIn.initDt = YAJL_get_string(val);
                 When key = 'initialTime';
                    dsIn.initTime = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;

        json = json + '}';

        If strAuthority = '*View';
           AddErrorMessageJson('User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ').'
            :ioRtnCode);
           Leavesr;
        Endif;

        // Company # must be valid if not then default to user parms.
        Monitor;
           decCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        If decCo# = *zero;
           decCo# = intCo#;
        Endif;

        // Social Security # must be passed
        If dsIn.ss# = ' ';
           AddErrorMessageJson('Social Security# is Blank':ioRtnCode);
           badData = *On;
        EndIf;

        // Accident Date must be valid
        Monitor;
           dt8Accident = %int(dsIn.accDt);
           dt7Accident = DtCYMD(dt8Accident);
           acdtYYMD    = DateConvertFROI(dsIn.accDt:'YYMD');
        On-error;
           AddErrorMessageJson('Invalid Accident Date':ioRtnCode);
           badData = *On;
        Endmon;

        // Initial Entry Date must be valid
        Monitor;
           dt8Initial = %int(dsIn.initDt);
           dt7Initial = DtCYMD(dt8Initial);
           initYYMD   = DateConvertFROI(dsIn.initDt:'YYMD');
        On-error;
           If dsIn.initDt <> *blank;
              AddErrorMessageJson('Invalid Initial Entry Date':ioRtnCode);
              badData = *On;
           Endif;
        Endmon;

        // Initial Entry Time must be valid
        Monitor;
           initTime   = %int(dsIn.initTime);
        On-error;
           AddErrorMessageJson('Invalid Initial Entry Time':ioRtnCode);
           badData = *On;
        Endmon;

        // Froi Must Exist
        If Not badData;
           If Not FroiFound(decCo#:dsIn.ss#:acdtYYMD:initYYMD:initTime);
              AddErrorMessageJson('Froi Does Not Exist':ioRtnCode);
              badData = *On;
           EndIf;
        EndIf;

       Endsr;

      /End-free
     PDeleteFROI       e

      //=============================================================
      // Edit FROI data
      //=============================================================
     PEditFROI         b
     DEditFROI         pi             1n
     D inCo#                          3  0 const
     D inSS#                          9    const
     D inAcDt                         8    const
     D inInitDt                       8    const
     D inInitTime                     6  0 const

     ddsWHFRPin      e ds                  extname(WHFRP) qualified inz
      /Free
       Reset dsWHFRPin;
       Exec sql
        select * into :dsWHFRPin
         from WHFRP_HLD
         where FRco = :inCo# and FRss = :inSS# and FRacDt = :inAcDt
          and FRiDt = :inInitDt and FRiTm = :inInitTime;
       //*** Begin Add  ***  05/23/22  ***************************
       If not IsValidFetch(sqlstt);
          Exec sql
           select * into :dsWHFRPin
            from WHFRP
            where FRco = :inCo# and FRss = :inSS# and FRacDt = :inAcDt
             and FRiDt = :inInitDt and FRiTm = :inInitTime;
       Endif;
       //*** End   Add  ***  05/23/22  ***************************
       If not IsValidFetch(sqlstt);
          AddErrorMessageJson('FROI not found for requested information.'
           :ioRtnCode);
       Endif;
       If isFroiImport = *On and dsWHFRPin.FRfnd <> *zero;
          ioRtnCode = *blanks;
       Else;
          // Group needs to be available in connect
          If dsWHFRPin.FRfnd <> *zero and
           not UseGroupInPortal(inCo#:dsWHFRPin.FRfnd:'FROI');
             AddErrorMessageJson('Group ' + %char(dsWHFRPin.FRfnd) +
              ' is not included for service requested ' +
              '(' + %trim(inRequest) + ').':ioRtnCode);
          Endif;
       Endif;
       Select;
       When ioRtnCode = 'Error';
       //When dsWHFRPin.FRJRST = ;
       Other;
          Exsr EditA1;
       Endsl;
       Return ioRtnCode = 'Error';
       //-------
       Begsr EditA1;
        // First/Last Name
        If dsWHFRPin.FRlsn = *blanks or dsWHFRPin.FRfsn = *blanks;
           AddErrorMessageJson('First and last name required.':
             ioRtnCode);
        Endif;
        // Address 1/City/St/Zip
        If dsWHFRPin.FRadr1 = *blanks;
           AddErrorMessageJson('Address required.':
             ioRtnCode);
        Endif;
        If dsWHFRPin.FRcty = *blanks or dsWHFRPin.FRst = *blanks or
         dsWHFRPin.FRzip = *zeros;
           AddErrorMessageJson('City, state and zip code required.':
             ioRtnCode);
        Endif;
        // Date Employer Notified
        If dsWHFRPin.FRdtRp = *blanks;
           AddErrorMessageJson('Date Employer Notified required.':
             ioRtnCode);
        Endif;
        // Type of Injury
        If dsWHFRPin.FRnatr = *blanks;
           AddErrorMessageJson('Type of Injury required.':
             ioRtnCode);
        Endif;
        // Part
        If dsWHFRPin.FRpart = *blanks;
           AddErrorMessageJson('Part of Body required.':
             ioRtnCode);
        Endif;
        // Employer Premises
        If dsWHFRPin.FReAdr = *blanks;
           AddErrorMessageJson('Employer Premises required.':
             ioRtnCode);
        Endif;
        // Activity
        If isFroiImport = *Off;
           If dsWHFRPin.FR60A3 = *blanks;
              AddErrorMessageJson('Activity Engaged In required.':
                ioRtnCode);
           Endif;
        Endif;
        // How Occurred
        If dsWHFRPin.FR80A1 = *blanks;
           AddErrorMessageJson('How Injury Occurred required.':
             ioRtnCode);
        Endif;
        // Initial Treatement
        If isFroiImport = *Off;
           If dsWHFRPin.FRiTrt = *blanks;
              AddErrorMessageJson('Initial Treatment required.':
                ioRtnCode);
           Endif;
        Endif;
       Endsr;
      /End-free
     PEditFROI         e

      //=============================================================
      // Determine if the FROI exists
      //=============================================================
     PFroiFound        b
     DFroiFound        pi              n
     D intCompany                     3  0 const
     D strSocSec#                     9    const
     D straccDate                     8    const
     D strinitDate                    8    const
     D inInitTime                     6  0 const

     d count           s              5  0 inz
     D company#        s              3  0
     D group           s              3  0 inz
     D socSec#         s              9
     D accDate         s              8
     D initDate        s              8
     D initTime        s              6

      /free

          company# = intCompany;
          socSec#  = strSocSec#;
          accDate  = strAccDate;
          initDate = strInitDate;

          // Find FROI
          Exec SQL
           Select FRfnd Into :group
            From WHFRP_HLD
            Where FRco = :company# And FRss = :socSec# And FRacDt = :accDate
             and FRiDt = :initDate and FRiTm = :inInitTime;

          If isFroiImport = *On and group <> *zero;
             ioRtnCode = *blanks;
          Else;
             If group <> *zero and not UseGroupInPortal(company#:group:'FROI');
                AddErrorMessageJson('Group ' + %char(group) +
                 ' is not included for service requested ' +
                 '(' + %trim(inRequest) + ').':ioRtnCode);
             Endif;
          Endif;

          Return IsValidFetch(sqlstt);

      /End-free

     PFroiFound        e

      //=============================================================
      // Get FROI information
      //=============================================================
     PGetFROI          b
     DGetFROI          pr

     ddecAgt#          s              5  0 inz
     ddecCo#           s              3  0 inz
     ddtAccident       s               d   inz
     ddt7Accident      s              7  0 inz
     ddt8Accident      s              8  0 inz
     ddtInitial        s               d   inz
     ddt7Initial       s              7  0 inz
     ddt8Initial       s              8  0 inz
     disError          s              1n   inz
     drrn1             s              9  0 inz
     drrn2             s              9  0 inz
     dstrName40        s             40    inz
     dstrStatus        s             50    inz varying
     dtmInitial        s               t   inz
     dtm6Initial       s              6  0 inz
      //*** Beg Add *** 08/29/22 *******************************//
     ddecPos           s              2  0 inz
      //*** End Add *** 08/29/22 *******************************//

     ddsSMCOP        e ds                  extname(SMCOP) inz qualified
     ddsWDELP        e ds                  extname(WDELP) inz qualified
     ddsWMALP        e ds                  extname(WMALP) inz qualified
     ddsWMEMP        e ds                  extname(WMEMP) inz qualified
     ddsWMFNP        e ds                  extname(WMFNP) inz qualified

     DdsIn             ds                  likeds(dsKeyInfo) inz
      /Free
       Exsr GetParms;
       Exsr BuildOutput;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        Exsr SqlProcess;
        //Exsr BuildStub;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ProcessErrorMessagesJson('*PRE');
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {GetFROI}'));
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;

        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'keyInfo';
              Reset i1;
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'company';
                    dsIn.co# = YAJL_get_string(val);
                 When key = 'claimantSSN';
                    dsIn.ss# = YAJL_get_string(val);
                 When key = 'accidentDate';
                    dsIn.accDt = YAJL_get_string(val);
                 When key = 'initialDate';
                    dsIn.initDt = YAJL_get_string(val);
                 When key = 'initialTime';
                    dsIn.initTime = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;
        json = json + '},';
        Monitor;
           decCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        If decCo# = *zero;
           decCo# = intCo#;
        Endif;
        Monitor;
           dt8Accident = %int(dsIn.accDt);
           dt7Accident = DtCYMD(dt8Accident);
           dtAccident = CvtToDate(dt8Accident);
        On-error;
        Endmon;
        Monitor;
           dt8Initial = %int(DateConvertFROI(dsIn.initDt:'YYMD'));
           dt7Initial = DtCYMD(dt8Initial);
           dtInitial = CvtToDate(dt8Initial);
        On-error;
        Endmon;
        Monitor;
           tm6Initial = %int(dsIn.initTime);
        On-error;
        Endmon;
        Exsr Validate;
       Endsr;

       //=================================================================
       // Build json
       //=================================================================
       Begsr BuildJson;
        json += '"status":{';
         json += BuildJsonNode('code':dsWHFRPout.FRstat) + ',';
         json += BuildJsonNode('description':strStatus) + '},';
        json += '"keyInfo":{' +
         BuildJsonNode('company':%char(dsWHFRPout.FRco)) + ',' +
         BuildJsonNode('claimantSSN':dsWHFRPout.FRss) + ',' +
         BuildJsonNode('accidentDate'
          :DateConvertFROI(dsWHFRPout.FRacDt:'MDYY')) + ',' +
         BuildJsonNode('initialDate'
          :DateConvertFROI(dsWHFRPout.FRiDt:'MDYY')) + ',' +
         BuildJsonNode('initialTime':%char(dsWHFRPout.FRiTm)) + '},';
        Exsr BuildJsonCompany;
        Exsr BuildJsonGroup;
        Exsr BuildJsonEmployer;
        Exsr BuildJsonAgent;
        Exsr BuildJsonClaimaint;
      //*** Beg Add *** 08/29/22 *******************************//
        Exsr BuildJsonWitness;
      //*** End Add *** 08/29/22 *******************************//
        Exsr BuildJsonPhysician;
        Exsr BuildJsonHospital;
        Exsr BuildJsonPreparer;
        Exsr BuildJsonAccident;
        json += BuildJsonNode('year':%char(dsWHFRPout.FRfyr)) + ',';
        json += BuildJsonNode('case':%char(dsWHFRPout.FRcase)) + ',';
        json += BuildJsonNode('jurisdictionState':dsWHFRPout.FRjrSt) + ',';
      //json += BuildJsonNode('agencyClaimNumber':dsWHFRPout.) + ',';
        json += '"class":{' +
         BuildJsonNode('code':%char(dsWHFRPout.FRclas)) + ',' +
         BuildJsonNode('description':
          GetPayrollDesc(decCo#:dt7Accident:dsWHFRPout.FRjrSt:dsWDELP.ELrtCd:
           dsWHFRPout.FRclas:'')) + '},';
        Exsr BuildJsonWage;
        json += '"nature":{' +
         BuildJsonNode('code':dsWHFRPout.FRnatr) + ',' +
         BuildJsonNode('description':NatureCdDs(decCo#:dsWHFRPout.FRnatr))+','+
         BuildJsonNode('otherdescription':'') + '},';
        json += '"part":{' +
         BuildJsonNode('code':dsWHFRPout.FRpart) + ',' +
         BuildJsonNode('description':PartCdDs(decCo#:dsWHFRPout.FRpart))+','+
         BuildJsonNode('otherdescription':'') + '},';
        json += '"cause":{' +
         BuildJsonNode('code':dsWHFRPout.FRcaus) + ',' +
         BuildJsonNode('description':CauseCdDs(decCo#:dsWHFRPout.FRcaus))+','+
         BuildJsonNode('otherdescription':'') + '},';
        Exsr BuildJsonDates;
        Exsr BuildJsonContact;
        json += BuildJsonNode('employerPremises':dsWHFRPout.FReAdr) + ',';
        json += '"initialTreatment":{' +
         BuildJsonNode('code':dsWHFRPout.FRiTrt) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(decCo#:'INIT':dsWHFRPout.FRiTrt)) + '},';
        json += '"safeguard":{' +
         BuildJsonNode('provided':dsWHFRPout.FR1a1) + ',' +
         BuildJsonNode('used':dsWHFRPout.FR1a2) + '},';
        json +=
         BuildJsonNode('equipmentUsed':dsWHFRPout.FR60a1+dsWHFRPout.FR60a2)+',';
        json +=
         BuildJsonNode('employeeActivity':dsWHFRPout.FR60a3+dsWHFRPout.FR60a4) +
          ',';
        json +=
         BuildJsonNode('workProcess':dsWHFRPout.FR60a5+dsWHFRPout.FR60a6) +
          ',';
        json +=
         BuildJsonNode('howInjuryOccurred':dsWHFRPout.FR80a1+dsWHFRPout.FR80a2)+
          ',';
        // Maintenance information
        json += '"maintenanceInfo":{' +
         BuildJsonNode('program':dsWHFRPout.FRpgm) + ',' +
         BuildJsonNode('user':dsWHFRPout.FRuser) + ',' +
         BuildJsonNode('date':DateConvertFROI(dsWHFRPout.FRdate:'MDYY')) +','+
         BuildJsonNode('time':%char(dsWHFRPout.FRtime)) + ',' +
         BuildJsonNode('initialProgram':dsWHFRPout.FRiPgm) + ',' +
         BuildJsonNode('initialUser':dsWHFRPout.FRiUsr) + ',' +
         BuildJsonNode('initialDate':
          DateConvertFROI(dsWHFRPout.FRiDt:'MDYY')) + ',' +
         BuildJsonNode('initialTime':%char(dsWHFRPout.FRiTm)) + '},';

        json = %scanrpl(',':'':json:%len(json)-1);
       Endsr;

       //=================================================================
       // Build json - accident information
       //=================================================================
       Begsr BuildJsonAccident;
        json += '"accident":{';
        json += BuildJsonNode('date':
         DateConvertFROI(dsWHFRPout.FRacDt:'MDYY')) + ',';
        json += BuildJsonNode('time':%char(dsWHFRPout.FRacTm)) + ',';
        json += BuildJsonAddress(dsWHFRPout.FR40a1:dsWHFRPout.FR40a2:
          dsWHFRPout.FR25a1:dsWHFRPout.FR2a1:%char(dsWHFRPout.FR9n2):
          'address') +',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - agent information
       //=================================================================
       Begsr BuildJsonAgent;
        decAgt# = GetEmpAgt(decCo#:dsWHFRPout.FRfnd:dsWHFRPout.FRemp:
         dsWHFRPout.FRdiv:dsWHFRPout.FRfyr:dt7Accident:strName40);
        json += '"agent":{';
        json += BuildJsonNode('number':%char(decAgt#)) + ',';
        json += BuildJsonNode('name':strName40) + ',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - claimant information
       //=================================================================
       Begsr BuildJsonClaimaint;
        json += '"claimant":{';
        json += BuildJsonNode('ssn':dsWHFRPout.FRss) + ',';
        json += BuildJsonName(dsWHFRPout.FRfsn:dsWHFRPout.FRlsn:
         dsWHFRPout.FRmid:'name') + ',';
        json += BuildJsonNode('dob':DateConvertFROI(dsWHFRPout.FRdtOB:'MDYY')) +
         ',';
        json += BuildJsonNode('dod':DateConvertFROI(dsWHFRPout.FRdOD:'MDYY')) +
         ',';
        json += BuildJsonAddress(dsWHFRPout.FRadr1:dsWHFRPout.FRadr2:
         dsWHFRPout.FRcty:dsWHFRPout.FRst:%char(dsWHFRPout.FRzip):'address') +
         ',';
        json += '"phone":[{' +
          BuildJsonPhone(%char(dsWHFRPout.FRarea):%char(dsWHFRPout.FRphn):
           '':'phone1') +'},{'+
          BuildJsonPhone('':'':'':'work') +'}],';
        json += BuildJsonNode('email':dsWHFRPout.FRmail) +',';
        json += BuildJsonNode('maritalStatus':dsWHFRPout.FRmSts) +',';
        json += BuildJsonNode('numberDependents':%char(dsWHFRPout.FRdep)) +',';
        json += BuildJsonNode('stateOfHire':dsWHFRPout.FRstHr) +',';
        json += BuildJsonNode('gender':dsWHFRPout.FRsex) +',';
        json += BuildJsonNode('occupationDescription':dsWHFRPout.FRocDs) +',';
        json += '"employmentStatus":{' +
         BuildJsonNode('code':dsWHFRPout.FReSts) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(decCo#:'EMST':dsWHFRPout.FReSts)) + '},';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - company information
       //=================================================================
       Begsr BuildJsonCompany;
        Reset dsSMCOP;
        Exec sql
         select * into :dsSMCOP
          from SMCOP
          where COco# = :dsWHFRPout.FRco;
        json += '"company":{';
        json += BuildJsonNode('number':%char(dsWHFRPout.FRco)) +',';
        json += BuildJsonNode('name':dsSMCOP.COname) +',';
        json += BuildJsonNode('fein':%char(dsSMCOP.COfein)) +',';
        json += BuildJsonAddress(dsSMCOP.COadr1:dsSMCOP.COadr2:dsSMCOP.COcty:
         dsSMCOP.COst:%char(dsSMCOP.COzip):'address') +',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - contact information
       //=================================================================
       Begsr BuildJsonContact;
        json += '"contact":{';
        json += '"name":{';
        json += BuildJsonNode('full':dsWHFRPout.FRcont);
        // json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
        json += '"phone":{';
        json += BuildJsonNode('areacode':dsWHFRPout.FRcar) +',';
        json += BuildJsonNode('phone':dsWHFRPout.FRcphn) +',';
        json += BuildJsonNode('type':' ') + '}';

        // json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - date information
       //=================================================================
       Begsr BuildJsonDates;
        json += '"dates":{';
        json += BuildJsonNode('lastWorked':
         DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';
        json += BuildJsonNode('reported':
         DateConvertFROI(dsWHFRPout.FRdtRp:'MDYY')) + ',';
        json += BuildJsonNode('disability':
         DateConvertFROI(dsWHFRPout.FRdbDt:'MDYY')) + ',';
        json += BuildJsonNode('returnToWork':
         DateConvertFROI(dsWHFRPout.FRrTW:'MDYY')) + ',';
        //json += BuildJsonNode('reportedAsWorkRelated':
        // DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';
        //json += BuildJsonNode('adminNotified':
        // DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';
        //json += BuildJsonNode('employeeProvidedClaimForm':
        // DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';
        json += BuildJsonNode('hire':
         DateConvertFROI(dsWHFRPout.FRhiDt:'MDYY')) + ',';
        //json += BuildJsonNode('preperation':
        // DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';
        //json += BuildJsonNode('lastFullDayPaid':
        // DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';
        json += BuildJsonNode('recieved':
         DateConvertFROI(dsWHFRPout.FRrcvd:'MDYY')) + ',';
        //json += BuildJsonNode('fullSalary':
        // DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';
        //json += BuildJsonNode('fifthDayOfTotalOrPartial':
        // DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';
        //json += BuildJsonNode('eightDayOfDisability':
        // DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY')) + ',';


        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - employer information
       //=================================================================
       Begsr BuildJsonEmployer;
        Reset dsWMEMP;
        Exec sql
         select * into :dsWMEMP
          from WMEMP
          where EMco# = :dsWHFRPout.FRco and EMfnd = :dsWHFRPout.FRfnd
           and EMemp# = :dsWHFRPout.FRemp and EMdiv = :dsWHFRPout.FRdiv;
        Reset dsWDELP;
        Exec sql
         select * into :dsWDELP
          from WDELP
          where ELco# = :dsWHFRPout.FRco and ELfnd = :dsWHFRPout.FRfnd
           and ELemp# = :dsWHFRPout.FRemp and ELdiv = :dsWHFRPout.FRdiv
           and ELfyr = :dsWHFRPout.FRfyr;
        If not IsValidFetch(sqlstt);
           Exec sql
            select * into :dsWDELP
             from WDELP
             where ELco# = :dsWHFRPout.FRco and ELfnd = :dsWHFRPout.FRfnd
              and ELemp# = :dsWHFRPout.FRemp and ELdiv = :dsWHFRPout.FRdiv
              and ELpfDt <= :dt7Accident and ELptDt > :dt7Accident;
        Endif;
        json += '"employer":{';
        json += BuildJsonNode('fein':%char(dsWMEMP.EMfein)) +',';
        json += BuildJsonNode('number':%char(dsWMEMP.EMemp#)) +',';
        json += BuildJsonNode('division':%char(dsWMEMP.EMdiv)) +',';
        json += BuildJsonNode('policyNumber':
         %editc(dsWMEMP.EMfnd:'X')+'-'+%editc(dsWMEMP.EMemp#:'X')+'-'+
         %editc(dsWHFRPout.FRfyr:'X')) + ',';
        json += BuildJsonNode('carrierPolicyNumber':
         GetCarrPol(dsWMEMP.EMco#:dsWMEMP.EMfnd:dsWMEMP.EMemp#:dsWMEMP.EMdiv:
         dsWHFRPout.FRfyr)) + ',';
        json += '"name":{';
        json += BuildJsonNode('dbaPersonalName':dsWMEMP.EMnamP) +',';
        json += BuildJsonNode('dba':dsWMEMP.EMname) +',';
        json += BuildJsonNode('federalPersonalName':dsWMEMP.EMfdNP) +',';
        json += BuildJsonNode('federal':dsWMEMP.EMfdNm) +',';
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
        json += '"address":{';
         json += BuildJsonAddress(dsWMEMP.EMmAd1:dsWMEMP.EMmAd2:dsWMEMP.EMmCty:
          dsWMEMP.EMmSt:%char(dsWMEMP.EMmZip):'mailing') +',';
         json += BuildJsonAddress(dsWMEMP.EMpAd1:dsWMEMP.EMpAd2:dsWMEMP.EMpCty:
          dsWMEMP.EMpSt:%char(dsWMEMP.EMpZip):'physical') +',';
         json = %scanrpl(',':'':json:%len(json)-1);
         json += '},';
        json += BuildJsonPhone(%char(dsWMEMP.EMarea):%char(dsWMEMP.EMphn#):
           '':'phone') +',';
        json += '"sic":{' +
         BuildJsonNode('code':dsWMEMP.EMsic) + ',' +
         BuildJsonNode('description':
          GetGenCodeDs(decCo#:'SIC':dsWMEMP.EMsic)) + '},';
        json += BuildJsonNode('email':'') +',';
        json += '"policyDates": {' +
         %trim(BuildJsonNode('effective':%char(dsWDELP.ELpfDt):'date')) +','+
         %trim(BuildJsonNode('expiration':%char(dsWDELP.ELptDt):'date')) +'},';
       Exsr BuildJsonLocation;

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - group information
       //=================================================================
       Begsr BuildJsonGroup;
        Reset dsWMFNP;
        Exec sql
         select * into :dsWMFNP
          from WMFNP
          where FNco# = :dsWHFRPout.FRco and FNfnd = :dsWHFRPout.FRfnd;
        json += '"group":{';
        json += BuildJsonNode('number':%char(dsWHFRPout.FRfnd)) +',';
        json += BuildJsonNode('name':dsWMFNP.FNname) +',';
        //json += BuildJsonNode('fein':%char(dsWMFNP.FNfein)) +',';
        json += BuildJsonNode('carrierCode':dsWMFNP.FNcarr) +',';
        json += '"address":{';
         json += BuildJsonAddress(dsWMFNP.FNmAd1:dsWMFNP.FNmAd2:dsWMFNP.FNmCty:
          dsWMFNP.FNmSt:%char(dsWMFNP.FNmZip):'mailing') +',';
         json += BuildJsonAddress(dsWMFNP.FNpAd1:dsWMFNP.FNpAd2:dsWMFNP.FNpCty:
          dsWMFNP.FNpSt:%char(dsWMFNP.FNpZip):'physical') +',';
         json = %scanrpl(',':'':json:%len(json)-1);
         json += '},';
        json += '"alternate":{';
        json += BuildJsonNode('name':dsWMFNP.FNaNam) +',';
        json += BuildJsonAddress(dsWMFNP.FNaAd1:dsWMFNP.FNaAd2:dsWMFNP.FNaCty:
         dsWMFNP.FNaSt:%char(dsWMFNP.FNaZip):'address') +',';
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - hospital information
       //=================================================================
       Begsr BuildJsonHospital;
        json += '"hospital":{';
        json += BuildJsonNode('name':dsWHFRPout.FRhosN) +',';
        json += BuildJsonAddress(dsWHFRPout.FRhosA:'':dsWHFRPout.FRhosC:
         dsWHFRPout.FRhosS:%char(dsWHFRPout.FRhosZ):'address') +',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - Location information
       //=================================================================
       Begsr BuildJsonLocation;
        Exec sql
         select * into :dsWMALP
          from WMALP
          where ALco# = :dsWHFRPout.FRco and ALfnd = :dsWHFRPout.FRfnd
           and ALemp# = :dsWHFRPout.FRemp and ALdiv = :dsWHFRPout.FRdiv
           and ALelId = :dsWHFRPout.FRctg1;
        json += '"location":{';
        json += BuildJsonNode('number':dsWMALP.ALelId) +',';
        json += BuildJsonNode('name':dsWMALP.ALname) +',';
        json += '"address":{';
         json += BuildJsonAddress(dsWMALP.ALmAd1:dsWMALP.ALmAd2:dsWMALP.ALmCty:
          dsWMALP.ALmSt:%char(dsWMALP.ALmZip):'mailing') +',';
         json += BuildJsonAddress(dsWMALP.ALpAd1:dsWMALP.ALpAd2:dsWMALP.ALpCty:
          dsWMALP.ALpSt:%char(dsWMALP.ALpZip):'physical') +',';
         json = %scanrpl(',':'':json:%len(json)-1);
         json += '},';
        json += BuildJsonNode('county':'') +',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //*** Beg Chg *** 08/29/22 *******************************//
       //=================================================================
       // Build json - Witness information
       //=================================================================
       Begsr BuildJsonWitness;
        decPos = 0;
        decPos = %scan(',':%Trim(dsWHFRPout.FR80a3):1);
        json += '"witness":{';
        If decPos > 1;
        json += BuildJsonNode('name':%SubSt(dsWHFRPout.FR80a3:1:decPos-1)) +',';
        json += BuildJsonNode('phone':%Trim(%SubSt(dsWHFRPout.FR80a3:decPos+1)))
                                                                           +',';
        Else;
        json += BuildJsonNode('name':'') +',';
        json += BuildJsonNode('phone':'') +',';
        EndIf;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;
       //*** End Chg *** 08/29/22 *******************************//
       //=================================================================
       // Build json - physician information
       //=================================================================
       Begsr BuildJsonPhysician;
        json += '"physician":{';
        json += BuildJsonNode('name':dsWHFRPout.FRphyN) +',';
        json += BuildJsonAddress(dsWHFRPout.FRphyA:'':dsWHFRPout.FRphyC:
         dsWHFRPout.FRphyS:%char(dsWHFRPout.FRphyZ):'address') +',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - preparer information
       //=================================================================
       Begsr BuildJsonPreparer;
        json += '"preparer":{';
        json += BuildJsonNode('name':dsWHFRPout.FRprNm) +',';
        If %len(%trimr(dsWHFRPout.FRprPh)) = 7;
           dsWHFRPout.FRprPh = '   ' + dsWHFRPout.FRprPh;
        Endif;
        json += BuildJsonPhone(%subst(dsWHFRPout.FRprPh:1:3):
         %subst(dsWHFRPout.FRprPh:4:7):'':'phone') +',';
        json += BuildJsonNode('preparedDate':
         DateConvertFROI(dsWHFRPout.FRprep:'MDYY')) + ',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build json - wage information
       //=================================================================
       Begsr BuildJsonWage;
        json += '"wage":{';
        json += BuildJsonNode('period':dsWHFRPout.FR20a9) +',';
        json += BuildJsonNode('fullPaid':dsWHFRPout.FRpoi) +',';
        json += '"averageWeekly":{';
         json += BuildJsonNode('wage':%editc(dsWHFRPout.FRaWW:'3')) +',';
         json += BuildJsonNode('estimateActual':'') +',';
         json = %scanrpl(',':'':json:%len(json)-1);
         json += '},';
        json += BuildJsonNode('fullSalaryContinue':dsWHFRPout.FRfSal) +',';
        json += BuildJsonNode('daysOverAWeek':%char(dsWHFRPout.FRdays)) +',';
        json += BuildJsonNode('beginTime':%char(dsWHFRPout.FRtim2)) +',';

        json = %scanrpl(',':'':json:%len(json)-1);
        json += '},';
       Endsr;

       //=================================================================
       // Build the sql statement to be processed
       //=================================================================
       Begsr SqlBuild;
        dsWHFRPin.FRco   = decCo#;
        dsWHFRPin.FRss   = dsIn.ss#;
        dsWHFRPin.FRacDt = DateConvertFROI(dsIn.accDt:'YYMD');
        dsWHFRPin.FRiTm = tm6Initial;
        dsWHFRPin.FRiDt = %editc(dt8Initial:'Z');
        Exec sql
         select * into :dsWHFRPout
          from WHFRP
          where FRco = :dsWHFRPin.FRco and FRss = :dsWHFRPin.FRss and
           FRacDt = :dsWHFRPin.FRacDt and FRiDt = :dsWHFRPin.FRiDt
           and FRiTm = :dsWHFRPin.FRiTm;
        If not IsValidFetch(sqlstt);
           Exec sql
            select * into :dsWHFRPout
             from WHFRP_HLD
             where FRco = :dsWHFRPin.FRco and FRss = :dsWHFRPin.FRss and
              FRacDt = :dsWHFRPin.FRacDt and FRiDt = :dsWHFRPin.FRiDt
              and FRiTm = :dsWHFRPin.FRiTm;
        Endif;
        If not IsValidFetch(sqlstt);
           AddErrorMessageJson('FROI not found for requested ' +
            'social security number and accident date.':
             ioRtnCode);
           isError = *on;
        Endif;
        If dsWHFRPout.FRfnd <> *zero and
         not UseGroupInPortal(dsWHFRPout.FRco:dsWHFRPout.FRfnd:'FROI');
           AddErrorMessageJson('Group ' + %char(dsWHFRPout.FRfnd) +
            ' is not included for service requested ' +
            '(' + %trim(inRequest) + ').':ioRtnCode);
           isError = *on;
        Endif;
       Endsr;

       //=================================================================
       // Process the sql statement
       //=================================================================
       Begsr SqlProcess;
        Exsr SqlBuild;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Select;
        When dsWHFRPout.FRstat = 'H';
           strStatus = 'Pending Submission';
        When dsWHFRPout.FRstat = 'X';
           strStatus = 'Deleted from ' +
            GetModuleProperty('Portal Module':'Name');
        When dsWHFRPout.FRstat = 'D';
           strStatus = 'Deleted';
        When dsWHFRPout.FRstat = 'T';
           strStatus = 'Claim Created';
        Other;
           strStatus = 'Submitted';
        Endsl;
        Exsr BuildJson;
       Endsr;

       //=================================================================
       // Validate parms
       //=================================================================
       Begsr Validate;
        If isError;
           Leavesr;
        Endif;
        If dsIn.ss# = *blanks or dsIn.accDt = *blanks
         or dsIn.initTime = *blanks;
           AddErrorMessageJson('Key information ' +
            '(ss#/accident date/initial time) is not available ' +
            'to process request.'
            :ioRtnCode);
        Endif;
       Endsr;

      /End-free
     PGetFROI          e

      /Free
       //============================================
       // Return FROI's status
       //============================================
       Dcl-Proc GetFROIStatus;
       Dcl-Pi *N VarChar(25);
        code Char(1);
       End-Pi;
        Select;
        When code = 'E';
           Return 'Error Submission';
        When code = 'T';
           Return 'Claim Created';
        When code = 'D';
           Return 'Deleted';
        When code = 'P';
           Return 'Pending Submission';
        When code = 'H';
           Return 'Unsubmitted';
        When code = 'X';
           Return 'Deleted';
        When code = ' ';
           Return 'Submitted';
        Other;
           Return code;
        EndSl;
       End-Proc;

       //============================================
       // Return list for FROI information
       //  from WHFRP_HLD based upon request parms
       //============================================
       Dcl-Proc ListFROI;
       Dcl-c maxDiv 999;
       Dcl-Ds dsIn Qualified Inz;
        company Char(3);
        agency Char(12); // fein||mod
        group Char(3);
        empFEIN char(9);
        employer Char(9);
        div char(5) dim(maxDiv);
        divInt zoned(5:0) dim(maxDiv);
        reportID Char(10);
        fromPrepUsaDate Char(8);
        toPrepUsaDate Char(8);
        nameFirst varchar(50);
        nameLast  varchar(50);
        dob char(8);
        accidentDt char(8);
        status Varchar(25);
       End-Ds;

      /End-free
     ddsWHFRP        e ds                  extname(WHFRP) qualified inz
      /free

       Dcl-S aryAgtFnd Zoned(3:0) Dim(99) Inz;
       Dcl-S chrComma Char(1) Inz;
       Dcl-S decCo# Zoned(3:0) Inz;
       Dcl-S i Zoned(3:0) Inz(1);
       Dcl-S intAgtFein Zoned(9:0) Inz;
       Dcl-S intAgtFnd Zoned(3:0) Inz;
       Dcl-S intAgtMod Zoned(3:0) Inz;
       Dcl-S intEmpFein Zoned(9:0) Inz;
       Dcl-S isoAccidentDateA Char(8) Inz;
       Dcl-S isoDobA Char(8) Inz;
       Dcl-S isoFromPrepDateA Char(8) Inz;
       Dcl-S isoToPrepDateA Char(8) Inz;
       Dcl-S ix Zoned(3:0) Inz;
       Dcl-S rrnWMEMP Packed(9:0) Inz(0);
       Dcl-S strAryFnd VarChar(1200) Inz;
       Dcl-S strSqlList1 VarChar(250) Inz;
       Dcl-S strSqlList2 VarChar(250) Inz;
       Dcl-S strSqlListFROI VarChar(2500) Inz;
       Dcl-S strSqlWhere VarChar(1000) Inz;
       Dcl-S empAgtFein Zoned(9:0) Inz;
       Dcl-S empAgtMod Zoned(3:0) Inz;
       Dcl-S accDate7 Zoned(7:0) Inz;
       Dcl-S effDate Zoned(7:0) Inz;
       Dcl-S aa_rrn Packed(9:0) Inz;
       Dcl-S ag_rrn Packed(9:0) Inz;
       Dcl-S ah_rrn Packed(9:0) Inz;
       Dcl-S qteFlg Char(1) Inz;
       Dcl-S strEmpAgtName Char(40) Inz;

       // Exsr BuildStub;
       Exsr GetParms;
       Exsr ValidateParms;
       Exsr BuildReturnJSON;
       json += ProcessErrorMessagesJson();
       json = %scanrpl(',':'':json:1:1);
       //--------------------------------------------
       Begsr BuildStub;
       //--------------------------------------------
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {ListFROI}'));
       Endsr;
       //--------------------------------------------
       Begsr GetParms;
       //--------------------------------------------
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;
        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.company = YAJL_get_string(val);
           When key = 'agency';
              dsIn.agency = YAJL_get_string(val);
           When key = 'group';
              dsIn.group = YAJL_get_string(val);
           When key = 'empFEIN';
              dsIn.empFEIN = YAJL_get_string(val);
           When key = 'employer';
              dsIn.employer = YAJL_get_string(val);
           When key = 'division';
              json = %scanrpl('""':'[':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              i1 = 0;
              Dow YAJL_array_loop(node1:i1:val);
                 dsIn.div(i1) = YAJL_get_string(val);
                 json += '"' + %trim(dsIn.div(i1)) + '",';
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ']';
           When key = 'reportID';
              dsIn.reportID = YAJL_get_string(val);
           When key = 'claimant';
              Exsr GetParmsClaimant;
           When key = 'accident';
              Exsr GetParmsAccident;
           When key = 'prepareDate';
              Exsr GetParmsPrepareDate;
           When key = 'status';
              dsIn.status = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '},';
       Endsr;
       //--------------------------------------------
       Begsr GetParmsAccident;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'date';
              dsIn.accidentDt = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //--------------------------------------------
       Begsr GetParmsClaimant;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'dob';
              dsIn.dob = YAJL_get_string(val);
           When key = 'name';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'first';
                    dsIn.nameFirst = YAJL_get_string(val);
                 When key = 'last';
                    dsIn.nameLast = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += '}';
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //--------------------------------------------
       Begsr GetParmsPrepareDate;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'from';
              dsIn.fromPrepUsaDate = YAJL_get_string(val);
           When key = 'to';
              dsIn.toPrepUsaDate = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //--------------------------------------------
       Begsr BuildReturnJSON;
       //--------------------------------------------
        If ioRtnCode = 'Error';
           LeaveSr;
        Endif;
        Exsr BuildSql;
        json += '"FROIList":[';
        Dou Not IsValidFetch(sqlStt);
           Exec SQL Fetch Next From cursorListFROI Into :dsWHFRP;

           Select;
           When Not IsValidFetch(sqlStt);
              Iter;
           When dsWHFRP.FRfnd <> *zero and
            not UseGroupInPortal(intCo#:dsWHFRP.FRfnd:'FROI');
              Iter;
           Other;
              If intAgtFein <> 0;
                 // Retrieve Agent Master RRN
                 ag_rrn = 0;
                 GetEmpAgt(intCo#:dsWHFRP.FRfnd:dsWHFRP.FRemp:
                  dsWHFRP.FRdiv:dsWHFRP.FRfyr:effDate:strEmpAgtName:
                  ah_rrn:aa_rrn:ag_rrn:qteFlg);
                 // Get the Agency Fein/Mod
                 If ag_rrn <> 0;
                    Exec SQl Select AGfein, AGMod
                              Into :empAgtFein, :empAgtMod
                              From WMAGP
                              Where rrn(WMAGP) = :ag_rrn;
                    If intAgtFein <> empAgtFein;
                       iter;
                    EndIf;
                    If intAgtMod <> empAgtMod;
                       iter;
                    EndIf;
                 EndIf;
              EndIf;
           Endsl;

           If dsWHFRP.FRacDt <> ' ';
              If %Dec(dsWHFRP.FRacDt:8:0) < 10000000;
                 accDate7 = %Dec(dsWHFRP.FRacDt:7:0);
              Else;
                 accDate7 = %Dec(dsWHFRP.FRacDt:8:0) - 19000000;
              EndIf;
           Else;
              accDate7 = 0;
           EndIf;

           json += %Trim(chrComma) +
            '{"reportID":"' + %Trim(dsWHFRP.FRrpt) + '"';
           json += ', "keyInfo":{' +
            BuildJsonNode('company':%char(dsWHFRP.FRco)) + ',' +
            BuildJsonNode('claimantSSN':dsWHFRP.FRss) + ',' +
            BuildJsonNode('accidentDate'
             :DateConvertFROI(dsWHFRP.FRacDt:'MDYY')) + ',' +
            BuildJsonNode('initialDate'
             :DateConvertFROI(dsWHFRP.FRiDt:'MDYY')) + ',' +
            BuildJsonNode('initialTime':%char(dsWHFRP.FRiTm)) + '}';
           json += ',' + BuildJsonNode('company':%editc(dsWHFRP.FRco:'X'));
           json += ',' +
            PolicyJSONData(
             dsWHFRP.FRco:dsWHFRP.FRfnd:dsWHFRP.FRemp:dsWHFRP.FRdiv:
             RtvPolYr(intCo#:dsWHFRP.FRfnd:dsWHFRP.FRemp:dsWHFRP.FRdiv:accDate7)
            );
           json += ',"division":"' + %EditC(dsWHFRP.FRdiv:'X') + '"';
           json += ',"injuryDate":"' +
            DateConvertFROI(dsWHFRP.FRacDt:'MDYY') + '"';
           json += ',' + BuildJsonNode('initialDate':dsWHFRP.FRiDt:'date');
           json += ',"initialTime":"' + %EditC(dsWHFRP.FRiTm:'X') + '"';
           json += ',' + BuildJsonNode('preparedDate':
            DateConvertFROI(dsWHFRP.FRprep:'MDYY'));
           json += ',"status":"' + GetFROIStatus(dsWHFRP.FRstat) + '"';
           json += ',"claimant":{';
           json += BuildJsonNode('ssn':dsWHFRP.FRss) + ',';
           json += BuildJsonName(dsWHFRP.FRfsn:dsWHFRP.FRlsn:
            dsWHFRP.FRmid:'name') + ',';
           json += BuildJsonNode('dob':DateConvertFROI(dsWHFRP.FRdtOB:'MDYY')) +
            ',';
           json += BuildJsonNode('dod':DateConvertFROI(dsWHFRP.FRdOD:'MDYY')) +
            ',';
           json += BuildJsonAddress(dsWHFRP.FRadr1:dsWHFRP.FRadr2:
            dsWHFRP.FRcty:dsWHFRP.FRst:%char(dsWHFRP.FRzip):'address') + ',';
           json = %scanrpl(',':'':json:%len(json)-1);
           json += '}';
           json += '}';
           chrComma = ',';
        Enddo;
        json += ']';
        Exec Sql Close cursorListFROI;
       Endsr;
       //--------------------------------------------
       Begsr BuildSql;
       //--------------------------------------------
       strSqlWhere += ' Where FRco = ' + %EditC(intCo#:'X');

       // Group#
       If dsIn.group <> *Blanks;
         strSqlWhere += ' And FRfnd = ' + dsIn.group;
       EndIf;

       // Employer FEIN
       If dsIn.empFEIN <> *Blanks;
          strSqlWhere += ' And FRfein = ' + dsIn.empFEIN;
       EndIf;

       // Employer#
       If dsIn.employer <> *Blanks;
         strSqlWhere += ' And FRfnd = ' + dsIn.group +
          ' And FRemp = ' + dsIn.employer;
       EndIf;

       // Division
       If dsIn.div(1) <> *blanks;
          strSqlWhere += ' and FRdiv in (';
          For i1 = 1 to maxDiv;
             Select;
             When dsIn.div(i1) = *blank;
                Iter;
             Endsl;
             strSqlWhere += %trim(dsIn.div(i1)) + ',';
          Endfor;
          strSqlWhere = %scanrpl(',':'':strSqlWhere:%len(strSqlWhere)-1);
          strSqlWhere += ')';
       EndIf;

       // Report ID
       If dsIn.reportID <> *Blanks;
          strSqlWhere += ' And FRrpt = ' + dsIn.reportID;
       Endif;

       // Preparation From Date
       If isoFromPrepDateA <> *Blanks;
          strSqlWhere += ' And FRprep >= ''' + isoFromPrepDateA + '''';
       Endif;

       // Preparation To Date
       If isoToPrepDateA <> *Blanks;
          strSqlWhere += ' And FRprep <= ''' + isoToPrepDateA + '''';
       Endif;

       // Names
       If dsIn.nameLast <> *blanks;
          strSqlWhere += ' and UPPER(FRlsn) like ''%' +
           %trim(Upper(dsIn.nameLast)) + '%''';
       Endif;
       If dsIn.nameFirst <> *blanks;
          strSqlWhere += ' and UPPER(FRfsn) like ''%' +
           %trim(Upper(dsIn.nameFirst)) + '%''';
       Endif;

       // DOB
       If dsIn.dob <> *blanks;
          strSqlWhere += ' and FRdtOB = ''' + isoDobA + '''';
       Endif;

       // Accident Date
       If dsIn.accidentDt <> *blanks;
          strSqlWhere += ' and FRacDt = ''' + isoAccidentDateA + '''';
       Endif;

       Select;
       When Upper(dsIn.status) = '*ALL';
          strSqlList1 = ' Select * ' +
           ' From WHFRP_HLD ';
          strSqlList2 = ' Select * ' +
           ' From WHFRP ';
       When dsIn.status = *Blanks;
          strSqlList1 = ' Select * ' +
           ' From WHFRP_HLD ';
          strSqlList2 = ' Select * ' +
           ' From WHFRP ';
          strSqlWhere += ' And FRstat not in (''X'', ''D'')';
       When dsIn.status = 'Error Submission';
          strSqlWhere += ' And FRstat = ''E''';
          strSqlList1 = ' Select * ' +
           ' From WHFRP_HLD ';
       When dsIn.status = 'Pending Submission';
          strSqlWhere += ' And FRstat = ''P''';
          strSqlList1 = ' Select * ' +
           ' From WHFRP ';
       When dsIn.status = 'Submitted';
          strSqlWhere += ' And FRstat = '' ''';
          strSqlList1 = ' Select * ' +
           ' From WHFRP ';
       When dsIn.status = 'Unsubmitted';
          strSqlWhere += ' And FRstat = ''H''';
          strSqlList1 = ' Select * ' +
           ' From WHFRP_HLD ';
       EndSl;

       strSqlListFROI = strSqlList1 + strSqlWhere;
       If strSqlList2 <> *blank;
          strSqlListFROI += ' union ' + strSqlList2 + strSqlWhere;
       Endif;

       Exec Sql close cursorListFROI;
       Exec Sql Prepare sqlListFROI From :strSqlListFROI;
       Exec Sql Declare cursorListFROI Cursor For sqlListFROI;
       Exec Sql Open cursorListFROI;

       Endsr;
       //--------------------------------------------
       Begsr ValidateParms;
       //--------------------------------------------

        // If an employer # is passed the group must be pased too
        If dsIn.employer <> *Blanks And dsIn.group = *Blanks;
           AddErrorMessageJson('Group and employer values are inclusive.':
            ioRtnCode);
        Endif;

        // Division(s)
        If dsIn.div(1) <> *blank;
           For i1 = 1 to maxDiv;
              Select;
              When dsIn.div(i1) = *blanks;
                 Iter;
              Endsl;
              Monitor;
                 dsIn.divInt(i1) = %int(dsIn.div(i1));
              On-Error;
                 AddErrorMessageJson('Division: Invalid numeric data.'
                  :ioRtnCode);
                 Leave;
              EndMon;
           Endfor;
        EndIf;

        // If Employer Fein passed then it must be a valid numeric
        If dsIn.empFEIN <> *blanks;
           Monitor;
              intEmpFein = %Dec(dsIn.empFEIN:9:0);
           On-Error;
              AddErrorMessageJson('Employer FEIN must be numeric.':
               ioRtnCode);
           EndMon;
        Endif;

        // If Agency Fein passed then it must be a valid numeric
        If %Subst(dsIn.agency:1:9) <> *Blanks;
           Monitor;
              intAgtFein = %Dec(%Subst(dsIn.agency:1:9):9:0);
           On-Error;
              AddErrorMessageJson('Agency Fein must be numeric.':
               ioRtnCode);
           EndMon;
        Endif;

        // If Agency Mod passed then it must be a valid numeric
        If %Subst(dsIn.agency:10:3) <> *Blanks;
           Monitor;
              intAgtMod = %Dec(%Subst(dsIn.agency:10:3):3:0);
           On-Error;
              AddErrorMessageJson('Agency Modifier must be numeric.':
               ioRtnCode);
           EndMon;
        Endif;

        // If DOB is passed it must be valid
        If dsIn.dob <> *Blanks;
           isoDobA = DateConvertFROI(dsIn.dob:'YYMD');
           If isoDobA = *Zero;
              AddErrorMessageJson('DOB is invalid':ioRtnCode);
           Endif;
        Endif;

        // If Accident Date is passed it must be valid
        If dsIn.accidentDt <> *Blanks;
           isoAccidentDateA = DateConvertFROI(dsIn.accidentDt:'YYMD');
           If isoAccidentDateA = *Zero;
              AddErrorMessageJson('Accident date is invalid':ioRtnCode);
           Endif;
        Endif;

        // If Prepare From Date is passed it must be valid
        If dsIn.fromPrepUsaDate <> *Blanks;
           isoFromPrepDateA = DateConvertFROI(dsIn.fromPrepUsaDate:'YYMD');
           If isoFromPrepDateA = *Zero;
              AddErrorMessageJson('From prepare date is invalid':ioRtnCode);
           Endif;
        Endif;

        // If Prepare To Date is passed it must be valid
        If dsIn.toPrepUsaDate <> *Blanks;
           isoToPrepDateA = DateConvertFROI(dsIn.toPrepUsaDate:'YYMD');
           If isoToPrepDateA = *Zero;
              AddErrorMessageJson('To prepare date is invalid':ioRtnCode);
           Endif;
        Endif;
       Endsr;
       End-Proc ListFROI;
      /End-Free
      //=============================================================
      // List FROI History
      //=============================================================
     PListFROIHistory  b
     DListFROIHistory  pr

     ddecAgt#          s              5  0 inz
     ddecCo#           s              3  0 inz
     ddtAccident       s               d   inz
     ddt7Accident      s              7  0 inz
     ddt8Accident      s              8  0 inz
     ddtInitial        s               d   inz
     ddt7Initial       s              7  0 inz
     ddt8Initial       s              8  0 inz
     disError          s              1n   inz
     dchrComma         s              1    inz
     drrn1             s              9  0 inz
     drrn2             s              9  0 inz
     dstrName40        s             40    inz
     dstrStatus        s             50    inz varying
     dtmInitial        s               t   inz
     dtm6Initial       s              6  0 inz
     dstrSqlHistFROI   s           1000    inz varying

     ddsWXFLP        e ds                  extname(WXFLP) inz qualified

     DdsIn             ds                  likeds(dsKeyInfo) inz
      /Free

       Exsr GetParms;
       Exsr BuildOutput;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;

        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'keyInfo';
              Reset i1;
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'company';
                    dsIn.co# = YAJL_get_string(val);
                 When key = 'claimantSSN';
                    dsIn.ss# = YAJL_get_string(val);
                 When key = 'accidentDate';
                    dsIn.accDt = YAJL_get_string(val);
                 When key = 'initialDate';
                    dsIn.initDt = YAJL_get_string(val);
                 When key = 'initialTime';
                    dsIn.initTime = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;
        json = json + '},';
        Monitor;
           decCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        If decCo# = *zero;
           decCo# = intCo#;
        Endif;
        Monitor;
           dt8Accident = %int(dsIn.accDt);
           dt7Accident = DtCYMD(dt8Accident);
           dtAccident = CvtToDate(dt8Accident);
        On-error;
        Endmon;
        Monitor;
           dt8Initial = %int(DateConvertFROI(dsIn.initDt:'YYMD'));
           dt7Initial = DtCYMD(dt8Initial);
           dtInitial = CvtToDate(dt8Initial);
        On-error;
        Endmon;
        Monitor;
           tm6Initial = %int(dsIn.initTime);
        On-error;
        Endmon;
        Exsr Validate;
       Endsr;

       //=================================================================
       // Validate parms
       //=================================================================
       Begsr Validate;
        If isError;
           Leavesr;
        Endif;
        If dsIn.co# = *blanks or dsIn.ss# = *blanks or dsIn.accDt = *blanks
         or dsIn.initTime = *blanks;
           AddErrorMessageJson('Key information ' +
            'is not available to process request.'
            :ioRtnCode);
        Endif;
       Endsr;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        Exsr SqlProcess;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ProcessErrorMessagesJson('*PRE');
       Endsr;

       //=================================================================
       // Process the sql statement
       //=================================================================
       Begsr SqlProcess;
        Exsr SqlBuild;
        If isError;
           Leavesr;
        Endif;
        Exsr BuildJson;
       Endsr;

       //=================================================================
       // Build the sql statement to be processed
       //=================================================================
       Begsr SqlBuild;
        dsWHFRPin.FRco   = decCo#;
        dsWHFRPin.FRss   = dsIn.ss#;
        dsWHFRPin.FRacDt = DateConvertFROI(dsIn.accDt:'YYMD');
        dsWHFRPin.FRiDt = %char(dt8Initial);
        dsWHFRPin.FRiTm = tm6Initial;
        Exec sql
         select * into :dsWHFRPout
          from WHFRP
          where FRco = :dsWHFRPin.FRco and FRss = :dsWHFRPin.FRss and
           FRacDt = :dsWHFRPin.FRacDt and FRiTm = :dsWHFRPin.FRiTm
           and FRiDt = :dsWHFRPin.FRiDt;
        If not IsValidFetch(sqlstt);
           Exec sql
            select * into :dsWHFRPout
             from WHFRP_HLD
             where FRco = :dsWHFRPin.FRco and FRss = :dsWHFRPin.FRss and
              FRacDt = :dsWHFRPin.FRacDt and FRiTm = :dsWHFRPin.FRiTm
              and FRiDt = :dsWHFRPin.FRiDt;
        Endif;
        If not IsValidFetch(sqlstt);
           AddErrorMessageJson('FROI not found for requested ' +
            'social security number and accident date.':
             ioRtnCode);
           isError = *on;
        Endif;
       Endsr;

       //=================================================================
       // Build the Json data
       //=================================================================
       Begsr BuildJson;

        strSqlHistFROI = 'Select * From WXFLP ' +
         ' Where FLCo# = ' + %EditC(dsWHFRPout.FRco:'X') +
         ' and FLss = ' + dsWHFRPout.FRss + ' and FLAcdt = ' +
         dsWHFRPout.FRacdt + ' and FLDtrp = ' + dsWHFRPout.FRdtrp;
        Exec Sql Prepare sqlHistFROI From :strSqlHistFROI;
        Exec Sql Declare cursorHistFROI Cursor For sqlHistFROI;
        Exec Sql Open cursorHistFROI;

        json += '"FROIHistory":[';
        Dou Not IsValidFetch(sqlStt);
           Exec SQL Fetch Next From cursorHistFROI Into :dsWXFLP;
           Select;
           When Not IsValidFetch(sqlStt);
              Iter;
           Endsl;
           json += %Trim(chrComma) +
            '{"seq#":"' + %editc(dsWXFLP.FLSEQ#:'X') + '"';
           json += ',"logDate":"' +
                            %editw(DtMDYY(dsWXFLP.FLLogDt):'  /  /    ') + '"';
           json += ',"userID":"' + %trim(dsWXFLP.FLUsrID) + '"';
           json += ',"description":"' + %trim(dsWXFLP.FLDesc) + '"';
           json += ',"comments":"' + %trim(dsWXFLP.FLComm) + '"}';
           chrComma = ',';
        Enddo;
        json += ']';
        Exec Sql Close cursorHistFROI;

       Endsr;

      /End-free
     PListFROIHistory  e

      //=============================================================
      // Submit FROI
      //=============================================================
     PLogFROI          b
     DLogFROI          pi
     D inCo#                          3  0 const
     D inFnd                          3  0 const
     D inEmp#                         9  0 const
     D inDiv                          5  0 const
     D inSS#                          9    const
     D inDateAcc                      8    const
     D inDateRpt                      8    const
     D inUser                        25    const varying
     D inDesc                       100    const varying
     D inComm                       500    const varying options(*nopass)

     d seq#            s              5  0
     d strComm         s            500    varying inz
     d strDesc         s            100    varying inz

     ddsWXFLP        e ds                  extname(WXFLP) inz qualified

      /Free
       If %parms >= 10  and inComm <> *blanks;
          strComm = inComm;
       Endif;
       strDesc = inDesc;

       Select;
       When %subst(strDesc:1:4) <> 'FROI';
          If strComm = *blanks;
             strComm = 'FROI has been ' + %trim(Lower(strDesc)) +
              ' by user ' + %trim(inUser) + '.';
          Endif;
          strDesc = 'FROI ' + %trim(TitleCase(strDesc));
       Endsl;

       seq# = 0;
       // Find FROI history log's highest seq#
       Exec SQL
        Select max(FLSEQ#) Into :seq#
         From WXFLP
         Where FLco# = :inCo# And FLSS = :inSS# And
          FLAcdt = :inDateAcc and FLDtrp = :inDateRpt;

       Reset dsWXFLP;

       dsWXFLP.FLCo#  = inCo#;
       dsWXFLP.FLFnd  = inFnd;
       dsWXFLP.FLEmp# = inEmp#;
       dsWXFLP.FLDiv  = inDiv;
       dsWXFLP.FLSS   = inSS#;
       dsWXFLP.FLAcdt = inDateAcc;
       dsWXFLP.FLDtrp = inDateRpt;

       dsWXFLP.FLSEQ# = seq# + 1;
       dsWXFLP.FLLogDt = sysDate7;
       dsWXFLP.FLUsrID = inUser;
       dsWXFLP.FLDesc  = strDesc;
       dsWXFLP.FLComm  = strComm;

       dsWXFLP.FLIDt  = sysDate7;
       dsWXFLP.FLDt   = sysDate7;
       dsWXFLP.FLITim = sysTime6;
       dsWXFLP.FLTime = sysTime6;
       dsWXFLP.FLPgm  = q1pgm;
       dsWXFLP.FLIPgm = q1pgm;
       dsWXFLP.FLUser = q1User;
       dsWXFLP.FLIUsr = q1User;

       Exec sql
        insert into WXFLP
          values(:dsWXFLP);

       If not IsValidFetch(sqlstt);
          AddErrorMessageJson('Error occured during FROI log.');
          ioRtnCode = 'Info Only';
          Return;
       Endif;

      /End-free
     PLogFROI          e

      //=============================================================
      // Print FROI
      //=============================================================
     PPrintFROI        b
     DPrintFROI        pr

     ddecCo#           s              3  0 inz
     ddtAccident       s               d   inz
     ddt7Accident      s              7  0 inz
     ddt8Accident      s              8  0 inz
     ddtInitial        s               d   inz
     ddt7Initial       s              7  0 inz
     ddt8Initial       s              8  0 inz
     drrn              s              9  0 inz
     dstrXmlFileName   s             60    inz
     dstrStForm        s              2    inz

     ddsSMCOP        e ds                  extname(SMCOP) inz qualified
     ddsSXFRP        e ds                  extname(SXFRP) inz qualified
     ddsWDELP        e ds                  extname(WDELP) inz qualified
     ddsWMALP        e ds                  extname(WMALP) inz qualified
     ddsWMEMP        e ds                  extname(WMEMP) inz qualified
     ddsWMFNP        e ds                  extname(WMFNP) inz qualified
     ddsWMRPP        e ds                  extname(WMRPP) inz qualified

     DdsIn             ds                  likeds(dsKeyInfo) inz
      /Free
       Exsr GetParms;
       Exsr BuildOutput;

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        If EditFROI(decCo#:dsIn.ss#:dsIn.accDt:dsIn.initDt
         :%int(dsIn.initTime))=*off;
           Exsr Print;
        Endif;
        json = %scanrpl(',':'':json:%len(json)-1);
        json += ProcessErrorMessagesJson('*PRE');
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {PrintFROI}'));
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;

        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'keyInfo';
              Reset i1;
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'company';
                    dsIn.co# = YAJL_get_string(val);
                 When key = 'claimantSSN';
                    dsIn.ss# = YAJL_get_string(val);
                 When key = 'accidentDate';
                    dsIn.accDt = YAJL_get_string(val);
                 When key = 'initialDate';
                    dsIn.initDt = YAJL_get_string(val);
                 When key = 'initialTime';
                    dsIn.initTime = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;
        json = json + '}';
        Monitor;
           decCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        Monitor;
           dt8Accident = %int(dsIn.accDt);
           dt7Accident = DtCYMD(dt8Accident);
           dtAccident = CvtToDate(dt8Accident);
           dsIn.accDt = DateConvertFROI(dsIn.accDt:'YYMD');
        On-error;
        Endmon;
        Monitor;
           dt8Initial = %int(dsIn.initDt);
           dt7Initial = DtCYMD(dt8Initial);
           dtInitial = CvtToDate(dt8Initial);
           dsIn.initDt = DateConvertFROI(dsIn.initDt:'YYMD');
        On-error;
        Endmon;
       Endsr;

       //=================================================================
       // Load XML fields
       //=================================================================
       Begsr LoadXml;
        Exec sql
         select * into :dsSMCOP
          from SMCOP
          where COco# = :dsWHFRPout.FRco;
        Exec sql
         select * into :dsWMFNP
          from WMFNP
          where FNco# = :dsWHFRPout.FRco and FNfnd = :dsWHFRPout.FRfnd;
        Exec sql
         select * into :dsWMEMP
          from WMEMP
          where EMco# = :dsWHFRPout.FRco and EMfnd = :dsWHFRPout.FRfnd
           and EMemp# = :dsWHFRPout.FRemp and EMdiv = :dsWHFRPout.FRdiv;
        Exec sql
         select * into :dsWMALP
          from WMALP
          where ALco# = :dsWHFRPout.FRco and ALfnd = :dsWHFRPout.FRfnd
           and ALemp# = :dsWHFRPout.FRemp and ALdiv = :dsWHFRPout.FRdiv
           and ALelId = :dsWHFRPout.FRctg1;
        Exec sql
         select * into :dsWDELP
          from WDELP
          where ELco# = :dsWHFRPout.FRco and ELfnd = :dsWHFRPout.FRfnd
           and ELemp# = :dsWHFRPout.FRemp and ELdiv = :dsWHFRPout.FRdiv
           and ELpfDt <= :dt7Accident and ELptDt > :dt7Accident;
        Exec sql
         select * into :dsWMRPP
          from WMRPP
          where RPco# = :dsWHFRPout.FRco and RPfnd = :dsWHFRPout.FRfnd
           and RPemp# = :dsWHFRPout.FRemp and RPdiv = :dsWHFRPout.FRdiv
           and RPfyr = :dsWDELP.ELfyr;

        strXmlFileName = GetXMLFileName(' ':*zeros:dsWHFRPout.FRco:
         dsWHFRPout.FRfnd:*blank:*blank:'First Report of Injury');
        dsSXFRP.FRxml = GetXMLTempName(strXmlFileName);

        //* Check Froi template setup file to pull proper template.
        Chain (strXmlFileName) xp100;

        //* Froi template check hard coded to 'A1' national form only for now.
        //* Coding to properly determine the state/form value to use can be seen
        //* in WTCLMTWRP where WA076R is used to determine "FROI/hardcopy value".
        //*  Note - there is no A1 setup so it will fail first section and then hit
        //* the blank/default template for everyone.

        strStForm = 'A1';

        Setgt (000:strStForm:sysDate7) pff00;
        Readpe (000:strStForm) pff00;
        If not %eof;
           //* Amend type of process to include state selected/found.
           XPType = %trim(XPType) + ' ' + %trim(strStForm);
           If FFDocC <> ' ';
              XPDcid = %trim(FFDocC);
              XPPath = %trim(FFPathC);
           Else;
              If FFDoc <> ' ';
                 XPDcid = %trim(FFDoc);
                 XPPath = %trim(FFPath);
              Endif;
           Endif;
        Else;
           Setgt (000:'  ':sysDate7) pff00;
           Readpe (000:'  ') pff00;
           If not %eof;
              If FFDocC <> ' ';
                 XPDcid = %trim(FFDocC);
                 XPPath = %trim(FFPathC);
              Else;
                 If FFDoc <> ' ';
                    XPDcid = %trim(FFDoc);
                    XPPath = %trim(FFPath);
                 Endif;
              Endif;
           Endif;
        Endif;

        Update xp100;

        // Company Info
        dsSXFRP.FRco#    = dsWHFRPout.FRco;
        dsSXFRP.FRcoFe   = dsSMCOP.COfein;
        dsSXFRP.FRcoNm   = dsSMCOP.COname;
        dsSXFRP.FRcoAd1  = dsSMCOP.COadr1;
        dsSXFRP.FRcoAd2  = dsSMCOP.COadr2;
        dsSXFRP.FRcoCty  = dsSMCOP.COcty;
        dsSXFRP.FRcoSt   = dsSMCOP.COst;
        dsSXFRP.FRcoZip  = dsSMCOP.COzip;
        // Group Info
        dsSXFRP.FRfnd    = dsWHFRPout.FRfnd;
        dsSXFRP.FRfName  = dsWMFNP.FNname;
        If dsWMFNP.FNsFin = 'Y';
           dsSXFRP.FRsFin   = 'X';
        EndIf;
        // Employer Info
        dsSXFRP.FReNam   = dsWMEMP.EMname;
        dsSXFRP.FReMad1  = dsWMEMP.EMmAd1;
        dsSXFRP.FReMad2  = dsWMEMP.EMmAd2;
        dsSXFRP.FReMcty  = dsWMEMP.EMmCty;
        dsSXFRP.FReMst   = dsWMEMP.EMmSt;
        dsSXFRP.FReMzip  = %EditC(dsWMEMP.EMmZip:'X');
        dsSXFRP.FRePad1  = dsWMEMP.EMpAd1;
        dsSXFRP.FRePad2  = dsWMEMP.EMpAd2;
        dsSXFRP.FRePcty  = dsWMEMP.EMpCty;
        dsSXFRP.FRePst   = dsWMEMP.EMpSt;
        dsSXFRP.FRePzip  = %EditC(dsWMEMP.EMpZip:'X');
        dsSXFRP.FReFei   = dsWMEMP.EMfein;
        dsSXFRP.FReArea  = dsWMEMP.EMarea;
        dsSXFRP.FRePhn#  = dsWMEMP.EMphn#;
        dsSXFRP.FRsic    = dsWMEMP.EMsic;
        dsSXFRP.FRsicDs  = GetGenCodeDs(decCo#:'SIC':dsWMEMP.EMsic);
        dsSXFRP.FRcont   = dsWMEMP.EMcont;
        dsSXFRP.FRfYr    = dsWDELP.ELfyr;
        If dsWMRPP.RPcap# <> ' ';
           dsSXFRP.FRPol# = dsWMRPP.RPcap#;
        Else;
           dsSXFRP.FRPol# = %EditC(dsWHFRPout.FRfnd:'X') +
            %EditC(dsWHFRPout.FRemp:'X') + %EditC(dsWDELP.ELfYr:'X');
        EndIf;
        Monitor;
           dsSXFRP.FRpfDt = %dec(%date(dsWDELP.ELpfDt:*cymd):*usa);
        On-Error;
        EndMon;
        Monitor;
           dsSXFRP.FRptDt = %dec(%date(dsWDELP.ELptDt:*cymd):*usa);
        On-Error;
        EndMon;
        // Location Info
        dsSXFRP.FRctg1   = dsWMALP.ALelId;
        dsSXFRP.FRlPad1  = dsWMALP.ALpAd1;
        dsSXFRP.FRlPad2  = dsWMALP.ALpAd2;
        dsSXFRP.FRlPcty  = dsWMALP.ALpCty;
        dsSXFRP.FRlPst   = dsWMALP.ALpSt;
        dsSXFRP.FRlPzip  = dsWMALP.ALpZip;
        dsSXFRP.FRlMad1  = dsWMALP.ALmAd1;
        dsSXFRP.FRlMad2  = dsWMALP.ALmAd2;
        dsSXFRP.FRlMcty  = dsWMALP.ALmCty;
        dsSXFRP.FRlMst   = dsWMALP.ALmSt;
        dsSXFRP.FRlMzip  = dsWMALP.ALmZip;
        // Agent Info
        dsSXFRP.FRagNm   = GetAgtName(
         GetEmpAgt(dsWHFRPout.FRco:dsWHFRPout.FRfnd:dsWHFRPout.FRemp:
          dsWHFRPout.FRdiv:dsWDELP.ELfyr:dt7Accident):rrn:rrn:
         dsWHFRPout.FRfnd:dsWHFRPout.FRco);
        // Claim Info
        dsSXFRP.FRcFsn   = dsWHFRPout.FRfsn;
        dsSXFRP.FRcLsn   = dsWHFRPout.FRlsn;
        dsSXFRP.FRcMid   = dsWHFRPout.FRmid;
        dsSXFRP.FRdtob   =
         %dec(DateConvertFROI(dsWHFRPout.FRdtOB:'MDYY':*on):8:0);
        dsSXFRP.FRss#    = MaskData('SSN':'FRss#':dsWHFRPout.FRss);
        dsSXFRP.FRstoh   = dsWHFRPout.FRstHr;
        dsSXFRP.FRsex    = dsWHFRPout.FRsex;
        dsSXFRP.FRcAd1   = dsWHFRPout.FRadr1;
        dsSXFRP.FRcAd2   = dsWHFRPout.FRadr2;
        dsSXFRP.FRcCty   = dsWHFRPout.FRcty;
        dsSXFRP.FRcSt    = dsWHFRPout.FRst;
        dsSXFRP.FRcZip   = dsWHFRPout.FRzip;
        dsSXFRP.FRcArea  = dsWHFRPout.FRarea;
        dsSXFRP.FRcPhn#  = dsWHFRPout.FRphn;
        dsSXFRP.FRmSt    = dsWHFRPout.FRmSts;
        dsSXFRP.FRjrSt   = dsWHFRPout.FRjrSt;
        dsSXFRP.FRocDs   = dsWHFRPout.FRocDs;
        dsSXFRP.FRestD   = GetGenCodeDs(decCo#:'EMST':dsWHFRPout.FReSts);
        dsSXFRP.FRdPnd   = dsWHFRPout.FRdep;
        dsSXFRP.FRclas   = dsWHFRPout.FRclas;
        dsSXFRP.FRpoi    = dsWHFRPout.FRpoi;
        dsSXFRP.FRaww    = dsWHFRPout.FRaww;
        dsSXFRP.FRdays   = dsWHFRPout.FRdays;
        dsSXFRP.FRfSal   = dsWHFRPout.FRfSal;
        dsSXFRP.FRacDt   = %dec(DateConvertFROI(dsWHFRPout.FRacDt:'MDYY'):8:0);
        dsSXFRP.FRacTm   = dsWHFRPout.FRacTm;
        dsSXFRP.FRdtLw   =
         %dec(DateConvertFROI(dsWHFRPout.FRdtLW:'MDYY':*on):8:0);
        dsSXFRP.FRdtRp   =
         %dec(DateConvertFROI(dsWHFRPout.FRdtRp:'MDYY':*on):8:0);
        dsSXFRP.FRdtAN   =
         %dec(DateConvertFROI(dsWHFRPout.FRdtRp:'MDYY':*on):8:0);
        dsSXFRP.FRdbDt   =
         %dec(DateConvertFROI(dsWHFRPout.FRdbDt:'MDYY':*on):8:0);
        dsSXFRP.FRrtw    =
         %dec(DateConvertFROI(dsWHFRPout.FRrTW:'MDYY':*on):8:0);
        dsSXFRP.FRhiDt    =
         %dec(DateConvertFROI(dsWHFRPout.FRhiDt:'MDYY':*on):8:0);
        dsSXFRP.FRdod    =
         %dec(DateConvertFROI(dsWHFRPout.FRdod:'MDYY':*on):8:0);
        dsSXFRP.FRrcvd   =
         %dec(DateConvertFROI(dsWHFRPout.FRrcvd:'MDYY':*on):8:0);
        dsSXFRP.FRnatr   = dsWHFRPout.FRnatr;
        dsSXFRP.FRnads   = NatureCdDs(decCo#:dsWHFRPout.FRnatr);
        dsSXFRP.FRnatrDs = dsSXFRP.FRnaDs;
        dsSXFRP.FRpart   = dsWHFRPout.FRpart;
        dsSXFRP.FRpads   = PartCdDs(decCo#:dsWHFRPout.FRpart);
        dsSXFRP.FRpartDs = dsSXFRP.FRpaDs;
        dsSXFRP.FRcaus   = dsWHFRPout.FRcaus;
        dsSXFRP.FRcausDs = CauseCdDs(decCo#:dsWHFRPout.FRcaus);
        dsSXFRP.FRePrm   = dsWHFRPout.FReAdr;
        dsSXFRP.FRinit   = dsWHFRPout.FRiTrt;
        dsSXFRP.FRinitDs = GetGenCodeDs(decCo#:'INIT':dsWHFRPout.FRiTrt);
        // Physician Info
        dsSXFRP.FRphyN   = dsWHFRPout.FRphyN;
        dsSXFRP.FRphyA   = dsWHFRPout.FRphyA;
        dsSXFRP.FRtCty   = dsWHFRPout.FRphyC;
        dsSXFRP.FRtSt    = dsWHFRPout.FRphyS;
        dsSXFRP.FRtZip   = dsWHFRPout.FRphyZ;
        // Hospital Info
        dsSXFRP.FRhosN   = dsWHFRPout.FRhosN;
        dsSXFRP.FRhosA   = dsWHFRPout.FRhosA;
        dsSXFRP.FRhosC   = dsWHFRPout.FRhosC;
        dsSXFRP.FRhosS   = dsWHFRPout.FRhosS;
        dsSXFRP.FRhosZ   = dsWHFRPout.FRhosZ;

        dsSXFRP.FRdte    = %dec(%date():*usa);
        dsSXFRP.FRiDt    = %dec(%date():*cymd);

        Select;
        //When dsWHFRPout.FRJRST = ;
        Other;
           Exsr LoadXmlA1;
        Endsl;

        Exec sql
         insert into SXFRP
           values(:dsSXFRP);
       Endsr;

       //=================================================================
       // Load A1 fields
       //=================================================================
       Begsr LoadXmlA1;
        dsSXFRP.FRwgp    = dsWHFRPout.FR20a9;
        dsSXFRP.FRbgTm   = dsWHFRPout.FRtim2;
        dsSXFRP.FRaccL1  = dsWHFRPout.FRacDr;
        dsSXFRP.FRaccL2  = dsWHFRPout.FR40a3;
        dsSXFRP.FRaCty   = dsWHFRPout.FRaCty;
        dsSXFRP.FRaSt    = dsWHFRPout.FRaSt;
        dsSXFRP.FRaZip   = dsWHFRPout.FRacZp;
        dsSXFRP.FReUse1  = dsWHFRPout.FR60A1;
        dsSXFRP.FReUse2  = dsWHFRPout.FR60A2;
        dsSXFRP.FRempA1  = dsWHFRPout.FR60A3;
        dsSXFRP.FRempA2  = dsWHFRPout.FR60A4;
        dsSXFRP.FRwPrc1  = dsWHFRPout.FR60A5;
        dsSXFRP.FRwPrc2  = dsWHFRPout.FR60A6;
        dsSXFRP.FRhowi1  = dsWHFRPout.FR80A1;
        dsSXFRP.FRhowi2  = dsWHFRPout.FR80A2;
        dsSXFRP.FRsafeP  = dsWHFRPout.FR1A1;
        dsSXFRP.FRsafeU  = dsWHFRPout.FR1A2;
        //dsSXFRP.FRphyE   = NI40A4;
        //dsSXFRP.FRhosE   = NI40A8;
        dsSXFRP.FRprPhA  = dsWHFRPout.FRprPh;
        Monitor;
           dsSXFRP.FRprPh   = %Dec(%Trim(dsWHFRPout.FRprPh):10:0);
        On-error;
        Endmon;
        Monitor;
           //dsSXFRP.FRdTan = %dec(%date(NIdt5:*cymd):*usa);
        On-Error;
        EndMon;
        dsSXFRP.FRprNm   = dsWHFRPout.FRprNm;

       Endsr;

       //=================================================================
       // Print FROI
       //=================================================================
       Begsr Print;
        Reset dsWHFRPin;
        dsWHFRPin.FRco   = decCo#;
        dsWHFRPin.FRss   = dsIn.ss#;
        dsWHFRPin.FRacDt = DateConvertFROI(dsIn.accDt:'YYMD');
        dsWHFRPin.FRiDt  = DateConvertFROI(dsIn.initDt:'YYMD');
        dsWHFRPin.FRiTm  = %int(dsIn.initTime);
        Exec sql
         select * into :dsWHFRPout
          from WHFRP_HLD
          where FRco = :dsWHFRPin.FRco and FRss = :dsWHFRPin.FRss and
           FRacDt = :dsWHFRPin.FRacDt and FRiDt = :dsWHFRPin.FRiDt
           and FRiTm = :dsWHFRPin.FRiTm;
        If not IsValidFetch(sqlstt);
           Exec sql
            select * into :dsWHFRPout
             from WHFRP
             where FRco = :dsWHFRPin.FRco and FRss = :dsWHFRPin.FRss and
              FRacDt = :dsWHFRPin.FRacDt and FRiDt = :dsWHFRPin.FRiDt
              and FRiTm = :dsWHFRPin.FRiTm;
        Endif;
        If not IsValidFetch(sqlstt);
           AddErrorMessageJson('FROI can not be found for printing.':
            ioRtnCode);
           Leavesr;
        Endif;
        Exsr LoadXml;
        Monitor;
           //Call Scriptura to print
           CreateXML(strXmlFileName:*off);
           In dsXmlMsg;
        On-error;
          strXmlMsg = '*Failure';
        Endmon;
        Select;
        When strXmlMsg = *blanks or %subst(strXmlMsg:1:7) = 'Success';
           // Return output information to Portal
           json += ',"documentInfo":' + ElementOpen('{') +
            BuildJsonNode('documentPath':GetXMLOutputName(strXmlFileName:*on)) +
            ElementClose;

           LogFROI(dsWHFRPout.FRCo:dsWHFRPout.FRFnd:dsWHFRPout.FREmp:
            dsWHFRPout.FRDiv:dsWHFRPout.FRSS:dsWHFRPout.FRAcdt:
            dsWHFRPout.FRdtrp:inUser:'FROI Printed':'FROI has been printed ' +
            ' by user ' + %trim(inUser) + '.');

        Other;
           AddErrorMessageJson('Scriptura unable to produce document.'
            :ioRtnCode);
        Endsl;
       Endsr;

      /End-free
     PPrintFROI        e

      //=============================================================
      // Save FROI information
      //=============================================================
     PSaveFROI         b
     DSaveFROI         pr

     d ic              s             10i 0 inz
     d nodec           s                   like(yajl_val)

     ddtAccident       s               d   inz
     ddt7Accident      s              7  0 inz
     ddt8Accident      s              8  0 inz
     ddt7Cancel        s              7  0 inz
     disUpdate         s              1n   inz(*off)

     ddsWDELP        e ds                  extname(WDELP) inz qualified
     ddsWMEMP        e ds                  extname(WMEMP) inz qualified
     ddsKeyIn          ds                  likeds(dsKeyInfo) inz
     ddsKeyOut         ds                  likeds(dsKeyInfo) inz

     ddsIn             ds                  inz qualified
     d key                                 likeds(dsKeyInfo)
     d companyInfo                         likeds(dsCompanyInfo)
     d groupInfo                           likeds(dsGroupInfo)
     d claimInfo                           likeds(dsClaimInfo)
     d employerInfo                        likeds(dsEmployerInfo)
     d locationInfo                        likeds(dsLocationInfo)
     d agentInfo                           likeds(dsAgentInfo)
     d claimantInfo                        likeds(dsClaimantInfo)
     d physicianInfo                       likeds(dsPhysicianInfo)
     d hospitalInfo                        likeds(dsHospitalInfo)
     d preparerInfo                        likeds(dsPreparerInfo)
     d accidentInfo                        likeds(dsAccidentInfo)
     d agencyClaim#                  25
     d class                               likeds(dsCodeInfo)
     d wageInfo                            likeds(dsWageInfo)
     d nature                              likeds(dsCodeInfo)
     d part                                likeds(dsCodeInfo)
     d cause                               likeds(dsCodeInfo)
     d dates                               likeds(dsDates)
     d contactInfo                         likeds(dsContactInfo)
     d emplrPremises                  1
     d initTreatment                       likeds(dsCodeInfo)
     d safeguardProv                  1
     d safeguardUsed                  1
     d equipmentUsed                120
     d employeeAct                  120
     d workProcess                  120
     d howInjOccurred               160
     d thirdParty                          likeds(dsThirdParty)
     d relative                            likeds(dsRelative)
     d contact                             likeds(dsContactInfo)
     d witness                             likeds(dsWitness)

     dstrData          s            500    inz varying
     dstrProcess       s             25    inz

      /Free
       Exsr GetParms;
       Exsr BuildOutput;
       json += ProcessErrorMessagesJson('*PRE');
       json = %scanrpl(',':'':json:1:1);

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Exsr DataSave;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {SaveFROI}'));
       Endsr;

       //=================================================================
       // Edit the data
       //=================================================================
       Begsr DataEdit;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Monitor;
           If isFroiImport = *On and
            %int(dsIn.groupInfo.basicInfo.number) <> *zero;
              ioRtnCode = *blanks;
           Else;
              If %int(dsIn.groupInfo.basicInfo.number) <> *zero
               and not UseGroupInPortal(%int(dsIn.companyInfo.basicInfo.number)
                :%int(dsIn.groupInfo.basicInfo.number):'FROI');
                 AddErrorMessageJson('Group ' +
                  dsIn.groupInfo.basicInfo.number +
                  ' is not included for service requested ' +
                  '(' + %trim(inRequest) + ').':ioRtnCode);
              Endif;
           Endif;
        On-error;
        Endmon;
        If dsIn.claimantInfo.ssn = *blanks or dsIn.claimantInfo.ssn = *zeros;
           AddErrorMessageJson('Social security number is required.':
             ioRtnCode);
        Endif;
        If dsIn.accidentInfo.date = *blanks or dsIn.accidentInfo.date = *zeros;
           AddErrorMessageJson('Accident date is required.':
             ioRtnCode);
        Endif;
        If ioRtnCode <> 'Error';
           Exec sql
            select * into :dsWDELP
             from WDELP
             where ELco# = :dsWHFRPout.FRco and ELfnd = :dsWHFRPout.FRfnd
              and ELemp# = :dsWHFRPout.FRemp and ELdiv = :dsWHFRPout.FRdiv
              and ELpfDt <= :dt7Accident and ELptDt > :dt7Accident;
           If not IsValidFetch(sqlstt);
              AddErrorMessageJson('The accident date entered does not fall ' +
               'within any of the effective policies.':ioRtnCode);
           Endif;
        Endif;
        If dt7Accident > sysDate7;
           AddErrorMessageJson('The accident date must be less or equal to ' +
            'system date.':ioRtnCode);
        Endif;
        dt7Cancel = GetCxDate(dsWHFRPout.FRco:dsWHFRPout.FRfnd:dsWHFRPout.FRemp
         :dsWHFRPout.FRdiv);
        If dt7Cancel <> 0 and dt7Cancel < dt7Accident;
           AddErrorMessageJson('Policy is cancelled as of the accident ' +
            'date.':ioRtnCode);
        Endif;
        Reset dsWHFRPin;
        Exec sql
         select * into :dsWHFRPin
          from WHFRP
          where FRco = :dsWHFRPout.FRco and
           FRss = :dsWHFRPout.FRss and FRacDt = :dsWHFRPout.FRacDt;
        If IsValidFetch(sqlstt);
           AddErrorMessageJson('FROI has previously been submitted for ' +
            'requested social security number and accident date.');
        Endif;
       Endsr;

       //=================================================================
       // Load the data
       //=================================================================
       Begsr DataLoad;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Reset dsWHFRPout;
        Monitor;
           dsWHFRPout.FRiDt = dsIn.key.initDt;
           dsWHFRPout.FRiTm = %int(dsIn.key.initTime);
        On-error;
        Endmon;
        If dsIn.companyInfo.basicInfo.number = *zero;
           dsIn.companyInfo.basicInfo.number = %char(intCo#);
        Endif;
        dsWHFRPout.FRstat = 'H';
        // Company information
        dsWHFRPout.FRco   = %int(dsIn.companyInfo.basicInfo.number);
        // Group information
        Monitor;
           dsWHFRPout.FRfnd  = %int(dsIn.groupInfo.basicInfo.number);
        On-error;
        Endmon;
        // Employer information
        Monitor;
           dsWHFRPout.FRemp  = %int(dsIn.employerInfo.basicInfo.number);
        On-error;
        Endmon;
        dsWHFRPout.FRdiv  = dsIn.employerInfo.division;
        Reset dsWMEMP;
        Exec sql
         select * into :dsWMEMP
          from WMEMP
          where EMco# = :dsWHFRPout.FRco and EMfnd = :dsWHFRPout.FRfnd
           and EMemp# = :dsWHFRPout.FRemp and EMdiv = :dsWHFRPout.FRdiv;
        dsWHFRPout.FRfein = dsIn.employerInfo.basicInfo.fein;
        If dsWHFRPout.FRfein = *zeros;
           dsWHFRPout.FRfein = dsWMEMP.EMfein;
        Endif;
        // Location information
        dsWHFRPout.FRctg1 = dsIn.locationInfo.basicInfo.number;
        // Claimaint information
        dsWHFRPout.FRss   = dsIn.claimantInfo.ssn;
        dsWHFRPout.FRlsn  = dsIn.claimantInfo.name.last;
        dsWHFRPout.FRfsn  = dsIn.claimantInfo.name.first;
        dsWHFRPout.FRmid  = dsIn.claimantInfo.name.middle;
        dsWHFRPout.FRdtOB = DateConvertFROI(dsIn.claimantInfo.dob:'YYMD');
        dsWHFRPout.FRdOD  = DateConvertFROI(dsIn.claimantInfo.dod:'YYMD');
        dsWHFRPout.FRstHr = dsIn.claimantInfo.stateOfHire;
        dsWHFRPout.FRsex  = dsIn.claimantInfo.gender;
        dsWHFRPout.FRadr1 = dsIn.claimantInfo.address.line1;
        dsWHFRPout.FRadr2 = dsIn.claimantInfo.address.line2;
        dsWHFRPout.FRcty  = dsIn.claimantInfo.address.city;
        dsWHFRPout.FRst   = dsIn.claimantInfo.address.state;
        dsWHFRPout.FRzip  = dsIn.claimantInfo.address.zip;
        dsWHFRPout.FRarea = dsIn.claimantInfo.phone1.areaCode;
        dsWHFRPout.FRphn  = dsIn.claimantInfo.phone1.phone;
        dsWHFRPout.FR3n1  = dsIn.claimantInfo.phoneWork.areaCode;
        dsWHFRPout.FR9n1  = dsIn.claimantInfo.phoneWork.phone;
        dsWHFRPout.FRmail = dsIn.claimantInfo.email;
        dsWHFRPout.FRmSts = dsIn.claimantInfo.maritalStatus;
        dsWHFRPout.FRocDs = dsIn.claimantInfo.occupationDesc;
        dsWHFRPout.FReSts = dsIn.claimantInfo.employmentSts.code;
        dsWHFRPout.FRdep  = dsIn.claimantInfo.dependents#;
        //*** Beg Add *** 08/29/22 *******************************//
        // Physician information
        dsWHFRPout.FR80a3 = %Trim(dsIn.witness.name) + ','+
                            %Trim(dsIn.witness.phone1);
        //*** End Add *** 08/29/22 *******************************//
        // Physician information
        dsWHFRPout.FRphyN = dsIn.physicianInfo.name;
        dsWHFRPout.FRphyA = dsIn.physicianInfo.address.line1;
        dsWHFRPout.FR40a5 = dsIn.physicianInfo.address.line2;
        dsWHFRPout.FRphyC = dsIn.physicianInfo.address.city;
        dsWHFRPout.FRphyS = dsIn.physicianInfo.address.state;
        dsWHFRPout.FRphyZ = dsIn.physicianInfo.address.zip;
        dsWHFRPout.FRphyR = dsIn.physicianInfo.phone1.areaCode;
        dsWHFRPout.FRphyP = dsIn.physicianInfo.phone1.phone;
        // Hospital information
        dsWHFRPout.FRhosN = dsIn.hospitalInfo.name;
        dsWHFRPout.FRhosA = dsIn.hospitalInfo.address.line1;
        dsWHFRPout.FRhosC = dsIn.hospitalInfo.address.city;
        dsWHFRPout.FRhosS = dsIn.hospitalInfo.address.state;
        dsWHFRPout.FRhosZ = dsIn.hospitalInfo.address.zip;
        dsWHFRPout.FR3n2  = dsIn.hospitalInfo.phone1.areaCode;
        dsWHFRPout.FR9n4  = dsIn.hospitalInfo.phone1.phone;
        // Preparer information
        dsWHFRPout.FRprep = DateConvertFROI(dsIn.preparerInfo.date:'YYMD');
        dsWHFRPout.FRprNm = dsIn.preparerInfo.name;
        dsWHFRPout.FRprPh = %char(dsIn.preparerInfo.phone1.areaCode) +
         %char(dsIn.preparerInfo.phone1.phone);
        // Accident information
        dsWHFRPout.FRacDt = DateConvertFROI(dsIn.accidentInfo.date:'YYMD');
        Monitor;
           dsWHFRPout.FRacTm = %int(dsIn.accidentInfo.time);
        On-error;
        Endmon;
        dsWHFRPout.FR40a1 = dsIn.accidentInfo.address.line1;
        dsWHFRPout.FR40a2 = dsIn.accidentInfo.address.line2;
        dsWHFRPout.FR25a1 = dsIn.accidentInfo.address.city;
        dsWHFRPout.FR2a1  = dsIn.accidentInfo.address.state;
        dsWHFRPout.FR9n2  = dsIn.accidentInfo.address.zip;
        dsWHFRPout.FRacDr = dsIn.accidentInfo.address.line1;
        dsWHFRPout.FR40a3 = dsIn.accidentInfo.address.line2;
        dsWHFRPout.FRaCty = dsIn.accidentInfo.address.city;
        dsWHFRPout.FRaSt  = dsIn.accidentInfo.address.state;
        dsWHFRPout.FRacZp = dsIn.accidentInfo.address.zip;
        // Claim information
        Monitor;
           dsWHFRPout.FRfyr  = %int(dsIn.claimInfo.year);
           dsWHFRPout.FRcase = %int(dsIn.claimInfo.case#);
        On-error;
        Endmon;
        dsWHFRPout.FRjrSt = dsIn.claimInfo.jurisdictionSt;
        // Wage information
        Select;
        When %Subst(dsIn.wageInfo.period:1:1) = 'W' Or
             %Subst(dsIn.wageInfo.period:1:1) = 'w';
           dsWHFRPout.FRfreq = '1';
        When %Subst(dsIn.wageInfo.period:1:1) = 'B' Or
             %Subst(dsIn.wageInfo.period:1:1) = 'b';
           dsWHFRPout.FRfreq = '2';
        When %Subst(dsIn.wageInfo.period:1:1) = 'M' Or
             %Subst(dsIn.wageInfo.period:1:1) = 'm';
           dsWHFRPout.FRfreq = '4';
        When %Subst(dsIn.wageInfo.period:1:1) = 'D' Or
             %Subst(dsIn.wageInfo.period:1:1) = 'd';
           dsWHFRPout.FRfreq = '6';
        EndSl;
        dsWHFRPout.FR20a9 = dsIn.wageInfo.period;
        dsWHFRPout.FRpoi  = dsIn.wageInfo.fullPaid;
        Monitor;
           dsWHFRPout.FRaww  = %int(dsIn.wageInfo.avgWeekly);
           dsWHFRPout.FRdays = %int(dsIn.wageInfo.daysOverAWeek);
           dsWHFRPout.FRtim2 = %int(dsIn.wageInfo.beginTime);
        On-error;
        Endmon;
        dsWHFRPout.FRfSal = dsIn.wageInfo.fullSalary;
        // Nature/Part/Cause information
        dsWHFRPout.FRnatr = dsIn.nature.code;
        dsWHFRPout.FRpart = dsIn.part.code;
        dsWHFRPout.FRcaus = dsIn.cause.code;
        // Contact information
        Select;
        When dsIn.employerInfo.contact <> *blanks;
           dsWHFRPout.FRcont = dsIn.employerInfo.contact;
        When dsIn.contactInfo.name.full <> *blanks;
           dsWHFRPout.FRcont = dsIn.contactInfo.name.full;
        When dsIn.contactInfo.name.last  <> *blanks Or
             dsIn.contactInfo.name.first <> *blanks;
           dsWHFRPout.FRcont =
            FormatName(dsIn.contactInfo.name.last:dsIn.contactInfo.name.first
             :dsIn.contactInfo.name.middle:'*FML');
        Endsl;
        dsWHFRPout.FRcAr  = %char(dsIn.contactInfo.phone1.areaCode);
        dsWHFRPout.FRcPhn = %char(dsIn.contactInfo.phone1.phone);
        // Other information
        Monitor;
           dsWHFRPout.FRclas = %int(dsIn.class.code);
        On-error;
        Endmon;
        dsWHFRPout.FRrcvd = DateConvertFROI(dsIn.dates.recieved:'YYMD');
        dsWHFRPout.FRdtLW = DateConvertFROI(dsIn.dates.lastWorked:'YYMD');
        dsWHFRPout.FRdtRp = DateConvertFROI(dsIn.dates.reported:'YYMD');
        dsWHFRPout.FRdbDt = DateConvertFROI(dsIn.dates.disability:'YYMD');
        dsWHFRPout.FRrTW  = DateConvertFROI(dsIn.dates.returnToWork:'YYMD');
        dsWHFRPout.FRhiDt = DateConvertFROI(dsIn.dates.hire:'YYMD');
        dsWHFRPout.FReAdr = dsIn.emplrPremises;
        dsWHFRPout.FR60a1 = %subst(dsIn.equipmentUsed: 1:60);
        dsWHFRPout.FR60a2 = %subst(dsIn.equipmentUsed:61:60);
        dsWHFRPout.FR60a3 = %subst(dsIn.employeeAct: 1:60);
        dsWHFRPout.FR60a4 = %subst(dsIn.employeeAct:61:60);
        dsWHFRPout.FR60a5 = %subst(dsIn.workProcess: 1:60);
        dsWHFRPout.FR60a6 = %subst(dsIn.workProcess:61:60);
        dsWHFRPout.FR80a1 = %subst(dsIn.howInjOccurred: 1:80);
        dsWHFRPout.FR80a2 = %subst(dsIn.howInjOccurred:81:60);
        dsWHFRPout.FR1a1  = dsIn.safeguardProv;
        dsWHFRPout.FR1a2  = dsIn.safeguardUsed;
        dsWHFRPout.FRiTrt = dsIn.initTreatment.code;
        // Maintenance information
        dsWHFRPout.FRpgm = q1pgm;
        dsWHFRPout.FRuser = inUser;
        dsWHFRPout.FRdate = %char(%date():*iso0);
        dsWHFRPout.FRtime = sysTime6;
       Endsr;

       //=================================================================
       // Save the data
       //=================================================================
       Begsr DataSave;
        Reset strData;
        Reset dsKeyOut;
        Exsr DataLoad;
        Exsr DataEdit;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        Reset dsWHFRPin;
        Monitor;
           dsWHFRPin.FRco = %int(dsIn.key.co#);
           dsWHFRPin.FRss = dsIn.key.ss#;
           dsWHFRPin.FRacDt = dsIn.key.accDt;
           dsWHFRPin.FRiDt = dsIn.key.initDt;
           dsWHFRPin.FRiTm = %int(dsIn.key.initTime);
        On-error;
        Endmon;
        Exec sql
         select * into :dsWHFRPin
          from WHFRP_HLD
          where FRco = :dsWHFRPin.FRco and
           FRss = :dsWHFRPin.FRss and FRacDt = :dsWHFRPin.FRacDt
           and FRiDt = :dsWHFRPin.FRiDt and FRiTm = :dsWHFRPin.FRiTm;
        If IsValidFetch(sqlstt);
           strProcess = 'updated';
           dsWHFRPout.FRiPgm = dsWHFRPin.FRiPgm;
           dsWHFRPout.FRiUsr = dsWHFRPin.FRiUsr;
           dsWHFRPout.FRiDt  = dsWHFRPin.FRiDt;
           dsWHFRPout.FRiTm  = dsWHFRPin.FRiTm;
           dsKeyOut.co# = %char(dsWHFRPin.FRco);
           dsKeyOut.ss# = dsWHFRPin.FRss;
           dsKeyOut.accDt = dsWHFRPin.FRacDt;
           dsKeyOut.initDt = dsWHFRPin.FRiDt;
           dsKeyOut.initTime = %char(dsWHFRPin.FRiTm);
           Exec sql
            update WHFRP_HLD
             set row = :dsWHFRPout
             where FRco = :dsWHFRPin.FRco and
              FRss = :dsWHFRPin.FRss and FRacDt = :dsWHFRPin.FRacDt
              and FRiDt = :dsWHFRPin.FRiDt and FRiTm = :dsWHFRPin.FRiTm;
           If dsKeyIn <> dsKeyOut;
              strData = 'FROI has been updated ' +
               ' by user ' + %trim(inUser) + '. Key information has changed' +
               ' from ' + dsKeyIn + ' to ' + dsKeyOut + '.';
           Endif;
        Else;
           strProcess = 'created';
           // Maintenance information
           dsWHFRPout.FRiPgm = dsWHFRPout.FRpgm;
           dsWHFRPout.FRiUsr = dsWHFRPout.FRuser;
           dsWHFRPout.FRiDt  = dsWHFRPout.FRdate;
           dsWHFRPout.FRiTm  = dsWHFRPout.FRtime;
           Exec sql
            insert into WHFRP_HLD
             values(:dsWHFRPout);
        Endif;
        If not IsValidFetch(sqlstt);
           Exec sql
            GET DIAGNOSTICS CONDITION 1
             :dsSqlDiag.MESSAGE_TEXT = MESSAGE_TEXT;
           AddErrorMessageJson('Error occured during save. ' +
            dsSqlDiag.MESSAGE_TEXT:ioRtnCode);
           Leavesr;
        Endif;

        LogFROI(dsWHFRPout.FRCo:dsWHFRPout.FRFnd:dsWHFRPout.FREmp:
         dsWHFRPout.FRDiv:dsWHFRPout.FRSS:dsWHFRPout.FRAcdt:
         dsWHFRPout.FRdtrp:inUser:strProcess:strData);

        // Key information
        json += ',"keyInfo":{' +
         BuildJsonNode('company':%char(dsWHFRPout.FRco)) + ',' +
         BuildJsonNode('claimantSSN':dsWHFRPout.FRss) + ',' +
         BuildJsonNode('accidentDate':
          DateConvertFROI(dsWHFRPout.FRacDt:'MDYY')) + ',' +
         BuildJsonNode('initialDate':
          DateConvertFROI(dsWHFRPout.FRiDt:'MDYY')) + ',' +
         BuildJsonNode('initialTime':%char(dsWHFRPout.FRiTm)) + '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        Reset dsKeyIn;
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;

        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'keyInfo';
              Exsr GetParmsKey;
           When key = 'company';
              Exsr GetParmsCompany;
           When key = 'group';
              Exsr GetParmsGroup;
           When key = 'employer';
              Exsr GetParmsEmployer;
           When key = 'agent';
              Exsr GetParmsAgent;
           When key = 'claimant';
              Exsr GetParmsClaimant;
        //*** Beg Add *** 08/29/22 *******************************//
           When key = 'witness';
              Exsr GetParmsWitness;
        //*** End Add *** 08/29/22 *******************************//
           When key = 'physician';
              Exsr GetParmsPhysician;
           When key = 'hospital';
              Exsr GetParmsHospital;
           When key = 'preparer';
              Exsr GetParmsPreparer;
           When key = 'accident';
              Exsr GetParmsAccident;
           When key = 'year';
              dsIn.claimInfo.year = YAJL_get_string(val);
           When key = 'case';
              dsIn.claimInfo.case# = YAJL_get_string(val);
           When key = 'jurisdictionState';
              dsIn.claimInfo.jurisdictionSt = YAJL_get_string(val);
           When key = 'agencyClaimNumber';
              dsIn.agencyClaim# = YAJL_get_string(val);
           When key = 'class';
              nodec = YAJL_object_find(node0:key);
              Exsr GetParmsCode;
              dsIn.class = dsCodeInfo;
           When key = 'wage';
              Exsr GetParmsWage;
           When key = 'nature';
              nodec = YAJL_object_find(node0:key);
              Exsr GetParmsCode;
              dsIn.nature = dsCodeInfo;
           When key = 'part';
              nodec = YAJL_object_find(node0:key);
              Exsr GetParmsCode;
              dsIn.part = dsCodeInfo;
           When key = 'cause';
              nodec = YAJL_object_find(node0:key);
              Exsr GetParmsCode;
              dsIn.cause = dsCodeInfo;
           When key = 'dates';
              Exsr GetParmsDates;
           When key = 'contact';
              Exsr GetParmsContact;
           When key = 'employerPremises';
              dsIn.emplrPremises = YAJL_get_string(val);
           When key = 'initialTreatment';
              nodec = YAJL_object_find(node0:key);
              Exsr GetParmsCode;
              dsIn.initTreatment = dsCodeInfo;
           When key = 'safeguard';
              Reset i1;
              node1 = YAJL_object_find(node0:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'provided';
                    dsIn.safeguardProv = YAJL_get_string(val);
                 When key = 'used';
                    dsIn.safeguardUsed = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '}';
           When key = 'equipmentUsed';
              dsIn.equipmentUsed = YAJL_get_string(val);
           When key = 'employeeActivity';
              dsIn.employeeAct = YAJL_get_string(val);
           When key = 'workProcess';
              dsIn.workProcess = YAJL_get_string(val);
           When key = 'howInjuryOccurred';
              dsIn.howInjOccurred = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';

        If strAuthority = '*View';
           AddErrorMessageJson('User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ').'
            :ioRtnCode);
           Leavesr;
        Endif;

        If dsIn.key.initDt <> *blank and
         not FroiFound(%int(dsIn.key.co#):dsIn.key.ss#:dsIn.key.accDt
          :dsIn.key.initDt:%int(dsIn.key.initTime));
           AddErrorMessageJson('FROI not found for key information entered.'
            :ioRtnCode);
        EndIf;

       Endsr;

       //=================================================================
       // Break parms out from inParms - accident info
       //=================================================================
       Begsr GetParmsAccident;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'date';
              dsIn.accidentInfo.date = YAJL_get_string(val);
           When key = 'time';
              dsIn.accidentInfo.time = YAJL_get_string(val);
           When key = 'address';
              node2 = YAJL_object_find(node1:key);
              ExtractJsonAddress(node2:strLine1:strLine2:strCity:strState:
               decZip:jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:%len(json)-1);
              dsIn.accidentInfo.address.line1 = %trim(strLine1);
              dsIn.accidentInfo.address.line2 = %trim(strLine2);
              dsIn.accidentInfo.address.city = %trim(strCity);
              dsIn.accidentInfo.address.state = %trim(strState);
              dsIn.accidentInfo.address.zip = decZip;
           Endsl;
        Enddo;
        json += '}';
        Monitor;
           dt8Accident = %int( dsIn.accidentInfo.date);
           dt7Accident = DtCYMD(dt8Accident);
           dtAccident = CvtToDate(dt8Accident);
        On-error;
        Endmon;
       Endsr;

       //=================================================================
       // Break parms out from inParms - agent info
       //=================================================================
       Begsr GetParmsAgent;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.agentInfo.basicInfo.number = YAJL_get_string(val);
           When key = 'name';
              dsIn.agentInfo.basicInfo.name = YAJL_get_string(val);
           When key = 'fein';
              Monitor;
                 dsIn.agentInfo.basicInfo.fein = %int(YAJL_get_string(val));
              On-error;
              Endmon;
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - claimant info
       //=================================================================
       Begsr GetParmsClaimant;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'ssn';
              dsIn.claimantInfo.ssn = YAJL_get_string(val);
           When key = 'name';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'first';
                    dsIn.claimantInfo.name.first = YAJL_get_string(val);
                 When key = 'last';
                    dsIn.claimantInfo.name.last = YAJL_get_string(val);
                 When key = 'middle';
                    dsIn.claimantInfo.name.middle = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += '}';
           When key = 'dob';
              dsIn.claimantInfo.dob = YAJL_get_string(val);
           When key = 'dod';
              dsIn.claimantInfo.dod = YAJL_get_string(val);
           When key = 'address';
              node2 = YAJL_object_find(node1:key);
              ExtractJsonAddress(node2:strLine1:strLine2:strCity:strState:
               decZip:jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:%len(json)-1);
              dsIn.claimantInfo.address.line1 = %trim(strLine1);
              dsIn.claimantInfo.address.line2 = %trim(strLine2);
              dsIn.claimantInfo.address.city = %trim(strCity);
              dsIn.claimantInfo.address.state = %trim(strState);
              dsIn.claimantInfo.address.zip = decZip;
           When key = 'phone';
              json = %scanrpl('""':'[':json:%len(json)-1);
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              Dow YAJL_array_loop(node2:i2:node3);
                 Reset i3;
                 Dow YAJL_object_loop(node3:i3:key:val);
                    json += '{';
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    node4 = YAJL_object_find(node3:key);
                    ExtractJsonPhone(node4:decAreaCode:decPhone:strType:
                     jsonExtract350);
                    json = %scanrpl('""':'{'+jsonExtract350+'}},':json:
                     %len(json)-1);
                    Select;
                    When key = 'phone1';
                       dsIn.claimantInfo.phone1.areaCode = decAreaCode;
                       dsIn.claimantInfo.phone1.phone = decPhone;
                       dsIn.claimantInfo.phone1.type = %trim(strType);
                    When key = 'work';
                       dsIn.claimantInfo.phoneWork.areaCode = decAreaCode;
                       dsIn.claimantInfo.phoneWork.phone = decPhone;
                       dsIn.claimantInfo.phoneWork.type = %trim(strType);
                    Endsl;
                 Enddo;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ']';
           When key = 'email';
              dsIn.claimantInfo.email = YAJL_get_string(val);
           When key = 'maritalStatus';
              dsIn.claimantInfo.maritalStatus = YAJL_get_string(val);
           When key = 'numberDependents';
              Monitor;
                 dsIn.claimantInfo.dependents# = %int(YAJL_get_string(val));
              On-error;
              Endmon;
           When key = 'stateOfHire';
              dsIn.claimantInfo.stateOfHire = YAJL_get_string(val);
           When key = 'gender';
              dsIn.claimantInfo.gender = YAJL_get_string(val);
           When key = 'occupationDescription';
              dsIn.claimantInfo.occupationDesc = YAJL_get_string(val);
           When key = 'employmentStatus';
              nodec = YAJL_object_find(node1:key);
              Exsr GetParmsCode;
              dsIn.claimantInfo.employmentSts = dsCodeInfo;
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - code info
       //=================================================================
       Begsr GetParmsCode;
        Reset dsCodeInfo;
        ExtractJsonCode(nodec:dsCodeInfo.code:dsCodeInfo.description:
         dsCodeInfo.otherDesc:jsonExtract600);
        json = %scanrpl('""':'{'+jsonExtract600+'}':json:%len(json)-1);
       Endsr;

       //=================================================================
       // Break parms out from inParms - company info
       //=================================================================
       Begsr GetParmsCompany;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.companyInfo.basicInfo.number = YAJL_get_string(val);
           When key = 'name';
              dsIn.companyInfo.basicInfo.name = YAJL_get_string(val);
           When key = 'fein';
              Monitor;
                 dsIn.companyInfo.basicInfo.fein = %int(YAJL_get_string(val));
              On-error;
              Endmon;
           When key = 'address';
              node2 = YAJL_object_find(node1:key);
              ExtractJsonAddress(node2:strLine1:strLine2:strCity:strState:
               decZip:jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:%len(json)-1);
              dsIn.companyInfo.address.line1 = %trim(strLine1);
              dsIn.companyInfo.address.line2 = %trim(strLine2);
              dsIn.companyInfo.address.city = %trim(strCity);
              dsIn.companyInfo.address.state = %trim(strState);
              dsIn.companyInfo.address.zip = decZip;
           When key = 'phone';
              json = %scanrpl('""':'[':json:%len(json)-1);
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              Dow YAJL_array_loop(node2:i2:node3);
                 Reset i3;
                 Dow YAJL_object_loop(node3:i3:key:val);
                    json += '{';
                    BuildGiantJSON(json:key:YAJL_get_string(val));
                    node4 = YAJL_object_find(node3:key);
                    ExtractJsonPhone(node4:decAreaCode:decPhone:strType:
                     jsonExtract350);
                    json = %scanrpl('""':'{'+jsonExtract350+'}},':json:
                     %len(json)-1);
                    Select;
                    When key = 'phone1';
                       dsIn.companyInfo.phone1.areaCode = decAreaCode;
                       dsIn.companyInfo.phone1.phone = decPhone;
                       dsIn.companyInfo.phone1.type = %trim(strType);
                    When key = 'fax';
                       dsIn.companyInfo.fax.areaCode = decAreaCode;
                       dsIn.companyInfo.fax.phone = decPhone;
                       dsIn.companyInfo.fax.type = %trim(strType);
                    Endsl;
                 Enddo;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += ']';
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - contact info
       //=================================================================
       Begsr GetParmsContact;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'first';
                    dsIn.contactInfo.name.first = YAJL_get_string(val);
                 When key = 'last';
                    dsIn.contactInfo.name.last = YAJL_get_string(val);
                 When key = 'middle';
                    dsIn.contactInfo.name.middle = YAJL_get_string(val);
                 When key = 'full';
                    dsIn.contactInfo.name.full = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += '}';
           When key = 'phone';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              ExtractJsonPhone(node2:decAreaCode:decPhone:strType:
               jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:
               %len(json)-1);
              dsIn.contactInfo.phone1.areaCode = decAreaCode;
              dsIn.contactInfo.phone1.phone = decPhone;
              dsIn.contactInfo.phone1.type = %trim(strType);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - date info
       //=================================================================
       Begsr GetParmsDates;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'lastWorked';
              dsIn.dates.lastWorked = YAJL_get_string(val);
           When key = 'reported';
              dsIn.dates.reported = YAJL_get_string(val);
           When key = 'disability';
              dsIn.dates.disability = YAJL_get_string(val);
           When key = 'returnToWork';
              dsIn.dates.returnToWork = YAJL_get_string(val);
           When key = 'reportedAsWorkRelated';
              dsIn.dates.reportedAsWork = YAJL_get_string(val);
           When key = 'adminNotified';
              dsIn.dates.adminNotified = YAJL_get_string(val);
           When key = 'employeeProvidedClaimForm';
              dsIn.dates.providedClmFrm = YAJL_get_string(val);
           When key = 'hire';
              dsIn.dates.hire = YAJL_get_string(val);
           When key = 'preperation';
              dsIn.dates.preperation = YAJL_get_string(val);
           When key = 'lastFullDayPaid';
              dsIn.dates.lastFullDayPd = YAJL_get_string(val);
           When key = 'recieved';
              dsIn.dates.recieved = YAJL_get_string(val);
           When key = 'fullSalary';
              dsIn.dates.fullSalary = YAJL_get_string(val);
           When key = 'fifthDayOfTotalOrPartial';
              dsIn.dates.fifthDay = YAJL_get_string(val);
           When key = 'eightDayOfDisability';
              dsIn.dates.eightDay = YAJL_get_string(val);
           Endsl;
        Enddo;
        //*** Begin Add *** 09/09/22 ***********************************
        If dsIn.dates.recieved = ' ';
           dsIn.dates.recieved = %EditC(DateTo8(systemDate()):'X');
        EndIf;
        //*** End   Add *** 09/09/22 ***********************************
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - employer info
       //=================================================================
       Begsr GetParmsEmployer;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'fein';
              Monitor;
                 dsIn.employerInfo.basicInfo.fein = %int(YAJL_get_string(val));
              On-error;
              Endmon;
           When key = 'number';
              dsIn.employerInfo.basicInfo.number = YAJL_get_string(val);
           When key = 'division';
              Monitor;
                 dsIn.employerInfo.division = %int(YAJL_get_string(val));
              On-error;
              Endmon;
           When key = 'name';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'dbaPersonalName';
                    dsIn.employerInfo.dbaPersonal = YAJL_get_string(val);
                 When key = 'dba';
                    dsIn.employerInfo.dbaName = YAJL_get_string(val);
                 When key = 'federalPersonalName';
                    dsIn.employerInfo.fedPersonal = YAJL_get_string(val);
                 When key = 'federal';
                    dsIn.employerInfo.fedName = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += '}';
           When key = 'address';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node2:i2:key:val);
                 json = %scanrpl(',':'':json:%len(json)-1);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 node3 = YAJL_object_find(node2:key);
                 ExtractJsonAddress(node3:strLine1:strLine2:strCity:strState:
                  decZip:jsonExtract350);
                 json = %scanrpl('""':'{'+jsonExtract350+'},':json:
                  %len(json)-1);
                 Select;
                 When key = 'mailing';
                    dsIn.employerInfo.addressMail.line1 = %trim(strLine1);
                    dsIn.employerInfo.addressMail.line2 = %trim(strLine2);
                    dsIn.employerInfo.addressMail.city = %trim(strCity);
                    dsIn.employerInfo.addressMail.state = %trim(strState);
                    dsIn.employerInfo.addressMail.zip = decZip;
                 When key = 'physical';
                    dsIn.employerInfo.addressPhys.line1 = %trim(strLine1);
                    dsIn.employerInfo.addressPhys.line2 = %trim(strLine2);
                    dsIn.employerInfo.addressPhys.city = %trim(strCity);
                    dsIn.employerInfo.addressPhys.state = %trim(strState);
                    dsIn.employerInfo.addressPhys.zip = decZip;
                 Endsl;
              Enddo;
              json = %scanrpl(',':'}':json:%len(json)-1);
           When key = 'location';
              Exsr GetParmsLocation;
           When key = 'phone';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              ExtractJsonPhone(node2:decAreaCode:decPhone:strType:
               jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:
               %len(json)-1);
              dsIn.employerInfo.phone1.areaCode = decAreaCode;
              dsIn.employerInfo.phone1.phone = decPhone;
              dsIn.employerInfo.phone1.type = %trim(strType);
           When key = 'email';
              dsIn.employerInfo.email = YAJL_get_string(val);
           When key = 'sic';
              nodec = YAJL_object_find(node1:key);
              Exsr GetParmsCode;
              dsIn.employerInfo.sic = dsCodeInfo;
           When key = 'contact';
              dsIn.employerInfo.contact = YAJL_get_string(val);
           When key = 'selfInsured';
              dsIn.employerInfo.selfInsured = YAJL_get_string(val);
           When key = 'natureOfBusiness';
              dsIn.employerInfo.natureOfBusi = YAJL_get_string(val);
           When key = 'typeOfEmployer';
              dsIn.employerInfo.typeOfEmployer = YAJL_get_string(val);
           When key = 'uiAccountNumber';
              dsIn.employerInfo.uiAccount# = YAJL_get_string(val);
           When key = 'stateRiskId';
              dsIn.employerInfo.stateRiskId = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - group info
       //=================================================================
       Begsr GetParmsGroup;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'number';
              dsIn.groupInfo.basicInfo.number = YAJL_get_string(val);
           When key = 'name';
              dsIn.groupInfo.basicInfo.name = YAJL_get_string(val);
           When key = 'fein';
              Monitor;
                 dsIn.groupInfo.basicInfo.fein = %int(YAJL_get_string(val));
              On-error;
              Endmon;
           When key = 'carrierCode';
              dsIn.groupInfo.carrierCode = YAJL_get_string(val);
           When key = 'address';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node2:i2:key:val);
                 json = %scanrpl(',':'':json:%len(json)-1);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 node3 = YAJL_object_find(node2:key);
                 ExtractJsonAddress(node3:strLine1:strLine2:strCity:strState:
                  decZip:jsonExtract350);
                 json = %scanrpl('""':'{'+jsonExtract350+'},':json:
                  %len(json)-1);
                 Select;
                 When key = 'mailing';
                    dsIn.groupInfo.addressMail.line1 = %trim(strLine1);
                    dsIn.groupInfo.addressMail.line2 = %trim(strLine2);
                    dsIn.groupInfo.addressMail.city = %trim(strCity);
                    dsIn.groupInfo.addressMail.state = %trim(strState);
                    dsIn.groupInfo.addressMail.zip = decZip;
                 When key = 'physical';
                    dsIn.groupInfo.addressPhys.line1 = %trim(strLine1);
                    dsIn.groupInfo.addressPhys.line2 = %trim(strLine2);
                    dsIn.groupInfo.addressPhys.city = %trim(strCity);
                    dsIn.groupInfo.addressPhys.state = %trim(strState);
                    dsIn.groupInfo.addressPhys.zip = decZip;
                 Endsl;
              Enddo;
              json = %scanrpl(',':'}':json:%len(json)-1);
           When key = 'phone';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              ExtractJsonPhone(node2:decAreaCode:decPhone:strType:
               jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:
               %len(json)-1);
              dsIn.groupInfo.phone1.areaCode = decAreaCode;
              dsIn.groupInfo.phone1.phone = decPhone;
              dsIn.groupInfo.phone1.type = %trim(strType);
           When key = 'alternate';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node2:i2:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'name';
                    dsIn.groupInfo.altName = YAJL_get_string(val);
                 When key = 'address';
                    node3 = YAJL_object_find(node2:key);
                    ExtractJsonAddress(node3:strLine1:strLine2:strCity:strState:
                     decZip:jsonExtract350);
                    json = %scanrpl('""':'{'+jsonExtract350+'},':json:
                     %len(json)-1);
                    dsIn.groupInfo.altAddress.line1 = %trim(strLine1);
                    dsIn.groupInfo.altAddress.line2 = %trim(strLine2);
                    dsIn.groupInfo.altAddress.city = %trim(strCity);
                    dsIn.groupInfo.altAddress.state = %trim(strState);
                    dsIn.groupInfo.altAddress.zip = decZip;
                 Endsl;
              Enddo;
              json = %scanrpl(',':'}':json:%len(json)-1);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - hospital info
       //=================================================================
       Begsr GetParmsHospital;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              dsIn.hospitalInfo.name = YAJL_get_string(val);
           When key = 'address';
              node2 = YAJL_object_find(node1:key);
              ExtractJsonAddress(node2:strLine1:strLine2:strCity:strState:
               decZip:jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:
               %len(json)-1);
              dsIn.hospitalInfo.address.line1 = %trim(strLine1);
              dsIn.hospitalInfo.address.line2 = %trim(strLine2);
              dsIn.hospitalInfo.address.city = %trim(strCity);
              dsIn.hospitalInfo.address.state = %trim(strState);
              dsIn.hospitalInfo.address.zip = decZip;
           When key = 'phone';
              node2 = YAJL_object_find(node1:key);
              ExtractJsonPhone(node2:decAreaCode:decPhone:strType:
               jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:
               %len(json)-1);
              dsIn.hospitalInfo.phone1.areaCode = decAreaCode;
              dsIn.hospitalInfo.phone1.phone = decPhone;
              dsIn.hospitalInfo.phone1.type = %trim(strType);
           When key = 'extraInfo';
              dsIn.hospitalInfo.extraInfo = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - key info
       //=================================================================
       Begsr GetParmsKey;
        Reset i1;
        json = %scanrpl('""':'{':json:%len(json)-1);
        node1 = YAJL_object_find(node0:key);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'company';
              dsIn.key.co# = YAJL_get_string(val);
           When key = 'claimantSSN';
              dsIn.key.ss# = YAJL_get_string(val);
           When key = 'accidentDate';
              dsIn.key.accDt = YAJL_get_string(val);
           When key = 'initialDate';
              dsIn.key.initDt = YAJL_get_string(val);
           When key = 'initialTime';
              dsIn.key.initTime = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
        dsIn.key.accDt = DateConvertFROI(dsIn.key.accDt:'YYMD');
        dsIn.key.initDt = DateConvertFROI(dsIn.key.initDt:'YYMD');
        dsKeyIn = dsIn.key;
       Endsr;

       //=================================================================
       // Break parms out from inParms - location info
       //=================================================================
       Begsr GetParmsLocation;
        Reset i2;
        node2 = YAJL_object_find(node1:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node2:i2:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'fein';
              Monitor;
                 dsIn.locationInfo.basicInfo.fein = %int(YAJL_get_string(val));
              On-error;
              Endmon;
           When key = 'number';
              dsIn.locationInfo.basicInfo.number = YAJL_get_string(val);
           When key = 'name';
              dsIn.locationInfo.basicInfo.name = YAJL_get_string(val);
           When key = 'county';
              dsIn.locationInfo.county = YAJL_get_string(val);
           When key = 'address';
              Reset i3;
              node3 = YAJL_object_find(node2:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node3:i3:key:val);
                 json = %scanrpl(',':'':json:%len(json)-1);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 node4 = YAJL_object_find(node3:key);
                 ExtractJsonAddress(node4:strLine1:strLine2:strCity:strState:
                  decZip:jsonExtract350);
                 json = %scanrpl('""':'{'+jsonExtract350+'},':json:
                  %len(json)-1);
                 Select;
                 When key = 'mailing';
                    dsIn.locationInfo.addressMail.line1 = %trim(strLine1);
                    dsIn.locationInfo.addressMail.line2 = %trim(strLine2);
                    dsIn.locationInfo.addressMail.city = %trim(strCity);
                    dsIn.locationInfo.addressMail.state = %trim(strState);
                    dsIn.locationInfo.addressMail.zip = decZip;
                 When key = 'physical';
                    dsIn.locationInfo.addressPhys.line1 = %trim(strLine1);
                    dsIn.locationInfo.addressPhys.line2 = %trim(strLine2);
                    dsIn.locationInfo.addressPhys.city = %trim(strCity);
                    dsIn.locationInfo.addressPhys.state = %trim(strState);
                    dsIn.locationInfo.addressPhys.zip = decZip;
                 Endsl;
              Enddo;
              json = %scanrpl(',':'}':json:%len(json)-1);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - physician info
       //=================================================================
       //*** Beg Add *** 08/29/22 *******************************//=======
       Begsr GetParmsWitness;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              dsIn.witness.name = YAJL_get_string(val);
           When key = 'phone';
              dsIn.witness.phone1 = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;
       //*** End Add *** 08/29/22 *******************************//=======
       //=================================================================
       Begsr GetParmsPhysician;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              dsIn.physicianInfo.name = YAJL_get_string(val);
           When key = 'address';
              node2 = YAJL_object_find(node1:key);
              ExtractJsonAddress(node2:strLine1:strLine2:strCity:strState:
               decZip:jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:
               %len(json)-1);
              dsIn.physicianInfo.address.line1 = %trim(strLine1);
              dsIn.physicianInfo.address.line2 = %trim(strLine2);
              dsIn.physicianInfo.address.city = %trim(strCity);
              dsIn.physicianInfo.address.state = %trim(strState);
              dsIn.physicianInfo.address.zip = decZip;
           When key = 'phone';
              node2 = YAJL_object_find(node1:key);
              ExtractJsonPhone(node2:decAreaCode:decPhone:strType:
               jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:
               %len(json)-1);
              dsIn.physicianInfo.phone1.areaCode = decAreaCode;
              dsIn.physicianInfo.phone1.phone = decPhone;
              dsIn.physicianInfo.phone1.type = %trim(strType);
           When key = 'extraInfo';
              dsIn.physicianInfo.extraInfo = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - preparer info
       //=================================================================
       Begsr GetParmsPreparer;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'name';
              dsIn.preparerInfo.name = YAJL_get_string(val);
           When key = 'title';
              dsIn.preparerInfo.title = YAJL_get_string(val);
           When key = 'extension';
              dsIn.preparerInfo.extension = YAJL_get_string(val);
           When key = 'preparedDate';
              dsIn.preparerInfo.date = YAJL_get_string(val);
           When key = 'phone';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              ExtractJsonPhone(node2:decAreaCode:decPhone:strType:
               jsonExtract350);
              json = %scanrpl('""':'{'+jsonExtract350+'}':json:
               %len(json)-1);
              dsIn.preparerInfo.phone1.areaCode = decAreaCode;
              dsIn.preparerInfo.phone1.phone = decPhone;
              dsIn.preparerInfo.phone1.type = %trim(strType);
           Endsl;
        Enddo;
        json += '}';
       Endsr;

       //=================================================================
       // Break parms out from inParms - wage info
       //=================================================================
       Begsr GetParmsWage;
        Reset i1;
        node1 = YAJL_object_find(node0:key);
        json = %scanrpl('""':'{':json:%len(json)-1);
        Dow YAJL_object_loop(node1:i1:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'period';
              dsIn.wageInfo.period = YAJL_get_string(val);
           When key = 'fullPaid';
              dsIn.wageInfo.fullPaid = YAJL_get_string(val);
           When key = 'averageWeekly';
              Reset i2;
              node2 = YAJL_object_find(node1:key);
              json = %scanrpl('""':'{':json:%len(json)-1);
              Dow YAJL_object_loop(node2:i2:key:val);
                 Select;
                 When key = 'wage';
                    dsIn.wageInfo.avgWeekly = YAJL_get_string(val);
                 When key = 'estimateActual';
                    dsIn.wageInfo.avgWeeklyEst = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json = %scanrpl(',':'':json:%len(json)-1);
              json += '}';
           When key = 'fullSalaryContinued';
              dsIn.wageInfo.fullSalary = YAJL_get_string(val);
           When key = 'daysOverAWeek';
              dsIn.wageInfo.daysOverAWeek = YAJL_get_string(val);
           When key = 'beginTime';
              dsIn.wageInfo.beginTime = YAJL_get_string(val);
           When key = 'atSame';
              dsIn.wageInfo.atSame = YAJL_get_string(val);
           When key = 'preInjuryWeekly';
              dsIn.wageInfo.preInjWeekly = YAJL_get_string(val);
           When key = 'postInjuryWeekly';
              dsIn.wageInfo.postInjWeekly = YAJL_get_string(val);
           When key = 'perHour';
              dsIn.wageInfo.perHour = YAJL_get_string(val);
           When key = 'perDay';
              dsIn.wageInfo.perDay = YAJL_get_string(val);
           When key = 'fiftyTwoWeeksPrior';
              dsIn.wageInfo.weeks52Prior = YAJL_get_string(val);
           Endsl;
        Enddo;
        json += '}';

        dsIn.wageInfo.period = Upper(dsIn.wageInfo.period);
        Select;
        When dsIn.wageInfo.period = 'D';
           dsIn.wageInfo.period = 'Day';
        When dsIn.wageInfo.period = 'H';
           dsIn.wageInfo.period = 'Hour';
        When dsIn.wageInfo.period = 'M';
           dsIn.wageInfo.period = 'Month';
        When dsIn.wageInfo.period = 'W';
           dsIn.wageInfo.period = 'Week';
        When dsIn.wageInfo.period = 'O';
           dsIn.wageInfo.period = 'Other';
        Endsl;
       Endsr;

      /End-free
     PSaveFROI         e

      //=============================================================
      // Submit FROI
      //=============================================================
     PSubmitFROI       b
     DSubmitFROI       pr

     ddecCo#           s              3  0 inz
     ddtAccident       s               d   inz
     ddt7Accident      s              7  0 inz
     ddt8Accident      s              8  0 inz
     ddtInitial        s               d   inz
     ddt7Initial       s              7  0 inz
     ddt8Initial       s              8  0 inz

     DdsIn             ds                  likeds(dsKeyInfo) inz
      /Free
       Exsr GetParms;
       Exsr BuildOutput;
       json = %scanrpl(',':'':json:%len(json)-1);
       json += ProcessErrorMessagesJson('*PRE');

       //=================================================================
       // Read through data based parameters
       //=================================================================
       Begsr BuildOutput;
        If ioRtnCode = 'Error';
           Leavesr;
        Endif;
        If EditFROI(decCo#:dsIn.ss#:dsIn.accDt:dsIn.initDt
         :%int(dsIn.initTime))=*off;
           Exsr Submit;
        Endif;
       Endsr;

       //=================================================================
       // Build stub results - Leave this in the code for possible future requests
       //=================================================================
       Begsr BuildStub;
        json +=
         %trim(BuildJsonNode('stub':%trim(inRequest) + ' (' +
         %trim(q1Pgm) + ') successfully executed. {SubmitFROI}'));
       Endsr;

       //=================================================================
       // Break parms out from inParms
       //=================================================================
       Begsr GetParms;
        inJson = inParms;
        Reset i0;
        node0 = yajl_string_load_tree(inJson:strErrorMsg);
        If strErrorMsg <> ' ';
           AddErrorMessageJson('JSON filter can''t be read.':ioRtnCode);
           Leavesr;
        Endif;

        json = json + ' "requestParms" : {';
        Dow YAJL_object_loop(node0:i0:key:val);
           BuildGiantJSON(json:key:YAJL_get_string(val));
           Select;
           When key = 'keyInfo';
              Reset i1;
              json = %scanrpl('""':'{':json:%len(json)-1);
              node1 = YAJL_object_find(node0:key);
              Dow YAJL_object_loop(node1:i1:key:val);
                 BuildGiantJSON(json:key:YAJL_get_string(val));
                 Select;
                 When key = 'company';
                    dsIn.co# = YAJL_get_string(val);
                 When key = 'claimantSSN';
                    dsIn.ss# = YAJL_get_string(val);
                 When key = 'accidentDate';
                    dsIn.accDt = YAJL_get_string(val);
                 When key = 'initialDate';
                    dsIn.initDt = YAJL_get_string(val);
                 When key = 'initialTime';
                    dsIn.initTime = YAJL_get_string(val);
                 Endsl;
              Enddo;
              json += '}';
           Endsl;
        Enddo;
        json = json + '},';

        If strAuthority = '*View';
           AddErrorMessageJson('User, ' + %trim(inUser) +
            ', not authorized to process ' +
            'request (' + %trim(inRequest) + ').'
            :ioRtnCode);
           Leavesr;
        Endif;

        Monitor;
           decCo# = %int(dsIn.co#);
        On-error;
        Endmon;
        Monitor;
           dt8Accident = %int(dsIn.accDt);
           dt7Accident = DtCYMD(dt8Accident);
           dtAccident = CvtToDate(dt8Accident);
           dsIn.accDt = DateConvertFROI(dsIn.accDt:'YYMD');
        On-error;
        Endmon;
        Monitor;
           dt8Initial = %int(dsIn.initDt);
           dt7Initial = DtCYMD(dt8Initial);
           dtInitial = CvtToDate(dt8Initial);
           dsIn.initDt = DateConvertFROI(dsIn.initDt:'YYMD');
        On-error;
        Endmon;
       Endsr;

       //=================================================================
       // Transfer FROI to *FIRST member
       //=================================================================
       Begsr Submit;
        Reset dsWHFRPin;
        dsWHFRPin.FRco   = decCo#;
        dsWHFRPin.FRss   = dsIn.ss#;
        dsWHFRPin.FRacDt = DateConvertFROI(dsIn.accDt:'YYMD');
        dsWHFRPin.FRiDt  = DateConvertFROI(dsIn.initDt:'YYMD');
        dsWHFRPin.FRiTm  = %int(dsIn.initTime);
        Exec sql
         select * into :dsWHFRPin
          from WHFRP_HLD
          where FRco = :dsWHFRPin.FRco and FRss = :dsWHFRPin.FRss and
           FRacDt = :dsWHFRPin.FRacDt and FRiDt = :dsWHFRPin.FRiDt and
           FRiTm = :dsWHFRPin.FRiTm;
        If not IsValidFetch(sqlstt);
           Leavesr;
        Endif;
        dsWHFRPout = dsWHFRPin;
        dsWHFRPout.FRstat = 'P';
        // Maintenance information
        dsWHFRPout.FRpgm = q1pgm;
        dsWHFRPout.FRuser = inUser;
        dsWHFRPout.FRdate = %char(%date():*iso0);
        dsWHFRPout.FRtime = sysTime6;
        Exec sql
         insert into WHFRP
          values (:dsWHFRPout);
        If not IsValidFetch(sqlstt);
           Leavesr;
        Endif;
        Exec sql
         delete
          from WHFRP_HLD
          where FRco = :dsWHFRPin.FRco and FRss = :dsWHFRPin.FRss and
           FRacDt = :dsWHFRPin.FRacDt and FRiDt = :dsWHFRPin.FRiDt and
           FRiTm = :dsWHFRPin.FRiTm;

        LogFROI(dsWHFRPout.FRCo:dsWHFRPout.FRFnd:dsWHFRPout.FREmp:
         dsWHFRPout.FRDiv:dsWHFRPout.FRSS:dsWHFRPout.FRAcdt:
         dsWHFRPout.FRdtrp:inUser:'FROI Submitted':'FROI has been submited ' +
         ' by user ' + %trim(inUser) + '.');

       Endsr;

      /End-free
     PSubmitFROI       e

