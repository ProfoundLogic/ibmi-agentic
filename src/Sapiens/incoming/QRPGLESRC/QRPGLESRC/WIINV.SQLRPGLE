     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')

     h/title WIINV - Submit Invoice Information
     h option(*nodebugio: *srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR') DECEDIT('0.')
      **********************************************************************
      *  Description:  Submit Invoice Information
      *
      *  Created: 05/07/21  C:33/62  Project: WC2663
      **********************************************************************
      * Revisions:
      *
      * 01/05/24 C:62  Ticket: 114720(129)
      *                Add processing for mass process for IVC
      *
      **********************************************************************
     d/copy sPrRunCmd
     d/copy sPrJobInfo
     d/copy sPrModule
     d/copy sPrSQL
     d/copy wPrPmtSvc
     D*** Begin Add *** 01/05/24 ****************************************
     D/Copy siWebSvcPr
     d/copy sPrdate
     D*** End   Add *** 01/05/24 ****************************************

     d sqlStmtStr      s            500a
     d jobName         s             10a   Inz('WIINV')
     d jobType         s              3a
     d jobRuntime      s              3a
     d submitJob       s            256a
     d delayTime       s            256a
     d chrCo#          s              3a
     d chrFnd          s              3a
     d chrEmp#         s              9a
     d rrnWIINV        s              9  0
     d inJson          s          32000a   Varying
     d dsWIINV       e ds                  extname(WIINV) inz qualified
     d dsWIPSP       e ds                  extname(WIPSP) inz qualified
     d dsWMEAP       e ds                  extname(WMEAP) inz qualified
     d invoice#        s             10s 0
     d chrTimeDelay    s            256a
     d timeDelay       s              5s 0
     d timeStamp       s               z
     d date            s               d   DatFmt(*usa)
     d chrDate         s             10a
     d chrTime         s              8a
     d invCount        s              5s 0
     d inzTimeStamp    s               z
     d wiinvStt        s                   Like(sqlStt)
     d wmeapStt        s                   Like(sqlStt)
     d billingExist    s               n   Inz(*Off)
     d*** Begin Add *** 01/05/24 *******************************************
     d clbBifData      s                   Sqltype(clob:64000)
     D cmdStr          s           3000    Inz varying
     D cmdLength       s             15  5 Inz
     D chrMass         s              1a   Inz(' ')
     D sysDate         s              7p 0 Inz
     D sysTime         s              6s 0 Inz

     D LodWC2663       ds                  DTAARA(LODWC2663)
     D   sweepID                     40a

      *entry
     DRequestPgm       pr                  ExtPgm('WIINV')
     D inMass                         1a   Const Options(*nopass)
     DRequestPgm       pi
     D inMass                         1a   Const Options(*nopass)
     d*** End   Add *** 01/05/24 *******************************************

      /free
        // Intialize compare time stamp
        inzTimeStamp = %TimeStamp('0001-01-01-00.00.00.000000');

        // Retrieve current job type
        jobType = GetJobType();

        // Retrieve job time delay
        chrTimeDelay = GetModuleProperty('3rd Party Payments':'SubmitDelay');
        If chrTimeDelay = ' ';
           chrTimeDelay='60';
        EndIf;

        timeDelay = %Dec(%Trim(chrTimeDelay):5:0);

        //*** Begin Add *** 01/05/24 *************************************
        // Is this being called for a mass run
        If %Parms() > 0;
           chrMass = inMass;
        EndIf;
        //*** End   Add *** 01/05/24 *************************************

        // Check to see if the job has already been submitted
        If Not ChkJobActive(jobName) And Not ChkJobJobQ(jobName);
           ExSr SubmitInvoices;
           Return;
        Else;
           If PgmInStack('W8010R');
              Return;
           EndIf;
        EndIf;

       //*** Begin Add *** 01/05/24 ***************************************
       // Is this a mass run
       If chrMass = 'Y';
          // Create Sweep Data Area
          cmdStr = 'CRTDTAARA DTAARA(QTEMP/LODWC2663) TYPE(*CHAR) LEN(40) ' +
                   'VALUE('' '')';
          cmdLength = %Len(cmdStr);
          runCmd(cmdStr:cmdLength);

          sysDate = DateTo7(systemDate());
          sysTime = TimeTo6(systemTime());

          // Populagte the sweep ID
          In *Lock LodWC2663;
          sweepID = 'SWP' + %EditC(sysDate:'X') + %EditC(sysTime:'X');
          Out LodWC2663;

          // Send End Sweep
          clbBifData_Data = '{"sweepID":"' + %Trim(sweepID) +
                         '","transaction":"sweep started"}';
          clbBifData_Len  = %Len(%Trim(clbBifData_Data));
          LoadSIWEBSVCDataQ(clbBifData);
       EndIf;
       //*** End   Add *** 01/05/24 **************************************

        sqlStmtStr = 'Select wiinv.*, rrn(wiinv) From WIINV';

        Exec SQL Close cursorWIINV;
        Exec Sql Prepare sqlWIINV From :sqlStmtStr;
        Exec SQL Declare cursorWiInv Cursor For sqlWIINV;
        Exec SQL Open cursorWIINV;

        Dou Not IsValidFetch(wiinvStt);
           Exec SQL fetch next from cursorWiInv into :dsWIINV, :rrnWIINV;
           wiinvStt = sqlStt;
           If IsValidFetch(wiinvStt);
              // Retrieve Quote Transfer flag and 3rd Party Update
              chrCo#  = %EditC(dsWiInv.INco#:'X');
              chrFnd  = %EditC(dsWiInv.INfnd:'X');
              chrEmp# = %EditC(dsWiInv.INemp#:'X');
              Clear dsWIPSP;
              Exec Sql Select * Into :dsWIPSP from wipsp Where
                 PSco# = :chrCo# And PSfnd = :chrFnd And PSemp# = :chrEmp#;
              // Only process entries if marked for 3rd Party Quote Or
              // 3rd Party Updates
              If SendInvoice(dsWiPsP.PSqteXfer:dsWiPsP.PS3ptyUpd);
                 // Process A/R
                 ExSr ProcessAR;
                 // Delete WIINV record
                 Exec Sql Delete from WIINV Where rrn(WIINV) = :rrnWIINV;
                 // Finalize 3rd Party Invoicing
                 FinalizeThirdPartyInvoicing(dsWIINV.INco#:dsWIINV.INfnd);
              EndIf;
           Endif;
        Enddo;

       //*** Begin Add *** 01/05/24 ***************************************
       // Is this a mass run
       If chrMass = 'Y';
          // Send End Sweep
          clbBifData_Data = '{"sweepID":"' + %Trim(sweepID) +
                            '","transaction":"sweep completed"}';
          clbBifData_Len  = %Len(%Trim(clbBifData_Data));
          LoadSIWEBSVCDataQ(clbBifData);

          chrMass = ' ';
       EndIf;
       //*** End   Add *** 01/05/24 ***************************************

        // Check to see if any invoices left and if so then resubmit job
        Exec Sql Select count(*) Into :invCount From wiinv;

        If invCount <> 0;
           ExSr SubmitInvoices;
        EndIf;

        *inLR = *On;

        //==================================================================
        // ProcessAR - Read Thru A/R records for the given Employer
        //==================================================================
        BegSR ProcessAR;

           // read Thru WMEAP

           sqlStmtStr = 'Select EAninv ' +
              'From wmeap ' +
              'Where '  +
              'EAco#='  + %EditC(dsWIINV.INco#:'X')  + ' And ' +
              'EAfnd='  + %EditC(dsWIINV.INfnd:'X')  + ' And ' +
              'EAemp#=' + %EditC(dsWIINV.INemp#:'X') + ' And ' +
              'EAdiv='  + %EditC(dsWIINV.INdiv:'X')  + ' And ' +
              'EAfyr='  + %EditC(dsWIINV.INfyr:'X')  + ' '     +
              'Group by EAninv '  +
              'Order by EAninv ';
           Exec SQL Close cursorWMEAP;
           Exec Sql Prepare sqlWMEAP From :sqlStmtStr;
           Exec SQL Declare cursorWMEAP Cursor For sqlWMEAP;
           Exec SQL Open cursorWMEAP;

           Dou Not IsValidFetch(wmeapStt);
              Exec SQL fetch next from cursorWMEAP into :invoice#;
              wmeapStt = sqlStt;
              If IsValidFetch(wmeapStt);
                 // Send Invoice# to ThirdPartyInvoicing
                 If invoice# <> 0 And BillingExists(dsWIINV.INco#:
                    dsWIINV.INfnd:dsWIINV.INemp#:dsWIINV.INdiv:invoice#);
                    inJson = '{' +
                        '"type":"billing",' +
                        '"invoice":"'  + %EditC(invoice#:'X') + '",' +
                        '"company":"'  + %EditC(dsWIINV.INco#:'X') + '",'  +
                        '"group":"'    + %EditC(dsWIINV.INfnd:'X') + '",'  +
                        '"employer":"' + %EditC(dsWIINV.INemp#:'X') + '",' +
                        '"division":"' + %EditC(dsWIINV.INdiv:'X') + '",'  +
                        '"active":"true"' +
                        '}';
                    ThirdPartyInvoicing(inJson);
                 EndIf;
              Endif;
           Enddo;
           Exec SQL Close cursorWMEAP;

        EndSR;

        //==================================================================
        // SubmitInvoices - Submits Invoices in the future
        //==================================================================
        BegSR SubmitInvoices;

           If Not ChkJobJobQ(jobName);

              // Create job schedule time
              timeStamp = %TimeStamp() + %Minutes(timeDelay);
              chrDate   = %Char(%Date(timestamp):*usa);
              chrTime   = %Char(%Time(timestamp):*hms);

              // Submit invoice job
              //*** Begin Chg *** 01/05/24 *********************************
              //*** SubmitJob = 'SBMJOB CMD(CALL PGM(WIINV)) ' +
              //***             'JOB(WIINV) JOBD(WUSERJ) SCDDATE(''' +
              //***          chrDate + ''') SCDTIME(''' + chrTime + ''') ' +
              //***            'MSGQ(*NONE)';
              SubmitJob = 'SBMJOB CMD(CALL PGM(WIINV) ' +
                          'PARM((''' + CHRMASS + '''))) ' +
                          'JOB(WIINV) JOBD(WUSERJ) SCDDATE(''' +
                           chrDate + ''') SCDTIME(''' + chrTime + ''') ' +
                          'MSGQ(*NONE)';
              //*** End   Chg *** 01/05/24 *********************************

              RunCmd(%Trim(submitJob):%Len(%Trim(submitJob)));

           EndIf;

        EndSR;

      /end-free
       //===================================================================
       // BillingExists - See if any billing record
       //===================================================================
     pBillingExists    b
     dBillingExists    pi              n
     d inCompany#                     3s 0
     d inGroup#                       3s 0
     d inEmployer#                    9s 0
     d inDivision                     5s 0
     d inInvoice#                    10s 0

     d hldStt          s                   Like(sqlStt)
     d billcount       s              5  0

      /free

       hldStt = sqlStt;

       Exec Sql Select Count(*) Into :billCount
                From WMEAP
                Where EAco#=:inCompany# And EAfnd=:inGroup# And
                      EAemp#=:inEmployer# And EAdiv=:inDivision And
                      EAninv=:inInvoice# And
                      EAarTy in ('A','B','D','F','H');

       sqlStt =hldStt;

       If billCount > 0;
          Return *On;
       Else;
          Return *Off;
       EndIf;

      /End-free

     pBillingExists    e
       //===================================================================
       // SendInvoice - See if this policies invoices can be sent to IC
       //===================================================================
     pSendInvoice      b
     dSendInvoice      pi              n
     d inQteXfer                       z
     d in3PtyUpd                       z

     d qteXferLapse    s               z
     d tPtyUpdLapse    s               z

      /free

       // Populate Invoice Lapse TimeStamps
       qteXferLapse = inQteXfer + %Minutes(15);
       tPtyUpdLapse = in3PtyUpd + %Minutes(15);

       // Process invoice since quote was not reported to IC
       If inQteXfer = inzTimeStamp And in3ptyUpd = inzTimeStamp;
          Return *On;
       EndIf;

       // Process invoice since enough time has passed
       If inQteXfer <> inzTimeStamp Or in3PtyUpd <> inzTimeStamp;
          If %TimeStamp >= qteXferLapse And %TimeStamp >= tPtyUpdLapse;
             Return *On;
          EndIf;
       EndIf;

       Return *Off;

      /End-free

     pSendInvoice      e
