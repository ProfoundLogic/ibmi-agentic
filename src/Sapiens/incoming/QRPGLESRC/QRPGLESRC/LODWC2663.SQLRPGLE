     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H dftactgrp(*no)
     //********************************************************************
     /TITLE LODWC2663 - Create/send initial invoice transactions
     //********************************************************************
     // Created: 05/23/21    C:62    Project: WC2663                      *
     //********************************************************************
      // Revisions:                                                        *
      //                                                                   *
      // 09/03/24 C:291 Project: WC2968                                    *
      //                Activate IVCNAMADDR table value.                   *
      //                                                                   *
      // 12/19/23 C:340 Project: WC2935                                    *
      //                Policy Rule - Failed BIF Notification              *
      //                                                                   *
      // 10/25/23 C:340 Project: WC2922                                    *
      //                When Swap is run a policy/division which is        *
      //                flagged as contested audit do not send any Audit   *
      //                invoice information to IVC.                        *
      //                                                                   *
      // 07/07/23  C:062 Project: WC2916                                   *
      //                 -Add tag to bif to indicate if part of sweep      *
      //                 -Send end sweep json to indiate the sweep ended   *
      //                 -Set group flag to indicate the sweep ran or not  *
      //                                                                   *
      // 10/10/22  C:062 Project: WC2663 task 103873                       *
      //                 -Fix various issues                               *
      //                                                                   *
      // 09/29/22  C:340 Support: 103733                                   *
      //                 Add InvoiceTypeID to existing A/R Transactions    *
      //                                                                   *
      // 10/19/21  C:33  Project: WC2663 #98816                            *
      //                 Activate 3PTYPMTCX                                *
      //********************************************************************
     d sqlStmtStr      s           1000a
     d inJson          s          32000a   varying
      //***  Beg Add  ***  09/29/22  *******************
     D
     D w@Today         S              8S 0
     D w@BnkAcc        S             20A
     D w@InvTypId      S             15S 0
     D
     D @@co            s              3
     D @@fnd           s              3
     D @@appl          s              2
     D @@srdt          s              8
     D @@call          s              1
     D @@hdcy          s              1
     D @@chkq          s             10
     D @@ftyp          s             10
     D @@efdt          s              7
     D @@cpyq          s             10
     D @@cktp          s             10
     D @@eftc          s              1
     D @@coid          s             20
     D @@rtcd          s              1
     D
      //***  End Add  ***  09/29/22  *******************
     d dsWDELP         ds                  qualified inz
     d  ELco#                         3  0
     d  ELfnd                         3  0
     d  ELemp#                        9  0
     d  ELdiv                         5  0

     D psds          esds                  extname(szq1p)

     D*** Begin Add *** 07/07/23 *******************************************
     d clbBifData      s                   Sqltype(clob:64000)

     D LodWC2663       ds                  DTAARA(LODWC2663)
     D   sweepID                     40a
     D*** End   Add *** 07/07/23 *******************************************

     d intCo#          s              3  0
     d sysTime         s              6  0
     d sysDate         s              7  0
     d countAR         s              5  0
     d countSR         s              5  0
     d invoice#        s             10  0
     d inKey           s             20a
     d isSelfReport    s               n
     d hldCo#          s              3  0
     d hldFnd          s              3  0
     d hldEmp#         s              9  0
     d hldDiv          s              5  0
     d hldInv#         s             10  0
     d hldPoDt         s              7  0
     d ELco#           s              3  0
     d ELfnd           s              3  0
     d ELemp#          s              9  0
     d ELdiv           s              5  0
     d EAninv          s             10  0
     d postDate        s              7  0
     d weGotAHit       s               n
     D*** Beg Add ***  10/25/23  *** WC2922 ********************************
     d isContested     S              1A   Inz(' ')
     d AuditedFyr      S              3  0
     d hldSqlStt       S                   Like(sqlStt)
     D*** Beg Add ***  10/25/23  *** WC2922 ********************************

     d LodHubPrc       pr                  ExtPgm('LODHUBPRC')
     d FixCgiObj       pr                  ExtPgm('FIXCGIOBJ')
     d LodSysGen       pr                  ExtPgm('LODSYSGEN')

     D*** Begin Add *** 07/07/23 *******************************************
     D/Copy siWebSvcPr
     D*** End   Add *** 07/07/23 *******************************************
     D/copy sPrDate
     D/copy sPrSql
     d/copy sprModule
     d/copy wprAR
     d/copy wprPmtSvc
     d/copy wprUsrPrms
      //***  Beg Add  ***  09/29/22  *******************
     D/Copy wPrBank
      //***  End Add  ***  09/29/22  *******************

      /Free
       Exec Sql Set Option Commit = *None;
       sysTime = TimeTo6(SystemTime);
       sysDate = DateTo7(SystemDate);
       intCo#  = UserCo#(q1user);
       //*** Begin Add *** 07/07/23 ****************************************
       // Set the Sweep ID
       In *Lock LodWC2663;
       sweepID = 'SWP' + %EditC(DateTo8(SystemDate):'X') +
                         %EditC(sysTime:'X');
       Out LodWC2663;
       //*** End   Add *** 07/07/23 ****************************************

       // Populate HUB Processor
       LodHubPrc();

       // Set up CGI environment
       FixCgiObj();

       // Update system generic table file
       LodSysgen();

       // Now activate the Invice cloud values
       Exec Sql Update STGTP Set GTstat='A'
            Where GTtbid='AXKY' And GTkey in ('QTE3PRTPMT', '3PRTPMTDIS');
       //***  Begin Add  ***  10/19/21  ***********
       //*** Beg Chg *** 12/19/23 *** WC2935 ***************************
       // Exec sql update STGTP set GTstat='A'
       //  where GTtbId='AXKY' And GTkey in ('3PRTPMTCX');
       //*** Beg Chg ***** 09/03/24 ****************************************
       //***Exec SQL update STGTP set GTstat='A'
       //***     where GTtbId='AXKY' And GTkey in ('3PRTPMTCX', 'BIFFAILURE');
       Exec SQL update STGTP set GTstat='A'
            where GTtbId = 'AXKY' And
            GTkey in ('3PRTPMTCX', 'BIFFAILURE', 'IVCNAMADDR');
       //*** End Chg ***** 09/03/24 ****************************************
       //*** End Chg *** 12/19/23 *** WC2935 ***************************
       //***  End   Add  ***  10/19/21  ***********

       // Set Module Property to 3 minutes
       SetModuleProperty('3rd Party Payments':'SubmitDelay':'3');

       // Update WMEAP with invoice numbers, only done to current/future
       ExSr ProcessAR;

       // Report Invoices to 3rd Party
       ExSr ReportInvoice;

       // Update WMXAP with invoice numbers
       ExSr UpdateWMXAP;

       // Set Module Property to 15 minutes
       SetModuleProperty('3rd Party Payments':'SubmitDelay':'15');

       *inlr = *on;
       //===================================================================
       BegSr ProcessAR;  // Assign Invoice # to AR
       //===================================================================

          sqlStmtStr = 'Select ELco#, ELfnd, ELemp#, ELdiv from wdelp ' +
                       'Group by ELco#, ELfnd, ELemp#, ELdiv ' +
                       'Order by ELco#, ELfnd, ELemp#, ELdiv';

          Exec Sql Prepare sqlWDELP From :sqlStmtStr;
          Exec SQL Declare cursorWDELP Cursor For sqlWDELP;
          Exec SQL Open cursorWDELP;

          Dou Not IsValidFetch(sqlStt);
             Exec SQL fetch next from cursorWDELP into :dsWDELP;
             If IsValidFetch(sqlStt);
                // Read thru WMEAP if current or future dated
                ExSr ReadWMEAP;
             EndIf;
          EndDo;
          Exec SQL Close cursorWDELP;
          sqlStt  = '00000';

       EndSr;

       //===================================================================
       BegSr ReadWMEAP;  // Reads thru WMEAP if WMEAP current/future dated
       //===================================================================

          // Load values in work fields
          ELco#  = dsWDELP.ELco#;
          ELfnd  = dsWDELP.ELfnd;
          ELemp# = dsWDELP.ELemp#;
          ELdiv  = dsWDELP.ELdiv;

          // Is this self reporting  policy
          Exec sql
            select count(*) into :countSR
            from WASRP
            where SRco# = :ELco# and SRfnd = :ELfnd and
               SRemp# = :ELemp# and SRdiv = :ELdiv;

           isSelfReport = *Off;
           If countSR <> 0;
              isSelfReport = *On;
           EndIf;

          // Are there any WMEAP records that need processing
          If isSelfReport;
             Exec Sql Select count(*) into :countAR from wmeap
                Where EAco#=:ELco# And EAfnd=:ELfnd And EAemp#=:ELemp# And
                EApoDt >= :sysDate And EAarTy in ('A', 'D', 'H');
          Else;
             Exec Sql Select count(*) into :countAR from wmeap
                Where EAco#=:ELco# And EAfnd=:ELfnd And EAemp#=:ELemp# And
                EApoDt >= :sysDate And EAarTy in ('A', 'B', 'D', 'F', 'H');
          EndIf;

          If countAR > 0;

             sqlStmtStr = 'Select EApoDt ' +
                          'from wmeap ' +
                          'Where EAco# = ' + %EditC(ELco#:'X') + ' And ' +
                          'EAfnd = ' + %EditC(ELfnd:'X') + ' And ' +
                          'EAemp# = ' + %EditC(ELemp#:'X') + ' And ' +
                          'EAdiv = ' + %EditC(ELdiv:'X') + ' And ' +
                          'EApoDt >= ' + %EditC(sysDate:'X') + ' And ' +
                          'EAarTy in (''A'', ''B'', ''D'', ''F'', ''H'') ' +
                          'Group by EApoDt ' +
                          'Order by EApoDt';

             Exec Sql Prepare sqlWMEAP From :sqlStmtStr;
             Exec SQL Declare cursorWMEAP Cursor For sqlWMEAP;
             Exec SQL Open cursorWMEAP;

             Dou Not IsValidFetch(sqlStt);
                Exec SQL fetch next from cursorWMEAP into :postDate;
                If IsValidFetch(sqlStt);
                   // get invoice #
                   If hldPoDt <> postDate;
                      // See if the posting date entries has an invoice
                      invoice# = 0;
                      //*** Begin Chg *** 10/10/22 *************************
                      //Exec Sql Select Max(EAnInv) Into invoice# From WMEAP
                      //***    Where EAco#=:ELco# And EAfnd=:ELfnd And
                      //***    EAemp#=:ELemp# And EAdiv=:ELdiv and
                      //***    EApodt=:postDate;
                      Exec Sql Select Max(EAnInv) Into :invoice# From WMEAP
                               Where EAco#=:ELco# And EAfnd=:ELfnd And
                               EAemp#=:ELemp# And EAdiv=:ELdiv and
                               EApodt=:postDate;
                      //*** End   Chg *** 10/10/22 *************************
                      sqlStt  = '00000';
                      // No invoice# assigned so let's get one
                      If invoice# = 0;
                         inKey = %EditC(ELco#:'X') + %EditC(ELfnd:'X') +
                                 %EditC(ELemp#:'X') + %EditC(ELdiv:'X');
                         invoice# = GetNext3rdPartySeq#(inKey);
                      EndIf;
                      // Store new posting date
                      hldPoDt  = postDate;
                   EndIf;
      //***  Beg Add  ***  09/29/22  *******************
      // Get Invoice Type ID

                   @@co   = %EditC(ELco#:'X');
                   @@fnd  = %EditC(ELfnd:'X');
                   @@appl = 'WP';
                   @@srDt = %EditC(DateTo8(systemDate()):'X');
                   @@call = *Blanks;
                   @@hdcy = *Blanks;
                   @@chkq = *Blanks;
                   @@ftyp = *Blanks;
                   @@efdt = *Zeros ;
                   @@cpyq = *Zeros ;
                   @@cktp = *Blanks;
                   @@eftc = '*';
                   @@coid = *Blanks;
                   @@rtcd = *Blanks;

                   w@BnkAcc = GetBankAcc(@@co : @@fnd: @@appl: @@srdt: @@call:
                              @@hdcy: @@chkq: @@ftyp: @@efdt: @@cpyq:
                              @@cktp: @@eftc: @@coid: @@rtcd);

                   Exec Sql
                     Select CkInvTypId Into :w@InvTypId
                     From   WSCKP
                     Where  CKCO# = :intCo# and CKACCT = :w@BnkAcc;

      //***  End Add  ***  09/29/22  *******************
      //***  Beg Chg  ***  09/29/22  *******************
      //***        // Update invoice#
      //***        If isSelfReport;
      //***           Exec Sql Update wmeap set eaninv = :invoice#
      //***             Where EAco# = :ELco# And EAfnd = :ELfnd And
      //***                   EAemp# = :ELemp# And EAdiv = :ELdiv And
      //***                   EApodt >= :postDate And
      //***                   EAarTy in ('A', 'D', 'H');
      //***        Else;
      //***           Exec Sql Update wmeap set eaninv = :invoice#
      //***             Where EAco# = :ELco# And EAfnd = :ELfnd And
      //***                   EAemp# = :ELemp# And EAdiv = :ELdiv And
      //***                   EApodt >= :postDate And
      //***                   EAarTy in ('A', 'B', 'D', 'F', 'H');
                   If isSelfReport;
                      Exec Sql Update wmeap set eaninv = :invoice# ,
                                            EAInvTypID = :w@InvTypId
                        Where EAco# = :ELco# And EAfnd = :ELfnd And
                              EAemp# = :ELemp# And EAdiv = :ELdiv And
                              EApodt = :postDate And
                              EAarTy in ('A', 'D', 'H');
                   Else;
                      Exec Sql Update wmeap set eaninv = :invoice# ,
                                            EAInvTypID = :w@InvTypId
                        Where EAco# = :ELco# And EAfnd = :ELfnd And
                              EAemp# = :ELemp# And EAdiv = :ELdiv And
                              EApodt = :postDate And
                              EAarTy in ('A', 'B', 'D', 'F', 'H');
      //***  End Chg  ***  09/29/22  *******************
                   EndIf;
                EndIf;
             EndDo;
             hldPoDt = 0;
             Exec SQL Close cursorWMEAP;
          EndIf;

       EndSr;

       //===================================================================
       BegSr ReportInvoice;  // Report invoices to Third Party
       //===================================================================

         //*** Begin Add *** 07/07/23 **************************************
         // Send End Sweep
         clbBifData_Data = '{"sweepID":"' + %Trim(sweepID) +
                           '","transaction":"sweep started"}';
         clbBifData_Len  = %Len(%Trim(clbBifData_Data));
         LoadSIWEBSVCDataQ(clbBifData);
          //*** Begin Chg *** 07/07/23 *************************************
          //*** sqlStmtStr = 'Select EAco#, EAfnd, EAemp#, EAdiv, eaninv ' +
          //***             'from wmeap ' +
          //***             'Where ' +
          //***        //*** Begin Chg *** 10/10/22 ************************
          //***        //*** 'eaninv <> 0 ' +
          //***             'EAninv <> 0 And ' +
          //***             'EApodt >= ' + %EditC(sysDate:'X') + ' ' +
          //***        //*** End   Add *** 10/10/22 ************************
          //***             'Group by EAco#, EAfnd, EAemp#, EAdiv, eaninv '+
          //***             'Order by EAco#, EAfnd, EAemp#, EAdiv, eaninv';
          sqlStmtStr = 'Select EAco#, EAfnd, EAemp#, EAdiv, eaninv ' +
                       'from wmeap Join wmexp On EAco#=EXco# And ' +
                                                'EAfnd=EXfnd ' +
                       'Where ' +
                       'EAninv <> 0 And EXswpRan <> ''Y'' And ' +
                       'EApodt >= ' + %EditC(sysDate:'X') + ' ' +
                       'Group by EAco#, EAfnd, EAemp#, EAdiv, eaninv ' +
                       'Order by EAco#, EAfnd, EAemp#, EAdiv, eaninv';
          //*** End   Chg *** 07/07/23 *************************************

         Exec Sql Prepare sqlWMEAP2 From :sqlStmtStr;
         Exec SQL Declare cursorWMEAP2 Cursor For sqlWMEAP2;
         Exec SQL Open cursorWMEAP2;

         Dou Not IsValidFetch(sqlStt);
            Exec SQL fetch next from cursorWMEAP2 into :ELco#, :ELfnd,
                 :ELemp#, :ELdiv, :EAninv;
            //*** Beg Add ***  10/25/23  *** WC2922 ***************
            hldSqlStt = sqlStt;
            //*** End Add ***  10/25/23  *** WC2922 ***************
            If IsValidFetch(sqlStt);
               If hldCo# <> ELco# Or  hldfnd <> ELfnd Or
                  hldEmp# <> ELemp# Or hldDiv <> ELdiv Or
                  hldInv# <> EAninv;

                  // If employer changes then finalize 3rd party invoicing
                  If (hldCo# <> 0 And hldFnd <> 0 And hldEmp# <> 0) And
                     (hldCo# <> ELco# Or  hldfnd <> ELfnd Or
                      hldEmp# <> ELemp#);
                     FinalizeThirdPartyInvoicing(hldCo#:hldFnd);
                     //*** Begin Add *** 07/07/23 **************************
                     // Set IVC Sweep Ran Flag
                     If hldFnd <> ELfnd;
                        Exec Sql Update WMEXP
                                 Set EXswpRan = 'Y'
                                 Where Exfnd = :hldFnd;
                     EndIf;
                     //*** End   Add *** 07/07/23 **************************
                  EndIf;

                  // create 3rd party Json
                  inJson = '{' +
                           '"type":"billing",' +
                           '"invoice":"'  + %EditC(EAninv:'X') + '",' +
                           //*** Begin Chg *** 10/10/22 ********************
                           //***'"company":" ' + %EditC(ELCo#:'X')  + '",' +
                           '"company":"' + %EditC(ELCo#:'X')  + '",' +
                           //*** End   Chg *** 10/10/22 ********************
                           '"group":"'    + %EditC(ELfnd:'X')  + '",' +
                           '"employer":"' + %EditC(ELemp#:'X') + '",' +
                           //*** Begin Chg *** 10/10/22 ********************
                           //***'"division":"' + %EditC(ELdiv:'X')  + '"}' +
                           '"division":"' + %EditC(ELdiv:'X')  + '"}';
                           //*** End   Chg *** 10/10/22 ********************

                  //*** Beg Add ***  10/25/23  *** WC2922 ***************
                  //Check If current invoice being processed is for an audit.
                  Exec Sql Select EAFYR into :AuditedFyr
                           From WMEAP
                           Where EACO# = :ELCo# and EAFND = :ELfnd and
                                 EAEMP# = :ELemp# and EADIV = :ELdiv and
                                 EANINV = :EAninv and EAARTY = 'D';

                  isContested = ' ';
                  If EAninv <> 0 and SQLCOD = 0;
                     Exec Sql Select ELCTST into :isContested
                              From WDELL
                              Where ELCO# = :ELCo# and ELFND = :ELfnd and
                                    ELEMP# = :ELemp# and ELDIV = :ELdiv and
                                    ELFYR  = :AuditedFyr;
                  EndIf;
                  sqlStt = hldSqlStt;
                  //*** End Add ***  10/25/23  *** WC2922 ***************

                  // Generate 3rd Party Invicing
                  //*** Beg Chg ***  10/25/23  *** WC2922 ***************
                  //ThirdPartyInvoicing(inJson);
                  If isContested = 'Y';
                     ReplaceHeldInvoices(ELCo#:ELfnd:ELemp#:ELdiv:AuditedFyr:
                                         EAninv:'A');
                  Else;
                     ThirdPartyInvoicing(inJson);
                     weGotAHit = *On;
                  EndIf;
                  //*** End Chg ***  10/25/23  *** WC2922 ***************

                  // Populate Hoding fields
                  hldCo#  = ELco#;
                  hldFnd  = ELfnd;
                  hldEmp# = ELemp#;
                  hldDiv  = ELdiv;
                  hldInv# = EAninv;
                  //*** Beg Del ***  10/25/23  *** WC2922 ***************
                  //*** weGotAHit = *On;
                  //*** Beg dDl ***  10/25/23  *** WC2922 ***************
               EndIf;
            Else;
               If weGotAHit;
                  FinalizeThirdPartyInvoicing(ELfnd:ELfnd);
                  //*** Begin Add *** 07/07/23 **************************
                  //*** Begin Del *** 10/25/23 *** WC2922 ***************
                  // Set IVC Sweep Ran Flag
                  // Exec Sql Update WMEXP
                  //         Set EXswpRan = 'Y'
                  //         Where Exfnd = :ELfnd;
                  // Leave;
                  //*** End   Del *** 10/25/23 *** WC2922 ***************
                  //*** End   Add *** 07/07/23 **************************
               EndIf;
               //*** Beg Add *** 10/25/23 *** WC2922 ***************
               // Set IVC Sweep Ran Flag
               Exec Sql Update WMEXP
                        Set EXswpRan = 'Y'
                        Where Exfnd = :ELfnd;
               Leave;
               //*** End Add *** 10/25/23 *** WC2922 ***************
            EndIf;
         EndDo;
         //*** Begin Add *** 10/10/22 **************************************
         Exec SQL Close cursorWMEAP2;
         //*** End   Add *** 10/10/22 **************************************
         //*** Begin Add *** 07/07/23 **************************************
         // Send End Sweep
         clbBifData_Data = '{"sweepID":"' + %Trim(sweepID) +
                           '","transaction":"sweep completed"}';
         clbBifData_Len  = %Len(%Trim(clbBifData_Data));
         LoadSIWEBSVCDataQ(clbBifData);

         // Clear Sweep ID
         In *Lock LodWC2663;
         sweepID = ' ';
         Out LodWC2663;
         //*** End   Add *** 07/07/23 **************************************

       EndSr;

       //===================================================================
       BegSr UpdateWMXAP;  // Assign Invoice # to WMXAP
       //===================================================================

          sqlStmtStr = 'Select XAco#, XAfnd, XAemp#, XAdiv, XApoDt ' +
                       'from wmxap ' +
                       'where XAarTy in (''A'', ''B'', ''D'', ''F'', ''H'') ' +
                       'Group by XAco#, XAfnd, XAemp#, XAdiv, XApoDt ' +
                       'Order by XAco#, XAfnd, XAemp#, XAdiv, XApoDt';

          Exec Sql Prepare sqlWMXAP From :sqlStmtStr;
          Exec SQL Declare cursorWMXAP Cursor For sqlWMXAP;
          Exec SQL Open cursorWMXAP;

          Dou Not IsValidFetch(sqlStt);
             Exec SQL fetch next from cursorWMXAP
             into :ELco#, :ELfnd, :ELemp#, :ELdiv, :postDate;
             If IsValidFetch(sqlStt);
                // get invoice #
                If hldPoDt <> postDate;
                   // See if the posting date entries has an invoice
                   //*** Begin Chg *** 10/10/22 ****************************
                   //***Exec Sql Select Max(XAnInv) Into invoice# From WMXAP
                   //***    Where XAco#=:ELco# And XAfnd=:ELfnd And
                   //***    XAemp#=:ELemp# And XAdiv=:ELdiv and
                   //***    XApodt=:postDate;
                   Exec Sql Select Max(XAnInv) Into :invoice# From WMXAP
                            Where XAco#=:ELco# And XAfnd=:ELfnd And
                            XAemp#=:ELemp# And XAdiv=:ELdiv and
                            XApodt=:postDate;
                   //*** End   Chg *** 10/10/22 ****************************
                   sqlStt  = '00000';
                   // No invoice# assigned so let's get one
                   If invoice# = 0;
                      // Assign Invoice #
                      inKey = %EditC(ELco#:'X') + %EditC(ELfnd:'X') +
                              %EditC(ELemp#:'X') + %EditC(ELdiv:'X');
                      invoice# = GetNext3rdPartySeq#(inKey);
                   EndIf;
                   hldPoDt = postDate;
                EndIf;

                Exec Sql Update WMXAP set XAninv = :invoice#
                   Where XAco# = : ELco# And XAfnd = : ELfnd And
                         XAemp# = : ELemp# And XAdiv = : ELdiv And
                         XApodt = :postDate And
                         XAarTy in ('A', 'B', 'D', 'F', 'H');
             EndIf;
          EndDo;
          Exec SQL Close cursorWMXAP;

       EndSr;

