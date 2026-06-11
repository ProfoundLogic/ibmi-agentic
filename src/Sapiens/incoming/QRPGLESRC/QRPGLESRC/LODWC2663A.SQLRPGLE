     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H dftactgrp(*no)
     //********************************************************************
     /TITLE LODWC2663 - Create/send initial invoice transactions
     //********************************************************************
     // Created: 05/23/21    C:62    Project: WC2663                      *
     //********************************************************************
      // Revisions:                                                        *
      //                                                                   *
      // 11/15/22  C:079 Project: WC2663 task 104269                       *
      //                 -Fix various issues from 103873                   *
      //                                                                   *
      //********************************************************************
     d sqlStmtStr      s           1000a
     d inJson          s          32000a   varying

     d dsWDELP         ds                  qualified inz
     d  ELco#                         3  0
     d  ELfnd                         3  0
     d  ELemp#                        9  0
     d  ELdiv                         5  0

     D psds          esds                  extname(szq1p)

     d intCo#          s              3  0
     d sysTime         s              6  0
     d sysDate         s              7  0
     d countAR         s              5  0
     d countSR         s              5  0
     d invoice#        s             10  0
     d inKey           s             20a
     d isSelfReport    s               n
     d hldCo#          s              3  0 Inz
     d hldFnd          s              3  0 Inz
     d hldEmp#         s              9  0 Inz
     d hldDiv          s              5  0 Inz
     d hldInv#         s             10  0 Inz
     d hldPoDt         s              7  0
     d ELco#           s              3  0
     d ELfnd           s              3  0
     d ELemp#          s              9  0
     d ELdiv           s              5  0
     d EAninv          s             10  0
     d postDate        s              7  0
     d weGotAHit       s               n

     d LodHubPrc       pr                  ExtPgm('LODHUBPRC')
     d FixCgiObj       pr                  ExtPgm('FIXCGIOBJ')
     d LodSysGen       pr                  ExtPgm('LODSYSGEN')

     D/copy sPrDate
     D/copy sPrSql
     d/copy sprModule
     d/copy wprAR
     d/copy wprPmtSvc
     d/copy wprUsrPrms

      /Free
       sysTime = TimeTo6(SystemTime);
       sysDate = DateTo7(SystemDate);
       intCo#  = UserCo#(q1user);

       // Update WMEAP with invoice numbers, only done to current/future
       ExSr ProcessAR;

       // Report Invoices to 3rd Party
       ExSr ReportInvoice;

       // Update WMXAP with invoice numbers
       ExSr UpdateWMXAP;

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
                      //*** Begin Chg *** 11/15/22 *************************
                      //Exec Sql Select Max(EAnInv) Into invoice# From WMEAP
                      //***    Where EAco#=:ELco# And EAfnd=:ELfnd And
                      //***    EAemp#=:ELemp# And EAdiv=:ELdiv and
                      //***    EApodt=:postDate;
                      Exec Sql Select Max(EAnInv) Into :invoice# From WMEAP
                               Where EAco#=:ELco# And EAfnd=:ELfnd And
                               EAemp#=:ELemp# And EAdiv=:ELdiv and
                               EApodt=:postDate;
                      //*** End   Chg *** 11/15/22 *************************
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
                   // Update invoice#
                   If isSelfReport;
                      Exec Sql Update wmeap set eaninv = :invoice#
                        Where EAco# = :ELco# And EAfnd = :ELfnd And
                              EAemp# = :ELemp# And EAdiv = :ELdiv And
                              EApodt = :postDate And
                              EAarTy in ('A', 'D', 'H');
                   Else;
                      Exec Sql Update wmeap set eaninv = :invoice#
                        Where EAco# = :ELco# And EAfnd = :ELfnd And
                              EAemp# = :ELemp# And EAdiv = :ELdiv And
                              EApodt = :postDate And
                              EAarTy in ('A', 'B', 'D', 'F', 'H');
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

          sqlStmtStr = 'Select EAco#, EAfnd, EAemp#, EAdiv, eaninv ' +
                       'from wmeap ' +
                       'Where ' +
                       //*** Begin Chg *** 11/15/22 ************************
                       //***'eaninv <> 0 ' +
                       'EAninv <> 0 And ' +
                       'EApodt >= ' + %EditC(sysDate:'X') + ' ' +
                       //*** End   Chg *** 11/15/22 ************************
                       'Group by EAco#, EAfnd, EAemp#, EAdiv, eaninv ' +
                       'Order by EAco#, EAfnd, EAemp#, EAdiv, eaninv';

         Exec Sql Prepare sqlWMEAP2 From :sqlStmtStr;
         Exec SQL Declare cursorWMEAP2 Cursor For sqlWMEAP2;
         Exec SQL Open cursorWMEAP2;

         Dou Not IsValidFetch(sqlStt);
            Exec SQL fetch next from cursorWMEAP2 into :ELco#, :ELfnd,
                 :ELemp#, :ELdiv, :EAninv;
            If IsValidFetch(sqlStt);
               If hldCo# <> ELco# Or  hldfnd <> ELfnd Or
                  hldEmp# <> ELemp# Or hldDiv <> ELdiv Or
                  hldInv# <> EAninv;

                  // If employer changes then finalize 3rd party invoicing
                  If (hldCo# <> 0 And hldFnd <> 0 And hldEmp# <> 0) And
                     (hldCo# <> ELco# Or  hldfnd <> ELfnd Or
                      hldEmp# <> ELemp# Or hldDiv <> ELdiv);
                     FinalizeThirdPartyInvoicing(hldCo#:hldFnd);
                  EndIf;

                  // create 3rd party Json
                  inJson = '{' +
                           '"type":"billing",' +
                           '"invoice":"'  + %EditC(EAninv:'X') + '",' +
                           //*** Begin Chg *** 11/15/22 ********************
                           //***'"company":"'  + %EditC(ELCo#:'X')  + '",' +
                           '"company":"' + %EditC(ELCo#:'X')  + '",' +
                           //*** End   Chg *** 11/15/22 ********************
                           '"group":"'    + %EditC(ELfnd:'X')  + '",' +
                           '"employer":"' + %EditC(ELemp#:'X') + '",' +
                           //*** Begin Chg *** 11/15/22 ********************
                           //***'"division":"' + %EditC(ELdiv:'X')  + '"}';
                           '"division":"' + %EditC(ELdiv:'X')  + '"}';
                           //*** End   Chg *** 11/15/22 ********************

                  // Generate 3rd Party Invicing
                  ThirdPartyInvoicing(inJson);

                  // Populate Hoding fields
                  hldCo#  = ELco#;
                  hldFnd  = ELfnd;
                  hldEmp# = ELemp#;
                  hldDiv  = ELdiv;
                  hldInv# = EAninv;
                  weGotAHit = *On;
               EndIf;
            Else;
               If weGotAHit;
                  FinalizeThirdPartyInvoicing(ELfnd:ELfnd);
               EndIf;
            EndIf;
         EndDo;
         Exec SQL Close cursorWMEAP2;

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
                   //*** Begin Chg *** 11/15/22 ****************************
                   //***Exec Sql Select Max(XAnInv) Into invoice# From WMXAP
                   //***         Where XAco#=:ELco# And XAfnd=:ELfnd And
                   //***         XAemp#=:ELemp# And XAdiv=:ELdiv and
                   //***         XApodt=:postDate;
                   Exec Sql Select Max(XAnInv) Into :invoice# From WMXAP
                            Where XAco#=:ELco# And XAfnd=:ELfnd And
                            XAemp#=:ELemp# And XAdiv=:ELdiv and
                            XApodt=:postDate;
                   //*** End   Chg *** 11/15/22 ****************************
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

       EndSr;
