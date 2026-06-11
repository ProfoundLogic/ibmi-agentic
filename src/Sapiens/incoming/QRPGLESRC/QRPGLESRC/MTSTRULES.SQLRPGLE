     hcopyright('(c) 2025 Sapiens, Inc. All rights reserved.')
     h/title MTSTRULES- Fee Schedule Rules.
     h dftname(MTSTRULES)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define profoundUI
     h bnddir('STBNDDIR')
     h bnddir('STAPPCFG')
     h**********************************************************************
      //*********************************************************************
      //===============================================================*
      //  Created   : 10/26/23                                         *
      //  Programmer: C:342                                            *
      //  Project   : FS668  - State Rules maintenance C/S Standards   *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      // 05/08/25 C:079 Support: 99-119033                             *
      //                1.Override state files to correct state        *
      //                2.Corrected criteria for SelBPGrp (Peer Group) *
      //                3.Corrected criteria for Error message 1/2/3   *
      //                                                               *
      // 05/30/24 C:342 Project: FS668 Support : 115674                *
      //                Fixed issue of State Rules Maintenence Filter  *
      //                not retaining values                           *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fMtStRules cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflStRules:scSflRRN)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdMcRule)
      * Fee Schedule Rules
     ffdfrl0    if a e           k disk    rename(pfr01:pfr00)
     f                                     infds(dbfds)
     ffdfrp     uf a e             disk    rename(pfr01:pfr0p)
     f                                     infds(dbfds1)
     ffdfrl     if   e           k disk
     f                                     rename(pfr01:pfr01)
     f                                     prefix(L1_)
     ffdfrl2    if   e           k disk
     f                                     rename(pfr01:pfr02)
     f                                     prefix(L2_)
     ffdfrl6    if   e           k disk    rename(pfr01:pfr06)
     ffdfrl7    if   e           k disk    rename(pfr01:pfr07)
     ffdfrl8    if   e           k disk    rename(pfr01:pfr08)
     ffdfrl9    if   e           k disk    rename(pfr01:pfr09)

      * System-Wide State Code Table File
     fststp     if   e             disk    rename(pst01:pst0p)
     fststl1    if   e           k disk    rename(pst01:pst01)

      * Generic table file
     fftgtl     if   e           k disk    rename(pgt01:pgt00)

     d psds          esds                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0

      // work variables
     d kkststp         s                   like(STkey)
     d kkfrst          s                   like(FRst)
     d kkFRrule        s                   like(FRrule)
     d flgRcdUpdated   s              1    inz
     d saveDT          s                   like(FRdt)
     d saveTM          s                   like(FRtime)
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d @@mode          s              1
     D indexTab        s              5  0
     d process         s               n   inz(*off)
     d processMe       s               n   inz(*off)
     d  mode           s              1    inz(' ')
     d isDelResp       s               n   inz
     d isEffDate       s               n   inz
     d isEnterR        s               n   inz
     d strKey1         s              7a   inz(' ')
     d strKey2         s              7a   inz(' ')
     d strKey3         s             20a   inz(' ')
     d strKey4         s             20a   inz(' ')
     d strst           s                   like(FRst)
     d isError         s               n   inz(*off)

     d svsflrrn        s              5  0 inz
     d xxRrn           s              9  0 inz
     d xxDate1         s              8  0

       // variable for Tab Names
     d arrTabNames     s            100    varying inz dim(20)
     d n1              s             10i 0 inz(1)
     d strTabNames     s           2000    varying inz

       // Rollover fiels
     d kkStR           s                   like(FRst)
     d kkefdtR         s                   like(FRefdt)
     d kkpcatR         s                   like(FRpcat)
     d kkruleR         s                   like(FRrule)
     d xxruleR         s                   like(FRrule)
     d isFoundR        s               n   inz(*off)

      // Filter screen variable
     d meetFltr        s               n
      //*** Beg Add ***  05/30/24  *******************************
     d dsFilter        ds                  likerec(rcdFSRFlt)
      //*** End Add ***  05/30/24  *******************************

     d instrdesc       s            100
     d infildesc       s            100
     d intrim          s              1    inz('1')
     d inwild          s              1    inz(' ')
     d instart         s              3  0 inz(1)
     d intrans         s              1    inz('0')
     d inpatlen        s              3  0 inz(40)
     d instrlen        s              3  0 inz(100)
     d outscnres       s              3  0

     d                 ds
     d TSkey                   1     10
     d TSst                    1      2
     d TStos                   3      5

      // Get client ID
     d GetClientID     pr                  ExtPgm('RTVCLTID')
     d   outCltID                     3

      // Load State Fee Schedule Rules.
     d LoadFSRules     pr

      * Main Processing
     d Main            pr

      * Process Addition of State Fee Schedule Rules.
     d ProcessAdd      pr

      * Process Rollover of State Fee Schedule Rules.
     d ProcessRoll     pr

      * Process Billed/Compared/Processing Information.
     d ProcessBCP      pr             1
     d strmode                        1

      * Process other Options for State Fee Schedule Rules.
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

     * Validate State Rules Primary Information
     d ValidateFS      pr              n

     * Validate Billed/Compared/Processing Information.
     d ValidateBCP     pr              n

     * Validate State Rules Rollover information
     d ValidateRoll    pr              n

      * Initialize errorID and indicators for Billed/Compared/Processing Info.
     d InitErrorBCP    pr

      * Filter for State Rules Maintenance screen.
     d MeetFlt         pr

      * Validate value entered on filter screen.
     d ValidateFSR     pr              n

     d qclscan         pr                  extpgm('QCLSCAN')
     d   instrdesc                         like(instrdesc)
     d   instrlen                          like(instrlen)
     d   instart                           like(instart)
     d   infildesc                         like(infildesc)
     d   inpatlen                          like(inpatlen)
     d   intrans                           like(intrans)
     d   intrim                            like(intrim)
     d   inwild                            like(inwild)
     d   outscnres                         like(outscnres)

     d/copy sPrUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrMask
     d/copy sPrString
     d*** Beg Add *** 04/24/25 ************************************************
     d/copy sPrFiles
     d*** End Add *** 04/24/25 ************************************************

      //   Key List for the Fdfrp file
     c     keyFdfrl0     klist
     c                   kfld                    kkfrst

     c     keyFdfrl2     klist
     c                   kfld                    kkfrst
     c                   kfld                    kkfrrule

      //   Key List for rollover process for fdfrp file
     c     keyfdfrl8     klist
     c                   kfld                    kkStR
     c                   kfld                    kkefdtR

     c     keyfdfrl6     klist
     c                   kfld                    kkStR
     c                   kfld                    kkefdtR
     c                   kfld                    kkpcatR
     c                   kfld                    kkruleR

     c     keyfdfrl9     klist
     c                   kfld                    kkStR
     c                   kfld                    kkefdtR
     c                   kfld                    kkpcatR

     c     keyfdfrl7     klist
     c                   kfld                    kkStR
     c                   kfld                    kkefdtR
     c                   kfld                    kkruleR

      *================================================================

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index


      /free

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       ProgramStart();

       If process;
          Main();
       Endif;

       If processMe;
          ProcessBCP(mode);
       Endif;

       //*** Beg Chg *** 05/08/25 ******************************************
       //****InLr = *On;
       ProgramEnd();
       //*** Beg Chg *** 05/08/25 ******************************************
      /end-free

       //===================================================================
       // Load State Rules Maintenance
       //===================================================================
     p LoadFSRules     b
     d LoadFSRules     pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdRules;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;
       vsbFSRMsg = *Off;

       // Populate the subfile
       Setll KeyFDFRL0 pfr00;
       Reade KeyFDFRL0 pfr00;
       Dow not %eof(fdfrl0);

          // Validate for filter
          meetFltr = *on;
          If scEffDtF <> *zeros or scBillTypF <> *blanks or scStatF <> *blanks
             or scDescF <> *blanks or scEobCdF <> *blanks or scMsgF <> *blanks;
             MeetFlt();
          Endif;

          If meetFltr = *on;
             scSflRRN += 1;
             scSflSel = *off;

             If svsflrrn = 0;
                If scSflRRN = 1;
                   scSflSel = *on;
                Endif;
             Else;
                If scSflRRN = svsflrrn;
                   scSflSel = *on;
                Endif;
             Endif;

             scRule#  = FRrule;
             //scEffDt  = CvtToDate(FRefdt);
             scEffDt  = DtMDYY(FRefdt);
             scPrty   = FRpty;
             scPCat   = FRpcat;
             scDesc   = FRdesc;
             scStat   = FRstat;

             scRRNFS  = dbrrn;
             Write sflStRules;

             If scSflRRN >= 9999;
               scFSRMsg  = 'Too many records returned; first 9999 are shown. ' +
                          'Please use Filter Button to narrow your results.';
               vsbFSRMsg = *On;
               scSflRRN  = 1;
               Leave;
             EndIf;

          Endif;
          Reade KeyFDFRL0 pfr00;
       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadFSRules     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     disexit           s               n   Inz(*off)
      /free

       BtnExit  = *off;
       // Process until exit button is pressed.
       DoU BtnExit  = *On;

          // Load State Rules Maintenance
          LoadFSRules();

          // Show State Rules Maintenance Screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Add Fee Schedule Rules
          When BtnFSAdd = *On;
             BtnFSAdd = *Off;
             ProcessAdd();

          // Process Rollover option for State Rules.
          When BtnFSRoll = *On;
             BtnFSRoll = *Off;
             ProcessRoll();

          // Filter Processing.
          When BtnFilter = *on;
             BtnFilter = *off;
             //*** Beg Del *** 05/30/24 ***********************************
             //*** Clear rcdFSRFlt;
             //*** End Del *** 05/30/24 ***********************************
             isExit = *off;
             // Set criteria fields
             SelEOBFile = 'FTEBP' + %trim(STkey);
             strKey1 = %trim(STkey) + '00000';
             strKey2 = %trim(STkey) + '99999';
             strKey3 = 'MIMSG# >= ''' + strKey1 + '''';
             strKey4 = 'MIMSG# <= ''' + strKey2 + '''';
             //*** Beg Chg *** 05/08/25 ************************************
             //*** SelErrMsg = strKey3 + ' and ' + strKey4;
             SelErrMsg = strKey3 + ' and ' + strKey4 + ' and ' + 'MIMSEQ=0';
             //*** End Chg *** 05/08/25 ************************************
             Dow isExit = *off;
                ExFmt rcdFSRFlt;
                Select;
                When BtnFltrApp = *on;
                   BtnFltrApp = *off;
                   If ValidateFSR();
                      //*** Beg Add *** 05/30/24 **************************
                      dsFilter.scEffDtF  = scEffDtF;
                      dsFilter.scBillTypF= scBillTypF;
                      dsFilter.scStatF   = scStatF;
                      dsFilter.scDescF   = scDescF;
                      dsFilter.scEobCdF  = scEobCdF;
                      dsFilter.scMsgF    = scMsgF;
                      //*** End Add *** 05/30/24 **************************
                      isExit = *on;
                      dspClrFltr = *on;
                   Endif;

                When BtnFltrCan = *on;
                   BtnFltrCan = *off;
                   //*** Beg Add *** 05/30/24 **************************
                   scEffDtF   =  dsFilter.scEffDtF;
                   scBillTypF =  dsFilter.scBillTypF;
                   scStatF    =  dsFilter.scStatF;
                   scDescF    =  dsFilter.scDescF;
                   scEobCdF   =  dsFilter.scEobCdF;
                   scMsgF     =  dsFilter.scMsgF;
                   //*** End Add *** 05/30/24 **************************
                   isExit = *on;
                Endsl;
             Enddo;

          // Filter clear processing
          When BtnClrFltr = *on;
             BtnClrFltr = *off;
             Clear rcdFSRFlt;
             dspClrFltr = *off;
             Clear dsFilter;

          Other;
             ProcOther();             // Process other Options for State Rules

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        indexTab = TabIndex1 + 1;

        Select;
        When indexTab = 1;
           clear rcdMcRule;
           scStateNm = %trim(STname);
           visPnl1 = *on;
           visPnl2 = *off;
           pnlHeight = 540;
           aPnlHeight = pnlHeight + 60;
           Write rcdMcRule;
           //Write rcdSysTbl;
           Write rcdPanel;
           Exfmt rcdRules;
           Read rcdMcRule;
           //Read rcdSystbl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of New State Fee Schedule Rule
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear PopAddEdit;
       mode = 'A';
       visPnl1 = *off;
       visPnl2 = *on;
       isexit = *off;
       BtnCanFSP = *off;
       Exsr MvDbsc;
       scFSHdg = 'Add State Rule Information';
       Dow BtnCanFSP = *off and isexit = *off;

          Exfmt PopAddEdit;
          Select;
          // Save Button
          When BtnNextFSP = *on;
             BtnNextFSP = *off;
             If ValidateFS();
                ProcessBCP(mode);
                isexit = *on;
             Endif;

          // Cancel Button
          When BtnCanFSP = *on;
             BtnCanFSP = *off;
             isexit = *on;
          EndSl;

       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MvDbsc;
        scStateE = %trim(STkey);
        scStatusE = 'A';

        // Get latest Batch Number
        kkfrrule = *hival;
        Setgt keyFdfrl2 pfr02;
        Readpe keyFdfrl0 pfr02;
        If not %eof(fdfrl2);
           scRule#E = L2_FRrule + 1;
        Else;
           scRule#E = 1;
        Endif;

       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcessRoll- Process Rollover option
       //===================================================================
     P ProcessRoll     B
     D ProcessRoll     PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdRollovr;
       mode = 'R';
       isexit = *off;
       BtnCanFS = *off;

       Dow BtnCanFS = *off and isexit = *off;

          Exfmt rcdRollovr;
          Select;
          //Save Button
          When BtnEntRoll = *on;
             BtnEntRoll = *off;
             If ValidateRoll();
                Exsr Rollover;
                isexit = *on;
             Endif;

          // Cancel Button
          When BtnCanRoll = *on;
             BtnCanRoll = *off;
             isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to copy records using Rollover option.
       BegSr Rollover;

        isFoundR = *off;
        kkStR    =  scToStateR;
        kkefdtR  =  DtCYMD(scToEffDtR);

        // Get rule number for To State.
        Setgt (kkStR : kkefdtR) pfr02;
        Readpe kkStR pfr02;
        If not %eof(fdfrl2);
           xxRuleR = L2_FRrule + 1;
        Else;
           xxRuleR = 1;
        Endif;

        //Load Key Fields
        kkStR    =  scFStateR;
        kkefdtR  =  DtCYMD(scFEffDtR);
        kkpcatR  =  scPCatR;
        kkruleR  =  scRule#R;

        Select;
        // When Rule and Category are Not Entered
        When scPCatR = *blanks and scRule#R = *zeros;
           Chain keyfdfrl8 pfr08;
           If %found(fdfrl8);
              isFoundR = *on;
           Endif;

        // When Rule and Category are Entered
        When scPCatR <> *blanks and scRule#R <> *zeros;
           Chain keyfdfrl6 pfr06;
           If %found(fdfrl6);
              isFoundR = *on;
           Endif;

        // When Category is Entered but Rule is Not
        When scPCatR <> *blanks and scRule#R = *zeros;
           Chain keyfdfrl9 pfr09;
           If %found(fdfrl9);
              isFoundR = *on;
           Endif;

        // When Rule is Entered but Category is Not
        When scPCatR = *blanks and scRule#R <> *zeros;
           Chain keyfdfrl7 pfr07;
           If %found(fdfrl7);
              isFoundR = *on;
           Endif;
        EndSl;

        Dow isFoundR = *on;
           If FRstat = 'A';
              FRst   = scToStateR;
              FRefdt = DtCYMD(scToEffDtR);
              FRrule = xxRuleR;
              xxRuleR = xxRuleR + 1;

              FRuser = q1user;                // Maint User
              FRpgm  = q1pgm;                 // Maint Program
              FRtime = Timeto6(SystemTime()); // Maint Time
              FRdt   = Dateto7(SystemDate()); // Maint Date

              FRiusr = q1user;                // Initial User
              FRipgm = q1pgm;                 // Initial Program
              FRitim = Timeto6(SystemTime()); // Initial Time
              FRidt  = Dateto7(SystemDate()); // Initial Date
              write pfr0p;
           Endif;

           Select;
           // When Rule and Category are Not Entered
           When scPCatR = *blanks and scRule#R = *zeros;
              reade keyfdfrl8 pfr08;
              If %eof(fdfrl8);
                 isFoundR = *off;
              Endif;

           // When Rule and Category are Entered
           When scPCatR <> *blanks and scRule#R <> *zeros;
              reade keyfdfrl6 pfr06;
              If %eof(fdfrl6);
                 isFoundR = *off;
              Endif;

           // When Category is Entered but Rule is Not
           When scPCatR <> *blanks and scRule#R = *zeros;
              reade keyfdfrl9 pfr09;
              If %eof(fdfrl9);
              isFoundR = *off;
           Endif;

           // When Rule is Entered but Category is Not
           When scPCatR = *blanks and scRule#R <> *zeros;
              reade keyfdfrl7 pfr07;
              If %eof(fdfrl7);
                 isFoundR = *off;
              Endif;
           EndSl;

        Enddo;

       EndSr;

      /end-free
     P ProcessRoll     E


       //===================================================================
       // ProcOther - Process other option for State Fee Schedule Rules
       //===================================================================
     p ProcOther       b
     d ProcOther       pi
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /Free

       Clear rcdPanel;
       Clear PopAddEdit;
       isexit = *off;
       ReadC sflStRules;
       Dow not %eof;
          //scFocus = '1';
          Select;

          // Process Edit option for Fee Schedule Rules
          When scFSEdit = *on;
             scFSEdit = *off;
             mode ='E';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRNFS pfr0p;
             Exsr Mvdbsc;
             scFSHdg = 'State Rule Information';
             Dow isexit = *off;
                Exfmt PopAddEdit;

                Select;
                When BtnNextFSP = *on;
                   BtnNextFSP = *off;
                   // Check if FDFRP updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('FDFRP':dbrrn1:'FRdt':'FRtime':
                    'FRuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRNFS pfr0p;
                      Exsr Mvdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      // Validate FS Rule.
                      If ValidateFS();
                         Chain scRRNFS pfr0p;
                         ProcessBCP(mode);
                         isExit = *on;
                      Endif;
                   Endif;
                When BtnCanFSP = *on;
                   BtnCanFSP = *off;
                   isExit = *on;
                EndSl;

             Enddo;

          // Process Copy option for Fee Schedule Rules
          When scFSCopy = *on;
             scFSCopy = *off;
             mode ='C';
             svsflrrn = scSflrrn;  //save rrn value
             Chain scRRNFS pfr0p;
             Exsr Mvdbsc;
             scFSHdg = 'Copy State Rule Information';
             Dow isexit = *off;
                Exfmt PopAddEdit;

                Select;
                When BtnNextFSP = *on;
                   BtnNextFSP = *off;
                   // Validate FS Rule.
                   If ValidateFS();
                      ProcessBCP(mode);
                      isExit = *on;
                   Endif;

                When BtnCanFSP = *on;
                   BtnCanFSP = *off;
                   isExit = *on;
                EndSl;

             Enddo;

          // Process Delete option for Fee Schedule Rules
          When scFSDlt = *on;
             scFSDlt = *off;
             mode ='D';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRNFS pfr0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;
             Exsr MvDbSc;
             scFSHdg = 'Delete State Rule Information';
             Dow isexit = *off;
                Write PopAddEdit;

                isDelresp = PopTwoBtn('Do you want to delete this Fee Schedule +
                Rule?':'Confirm Delete':'Yes':'No');
                Select;
                When isDelresp = *On;
                   Chain scRRNFS pfr0p;
                   Delete pfr0p;
                   PopOneBtn('Fee Schedule Rule Deleted!':'Delete':'Ok');
                   isexit = *on;
                   svsflrrn = *zeros;
                When isDelresp = *Off;
                   svsflrrn = scSflrrn;  //save rrn value
                   isexit = *on;
                EndSl;

             Enddo;

          // Process Inquiry option for Fee Schedule Rules
          When scFSInq = *on;
             scFSInq = *off;
             mode ='I';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRNFS pfr0p;
             Exsr Mvdbsc;
             inquiry = *on;
             dspEffdt = *on;
             Dow isexit = *off;
                Exfmt PopAddEdit;

                Select;
                When BtnNextFSP = *on;
                   BtnNextFSP = *off;
                   ProcessBCP(mode);
                   isexit = *on;

                When BtnCanFSP = *on;
                   BtnCanFSP = *off;
                   isexit = *on;
                EndSl;

             Enddo;

          Endsl;

          Unlock Fdfrp;

          ReadC sflStRules;
       Enddo;


       // Move screen fields to data base file.
       Begsr MVdbsc;
        scStatusE = FRstat;
        scStateE = FRst;
        scEffDtE = DtMDYY(FRefdt);
        scExpDtE = DtMDYY(FRexdt);
        scRule#E = FRrule;
        If mode ='C';
           scStatusE  = 'A';
           dspEffdt = *on;
           kkfrrule = *hival;
           Setgt keyFDFRL2 pfr02;
           Readpe keyFDFRL0 pfr02;
           If not %eof(fdfrl2);
              scRule#E = L2_FRrule + 1;
           Else;
              scRule#E = 1;
           Endif;
        Endif;
        scPrtyE  = FRpty;
        scPCatE  = FRpcat;
        scFSBPg# = FRpage;
        scInfoE  = FRinfo;
        scBillTypE = FRsys;
        scDescE    = FRdesc;
        scCodeTypE = FRct;
        scCltNoE   = FRclt#;
        saveDT     = FRdt;
        saveTM     = FRtime;
       Endsr;

      /End-Free
     p ProcOther       e


       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
      /free

       strTabNames += AddToList('Billed Information':arrTabNames:n1);
       strTabNames += AddToList('Compared to Information':arrTabNames:n1);
       strTabNames += AddToList('Processing Information':arrTabNames:n1);

       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'C';
             process = *on;
          When @@mode = 'D';
             processMe = *on;
             mode = 'I';
          Endsl;
       Endif;

       // called from F2412R program
       If Process;
          chain xxRrn pst0p;
          If %found(ststp);
             KKfrst = STkey;
             scState = %trim(STkey) + ' - '  + %trim(STname);
          Else;
             Process = *off;
             Return;
          Endif;
       Endif;

       // called from F0390R program
       If ProcessMe;
          chain(n) xxRrn pfr0p;
          If %found(fdfrp);
             KKfrst = FRst;
             kkststp = FRst;
             chain kkststp pst01;
             scState = %trim(STkey) + ' - '  + %trim(STname);
             scStateNm = %trim(STname);
          Else;
             ProcessMe = *off;
             Return;
          Endif;
       Endif;

       // Tab Index
       tabIndex1 = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex1 = %int(@@TabInd);
             If tabIndex1 < 0 or tabIndex1 > 2;
                TabIndex1 = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       // Get the client id
       GetClientID(clientID);

       intCo# = GetUserInfo(q1user);

       //*** Beg Add *** 05/30/24 *********************************
       Clear dsFilter;
       Clear rcdFSRFlt;
       //*** End Add *** 05/30/24 *********************************
       Return;

      /end-free
     p ProgramStart    e

       //*** Beg Add *** 05/08/25 ************************************
       //===================================================================
       // ProgramEnd - Final Program tasks
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi
       *inlr = *on;
       Return;
     p ProgramEnd      e
       //*** End Add *** 05/08/25 ************************************

      //===================================================================
       //===================================================================
       // ProcessBCP - Process Billed/Compared/Processing Information.
       //===================================================================
     p ProcessBCP      b
     d ProcessBCP      pi             1
     d strmode                        1
     d  isexit         s               n   Inz(*off)
      /free

       clear rcdBillInf;
       clear rcdCmpInfo;
       clear rcdProInfo;
       visPnl1 = *off;
       visPnl2 = *on;
       isexit = *off;
       BtnCanFS = *off;
       tabIndex2 = *zeros;
       InitErrorBCP();
       If strmode = 'E' or strmode = 'C' or strmode = 'I';
          Exsr Movedbsc;
       Endif;
       If strmode = 'I';
          inquiry = *on;
          dspEffdt = *on;
       Endif;
       Exsr SetCriteria;
       //*** Beg Add *** 05/08/25 ******************************************
       Exsr SetChoicesDBfile;
       //*** End Add *** 05/08/25 ******************************************
       Exsr SetActPanel;
       Dow BtnCanFS = *off and isexit = *off;
          Select;
          When arrTabNames(tabIndex2+1) = 'Billed Information';
             //ConTop = 50;
             Pnl2Height = 820;
             Pnl2Top = 70;
             APnlHeight = Pnl2Height + 60;
             TopCan = APnlHeight - 30;
             Write rcdMcRule;
             Write rcdPanel;
             Exfmt rcdBillInf;
             Read rcdPanel;
             Read rcdMcRule;

          When arrTabNames(tabIndex2+1) = 'Compared to Information';
             //ConTop = 50;
             Pnl2Height = 560;
             Pnl2Top = 70;
             APnlHeight = Pnl2Height + 60;
             TopCan = APnlHeight - 30;
             Write rcdMcRule;
             Write rcdPanel;
             Exfmt rcdCmpInfo;
             Read rcdPanel;
             Read rcdMcRule;

          When arrTabNames(tabIndex2+1) = 'Processing Information';
             //ConTop = 50;
             Pnl2Height = 710;
             Pnl2Top = 70;
             APnlHeight = Pnl2Height + 60;
             TopCan = APnlHeight - 30;
             Write rcdMcRule;
             Write rcdPanel;
             Exfmt rcdProInfo;
             Read rcdPanel;
             Read rcdMcRule;
          Endsl;

          // Select action from Tab/screen
          Select;
          When BtnSave = *on;
             Btnsave = *off;
             If strmode = 'A' or strmode = 'E' or strmode = 'C';
                If ValidateBCP();
                   Exsr Movescdb;
                   isexit = *on;
                Endif;
             Endif;
          When BtnCanFS = *on;
             BtnCanFS = *off;
             isexit = *on;
          When BtnRefresh = *on;
             BtnRefresh = *off;
             Clear rcdBillInf;
             Clear rcdCmpInfo;
             Clear rcdProInfo;
             Exsr SetCriteria;
             //*** Beg Add *** 05/08/25 ******************************************
             Exsr SetChoicesDBfile;
             //*** End Add *** 05/08/25 ******************************************
             If strmode <> 'A';
                Exsr Movedbsc;
             Endif;
             If strmode = 'I';
                inquiry = *on;
                dspEffdt = *on;
             Endif;
          Endsl;

       Enddo;

       Return strmode;


       Begsr Movedbsc;
        // Move Tab1 fields
        scSpCd = FRspcd;
        scBsPb = FRbspb;
        scPCIn = FRpcin;
        sc50IN = FR50in;
        scBRT  = FRbrt;
        scBFCD = FRbfcd;
        scBfMd = FRbfmd;
        scVal  = FRval;
        scBTCd = FRbtcd;
        scBtMd = FRbtmd;
        scFToS = FRftos;
        scFTSS = FRftss;
        scTToS = FRttos;
        scTTSS = FRttss;
        scBMRI = FRbmri;
        scFToB = FRftob;
        scTToB = FRttob;
        scBFPS = FRbfps;
        scBTPS = FRbtps;
        scBAst = FRbast;
        scFUD  = FRfud;
        scBFUT = FRbfut;
        scNCt  = FRnct;
        scBSep = FRbsep;
        scTAne = FRtane;
        scPTyp = FRptyp;
        scBPrt = FRbprt;
        scPGrp = FRpgrp;
        scAVAL = FRaval;
        scBDex = FRbdex;
        scCCod = FRccod;
        TSst   = FRst;
        TSToS  = FRbtos;
        scBToS = TSkey;
        scBMD2 = FRbmd2;
        scBMD3 = FRbmd3;
        scBMD4 = FRbmd4;
        sc51IN = FR51in;
        sc62IN = FR62in;
        sc66IN = FR66in;
        sc80IN = FR80in;
        scFADt = DtMDYY(FRfadt);
        scTADt = DtMDYY(FRtadt);
        scBSrg = FRbsrg;
        scBFhe = FRbfhe;
        scCoMP = FRcomp;
        scMeMa = FRmema;
        scMRMa = FRmrma;
        scDRMa = FRdrma;
        scFEMa = FRfema;
        scPOMa = FRpoma;
        scSVal = FRsval;
        scSCMa = FRscma;
        scAPSI = FRapsi;
        scImSt = FRimst;
        scASPI = FRaspi;
        scBInvl = FRbinvl;
        scBFAd = FRbfad;


        // Move Tab2 fields
        scCFCd = FRcfcd;
        scCTCd = FRctcd;
        scCPTS = FRcpts;
        scCSEP = FRcsep;
        scEFCd = FRefcd;
        scETCd = FRetcd;
        scCAst = FRcast;
        scHMRA = FRhmra;
        scLoss = FRloss;
        scTlos = FRtlos;
        scCMRI = FRcmri;
        scMaj  = FRmaj;
        scCMod = FRcmod;
        scIGMd = FRigmd;
        scHsDt = FRchdt;
        scCPoS = FRcpos;
        scCSDt = FRcsdt;
        scCSPr = FRcspr;
        scCPrt = FRcprt;
        scCDPr = FRcdpr;
        scCSBl = FRcsbl;
        scCToB = FRctob;
        sc#Prc = FR#prc;
        sc#GPC = FR#gpc;
        scCFDy = FRcfdy;
        sc#Day = FR#day;
        sc#Gdy = FR#gdy;
        scCPDy = FRcpdy;
        scPTTM = FRpttm;
        scTRMT = FRtrmt;
        scMUnt = FRmunt;
        scCAPCSI = FRcapcsi;
        sc#DSD = FR#dsd;
        scOvrd = FRovrd;
        scCTPC = FRctpc;

        // Move Tab3 fields
        scEobCd1 =  FReobcd1;
        scEobCd2 =  FReobcd2;
        scMsg   =  FRmsg;
        scMsg2  =  FRmsg2;
        scMsg3  =  FRmsg3;
        scPMR   =  FRpmr;
        scProf  =  FRprof;
        scPCVL  =  FRpcvl;
        scTech  =  FRtech;
        scTCVl  =  FRtcvl;
        scDis   =  FRdis;
        scAmt   =  FRamt;
        scPct   =  FRpct;
        scPCh   =  FRpch;
        scPAne  =  FRpane;
        scTMDv  =  FRtmdv;
        scRnd   =  FRrnd;
        scFTM   =  FRftm;
        scTTM   =  FRttm;
        scAUnt  =  FRaunt;
        scConv  =  FRconv;
        scCon2  =  FRcon2;
        scPRX   =  FRprx;
        scPCPT  =  FRpcpt;
        scUnit  =  FRunit;
        scPPSC  =  FRppsc;
        scPPDM  =  FRppdm;
        scPSRV  =  FRpsrv;
        scPPGp  =  FRppgp;
        scPOut  =  FRpout;
        scPDif  =  FRpdif;
        scPDRG  =  FRpdrg;
        scPCRF  =  FRpcrf;
        scPORC  =  FRporc;
        scPASC  =  FRpasc;
        scPPHI  =  FRpphi;
        scPPIR  =  FRppir;
        scPPHO  =  FRppho;
        scPPOR  =  FRppor;
        scPSNI  =  FRpsni;
        scPSNO  =  FRpsno;
        scPHHI  =  FRphhi;
        scPHHO  =  FRphho;
        scPFAC  =  FRpfac;
        scPPTV  =  FRpptv;
        scPARE  =  FRpare;
        scMR    =  FRmr;
        scPAS2  =  FRpas2;
       Endsr;

       Begsr Movescdb;
        // Move State Rules Primary Information field
        FRstat  =  scStatusE;
        FRst    =  scStateE;
        FRefdt  =  DtCYMD(scEffDtE);
        FRexdt  =  DtCYMD(scExpDtE);
        FRrule  =  scRule#E;
        FRpty   =  scPrtyE;
        FRpcat  =  scPCatE;
        FRpage  =  scFSBPg#;
        FRinfo  =  scInfoE;
        FRsys   =  scBillTypE;
        FRdesc  =  scDescE;
        FRct    =  scCodeTypE;
        FRclt#  =  scCltNoE;

        // Move Tab1 fields
        FRspcd  =  scSpCd;
        FRbspb  =  scBsPb;
        FRpcin  =  scPCIn;
        FR50in  =  sc50IN;
        FRbrt   =  scBRT;
        FRbfcd  =  scBFCD;
        FRbfmd  =  scBfMd;
        FRval   =  scVal;
        FRbtcd  =  scBTCd;
        FRbtmd  =  scBtMd;
        FRftos  =  scFToS;
        FRftss  =  scFTSS;
        FRttos  =  scTToS;
        FRttss  =  scTTSS;
        FRbmri  =  scBMRI;
        FRftob  =  scFToB;
        FRttob  =  scTToB;
        FRbfps  =  scBFPS;
        FRbtps  =  scBTPS;
        FRbast  =   scBAst;
        FRfud   =  scFUD;
        FRbfut  =  scBFUT;
        FRnct   =  scNCt;
        FRbsep  =  scBSep;
        FRtane  =  scTAne;
        FRptyp  =  scPTyp;
        FRbprt  =  scBPrt;
        FRpgrp  =  scPGrp;
        FRaval  =  scAVAL;
        FRbdex  =  scBDex;
        FRccod  =  scCCod;
        TSkey   =  scBToS;
        FRbtos  =  TSToS;

        FRbmd2  =  scBMD2;
        FRbmd3  =  scBMD3;
        FRbmd4  =  scBMD4;
        FR51in  =  sc51IN;
        FR62in  =  sc62IN;
        FR66in  =  sc66IN;
        FR80in  =  sc80IN;
        FRfadt  =  DtCYMD(scFADt);
        FRtadt  =  DtCYMD(scTADt);
        FRbsrg  =  scBSrg;
        FRbfhe  =  scBFhe;
        FRcomp  =  scCoMP;
        FRmema  =  scMeMa;
        FRmrma  =  scMRMa;
        FRdrma  =  scDRMa;
        FRfema  =  scFEMa;
        FRpoma  =  scPOMa;
        FRsval  =  scSVal;
        FRscma  =  scSCMa;
        FRapsi  =  scAPSI;
        FRimst  =  scImSt;
        FRaspi  =  scASPI;
        FRbinvl = scBInvl;
        FRbfad  =  scBFAd;

        // Move Tab2 fields
        FRcfcd  =  scCFCd;
        FRctcd  =  scCTCd;
        FRcpts  =  scCPTS;
        FRcsep  =  scCSEP;
        FRefcd  =  scEFCd;
        FRetcd  =  scETCd;
        FRcast  =  scCAst;
        FRhmra  =  scHMRA;
        FRloss  =  scLoss;
        FRtlos  =  scTlos;
        FRcmri  =  scCMRI;
        FRmaj   =  scMaj;
        FRcmod  =  scCMod;
        FRigmd  =  scIGMd;
        FRchdt  =  scHsDt;
        FRcpos  =  scCPoS;
        FRcsdt  =  scCSDt;
        FRcspr  =  scCSPr;
        FRcprt  =  scCPrt;
        FRcdpr  =  scCDPr;
        FRcsbl  =  scCSBl;
        FRctob  =  scCToB;
        FR#prc  =  sc#Prc;
        FR#gpc  =  sc#GPC;
        FRcfdy  =  scCFDy;
        FR#day  =  sc#Day;
        FR#gdy  =  sc#Gdy;
        FRcpdy  =  scCPDy;
        FRpttm  =  scPTTM;
        FRtrmt  =  scTRMT;
        FRmunt  =  scMUnt;
        FRcapcsi = scCAPCSI;
        FR#dsd  =  sc#DSD;
        FRovrd  =  scOvrd;
        FRctpc  =  scCTPC;


        // Move Tab3 fields
        FReobcd1 =  scEobCd1;
        FReobcd2 =  scEobCd2;
        FRmsg    =  scMsg;
        FRmsg2   =  scMsg2;
        FRmsg3   =  scMsg3;
        FRpmr    =  scPMR;
        FRprof   =  scProf;
        FRpcvl   =  scPCVL;
        FRtech   =  scTech;
        FRtcvl   =  scTCVl;
        FRdis    =  scDis;
        FRamt    =  scAmt;
        FRpct    =  scPct;
        FRpch    =  scPCh;
        FRpane   =  scPAne;
        FRtmdv   =  scTMDv;
        FRrnd    =  scRnd;
        FRftm    =  scFTM;
        FRttm    =  scTTM;
        FRaunt   =  scAUnt;
        FRconv   =  scConv;
        FRcon2   =  scCon2;
        FRprx    =  scPRX;
        FRpcpt   =  scPCPT;
        FRunit   =  scUnit;
        FRppsc   =  scPPSC;
        FRppdm   =  scPPDM;
        FRpsrv   =  scPSRV;
        FRppgp   =  scPPGp;
        FRpout   =  scPOut;
        FRpdif   =  scPDif;
        FRpdrg   =  scPDRG;
        FRpcrf   =  scPCRF;
        FRporc   =  scPORC;
        FRpasc   =  scPASC;
        FRpphi   =  scPPHI;
        FRppir   =  scPPIR;
        FRppho   =  scPPHO;
        FRppor   =  scPPOR;
        FRpsni   =  scPSNI;
        FRpsno   =  scPSNO;
        FRphhi   =  scPHHI;
        FRphho   =  scPHHO;
        FRpfac   =  scPFAC;
        FRpptv   =  scPPTV;
        FRpare   =  scPARE;
        FRmr     =  scMR;
        FRpas2   =  scPAS2;
        FRuser   =  q1user;                // Maint User
        FRpgm    =  q1pgm;                 // Maint Program
        FRtime   =  Timeto6(SystemTime()); // Maint Time
        FRdt     =  Dateto7(SystemDate()); // Maint Date

        // Add and Copy mode
        If strmode = 'A' or strmode = 'C';
         FRiusr    =  q1user;                // Initial User
         FRipgm    =  q1pgm;                 // Initial Program
         FRitim    =  Timeto6(SystemTime()); // Initial Time
         FRidt     =  Dateto7(SystemDate()); // Initial Date
         write pfr0p;
        Endif;

        // Update record if Edit mode.
        If strmode = 'E';
           update pfr0p;
        Endif;

       Endsr;


       Begsr SetCriteria;
        // First Tab
        SelBPCIn = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''MIPC''' ;
        SelB50IN = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''MI50''' ;
        SelBBRT = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''BYRP''' ;
        selBfMd = 'PMco# =' + %char(intCo#) +
          ' and PMstat <> ''I''';
        selBtMd = 'PMco# =' + %char(intCo#) +
          ' and PMstat <> ''I''';
        SelBFTos = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''SVTY''' ;
        SelBTTos = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''SVTY''' ;
        SelBMRI = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''RLUS''' ;
        SelBFToB = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''NYBT''' ;
        SelBTToB = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''NYBT''' ;
        SelBFPS = 'PSco# =' + %char(intCo#) +
          ' and PSstat <> ''I''';
        SelBTPS = 'PSco# =' + %char(intCo#) +
          ' and PSstat <> ''I''';
        SelBFUT  = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''FOLU''' ;
        SelBTAne = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''ANTY''' ;
        SelBPTyp = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''PROD''' ;
        SelBPrt  = 'PAco# =' + %char(intCo#) +
          ' and PAstat <> ''I''';
        SelBPGrp = 'FTco# =' + %char(intCo#) +
        //*** Beg Chg *** 05/08/25 ************************************
        //***  ' and PAstat <> ''I''';
          ' and FTstat <> ''I''';
        //*** Beg Chg *** 05/08/25 ************************************
        SelBAVAL = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''VALU''' ;
        SelBCCod = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''CCRU''' ;
        strst = %trim(STkey);
        SelBTos = 'TSco# =' + %char(intCo#) +
          ' and TSstat <> ''I'' and TSkey like ''' + strst + '%'+ '''';
        SelBMD2 = 'PMco# =' + %char(intCo#) +
          ' and PMstat <> ''I''';
        SelB51IN = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''MI51''' ;
        SelB62IN = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''MI62''' ;
        SelB66IN = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''MI66''' ;
        SelB80IN = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''MI80''' ;
        SelBAPSI = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''APSI''' ;
        SelBIMST = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''IMST''' ;
        SelBASPI = 'GTco# =' + %char(intCo#) +
          ' and GTstat <> ''I''' +
          ' and GTtbid = ''ASPI''' ;

        // Third Tab
        SelEOBFile = 'FTEBP' + %trim(STkey);
        strKey1 = %trim(STkey) + '00000';
        strKey2 = %trim(STkey) + '99999';
        strKey3   = 'MIMSG# >= ''' + strKey1 + '''';
        strKey4   = 'MIMSG# <= ''' + strKey2 + '''';
        //*** Beg Chg *** 05/08/25 ************************************
        //*** SelErrMsg = strKey3 + ' and ' + strKey4;
        SelErrMsg = strKey3 + ' and ' + strKey4 + ' and ' + 'MIMSEQ=0';
        //*** End Chg *** 05/08/25 ************************************

       Endsr;
        //*** Beg Add *** 05/08/25 ************************************
       Begsr SetChoicesDBfile;
        // Set Choices Database File files for the selected state
        ChFTPMPxx = 'FTPMP' + %Trim(kkFrSt);     //From/To Modifier,From Modifier 2/3/4
        chFTPSPxx = 'FTPSP' + %Trim(kkFrSt);     //From Place of Service
        chFTPAPxx = 'FTPAP' + %Trim(kkFrSt);     //Body part
        chFTFTPxx = 'FTFTP' + %Trim(kkFrSt);     //Peer group

        // Compared to Information
        //uses same database file as the Billed Information screen

        // Processing Information
        chFMPRPxx = 'FMPRP' + %Trim(kkFrSt);     //Pay Procedure Code Fee Schedule Allowance
       Endsr;
        //*** End Add *** 05/08/25 ************************************

       Begsr SetActPanel;
        // Set screen field value if called from F0390R program
        If ProcessMe;
           Exsr SetHdgBCP;
        Endif;
        scStateH = scState;
        scEffDtH = scEffDtE;
        scExpDtH = scExpDtE;
        scRule#H = scRule#E;
        scPrtyH  = scPrtyE;
        scFSBPg#H = scFSBPg#;
        scInfoH   = scInfoE;
        scDescH   = scDescE;
        scCltNoH  = scCltNoE;

        Select;
        When scBillTypE ='D';
           scBillTypH = 'Dental';
        When scBillTypE ='H';
           scBillTypH = 'Hospital';
        When scBillTypE ='P';
           scBillTypH = 'Physician';
        When scBillTypE ='X';
           scBillTypH = 'Pharmacy';
        Endsl;

        Select;
        When scCodeTypE = *blanks;
           scCodeTypH = *blanks;
        When scCodeTypE ='C';
           scCodeTypH = 'CPT CODE';
        When scCodeTypE ='R';
           scCodeTypH = 'REASONABLE AND CUSTOMARY CODE';
        Endsl;

        Select;
        When scStatusE = 'A';
           scStatusH = 'Active';
        When scStatusE = 'I';
           scStatusH = 'Inactive';
        Endsl;

        // File code for Procedure category is "PCAT"
        scPCatH = *blanks;
        If scPCatE <> *blanks;
           Chain (intCo# :'PCAT' :scPCatE) pgt00;
           If %found(ftgtl);
              scPCatH = %trim(GTkey) + ' - ' + %trim(GTdesc);
           Endif;
        Endif;
       Endsr;

        // Set screen field value if called from F0390R program
       Begsr SetHdgBCP;
        scEffDtE =  DtMDYY(FRefdt);
        scExpDtE =  DtMDYY(FRexdt);
        scRule#E =  FRrule;
        scPrtyE  =  FRpty;
        scFSBPg# =  FRpage;
        scInfoE  =  FRinfo;
        scDescE  =  FRdesc;
        scCltNoE =  FRclt#;
        scBillTypE = FRsys;
        scCodeTypE = FRct;
        scStatusE =  FRstat;
        scPCatE    = FRpcat;
       Endsr;

      /end-free
     p ProcessBCP      e

      //===================================================================
      // Validate the State Rules Primary Information
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateFS      b
     d ValidateFS      pi              n
     d kkefdtE         s                   like(FRefdt)
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erEffDtE = *off;
        msEffDtE = *blanks;
        erExpDtE = *off;
        msExpDtE = *blanks;
        erPCatE  = *off;
        msPCatE  = *blanks;
        erBillTypE = *off;
        msBillTypE = *blanks;
        erDescE = *off;
        msDescE = *blanks;
        erCodeTypE = *off;
        msCodeTypE = *blanks;

        xxDate1 = dtCheck(scEffDtE);
        If xxDate1 = 0;
           isError = *on;
           erEffDtE   = *on;
           msEffDtE   = 'Invalid date entered.';
        Endif;

        If scExpDtE <> *zeros;
           xxDate1 = dtCheck(scExpDtE);
           If xxDate1 = 0;
              isError = *on;
              erExpDtE = *on;
              msExpDtE = 'Invalid date entered.';
           Endif;
        Endif;

        If scPCatE = *blanks;
           msPCatE  = 'This field can not be blank.';
           erPCatE  = *on;
           isError = *on;
        Endif;

        If scDescE = *blanks;
           msDescE = 'This field can not be blank.';
           erDescE = *on;
           isError = *on;
        Endif;

        If scBillTypE = *blanks;
           msBillTypE = 'This field can not be blank.';
           erBillTypE = *on;
           isError = *on;
        Endif;

        If isError = *off;
           kkefdtE = DtCYMD(scEffDtE);
           Chain (scStateE : kkefdtE) pfr01;
           If not %found(fdfrl);
              isEffDate = PopTwoBtn('The effective date for this rule have +
                 not been used. Verify this effective date before +
                 continuing.':'Confirm Effective Date':'Enter':'Cancel');
              Select;
              When isEffDate = *On;
              When isEffDate = *Off;
                 isError = *on;
              EndSl;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateFS      e


      //========================================================================
      // Validate Billed/Compared/Processing Information
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //========================================================================
     p ValidateBCP     b
     d ValidateBCP     pi              n
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erBTCd = *off;
        msBTCd = *blanks;
        erFADt = *off;
        msFADt = *blanks;
        erTADt = *off;
        msTADt = *blanks;

        // Tab 1 error
        If scBTCd = *blanks;
           msBTCd  = 'This field can not be blank.';
           erBTCd  = *on;
           isError = *on;
        Endif;

        If scFADt <> *zeros;
           xxDate1 = dtCheck(scFADt);
           If xxDate1 = 0;
              isError = *on;
              erFADt  = *on;
              msFADt  = 'Invalid date entered.';
           Endif;
        Endif;

        If scTADt <> *zeros;
           xxDate1 = dtCheck(scTADt);
           If xxDate1 = 0;
              isError = *on;
              erTADt  = *on;
              msTADt  = 'Invalid date entered.';
           Endif;
        Endif;

        // Tab 2 error
        // Tab 3 error

      // Return *on indicator when No error exists.
        If isError = *on;
           TabIndex2 = *zeros;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateBCP     e
 
 
      //=======================================================================
      // Initialize errorID and indicators for Billed/Compared/Processing Info.
      //=======================================================================
     p InitErrorBCP    b
     d InitErrorBCP    pi
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erBTCd = *off;
        msBTCd = *blanks;
        erFADt = *off;
        msFADt = *blanks;
        erTADt = *off;
        msTADt = *blanks;

      /End-Free
     p InitErrorBCP    e


      //===================================================================
      // Validate the State Rules Rollover screen
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateRoll    b
     d ValidateRoll    pi              n
      /Free

        isError = *off;
       // Initialize all the messageId and error indicators.
        erFStateR = *off;
        msFStateR = *blanks;
        erFEffDtR = *off;
        msFEffDtR = *blanks;
        erPCatR   = *off;
        msPCatR   = *blanks;
        erRule#R  = *off;
        msRule#R  = *blanks;
        erToStateR = *off;
        msToStateR = *blanks;
        erToEffDtR = *off;
        msToEffDtR = *blanks;

        kkStR    =  scFStateR;
        kkefdtR  =  DtCYMD(scFEffDtR);
        kkpcatR  =  scPCatR;
        kkruleR  =  scRule#R;

        If scFStateR = *blanks;
           msFStateR = 'This field can not be blank.';
           erFStateR = *on;
           isError = *on;
        Endif;

        xxDate1 = dtCheck(scFEffDtR);
        If xxDate1 = 0;
           isError = *on;
           erFEffDtR  = *on;
           msFEffDtR  = 'Invalid date entered.';
        Endif;


        If (scPCatR = *blanks and scRule#R = *zeros) and isError = *off;
           //Chain to file
           Chain keyfdfrl8 pfr08;
           If not %found(fdfrl8);
              msFStateR = 'Rules do not exist for this State and Effective +
                           Date';
              erFStateR = *on;
              msFEffDtR = 'Rules do not exist for this State and Effective +
                           Date';
              erFEffDtR = *on;
              isError = *on;
           Endif;
        Endif;

        If (scPCatR <> *blanks and scRule#R = *zeros) and isError = *off;
           //Chain to file
           Chain keyfdfrl9 pfr09;
           If not %found(fdfrl9);
              msPCatR  = 'Invalid category code entered';
              erPCatR  = *on;
              isError = *on;
           Endif;
        Endif;

        If (scPCatR = *blanks and scRule#R <> *zeros) and isError = *off;
           //Chain to file
           Chain keyfdfrl7 pfr07;
           If not %found(fdfrl7);
              msRule#R  = 'Invalid Rule Number Entered';
              erRule#R  = *on;
              isError = *on;
           Endif;
        Endif;

        If (scPCatR <> *blanks and scRule#R <> *zeros) and isError = *off;
           //Chain to file
           Chain keyfdfrl6 pfr06;
           If not %found(fdfrl6);
              msRule#R  = 'Invalid Rule Number Entered';
              erRule#R  = *on;
              msPCatR  = 'Invalid category code entered';
              erPCatR  = *on;
              isError = *on;
           Endif;
        Endif;

        If scToStateR = *blanks;
           msToStateR = 'This field can not be blank';
           erToStateR = *on;
           isError = *on;
        Endif;

        xxDate1 = dtCheck(scToEffDtR);
        If xxDate1 = 0;
           isError = *on;
           erToEffDtR = *on;
           msToEffDtR = 'Invalid date entered.';
        Endif;

        If isError = *off;
           If scFStateR = scToStateR and scFEffDtR = scToEffDtR;
              msToStateR = 'Rules Already Exist for this State and Effective +
                           Date';
              erToStateR = *on;
              msToEffDtR = 'Rules Already Exist for this State and Effective +
                           Date';
              erToEffDtR = *on;
              isError = *on;
           Endif;
        Endif;

        If isError = *off;
           kkefdtR  =  DtCYMD(scToEffDtR);
           kkStR    =  scToStateR;
           kkruleR  =  scRule#R;
           // Condition I) If rule is entered on screen
           If scRule#R <> *zeros and scFStateR <> scToStateR;
              Chain keyfdfrl7 pfr07;
              If %found(fdfrl7);
                 isEnterR = PopTwoBtn('Some or All Rule Numbers Exist for This +
                 State and Effective Date. Press Yes to Continue'
                 :'Warning Message':'Yes':'No');
                 Select;
                 When isEnterR = *On;
                    //PopOneBtn('Rule/s copied':'Completed':'Ok');
                 When isEnterR = *Off;
                    isError = *on;
                 EndSl;
              Endif;
           Endif;
           // Condition II) If rule is not entered on screen
           If scRule#R = *zeros and scFStateR <> scToStateR;
              kkruleR = *hival;
              Setgt keyfdfrl7 pfr07;
              readp pfr07;
              If kkefdtR = FRefdt and scToStateR = FRst;
                 isEnterR = PopTwoBtn('Some or All Rule Numbers Exist for This +
                 State and Effective Date. Press Yes to Continue'
                 :'Warning Message':'Yes':'No');
                 Select;
                 When isEnterR = *On;
                    //PopOneBtn('Rule/s copied':'Completed':'Ok');
                 When isEnterR = *Off;
                    isError = *on;
                 EndSl;
              Endif;
           Endif;
        Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *off;
        Else;
           return *on;
        Endif;

      /End-Free
     p ValidateRoll    e

       //==================================================================
       // MeetFlt -  Determine if the records meet the Filter criteria
       //==================================================================
     p MeetFlt         b
     d MeetFlt         pi
      /free

       // Check for effective date
       If scEffDtF <> *zeros and DtCYMD(scEffDtF) <> FRefdt;
          meetFltr = *off;
       Endif;

       // Check for description
       If meetFltr = *on and scDescF <> *blanks;

          clear instrdesc;
          clear infildesc;
          infildesc = Upper(FRdesc);
          instrdesc = Upper(scDescF);

          QClscan(infildesc:instrlen:instart:instrdesc:inpatlen:intrans:
           intrim:inwild:outscnres);

          If outscnres = 0;
             meetFltr = *off;
          Endif;
       Endif;

       // Check for EOB code
       If meetFltr = *on and scEobCdF <> *blanks and (scEobCdF <> FReobcd1 and
          scEobCdF <> FReobcd2);
          meetFltr = *off;
       Endif;

       // Check for Error message code
       If meetFltr = *on and scMsgF <> *blanks and (scMsgF <> FRmsg and
          scMsgF <> FRmsg2 and scMsgF <> FRmsg3);
          meetFltr = *off;
       Endif;

       // Check for System Type code
       If meetFltr = *on and scBillTypF <> *blanks and scBillTypF <> FRsys;
          meetFltr = *off;
       Endif;

       // Check for System Type code
       If meetFltr = *on and scStatF <> *blanks and scStatF <> FRstat;
          meetFltr = *off;
       Endif;

      /end-free
     p MeetFlt         e


      //===================================================================
      // Validate Filter criteria for State Rules Maintenance
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateFSR     b
     d ValidateFSR     pi              n
     d isError         s               n   inz
      /Free

       isError = *off;

       msEffDtFlt = *blanks;
       erEffDtFlt = *off;

       //ValFrmMt  = dtCYMD(scValFrmMt)/100;
       //SubToDt   = dtCYMD(scSubToDt);

       If scEffDtF <> *zeros;
          xxDate1 = dtCheck(scEffDtF);
          If xxDate1 = 0;
             isError = *on;
             erEffDtFlt = *on;
             msEffDtFlt = 'Invalid date entered.';
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateFSR     e

       //===================================================================
       // Add information to list
       //===================================================================
     PAddToList        b
     DAddToList        pi           101a   varying
     D inString                     100a   varying const
     D inArray                      100a   varying dim(20)
     D inIndex                       10i 0

     DoutString        s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;
      /End-free
     PAddToList        e
