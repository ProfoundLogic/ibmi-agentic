     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
      //*********************************************************************
     h/title MTGRPEMP - Group Employer Information
     h dftname(MTGRPEMP)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('STAPPCFG')
      //****************************************************************
      //==============================================================**
      //  Created   : 06/10/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : MC447  - Redesign of Work with Groups            *
      //                                                               *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fMtGrpEmp  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflEmpInfo:scSflRRN)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Fund Master file
     ffmfnp     if   e             disk    rename(pfn01:pfn0p)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Application Security File
     fsdtbl1    if   e           k disk
      * Employee Master File
     ffmeml     if   e           k disk    rename(pem01:pem00)
     ffmeml4    if   e           k disk    rename(pem01:pem04)
     f                                     infds(dbfds)
     ffmemp     uf a e             disk    rename(pem01:pem0p)
     f                                     infds(dbfds1)
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d psds          esds                  extname(szq1p)
      // work variables
     d saveDT          s                   like(EMdt)
     d saveTM          s                   like(EMitim)
     d flgRcdUpdated   s              1    inz
     d svsflrrn        s              5  0 inz
     d charmsg         s            200    inz(*blanks)
       dcl-s  inDspFileName char(10);
       dcl-s  inPgmName     char(50);
       dcl-s  inTabName     char(50);

      * variable for Tab Names
     d arrTabNames     s            100    varying inz dim(20)
     d n1              s             10i 0 inz(1)
     d strTabNames     s           2000    varying inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0 inz
     d inCo#           s              3  0 inz
     d inSub           s              5  0 inz
     d inFnd           s              3  0 inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d process         s               n   inz(*off)
     d outAcct         s               n   inz(*off)
     d isAllDiv        s               n   inz(*off)
     d isUserAuth      s               n   inz(*off)
     d isDelResp       s               n   inz

     d outCltID        s              3    inz
     d strDartsKey     s             40    inz
     d empName         s             40    inz
     d charKeyEnv      s              1    inz
     d UsrDftFnd       s                   like(UPfnd) inz
     d UsrDftSub       s                   like(UPsub#) inz
     d UsrDftFyr       s                   like(UPfyr) inz
     d UsrDftEmp       s                   like(UPEmp#) inz
     d UsrDftDiv       s                   like(UPdiv) inz
     d ##mrsu          s                   like(UPmrsu)inz
     d ##mrgr          s                   like(UPmrgr) inz
     d ##mrem          s                   like(UPmrem) inz
     d isMgri          s               n   inz
     d isWcif          s               n   inz
     d isWcin          s               n   inz
     d xxRrn           s              9  0

      * Work variable for GroupType and ValidCase
     d  outRtCD        s              1
     d  outCo          s              3
     d  outSub#        s              5
     d  outFnd         s              3
     d  outSts         s              1
     d  outEmp         s              9
     d  outDiv         s              5

     d  $$co#          s              3  0
     d  $$usr          s             10
     d  $$appl         s              1
     d  $$rtcd         s              1
     d  $$auth         s              1
     d  $$fnd          s              3  0
     d  $$emp#         s              9  0
     d  $$sub#         s              5  0
     d  MultiGrpRst    s              1n   inz(*off)

      * Check Group Restriction
     d CheckGrpRst     pr                  extpgm('SA008R')
     d $$co#                          3  0
     d $$usr                         10
     d $$appl                         1    Const
     d $$rtcd                         1
     d $$auth                         1    options(*nopass)
     d $$fnd                          3  0 options(*nopass)
     d $$emp#                         9  0 options(*nopass)
     d $$sub#                         5  0 options(*nopass)

      * Delete Employer
     d DltEmployer     pr                  extpgm('FA029R')
     d                                1
     d                                3
     d                                5
     d                                3
     d                                9
     d                                5
     d                                9  0

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d GroupType       pr                  Extpgm('FA701R')
     d                                1
     d                                3
     d                                5
     d                                3
     d                                1

      * Load Group Employer Information
     d LoadEmpInfo     pr

      * Main Processing
     d Main            pr

      * Process Add option for Employer
     d ProcessAdd      pr

      * Process Edit option for Employer
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr

      * Validate Employer info
     d ValidateEmp     pr              n
     d strmode                        1

     d Validcase       pr                  Extpgm('FA207R')
     d                                1
     d                                3
     d                                5
     d                                3
     d                                9
     d                                5

     d/copy sPrAtrium
     d/copy sPrGenPop
     d/copy sPrCompany
     d/copy sPrUsrPrms
     d/copy sPrSecure
     d/copy sPrDate
     d/copy sPrModule

      // Entry of Program.
     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mod
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index

      /Free

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pfn0p;
       inCo# = FNco#;
       inSub = FNSub#;
       inFnd = FNfnd;

       GetClientID(clientID);

       ProgramStart();

       If process;

          If TbNmRcdPnl <> *Blanks;
             Main();
          Else;
             isNoAccess = *on;
             TbNmRcdPnl = 'Security';
             pnlHeight = 560;
             aPnlHeight = 620;
             Write rcdGrpPnl;
             Exfmt rcdPanel;
             Read rcdGrpPnl;
             If scSelOpt <> *blanks;
                @@next = scSelOpt;
             Endif;
          Endif;

       Endif;

       *InLr = *On;

      /End-Free
       //===================================================================
       // Add information to list
       //===================================================================
     p AddToList       b
     d AddToList       pi           101a   varying
     d inString                     100a   varying const
     d inArray                      100a   varying dim(20)
     d inIndex                       10i 0

     d outString       s            101a   varying inz
      /Free
       If inIndex > 1;
          outString = ',';
       Endif;
       outString += inString;
       inArray(inIndex) = inString;
       inIndex += 1;
       Return outString;

      /End-free
     p AddToList       e

       //===================================================================
       // Load Employer Information
       //===================================================================
     p LoadEmpInfo     b
     d LoadEmpInfo     pi

      /free
       // Clear the subfile
       scSflRRN = 0;
       scSflDsp = *off;
       scSflClr = *on;
       Write rcdEmpInfo;
       scSflClr = *off;
       scSflDsp = *on;
       scRowSel = 1;
       If OutAcct  = *on and dspMode <> *on;
          visEdit  = *on;
       Else;
          visEdit  = *off;
       Endif;

       // Populate the subfile
       Setll (inCo#:inSub:inFnd) fmeml4;
       Reade (inCo#:inSub:inFnd) fmeml4;
       Dow not %eof(fmeml4);
          isUserAuth = *on;
          scAuth = *blanks;
          If isMgri = *on;
             scAuth = 'A';
             If ##Mrem ='Y' and  $$rtcd = *on;
                $$Auth = *blanks;
                $$fnd  = EMfnd;
                $$emp# = EMemp#;
                $$Sub# = EMsub#;
                CheckGrpRst($$co#:$$usr:$$appl:$$rtcd:$$Auth:$$Fnd:
                 $$Emp#:$$sub#);
                scAuth = $$Auth;
                If $$rtcd = *on;
                   isUserAuth = *off;
                Endif;
             Endif;
          Endif;
          If ((UsrDftEmp = *zeros) or (UsrDftEmp<>*zeros and UsrDftEmp = EMemp#
            and isAllDiv = *on) or (UsrDftEmp<>*zeros and UsrDftEmp = EMemp#
            and UsrDftDiv = EMdiv and isAllDiv = *off)) and isUserAuth = *on;
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
             scEmpNm  = EMname;
             scEmpNo  = EMemp#;
             scDiv    = EMdiv;
             If EMstat = 'I';
                scStatus = 'Inactive';
             Else;
                scStatus = 'Active';
             Endif;
             Clear charKeyEnv;
             Clear scDartURL ;
             empName     =  EMname;
             If empName = *blanks;
                empName = '<No name available>';
             Endif;
             scDBANm  = empName;
             strDartsKey = %editc(emCo#:'X') + %editc(emFnd:'X') +
                %editc(emEmp#:'X') + %editc(emDiv:'X');
                %subst(strDartsKey:40:1) = charKeyEnv;
             scDartURL  = %subst('iDARTS - ' + scDBANm:1:50) +
                CreateGenUrl('/iDartsWeb/profound.do':
                'user=' + %Trim(q1User):
                '&keyProgram=W0050R':
                '&key=' + strDartsKey:
                '&action=listing':
                '&mode=remarks');
             scRRN = dbrrn;
             Write sflEmpInfo;
          Endif;
          Reade (inCo#:inSub:inFnd) fmeml4;
       Enddo;

       scSflRRN = 1;
       If svSflRRN <> 0;
          scSflRRN = svSflRRN;
          scRowSel = svSflRRN;
          svSflRRN = 0;
       Endif;

      /end-free
     p LoadEmpInfo     e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       BtnCancel = *off;

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // Load Holiday Blackout Dates
          LoadEmpInfo();

          // Display Screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Add button
          When BtnEmpAdd  = *On;
             BtnEmpAdd  = *Off;
             ProcessAdd();

          // Refresh
          When btnRefresh = *On;
             btnRefresh = *Off;

          // Save
          When btnSave = *On;
             btnSave = *Off;

          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'MTGRPEMP';
             inPgmName     = 'Employer Information';
             inTabName     =  arrTabNames(TabIndex + 1);
             inTabName     =  %Trim(inTabName) + ' - ' + 'User Access';
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          Other;
             ProcOther();             // Process other Options Employer

          EndSl;

       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        //When tabIndex = 0;
        When arrTabNames(TabIndex+1) = 'Employer Information';
           PnlHeight = 370;
           APnlHeight = PnlHeight + 60;
           Write rcdGrpPnl;
           Write rcdPanel;
           Exfmt rcdEmpInfo;
           Read rcdGrpPnl;
           Read rcdPanel;
        EndSl;

       Endsr;

      /End-Free

     p Main            e

       //===================================================================
       // ProcessAdd - Process Addition of Employer Information
       //===================================================================
     P ProcessAdd      B
     D ProcessAdd      PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdEmpAdd;
       mode = 'A';
       isexit = *off;
       BtnCanEmp = *off;
       scStatusE = 'A';

       Dow BtnCanEmp = *off and isexit = *off;

          scEmpHdg = 'Add Employer Information';

          Exfmt rcdEmpAdd;
          Select;
          // Save Button
          When BtnSavEmp = *on;
             BtnSavEmp = *off;
             If ValidateEmp(mode);
                Exsr WrtEmployer;
                isexit = *on;
             EndIf;

          // Cacel Button
          When BtnCanEmp = *on;
             BtnCanEmp = *off;
             isexit = *on;
          EndSl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        EMco#   = inCo#;
        EMsub#  = inSub;
        EMfnd   = inFnd;
        EMemp#  = scEmpNoE;
        EMdiv   = scDivE;
        EMstat  = scStatusE;
        EMname  = scNameE;
        EMdba   = scDBANmE;
        EMnob   = scBizNatE;

        EMmad1  = scMad1;
        EMmad2  = scMad2;
        EMmcty  = scMcty;
        EMmst   = scMst;
        EMmzip  = scMzip;
        EMpad1  = scPad1;
        EMpad2  = scPad2;
        EMpcty  = scPcty;
        EMpst   = scPst;
        EMpzip  = scPzip;

        If scPhone> *zeros;
           EMarea  = %dec(%subst(%editc(scPhone:'X'):1:3):3:0); // Phone  Area
           EMphn#  = %dec(%subst(%editc(scPhone:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scFax> *zeros;
           EMfara  = %dec(%subst(%editc(scFax:'X'):1:3):3:0); // Fax  Area
           EMfax#  = %dec(%subst(%editc(scFax:'X'):4):7:0);   // Fax  Num
        EndIf;
        EMfein = scFedID;

        EMuser  = q1user;                // User
        EMpgm   = q1pgm;                 // Program
        EMtime  = Timeto6(SystemTime()); // Time
        EMdt    = Dateto7(SystemDate()); // Date Mintained
        EMiusr  = q1user;                // Initial User
        EMipgm  = q1pgm;                 // InitialtProgram
        EMitim  = Timeto6(SystemTime()); // Initial Time
        EMidt   = Dateto7(SystemDate()); // Initial Date
       EndSr;

       // Writing new Office Information
       BegSr WrtEmployer;
        clear pem0p;
        Exsr MoveScDb;
        Write pem0p;
       EndSr;

      /end-free
     P ProcessAdd      E

       //===================================================================
       // ProcOther - Process other Options for Employer
       //===================================================================
     p ProcOther       B
     d ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       Clear rcdEmpAdd;
       ReadC sflEmpInfo;
       Dow not %eof;

          Select;
          When scEmpEdt = *On; // Processing for Edit
             scEmpEdt = *Off;
             Chain(e) scRRN pem0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             If isMgri = *on and MultiGrpRst = *on;
                If scAuth = 'I';
                   PopOneBtn(' User does not have update authorization for +
                   this record.':'Edit Error');
                   Return;
                Endif;
             Endif;

             mode = 'C';
             svsflrrn = scSflrrn;  //save rrn value
             Chain(n) scRRN pem0p;
             Exsr Movedbsc;
             scEmpHdg = 'Edit Employer Information';
             EmpEdt  = *on;
             Dow isexit = *off;
                Exfmt rcdEmpAdd;

                Select;
                When BtnSavEmp = *on;
                   BtnSavEmp = *off;
                   // Check if fmemp updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('FMEMP':dbrrn1:'EMdt':'EMtime':
                    'EMuser':saveDT:saveTM:'1');
                   if flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRN pem0p;
                      Exsr Movedbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      // Validate the Employer Information
                      If ValidateEmp(mode);
                         Chain scRRN pem0p;
                         Exsr Movescdb;
                         update pem0p;
                         isExit = *on;
                      Endif;
                   Endif;
                When BtnCanEmp = *on;
                   BtnCanEmp = *off;
                   isExit = *on;
                EndSl;
             Enddo;

          When scEmpDlt = *On; // Processing for Delete
             scEmpDlt = *Off;
             svsflrrn = scSflRRN;
             Chain(e) scRRN pem0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;
             If isMgri = *on;
                If scAuth = 'I';
                   PopOneBtn('Option/Function not available. You are only +
                   authorized to Inquiry mode.':'Delete Error');
                   Return;
                Endif;
             Endif;
             If isWcif = *on and  isWcin = *on;
                outRTCD = *blanks;
                outSTS  = *blanks;
                outCo   = %editc(EMco#:'X');
                outSub# = %editc(EMsub#:'X');
                outFnd  = %editc(EMfnd:'X');
                outEmp  = %editc(EMemp#:'X');
                outDiv  = %editc(EMdiv:'X');
                GroupType(outRTCD:outCO:outSUB#:outFND:outSTS);

                If outsts = 'N';
                   PopOneBtn('This option must be initiated by Workers Comp.':
                             'Delete Error':'Ok');
                   Return;
                Endif;
             Endif;

             outRTCD = *blanks;
             ValidCase(outRTCD:outCO:outSUB#:outFND:outEmp:outDiv);
             If outRTCD = *on;
                PopOneBtn('WCMC Cases still exist on this Employer.':
                          'Delete Error':'Ok');
                Return;
             Endif;

             //delete employer if no error
             Chain scRrn pem0p;
             scEmpHdg = 'Delete Employer Information';
             Exsr Movedbsc;
             Write rcdEmpAdd;
             isDelresp = PopTwoBtn('Do you want to delete this Employer ?'
                :'Confirm Delete':'Yes':'No');
             Select;
             When isDelresp = *On;
                outRTCD = *blanks;
                outCo   = *zeros;
                outSub# = *zeros;
                outFnd  = *zeros;
                outEmp  = *zeros;
                outDiv  = *zeros;
                outDiv  = *zeros;
                DltEmployer(outRTCD:outCo:outSub#:outFnd:outEmp
                  :outDiv:scrrn);
                Delete pem0p;
                PopOneBtn('Employer Deleted!':
                          'Delete':'Ok');
                svsflrrn = *zeros;
                Return;
             When isDelresp = *Off;
                svsflrrn = scsflRRN;  //save rrn value
                Return;
             Endsl;
          Endsl;

          Unlock fmemp;
          ReadC sflEmpInfo;
       Enddo;

       Begsr Movedbsc;
        scEmpNoE   =  EMemp#;
        scDivE     =  EMdiv;
        scNameE    =  EMname;
        scDBANmE   =  EMdba;
        scBizNatE  =  EMnob;
        scMad1     =  EMmad1;
        scMad2     =  EMmad2;
        scMcty     =  EMmcty;
        scMst      =  EMmst;
        scMzip     =  EMmzip;
        scPad1     =  EMpad1;
        scPad2     =  EMpad2;
        scPcty     =  EMpcty;
        scPst      =  EMpst;
        scPzip     =  EMpzip;
        scPhone    =  EMarea * 10000000 + EMphn#;
        scFax      =  EMfara * 10000000 + EMfax#;
        scFedID    =  EMfein;
        scStatusE  =  EMstat;
        saveDT     =  EMdt;
        saveTm     =  EMtime;
       Endsr;

       Begsr Movescdb;
        EMstat  = scStatusE;
        EMname  = scNameE;
        EMdba   = scDBANmE;
        EMnob   = scBizNatE;

        EMmad1  = scMad1;
        EMmad2  = scMad2;
        EMmcty  = scMcty;
        EMmst   = scMst;
        EMmzip  = scMzip;
        EMpad1  = scPad1;
        EMpad2  = scPad2;
        EMpcty  = scPcty;
        EMpst   = scPst;
        EMpzip  = scPzip;

        If scPhone> *zeros;
           EMarea  = %dec(%subst(%editc(scPhone:'X'):1:3):3:0); // Phone  Area
           EMphn#  = %dec(%subst(%editc(scPhone:'X'):4):7:0);   // Phone  Num
        EndIf;
        If scFax> *zeros;
           EMfara  = %dec(%subst(%editc(scFax:'X'):1:3):3:0); // Fax  Area
           EMfax#  = %dec(%subst(%editc(scFax:'X'):4):7:0);   // Fax  Num
        EndIf;
        EMfein = scFedID;

        EMuser  = q1user;                // User
        EMpgm   = q1pgm;                 // Program
        EMtime  = Timeto6(SystemTime()); // Time
        EMdt    = Dateto7(SystemDate()); // Date Mintained
       Endsr;

      /End-Free

     p ProcOther       e

       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       TbNmRcdPnl = 'Employer Information';
       apShowMenu = *on;
       // Set Menu options
       scMenuChoi = 'Primary Group Information';
       scMenuValu = 'MTGRPPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Employer Information';
       scMenuValu = %trim(scMenuValu) + ',MTGRPEMP';

       scMenuChoi = %trim(scMenuChoi) + ',Accounting Information';
       scMenuValu = %trim(scMenuValu) + ',MTGRPACT';

       // Set Screen Heading
       scHeading = 'Employer Information';

       aMenHeight = 80;

       apFootHght = aMenHeight + 125;

       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
       //Update Action Panel Info (Rrn, Group Name, Group Number)
       scActRrn = xxRrn;
       scCompName = %trim(FNname);
       scGrpNum = FNfnd;
       If scCompName = ' ';
          Chain intCo# pco00;
          If COname <> ' ';
             scCompName = COname;
          Endif;
       Endif;

       // Get User Default Group
       UsrDftFnd = 0;
       Chain q1user pup00;
       If %found(smupl);
          UsrDftFnd = UPfnd;
          UsrDftSub = UPsub#;
          UsrDftFyr = UPfyr;
       EndIf;

       // Retrieve System Values
       isWcif   = *off;
       isWcif   = IsModEnabled('WorkComp');

       isWcin   = *off;
       isWcin   = IsModEnabled('Work Comp Interface');

       isMgri   = *off;
       isMgri   = IsModEnabled('Multi Group Restriction');

       // Check for Multi Group Restriction
       $$co#  = intCo#;
       $$usr  = q1user;
       $$appl = 'M';
       $$rtcd = *off;
       CheckGrpRst($$co#:$$usr:$$appl:$$rtcd);
       MultiGrpRst = $$rtcd;

       ##mrsu = UPmrsu;
       ##mrgr = UPmrgr;
       ##mrem = *blanks;
       If UPmrsu = 'Y' or UPmrgr = 'Y' or UPmrem = 'Y';
          ##mrem = 'Y';
       Endif;

       If (##mrem = 'Y' and MultiGrpRst = *off and UPemp# <> *zeros) or
        (isMgri = *on and UPmrem = 'Y' and UPemp# <> *zeros);
          UsrDftEmp = UPemp#;
          UsrDftDiv = UPdiv;
       Endif;
       If UPfl5  <> 'Y';
          isAllDiv = *on;
       Endif;

       isNoAccess = *off;

       // Set User Access button visibility indicator based on Users Security
       Chain (Q1user) pup00;
       // based on Users Security Type and Level defined in Application Confguration
       If CheckSecLevel('GrpCfgAut':UPtype:UPlvl);
          isUsrAcc = *on;
       Else;
          isUsrAcc = *off;
       Endif;

       // Display Group Information in Inquiry Mode even if Users Security
       // level does not allow them to Edit Group Information.
       If dspMode = *on;
          isUsrAcc = *on;
       Endif;

       // Determine if user is in the Group Config Tab file for each tab.  This
       // will determine which tabs are displayed.  If the user has 999 * then
       // all tabs are displayed
       // Employer Information tab
       Chain (intCo#:'MTGRPEMP':'Employer Information':
        'Employer Information - User Access':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Employer Information':arrTabNames:n1);
       Endif;
       TbNmRcdPnl = strTabNames;

       outRTCD = *blanks;
       outSTS  = *blanks;
       outCO   = %editc(inCo#:'X');
       outSUB# = %editc(inSub:'X');
       outFND  = %editc(inFnd:'X');
       GroupType(outRTCD:outCO:outSUB#:outFND:outSTS);
       If outSts = 'Y';
          OutAcct = *on;
       Endif;

       process = *on;
       Return;

      /End-Free

     p ProgramStart    e

      //===================================================================
      // Validate the Employer Information
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p ValidateEmp     b
     d ValidateEmp     pi              n
     d strmode                        1
     d isError         s               n   inz
      /Free

      // Initialize all the messageId and error indicators.
       isError = *off;
       erEmpNoE = *off;
       msEmpNoE = *blanks;
       erDivE   = *off;
       msDivE   = *blanks;
       erNameE  = *off;
       msNameE  = *blanks;
       erDBANmE = *off;
       msDBANmE = *blanks;
       erMad1   = *off;
       msMad1   = *blanks;
       erMcty   = *off;
       msMcty   = *blanks;
       erMst    = *off;
       msMst    = *blanks;
       erMzip   = *off;
       msMzip   = *blanks;
       erPad1   = *off;
       msPad1   = *blanks;
       erPcty   = *off;
       msPcty   = *blanks;
       erPst    = *off;
       msPst    = *blanks;
       erPzip   = *off;
       msPzip   = *blanks;

       If strmode = 'A';
          If scEmpNoE = *zeros;
             erEmpNoE = *on;
             msEmpNoE = 'MCR0556';
             isError  = *on;
          Endif;

          If scEmpNoE <> *zeros;
             chain(n) (inCo#:inSub:inFnd:scEmpNoE:scDivE) pem00;
             If %found(fmeml);
                erEmpNoE = *on;
                msEmpNoE = 'MC40042';
                erDivE   = *on;
                msDivE   = 'MC40042';
                isError  = *on;
             Endif;
          Endif;
       Endif;

       If scNameE = *blanks;
          erNameE = *on;
          msNameE = 'MCR0503';
          isError = *on;
       Endif;

       If scDBANmE = *blanks;
          erDBANmE = *on;
          msDBANmE = 'MCR0503';
          isError  = *on;
       Endif;

       // Enter complete info for Mailing address, if any of the field is enter
       If scMad1 <> *blanks or scMad2 <> *blanks or
        scMcty <> *blanks or scMst <> *blanks or scMzip <> 0;
          If scMad1 = *Blanks;
             erMad1 = *on;
             msMad1 = 'MCR0503';
             isError = *on;
          Endif;
          If scMcty = *blanks;
             erMcty = *on;
             msMcty = 'MCR0503';
             isError = *on;
          Endif;
          If scMst  = *blanks;
             erMst  = *on;
             msMst  = 'MCR0503';
             isError = *on;
          Endif;
          If scMzip = *zeros;
             erMzip = *on;
             msMzip = 'MCR0556';
             isError = *on;
          Endif;
       Endif;

       // Enter complete info for Physical address, if any of the field is enter
       If scPad1 <> *blanks or scPad2 <> *blanks or
        scPcty <> *blanks or scPst <> *blanks or scPzip <> 0;
          If scPad1 = *Blanks;
             erPad1 = *on;
             msPad1 = 'MCR0503';
             isError = *on;
          Endif;
          If scPcty = *blanks;
             erPcty = *on;
             msPcty = 'MCR0503';
             isError = *on;
          Endif;
          If scPst  = *blanks;
             erPst  = *on;
             msPst  = 'MCR0503';
             isError = *on;
          Endif;
          If scPzip = *zeros;
             erPzip = *on;
             msPzip = 'MCR0556';
             isError = *on;
          Endif;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateEmp     e
