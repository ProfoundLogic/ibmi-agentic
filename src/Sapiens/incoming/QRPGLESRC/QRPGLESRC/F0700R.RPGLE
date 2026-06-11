     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE F0700R - Rightcoding
     H DFTNAME(F0700R)
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP('QILE')
     H/DEFINE   PROFOUNDUI
     H BNDDIR('WTBNDDIR')
     h**********************************************************************
      *===============================================================*
      *  Created   : 08/01/14                                         *
      *  Programmer: C:212                                            *
      *  Project   : 114MNV                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     fF0700F    cf   e             workstn extfile('F0700F')
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflRightCd:rrnRightCd)
     ffarcp     uf a e             disk    rename(prc01:prc0p)
     f                                     infds(dbfds1)
     ffarcl2    if   e           k disk    rename(prc01:prc02)
     fftgtl     if   e           k disk
     fststl3    if   e           k disk    rename(pst01:pst03)
     fftfep     if   e             disk    usropn
     fftfel     if   e           k disk    usropn
     f                                     rename(pfe01:pfe02)
     ffmprl     if   e           k disk    usropn
     ffmrcl1    if   e           k disk    usropn

     d* procedure definitions
     d*
     d F2013R          pr                  extpgm('F2013R')
     d ##kc                           1
     d ##kl                           1
     d ##rrn                          9  0
     d ##proc                         5
     d ##prod                        50
     d ##rtcd                         1
     d ##ast                          2

     d F2521R          pr                  extpgm('F2521R')
     d ##call                         1
     d ##kc                           1
     d ##kl                           1
     d ##ast                          2
     d ##rrn                          9  0

     d*
     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  rrnfarcp             397    400b 0

     d                 ds
     d  @@cmd                  1     56
     d  @@st                  38     39

     d* array definitions
     d*
     d qca             s             56    dim(6) ctdata perrcd(1)              qcmdexc

     d* standalone variables
     d*
     d ##kc            s              1    inz
     d ##kl            s              1    inz
     d ##rrn           s              9  0 inz
     d ##proc          s              5    inz
     d ##prod          s             50    inz
     d ##rtcd          s              1    inz
     d ##ast           s              2    inz
     d ##call          s              1    inz

     D inCmd           s           2750
     D inCmdLen        s             15  5

     d ##mode          s              1    inz
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isValid         s               n   inz(*off)
     d mode            s              1    inz
     d process         s               n   inz(*off)
     d xxRrn           s              9  0 inz
     d xxProcCd        s              5  0 inz
     d xxKey           s             10    inz
     d yyKey           s              5p 0 inz
     d @@mode          s              1
     d company         s              3p 0 inz
     d efdtky          s                   like(RCefdt) inz
     d stky            s                   like(RCst)   inz
     d typeky          s                   like(RCtype) inz
     d fcodky          s                   like(RCfcod) inz
     d tcodky          s                   like(RCtcod) inz
     d sv_scAddEffDt   s                   like(RCefdt) inz
     d sv_scAddSt      s                   like(RCst)   inz
     d sv_scAddCdTyp   s                   like(RCtype) inz
     d sv_scAddFrmCd   s                   like(RCfcod) inz
     d sv_scAddToCd    s                   like(RCtcod) inz

     d* Clear Subfiles
     d ClrSubFiles     pr

     d* load rightcoding grid
     dloadRightcoding  pr

     d* main Processing
     d main            pr

     d* program Start
     d programStart    pr
     d processMe                       n

     d* process addition
     d processAdd      pr

     d* process other options
     d procOther       pr

     d* Validate Add/Edit screen
     d Validate        pr

     d* Add details
     d addDetails      pr

     d* Move database record to screen
     d MoveDbSc        pr

     d*/copy sPrAtrium
     d/copy sPrDate
     d*/copy sPrGenPop
     d/copy sPrString
     d*/copy sPrGenErr

     d*/copy wPrAgent
     d*/copy wPrProduce
     d*/copy wPrGroup
     d*/copy wPrRateSet
     d*/copy wPrEmpLr
     d*/copy wPrCarrier
     d*/copy wPrAR
     d/copy wPrUsrPrms
     d*/copy wPrLoc
     d*/copy wPrClaims

     c* Entry of Program.
     c     *Entry        Plist
     c                   Parm                    @@rrn             9            wdelp
     c                   Parm                    @@mode                         mode
     c                   Parm                    @@TabInd          5            Tab index

      /free

       // main program processing
       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Else;
          xxRrn = 1;
       Endif;

       // Program Mode.
       If %parms >= 2;
          mode = @@mode;
       Endif;

       // Tab Index Mode.
       TabIndex = 0;

       Select;
       When mode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When mode = 'A';
          addMode = *On;
          dspVisb = *On;
       When mode = 'C';
          chgMode= *On;
          dspVisb = *On;
       Endsl;

       ProgramStart(process);

       If process;
          Main();
          @@rrn = %Editc(xxRRN:'X');
       Endif;

       Unlock FARCP;
       *inlr = *on;
       Return;

      /end-free
       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
     P ClrSubFiles     B
     D ClrSubFiles     PI
      /Free

       rrnRightCd = 0;
       clrRCSub = *On;
       dspRCSub = *Off;
       dspRCCtl = *Off;
       Write rcdRightCd;

      /End-Free
     P ClrSubfiles     E
       //===================================================================
       // Load Rightcoding Information
       //===================================================================
     PLoadRightcoding  B
     dLoadRightcoding  PI
      /free

       // set subfile indicators
       clrRCSub = *Off;
       dspRCSub = *On;
       dspRCCtl = *On;

       // read file and load the subfile
       Setll *start farcp;
       Read prc0p;
       dow not %eof(farcp);
          clear sflRightCd;

          scRCEffDat = CvtToDate(RCefdt);
          scRCState = RCst;
          scRCCdTyp = RCtype;
          scRCFrmCd = RCfcod;
          scRCToCd  = RCtcod;
          scRCPmnt  = RCpmtt;
          scRCAneH  = RCaneh;
          scRCStat  = RCstat;

          scRCRRN# = rrnfarcp;

          rrnRightCd = rrnRightCd + 1;
          If rrnRightCd = 1;
             scRCRowSl = 'A';
          Else;
             clear scRCRowSl;
          Endif;
          If mode = 'D';
             dspVisb = *Off;
          Else;
             dspVisb = *On;
          Endif;

          Write sflRightCd;

          Read prc0p;
       Enddo;

       rrnRightCd = 1;

      /end-free
     PLoadRightcoding  E
      //===================================================================
       // Main - Main processing
      //===================================================================
     P Main            B
     d Main            PI

      /free

       // Process until exit button is pressed.
       Dou ExitNow = *On;

          // Clear Sub-files
          ClrSubfiles();

          // Load Rightcoding Information
          LoadRightcoding();

          // Process Rightcoding screen.
          Exsr ExfmtScreens;

          Select;
          // do nothing - catch tab click event
          When tabClick = *on;
             tabClick = *off;

          When btnRefresh = *On;
             btnRefresh =*Off;

          // Add button processing
          When scRCAdd = *on;
             clear RcdAddRC;
             ##mode = 'A';
             BtnAddRcd = *off;
             BtnCanRcd = *off;
             processAdd();

          // Other options processing
          Other;
             procOther();

          Endsl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 1050;
          Write rcdTabPan;
          Exfmt rcdRightCd;
          Read rcdTabPan;
       EndSl;

       Endsr;

      /end-free
     P Main            E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B
     d ProgramStart    PI
     d processMe                       n

      /free

       // If Rrn is passed, verify
       If xxRrn > *zeros;

         // Chain to Rightcoding File for Program.
         Chain xxRrn prc0p;

         // Process only if valid record found.
         If not %found(farcp);
          processme = *Off;
          Return;
         Endif;

       Endif;

       processMe = *on;

       TabIndex = 0;

       // get company of the user
       company = UserCo#(q1user);

       Return;

      /end-free
     P ProgramStart    E
       //===================================================================
       // processAdd - process addition
       //===================================================================
     P processAdd      B
     d processAdd      PI
      /free

       isValid  = *on;

       // processing for Edit mode
       If ##mode = 'C';

        // effective date should be disabled in edit mode
          dsbAddEfDt = *on;

          Chain(n) scRCRrn# prc0p;
          MoveDbSc();

       Endif;

       // processing for Add mode
       If ##mode = 'A';

        // effective date should not be disabled in edit mode
          dsbAddEfDt = *off;

       Endif;

       Dou BtnCanRcd = *on or (BtnAddRcd = *on and isValid = *on);
          Exfmt rcdAddRC;
          isValid = *on;
          scAddFCdFo = *off;
          scAddTCdFo = *off;
          Select;

          When BtnAddRcd = *on;
             Validate();
             If isValid = *on;
                addDetails();
             Endif;

          When BtnCanRcd = *on;
             BtnCanRcd = *off;
             Leave;

         // search from code
          When scAddFCdVw = *on;
             scAddFCdVw = *off;

            // verify State is entered and valid
             erAddSt = *off;
             If scAddSt = *blanks;
                msAddSt = 'WCR0503';
                erAddSt = *on;
                isValid = *off;
                iter;
             Else;
                xxkey = scAddSt;
                Setll xxkey pst03;
                if not %equal(ststl3);
                   msAddSt = 'WCR0513';
                   erAddSt = *on;
                   isValid = *off;
                   iter;
                endif;
             Endif;

            // verify Code Type is entered and valid
             erAddCdTyp = *off;
             If scAddCdTyp = *blanks;
                msAddCdTyp = 'WCR0503';
                erAddCdTyp = *on;
                isValid  = *off;
                iter;
             Else;
                Setll (company:'CDTP':scAddCdTyp) pgt01;
                If not %equal(ftgtl);
                   msAddCdTyp = 'WCR0513';
                   erAddCdTyp = *on;
                   isValid  = *off;
                   iter;
                Endif;
             Endif;

             If isValid = *on;

                ##kc = *blanks;
                ##kl = *blanks;
                ##rrn = *zeros;
                ##proc = *blanks;
                ##prod = *blanks;
                ##rtcd = *blanks;
                ##ast = scAddSt;
                ##call = '2';

                If scAddCdTyp = 'D';
                  // if open close file
                   if %open(FTFEP);
                      close FTFEP;
                   endif;
                   if %open(FTFEL);
                      close FTFEL;
                   endif;
                  // delete existing override
                   inCmdlen = 56;
                   inCmd = QCA(1);
                   monitor;
                    callp RunCommand(inCmd:inCmdLen);
                   on-error;
                   endmon;
                   inCmdlen = 56;
                   inCmd = QCA(3);
                   monitor;
                    callp RunCommand(inCmd:inCmdLen);
                   on-error;
                   endmon;
                  // override with state file
                   @@cmd = qca(2);
                   @@st  = scAddSt;
                   inCmd = @@cmd;
                   monitor;
                    callp RunCommand(inCmd:inCmdLen);
                   on-error;
                   endmon;
                   @@cmd = qca(4);
                   @@st  = scAddSt;
                   inCmd = @@cmd;
                   monitor;
                    callp RunCommand(inCmd:inCmdLen);
                   on-error;
                   endmon;
                  // open file for use
                   erAddSt = *off;
                   monitor;
                    if not %open(FTFEP);
                       open FTFEP;
                    endif;
                    if not %open(FTFEL);
                       open FTFEL;
                    endif;
                   on-error;
                    msAddSt = 'WCR0513';
                    erAddSt = *on;
                    iter;
                   endmon;
                  // call program for selection of value
                   callp F2521R (##call:##kc:##kl:##ast:##rrn);
                   chain(n) ##rrn pfe01;
                   if %found(ftfep);
                      scAddFrmCd = %editc(FEdrg#:'X');
                      scAddFCdFo = *on;      // focus on the search field
                   endif;
                Endif;
                If scAddCdTyp = 'A' or scAddCdTyp = 'P' or scAddCdTyp = 'H';
                 // call program for selection of value
                   callp F2013R (##kc:##kl:##rrn:##proc:##prod:##rtcd:##ast);
                   if ##rtcd = '1' or ##prod = *blanks;
                   else;
                      scAddFrmCd = ##proc;
                      scAddFCdFo = *on;      // focus on the search field
                   endif;
                Endif;
             Endif;

         // search To code
          When scAddTCdVw = *on;
             scAddTCdVw = *off;

            // verify State is entered and valid
             erAddSt = *off;
             If scAddSt = *blanks;
                msAddSt = 'WCR0503';
                erAddSt = *on;
                isValid = *off;
                iter;
             Else;
                xxkey = scAddSt;
                Setll xxkey pst03;
                if not %equal(ststl3);
                   msAddSt = 'WCR0513';
                   erAddSt = *on;
                   isValid = *off;
                   iter;
                endif;
             Endif;

            // verify Code Type is entered and valid
             erAddCdTyp = *off;
             If scAddCdTyp = *blanks;
                msAddCdTyp = 'WCR0503';
                erAddCdTyp = *on;
                isValid  = *off;
                iter;
             Else;
                Setll (company:'CDTP':scAddCdTyp) pgt01;
                If not %equal(ftgtl);
                   msAddCdTyp = 'WCR0513';
                   erAddCdTyp = *on;
                   isValid  = *off;
                   iter;
                Endif;
             Endif;

             If isValid = *on;

                ##kc = *blanks;
                ##kl = *blanks;
                ##rrn = *zeros;
                ##proc = *blanks;
                ##prod = *blanks;
                ##rtcd = *blanks;
                ##ast = scAddSt;
                ##call = '2';

                If scAddCdTyp = 'D';
                  // if open close file
                   if %open(FTFEP);
                      close FTFEP;
                   endif;
                   if %open(FTFEL);
                      close FTFEL;
                   endif;
                  // delete existing override
                   inCmdlen = 56;
                   inCmd = QCA(1);
                   monitor;
                    callp RunCommand(inCmd:inCmdLen);
                   on-error;
                   endmon;
                   inCmdlen = 56;
                   inCmd = QCA(3);
                   monitor;
                    callp RunCommand(inCmd:inCmdLen);
                   on-error;
                   endmon;
                  // override with state file
                   @@cmd = qca(2);
                   @@st  = scAddSt;
                   inCmd = @@cmd;
                   monitor;
                    callp RunCommand(inCmd:inCmdLen);
                   on-error;
                   endmon;
                   @@cmd = qca(4);
                   @@st  = scAddSt;
                   inCmd = @@cmd;
                   monitor;
                    callp RunCommand(inCmd:inCmdLen);
                   on-error;
                   endmon;
                  // open file for use
                   erAddSt = *off;
                   monitor;
                    if not %open(FTFEP);
                       open FTFEP;
                    endif;
                    if not %open(FTFEL);
                       open FTFEL;
                    endif;
                   on-error;
                    msAddSt = 'WCR0513';
                    erAddSt = *on;
                    iter;
                   endmon;
                  // call program for selection of value
                   callp F2521R (##call:##kc:##kl:##ast:##rrn);
                   chain(n) ##rrn pfe01;
                   if %found(ftfep);
                      scAddToCd = %editc(FEdrg#:'X');
                      scAddTCdFo = *on;      // focus on the search field
                   endif;
                Endif;
                If scAddCdTyp = 'A' or scAddCdTyp = 'P' or scAddCdTyp = 'H';
                 // call program for selection of value
                   callp F2013R (##kc:##kl:##rrn:##proc:##prod:##rtcd:##ast);
                   if ##rtcd = '1' or ##prod = *blanks;
                   else;
                      scAddToCd = ##proc;
                      scAddTCdFo = *on;      // focus on the search field
                   endif;
                Endif;
             Endif;

          Other;
          Endsl;
       Enddo;
       unlock FARCP;
      /end-free
     P processAdd      E
       //===================================================================
       // procOther - process other options
       //===================================================================
     P procOther       B
     d procOther       PI
      /free

       ReadC sflRightCd;

       Select;

       // processing for Edit link
       When scRCEdt = *on;
          scRCEdt = *off;
          clear RcdAddRC;
          ##mode = 'C';
          BtnAddRcd = *off;
          BtnCanRcd = *off;
          processAdd();

       // processing for Delete link
       When scRCDlt = *on;
          scRCDlt = *off;
          Chain scRCRrn# prc0p;
          delete prc0p;
          unlock FARCP;

       // processing for Change Status link
       When scRCChgSts = *on;
          scRCChgSts = *off;
          Chain scRCRrn# prc0p;
          if RCstat = 'A';
             RCstat = 'I';
          else;
             RCstat = 'A';
          endif;
          update prc0p;
          unlock FARCP;
       Endsl;

      /end-free
     P procOther       E
       //===================================================================
       // Validate - Validate Add/Edit screen
       //===================================================================
     P Validate        B
     d Validate        PI
     c
      /free

       isValid  = *on;
       erAddEffDt = *off;
       erAddSt = *off;
       erAddCdTyp = *off;
       erAddFrmCd = *off;
       erAddToCd = *off;
       erAddPmnt = *off;
       erAddAnes = *off;

       If ##mode = 'A';

         // Effective Date is a required entry
         If %char(scAddEffDt) = *blanks or scAddEffDt = *loval;
            msAddEffDt = 'WCR0503';
            erAddEffDt = *on;
            isValid  = *off;
            return;
         Endif;

         // Effective Date must be valid
         // the screen field is a date field that enforces it

       EndIf;

       // State is a required entry
       If scAddSt = *blanks;
          msAddSt = 'WCR0503';
          erAddSt = *on;
          isValid = *off;
          return;
       Endif;

       // State must be valid
       If scAddSt <> *blanks;
          if scAddSt <> 'PA' and scAddSt <> 'NJ';
             msAddSt = 'WCR0513';
             erAddSt = *on;
             isValid = *off;
             return;
          endif;
          xxkey = scAddSt;
          Setll xxkey pst03;
          if not %equal(ststl3);
             msAddSt = 'WCR0513';
             erAddSt = *on;
             isValid = *off;
             return;
          endif;
       Endif;

       // Code Type is a required entry
       If scAddCdTyp = *blanks;
          msAddCdTyp = 'WCR0503';
          erAddCdTyp = *on;
          isValid  = *off;
          return;
       Endif;

       // Code Type must be valid
       Setll (company:'CDTP':scAddCdTyp) pgt01;
       If not %equal(ftgtl);
          msAddCdTyp = 'WCR0513';
          erAddCdTyp = *on;
          isValid  = *off;
          return;
       Endif;

       // From Code is a required entry
       If scAddFrmCd = *blanks;
          msAddFrmCd = 'WCR0503';
          erAddFrmCd = *on;
          isValid  = *off;
          return;
       Endif;

       // From Code must be valid
       If scAddFrmCd <> *blanks;
          If scAddCdTyp = 'D';
            // verify numeric data is present
             if %check('0123456789':scAddFrmCd) <> 0;
                msAddFrmCd = 'WCR0513';
                erAddFrmCd = *on;
                isValid = *off;
                return;
             endif;
            // if open close file
             if %open(FTFEL);
                close FTFEL;
             endif;
            // delete existing override
             inCmdlen = 56;
             inCmd = QCA(3);
             monitor;
              callp RunCommand(inCmd:inCmdLen);
             on-error;
             endmon;
            // override with state file
             @@cmd = qca(4);
             @@st  = scAddSt;
             inCmd = @@cmd;
             monitor;
              callp RunCommand(inCmd:inCmdLen);
             // open file for use
              if not %open(FTFEL);
                 open FTFEL;
              endif;
             on-error;
              msAddSt = 'WCR0513';
              erAddSt = *on;
              isValid = *off;
              return;
             endmon;
            // validate from FTFELxx
             yyKey = %dec(%trim(scAddFrmCd):5:0);
             Setll yykey pfe02;
             if not %equal(ftfel);
                msAddFrmCd = 'WCR0513';
                erAddFrmCd = *on;
                isValid = *off;
                return;
             else;
                scAddFrmCd = %editc(yykey:'X');
             endif;
          Endif;
          If scAddCdTyp = 'A' or scAddCdTyp = 'P' or
             scAddCdTyp = 'H';

             If scAddSt = 'NJ';

               // if open close file
                if %open(FMRCL1);
                   close FMRCL1;
                endif;
               // open file for use
                monitor;
                 if not %open(FMRCL1);
                    open FMRCL1;
                 endif;
                on-error;
                 msAddSt = 'WCR0513';
                 erAddSt = *on;
                 isValid = *off;
                 return;
                endmon;
               // validate from FMRCL1
                Setll (company:scAddFrmCd) prc01;
                if not %equal(fmrcl1);
                   msAddFrmCd = 'WCR0513';
                   erAddFrmCd = *on;
                   isValid = *off;
                   return;
                endif;

             Else;

               // if open close file
                if %open(FMPRL);
                   close FMPRL;
                endif;
               // delete existing override
                inCmdlen = 56;
                inCmd = QCA(5);
                monitor;
                 callp RunCommand(inCmd:inCmdLen);
                on-error;
                endmon;
               // override with state file
                @@cmd = qca(6);
                @@st  = scAddSt;
                inCmd = @@cmd;
                monitor;
                 callp RunCommand(inCmd:inCmdLen);
                // open file for use
                 if not %open(FMPRL);
                    open FMPRL;
                 endif;
                on-error;
                 msAddSt = 'WCR0513';
                 erAddSt = *on;
                 isValid = *off;
                 return;
                endmon;
               // validate from FMPRLxx
                Setll (company:scAddFrmCd) ppr01;
                if not %equal(fmprl);
                   msAddFrmCd = 'WCR0513';
                   erAddFrmCd = *on;
                   isValid = *off;
                   return;
                endif;
             EndIf;
          Endif;
          Select;
             When scAddCdTyp = 'A';
                If scAddFrmCd < '00100' or scAddFrmCd > '01999';
                   msAddFrmCd = 'WCR0513';
                   erAddFrmCd = *on;
                   isValid  = *off;
                   return;
                Endif;
             When scAddCdTyp = 'P';
                If scAddFrmCd < '10000' or scAddFrmCd > '99999';
                   msAddFrmCd = 'WCR0513';
                   erAddFrmCd = *on;
                   isValid  = *off;
                   return;
                Endif;
             When scAddCdTyp = 'H';
                If scAddFrmCd < 'A0001' or scAddFrmCd > 'Z9999';
                   msAddFrmCd = 'WCR0513';
                   erAddFrmCd = *on;
                   isValid  = *off;
                   return;
                Endif;
             When scAddCdTyp = 'D';
                xxProcCd = %dec(scAddFrmCd:5:0);
                If xxProcCd < 1 or xxProcCd > 999;
                   msAddFrmCd = 'WCR0513';
                   erAddFrmCd = *on;
                   isValid  = *off;
                   return;
                Endif;
          Endsl;
       Endif;

       // To Code is a required entry
       If scAddToCd = *blanks;
          msAddToCd = 'WCR0503';
          erAddToCd = *on;
          isValid  = *off;
          return;
       Endif;

       // To Code must be valid
       If scAddToCd <> *blanks;
          If scAddCdTyp = 'D';
            // verify numeric data is present
             if %check('0123456789':scAddToCd) <> 0;
                msAddToCd = 'WCR0513';
                erAddToCd = *on;
                isValid = *off;
                return;
             endif;
            // if open close file
             if %open(FTFEL);
                close FTFEL;
             endif;
            // delete existing override
             inCmdlen = 56;
             inCmd = QCA(3);
             monitor;
              callp RunCommand(inCmd:inCmdLen);
             on-error;
             endmon;
            // override with state file
             @@cmd = qca(4);
             @@st  = scAddSt;
             inCmd = @@cmd;
             monitor;
              callp RunCommand(inCmd:inCmdLen);
             // open file for use
              if not %open(FTFEL);
                 open FTFEL;
              endif;
             on-error;
              msAddSt = 'WCR0513';
              erAddSt = *on;
              isValid = *off;
              return;
             endmon;
            // validate from FTFELxx
             yyKey = %dec(%trim(scAddToCd):5:0);
             Setll yykey pfe02;
             if not %equal(ftfel);
                msAddToCd = 'WCR0513';
                erAddToCd = *on;
                isValid = *off;
                return;
             else;
                scAddToCd = %editc(yykey:'X');
             endif;
          Endif;
          If scAddCdTyp = 'A' or scAddCdTyp = 'P' or
             scAddCdTyp = 'H';

             If scAddSt = 'NJ';

               // if open close file
                if %open(FMRCL1);
                   close FMRCL1;
                endif;
               // open file for use
                monitor;
                 if not %open(FMRCL1);
                    open FMRCL1;
                 endif;
                on-error;
                 msAddSt = 'WCR0513';
                 erAddSt = *on;
                 isValid = *off;
                 return;
                endmon;
               // validate from FMRCL1
                Setll (company:scAddFrmCd) prc01;
                if not %equal(fmrcl1);
                   msAddFrmCd = 'WCR0513';
                   erAddFrmCd = *on;
                   isValid = *off;
                   return;
                endif;

             Else;

               // if open close file
                if %open(FMPRL);
                   close FMPRL;
                endif;
               // delete existing override
                inCmdlen = 56;
                inCmd = QCA(5);
                monitor;
                 callp RunCommand(inCmd:inCmdLen);
                on-error;
                endmon;
               // override with state file
                @@cmd = qca(6);
                @@st  = scAddSt;
                inCmd = @@cmd;
                monitor;
                 callp RunCommand(inCmd:inCmdLen);
                // open file for use
                 if not %open(FMPRL);
                    open FMPRL;
                 endif;
                on-error;
                 msAddSt = 'WCR0513';
                 erAddSt = *on;
                 isValid = *off;
                 return;
                endmon;
               // validate from FMPRLxx
                Setll (company:scAddToCd) ppr01;
                if not %equal(fmprl);
                   msAddToCd = 'WCR0513';
                   erAddToCd = *on;
                   isValid = *off;
                   return;
                endif;
             EndIf;
          Endif;
          Select;
             When scAddCdTyp = 'A';
                If scAddToCd < '00100' or scAddToCd > '01999';
                   msAddToCd = 'WCR0513';
                   erAddToCd = *on;
                   isValid  = *off;
                   return;
                Endif;
             When scAddCdTyp = 'P';
                If scAddToCd < '10000' or scAddToCd > '99999';
                   msAddToCd = 'WCR0513';
                   erAddToCd = *on;
                   isValid  = *off;
                   return;
                Endif;
             When scAddCdTyp = 'H';
                If scAddToCd < 'A0001' or scAddToCd > 'Z9999';
                   msAddToCd = 'WCR0513';
                   erAddToCd = *on;
                   isValid  = *off;
                   return;
                Endif;
             When scAddCdTyp = 'D';
                xxProcCd = %dec(scAddToCd:5:0);
                If xxProcCd < 1 or xxProcCd > 999;
                   msAddToCd = 'WCR0513';
                   erAddToCd = *on;
                   isValid  = *off;
                   return;
                Endif;
          Endsl;
       Endif;

       // Verify if the combination entered is not already present
       If isValid  = *on;
          efdtky = Dateto7(scAddEffDt);
          stky = scAddSt;
          typeky = scAddCdTyp;
          fcodky = scAddFrmCd;
          tcodky = scAddToCd;

          // if any of the unique combination value has changed,
          // verify the new combination does not already exist

          if sv_scAddEffDt <> efdtky or
             sv_scAddSt    <> stky or
             sv_scAddCdTyp <> typeky or
             sv_scAddFrmCd <> fcodky or
             sv_scAddToCd  <> tcodky;

            Setll (efdtky:stky:typeky:fcodky:tcodky) prc02;
            if %equal(farcl2);
               msAddEffDt = 'WC40160';
               erAddEffDt = *on;
               isValid = *off;
               return;
            endif;

          endif;
       Endif;

      /end-free
     P Validate        E
       //===================================================================
       // addDetails - Add Details
       //===================================================================
     P addDetails      B
     d addDetails      PI
     c
      /free

       Chain scRCRrn# prc0p;
       RCtype = scAddCdTyp;
       RCfcod = scAddFrmCd;
       RCtcod = scAddToCd;
       RCpmtt = scAddPmnt;
       RCaneh = scAddAnes;
       RCstat = scRCStat;

       If ##mode = 'A';
          RCefdt = Dateto7(scAddEffDt);
          RCst   = scAddSt;
          RCstat = 'A';
          RCuser = q1user;
          RCpgm  = q1pgm;
          RCdt   = Dateto7(SystemDate());
          RCtime = Timeto6(SystemTime());
          RCiusr = q1user;
          RCipgm = q1pgm;
          RCidt  = Dateto7(SystemDate());
          RCitim = Timeto6(SystemTime());
          Write prc0p;
       Else;
          RCuser = q1user;
          RCpgm  = q1pgm;
          RCdt   = Dateto7(SystemDate());
          RCtime = Timeto6(SystemTime());
          Update prc0p;
       Endif;
       Unlock FARCP;

      /end-free
     P addDetails      E
       //===================================================================
       // MoveDbSc - Move database record to screen
       //===================================================================
     P MoveDbSc        B
     d MoveDbSc        PI
     c
      /free

       scAddEffDt = CvtToDate(RCefdt);
       scAddSt    = RCst;
       scAddCdTyp = RCtype;
       scAddFrmCd = RCfcod;
       scAddToCd  = RCtcod;
       scAddPmnt  = RCpmtt;
       scAddAnes  = RCaneh;

       sv_scAddEffDt = Dateto7(scAddEffDt);
       sv_scAddSt    = scAddSt;
       sv_scAddCdTyp = scAddCdTyp;
       sv_scAddFrmCd = scAddFrmCd;
       sv_scAddToCd  = scAddToCd;

      /end-free
     P MoveDbSc        E
       //===================================================================

**  QCMDEXC TABLE
DLTOVR FILE(FTFEP)
OVRDBF FILE(FTFEP) TOFILE(*LIBL/FTFEP  )
DLTOVR FILE(FTFEL)
OVRDBF FILE(FTFEL) TOFILE(*LIBL/FTFEL  )
DLTOVR FILE(FMPRL)
OVRDBF FILE(FMPRL) TOFILE(*LIBL/FMPRL  )
