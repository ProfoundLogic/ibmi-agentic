     hcopyright('(c) 2022 Sapiens, Inc. All rights reserved.')
     h**********************************************************************
     h/title WTCLMFROI- FROI Intake Process
     h dftname(WTCLMFROI)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Called to Display and Process FROI new Screen.
     h*
     h*  Called from: W0071R (old program)
     h*  Calls to: WTCLMFROIP, WTCLMADD
     h*
      //*********************************************************************
      //==============================================================*
      //  Created   : 07/20/22                                         *
      //  Programmer: C:333                                            *
      //  Project   : WC2824- FROI Intake CS Standards                 *
      //==============================================================*
      // Revisions:                                                    *
      //                                                               *
      // 08/08/23 C:342 Support: 113261 (Prescient)                    *
      //                Add Delete option link in FROI list grid.      *
      //                                                               *
      //===============================================================*

      * FROI Intake Process File
     fWTCLMFROI cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflFroiLst:rrnFroi)
      * Action panel display file
     fwtactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdcompnl)
     f*
      * company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      *  Physical file definition (for refreshing subfile record)
     fwhfrl2    if   e           k disk    infds(dbfds)
     f                                     rename(PFR01:pfr0l2)
      * FROI Work File
     fWHFRP     if   e             disk
      * Employer Master File - Logical
     fWMEML     if   e           k disk
      * Fund master File - logical
     fWMFNL     if   e           k disk
      * Logical over all table files
     fWTABL     if   e           k disk
      *

     fwmcmlb    if   e           k disk
     f* Employer Detail File - logical
     fWDELL2    if   e           k disk
     f* Pending FROI Add Claim Errors
     fWHERP     iF a e           k disk

      // work variables
     d @@call          s              1
     d isPosDupClm     s               n
     d indexTab        s              5  0
     d outCltId        s              3    inz
     d process         s               n   inz(*off)
     d xxStat          s                   like(FRSTAT)
     d xxDESC          s                   like(ERDESC)
     d xxErrCnt        s              2  0
     d xxrrnfroi       s              9  0 inz
     d Q               C                   CONST('''')
     d xxAcc_Dt        s               d   datfmt(*usa)
     d xxAcc_Dt1       s                   like(cmacdt)
     d y               s              1  0
     d ##rrn           s              9
     d xxSS            s              9
     d xxcount         s              5  0 inz(1)
     d**** Beg Add *** 08/08/23 **************************************
     d isDelResp       s               n   inz
     d**** End Add *** 08/08/23 **************************************
     d*
     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  dbrrn                397    400b 0

     d                 ds                  inz
     d  ed                     1     35  0
     d                                     dim(5) ascend
     d  emefd1                 1      7  0
     d  emefd2                 8     14  0
     d  emefd3                15     21  0
     d  emefd4                22     28  0
     d  emefd5                29     35  0

     d                 ds                  inz
     d  cd                     1     35  0
     d                                     dim(5) ascend
     d  emcad1                 1      7  0
     d  emcad2                 8     14  0
     d  emcad3                15     21  0
     d  emcad4                22     28  0
     d  emcad5                29     35  0

      *

      // external program WTCLMFROIP
     d DisplayFROI     pr                  extpgm('WTCLMFROIP')
     d                                9  0

      // external program WTCLMADD
     d FroiAddClaim    pr                  extpgm('WTCLMADD')
     d                                9

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      // Load FROI Grid
     d LoadFroiGrid    pr

      // Main Processing
     d Main            pr

      // ProcOther
     d ProcOther       pr

      // program start
     d programStart    pr
     d processMe                       n

     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrAtrium
     d/copy sPrMask

      // Entry of Program.
     c     *entry        plist
     c                   parm                    @@call


      /free
       // main program processing

       programStart(process);

       If process;
          main();
       Endif;

       *inlr = *on;
       Return;

      /end-free

       //===================================================================
       // Load Froi Intake Grid
       //===================================================================
     p LoadFroiGrid    b
     d LoadFroiGrid    pi

      /free
        // clear Sub-files
        rrnFroi   = 0;
        clrFroi   = *On;
        dspFroi   = *Off;
        dspFroiErr= *Off;
        Write rcdFroiInt;

        // set subfile indicators
        clrFroi = *off;
        dspFroi = *on;
        dspfroierr = *on;

        exec sql
           Set Option
              commit = *none;
        exec sql
           Delete from WHERP;

        // Populate the subfile
        xxStat ='P';
        scLoadCnt = xxcount;
        Setll (scCo#:xxStat) pfr0l2;
        Reade (scCo#:xxStat) pfr0l2;
        Dow not %eof(whfrl2);

           Exsr clrScFld;

           If FRrcvd <> ' ';
              scRcvDt   =   dtMDYY2(%dec(FRrcvd:8:0));
           Else;
              scRcvDT   = 0;
           Endif;

           If FRlsn <> ' ' and FRfsn <> ' ';
              scNameCl  =   %trim(Frlsn)+', '+%trim(FRfsn);
           Elseif FRlsn = ' ';
              scNameCl  = ' , '+%trim(FRfsn);
           Else;
              scNameCl  = %trim(FRlsn);
           Endif;

           If FRSS <> ' ';
              xxSS      =   MaskData('SSN':'FRSS':FRSS);
              scSS      =   %Subst(xxSS:1:3)+'-'+%Subst(xxSS:4:3)+'-' +
                            %Subst(xxSS:7:3);
           Else;
              scSS = *blanks;
           Endif;

           scSS_Sql  =   FRSS;                            // for Ajex sql

           If FRacdt <> ' ';
              scInjDt   =   dtMDYY2(%dec(FRacdt:8:0));       // YYYYMMDD to MMDDYYYY
           Else;
              scInjDT   = 0;
           Endif;

           scEmpNo   =   FRemp;
           if FRemp <> *zeros ;
              Chain (FRco:FRfnd:FRemp:FRdiv) pem01;
              If %found;
                  scNameEmp = EMname ;
              Endif;
           Endif;
           scGrp     = FRfnd;
           scStatus  = 'Pending';

           //------------ Detail Box Info start -----------------//
           scClAdd1  = FRadr1;
           if FRst <> ' ';
           scClAdd2  = %trim(FRcty)+', '+%trim(FRst);
           Else;
              scClAdd2 = %trim(FRcty);
           Endif;

           scClAdd3  = FRzip;

           scAccDes  = %trim(FR80a1)+''+%trim(FR80a2);

           If FRdtrp <> ' ';
              scDtrp = dtMDYY2(%dec(FRdtrp:8:0));
           Else;
              scDtrp = 0;
           Endif;

           if FRnatr <> ' ';
              Chain (FRco:Frnatr) pna01;
              If %found;
                  scNature  = %trim(NAkey) +' - ' + %trim(NAdesc);
              Endif;
           Endif;

           if FRpart <> ' ';
              Chain (FRco:FRpart) ppa01;
              If %found;
                  scPART    = %trim(PAkey) +' - ' + %trim(PAdesc);
              Endif;
           Endif;

           if FRcaus <> ' ';
              Chain (FRco:FRcaus) pca01;
              If %found;
                  scCause   = %trim(CAkey) +' - ' + %trim(CAdesc);
              Endif;
           Endif;


           if FRfnd <> *zeros ;
              Chain (FRco:FRfnd) pfn01;
              If %found;
                  scGrpNm = FNname;
              Endif;
           Endif;
           //------------ Detail Box Info End  -----------------//

           // ------------ Felds for Error File ----------//

           scTime = FRtime;
           scDate = FRdate;

           // --------------------------------------------//

           exsr addValidation;                              // Validation for ADD Claim

           scErrCnt = xxErrCnt;
           isPosDup = isPosDupClm;

           scrrnFroi = dbrrn;
           rrnFroi += 1;
           if rrnFroi = 1;
              scFroiSel = *on;
           else;
              clear scFroiSel;
           Endif;

           Write sflFroiLst;

           Reade (scCo#:xxStat) pfr0l2;
        Enddo;
        rrnFroi = 1;

        //===================================================================
        // Clear Screen Fileds
        //===================================================================
        BegSr clrScfld;
         scRcvDt  = *zeros;
         scNameCl = *blanks;
         scSS     = *blanks;
         scInjDt  = *zeros;
         scEmpNo  = *zeros;
         scNameEmp= *blanks;
         scGrp    = *zeros;
         scStatus = *blanks;
         scClAdd1 = *blanks;
         scClAdd2 = *blanks;
         scClAdd3 = *zeros;
         scAccDes = *blanks;
         scNature = *blanks;
         scPART   = *blanks;
         scCause  = *blanks;
         scGrpNm  = *blanks;
         scTime   = *zeros;
         scDate   = *blanks;
        Endsr;

        //===================================================================

        //===================================================================
        // Validation for ADD Claims
        //===================================================================
        BegSr addValidation;

         xxErrCnt = 0;

         //*******  Check for duplicate claims **********//
         isPosDupClm = *Off;
         Chain (FRco:FRSS) pcm01;
         If %found;
            isPosDupClm = *On;
            xxErrCnt +=1;
            // Logging Errors to Files
            xxDESC = 'Possible duplicate claim exists.';
            exsr wrtDBErr;
         Endif;

         //******* Validate Employer number *************//
         Chain (FRco:FRfnd:FRemp:FRdiv) pem01;
         If not %found;
            xxErrCnt +=1;
                               // Logging Errors to Files
            xxDESC = 'Invalid Employer.';
            exsr wrtDBErr;
         Else;                                            //Accident Date should within +
            xxAcc_Dt1 = dtCYMD(scInjDT);                  // employer effective/cancellation +
                                                          // date range.
            y = %lookup(0:ed);
            if y = 0;
               y = 5;
            else;
               y -=1;
            Endif;

            Dow y <> 0;
               If xxAcc_Dt1 >= ed(y);
                  leave;
               else;
                  y -=1;
              Endif;
            Enddo;

            If y = *zeros;
               xxErrCnt += 1;
                                        // Logging Errors to Files
               xxDESC = 'The accident date doesn'+Q+'t fall within '+
                        'the effective and cancellation date ranges in'+
                             ' the Employer Master File.';
               exsr wrtDBErr;
            Else;
               If cd(y) = xxAcc_Dt1 and cd(y) > *zeros;
                  xxErrCnt += 1;
                                        // Logging Errors to Files
                  xxDESC = 'The accident date doesn'+Q+'t fall within '+
                           'the effective and cancellation date ranges in'+
                          ' the Employer Master File.';
                  exsr wrtDBErr;

               Endif;
            Endif;

         EndIf;

         //******* Validate Social Security Number *****************//
         IF FRSS = *blanks or FRSS = *zeros;
            xxErrCnt +=1;
                                  // Logging Errors to Files
            xxDESC = 'Invalid Social Security Number.';
            exsr wrtDBErr;
         EndIf;

         //******* Validate Accident Date (YYYYMMDD) **************//

         IF scInjDT = *zeros;                               // InValid Date Format
            xxErrCnt +=1;
                                  // Logging Errors to Files
            xxDESC = 'Invalid '+Q+'Date of Injury'+Q+'.';
            exsr wrtDBErr;
         Else;

         //**** Validate Accident Date less than System date ****//

            xxAcc_Dt =  %date(scInjDT : *usa);              //Output MM/DD/YYYY
            If xxAcc_Dt > %Date();
               xxErrCnt +=1;
                                  // Logging Errors to Files
               xxDESC = 'Date of Injury must be less or equal to Today'+Q+
                        's date.';

               exsr wrtDBErr;
            Endif;

           //****** Verify policy year coverage (WDELP) ******************//

            SETGT (FRco:FRfnd:FRemp:FRdiv:xxAcc_Dt1) pel01;
            Readpe (FRco:FRfnd:FRemp:FRdiv) pel01;

            If ELpfdt > xxAcc_Dt1
               or ELptdt <= xxAcc_Dt1
               or ELpfdt = *Zeros;

               xxErrCnt +=1;
                                  // Logging Errors to Files
               xxDESC = 'The accident date doesn'+Q+'t fall within the '+
                          'policy start and end dates in the Fund Detail File.';

               exsr wrtDBErr;
            Endif;

         Endif;
        Endsr;

        //===================================================================
        //===================================================================
        // Write Error to WHERP
        //===================================================================
        BegSr wrtDBERR;
         chain (frfnd:fremp:frtime:frdate:frss:xxdesc) per01;
         If not %found;
            ERco   = FRco;
            ERfnd  = FRfnd;
            ERemp  = FRemp;
            ERSS   = FRSS;
            ERdate = FRdate;
            ERtime = FRtime;
            ERdesc = xxdesc;
            write per01;
         Endif;
        Endsr;

        //===================================================================

      /end-free
     p LoadFroiGrid    e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
      /free
       // process until exit button is pressed.
       Dou ExitNow = *On;

          // load Froi List information
          LoadFroiGrid();

          // process Froi List screen
          Exsr exfmtscreens;

          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             reset xxcount;
             btnRefresh =*Off;

          other;
             ProcOther();
          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        indexTab = TabIndex + 1;

        Select;
        When indexTab = 1;
           pnlHeight = 720;
           APnlHeight = pnlHeight + 60;

           Write rcdComPnl;
           Write rcdPanel;
           Exfmt rcdFroiInt;
           Read  rcdComPnl;
           Read rcdPanel;

        EndSl;

       Endsr;

      /end-free
     p Main            e
       //==================================================================

       //===================================================================
       // ProcOther - Process Add and Display link
       //===================================================================
     p ProcOther       B
     d ProcOther       pi
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)
      /free

       isexit = *off;
       ReadC sflFroiLst;
       Dow not %eof;
          Select;
          When isAddClm = *On;                  // Processing for Add Claim
             xxcount += 1;
             isAddClm = *Off;
             //mode = 'C';              //-----------> Not required as of now
             xxrrnFroi = scRrnFroi;            //Save Rrn value for Add Claim

             Chain xxrrnFroi pfr01;
             If scErrCnt <> *zeros or FRstat <> 'P';
                If scErrCnt = 1 And isPosDup = *On;
                   PopOneBtn('Possible Duplicate Claim: Info Only.':'Error');
                   ##rrn = %editc(xxRrnFroi:'X');
                   FroiAddClaim(##rrn);                           // calling WTCLMADD
                Else;
                   PopOneBtn('Can'+Q+'t Add Claim due to Error.':'Error');
                   isexit = *on;
                EndIf;
             Else;
                ##rrn = %editc(xxRrnFroi:'X');
                FroiAddClaim(##rrn);                                 // calling WTCLMADD
             Endif;

          When isDspFroi = *On;                                      // Displaying FROI
             xxcount += 1;
             isDspFroi = *Off;

             If scGrp <> *zeros;
                Chain (FRco:scGrp) pfn01;
                If not %found;
                   PopOneBtn('Valid Group required.':'Error');
                Else;
                   If FRcaus = ' ';
                      PopOneBtn('Type of Cause required.':'Error');
                   Else;
                      If scDtrp = *zeros;
                         PopOneBtn('Date Employer Notified required.':'Error');
                      Else;
                         DisplayFroi(scRrnFroi);                     // calling WTCLMFROIP
                      Endif;
                   Endif;
                Endif;

             Else;
                PopOnebtn('No Group in FROI record.':'Error');
             Endif;

          //*** Beg Add *** 08/08/23 ************************************
          When isDltFroi = *On;                                      // Delete FROI
             isDltFroi = *Off;

             Chain(e) scRrnFroi Whfrp;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRrnFroi Whfrp;
             Dow isexit = *off;
                isDelresp = PopTwoBtn('You are about to delete this FROI, +
                do you want to continue?':'Delete FROI':'Yes':'No');
                Select;
                When isDelresp = *On;
                   Exec SQL
                    Update WHFRP
                     Set FRstat = 'D'
                     Where rrn(whfrp) = :scRrnFroi;
                   PopOneBtn('FROI Deleted!':'Delete':'Ok');
                   isexit = *on;
                When isDelresp = *Off;
                   isexit = *on;
                EndSl;
             Enddo;
          //*** End Add *** 08/08/23 ************************************
          Endsl;
          ReadC sflFroiLst;
       Enddo;

      /end-free
     p ProcOther       E

       //===================================================================

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       processMe = *on;

       // get company of the user
       scCo# = getUserInfo(q1user);
       Chain scCo# pco00;
       If coNAME <> ' ';
          scCompName = coNAME;
       Endif;


       // Get Client ID
       getClientID(outCltId);

      /end-free
     p ProgramStart    e

       //===================================================================

