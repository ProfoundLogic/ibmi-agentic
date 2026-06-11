     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H**********************************************************************
     H/TITLE FPINETDISC - Managed Care Network Discount Module
     H NOMAIN
     H BNDDIR('FTBNDDIR')
     h**********************************************************************
     h**   PROGRAM DESCRIPTION:                                       *
     h**                                                              *
     h**    This program is used to display a screen to maintain the  *
     h**    network discount codes                                    *
     h**                                                              *
      *===============================================================*
      *  Created   : 03/18/15                                         *
      *  Programmer: C:212                                            *
      *  Project   : 114MRC                                           *
      *===============================================================*
      *  Revisions:                                                   *
      *                                                               *
      *   05/21/19 C:180   Project:MC432                              *
      *                    - Add D7 processing                        *
      *                    - Fix RRN parm error (Previous issue prior *
      *                      to project MC432)                         *
      *                                                               *
      *   03/03/17 C:309  Project: 114MEZ                             *
      *                   Recompile only (done screen changes)        *
      *                                                               *
      *  05/06/16 C:212  Project: 114MYZ                              *
      *                  add option for flat amount for ReEval in DSPF*
      *                  and related processing                       *
      *                                                               *
      * ------------------------------------------------------------- *
      *  NOTE: Please add comments to the top of Revisions.           *
      * ------------------------------------------------------------- *
      *===============================================================*

     f* file definitions
     fFPINETDISCcf   e             workstn usropn
     f                                     HANDLER('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflNova:rrnNova)
      *** Begin Add *** 05/21/19 *************************************
     f                                     Sfile(sflD7:rrnD7)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
      *** End   Add *** 05/21/19 *************************************
     fndcop     uf a e             disk    rename(pco01:pco0p)
     f                                     usropn
     fndcol     if   e           k disk    infds(dbfds1)
     f                                     usropn
     fndcol2    if   e           k disk    rename(pco01:pco02)
     f                                     usropn
     fnmbdl5    uf a e           k disk    usropn
     fftgtl     if   e           k disk    usropn

     d* prototype definitions
     d*
     d* network discount
     d networkDisc     pr
     d inType                         2    const
     d inPrDs                        40    const
     d inCall                         1    const
     d inDsCode                       3    const
     d inEfdt                         8    const
     d inRrn                          9    const options(*nopass)
     d inTabInd                       5    const options(*nopass)

     d* NovaCare network discount
     d NovaCareDisc    pr
      *
      **** Begin Add *** 05/21/19 *************************************
     d* D7 network discount
     d D7Disc          pr
      **** Begin Add *** 05/21/19 *************************************

     d* program Start
     d programStart    pr
     d processMe                       n

     d* program end
     d programEnd      pr

     d RunCommand      pr                  extPgm('QCMDEXC')
     d inCmd                       2750    const
     d inCmdLen                      15  5 const

     d FA753R          pr                  extPgm('FA753R')
     d                                3    const
     d                                2    const
     d                                1    const
     d                                7  0 const
     d                                5    const
     d                                2    const
     d                               10    const
     d                               10    const
     d                               10    const
     d                               10    const
     d                               10    const
     d                               80    const
     d                               80    const

     d RtvCltId        pr                  extPgm('RTVCLTID')
     d                                3

     d* data structures
     d*
     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  $rrnndcol            397    400b 0

     d* array definitions
     d*

     d* constant definitions
     d*
     d $AlphaNum       c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ+
     d                                     abcdefghijklmnopqrstuvwxyz+
     d                                     0123456789 '
     d $lo             c                   'abcdefghijklmnopqrstuvwxyz'
     d $up             c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

     d* standalone variables
     d*
     d $DsType         s              2    inz
     d $PrDs           s             40    inz
     d $Mode           s              1    inz
     d $DsCode         s              3    inz
     d $Efdt           s              7p 0 inz
     d $Rrn            s              9p 0 inz
     d $TabInd         s              5p 0 inz

     d $Cmd            s           2750    inz
     d $CmdLen         s             15  5 inz

     d $co#            s              3p 0 inz
     d $dsdesc         s             40    inz
     d $com1           s             60    inz
     d $com2           s             60    inz

      *** Begin Add *** 05/21/19 ***********************************
     d @@Code          s              3    inz
     d @@TYPE          s              2    inz
     d @@RCDT          s              1    inz
     d @@EFDT          s              7  0 inz
     d @@CPT4          s              5    inz
     d @@PMOD          s              2    inz
     d @@10A1          s             10    inz
     d @@10A2          s             10    inz
     d @@10A3          s             10    inz
     d @@10A4          s             10    inz
     d @@FLD           s             10    inz
     d @@FVAL          s             80    inz
     d @@TVAL          s             80    inz
     d CltId           s              3    inz

     d HDcom1          s                   like(BDcom1) inz
     d HDcom2          s                   like(BDcom2) inz
     d HDam01          s                   like(BDam01) inz
     d HDam02          s                   like(BDam02) inz

     d @UWFLh          s              1    inz
     d @UWFLd          s              1    inz
     d str9999Msg      s            256
     d isFilter        s              1n   inz
     d svFilter        s              1n   inz
     d isOverflow      s              1n   inz
     d ssPrCdFrom      s                   like(COcpt4) inz
     d ssPrCdTo        s                   like(COcpt4) inz
     d str999Txt1      c                   CONST('Too many records returned -
     d                                     for discount code D7. Enter the -
     d                                     From and To Codes to filter the  -
     d                                     results')
     d str999Txt2      c                   CONST('or Click Cancel to -
     d                                     see the first 999 records.')

      *** End   Add *** 05/21/19 ***********************************

     d* copybook declarations
     d*
     d/copy sPrUsrPrms
      *** Begin Add *** 05/21/19 ***********************************
     d/copy sPrCompany
     d/copy sPrGenPop
      *** End   Add *** 05/21/19 ***********************************

     P* procedure implementations
     P*====================================================================
     P*  NetworkDisc - maintain network discount
     P*====================================================================
     P NetworkDisc     B                   Export
     D NetworkDisc     PI
     D inType                         2    const
     D inPrDs                        40    const
     D inCall                         1    const
     D inDsCode                       3    const
     D inEfdt                         8    const
     D inRrn                          9    const options(*nopass)
     D inTabInd                       5    const options(*nopass)
     D*
     d* standalone variables
     d process         s               n
     D*
      /free

       // Discount type
       If %parms >= 1;
          $DsType = inType;
       Endif;

       // PrDs data
       If %parms >= 2;
          If inPrDs <> *blanks;
             $PrDs = inPrDs;
          Else;
             $PrDs = 'Discount Code Information';
          Endif;
       Endif;

       // Program Mode
       $mode = 'D';
       If %parms >= 3;
          if inCall = '1' or inCall = 'D';
             $mode = 'D';
             *in09 = *on;
          endif;
          if inCall = ' ' or inCall = 'C';
             $mode = 'C';
             *in09 = *off;
          endif;
          if inCall = 'A';
             $mode = 'A';
             *in09 = *off;
          endif;
       Endif;

       // Discount Code
       If %parms >= 4;
          $DsCode = inDsCode;
       Endif;

       // Effective Date
       If %parms >= 5;
          Monitor;
           $EfDt = %dec(%char(%date(inEfdt:*iso0):*cymd0):7:0);
          on-error;
           $Efdt = 0010101;
          Endmon;
       Endif;

       //*** Begin Delete ***  05/21/19 ***********************************
       //If %parms >= 6;
          //if inRrn <> *blanks;
          //   $Rrn = %int(inRrn);
          //else;
          //   $Rrn = 1;
          //endif;
       //Else;
          //$Rrn = 1;
       //Endif;
          //*** End Delete***  05/21/19 ***********************************

       // Tab Index
       If %parms >= 7;
          if inTabInd <> *blanks;
             $TabInd = %int(inTabInd);
          else;
             $TabInd = 0;
          endif;
       Else;
          $TabInd = 0;
       Endif;

       programStart(process);

       If process = *on;

          Select;

           When $dstype = 'DX';
            NovaCareDisc();
       //*** Being Add *** 05/21/19 **************************************
           When $dstype = 'D7';
            D7Disc();
       //*** End   Add *** 05/21/19 **************************************
           Other;

          EndSl;

       Endif;

       ProgramEnd();

       *inlr = *on;
       Return;

      /end-free
     P NetworkDisc     E

     P*====================================================================
     P*  NovaCareDisc - maintain network discount for NovaCare
     P*====================================================================
     P NovaCareDisc    B                   Export
     D NovaCareDisc    PI
     D*
     d* standalone variables
     d*
     d ##mode          s              1    inz
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isValid         s               n   inz(*off)
     d #IndGrEvt       s               n   inz(*off)

      /free

       Select;
       When $mode = 'D';
          dspMode = *On;
          dspVisb = *Off;
       When $mode = 'A';
          addMode = *On;
          dspVisb = *On;
       When $mode = 'C';
          chgMode= *On;
          dspVisb = *On;
       Endsl;

       // process until exit button is pressed.
       Dou ExitNow = *On;

          // clear Sub-files
          Exsr ClrSubfiles;

          // load discount header info
          Exsr LoadDsHdr;

          // load discount detail info
          Exsr LoadNCDtl;

          // display and process discount screen
          Exsr ExfmtScreens;

          Select;

          // do nothing - catch tab click event
          When scBtnTab = *on;
             scBtnTab = *off;

          When BtnRefresh = *On;
             BtnRefresh =*Off;

          When scBtnCan = *On;
             scBtnCan = *Off;
             leave;

          When scBtnSave = *on;
             scBtnSave = *Off;
             ##mode = $mode;
             // process header data
             Exsr processHdr;
             // process other data
             Exsr procNCDtl;

          // grid add button processing
          When scNovaAdd = *on;
             clear RcdAddNC;
             ##mode = 'A';
             #IndGrEvt = *on;
             Exsr procAddGrid;

          // other options processing
          Other;
             Exsr procOther;

          Endsl;

       Enddo;

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
       BegSr ClrSubFiles;

        rrnNova = 0;
        clrNovaSub = *On;
        dspNovaSub = *Off;
        dspNovaCtl = *Off;
        Write RcdNova;

       EndSr;

       //===================================================================
       // LoadDsHdr - Load Header Information
       //===================================================================
       Begsr LoadDsHdr;

        // If Grid event happens do not re-load the rest of screen else do
        If #IndGrEvt = *off;

          // get discount comments
           Chain(n) ($dscode:$efdt) pbd01;
           If %found(nmbdl5);
              $Com1 = BDCom1;
              $Com2 = BDCom2;
           Endif;

           // get discount heading
           Chain ($co#:'DSTY':$dstype) pgt01;
           If %found(ftgtl);
              $DsDesc = GTdesc;
           Endif;

           // load header fields
           scHdDsHdg = $prds;
           scHdDsTy = $dstype;
           scHdDsTyDs = $dsdesc;
           scHdDsCd = $dscode;
           scHdEfDt = %date($efdt:*cymd);
           scHdCom1 = $com1;
           scHdCom2 = $com2;
           //*** Begin Add *** 05/21/19 *************************************
           scL2Pos = 255;
           //*** End Add *** 05/21/19 *************************************
           scHMsgTop = 610;
        EndIf;

       EndSr;

       //===================================================================
       // LoadNCDtl - Load NovaCare Detail Information
       //===================================================================
       BegSr LoadNCDtl;

        // If Grid event happens do not re-load the rest of screen else do
        If #IndGrEvt = *off;

           // Load NovaCare fields

           Chain(n) ($dscode:$efdt) pbd01;
           If %found(nmbdl5);
              scPTEvals = BDAm01;
              scPTReEval = BDAm02;
              scPTAllpd = BDAm03;
              scPTAllFs = BDAm04;
              scPTExcept = BDAm05;
              //***** Beg Add *** 05/06/16 *********************************
              scPTReEvFL = BDAm06;
              //***** End Add *** 05/06/16 *********************************
           Endif;

        Endif;

        // Load NovaCare Grid

        // set subfile indicators
        clrNovaSub = *Off;
        dspNovaSub = *On;
        dspNovaCtl = *On;

        // read file and load the subfile
        Setll ($dscode:$efdt) pco01;
        Reade ($dscode:$efdt) pco01;
        dow not %eof(ndcol);
           clear sflNova;

           scGrPrCd = COcpt4;
           scGrMod  = COpmod;
           scGrSts  = COstat;

           scNovaRRN# = $rrnndcol;

           rrnNova = rrnNova + 1;
           If rrnNova = 1;
              scNovaRwSl = 'A';
           Else;
              clear scNovaRwSl;
           Endif;
           If $mode = 'D';
              dspVisb = *Off;
           Else;
              dspVisb = *On;
           Endif;

           Write sflNova;

           Reade ($dscode:$efdt) pco01;
        Enddo;

        rrnNova = 1;

       EndSr;

       //===================================================================
       // ExfmtScreens - Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        Select;

        When TabIndex = 0;

           //Begin Change *** 05/21/19 ***************************************
           //TabHeight = 1050;
           TabHeight = 560;
           APnlHeight = TabHeight + 60;
           Write RcdSysTbl;
           //End   Change *** 05/21/19 ***************************************

           Write RcdPanel;
           Write RcdDiscHdr;
           Exfmt rcdNova;
           //Begin Add *** 05/21/19 ***************************************
           Read RcdSysTbl;
           //End   Add *** 05/21/19 ***************************************
           Read RcdPanel;
           Read RcdDiscHdr;

           clear scHdrMsg;
           dspHdrMsg = *off;

           #IndGrEvt = *off;

        EndSl;

       Endsr;

       //===================================================================
       // ProcAddGrid - Process addition in Grid
       //===================================================================
       BegSr ProcAddGrid;

        // processing for Edit mode
        If ##mode = 'C';

         // code should be disabled in edit mode
           dsbAddPrCd = *on;

         // Save/Add Next should be disabled in edit mode
           dsbAddSvNx = *on;

           Chain(n) scNovaRrn# pco0p;
           Exsr MovDbScAdd;

        Endif;

        // processing for Add mode
        If ##mode = 'A';

         // code should not be disabled in edit mode
           dsbAddPrCd = *off;

        Endif;

        Dou BtnAddCan = *on or (BtnAddSav = *on and isValid = *on);
           Exfmt RcdAddNC;
           clear scAddMsg;
           dspAddMsg = *off;
           Select;

           When BtnAddSav = *on or BtnAddSvNx = *on;
              isValid = *on;
              Exsr ValidGrAdd;
              If isValid = *on;
                 // add data
                 Exsr addGrData;
              Else;
                 // show invalid message
                 Iter;
              Endif;
              If BtnAddSav = *on;
                 BtnAddSav = *off;
                 BtnAddSvNx = *off;
                 Clear RcdAddNC;
                 Leave;
              Endif;
              If BtnAddSvNx = *on;
                 BtnAddSvNx = *off;
              Endif;

           When BtnAddCan = *on;
              BtnAddCan = *off;
              Clear RcdAddNC;
              Leave;

           Other;
           Endsl;
        Enddo;
        unlock ndcop;

       EndSr;

       //===================================================================
       // ProcessHdr - Process Header
       //===================================================================
       BegSr ProcessHdr;

        If ##mode = 'A' or ##mode = 'C';

           isValid = *on;
           Exsr validHdr;
           If isValid = *on;
              Exsr addHdrData;
           Endif;

        EndIf;

       EndSr;

       //===================================================================
       // ProcNCDtl - Process NovaCare Detail fields
       //===================================================================
       BegSr ProcNCDtl;

        If ##mode = 'A' or ##mode = 'C';
           If dspHdrMsg = *on;         // Header is processed successfully

              dspHdrMsg = *off;
              isValid = *on;
              Exsr validNCDtl;
              If isValid = *on;
                 Exsr addNCDtl;
              Endif;

           Endif;
        EndIf;

       EndSr;

       //===================================================================
       // ProcOther - Process Other options
       //===================================================================
       BegSr ProcOther;

        ReadC sflNova;

        Select;

        // processing for Edit link
        When scGrEdt = *on;
           scGrEdt = *off;
           clear RcdAddNC;
           ##mode = 'C';
           #IndGrEvt = *on;
           Exsr procAddGrid;

        // processing for Delete link
        When scGrDlt = *on;
           scGrDlt = *off;
           #IndGrEvt = *on;
           Chain scNovaRrn# pco0p;
           delete pco0p;
           unlock ndcop;

        // processing for Change Status link
        When scGrChgSts = *on;
           scGrChgSts = *off;
           #IndGrEvt = *on;
           Chain scNovaRrn# pco0p;
           if COstat = 'A';
              COstat = 'I';
           else;
              COstat = 'A';
           endif;
           update pco0p;
           unlock ndcop;
        Endsl;

       EndSr;

       //===================================================================
       // ValidHdr - Validate Header
       //===================================================================
       BegSr ValidHdr;

        isValid = *on;
        // validation on discount code/type, duplicacy already in N2330R01

       EndSr;

       //===================================================================
       // addHdrData - Add Header Data
       //===================================================================
       BegSr addHdrData;

        Chain ($dscode:$efdt:$dstype) pbd01;

        If not %found (nmbdl5);

           clear pbd01;
           BDcode = scHdDsCd;
           BDefdt = %dec(%char(scHdEfDt:*cymd0):7:0);
           BDcom1 = scHdCom1;
           BDcom2 = scHdCom2;
           BDtype = scHdDsTy;
           BDrcdt = 'D';
           BDstat = 'A';
           BDuser = q1user;
           BDpgm  = q1pgm;
           BDtime = %dec(%char(%time():*iso0):6:0);
           BDdt   = %dec(%char(%date():*cymd0):7:0);
           BDiusr = q1user;
           Write pbd01;
           scHdrMsg = 'This record has been added';
           dspHdrMsg = *on;
        Else;
           BDcom1 = scHdCom1;
           BDcom2 = scHdCom2;
           BDuser = q1user;
           BDpgm  = q1pgm;
           BDtime = %dec(%char(%time():*iso0):6:0);
           BDdt   = %dec(%char(%date():*cymd0):7:0);
           Update pbd01;

           //*** Begin Change *** 05/21/19 ***********************************
           //scHdrMsg = 'This record has been updated';
           //dspHdrMsg = *on;
           PopDataSaved();
           scHdrMsg = *blanks;
           dspHdrMsg = *Off;
           //*** End   Change *** 05/21/19 ***********************************
        Endif;

       EndSr;

       //===================================================================
       // ValidNCDtl - Validate NovaCare detail fields
       //===================================================================
       BegSr ValidNCDtl;

        isValid = *on;

       EndSr;

       //===================================================================
       // addNCDtl - Add NovaCare detail
       //===================================================================
       BegSr addNCDtl;

        Chain ($dscode:$efdt:$dstype) pbd01;

        If not %found (nmbdl5);

           clear pbd01;
           BDcode = scHdDsCd;
           BDefdt = %dec(%char(scHdEfDt:*cymd0):7:0);
           BDam01 = scPTEvals;
           BDam02 = scPTReEval;
           BDam03 = scPTAllpd;
           BDam04 = scPTAllfs;
           BDam05 = scPTExcept;
           //***** Beg Add *** 05/06/16 ***********************************
           BDam06 = scPTReEvFL;
           //***** End Add *** 05/06/16 ***********************************
           //BDcom1 = scHdCom1;    // this added in addHdrData
           //BDcom2 = scHdCom2;    // this added in addHdrData
           BDtype = scHdDsTy;
           BDrcdt = 'D';
           BDstat = 'A';
           BDuser = q1user;
           BDpgm  = q1pgm;
           BDtime = %dec(%char(%time():*iso0):6:0);
           BDdt   = %dec(%char(%date():*cymd0):7:0);
           BDiusr = q1user;
           Write pbd01;
           scHdrMsg = 'This record has been added';
           dspHdrMsg = *on;
        Else;
           BDam01 = scPTEvals;
           BDam02 = scPTReEval;
           BDam03 = scPTAllpd;
           BDam04 = scPTAllfs;
           BDam05 = scPTExcept;
           //***** Beg Add *** 05/06/16 ***********************************
           BDam06 = scPTReEvFL;
           //***** End Add *** 05/06/16 ***********************************
           //BDcom1 = scHdCom1;    // this added in addHdrData
           //BDcom2 = scHdCom2;    // this added in addHdrData
           BDuser = q1user;
           BDpgm  = q1pgm;
           BDtime = %dec(%char(%time():*iso0):6:0);
           BDdt   = %dec(%char(%date():*cymd0):7:0);
           Update pbd01;
           scHdrMsg = 'This record has been updated';
           dspHdrMsg = *on;
        Endif;

       EndSr;

       //===================================================================
       // ValidGrAdd - Validate Grid Add/Edit screen
       //===================================================================
       BegSr ValidGrAdd;

        isValid = *on;
        erAddPMod = *off;
        erAddPrCd = *off;
        msAddPMod = *blanks;
        msAddPrCd = *blanks;
        scAddPrCd = %xlate($lo:$up:scAddPrCd);
        scAddPMod = %xlate($lo:$up:scAddPMod);

        // procedure code is required
        If scAddPrCd = *blanks;
           isValid = *off;
           erAddPrCd = *on;
           msAddPrCd = 'This is required';
        EndIf;

        // procedure code contains valid characters
        If %check($AlphaNum:scAddPrCd) <> 0;
           isValid = *off;
           erAddPrCd = *on;
           msAddPrCd = 'Invalid characters in data';
        EndIf;

        // modifier contains valid characters
        If %check($AlphaNum:scAddPMod) <> 0;
           isValid = *off;
           erAddPMod = *on;
           msAddPMod = 'Invalid characters in data';
        EndIf;

        // verify duplicate entry
        Setll ($dscode:scAddPrCd:scAddPMod:$efdt) pco02;
        If %equal(ndcol2);
           isValid = *off;
           erAddPMod = *on;
           msAddPMod = 'This combination exists';
           erAddPMod = *on;
           msAddPMod = 'This combination exists';
        EndIf;

       EndSr;

       //===================================================================
       // addGrData - Add Grid Data
       //===================================================================
       BegSr addGrData;

        If ##mode = 'A';
           clear pco0p;
           COcode = $dscode;
           COefdt = $efdt;
           COcpt4 = %xlate($lo:$up:scAddPrCd);
           COpmod = %xlate($lo:$up:scAddPMod);
           COstat = 'A';
           COuser = q1user;
           COpgm  = q1pgm;
           COdt   = %dec(%char(%date():*cymd0):7:0);
           COtime = %dec(%char(%time():*iso0):6:0);
           COiusr = q1user;
           COidt  = %dec(%char(%date():*cymd0):7:0);
           COitim = %dec(%char(%time():*iso0):6:0);
           COipgm = q1pgm;
           Write pco0p;
           scAddMsg = 'This record has been added';
           dspAddMsg = *on;
        Else;
           Chain scNovaRrn# pco0p;
           COpmod = scAddPMod;
           COuser = q1user;
           COpgm  = q1pgm;
           COdt   = %dec(%char(%date():*cymd0):7:0);
           COtime = %dec(%char(%time():*iso0):6:0);
           Update pco0p;
           scAddMsg = 'This record has been updated';
           dspAddMsg = *on;
        Endif;
        unlock ndcop;

       EndSr;

       //===================================================================
       // MovDbScAdd - Move database record to Add screen
       //===================================================================
       BegSr MovDbScAdd;

        scAddPrCd = COcpt4;
        scAddPMod  = COpmod;

       EndSr;

      /end-free
     P NovaCareDisc    E
     P*====================================================================
     P*  D7Disc - Maintain D7 network discount
     P*====================================================================
     P D7Disc          B                   Export
     d D7Disc          PI
     D*
     d* standalone variables
     d*
     d ##mode          s              1    inz
     d addMode         s               n   inz(*off)
     d chgMode         s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isValid         s               n   inz(*off)

      /free

       Select;
       When $mode = 'D';
          dspMode = *On;
          dspVisb = *Off;
          scCol0  = '0';
       When $mode = 'A';
          addMode = *On;
          dspVisb = *On;
       When $mode = 'C';
          chgMode= *On;
          dspVisb = *On;
       Endsl;

       // process until exit button is pressed.
       Dou ExitNow = *On;

          If BtnRefresh = *On;
             Exsr ClrSubfiles;       // clear Sub-files
             Exsr LoadD7Hdr;         // load D7 discount header info
             Exsr LoadD7Dtl;         // load discount detail info
             BtnRefresh = *Off;
          Endif;

          //Check for Overflow;

         If isOverflow = *On;
            Exsr SetD7Filter;
            isOverflow = *Off;
            If BtnAplyD7F = *on;
               BtnRefresh = *On;
               Iter;
            Endif;
         Endif;

          // display and process discount screen
          Exsr ExfmtScreens;

          Select;

          // do nothing - catch tab click event
          When scBtnTab = *on;
             scBtnTab = *off;

          When BtnRefresh = *On;
             Iter;

          When scBtnCan  = *On;
             scBtnCan  = *Off;
             leave;

          When scBtnSave = *On;
             scBtnSave = *Off;
             If $mode = 'A' or $mode = 'C';
                Exsr ProcD7Hdr;
             Endif;

          When BtnD7Flt = *on;       //Filter Button
             Exsr SetD7Filter;
             If BtnAplyD7F = *on;
                BtnRefresh = *On;
                Iter;
             Endif;

          When scD7Add = *on;        //Add button
             ##mode = 'A';
             clear RcdAddD7;
             dsbPrCd = *off;
             dsbMod1 = *off;
             dsbCdTy = *off;
             dsbBrpt = *off;
             Exsr ProcD7dtl;
             scPrCdSel = 'PRCO# = ' + %Editc($CO#:'X') + ' AND PRSTAT = ''A''' ;
             scPrCdBO = '1';
             scMod1Sel = 'PMCO# = ' + %Editc($CO#:'X') + ' AND PMSTAT = ''A''' ;
             scMod1BO = '1';
             BtnRefresh = *On;
          Other;
             ReadC sflD7;
             ##mode = $mode;
             If scD7Edt = *on;        //Edit link
                scD7Edt = *off;
                dsbPrCd = *on;        //Protected in change mode.
                dsbMod1 = *off;       //Unprotected in change mode (QA test request 8/19/19)
                dsbCdTy = *on;
                dsbBrpt = *on;
                dsbAddSvNx = *on;
                scPrCdSel =  'PRCO#=' + %Editc($CO#:'X') +
                             ' AND PRCPT4 = ''' + scPrcd +
                             ''' AND PRSTAT = ''A''' ;
                scPrCdBO = '0';
                //scMod1Sel = 'PMCO#=' + %Editc($CO#:'X') +  Unprotected in change mode
                //            ' AND PMKEY = ''' + scMod1 +   (QA test request 08/19/19)
                //            ''' AND PMSTAT=''A''';
                // scMod1BO = '0';
                scMod1BO = '1';
                scMod1Sel = 'PMCO# = ' + %Editc($CO#:'X') +
                            ' AND PMSTAT = ''A''' ;
                Chain(n) scD7rrn# pco0p;
                Exsr ProcD7dtl;
                BtnRefresh = *On;
             Endif;

             If scD7Dlt = *on;        //Delete link
                scD7Dlt = *off;
                @UWFLd  = 'D';
                Chain scD7Rrn# pco0p;
                delete pco0p;
                unlock ndcop;
                BtnRefresh = *On;
                If CltId = '114';
                   Exsr WrtTrD;
                Endif;
             Endif;
          Endsl;

       Enddo;

       //===================================================================
       // ClrSubFiles - Clears SubFiles
       //===================================================================
       BegSr ClrSubFiles;
        rrnD7 = 0;
        clrD7Sub = *On;
        dspD7Sub = *Off;
        dspD7Ctl = *Off;
        Write RcdD7;

       EndSr;

       //===================================================================
       // LoadD7Hdr - Load D7 Discount Header Information
       //===================================================================
       Begsr LoadD7Hdr;
        scHdDsHdg  = $prds;
        scHdDsTy   = $dstype;

        Chain ($co#:'DSTY':$dstype) pgt01;
        If %found(ftgtl);
           scHdDsTyDs = GTdesc;
        Endif;

        scHdDsCd   = $dscode;
        scHdEfDt   = %date($efdt:*cymd);

        Chain(n) ($dscode:$efdt) pbd01;
        If %found(nmbdl5);
           scHdCom1 = BDcom1;
           scHdCom2 = BDcom2;
           scAcf    = BDam01;
           scAtim   = BDam02;
           HDcom1 = BDcom1;
           HDcom2 = BDcom2;
           HDam01 = BDam01;
           HDam02 = BDam02;
           @UWFLh = *blanks;
        Endif;

        scL2Pos    = 305;
        scHMsgTop  = 705;
       EndSr;

       //===================================================================
       // LoadD7Dtl - Load D7 Grid Detail Information
       //===================================================================
       BegSr LoadD7Dtl;

        // set subfile indicators
        clrD7Sub = *Off;
        dspD7Sub = *On;
        dspD7Ctl = *On;

        // read file and load the subfile
        Setll ($dscode:$efdt) pco01;
        Reade ($dscode:$efdt) pco01;
        dow not %eof(ndcol);
           clear sflD7;

           If $mode = 'D';                              //Set sfl mode options
              dspVisb = *Off;
           Else;
              dspVisb = *On;
           Endif;

           If BtnAplyD7F = *Off or
             (BtnAplyD7F = *On and COcpt4>=ssPrCdFrom and COcpt4 <=ssPrCdTo);
              scPrCd  = COcpt4;
              scMod1  = COpmod;
              scCdTy  = COfl01;
              scNfAmt = COfee;
              scFamt  = COfFee;
              scPCamt = COaAmt;
              scAunit = COanes;
              scAmtTy = COfl02;
              scBrpt  = COfl03;
              scD7RRN# = $rrnndcol;                      //NDCOP rrn

              HDfee  = COfee;                            //Hold fields
              HDfFee = COfFee;
              HDaAmt = COaAmt;
              HDanes = COanes;
              HDfl02 = COfl02;
              @UWFLd = *blanks;

              rrnD7 = rrnD7 + 1;


              Write sflD7;

              If rrnD7 >=9999;
               isOverflow = *On;
               Leave;
              Endif;
           Endif;

           Reade ($dscode:$efdt) pco01;
        Enddo;

        rrnD7 = 1;

       EndSr;

       //===================================================================
       // ExfmtScreens - Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

        TabHeight = 620;
        APnlHeight = TabHeight + 60;
        Write RcdSysTbl;
        Write RcdPanel;

        Select;
        When isFilter = *On;
           Exfmt RcdD7Flt;

        When TabIndex = 0;
           Write RcdDiscHdr;
           Exfmt rcdD7;
           Read RcdSysTbl;
           Read RcdPanel;
           Read RcdDiscHdr;
        EndSl;


        clear scHdrMsg;
        dspHdrMsg = *off;

       Endsr;

       //===================================================================
       // SetD7Filter - Set D7 Filter;
       //===================================================================
       BegSr SetD7Filter;
        scPrCdSel = 'PRCO# = ' + %Editc($CO#:'X') + ' AND PRSTAT = ''A''';
        scPrCdBO = '1';
        scPrCdFrom = ssPrCdFrom;
        scPrCdTo   = ssPrCdTo;

        If isOverFlow = *on;
           scFltBtnD = 'Cancel';
           sc999txt1 = str999txt1;
           sc999txt2 = str999txt2;
        Else;
           scFltBtnD = 'Cancel';
           sc999txt1 = *blanks;
           sc999txt2 = *blanks;
        Endif;

        BtnD7Flt = *Off;
        svFilter = BtnAplyD7F;

        Dou erPrCdFrom = *Off and erPrCdTo = *Off;
            isFilter = *On;
            Exsr ExFmtScreens;
            isFilter = *Off;
            erPrCdFrom = *off;
            erPrCdTo = *off;

           If BtnCnlD7F  = *On;
              If isOverflow = *off;
                 BtnAplyD7F = svFilter;
              Endif;
              Leave;
           Endif;

          If scPrCdTo < scPrCdFrom;
             MsPrCdTo = 'To Code cannot be less than From Code';
             erPrCdTo = *On;
             Iter;
          Endif;

          If BtnAplyD7F= *on;
             ssPrCdFrom = scPrCdFrom;
             ssPrCdTo =   scPrCdTo;
             If scPrCdFrom = *blanks and scPrCdTo = *blanks;
                BtnAplyD7F =  *off;            //Filter Off
             Endif;
             BtnRefresh = *On;
          Endif;
       Enddo;
       Endsr;
       //===================================================================
       // ProcD7dtl - Process D7 disc detail (Addition or Update)
       //===================================================================
       BegSr ProcD7dtl;

        Dou BtnAddCan = *on or (BtnAddSav = *on and isValid = *on);
           Exfmt RcdAddD7;
           clear scAddMsg;
           dspAddMsg = *off;
           Select;

           When BtnAddSav = *on or BtnAddSvNx = *on;
              isValid = *on;
              Exsr ValidD7dtl;
              If isValid = *on;
                 Exsr AddD7Dtl;              //Add data
              Else;
                 Iter;                       //Show error message
              Endif;

              If BtnAddSav = *on;
                 BtnAddSav = *off;
                 BtnAddSvNx = *off;
                 Clear RcdAddD7;
                 Leave;
              Endif;

              If BtnAddSvNx = *on;
                 BtnAddSvNx = *off;
                 Clear RcdAddD7;
              Endif;

           When BtnAddCan = *on;
              BtnAddCan = *off;
              Clear RcdAddD7;
              Leave;
           Other;
              isValid = *on;
              Exsr ValidD7dtl;
           Endsl;
        Enddo;

        unlock ndcop;

       EndSr;

       //===================================================================
       // ProcD7Hdr - Process D7 Disc Header fields
       //===================================================================
       BegSr ProcD7Hdr;
        ##mode = $mode;
        isValid = *on;
        Exsr ValidD7Hdr;
        If isValid = *on;
           Exsr AddD7Hdr;
        Endif;

       EndSr;

       //===================================================================
       // ValidD7Hdr - Validate D7 Disc Header Data
       //===================================================================
       BegSr ValidD7Hdr;
        isValid = *on;
       EndSr;
       //===================================================================
       // AddD7Hdr - Add D7 Disc Header
       //===================================================================
       BegSr AddD7Hdr;

        Chain ($dscode:$efdt:$dstype) pbd01;

        If not %found (nmbdl5);

           clear pbd01;
           BDtype = scHdDsTy;                                   //OK
           BDcode = scHdDsCd;                                   //ok
           BDefdt = %dec(%char(scHdEfDt:*cymd0):7:0);
           BDcom1 = scHdCom1;
           BDcom2 = scHdCom2;
           BDam01 = scAcf;
           BDam02 = scAtim;
           BDrcdt = 'D';
           BDstat = 'A';
           BDuser = q1user;
           BDpgm  = q1pgm;
           BDtime = %dec(%char(%time():*iso0):6:0);
           BDdt   = %dec(%char(%date():*cymd0):7:0);
           BDiusr = q1user;
           Write pbd01;

           @UWFLh = 'W';
           If CltId = '114';
              Exsr WrtTrH;
           Endif;

           clear RcdAddD7;

        Else;
           BDcom1 = scHdCom1;
           BDcom2 = scHdCom2;
           BDam01 = scAcf;
           BDam02 = scAtim;
           BDuser = q1user;
           BDpgm  = q1pgm;
           BDtime = %dec(%char(%time():*iso0):6:0);
           BDdt   = %dec(%char(%date():*cymd0):7:0);
           Update pbd01;
           PopDataSaved();
           @UWFLh = 'U';
           If CltId = '114';
              Exsr WrtTrH;
           Endif;
        Endif;

       EndSr;

       //===================================================================
       // ValidD7dtl - Validate D7 Disc Detail
       //===================================================================
       BegSr ValidD7dtl;

        isValid = *on;
        erPrCd  = *off;
        erMod1  = *off;
        erCdTy  = *off;
        erNFamt = *off;
        erFamt  = *off;
        erPCamt = *off;
        erAmtTy = *off;
        msPrcd  = *blanks;
        msMod1  = *blanks;
        msCdty  = *blanks;
        msNFamt = *blanks;
        msFamt  = *blanks;
        msPCamt = *blanks;
        msAmtTy = *blanks;

        scPrCd = %xlate($lo:$up:scPrCd);
        scMod1= %xlate($lo:$up:scMod1);

        // procedure code is required
        If scPrCd = *blanks;
           isValid = *off;
           erPrCd = *on;
           msPrCd = 'This is required';
           isValid = *Off;
        EndIf;

        // procedure code contains valid characters
        If %check($AlphaNum:scPrCd) <> 0;
           isValid = *off;
           erPrCd = *on;
           msPrCd = 'Invalid characters in data';
           isValid = *Off;
        EndIf;

        // modifier contains valid characters
        If %check($AlphaNum:scMod1) <> 0;
           isValid = *off;
           erMod1 = *on;
           msMod1 = 'Invalid characters in data';
           isValid = *Off;
        EndIf;

        // procedure-modifier verify duplicate entry on Add mode
        If ##Mode = 'A';
           Setll ($dscode:scPrCd:scMod1:$efdt) pco02;
           If %equal(ndcol2);
              isValid = *off;
              erMod1 = *on;
              msMod1 = 'This combination exists';
           Endif;
        Endif;

        //Code type: Cannot be blanks
        If scCdTy = *blanks;
           isValid = *off;
           erCdTy = *on;
           msCdTy = 'Code type is required';
        Endif;

        //Non-Facility, Facility, Prof Component: At least 1 amount.
        If scNFamt = *zero and scFamt = *Zero and scPcAmt = *zero;
           erNFamt = *on;
           msNFamt = 'Amount cannot be zero';
           erFamt  = *on;
           msFamt  = 'Amount cannot be zero';
           erPCamt = *on;
           msPCamt = 'Amount cannot be zero';
           isValid = *off;
        Endif;

        //Type Amt/Perc: Cannot be blanks
        If scAmtTy = *blanks;
           isValid = *off;
           erAmtTy = *on;
           msAmtTy = 'Typ Amt/Perc is required';
        Endif;

       EndSr;

       //===================================================================
       // AddD7Dtl - Add/Update D7 Disc Detail Data
       //===================================================================
       BegSr AddD7Dtl;

        If ##mode = 'A';
           clear pco0p;
           @UWFLd = 'W';
        Else;
           Chain scD7Rrn# pco0p;
           @UWFLd = 'U';
        Endif;

        COcode = $dscode;
        COefdt = $efdt;
        COcpt4 = %xlate($lo:$up:scPrCd);
        COpmod = %xlate($lo:$up:scMod1);
        COfl01 = scCdTy;
        COfee  = scNfAmt;
        COfFee = scFamt;
        COaAmt = scPCamt;
        COanes = scAunit;
        COfl02 = scAmtTy;
        COfl03 = scBrpt;

        COstat = 'A';
        COuser = q1user;
        COpgm  = q1pgm;
        COdt   = %dec(%char(%date():*cymd0):7:0);
        COtime = %dec(%char(%time():*iso0):6:0);
        COiusr = q1user;
        COidt  = %dec(%char(%date():*cymd0):7:0);
        COitim = %dec(%char(%time():*iso0):6:0);
        COipgm = q1pgm;

        If ##mode = 'A';
           Write pco0p;
        Else;
           Update pco0p;
        Endif;

        If CltId = '114';
           Exsr WrtTrD;
        Endif;
        unlock ndcop;

       EndSr;

       //===================================================================
       //WRTTRH - Write Tracking Record - Header
       //===================================================================
       BegSr WrtTrH;
        Select;

        When @UWFLH = 'U';
           If HDcom1 <> BDcom1;
              @@FLD  = 'BDcom1';
              @@FVAL = HDcom1;
              @@TVAL = BDcom1;
              FA753R(BDcode:BDtype:BDrcdt:BDefdt:@@CPT4:@@PMOD:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

           If HDcom2 <> BDcom2;
              @@FLD  = 'BDcom2';
              @@FVAL = HDcom2;
              @@TVAL = BDcom2;
              FA753R(BDcode:BDtype:BDrcdt:BDefdt:@@CPT4:@@PMOD:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

           If HDam01 <> BDam01;
              @@FLD  = 'BDam01';
              @@FVAL = %Editc(HDam01:'X');
              @@TVAL = %Editc(BDam01:'X');
              FA753R(BDcode:BDtype:BDrcdt:BDefdt:@@CPT4:@@PMOD:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

           If HDam02 <> BDam02;
              @@FLD  = 'BDam02';
              @@FVAL = %Editc(HDam02:'X');
              @@TVAL = %Editc(BDam02:'X');
              FA753R(BDcode:BDtype:BDrcdt:BDefdt:@@CPT4:@@PMOD:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

        When @UWFLH = 'W';
           @@FLD = 'Added';
           @@FVAL = *blanks;
           @@TVAL = *blanks;
           FA753R(BDcode:BDtype:BDrcdt:BDefdt:@@CPT4:@@PMOD:@@10A1:@@10A2:
                  @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
        Endsl;
       EndSr;
       //===================================================================
       //WRTTRD - Write Tracking Record - Detail
       //===================================================================
       BegSr WrtTrD;

       @@10A1 = *blanks;
       @@10A2 = *blanks;
       @@10A3 = *blanks;
       @@10A4 = *blanks;

        Select;
        When @UWFLd = 'U';
           If HDfee <> COfee;
              @@FLD  = 'COFEE';
              @@FVAL = %Editc(HDfee:'X');
              @@TVAL = %Editc(COfee:'X');
              FA753R(COcode:$DsType:'D':$Efdt:COcpt4:COpmod:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

           If HDfFee <> COfFee;
              @@FLD  = 'COFFEE';
              @@FVAL = %Editc(HDffee:'X');
              @@TVAL = %Editc(COfFee:'X');
              FA753R(COcode:$DsType:'D':$Efdt:COcpt4:COpmod:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

           If HDaAmt <> COaAmt;
              @@FLD  = 'COAAMT';
              @@FVAL = %Editc(HDaAmt:'X');
              @@TVAL = %Editc(COaAmt:'X');
              FA753R(COcode:$DsType:'D':$Efdt:COcpt4:COpmod:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

           If HDanes <> COanes;
              @@FLD  = 'COANES';
              @@FVAL = %Editc(HDanes:'X');
              @@TVAL = %Editc(COanes:'X');
              FA753R(COcode:$DsType:'D':$Efdt:COcpt4:COpmod:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

           If HDfl02 <> COfl02;
              @@FLD  = 'COFL02';
              @@FVAL = HDfl02;
              @@TVAL = COfl02;
              FA753R(COcode:$DsType:'D':$Efdt:COcpt4:COpmod:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
           Endif;

        When @UWFLd = 'W';
           @@FLD = 'Added';
           @@FVAL = *blanks;
           @@TVAL = *blanks;
              FA753R(COcode:$DsType:'D':$Efdt:COcpt4:COpmod:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);

        When @UWFLd = 'D';
           @@FLD = 'Deleted';
           @@FVAL = *blanks;
           @@TVAL = *blanks;
              FA753R(COcode:$DsType:'D':$Efdt:COcpt4:COpmod:@@10A1:@@10A2:
                     @@10A3:@@10A4:@@FLD:@@FVAL:@@TVAL);
        Endsl;
       EndSr;
      /end-free
     P D7Disc          E
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     P ProgramStart    B                   Export
     d ProgramStart    PI
     d processMe                       n
      /free

        processme = *on;

        If $dstype = *blanks;
           processme = *off;
        Endif;

        If $dscode = *blanks;
           processme = *off;
        Endif;

        If $efdt = 0010101;
           processme = *off;
        Endif;

        If processme = *on;

           // open files
           open fpinetdisc;
           open ndcop;
           open ndcol;
           open ndcol2;
           open nmbdl5;
           open ftgtl;

           //*** Begin Delete *** 05/21/19 ************************************
           //If $Rrn > *zeros;

              // Chain to File for verification
              //Chain(n) $Rrn pco0p;

              // Process only if valid record found.
              //If not %found(ndcop);
              //   processme = *off;
              //Endif;

           //Endif;
           //*** End   Delete *** 05/21/19 ************************************

           // initialize variables
           TabIndex = $TabInd;
           scCol0 = '1';


           // Get Company number and name
           $co# = GetUserInfo(q1user);
           scCompName = getCompNam($Co#);

           //Get Client Id
           RtvCltId(CltId);

           If not %open(STACTPNL);
              Open STACTPNL;
           Endif;

           BtnAplyD7F = *Off;
           BtnRefresh = *On;
           ssPrCdFrom = *blanks;
           ssPrCdTo = *blanks;
           TabHeight = 620;
           APnlHeight = TabHeight + 60;
        Endif;


      /end-free
     P ProgramStart    E

       //===================================================================
       // ProgramEnd - Program End tasks
       //===================================================================
     P ProgramEnd      B                   Export
     d ProgramEnd      PI
      /free

        clear $rrnndcol;
        clear $DsType;
        clear $PrDs;
        clear $Mode;
        clear $DsCode;
        clear $Efdt;
        //*** Begin Delete *** 05/21/19 **************************************
        //clear $Rrn;
        //*** End   Delete *** 05/21/19 **************************************
        clear $TabInd;
        clear $Cmd;
        clear $CmdLen;
        clear $co#;
        clear $dsdesc;
        clear $com1;
        clear $com2;

        clear rcdPanel;
        clear RcdDiscHdr;
        clear rcdNova;
        clear rcdAddNC;
        clear rcdD7;
        clear RcdAddD7;

        if %open(fpinetdisc);
           close fpinetdisc;
        endif;

        if %open(ndcop);
           unlock ndcop;
           close ndcop;
        endif;

        if %open(ndcol);
           close ndcol;
        endif;

        if %open(ndcol2);
           close ndcol2;
        endif;

        if %open(nmbdl5);
           close nmbdl5;
        endif;

        if %open(ftgtl);
           close ftgtl;
        endif;

        if %open(STACTPNL);
           close STACTPNL;
        endif;

      /end-free
     P ProgramEnd      E
