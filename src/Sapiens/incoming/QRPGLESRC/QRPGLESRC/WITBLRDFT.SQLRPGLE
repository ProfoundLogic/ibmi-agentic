      * (c) 2021 Sapiens. All Rights Reserved.                             *
     H/TITLE WITBLRDFT - Portal Carrier Rating Defaults
     H OPTION(*NODEBUGIO: *SRCSTMT)
     H*DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('WTBNDDIR')
      //==================================================================
      //  Created on 02/28/2019 by C:136 for Project SY407
      //  Note: LodNotific loads the factor keys into WTGTP
      //==================================================================
      //  Revisions:
      //
      // 12/02/21  C:33  Support: 99730
      //                 Update RFPGM
      //
      // 03/08/21  C:298 Project: WC2196
      //                 Add new procedure for Connect Tab in Statistics
      //                 and Rating screen.
      //                 Added a visibility indicator for RcdPanel
      //                 and fields alignment changes in display file.
      //==================================================================
     fwitblrdft cf   e             workstn extfile('WITBLRDFT')
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     sfile(sflDftM:rrnSflDftM)
     f                                     sfile(sflEditDtl:rrnEditDtl)
     fwtactpnl  cf   e             workstn include(rcdComPnl)
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                                                                             `
     fwiratdft  uf a e             Disk    Rename(pdf01:pdf0p)
     f                                     Infds(pMasterFDS)
     fwiratdftl if   e           k Disk    Rename(pdf01:pdf00)
     f                                     Infds(lMasterFDS)
     fwiratddt  uf a e             Disk    Rename(pdt01:pdt0p)
     fwiratddtl uf   e           k Disk    Rename(pdt01:pdt00)
     f                                     Infds(detailFDS)
     fwdrrp     if   e             Disk    Rename(prr01:prr0p)
     fwdrrl     if   e           k Disk    Rename(prr01:prr00) prefix(l_)
     fwtgtl     if   e           k Disk    Rename(pgt01:pgt00)
     fwtlml2    if   e           k Disk    Rename(plm01:plm02)
     fwtddl1    if   e           k Disk    Rename(pdd01:pdd01)

      //Entry
     dRatingDefaults   pr                  extpgm('WITBLRDFT')
     d                                9  0
     d                                1
      ***** Beg Del ***** 03/08/21 ****************************
     d*RatingDefaults   pi
     d*@@rrn                           9  0
     d*@@mode                          1
      ***** End Del ***** 03/08/21 ****************************

      ***** Beg Add ***** 03/08/21 ****************************
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1
      ***** End Add ***** 03/08/21 ****************************
      // Definitions
     d psds          esds                  extname(SZQ1P)
     d comma           s              1a
     d error           s              1n   inz(*off)
     d mode            s              1a
     d process         s               n
     d strRateDesc     s             20a
     d rKey            s             10a   Dim(50)
     d rDesc           s             20a   Dim(50)
     d ix              s              2  0
     d isAddingEntry   s               n
      ***** Beg Add ***** 03/08/21 ****************************
     d isSt_Rat        s              1n   inz(*off)
     d inRrn           s              9  0
     d Callpgm         s              1
      ***** End Add ***** 03/08/21 ****************************

     d pMasterFDS      ds
     d  pMasterRRN           397    400b 0
     d lMasterFDS      ds
     d  lMasterRRN           397    400b 0
     d detailFDS       ds
     d  detailRRN            397    400b 0

     d/Copy sPrCompany
     d/Copy sPrDate
     d/Copy sPrFiles
     d/Copy sPrGenPop
     d/Copy sPrModule
     d/Copy sPrRtvBrnd
     d/Copy sPrRunCmd
     d/Copy sPrString
     d/Copy wPrUsrPrms
     d/Copy wPrGroup

      ***** Beg Add ***** 03/08/21 ***********************************
     dRatingDefaults   pi
     d@@rrn                           9  0
     d@@mode                          1
      ***** End Add ***** 03/08/21 ***********************************
      /Free
       Exec Sql Set Option Commit = *None;
       //***** Beg Add ***** 03/08/21 ********************************
       inRrn = @@rrn;
       Mode = @@mode;
       isSt_Rat = *off;
       //***** End Add ***** 03/08/21 ********************************
       If ProgramStart = *On;
          Main();
       Endif;
       *Inlr = *On;
       Return;
      /End-Free
       //***** Beg Add ***** 03/08/21 ********************************
     pProc_RatDft      b                   export
     dProc_RatDft      pi
     dGr_Rrn                          9  0
     dGr_Mode                         1
      /Free
       inRrn = Gr_rrn;
       Mode = Gr_mode;
       isSt_Rat = *on;
       Exec Sql Set Option Commit = *None;
       If ProgramStart = *On;
          Main();
       Endif;
       *Inlr = *On;
       Return;
      /End-Free
     pProc_RatDft      e
       //***** End Add ***** 03/08/21 ********************************
      //===================================================================
      // Add/Edit Rating Default Detail Records
      //===================================================================
     p AddEditDetail   b
     d AddEditDetail   pi              n
     d  dspDetail      s               n   Inz(*Off)
     d  isDtlWork      s               n

     d#ofType          c                   5
     daryType          s             10a   dim(#ofType)
     da1               s             10i 0
     da2               s             10i 0
     dcomma            s              1a
     dstrCode          s             10a
     dstrCodeDesc      s             40a

      /Free
       visSaveDtl = *Off;
       visNextDtl = *Off;
       Select;
       When btnAddDtl = *On;
          If OKtoAddDtl() = *Off;
             Return *On;
          Endif;
          posCxDtl = 275;
          dspDetail = *On;
          isDtlWork = *On;
          visSaveDtl = *On;
          visNextDtl = *On;
          rrnSflDtl = 0;
          Exsr LoadEditDetail;
       Other;
          Dou %Eof;
             Readc sflEditDtl;
             If %Eof;
                Leave;
             Endif;
             Select;
             When isEdtDtl = *On;
                isEdtDtl = *Off;
                posCxDtl = 385;
                dspDetail = *On;
                isDtlWork = *On;
                visSaveDtl = *On;
                Exsr LoadEditDetail;
             When isDltDtl = *On;
                isDltDtl = *Off;
                isDtlWork = *On;
                dspDetail = *Off;
                If PopTwoBtn('Please confirm deletion.':
                   'Carrier Rating Default Detail':'Delete':'Cancel':500:300);
                   Exsr DeleteData;
                Endif;
             Other;
                Return *Off;
             Endsl;
          Enddo;
       Endsl;
       Dou dspDetail = *Off;
          If dspDetail = *Off;
             Leave;
          Endif;
          dspDetail = *Off;
          Exfmt RcdDtlEdit;
          Select;
          When btnCxDtl = *On;
             btnCxDtl = *Off;
             dspDetail = *Off;
             Iter;
          When btnSvNxtDt = *On
           Or btnSaveDtl = *On;
             If DetailError() = *On;
                dspDetail = *On;
                Iter;
             Endif;
             Exsr SaveDetail;
             If btnSvNxtDt = *On;
                Exsr LoadEditDetail;
                dspDetail = *On;
             Endif;
             btnSave = *Off;
             btnSvNxtDt = *Off;
          Endsl;
       Enddo;
       Return isDtlWork;
       //==================================================
       Begsr LoadCodeChoices;
        Clear comma;
        Clear strCode;
        Clear strCodeDesc;
        Clear scCdChoice;
        Clear scCdValues;
        Select;
        When scRateKey = 'PRMDISC';
           Exec Sql
            Declare c1 Cursor For
            Select Distinct DScode
            From WMDSP
            Where DSco# = :RRco#
              And DSfnd = :RRFnd
              And DSst  = :RRst
            Order by DScode;
           Exec Sql
            Open c1;
           Dou sqlStt <> '00000';
              Exec Sql
               Fetch Next
               From c1
               Into :strCode;
              If sqlStt <> '00000';
                 Leave;
              Endif;
              If comma = *Blank;
                 comma = ',';
              Endif;
              scCdValues = %Trim(scCdValues) + %Trim(comma + strCode);
              scCdChoice = scCdValues;
           Enddo;
           Exec Sql
            Close c1;
        When scRateKey = 'DEDUCT';
           Exec Sql
            Declare c2 Cursor For
            Select DDcode, DDdesc
            From WTDDP a
            Where a.DDco# = :RRco#
              And a.DDfnd = :RRFnd
              And a.DDst  = :RRst
              And a.DDstat = 'A'
              And a.DDefdt =
               (Select Max(b.DDefdt)
                From WTDDP b
                Where b.DDco# = :RRco#
                  And b.DDfnd = :RRFnd
                  And b.DDst  = :RRst
                  And b.DDcode = a.DDcode
                  And b.DDstat = 'A')
            Order by DDcode, DDdesc;
           Exec Sql
            Open c2;
           Dou sqlStt <> '00000';
              Exec Sql
               Fetch Next
               From c2
               Into :strCode, :strCodeDesc;
              If sqlStt <> '00000';
                 Leave;
              Endif;
              If comma = *Blank;
                 comma = ',';
              Endif;
              scCdValues = %Trim(scCdValues) +
               %Trim(comma + strCode);
              scCdChoice = %Trim(scCdChoice) +
               %Trim(comma + '"' + %Trim(strCode) + '-' +
               %Trim(strCodeDesc)) + '"';
           Enddo;
           Exec Sql
            Close c2;
           If scCdChoice <> *Blanks;
              scCdChoice = '[' + %Trim(scCdChoice) + ']';
           Endif;
        When scRateKey = 'INCLIMIT';
           Exec Sql
            Declare c3 Cursor For
            Select Distinct LMkey, LMdesc
            From WTLMP a
            Where a.LMco# = :RRco#
              And a.LMfnd = :RRFnd
              And a.LMst  = :RRst
              And a.LMstat = 'A'
              And a.LMefdt =
               (Select Max(b.LMefdt)
                From WTLMP b
                Where b.LMco# = :RRco#
                  And b.LMfnd = :RRFnd
                  And b.LMst  = :RRst
                  And b.LMkey = a.LMkey
                  And b.LMstat = 'A')
            Order by LMkey, LMdesc;
           Exec Sql
            Open c3;
           Dou sqlStt <> '00000';
              Exec Sql
               Fetch Next
               From c3
               Into :strCode, :strCodeDesc;
              If sqlStt <> '00000';
                 Leave;
              Endif;
              If comma = *Blank;
                 comma = ',';
              Endif;
              scCdValues = %Trim(scCdValues) +
               %Trim(comma + strCode);
              scCdChoice =  %Trim(scCdChoice) +
              %Trim(comma + '"' + %Trim(strCode) + '-' +
               %Trim(strCodeDesc)) + '"';
           Enddo;
           Exec Sql
            Close c3;
           If scCdChoice <> *Blanks;
              scCdChoice = '[' + %Trim(scCdChoice) + ']';
           Endif;
        EndSl;
       Endsr;
       //==================================================
       //==================================================
       Begsr LoadTypeChoices;
       //==================================================
        Reset a1;
        Reset a2;
        Reset aryType;
        Chain (RRco#:'CRDF':scRateKey) pgt00;
        If %found;
           aryType(1) = GTcf01;
           aryType(2) = GTcf02;
           aryType(3) = GTcf03;
           aryType(4) = GTcf04;
           aryType(5) = GTcf05;
        Endif;
        Sorta(A) aryType;
        Clear chEntType;
        For a1 = 1 to #ofType;
           If aryType(a1) <> *blanks;
              a2 += 1;
              If a2 > 1;
                 chEntType = %trim(chEntType) + ',';
              Endif;
              chEntType = %trim(chEntType) + aryType(a1);
           Endif;
        Endfor;
        Clear scEntType;
        Clear tipEntType;
        disEntType = *off;
        Select;
        When DTeTyp <> *blanks;
           disEntType = *on;
           tipEntType = 'Entry Type is not available for edit.';
        When a2 = 1;
           DTeTyp = chEntType;
           disEntType = *on;
           tipEntType = 'There is only one type available.';
        When a2 > 1;
           chEntType = ',' + %trim(chEntType);
        Endsl;
       Endsr;
       //==================================================
       Begsr LoadEditDetail;
       //==================================================
        Clear pdt0p;
        Chain(n) rrnSflDtl pdt0p;
        Exsr LoadTypeChoices;
        scEntType = DTetyp;
        Monitor;
           Select;
           When scEntType = 'Amount';
            //scAmount = %dec(DTentry:%len(scAmount):%decpos(scAmount));
              scAmount = %dec(DTentry:11:2);
           When scEntType = 'Code';
              Exsr LoadCodeChoices;
              scCode = DTentry;
           When scEntType = 'Factor';
              scFactor = %dec(DTentry:5:4);
           When scEntType = 'Percent';
              scPercent = %dec(DTentry:5:2);
           When scRateKey = 'PRMDISC' and scEntType = *Blank;
              Exsr LoadCodeChoices;
           Endsl;
        On-error;
           Clear scAmount;
           Clear scCode;
           Clear scFactor;
           Clear scPercent;
        Endmon;
        Select;
        When DTdftFlg <> *Blank;
           scDftValue = DTdftflg;
        Other;
           scDftValue = 'No';
        EndSl;
        scDtlDesc = DTdesc;
        scWavType = DTwvtype;
        scWvFl = DTprmlvl;
        scMinAmt = DTmin;
        scMaxAmt = DTmax;
        Write RcdDtlEdit;
       Endsr;
       //==================================================
       Begsr SaveDetail;
       //==================================================
        Clear pdt0p;
        Chain rrnSflDtl pdt0p;
        DTco#    = RRco#;
        DTfnd    = RRfnd;
        DTst     = RRst;
        DTefdt   = RRefdt;
        DTkey    = scRateKey;
        DTdesc   = scDtlDesc;
        DTetyp   = scEntType;
        Select;
        When scEntType = 'Amount';
           DTentry = %editc(scAmount:'P');
        When scEntType = 'Code';
           DTentry = scCode;
        When scEntType = 'Factor';
           DTentry = %editc(scFactor:'P');
        When scEntType = 'Percent';
           DTentry = %editc(scPercent:'P');
        Endsl;
        DTdftflg = scDftValue;
        DTwvtype = scWavType;
        DTprmlvl = scWvFl;
        DTmin  = scMinAmt;
        DTmax  = scMaxAmt;
        DTuser   = Q1user;
        DTpgm    = Q1pgm;
        DTdt     = DateTo7(SystemDate);
        DTtime   = TimeTo6(SystemTime);
        If Not(%Found(WIRATDDT));
           DTiusr = DTuser;
           DTipgm = DTpgm;
           DTidt  = DTdt;
           DTitim = DTtime;
           Write pdt0p;
        Else;
           Update pdt0p;
        Endif;
       Endsr;
       //==================================================
       Begsr DeleteData;
       //==================================================
        Chain rrnSflDtl pdt0p;
        Delete pdt0p;
       Endsr;
      /End-Free
     p AddEditDetail   e
       //==================================================
       // Validate Detail
       //==================================================
     p DetailError     b
     d DetailError     pi              n
     d intEfdt7        s              7s 0
     d intRrn          s              9s 0
     d isError         s               n
      /Free
       // Allow just one default per detail record set
       intEfdt7 = RRefdt;
       Exec Sql
         Declare d1 Cursor For
         Select rrn(WIRATDDT)
         From WIRATDDT
         Where DTco#  = :RRco#
           And DTfnd  = :RRFnd
           And DTst   = :RRst
           And DTkey  = :scRateKey
           And DTefdt = :intEfdt7
           And DTdftFlg = 'Yes';
       Exec Sql
        Open d1;
       Dou sqlStt <> '00000';
          Exec Sql
           Fetch Next
           From d1
           Into :intRrn;
          If sqlStt <> '00000';
             Leave;
          Endif;
          If intRrn <> rrnSflDtl And scDftValue = 'Yes';
             PopOneBtn('Another detail record is flagged as the default.':
              'Error':'OK':500:300);
             isError = *On;
          Endif;
       Enddo;
       Exec Sql
        Close d1;
       If scAmount = 0 And scFactor = 0
        And scPercent = 0 And scCode = *Blanks;
          PopOneBtn('A value is required.':
           'Error':'OK':500:300);
          isError = *On;
       Endif;
       If scRateKey = 'WAIVER' And scWvFl = *Blanks;
          PopOneBtn('A premium level is required.':
           'Error':'OK':500:300);
          isError = *On;
       Endif;
       Return isError;
      /End-Free
     p DetailError     e
       //==================================================
       // Make sure master record has detail before canceling out
       //  or SaveNext
       //==================================================
     p DetailExists    b
     d DetailExists    pi              n
      /Free
       Chain(n) (RRco#:RRfnd:RRst:RRefdt:scRateKey) pdf00;
       If %Found;
          Chain(n) (DFco#:DFfnd:DFst:DFefdt:DFkey) pdt00;
          If Not(%Found);
             Return PopTwoBtn('Detail does not exist for this rate default.' +
              ' Press Cancel to exit or Continue to enter detail.':
              'Informational Message':'Cancel':'Continue':500:300);
          Endif;
       EndIf;
       Return *On;
      /End-Free
     p DetailExists    e
      //==================================================================
      // Main - Main processing
      //==================================================================
     p Main            b
     d Main            pi

     d decRRN          s              9  0 inz
     d isDftDisplay    s               n

      /Free
       exitNow = *Off;
       error = *Off;
       Dou exitNow = *On;
          Exsr LoadSfl;
          Exsr ExfmtScreens;
          dspDftSave = *Off;
          dspDftNext = *Off;
          error = *Off;
          Select;
          When btnCancel = *On;
             exitNow = *On;
          When btnAddDflt = *On;
             btnAddDflt = *Off;
             isAddingEntry = *On;
             dspDftSave = *On;
             dspDftNext = *On;
             isDftDisplay = *On;
             posDftCx = 625;
             rrnSflMast = 0;
             lMasterRRN = 0;
             Exsr LoadScreen;
             pMasterRRN = 0;
          Other;
             Dou %Eof;
                Readc SflDftM;
                If %Eof;
                   Leave;
                Endif;
                Select;
                When isEdtDftM = *On;
                   isEdtDftM = *Off;
                   dspDftSave = *On;
                   posDftCx = 735;
                   isDftDisplay = *On;
                   Exsr LoadScreen;
                When isDltDftM = *On;
                   isDltDftM = *Off;
                   isDftDisplay = *Off;
                   If PopTwoBtn('Please confirm deletion.':
                      'Carrier Rating Defaults':'Delete':'Cancel':500:300);
                      Exsr DeleteData;
                   Endif;
                EndSl;
             Enddo;
          Endsl;
         // Stay on maintenance screen until user hits Cancel so
         //  that detail records can be easily entered.
          Dou isDftDisplay = *Off;
             If isDftDisplay = *Off;
                Leave;
             EndIf;
             Exfmt RcdDMEdit;
             erRateKey = *Off;
             Select;
             When btnCancel = *On;
                btnCancel = *Off;
                If DetailExists() = *On;
                   isDftDisplay = *Off;
                Endif;
                Iter;
             When btnSave = *On or btnSaveNxt = *On;
                If btnSaveNxt = *On;
                   isAddingEntry = *On;
                EndIf;
                If btnSave = *On;
                   isDftDisplay = *Off;
                Endif;
                If MasterErrors() = *On;
                   Iter;
                Endif;
                SaveData();
                isAddingEntry = *Off;
                Select;
                When btnSaveNxt = *on;
                   If DetailExists() = *On;
                      rrnSflMast = 0;
                      Exsr ClrSubFiles;
                      Exsr LoadScreen;
                      pMasterRRN = 0;
                   Endif;
                Endsl;
                btnSave = *Off;
                btnSaveNxt = *Off;
             When AddEditDetail() = *On;
                Exsr ClrSubFiles;
                Exsr LoadDtlSfl;
             Endsl;
          Enddo;
          Exsr ClrSubFiles;
          //***** Beg Add ***** 03/08/21 ****************************
          callpgm = *off;
          ChkStack('WTGRPSTRAT':CallPgm);
          If callpgm = *on;
             ExitNow = *on;
          Endif;
          //***** End Add ***** 03/08/21 ****************************
       Enddo;
       //===================================================================
       // Clear Subfiles
       //===================================================================
       BegSr ClrSubFiles;
        rrnSflDftM = 0;
        clrSflDftM  = *On;
        dspSflDftM  = *Off;
        Write RcdDftM;
        rrnEditDtl = 0;
        clrEditDtl  = *On;
        dspEditDtl  = *Off;
        Write RcdDMEdit;
       Endsr;
       //===================================================================
       // Delete Data
       //===================================================================
       Begsr DeleteData;
        Chain rrnSflMast pdf0p;
        Delete pdf0p;
        Setll (DFco#:DFfnd:DFst:DFefdt:DFkey) pdt00;
        Dou %Eof(WIRATDDTL);
           Reade (DFco#:DFfnd:DFst:DFefdt:DFkey) pdt00;
           If %Eof;
              Leave;
           Endif;
           Delete pdt00;
        Enddo;
       Endsr;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;
        TabHeight = 770;
        APnlHeight = TabHeight + 60;
        //***** Beg Chg ***** 03/08/21 ****************************
        If isSt_Rat = *off;
        //Write rcdComPnl;
        //Write rcdPanel;
           Write rcdComPnl;
           Write rcdPanel ;
        Endif;
        //***** End Chg ***** 03/08/21 ****************************
        Exfmt rcdDftM;
        //***** Beg Chg ***** 03/08/21 ****************************
        If isSt_Rat = *off;
         //Read rcdComPnl;
         //Read rcdPanel;
           Read rcdComPnl;
           Read rcdPanel;
        Endif;
        //***** End Chg ***** 03/08/21 ****************************
       Endsr;
       //===================================================================
       // Load Default Detail Grid
       //===================================================================
       Begsr LoadDtlSfl;
        dspEditDtl = *On;
        clrEditDtl = *Off;
        selEditDtl = *On;
        Setll (RRco#:RRfnd:RRst:RRefdt:scRateKey) pdt00;
        Dou %Eof(WIRATDDTL);
           Reade(n) (RRco#:RRfnd:RRst:RRefdt:scRateKey) pdt00;
           If %Eof;
              Leave;
           Endif;
           // Skip the Std Limits record written by GetCarrierRating
           If DTpgm = 'WICARRAT';
              Iter;
           EndIf;
           sfEntType = DTetyp;
           sfEntAmt  = DTentry;
           sfDesc    = DTdesc;
           sfDefault = DTdftflg;
           sfMinAmt  = DTmin;
           sfMaxAmt  = DTmax;
           sfWavType = DTwvtype;
           sfPremLvl = DTprmlvl;
           rrnSflDtl = detailRRN;
           rrnEditDtl += 1;
           Write SflEditDtl;
           selEditDtl = *Off;
        Enddo;
        Write RcdDMEdit;
       Endsr;
       //===================================================================
       // Load Maintenance Screens
       //===================================================================
       Begsr LoadScreen;
        Clear tipRateKey;
        disRateKey = *off;
        Clear pdf0p;
        Chain(n) rrnSflMast pdf0p;
        If %found;
           disRateKey = *on;
           tipRateKey = 'Rating Description not available for edit.';
        Endif;
        scRateKey = DFkey;
        Select;
        When DFrequire <> *Blank;
           scRequire = DFrequire;
        Other;
           scRequire = 'No';
        EndSl;
        scAlwAgt  = DFalwagt;
        Select;
        When DFalwagt <> *Blank;
           scAlwAgt  = DFalwagt;
        Other;
           scAlwAgt = 'No';
        EndSl;
        Exsr LoadDtlSfl;
       Endsr;
       //===================================================================
       // Load Default Master Subfile
       //===================================================================
       Begsr LoadSfl;
        Clear DFkey;
        selSflDftM = *On;
        dspsflDftM = *On;
        clrSflDftM = *Off;
        Setll (RRco#:RRfnd:RRst:RRefdt) pdf00;
        Dou %Eof(WIRATDFTL);
           Reade (RRco#:RRfnd:RRst:RRefdt) pdf00;
           If %Eof;
              Leave;
           Endif;
           // Skip the Std Limits record written by GetCarrierRating
           If DFpgm = 'WICARRAT';
              Iter;
           EndIf;
           sfRateKey = DFkey;
           sfEfDt = RRefdt;
           scRateDesc = DFdesc;
           scDftReq = DFrequire;
           scAllwAgt = DFalwagt;
           isEdtDftM = *Off;
           isDltDftM = *Off;
           rrnSflDftM += 1;
           rrnSflMast = lMasterRRN;
           Write sflDftM;
           selSflDftM = *Off;
           // Clear detail records written by WICARRAT, if other exist
           Reset decRRN;
           Exec sql
            select rrn(WIRATDDT) into :decRRN
             from WIRATDDT
             where DTco# = :RRco# and DTfnd = :RRfnd and DTst = :RRst
             and DTkey = :DFkey and DTefdt = :RRefDt and DTpgm <> 'WICARRAT';
           If decRRN <> *zero;
              Exec sql
               delete
                from WIRATDDT
                where DTco# = :RRco# and DTfnd = :RRfnd and DTst = :RRst
                 and DTkey = :DFkey and DTefdt = :RRefDt and DTpgm = 'WICARRAT';
           Endif;
        Enddo;
       Endsr;
      /End-Free
     p Main            e
      //===================================================================
      // Validate Master Record
      //===================================================================
     p MasterErrors    b
     d  MasterErrors   pi              n
      /Free
       // The Inc Lim WICARRAT record doesn't show in the subfile.
       // This provides a way to edit the record even though
       // the user thinks they are adding it.
       If scRateKey = 'INCLIMIT';
          Chain(n) (RRco#:RRfnd:RRst:RRefdt:scRateKey) pdf00;
          If %Found And DFpgm = 'WICARRAT';
             rrnSflMast = lMasterRrn;
          EndIf;
       Endif;
       If pMasterRrn = 0;
          Chain(n) (RRco#:RRfnd:RRst:RRefdt:scRateKey) pdf00;
          If %Found;
             erRateKey = *On;
             msRateKey = 'A record with this rate default already exists.';
             Return *On;
          Endif;
       EndIf;
       Return *Off;
      /End-Free
     p MasterErrors    e
       //==================================================
       // Make sure master record exists before adding detail
       //==================================================
     p OKtoAddDtl      b
     d OKtoAddDtl      pi              n
      /Free
       //***  Begin Del  ***  12/02/21  ****************
       //Chain (RRco#:RRfnd:RRst:RRefdt:scRateKey) pdf00;
       //If Not(%Found);
       //***  End   Del  ***  12/02/21  ****************
       //***  Begin Add  ***  12/02/21  ****************
       Clear rrnSflMast;
       Exec sql
        select rrn(WIRATDFT) into :rrnSflMast
         from WIRATDFT
         where DFco#=:RRco# and DFfnd=:RRfnd and DFst=:RRst
          and DFefDt=:RRefDt and DFkey=:scRateKey;
       //***  End   Add  ***  12/02/21  ****************
          SaveData();
       //***  Begin Del  ***  12/02/21  ****************
       //Endif;
       //***  End   Del  ***  12/02/21  ****************
       Return *On;
      /End-Free
     p OKtoAddDtl      e
      //===================================================================
      // ProgramStart - Initial Program tasks
      //===================================================================
     p ProgramStart    b
     d ProgramStart    pi              n

      /Free
       //***** Beg Chg ***** 03/08/21 ****************************
       //*****Chain @@RRN prr0p;
       Chain inRRN prr0p;
       //***** End Chg ***** 03/08/21 ****************************
       If Not(%Found);
          Return *Off;
       Endif;
       Exsr LoadRateChoices;
       //***** Beg Chg ***** 03/08/21 ****************************
       //*****If Q1prms >= 2;
       If %parms >= 2;
       //***** End Chg ***** 03/08/21 ****************************
          mode = @@mode;
       Endif;
       isDspMode = *Off;
       If mode = 'D';
          isDspMode = *On;
       Endif;
       scCo# = RRco#;
       scCompName = GetCompNam(RRco#);
       scFnd = RRfnd;
       scGrpName = GetGrpName(RRfnd);
       scState = RRst;
       scEfDt = %Char(%Date(RRefdt:*Cymd):*Usa/);
       scPnlHdg = 'Carrier Rate Defaults';
       scTabHdg = 'Carrier Rate Defaults';
       scGrdHdg = 'Carrier Rate Defaults';
       //***** Beg Add ***** 03/08/21 ****************************
       callpgm = *off;
       ChkStack('WTGRPSTRAT':CallPgm);
       If callpgm = *on;
          isRatInfo = *off;
       Else;
          isRatInfo = *on;
       Endif;
       //***** End Add ***** 03/08/21 ****************************
       Return *On;
       //===================================================================
       // Load Rate Default Choices
       //===================================================================
       Begsr LoadRateChoices;
        Clear scChoices;
        Clear scChValues;
        Setll (RRco#:'CRDF') pgt00;
        Dou %Eof(WTGTL);
           Reade (RRco#:'CRDF') pgt00;
           Select;
           When %Eof;
              Iter;
           When GTkey = 'CONCREDIT';
              Select;
              When RRcccd = *Zero;
                 Iter;
              When RRccHd <> *Blanks;
                 GTdesc = RRcchd;
              Endsl;
           When GTkey = 'DEDUCT';
              Setll (RRco#:RRfnd:RRst) pdd01;
              Dou %Eof;
                 Reade (RRco#:RRfnd:RRst) pdd01;
                 Select;
                 When %Eof;
                    Iter;
                 When DDstat <> 'I';
                    Leave;
                 Endsl;
              Enddo;
              If %Eof;
                 Iter;
              Endif;
           When GTkey = 'DEV1';
              Select;
              When RRdv1c = *Zero;
                Iter;
              When RRdv1h <> *Blanks;
                 GTdesc = RRdv1h;
              Endsl;
           When GTkey = 'DEV2';
              Select;
              When RRdv2c = *Zero;
                 Iter;
              When RRdv2h <> *Blanks;
                 GTdesc = RRdv2h;
              Endsl;
           When GTkey = 'EXPMOD';
              If RRmrrd <> *Zero Or RRmrrn <> *Zero Or RRmrrc <> *Zero;
                 GTdesc = 'Merit Rating';
              Endif;
           When GTkey = 'INCLIMIT';
              Setll (RRco#:RRfnd:RRst) plm02;
              Dou %Eof;
                 Reade (RRco#:RRfnd:RRst) plm02;
                 Select;
                 When %Eof;
                    Iter;
                 When LMstat <> 'I';
                    Leave;
                 Endsl;
              Enddo;
              If %Eof;
                 Iter;
              Endif;
           When GTkey = 'PRMDISC';
           When GTkey = 'REINSUR ';
              Select;
              When RRarcc = *Zero;
                 Iter;
              Endsl;
           When GTkey = 'SAFETYFEE';
              Select;
              When RRsfpc = *Zero;
                 Iter;
              When RRsfhd <> *Blanks;
                 Gtdesc = RRsfhd;
              Endsl;
           When GTkey = 'SCHCREDIT';
              Select;
              When RRcrcd = *Zero;
                Iter;
              When RRcrhd <> *Blanks;
                 GTdesc = RRcrhd;
              Endsl;
           When GTkey = 'SCHDEBIT';
              Select;
              When RRdbcd = *Zero;
                Iter;
              When RRdbhd <> *Blanks;
                 GTdesc = RRdbhd;
              Endsl;
           When GTkey = 'SUBJECT1A';
              Select;
              When RR1acd = *Zero;
                Iter;
              When RR1ahd <> *Blanks;
                 GTdesc = RR1ahd;
              Endsl;
           When GTkey = 'SUBJECT1B';
              Select;
              When RR1bcd = *Zero;
                Iter;
              When RR1bhd <> *Blanks;
                 GTdesc = RR1bhd;
              Endsl;
           When GTkey = 'VOLUNTARY';
              Select;
              When RRvlcc = *Zero;
                Iter;
              When RRvlch <> *Blanks;
                 GTdesc = RRvlch;
              Endsl;
           When GTkey = 'WAIVER';
              Select;
              When RRwvcd = *Zero;
                Iter;
              When RRwvhd <> *Blanks;
                 GTdesc = RRwvhd;
              Endsl;
           When GTkey = 'ARAP';
              If RRst <> 'MA';
                 Iter;
              Endif;
           EndSl;
           ix += 1;
           rKey(ix) = GTkey;
           rDesc(ix) = GTdesc;
           scChoices = %Trim(scChoices) + %Trim(comma + GTdesc);
           scChValues = %Trim(scChValues) + %Trim(comma + GTkey);
           comma = ',';
        Enddo;
       Endsr;
      /End-Free

     p ProgramStart    e
      //===================================================================
      // Save Data
      //===================================================================
     p SaveData        b
      /Free
        Clear pdf0p;
        Chain rrnSflMast pdf0p;
        DFco# = RRco#;
        DFfnd = RRfnd;
        DFst  = RRst;
        DFkey = scRateKey;
        DFefdt = RRefdt;
        ix = %Lookup(DFkey:rKey);
        DFdesc = rDesc(ix);
        DFrequire = scRequire;
        DFalwagt = scAlwAgt;
        DFuser = Q1user;
        DFpgm = Q1pgm;
        DFdt = DateTo7(SystemDate);
        DFtime = TimeTo6(SystemTime);
        If Not(%Found(WIRATDFT));
           DFiusr = DFuser;
           DFipgm = DFpgm;
           DFidt = DFdt;
           DFitim = DFtime;
           Write pdf0p;
           rrnSflMast = pMasterRRN;
        Else;
           Update pdf0p;
        Endif;
      /End-Free
     p SaveData        e
