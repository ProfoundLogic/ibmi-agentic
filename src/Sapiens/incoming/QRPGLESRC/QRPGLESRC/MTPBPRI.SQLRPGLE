     h*copyright('(c) 2024 Sapiens, Inc. All rights reserved. -
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     H bnddir('STBNDDIR': 'FTBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Provider Bill Maintenance Program
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes s
     h*
     h*  Called from: Menu/Medical Cases/Claims
     h*  Calls to:
     h*
     h*  Created on 06/18/2024 by C:333/079 for Project MC448
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h**********************************************************************

     fmtpbpri   cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflPrvBill:rrnPb)
     f                                     Sfile(sflPbPhyDt:rrnPbDtl)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     f                                     include(rcdMcPnl)
     ffmfnp     if   e             disk    rename(pfn01:pfn0p)

     Ffaeolh    if   e           k disk    rename(peo01:peoprd)
     f                                     infds(dbfds2)
     Ffaeol3    if   e             disk    rename(peo01:peoprd2)
     f                                     prefix(p_)
     Ffaeop     uf a e             disk    rename(peo01:peoprd1)
     f                                     infds(dbfds)
     Ffdcal19   if   e           k disk
     f                                     infds(dbfds3)
     Ffddhl     if   e           k disk
     Ffddel     if   e           k disk
     Fsmcol     if   e           k disk

     Fftgtl     if   e           k disk
      //======================================================================
      // Data Structures
      //======================================================================
     d psds          esds                  extname(szq1p)

     d dbfds           ds
     d  dbrrn                397    400b 0

     d dbfds2          ds
     d  dbrrn2               397    400b 0

     d dbfds3          ds
     d  dbrrn3               397    400b 0

      //======================================================================
      // Begin Standalone Fields
      //======================================================================
     d saveDT          s                   like(FNdt)
     d saveTM          s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d flgRcdUpdated   s              1    inz
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     D indexTab        s              5  0
     d saveRrn         s              9  0 inz
     d saveRrnDt       s              9  0 inz
     d inRrn1          s              9  0 inz
     d inRrn2          s              9  0 inz
     D xxRrn           s              9  0 inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d xxDelResp       s               n   inz
     d xxCpyResp       s               n   inz
     d erCtyE          s               n   inz
     d msCtyE          s             15A
     d ##Wcif          s              1
     d ##Mcif          s              1
     d ##Wcin          s              1
      //New varibales for copying a bill
     d ClmtFNmE        s             15A
     d ClmtLNmE        s             30A
     d CaseNoE         s              7P 0
     d EOSS#E          s              9A
     d FedIDE          s              9P 0
     d ModifE          s              3P 0
     d BillNPI         s             15A
     d EostE           s              2A
     d EopostE         s              2A
     d EobtypeE        s              1A
     d EostypE         s              3A
     d EocscdE         s              3A
     d EocrdtE         s              7P 0
     d EochrgE         s             11P 2
     d inRrn           s              9a
     d inMode          s              1a
      //end varibales for copying a bill
      //======================================================================
      // End Standalone Fields
      //======================================================================

      //======================================================================
      // Proto-types
      //======================================================================

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3
       Dcl-Pr RunCommand ExtPgm('QCMDEXC');
        inCmd    char(2750) const;
        inCmdLen packed(15:5) const;
       End-Pr;

       // Initialize the Screen fields for Provider Bills
     d InzProvBills    pr

       // clear subfiles
     d clrSubFiles     pr
     d*** clrSubFileMain  pr
     d*** clrSubFileDtl   pr

      // Load Provider Bill Main Grid
     d LoadProvBill    pr

      // Load Provider Bill Detail Grid
     d LoadPbDetail    pr

      * Main Processing
     d Main            pr

      * Process Edit Provider Bill information
     d ProcOther       pr

      * Validate Provider Bill info procedure.
     d ValidatePbill   pr              n
     d strmode                        1

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      * Process Physician Bills
     d ProcessPhys     pr                  Extpgm('MTPBPHHD')
     d inRrn                          9
     d inMode                         1
      //=======================================================================
      // Copy books
      //=======================================================================
     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrFiles
     d/copy sPrMessage
     d/copy sPrModule
     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sPrTable
     d/copy sPrTrigger
     d/copy sPrTrig
     d/copy sPrAtrium
     d/copy mPrPbDtPop
     c***  *entry        plist
     c***                parm                    inRrn             9
     c***                parm                    inMode            1
       //===================================================================
       // Begin Processing
       //===================================================================
       ProgramStart(process);
       If process;
          Main();
       Endif;

       *InLr = *On;
       Return;
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d  isexit         s               n   Inz(*off)
     d  mode           s              1a

       //LoadActionBar();

       BtnCancel = *off;

       // Process until Cancel button is pressed.
       DoW BtnCancel = *off;

          // clear Sub-files
          //***clrSubfiles();
          //***clrSubfileMain();
          //***clrSubfileDtl();

          // Load Provider Bill information
          loadProvBill();
          //*** Beg Add 07/17/25 **********************************************

          // Main
          If rrnPosPb <> 0 and rrnPosPb <= rrnPbRow;
             rrnPb = rrnPosPb;
          Else;
             rrnPb = 1;
          Endif;
          saveRrn = rrnPbRow;

          // Detail
          If RrnPosPbDt <> 0 and RrnPosPbDt <= rrnPbRowDt;
             rrnPbDtl = RrnPosPbDt;
          Else;
             rrnPbDtl = 1;
          Endif;
          saveRrnDt = rrnPbRowDt;

          // display screen
          Exsr ExfmtScreens;

          // Main
          If rrnPbRow = 0 and saveRrn <> 0;
             rrnPbRow = saveRrn;
          Endif;

          // Detail
          If rrnPbRowDt = 0 and saveRrnDt <> 0;
             rrnPbRowDt = saveRrnDt;
          Endif;

          error = *off;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Refresh
          When btnRefresh = *On;
             btnRefresh =*Off;

          // (Enter key or Save button)
          When btnSave = *on;
             btnSave = *off;

          // Add Provider Bill Button
          When BtnBillAdd = *On;
             BtnBillAdd = *Off;
             ProcessAdd();

          // Add Provider Bill Button
          When BtnDtlAdd = *On;
             BtnDtlAdd = *Off;
             mode = 'A';
             inRrn1 = scRrnBill;
             inRrn2 = 0;
             Select;
             When EObTyp = 'N';
                PhyDtlEdit(EOco#:inRrn1:inRrn2:mode);
             When EObTyp = 'H';
             When EObTyp = 'X';
             When EObTyp = 'D';
             Endsl;

          // On Row Click Load Detail Grid
          When BtnSfRowCl = *on;
             LoadProvBill();
          //***doLoad = *Off;

          Other;
             procOther();

          Endsl;
       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;

       Select;
       When indexTab = 1;
          pnlHeight = 1000;
          APnlHeight = pnlHeight + 62;

          // Load initial values
          InzProvBills();

          If dspMode;
             inquiry_1 = *on;
          Endif;

          Write rcdSysTbl;
          Write rcdTabPnl;
          Exfmt rcdPrvBill;
          Read  rcdSysTbl;
          Read rcdTabPnl;

       EndSl;

       Endsr;

     p Main            e
       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n

       // Tab Index
       tabIndex = 0;

       GetClientID(clientID);

       intCo# = GetUserInfo(q1user);
       Chain intCo# pco01;
       If COname <> ' ';
          scCompName = COname;
       Endif;

       processMe = *on;

       Return;

      /end-free
     p ProgramStart    e
       //===================================================================
       // Load Provider Bill
       //===================================================================
     p LoadProvBill    b
     d LoadprovBill    pi

        If RrnPbRow = 0;
           scRowSel = 1;
        Else;
           scRowSel = RrnPbRow;
        Endif;
        clrSubfiles();

       // set subfile indicators
       //***clrPrvSub = *Off;
       //***dspPrvSub = *On;
       //***dspPrvCtl = *On;

        // Populate the subfile
        rrnPb = 0;
        Setll *loval peoprd;
        Read peoprd;
        Dow not %eof(faeolh);
           If EObTyp = 'N';
           rrnPb += 1;
           // set the selection on 1st record for first load
           scPbRowSl = *off;
           If saveRrn = *zeros;
              If rrnPb = 1;
                 scPbRowSl = *on;
                 saveRrn = dbRrn2;
              Endif;
           Else;
              If saveRrn   = rrnPb;
                 scPbRowSl  = *on;
              Endif;
           Endif;

           // Assign grid column values
           scClmtName =  %trim(EOlsn)+', '+%trim(EOfsn);
           scEOMB     = EOeob#;
           scPrvdr    = %char(EOfein)+'-'+%char(EOmod);
           scSrvStrDt = dtMDYY(EOfrdt);
           scSrvEndDt = dtMDYY(EOtodt);
           scChrg     = EOchrg;
           scPaid     = EOpaid;
           scAst      = EOast;

           Chain (EOco#:'BTYP':EObtyp) pgt01;
           If %Found(FTGTL);
              scBtyp = GTdesc;
           Endif;

           Select;
           When EOblSt = 'C';
              scSts = 'Completed';
           When EOblSt = 'P';
              scSts = 'Pended';
           When EOblSt = 'U';
              scSts = 'Un-Pended';
           When EOblSt = 'X';
              scSts = 'Cancelled';
           When EOblSt = 'D';
              scSts = 'Deleted';
           Other;
              scsts = EOblSt;
           Endsl;

           scrrnBill = dbrrn2;

           Clear scPbRowSl;
           If (rrnPb = rrnPbRow) or (rrnPb = 1 and rrnPbRow = 0);
             scPbRowSl = 'A';
           Endif;

           Write sflPrvBill;

           Endif;

           Read faeolh;

        Enddo;

        LoadPbDetail();

     p LoadProvBill    e
       //*** Beg Add *** 07/17/25 **********************************************
       //===================================================================
       // Load Provider Bill Detail
       //===================================================================
     p LoadPbDetail    b
     d LoadPbDetail    pi
     d
     d ArrDxr          s              1a   Dim(4)
     d n               s              1s 0

       If RrnPbDtl = 0;
          scRowSelDt = 1;
       Else;
          scRowSelDt = RrnPbRowDt;
       Endif;

       // Read from subfile
       Chain rrnPbRow sflPrvBill;

       If %Found;
          //***ClrSubfileDtl();
          Chain scRrnBill peoprd1;
          If %Found(FAEOP);
             // Load detail line items
             rrnPbDtl = 0;
             Setll (EOco#:EOSub#:EOfnd:EOfyr:EOcase:EOeob#) pca01;
             Reade (EOco#:EOSub#:EOfnd:EOfyr:EOcase:EOeob#) pca01;
             Dow Not %Eof(FDCAL19);
             // set the selection on 1st record for first load
             scPbRowSDt = *off;
             If saveRrnDt = *zeros;
                If rrnPbDtl = 1;
                   scPbRowSdt = *on;
                   saveRrnDt = dbRrn2;
                Endif;
             Else;
                If saveRrnDt   = rrnPbDtl;
                   scPbRowSdt  = *on;
                Endif;
             Endif;
                scFrmSrvDt = Dtmdyy(CAfdat);
                scDtlChrg  = CAchrg;
                scUnits    = CAunts;
                scDtlPaid  = CApaid;

                Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq)
                 pdh01;
                 If %Found(FDDHL);
                    scPos      = DHpos;
                    scPrCd     = DHprCd;
                    scPrDs     = DHprDs;
                    scMod1     = DHmod1;
                    Clear arrDxr;
                    If DHdxr1 = 'X';
                       ArrDxr(1) = 'A';
                    Endif;
                    If DHdxr2 = 'X';
                       ArrDxr(2) = 'B';
                    Endif;
                    If DHdxr3 = 'X';
                       ArrDxr(3) = 'C';
                    Endif;
                    If DHdxr4 = 'X';
                       ArrDxr(4) = 'D';
                    Endif;
                    Clear scDxRef;
                    For n = 1 to 4;
                       scDxRef = %Trim(scDxRef) + %Trim(ArrDxr(n));
                    Endfor;
                Endif;

                Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq) pde01;
                If %Found(FDDEL);
                   scEob      = DEeob;
                Endif;

                scRrnDtl   = dbrrn3;
                rrnPbDtl = rrnPbDtl + 1;
                write sflPbPhyDt;

           Clear scPbRowSdt;
           If (rrnPbDtl = rrnPbRowDt) or (rrnPbDtl = 1 and rrnPbRowDt = 0);
             scPbRowSDt = 'A';
           Endif;

                Reade (EOco#:EOSub#:EOfnd:EOfyr:EOcase:EOeob#) pca01;
             Enddo;
          Endif;
       Endif;

     p LoadPbDetail    e
       //*** End Add *** 07/17/25 **********************************************
       //===================================================================
       // ProcOther - Process other Options
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

       Clear rcdEdtBill;
       isexit = *off;
       ReadC sflPrvBill;

       Dow not %eof;

          Select;
          When ScBillEdit = *On;

             ScBillEdit = *Off;
             mode = 'C';
             BillEdt = *on;
             saveRrn  = rrnPb;

             Chain(n) scRrnBill PEOPRD1;

             Exsr MoveDbSc;
             scBillHdg = 'Edit Provider Bill Information';

             Dow isexit = *off;
                Exfmt rcdEdtBill;

                Select;
                When BtnBillSav = *on;
                   BtnBillsav = *off;

                   If ValidatePbill(mode);
                      Chain scRrnBill faeop;
                      Exsr MoveScDb;
                      Update PEOPRD1;
                      ProcessAll();
                      isexit = *on;
                   Else;
                      Exfmt rcdEdtBill;
                   EndIf;

                When BtnBillCan= *on;
                   BtnBillCan = *off;
                   isexit = *on;
                EndSl;
             EndDo;

          When ScBillDlt = *On;
             ScBillDlt = *Off;
             Chain(e) scRrnBill PEOPRD1;
             If %status = 1218;
                PopOneBtn('Record locked by another user.  Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRrnBill PEOPRD1;
             Exsr MoveDbSc;

             scBillHdg = 'Delete Provider Bill Information';

             Dow isexit = *off;

                Write rcdEdtBill;
                xxDelresp = PopTwoBtn('Do you want to delete this provider +
                bill information?':'Confirm Delete':'Yes':'No');
                Select;
                When xxDelResp = *On;
                   Delete PEOPRD1;
                   PopOneBtn('Provider bill information is +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   saveRrn  = rrnPb;
                   isexit = *on;
                EndSl;
             EndDo;
          When ScBillCopy= *On;
             ScBillCopy= *Off;
             Dow isexit = *off;

                xxCpyresp = PopTwoBtn('Do you want to Copy this provider +
                bill information?':'Confirm Copy':'Yes':'No');
                Select;
                When xxCpyResp = *On;
                   Exsr BillCopy;
                   Exsr WrtBillCopy;
                   PopOneBtn('Provider bill information is +
                   Copied!':'Copied':'Ok');
                   isexit = *on;
                When xxCpyResp = *Off;
                   saveRrn  = rrnPb;
                   isexit = *on;
                EndSl;
             EndDo;

          endsl;

          ReadC sflPrvBill;

       Enddo;

       ReadC sflPbPhyDt;
       Dow not %eof;
          // Edit Provider Bill Detail Button
          Select;
          When SvcLnEdit = *On;
             SvcLnEdit = *Off;
             mode = 'E';
             inRrn1 = scRrnBill;
             inRrn2 = scRrnDtl;
             Select;
             When EObTyp = 'N';
                PhyDtlEdit(CAco#:inRrn1:inRrn2:mode);
             When EObTyp = 'H';
             When EObTyp = 'X';
             When EObTyp = 'D';
             Endsl;
          Endsl;
          ReadC sflPbPhyDt;
       Enddo;

       //======================================================================
       // MoveDbSc - Subroutine to move database fields into screen fields
       //======================================================================
       BegSr MoveDbSc;
        scClmtFNmE= EOfsn ;
        scClmtLNmE= EOlsn ;
        scCaseNo  = EOCASE;
        scCaseNoE  = %Editc(EOfnd:'X') + '-' + %Editc(EOfyr:'X') + '-' +
                     %Editc(EOcase:'X');
        scEOSS#E  = EOSS#  ;
        scFedIDE  = EOfein;
        scModifE  = EOmod ;
        scBillNPI = EOBNPI;
        scEostE   = EOst  ;
        scEopostE = EOst;
        scEobtypeE= EOBTYP ;
        scEostypE = EOSTYP ;
        scEocscdE = EOCSCD ;
        scEocrdtE = dtMDYY(EOtodt) ;
        ScEochrgE = EOCHRG ;
       EndSr;
       //======================================================================
       // MoveScDb - Subroutine to move database fields into screen fields
       //======================================================================
       BegSr MoveScDb;
        EOCHRG=ScEochrgE;
        EOfein=scFedIDE ;
        EOmod =scModifE ;
       EndSr;

       //======================================================================
       // BillCopy - Subroutine to move Screen fields into database fields
       //======================================================================
       BegSr BillCopy;
          Chain(e) scRrnBill PEOPRD2;
          ClmtFNmE= p_EOfsn ;
          ClmtLNmE= p_EOlsn ;
          CaseNoE = p_EOCASE;
          EOSS#E  = p_EOSS#  ;
          FedIDE  = p_EOfein;
          ModifE  = p_EOmod ;
          BillNPI = p_EOBNPI;
          EostE   = p_EOst  ;
          EopostE = p_EOst;
          EobtypeE= p_EOBTYP ;
          EostypE = p_EOSTYP ;
          EocscdE = p_EOCSCD ;
          EocrdtE = p_EOtodt  ;
          EochrgE = p_EOCHRG ;
       EndSr;

       //======================================================================
       // SrtBillCopy - Subroutine to write copied bill
       //======================================================================
       BegSr WrtBillCopy;
        Clear PEOPRD1;
          EOfsn =     ClmtFNmE   ;
          EOlsn =     ClmtLNmE   ;
          EOCASE=     CaseNoE    ;
          EOSS# =     EOSS#E     ;
          EOfein=     FedIDE     ;
          EOmod =     ModifE     ;
          EOBNPI=     BillNPI    ;
          EOst  =     EostE      ;
          EOst  =     EopostE    ;
          EOBTYP=     EobtypeE   ;
          EOSTYP=     EostypE    ;
          EOCSCD=     EocscdE    ;
          EOtodt=     dtCYMD(p_EOtodt);
          EOCHRG=     EochrgE    ;
        Write PEOPRD1;
       EndSr;

     P ProcOther       E
       //===================================================================
       // ProcessAdd -Add Bill
       //===================================================================
     P ProcessAdd      b
     D ProcessAdd      pi
     d*
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

       Clear rcdEdtbill;
       mode = 'A';
       isexit = *off;
       BtnBillCan = *off;

       Dow BtnBillCan = *off and isexit = *off;

          scBillHdg = 'Add Bill Information';
          visBillAd = *On;
          Exfmt rcdEdtBill;

          Select;
          When BtnBillSAd = *on;
              BtnBillSAd= *off;
            If ValidatePbill(mode);
             Exsr WrtBill;
             // Initialize Screen fields for re-load
             Clear rcdEdtBill;
             BtnBillCan = *off;
            EndIf;

          When BtnBillSav = *on;
             BtnBillSav = *off;
             If ValidatePbill(mode);
             Exsr WrtBill;
             isexit = *on;
             EndIf;

          When BtnBillCan= *on;
             BtnBillCan= *off;
             isexit = *on;

          EndSl;
       Enddo;
       //======================================================================
       // MoveScDb - Subroutine to move Screen fields into database fields.
       //======================================================================
       BegSr MoveScDb;
         EOfsn   =scClmtFNmE  ;
         EOlsn   =scClmtLNmE  ;
         EOCASE  =scCaseNo ;
         EOSS#   = scEOSS#E   ;
         EOfein  = scFedIDE  ;
         EOmod   = scModifE  ;
         EOBNPI  = scBillNPI ;
         EOst    = scEostE  ;
         EOst    = scEopostE ;
         EOBTYP  = scEobtypeE ;
         EOSTYP  = scEostypE ;
         EOCSCD  = scEocscdE ;
         EOCRDT  = scEocrdtE ;
         EOCHRG  = ScEochrgE ;
       EndSr;

       //======================================================================
       // WrtBill - Writing Details
       //======================================================================
       BegSr WrtBill;
        Clear PEOPRD1;
        Exsr MoveScDb;
        Write PEOPRD1;
       EndSr;

     P ProcessAdd      e
       //===================================================================
       // Validate Bill Submission Details addition information.
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
      //===================================================================
     p ValidatePbill   b
     d ValidatePbill   pi              n
     d strmode                        1
     d isError         s               n   inz

       isError = *off;
       // Initialize all the messageId and error indicators.
       erLName= *off;
       msLName= *blanks;
       erFName= *off;
       msFName= *blanks;
       erOvdtE= *off;
       msOvdtE = *blanks;
       erFedID= *off;
       msFedID= *blanks;
       erModifier= *off;
       msModifier= *blanks;
       erTotChrg= *off;
       msTotChrg = *blanks;

       // Validation Check for Carrier Received Date
       If scEocrdtE<> *Zeros;
          If dtCheck(%Dec(scEOcrDtE:8:0)) = 0;
             erOvdtE = *on;
             msOvdtE = 'Invalid Date in Field';
             isError = *on;
          Endif;
       Endif;

       // Enter complete info for Mailing address, if any of the field is entere
         If scClmtFNmE <> *blanks or scClmtLNmE <> *blanks or
             scFedIDE <> 0 or scModifE <> 0 or ScEochrgE<> 0  ;
            If scClmtFNmE = *Blanks;
               erFName = *on;
               msFName = 'WCR0503';
               isError = *on;
            Endif;
            If scClmtLNmE = *blanks;
               erLName= *on;
               msLName= 'WCR0503';
               isError = *on;
            Endif;
            // Provider Federal ID
            If scFedIDE= *zeros;
               erFedID = *on;
               msFedID = 'WCR0556';
               isError = *on;
            Endif;
            // Billed Charges
            If ScEochrgE = *zeros;
               erTotChrg= *on;
               msTotChrg = 'WCR0556';
               isError = *on;
            Endif;
         Endif;

       // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

     p ValidatePbill   e

       //===================================================================
       // InzProvBills - Initialize Provider Bills fields
       //===================================================================
     p InzProvBills    b
     d InzProvBills    pi

     p InzProvBills    e

       //===================================================================
       // ClrSubFiles - Clear Subfiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi

       rrnPb = 0;
       clrPrvSub = *On;
       dspPrvSub = *Off;
       dspPrvCtl = *Off;

       rrnPbDtl = 0;
       clrPbPhyDt = *On;
       dspPbPhyDt = *Off;
       dspPhyDCtl = *Off;

       Write rcdPrvBill;

       clrPrvSub = *Off;
       dspPrvSub = *On;
       dspPrvCtl = *On;

       clrPbPhyDt = *Off;
       dspPbPhyDt = *On;
       dspPhyDCtl = *On;

     p ClrSubFiles     e
       //===================================================================
       // ClrSubFileMain - Clear main subfile
       //===================================================================
     p*** ClrSubFileMain  b
     d*** ClrSubFileMain  pi

       //***rrnPb = 0;
       //***clrPrvSub = *On;
       //***dspPrvSub = *Off;
       //***dspPrvCtl = *Off;

       //***Write rcdPrvBill;

       //***clrPrvSub = *Off;
       //***dspPrvSub = *On;
       //***dspPrvCtl = *On;

     p*** ClrSubFileMain  e
       //===================================================================
       // ClrSubFileDtl - Clear detail subfile
       //===================================================================
     p*** ClrSubFileDtl   b
     d*** ClrSubFileDtl   pi

       //***rrnPbDtl = 0;
       //***clrPbPhyDt = *On;
       //***dspPbPhyDt = *Off;
       //***dspPhyDCtl = *Off;

       //***Write rcdPrvBill;

       //***clrPbPhyDt = *Off;
       //***dspPbPhyDt = *On;
       //***dspPhyDCtl = *On;

     p*** ClrSubFileDtl   e

       //===================================================================
       // ProcessAll - Process all Bill Types
       //===================================================================
     p ProcessAll      b
     d ProcessAll      pi

       // Override files based on the Adjducation State
       OverrideFiles();

       Select;
       When EObtyp = 'N';
          inRrn = %Editc(DBrrn:'X');
          inMode = 'C';
          Callp ProcessPhys(inRrn:inMode);
       When EObtyp = 'H';
       When EObtyp = 'X';
       When EObtyp = 'D';
       Endsl;

     p ProcessAll      e
       //===================================================================
       // OverrideFiles - Override files to the adjudication state
       //===================================================================
     p OverrideFiles   b
     d OverrideFiles   pi
     d strFile         s             10a
       Dcl-S inCmdLen packed(15:5) inz(40);
       Dcl-S inCmd char(40);

       strFile = 'FTABL' + %Trim(EOast);
       inCmd = 'OVRDBF FILE(FTABL) TOFILE(*LIBL/' +
        %Trim(strFile) + ')';
       RunCommand(inCmd:inCmdLen);

     p OverrideFiles   e

       //===================================================================
