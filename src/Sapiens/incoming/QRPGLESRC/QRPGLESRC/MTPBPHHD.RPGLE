     h*copyright('(c) 2025 Sapiens, Inc. All rights reserved. -
     h**********************************************************************
     h dftactgrp(*no) actgrp(*caller)                                           5  0 inz
     h option(*nodebugio: *srcstmt)
     H bnddir('STBNDDIR')
     h**********************************************************************
     h*
     h*  Description: Provider Bill Physician Type
     h*               - Activation group should be *CALLER
     h*               - Do not seton LR, just do a return
     h*               - This process should not lock any file
     h*               - This process should only read files, no updates/writes s
     h*
     h*  Called from: Provider Bill Primary Information
     h*  Calls to:
     h*
     h*  Created on 06/18/2024 by C:333/079 for Project MC448
     h**********************************************************************
     h* Revisions:                                                         *
     h*                                                                    *
     h**********************************************************************

     fmtpbphhd  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflPbPhyDt:rrnPbPhyDt)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdMcPnl)
     ffmfnl     if   e           k disk

     Ffaeop     uf a e             disk
     f                                     infds(dbfds1)
     Ffaeal1    uf a e           k disk
     Ffaehl     uf a e           k disk
     Ffdcal19   if   e           k disk
     f                                     infds(dbfds2)
     Ffddhl     if   e           k disk
     Ffddel     if   e           k disk
     Ffmcml1    if   e           k disk
     Ffmcdl     if   e           k disk
     Ffmpdl     if   e           k disk
     Fsmcol     if   e           k disk
     Ffmcxl     if   e           k disk

     Fftgtl     if   e           k disk
     Ffmicd10l1 if   e           k disk
     Fststl     if   e           k disk
     ffmprl     if   e           k disk    UsrOpn
      //======================================================================
      // Data Structures
      //======================================================================
     d psds          esds                  extname(szq1p)

     d dbfds1          ds
     d  dbrrn1               397    400b 0

     d dbfds2          ds
     d  dbrrn2               397    400b 0

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
     d svrrnPrv        s              9  0 inz
     D decRrn          s              9s 0 inz
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d error           s               n   inz(*off)
     d process         s               n   inz(*off)
     d charMsg         s            100
     d strClmZip       s             10a
     d xxDelResp       s               n   inz
     d xxCpyResp       s               n   inz
     d erCtyE          s               n   inz
     d msCtyE          s             15A
     d ##Wcif          s              1
     d ##Mcif          s              1
     d ##Wcin          s              1
     d isValid         s              1n
     d outCancelBtn    s              1
     d inMode          s              1
     d inRrn1          s              9  0
     d inComPndDft     s              1
     d inComPndDCd     s              3
     d inCancelBill    s              1
     d inCharges       s             11  2

      //New variables for copying a bill
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
     d svsflrrn        s              5  0 inz
      //end variables for copying a bill
      //======================================================================
      // End Standalone Fields
      //======================================================================

      //======================================================================
      // Proto-types
      //======================================================================

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

      // Process Provider Bill
     d ProcessPrvBill  pr                  extpgm('F0393R')
     d   outCancelBtn                 1
     d   inMode                       1
     d   inRrn1                       9  0
     d   inComPndDft                  1
     d   inComPndDCd                  3
     d   inCancelBill                 1
     d   inCharges                   11  2

       // Initialize the Screen fields for Provider Bills
     d InzProvBills    pr

       // clear subfiles
     d clrSubFiles     pr

      // Load Claimant Grid
     d LoadPhysBill    pr

      * Main Processing
     d Main            pr

      * Process Edit Provider Bill information
     d ProcOther       pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      * Program End
     d ProgramEnd      pr
      //=======================================================================
      // Copy books
      //=======================================================================
     d/copy sprUsrPrms
     d/copy sPrGenPop
     d/copy sPrDate
     d/copy sPrString
     d/copy sPrSql
     d/copy sPrMessage
     d/copy sPrModule
     d/copy sCopyCompr
     d/copy sprCompany
     d/copy sPrTable
     d/copy sPrTrigger
     d/copy sPrTrig
     d/copy sPrAtrium
     d/copy sPrMask
     d/copy sCopyMCPr
     d/copy mPrPbDtPop
     d/copy sPrFiles
     c     *entry        plist
     c                   parm                    inRrn             9
     c                   parm                    inMode            1
       //===================================================================
       // Begin Processing
       //===================================================================
       ProgramStart(process);
       If process;
          If inMode = 'D';
             inquiry = *on;
          Endif;
          Main();
       Endif;

       ProgramEnd();
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
     d*
     d  isexit         s               n   Inz(*off)
     d  Mode           s              1a


       BtnCancel = *off;

       // Process until Cancel button is pressed.
       DoW BtnCancel = *off;

          // Load the MC action panel (this proc is defined through SCOPYMCPI)
          LoadMCActPan();

          // clear Sub-files
          clrSubfiles();

          // Load Provider Bill information
          Select;
          When EObTyp = 'N';
             loadPhysBill();
          Endsl;

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;
          // do nothing - catch tab click event
          When TabClick = *on;
             TabClick = *off;

          // Add
          When btnAdd = *On;
              btnAdd = *off;
              Mode = 'A';
              PhyDtlEdit(CAco#:inRrn1:scRrnDtl:Mode);

          // Refresh
          When btnRefresh = *On;
              btnRefresh =*Off;

          // (Enter key or Save button)
          When btnSave = *on;
             btnSave = *off;
             If inMode = 'A' or inMode = 'C';
                Select;
                When EObTyp = 'N';
                   ValidPhysHead();
                   If isValid = *on;
                      ProcPhyHead();
                   Endif;
                Endsl;
             Endif;

          // Process
          When btnProcess = *On;
             btnSave = *off;
             If inMode = 'A' or inMode = 'C';
                Select;
                When EObTyp = 'N';
                   ValidPhysHead();
                   If isValid = *on;
                      ProcPhyHead();
                   Endif;
                Endsl;
             Endif;
             OutCancelBtn = *blank;
             Chain (EOco#:EOsub#:EOfnd) pcx01;
             If %Found(FMCXL);
                inComPndDft = CXCpDf;
                inComPndDCd = CXCpPn;
             Endif;
             If inMode = 'A';
                inCancelBill = 'Y';
             Else;
                inCancelBill = ' ';
             Endif;
             inCharges = EOchrg;

             callp ProcessPrvBill(outCancelBtn:inMode:inRrn1
              :inComPndDft:inComPndDCd:inCancelBill:inCharges);
             ProgramEnd();
             Return;
          Other;
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
          pnlHeight = 640;
          APnlHeight = pnlHeight + 62;

          // Load initial values
          Initialize();

          If dspMode;
             inquiry = *on;
          Endif;

          Write rcdMcPnl;
          Write rcdTabPnl;
          Select;
          When EObTyp = 'N';
             Write rcdPhys;
             Exfmt rcdPhys;
          Endsl;
          Read rcdMcPnl;
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
     d strFile         s             10a

       // Tab Index
       tabIndex = 0;

       GetClientID(clientID);

       intCo# = GetUserInfo(q1user);

       decRrn = %Dec(inRrn:9:0);
       Chain(n) decRrn peo01;
       If %Found(FAEOP);
          processMe = *on;
          inRrn1  = dbRrn1;
          scHdRrn = dbRrn1;
          // Override FMPRL to the Adjudication State to get description
          If not %Open(FMPRL);
             strFile = 'FMPRL' + %Trim(EOast);
             ovrFile('FMPRL':' ':strFile:' ');
             Open FMPRL;
          Endif;
       Else;
          processMe = *off;
       endif;

       ProgramEnd();

      /end-free
     p ProgramStart    e
       //===================================================================
       // End Processing
       //===================================================================
     p ProgramEnd      b
     d ProgramEnd      pi

       // Delete Overrides
       If %Open(FMPRL);
          DltOvr('FMPRL');
          Close FMPRL;
       Endif;

       *InLr = *On;
       Return;

     p ProgramEnd      e
       //===================================================================
       // Load Provider Bill
       //===================================================================
     p LoadPhysBill    b
     d LoadPhysBill    pi

     d ArrDxr          s              1a   Dim(4)
     d n               s              1s 0

       Chain (CMss#) pcd01;
       scClmNm    = %Trim(EOlsn) + ', ' + %Trim(EOfsn) + ' ' + EOmid;
       scClmDob   = DtmDyy(CDdtoB);
       scClmAdr   = CDadr1;
       strClmZip   = %Editc(CDzip:'X');
       strClmZip   = %Subst(strClmZip:1:5) + '-' + %Subst(strClmZip:6:4);
       scClmCsz = %Trim(CDcty) + ', ' + CDst + ' ' + strClmZip;
       scClmPhone = %Editc(CDphn#:'X');
       scClmPhone = '(' + %Editc(CDarea:'X') + ') ' + %Subst(scClmPhone:1:3) +
        '-' + %Subst(scClmPhone:4:4);

       Chain (EOco#:EOSub#:EOfnd:EOfyr:EOcase:EOeob#) peh01;

       scIcdInd   = EHIcdInd;

       Select;
       When EHicdInd = 'Y';
          If EHicd1001 <> *blanks;
             scDiagA = %Trim(EHicd1001);
          Endif;
          If EHicd1002 <> *blanks;
             scDiagB = %Trim(EHicd1002);
          Endif;
          If EHicd1003 <> *blanks;
             scDiagC = %Trim(EHicd1003);
          Endif;
          If EHicd1004 <> *blanks;
             scDiagD = %Trim(EHicd1004);
          Endif;
          If EHicdE <> *blanks;
             scDiagE = %Trim(EHicdE);
          Endif;
          If EHicdF <> *blanks;
             scDiagF = %Trim(EHicdF);
          Endif;
          If EHicdG <> *blanks;
             scDiagG = %Trim(EHicdG);
          Endif;
          If EHicdH <> *blanks;
             scDiagH = %Trim(EHicdH);
          Endif;
          If EHicdI <> *blanks;
             scDiagI = %Trim(EHicdI);
          Endif;
          If EHicdJ <> *blanks;
             scDiagJ = %Trim(EHicdJ);
          Endif;
          If EHicdK <> *blanks;
             scDiagK = %Trim(EHicdK);
          Endif;
          If EHicdL <> *blanks;
             scDiagL = %Trim(EHicdL);
          Endif;
       When EHicdInd <> 'Y';
          scDiagA    = EHdia1;
          scDiagB    = EHdia2;
          scDiagC    = EHdia3;
          scDiagD    = EHdia4;
          scDiagE    = *blanks;
          scDiagF    = *blanks;
          scDiagG    = *blanks;
          scDiagH    = *blanks;
          scDiagI    = *blanks;
          scDiagJ    = *blanks;
       Endsl;

       scAcct        = EHacct;
       scfein        = EOfein;

       // Load detail line items
       Setll (EOco#:EOSub#:EOfnd:EOfyr:EOcase:EOeob#) pca01;
       Reade (EOco#:EOSub#:EOfnd:EOfyr:EOcase:EOeob#) pca01;
       Dow Not %Eof(FDCAL19);
          scFrmSrvDt = Dtmdyy(CAfdat);
          scChrg     = CAchrg;
          scUnits    = CAunts;
          scPaid     = CApaid;

          Chain (CAco#:CAsub#:CAfnd:CAfyr:CAcase:CAeob#:CAdSeq) pdh01;
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

          scRrnDtl   = dbrrn2;
          rrnPbPhyDt = rrnPbPhyDt + 1;
          write sflPbPhyDt;

          Reade (EOco#:EOSub#:EOfnd:EOfyr:EOcase:EOeob#) pca01;
       Enddo;


     p LoadPhysBill    e
       //===================================================================
       // ValidatePhysHead - Validate Physician Header Information
       //===================================================================
     P ValidPhysHead   B
     D ValidPhysHead   PI
       isValid = *off;

       isValid = *on;

     p ValidPhysHead   e
       //===================================================================
       // ProcPhyHead - Write/Update Physician Header Information
       //===================================================================
     P ProcPhyHead     B
     D ProcPhyHead     PI

       Select;
       When EhIcdInd = 'Y';
          EHicd1001 = scDiagA;
          EHicd1002 = scDiagB;
          EHicd1003 = scDiagC;
          EHicd1004 = scDiagD;
          EHicdE = scDiagE;
          EHicdF = scDiagF;
          EHicdG = scDiagG;
          EHicdH = scDiagH;
          EHicdI = scDiagI;
          EHicdJ = scDiagJ;
          EHicdK = scDiagK;
          EHicdL = scDiagL;
       When EhIcdInd <> 'Y';
          EHdia1 = scDiagA;
          EHdia2 = scDiagB;
          EHdia3 = scDiagC;
          EHdia4 = scDiagD;
          EHicdE = *blanks;
          EHicdF = *blanks;
          EHicdG = *blanks;
          EHicdH = *blanks;
          EHicdI = *blanks;
          EHicdJ = *blanks;
          EHicdK = *blanks;
       Endsl;

       EHacct = scAcct;
       EOfein = scfein;

       Select;
       When InMode = 'A';
          EHiUsr = Q1user;
          EHiPgm = Q1pgm;
          EHuser = Q1user;
          EHpgm  = Q1pgm;
          EHiDt  = DateTo7(SystemDate());
          EHiTim = TimeTo6(SystemTime());
          EHdt   = DateTo7(SystemDate());
          EHtime = TimeTo6(SystemTime());
          Write peh01;
       When InMode = 'C';
          EHuser = Q1user;
          EHpgm  = Q1pgm;
          EHdt   = DateTo7(SystemDate());
          EHtime = TimeTo6(SystemTime());
          Update peh01;
       Endsl;

     p ProcPhyHead     e
       //===================================================================
       // ProcOther - Process other Options for Physician Bills
       //===================================================================
     P ProcOther       B
     D ProcOther       PI
     d  Mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

       //***Clear rcdTabPnl;
       Clear PopAddEdit;
       isexit = *off;
       ReadC sflPbPhyDt;
       Dow not %eof;
          Select;
          When SvcLnEdit = *on;
             SvcLnEdit = *off;
             mode = 'E';
             svsflrrn = rrnPbPhyDt;
             PhyDtlEdit(CAco#:inRrn1:scRrnDtl:Mode);
          Endsl;
          ReadC sflPbPhyDt;
       Enddo;

     P ProcOther       E

       //===================================================================
       // Initialize - Initialize Physician fields
       //===================================================================
     p Initialize      b
     d Initialize      pi

     p Initialize      e

       //===================================================================
       // ClrSubFiles - Clear Subfiles
       //===================================================================
     p ClrSubFiles     b
     d ClrSubFiles     pi
       Select;
       When EObTyp = 'N';
          // Physician Detail Subfile
          rrnPbPhyDt = 0;
          clrPbPhyDt = *on;
          dspPbPhyDt = *Off;
          dspPhyDCtl = *off;
          Write rcdPhys;
          clrPbPhyDt = *off;
          dspPbPhyDt = *on;
          dspPhyDCtl = *on;
          scRowSel = 1;
       Endsl;

     p ClrSubFiles     e

     P/copy SCopyMCPi
