     hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h*****************************************************************
     h/title MTRPTDTCHP - Detail Charges Listing
     h/define CONVERT
     h dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('STBNDDIR')
     h*****************************************************************
     h* Created: 06/16/17 Project: MC425                              *
     h*****************************************************************
     h* Revisions:                                                    *
     h*                                                               *
     h* 12/18/24 C:342 Project : MC447 Phase 2                        *
     h*                DSPF Changes:                                  *
     h*                1.Changed Existing Group field into Typeahead  *
     h*                                                               *
     h*****************************************************************
      *
     fMtrptdtchpcf   e             workstn extfile('MTRPTDTCHP')
     f                                     handler('PROFOUNDUI(HANDLER)')
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdSysTbl)
     f*
     f* User Parameter Master File
     fSmupl     IF   E           K DISK
     fFmsml     IF   E           K DISK
     fFmfnl     IF   E           K DISK
     fFmfnp     IF   E             DISK
     f                                     RENAME(PFN01:PFN02)
     fFmpdl     IF   E           K DISK
     fFmcml1    IF   E           K DISK
     f                                     RENAME(PCM01:PCM02)
      *
      *****************************************************************
      * Prototype Declaration
      *****************************************************************

     d/copy sPrString
     d/copy sPrCompany
     d/copy sPrUsrPrms
     d/copy sPrDate
     d/copy sPrGenPop

      * Entry of Program.
     d MTrptDtChPprms  pr                    Extpgm('MTRPTDTCHP')
     d                                3  0
     d                                3  0
     d                                7  0
     d                                9  0
     d                                3  0
     d MTrptDtChPprms  pi
     d  @@Fnd                         3  0
     d  @@Fyr                         3  0
     d  @@Case                        7  0
     d  @@Fein                        9  0
     d  @@Mod                         3  0

      * Main Processing
     d Main            pr

      * Program Start
     d ProgramStart    pr

      * Intialize the Screen fields
     d LoadInVal       pr

      * Clear Error/Message fields
     d ClrErr          pr

      * Validate Panel Sreen Values
     d ValidPnl        pr

      * Group Search
     d GroupSrch       pr

     d GrpSearch       pr                  ExtPgm('F0060R')
     d inCall                         1
     d                                1
     d                                1
     d                                9  0

      * Claim Number Search
     d ClaimSrch       pr
      *
     d ClmSearch       pr                  ExtPgm('F0269R')
     d                                1
     d                                5  0
     d                                3  0
     d                                3  0
     d                                7  0
     d                                9
     d                                8  0
     d                                1

      * Provider Search
     d ProviderSrch    pr
      *
     d PrvdrSearch     pr                  ExtPgm('F0032R')
     d                                1
     d                                9
     d                                3
     d                                1
     d                                1

      * Detail Charges Listing Process
     d DetChgListing   pr

     d DtlChgListing   pr                  ExtPgm('F9950C')
     d                                3
     d                                5
     d                                3
     d                                3
     d                                7
     d                                9
     d                                8
     d                                9
     d                                3
     d                                1
     d                               10
     d                                1    Const

      * Program Status DS
     d psds          esds                  extname(szq1p)

      * Stand Alone
     d MsgF            s             10    inz('FSMESG')
     d MsgFlib         s             10    inz('*LIBL')
     d MsgIdSub        s             10    INZ('MCR0301')
     d MsgIdFnd        s             10    INZ('MCR0508')
     d MsgIdYear       s             10    INZ('MCR0681')
     d MsgIdClmN       s             10    INZ('MCR0681')
     d MsgIdFein       s             10    INZ('MCR0546')
     d MsgIdMod        s             10    INZ('MCR0546')
     d MsgIdChTyp      s             10    INZ('MCR0324')


     d inKC            s              1
     d inKL            s              1

     d inCall          s              1
     d inMode          s              1
     d inSort          s              2
     d inSub#          s              5  0 inz
     d inSub           s              5    inz
     d inFnd           s              3  0 inz
     d inFund          s              3    inz
     d inFyr           s              3  0 inz
     d inYear          s              3    inz
     d inCase          s              7  0 inz
     d inCase#         s              7    inz
     d inSS#           s              9    inz
     d inACDT          s              8  0 inz
     d inADT           s              8    inz
     d inRTCD          s              1    inz
     d inFein          s              9    inz
     d inMod           s              3    inz
     d inType          s              1    inz
     d inOutq          s                   like(UPoutq) inz
     d OutRrn1         s              9  0

     d isValid         s               n   Inz(*on)
      *
     d inCo#           s              3
      *
     d intCo#          s                   Like(UPco#)
     d strSS#          s              9
     d intACDT         s              8  0
     d PopMsg          s            256
     d PopTitle        s             50    inz('Detail Charges Listing')


      /Free
       // Start Of Program
       ProgramStart();

       // Main Procesing
       Main();

       *InLr = *on;

      /End-Free
       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi

      /Free

       // Process until exit button is pressed.
       Dou ExitNow = *On ;

          Exsr ExfmtScreens;                     //Display screen

          Select;
       //*** Beg Del *** 05/03/19 ******************************************
       //*** When selGrpSrch = *On;                 //Group search
       //*** GroupSrch();
       //*** End Del *** 05/03/19 ******************************************
          When selClmSrch = *On;                 //Claim search
             ClaimSrch();
          When selFeinSch = *On;                 //Provider search
             ProviderSrch();
          When selModSrch = *On;
             ProviderSrch();
          When BtnSubmit = *on;                  // Submit
             ClrErr();                             //Clear errors
             ValidPnl();                           //Validate
             If isValid = *On;
                DetChgListing();
                Leave;
             EndIf;
          When BtnCancel = *on;                  //Cancel
             Leave;
          When tabClick = *on;
             tabClick = *off;
          When BtnRefresh = *On;
             BtnRefresh =*Off;
             LoadInVal();                        //Initial values
             ClrErr();                           //Clear errors
          EndSl;

       Enddo;
       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       Select;

       When TabIndex = 0;
          TabHeight = 770;
          APnlHeight = TabHeight + 60;

          Write rcdSysTbl;
          Write RcdTabRpt;

          Exfmt RcdDtlChgR;

          Read rcdSysTbl;
          Read RcdTabRpt;

       EndSl;

       Endsr;

      /End-Free

     p Main            e
       //===================================================================
       // ProgramStart - Initial Program
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi

      /Free
       // Intialize the Screen fields
       LoadInVal();

       // Load Action panel with the Company Name.
       intCo# = getUserInfo(q1user);
       scCompName = GetCompNam(intCo#);

       tabClick = *Off;

       Return;

      /End-Free

     p ProgramStart    e
       //===================================================================
       // LoadInVal - Intialize the Screen fields
       //===================================================================
     p LoadInVal       b
     d LoadInVal       pi
      /Free
       // Retrieve Subscriber
       Chain q1user pup01;
       If %Found(Smupl);
          scFnd  = UPfnd;
          scSub# = Upsub#;
          inOutq = UPoutq;
       EndIf;
       //*** Beg Add *** 05/03/19 *****************************************
       // If NOT called from the menu use entry parms
       If Q1prms > 0;
          scFnd = @@Fnd;
          scYear = @@Fyr;
          scClmNbr = @@Case;
          scFein = @@Fein;
          scMod  = @@Mod;
       Endif;
       //*** End Add *** 05/03/19 *****************************************

      /End-Free
     p LoadInVal       e
       //===================================================================
       // ClrErr - Clear Error/Message fields
       //===================================================================
     p ClrErr          b
     d ClrErr          pi
      /Free
       erSub    = *off;
       erFnd    = *off;
       erYear   = *off;
       erClmNbr = *off;
       erChgTyp = *off;
       erFein   = *off;
       erMod    = *off;
       isValid  = *on;
      /End-Free
     p ClrErr          e
       //===================================================================
       // ValidPnl - Validate Panel Screen Fields
       //===================================================================
     p ValidPnl        b
     d ValidPnl        pi

      /Free
        // Validate Subscriber
           Chain (UPco#:scSub#) FMsml;
           If not %found(FMsml);
              erSub = *on;
              isValid = *off;
           Else;
        // Validate Fund
              Chain (UPco#:scSub#:scFnd) FMfnl;
              If not %found(FMfnl) or scFnd = *zeros;
                 erFnd = *on;
                 isValid = *off;
              Endif;
           Endif;

       // Validate claim number
          Chain (UPco#:scSub#:scFnd:scYear:scClmNbr) fmcml1;

          If not %found(fmcml1);
             erClmNbr = *on;
             isValid = *off;
          Endif;

       // Validate type of charge
          If scChgTyp = *blanks;
             erChgTyp = *on;
             isValid = *off;
          Endif;

       // Validate provider
         If scFein > *zeros or scMod > *zeros;
            Chain (scFein:scMod) FMpdl;
            If not %found(FMpdl) ;
               erFein = *on;
               erMod = *on;
                isValid = *off;
            Endif;
         Endif;
      /End-Free
     p ValidPnl        e
       //===================================================================
       // DetChgListing - Detail Charges Listing Submit
       //===================================================================
     p DetChgListing   b
     d DetChgListing   pi

      /Free
       inCo#   = %editc(CMco#:'X');
       inSub   = %editc(CMsub#:'X');
       inFund  = %editc(CMfnd:'X');
       inYear  = %editc(CMfyr:'X');
       inCase# = %editc(CMcase:'X');
       inSS#   = CMss#;
       inADT   = %editc(dtMDYY(CMacdt):'X');
       inFein  = %editc(scFein:'X');
       inMod   = %editc(scMod:'X');
       inType  = scChgTyp;
       inOutq  = UPoutq;

       DtlChgListing(inCo#:inSub:inFund:inYear:inCase#:inSS#:inADT:inFein:
                     inMod:inType:inOutq:'D');

       PopMsg = 'Detail Charges Listing has been generated.';
       PopOneBtn(PopMsg:PopTitle);
      /End-Free

     p DetChgListing   e
       //===================================================================
       // Group search
       //===================================================================
     p GroupSrch       b
     d GroupSrch       pi
      /Free
       inCall = '2';
       inKC = '0';
       inKL = '0';
       outRrn1 = *zeros;

       GrpSearch(inCall:inKC:inKL:outRrn1);
       Chain outRrn1 Fmfnp;
       If %found(Fmfnp) and inKC = '0';
          Scfnd = FNfnd;
          scSub# = FNsub#;
          erFnd = *off;
       Endif;
      /End-Free
     p GroupSrch       e
       //===================================================================
       // Claim search
       //===================================================================
     p ClaimSrch       b
     d ClaimSrch       pi
      /Free
       inCall = '2';
       inSub# = *zeros;
       inFnd  = *zeros;
       inFyr  = *Zeros;
       inCase = *zeros;
       inSS#  = *blanks;
       inAcdt = *zeros;
       inRtcd = *blanks;
       outRrn1 = *zeros;

       ClmSearch(inCall:inSub#:inFnd:inFyr:inCase:inSS#:inACDT:inRTCD);

       If inRTCD = '*';
          scClmNbr = inCase;
          scYear   = inFyr;
          strSS#   = inSS#;
          intAcdt  = inAcdt;
          scFnd    =  inFnd;
          scSub#   =  inSub#;
          erClmNbr = *off;
       Endif;
      /End-Free
     p ClaimSrch       e

       //===================================================================
       // Provider search
       //===================================================================
     p ProviderSrch    b
     d ProviderSrch    pi
      /Free
       inCall = '2';
       inFein = *blanks;
       inMod  = *blanks;
       inRtcd = *blanks;
       inType = *blanks;

       PrvdrSearch(inCall:inFein:inMod:inRtcd:inType);

       If inRTCD = '*';
          scFein = %int(inFein);
          scMod  = %int(inMod);
          erFein = *off;
          erMod  = *off;
       Endif;
      /End-Free
     p ProviderSrch    e

