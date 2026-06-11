     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')

     h/title SPIMODULE - Service program for Modules
     h nomain
     h DFTNAME(SPIMODULE)
     h option(*nodebugio: *srcstmt) BNDDIR('SPIMODULE')
      **********************************************************************
      *  Description:  This copybook will be used to check if a particular
      *  module is available or not
      *
      *  Called from: SPIMODULE
      *  Calls to:
      *
      *  Created: 12/01/14 C:300   Project: 114WLF
      **********************************************************************
      * Revisions:
      *
      * 07/18/25 C:342 Project: MC451
      *                NCCI Med Data Call Enhancements
      *
      * 09/25/24 C:65  Project: SY446 (Design and Compose Revamp)
      *                Add "Design and Compose" to isModEnabled.
      *
      * 06/10/24 C:342 Project: MC447
      *                MC447 - Redesign of Work with Groups
      *
      * 08/12/24 C:333 Ticket: 99/115952
      *                Fix positioning on Documents grid
      *                - Recompiled due to changes in STAPPCFG.
      *
      * 08/21/23 C:333 Project: WC2876
      *                Output Control Functionality Enhancements
      *
      * 02/23/23 C:340 Project: WC2430
      *                Add New Paymnets to isModEnabled.
      *
      * 08/18/22 C:333 Project: MC443
      *                Modernized EOB Print
      *
      * 03/11/21 C:33  Project: WC2663
      *                Add branding for S/R Online
      *
      * 01/11/21 C:33  Project: WC2663
      *                Add Invoice Cloud to isModEnabled.
      *
      * 09/18/20 C:136 Project: WC2749
      *                Add 'EFT', 'Claims EFT', 'Premium EFT',
      *                and 'Agent EFT' to isModEnabled.
      *
      * 07/20/20 C:79  Project: WC2749
      *                Add E-Pay to isModEnabled.
      *
      * 06/24/19 C:65  Project: SY407
      *                Add Third Party/Online Payment module flag.
      *
      * 06/21/19 C:180 Project: WC2604
      *                Add WCsc07 (Pos 1) as IDC installed.
      *
      * 06/20/19 C:62  Project: 152MAV
      *                Add Negative Fee Check flag.
      *
      * 05/22/19 C:79  Support: 152/79622
      *                Add Work Comp Interface to isModEnabled
      *
      * 03/18/19 C:79  Project: 152WAA
      *                Add SYFL02 as Managed Care Installed
      *
      * 03/14/19 C:79  Support: 99/77896
      *                Add Pbills Lite to IsModEnabled
      *
      * 03/04/19 C:33  Project: SY390
      *                Add GetModuleProperty & SetModuleProperty
      *
      * 08/23/18 C:062 Project: SY390R
      *                Add "Portal Module"
      *
      * 05/14/18 C:291 Project: 150WCR
      *                Add "PEO Module"
      *
      * 04/05/18 C:33  Project: 150WCF
      *                Add "Group Copy"
      *
      * 03/08/18 C:079 Project: 114MJC
      *                Add "Use Image Extension"
      *
      * 02/09/18 C:180 Project: 114MJC
      *                Add "835 ASC X12 Payment Advice"
      *
      * 09/25/17 C:180 Project: 114MJB
      *                Add "837 ASC X12 Prof", "837 ASC X12 Inst"
      *
      * 07/20/17 C:33  Project: WC2405
      *                Add "Rule SQL Action"
      *
      * 05/17/17 C:79  Project: MC410
      *                Add "WCMCInterface"
      *
      * 05/10/17 C:33  Project: WC2405
      *                Add "SMS Enabled"
      *
      * 04/12/17 C:180 Project: MC325
      *                Add "Managed Care" module check.
      *
      * 01/24/17 C:33  Project: SY388
      *                Use system flag for Dashboard Configuration
      *
      * 12/08/16 C:079 Project: MC325
      *                Add Import/Export installed
      *
      * 11/22/16 C:309 Project: 25MUJ
      *                Add "Line Validation"
      *
      * 08/04/16 C:079 Support: 42552/114
      *                Add "Multiple Units"
      *
      * 07/25/16 C:136 Project: 114WYQ
      *                Add Relay module for SMS texting
      *
      * 07/15/16 C:308 Project: C114WXY
      *                Add "ISO WorkComp"
      *
      * 06/10/16 C:33  Project: WC2405
      *                Add "Rules Engine"
      *
      * 05/24/16 C:302 Project: 114WYS
      *                Add to procedure IsModEnabled process that checks
      *                to see if the Incurred Loss Retrospective Rating Module
      *                is installed.
      *
      * 04/12/16 C:79  Project: 114MWK
      *                Add Workers Comp Installed and Interface
      *
      * 03/17/16 C:33  Project: SY285
      *                Add CiCenter
      *
      * 10/23/15 C:33  Project: SY371
      *                Add Scriptura Module
      *
      * 08/07/15 C:136 Project: 133WPH
      *                Add Lockbox Module
      *
      * 05/22/15 C:279 Project: 114WKA
      *                Add Auto Diary Triggers Module
      *
      * 12/04/14 C:242 Project 114WGU
      *                Add Visit Tracking Module

      **********************************************************************

      *** Beg Add ***** 08/21/23 ***************************************
     fstappcfg  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflToGrp:rrnToGrp)
     f                                     Sfile(sflDupl:rrnDupl)
     f                                     usropn
     fwmfnl     if   e           k disk    rename(pfn01:pfn00)
     f                                     usropn
     f                                     infds(dbfd14)
     fstsp1p    uf a e             disk    rename(psp011:psp11p)
     f                                     usropn
     fstsp2l    uf a e           k disk    rename(psp012:psp12l)
     f                                     infds(dbfdsC)
     f                                     usropn
     fstsp2p    uf a e             disk    rename(psp012:psp12p)
     f                                     usropn
     fstsp3l    uf a e           k disk    rename(psp013:psp13l)
     f                                     infds(dbfdsD)
     f                                     usropn
     fstsp3p    uf a e             disk    rename(psp013:psp13p)
     f                                     usropn
     fstsp4l    uf a e           k disk    rename(psp014:psp14l)
     f                                     infds(dbfdsE)
     f                                     usropn
     fstsp4p    uf a e             disk    rename(psp014:psp14p)
     f                                     usropn
     fwmfnp     uf a e             disk    rename(pfn01:pfn0p)
     f                                     usropn
     fwdf2l     if   e           k disk    rename(pf201:pf200)
     f                                     usropn
     fwscrl     uf a e           k disk    rename(PCR01:PCR00)
     f                                     infds(dbfds)
     f                                     usropn
     fwscrp     uf a e             disk    rename(PCR01:PCR0P)
     f                                     usropn
     fwscil     uf a e           k disk    rename(PCI01:PCI00)
     f                                     infds(dbfds1)
     f                                     usropn
     fwscip     uf a e             disk    rename(PCI01:PCI0P)
     f                                     usropn
     fwsffl     uf a e           k disk    rename(PFF01:PFF00)
     f                                     infds(dbfds2)
     f                                     usropn
     fwsffp     uf a e             disk    rename(PFF01:PFF0P)
     f                                     usropn
     fwsppl     uf a e           k disk    Rename(PPP01:PPP00)
     f                                     infds(dbfds3)
     f                                     usropn
     fwsppl1    uf a e           k disk    Rename(PPP01:PPP01)
     f                                     infds(dbfds4)
     f                                     usropn
     fwsppp     uf a e             disk    Rename(PPP01:PPP0P)
     f                                     usropn
      *** End Add ***** 08/21/23 ***************************************
     fswcpf     if   e             disk
     fssypf     if   e             disk
     fsfbpf     if   e             disk

     f*** Beg Add *** 08/21/23 *************************************************
     d clientID        s              3    inz('000')
     d xxSelCount      s              3  0 inz

      // Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3


     d psds          esds                  extname(szq1p)
     d dbfd14          ds
     d  dbrrn14              397    400b 0
     d dbfdsC          ds
     d  dbrrnC               397    400b 0
     d dbfdsD          ds
     d  dbrrnD               397    400b 0
     d dbfdsE          ds
     d  dbrrnE               397    400b 0
     d dbfds           ds
     d  dbrrn                397    400b 0
     d dbfds1          ds
     d  dbrrn1               397    400b 0
     d dbfds2          ds
     d  dbrrn2               397    400b 0
     d dbfds3          ds
     d  dbrrn3               397    400b 0
     d dbfds4          ds
     d  dbrrn4               397    400b 0
     f*** End Add *** 08/21/23 *************************************************
      * copy books
     d/copy sPrModule
     d/copy sPrSql
      *** Beg Add *** 08/21/23 *************************************************
     d/copy sPrDate
     d/copy sprUsrPrms
     d/copy sPrRunCmd
      *** End Add *** 08/21/23 *************************************************

       //===================================================================
       // Never processed. Only used for setting COMMIT
       //===================================================================
     pAAA              b
      *** Beg Add *** 08/21/23 *************************************************
       GetClientID(clientID);
      *** End Add *** 08/21/23 *************************************************
      /Free
       Exec SQL
        set option commit = *none;
      /End-free
     pAAA              e

       //===================================================================
       // GetModuleProperty
       //===================================================================
     pGetModuleProperty...
     p                 b                   export
     dGetModuleProperty...
     d                 pi           256    varying
     d inModule                      20    const varying
     d inKey                         25    const varying
      *
     d**********strProperty      s            256    varying inz('Connect')
     dstrProperty      s            256    varying inz
      /Free
       Reset strProperty;
       Select;
       When inModule = 'Portal Module' and inKey = 'Name';
          strProperty = 'Connect';
       When inModule = 'S/R Online' and inKey = 'Name';
          strProperty = 'Pay as You Go';
       Endsl;
       Exec sql
        select MIproperty into :strProperty
         from STMIP
         where MImodule = :inModule and MIkey = :inKey;
       Return %trim(strProperty);
      /End-free
     pGetModuleProperty...
     p                 e

       //===================================================================
       // IsModEnabled - Is Module Enalbled
       //===================================================================
     p IsModEnabled    b                   Export
     d IsModEnabled    pi             1n
     d strMod                        20    const
     d isPortal        s               n   Inz(*Off)

      /Free
       Setll *start swcpf;
       Read swcpf;
       If not %eof(swcpf);
          Setll *start ssypf;
          Read ssypf;
          Select;
          When strMod = 'CLAIMS WORKSHEET';
             If %subst(WCsc01:1:1) = 'Y';
                return *on;
             Endif;
          When (strMod = 'VISIT TRACKING') and %subst(WCsc02:1:1) = 'Y';
             return *on;
          When (strMod = 'Group Copy') and %subst(SYfl12:1:1) = 'Y';
             Return *on;
          When (strMod = 'Auto-Diary Triggers') and not %eof(ssypf) and
           %subst(SYfl12:3:1) = 'Y';
             return *on;
          When (strMod = 'Rules Engine') and %subst(SYfl12:5:1) = 'Y';
             Return *on;
          When (strMod = 'SMS Enabled') and %subst(SYfl12:7:1) = 'Y';
             Return *on;
          When (strMod = 'Rule SQL Action') and %subst(SYfl12:8:1) = 'Y';
             Return *on;
          When (strMod = 'LOCKBOX') and %subst(WCsc03:1:1) = 'Y';
             Return *on;
          When (strMod = 'Scriptura-Documents') and %subst(SYfl11:7:1) = 'Y';
             Return *on;
          When (strMod = 'Scriptura-Various') and %subst(SYfl11:8:1) = 'Y';
             Return *on;
          When (strMod = '3rd Party XML') and %subst(SYfl11:9:1) = 'Y';
             Return *on;
       //*** Beg Add ** 09/25/24 **************************************************
          When strMod = 'Design and Compose' and %subst(SYfl11:10:1) = 'Y';
             Return *on;
       //*** End Add ** 09/25/24 **************************************************
          When (strMod = 'CiCenter') and WCfl19 = 'Y'                       ;
             Return *on;
          When (strMod = 'WorkComp') and SYWCIF = 'Y'                       ;
             Return *on;
          When (strMod = 'NonPDSWC') and SYFL05 = 'Y'                       ;
             Return *on;
          When (strMod = 'Incurred Loss') and WCfl11 = 'Y';
             Return *on;
          When (strMod = 'ISOWorkComp') and %subst(SYfl12:4:1) = 'Y';
             Return *on;
          When (strMod = 'RelaySMS') and %subst(SYfl12:6:1) = 'Y';
             Return *on;
          When (strMod = 'Dashboard Config') and %subst(SYfl15:1:1) = 'Y';
             Return *on;
         //*** When (strMod = 'Managed Care') and SYfbif = 'Y';
          When (strMod = 'Managed Care') and (SYfbif = 'Y' or SYfl02 = 'Y');
             Return *on;
          When (strMod = 'WCMCInterface') and SYFIIF = 'Y';
             Return *on;
          When (strMod = 'Use Image Extension') and %Subst(SYFL12:9:1) = 'Y';
             Return *on;
          When (strMod = 'PEO Module') and WCfl30 = 'Y';
             Return *on;
          When (strMod = 'S/R Online') and WCfl38 = 'Y';
             Return *on;
          When (strMod = 'Portal Module');
             If %Subst(SYfl16:1:1) = 'P';
                isPortal = *On;
             EndIf;
             Return isPortal;
          When (strMod = 'Negative Fee Checks') and %Subst(WCsc06:1:1) = 'Y';
             Return *On;
          When (strMod = 'IDC') and %subst(WCsc07:1:1) = 'Y';
             Return *on;
          When (strMod = 'Third Party Payment') and %subst(SYFL16:2:1) = 'Y';
             Return *on;
          When (strMod = 'E-Pay') and %subst(WCSC07:2:1) = 'Y'              ;
             Return *on;
          When strMod = 'EFT'
           And (WCFL17 = 'Y' Or WCFL24 = 'Y' Or WCFL35 = 'Y');
             Return *On;
          When strMod = 'Agent EFT' And WCFL17 = 'Y';
             Return *On;
          When strMod = 'Premium EFT' And WCFL24 = 'Y';
             Return *On;
          When strMod = 'Claims EFT' And WCFL35 = 'Y';
             Return *On;
          When strMod = 'Invoice Cloud' and %subst(WCfl54:1:1) = 'Y';
             Return *on;
       //*** Beg Add ** 02/23/23 *** WC2430 ***************************************
          When strMod = 'New Payments' and %subst(SYfl16:3:1) = ' ';
             Return *on;
       //*** End Add ** 02/23/23 *** WC2430 ***************************************
       //*** Beg Add ** 06/10/24 **************************************************
          When strMod = 'Multi Group Restrict' and SYfl06 = 'Y';
             Return *on;
       //*** End Add ** 06/10/24 **************************************************
          Endsl;
       Endif;
       Setll *start sfbpf;
       Read sfbpf;
       If not %eof(sfbpf);
          Select;
          When (strMod = 'Multiple Units') and %subst(FBst07:2:1) = 'N';
             Return *on;
          When (strMod = 'Line Validation') and %Subst(FBst10:2:1) = 'Y';
             Return *on;
          When (strMod = 'Claims Export') and %Subst(FBal02:1:1) = 'Y';
             Return *on;
          When (strMod = 'Employer Export') and %Subst(FBal02:2:1) = 'Y';
             Return *on;
          When (strMod = 'Provider Export') and %Subst(FBal02:3:1) = 'Y';
             Return *on;
          When (strMod = 'Paid Date Export') and %Subst(FBal02:4:1) = 'Y';
             Return *on;
          When (strMod = 'Bill Upload') and %Subst(FBal02:5:1) = 'Y';
             Return *on;
          When (strMod = '837 ASC X12 Prof') and
                %subst(Fbal02:6:1) = 'Y';
             Return *on;
          When (strMod = '837 ASC X12 Inst') and
                %subst(Fbal02:7:1) = 'Y';
             Return *on;
          When (strMod = '835 ASC X12 Pay Adv') and
                %subst(Fbal02:8:1) = 'Y';
             Return *on;
          When (strMod = 'Pbills Lite') and
                %subst(FBst02:1:1) = 'Y';
             Return *on;
          When (strMod = 'Work Comp Interface') and
                %subst(Fbfl01:1:1) = 'Y';
             Return *on;
       //*** Beg Add *** 08/18/22 *******************************//
          When (strMod = 'Modernized EOB Print') and
                %Subst(FBal02:9:1) = 'Y';
             Return *on;
       //*** End Add *** 08/18/22 *******************************//
       //*** Beg Add *** 06/10/24 *******************************//
          When (strMod = 'Managed Care AR') and
                (%Subst(FBal04:5:1) = 'Y');
             Return *on;
          When (strMod = 'Managed Care Network') and
                        FBfl06 = 'Y';
             Return *on;
          When (strMod = 'Claim Check Install') and
                (%Subst(FBst01:1:1) = 'Y');
             Return *on;
          When (strMod = 'Med Case Management') and
                (%Subst(FBal01:9:1) = 'Y');
             Return *on;
       //*** End Add *** 06/10/24 *******************************//
       //*** Beg Add *** 07/18/25 ******************************************
          When (strMod = 'NCCI Med Data Call') and
                (%Subst(FBst07:1:1) = 'Y');
             Return *on;
       //*** End Add *** 07/18/25 ******************************************

          Endsl;
       Endif;

       Return *off;

      /End-Free
     p IsModEnabled    e

       //===================================================================
       // SetModuleProperty
       //===================================================================
     pSetModuleProperty...
     p                 b                   export
     dSetModuleProperty...
     d                 pi             1n
     d inModule                      20    const varying
     d inKey                         25    const varying
     d inProperty                   256    const varying
      *
     dstrProperty      s            256    varying inz
      /Free
       Exec sql
        select MIproperty into :strProperty
         from STMIP
         where MImodule = :inModule and MIkey = :inKey;
       If IsValidFetch(sqlstt);
          Exec sql
           update STMIP
            set MIproperty = :inProperty
            where MImodule = :inModule and MIkey = :inKey;
       Else;
          Exec sql
           insert into STMIP
            values (:inModule, :inKey, :inProperty);
       Endif;
       Return IsValidFetch(sqlstt);
      /End-free
     pSetModuleProperty...
     p                 e

       //*** Beg Add ***** 08/21/23 **********************************
      *=====================================================================
      * ProceedToCopy - Copy Group setup
      *  Parm1 - RRN  (9A)   Group master (WMFNP)
      *  Parm2 - Data (500a) "Copy Configuration" or "Copy Endorsements"
      *=====================================================================
     p ProceedToCopy   b                   export
     d ProceedToCopy   pi
     d inFndRRN                       9  0 const
     d inData                       500a   const
     d inType                       100a   options(*OMIT)
     d inEfdt                         8  0 const
     d inRRN                          9  0 const
     d inCo#                          3  0 options(*OMIT)
     d inState                        2a   options(*OMIT)
     d inFlag                         1a   options(*OMIT)
     d inSType                        1a   options(*OMIT)
     d inEndFrm                      15a   options(*OMIT)
     d inLeft                         5  0 options(*OMIT)
     d inTop                          5  0 options(*OMIT)
     d ExitNow1        s               n   inz
     d isDupStRt       s               n   inz
     d isDupPrDs       s               n   inz
     d scToGroup#      s              3  0 inz
      /free

       FileOpen();

       Clear RcdCopyCfg;
       visToGrp = *Off;
       scCpyPnlLt = inLeft;
       scCpyPnlTp = inTop;
       scCpyPnlHt = 315;
       scPnlWdth = 670;
       Exsr ClrSflToGrp;
       Exsr LoadSflToGrp;

       If inFndRRN <> 0;
          Chain(n) inFndRRN pfn0p;
       Endif;

       Exsr Duplicates;

       FileClose();

       //****************************************************************************
       // ClrSflDuplicate - Clear Duplicate subfile
       //****************************************************************************
       Begsr ClrSflDuplicate;
          Clear sflDupl;
          RrnDupl = 0;
          dspDupl = *Off;
          clrDupl = *On;
          Write rcdCopyCfg;
          clrDupl = *Off;
          dspDupl = *On;
       Endsr;
       //****************************************************************************
       // ClrSflToGrp - Clear To Group Subfile
       //****************************************************************************
       Begsr ClrSflToGrp;
          RrnToGrp = *Zeros;
          dspToGrpSf = *Off;
          dspToGrpCt = *Off;
          clrToGrp = *On;
          Write rcdCopyCfg;
          dspToGrpSf = *On;
          dspToGrpCt = *On;
          clrToGrp = *Off;
       Endsr;
       //****************************************************************************
       // Duplicates - Getting Duplicates and copy to selected groups               *
       //****************************************************************************
       Begsr Duplicates;
        scCpCfgHdg = inData;
        If inFndRRN = 0;
           scFrGroup# = 0;
           scFrGrpAll = 'All Groups';
           visFrGrp = *off;
        Else;
           scFrGroup# = FNfnd;
           visFrGrp = *on;
        Endif;
        visDupl = *Off;
        visNDupl = *Off;
        scCpyPnlHt = 315;
        scDuplRs = ' ';
        BtnCopy  = *Off;

        Dou ExitNow1 = *On;
           Exfmt rcdCopyCfg;
           Select;
           When btnVerify = *On;
              scDuplRS = ' ';
              disCpyDupl = *on;
              Exsr ClrSflDuplicate;

              clear xxSelCount;
              ReadC sflToGrp;
              Dow Not %Eof;
                 If scSelToGrp = 'Y';
                    xxSelCount += 1;
                    scToGroup# = scToGrp;
                    nxChgToGrp = *On;
                    update sflToGrp;

                    Select;
                    When inData = 'Copy Logo Image';
                       Exsr DuplicateLogo;

                    When inData = 'Copy Signature Image';
                       Exsr DuplicateSignature;

                    When inData = 'Copy Letter Template';
                       Exsr DuplicateLetter;

                    When inData = 'Copy Cancellation/Reinstatement';
                       Exsr DuplicateCanReins;

                    When inData = 'Copy Certificate of Insurance Forms';
                       Exsr DuplicateCertificate;

                    When inData = 'Copy FROI Forms';
                       Exsr DuplicateFROI;

                    When inFlag = 'S' or inFlag = 'N';
                       Exsr DuplicatePolicy;

                    When inFlag = 'E';
                       Exsr DuplicateEndorsement;
                    Endsl;

                 Endif;
                 ReadC sflToGrp;
              Enddo;

              If rrnDupl > 0;
                 visDupl = *On;
                 visNDupl = *Off;
                 TopDupl = inTop + 296;
                 scCpyPnlHt = 521;
                 dspDupl = *On;
                 dspDuplCtl = *On;
              Endif;
              If rrnDupl = *Zero and BtnCancel = *Off; //and scError1 = *blank;
                 visNDupl = *On;
                 leftBtnCpy = 535;
                 visDupl = *Off;
                 TopNDupl = inTop + 300;
                 scCpyPnlHt = 347;
              Endif;
              rrnDupl = 1;
              If rrnPsToGrp <> 0;
                 rrnToGrp = rrnPsToGrp;
              Else;
                 rrnToGrp = 1;
              Endif;
              btnVerify = *Off;

           //Duplicates found in To Group: Skip Duplicate in To Group
           When BtnCopy = *On and scDuplRS = 'S';
              CopyToGroup(scFrGroup#:'S':inData:inRRN:inType:inEfdt:inCo#:
                            inState);
              ExitNow1 = *On;
           //Duplicates found in To Group: Replace Duplicate in To Group
           When BtnCopy = *On and scDuplRS = 'R';
              CopyToGroup(scFrGroup#:'R':inData:inRRN:inType:inEfdt:inCo#:
                            inState);
              ExitNow1 = *On;
           //No Duplicates found in To Group: Copying to all selected To Group
           When BtnCopy = *On;
              CopyToGroup(scFrGroup#:'A':inData:inRRN:inType:inEfdt:inCo#:
                            inState);
              ExitNow1 = *On;
           When BtnCancel = *On;
              ExitNow1 = *On;
           Endsl;
        Enddo;

       Endsr;
       //****************************************************************************
       // LoadSflToGrp - Load To Group Subfile
       //****************************************************************************
       Begsr LoadSflToGrp;
        Chain(n) inFndRRN pfn0p;
        scFrGroup# = FNfnd;
        Open WMFNL;
        Setll *loval Wmfnl;
        Read Wmfnl;
        Dow not %Eof(Wmfnl);
           If FNfnd <> scFrGroup#;
              Chain(n) (FNco#:FNfnd) Wdf2l;
              scSelToGrp = 'N';
              scToGrp = FNfnd;
              scToGrpDes = %Editc(FNfnd:'X') + ' - ' + FNname;
              If %Found(Wdf2l);
                 scCar# = F2Car#;
              Else;
                 scCar# = *Zeros;
              Endif;
              //Peo Type
              Select;
              When FNpeof = *Blank;
                 scPeoType = 'Standard';
              When FNpeof = 'S';
                 scPeoType = 'Master PEO';
              When FNpeof = 'M';
                 scPeoType = 'MCP PEO';
              When FNpeof = 'C';
                 scPeoType = 'Combined PEO';
              Endsl;
              RrnToGrp = RrnToGrp + 1;
              nxChgToGrp = *Off;
              Write SflToGrp;
           Endif;
           Read Wmfnl;
        Enddo;
        Close WMFNL;

        If RrnToGrp = *Zeros;
           DspToGrpSf = *Off;
           VisToGrp = *Off;
        Else;
           RrnToGrp = 1;
           VisToGrp = *On;
           scCpyLeft = inLeft + 10;
           scCpyLeft2 = scCpyLeft + 105;
           scCpyLeft3 = scCpyLeft2 + 145;
           scCpyLeft4 = scCpyLeft3 + 135;
           scCpyLeft5 = scCpyLeft4 + 135;
           scCpyLeft6 = scCpyLeft + 70;
           scCpyTp1 = inTop + 30;
           scCpyTp2 = scCpyTp1 + 15;
           scCpyTp3 = scCpyTp2 + 35;
           scCpyTp4 = scCpyTp2 + 30;
           TopToGrp = inTop + 80;
        Endif;
       Endsr;
       //****************************************************************************
       // DuplicateLogo - Load To Logo Duplicate Subfile
       //****************************************************************************
       Begsr DuplicateLogo;
        chain(n) (inType:scToGroup#:dtCYMD(inEfdt)) psp12l;
        If %Found(stsp2l);
           scDuplGrp = S2Fnd;
           scDEfdt = dtMDYY(S2Efdt);
           scDuplRrn# = dbrrnC;
           scDuplTyp = S2Type;
           rrnDupl = rrnDupl + 1;
           Write sflDupl;
        Endif;
       Endsr;
       //****************************************************************************
       // DuplicateSignature - Load To Signature Duplicate Subfile
       //****************************************************************************
       Begsr DuplicateSignature;
        chain(n) (inType:scToGroup#:dtCYMD(inEfdt)) psp13l;
        If %Found(stsp3l);
           scDuplGrp = S3Fnd;
           scDEfdt = dtMDYY(S3Efdt);
           scDuplRrn# = dbrrnD;
           scDuplTyp = S3Type;
           rrnDupl = rrnDupl + 1;
           Write sflDupl;
        Endif;
       Endsr;
       //****************************************************************************
       // DuplicateLetter - Load To Letter Duplicate Subfile
       //****************************************************************************
       Begsr DuplicateLetter;
        chain(n) (inType:scToGroup#:dtCYMD(inEfdt)) psp14l;
        If %Found(stsp4l);
           scDuplGrp = S4Fnd;
           scDEfdt = dtMDYY(S4Efdt);
           scDuplRrn# = dbrrnE;
           scDuplTyp = S4Type;
           rrnDupl = rrnDupl + 1;
           Write sflDupl;
        Endif;
       Endsr;
       //****************************************************************************
       // DuplicateCanReins - Load To Cancel/Reinstate Duplicate Subfile
       //****************************************************************************
       Begsr DuplicateCanReins;
        chain(n) (inCo#:scToGroup#:inState:dtCYMD(inEfdt)) pcr00;
        If %Found(WSCRL);
           scDuplGrp = CRFnd;
           scDEfdt = dtMDYY(CREfdt);
           scDuplRrn# = dbrrn;
           scDuplTyp = inType;
           rrnDupl = rrnDupl + 1;
           Write sflDupl;
        Endif;
       Endsr;
       //****************************************************************************
       // DuplicateCertificate - Load To Certificate Duplicate Subfile
       //****************************************************************************
       Begsr DuplicateCertificate;
        chain(n) (inCo#:scToGroup#:inState:dtCYMD(inEfdt)) pci00;
        If %Found(WSCIL);
           scDuplGrp = CIFnd;
           scDEfdt = dtMDYY(CIEfdt);
           scDuplRrn# = dbrrn1;
           scDuplTyp = inType;
           rrnDupl = rrnDupl + 1;
           Write sflDupl;
        Endif;
       Endsr;
       //****************************************************************************
       // DuplicateFROI - Load To FROI Duplicate Subfile
       //****************************************************************************
       Begsr DuplicateFROI;
        chain(n) (scToGroup#:inState:dtCYMD(inEfdt)) pff00;
        If %Found(WSFFL);
           scDuplGrp = FFFnd;
           scDEfdt = dtMDYY(FFEfdt);
           scDuplRrn# = dbrrn2;
           scDuplTyp = inType;
           rrnDupl = rrnDupl + 1;
           Write sflDupl;
        Endif;
       Endsr;
       //****************************************************************************
       // DuplicatePolicy - Load To Policy Page/Template Duplicate Subfile
       //****************************************************************************
       Begsr DuplicatePolicy;
        chain(n) (inFlag:inSType:scToGroup#:inState:dtCYMD(inEfdt)) ppp00;
        If %Found(WSPPL);
           scDuplGrp = PPFnd;
           scDEfdt = dtMDYY(PPEfdt);
           scDuplRrn# = dbrrn3;
           scDuplTyp = inType;
           rrnDupl = rrnDupl + 1;
           Write sflDupl;
        Endif;
       Endsr;
       //****************************************************************************
       // DuplicateEndorsement - Load To Policy Endorsement Duplicate Subfile
       //****************************************************************************
       Begsr DuplicateEndorsement;
        chain(n) (inFlag:scToGroup#:inEndFrm:inState:dtCYMD(inEfdt)) ppp01;
        If %Found(WSPPL1);
           scDuplGrp = PPFnd;
           scDEfdt = dtMDYY(PPEfdt);
           scDuplRrn# = dbrrn4;
           scDuplTyp = inType;
           rrnDupl = rrnDupl + 1;
           Write sflDupl;
        Endif;
       Endsr;
     p ProceedToCopy   e

       //===================================================================
       // CopyToGroup - Copying selected to To Group
       //===================================================================
     p CopyToGroup     b
     d CopyToGroup     pi
     d inFromGroup                    3  0
     d inSkipRepl                     1    const
     d inData                       500a   const
     d inRRN                          9  0 const
     d inType                       100a   const
     d inEfdt                         8  0 const
     d inCo#                          3  0 const
     d inState                        2a   const

     d inToGroup       s              3  0
     d xxGrpCount      s              3  0 inz
     d xxefdt          s              7  0 inz
      /Free

       Exec Sql
       Select count(*) into :xxGrpCount
        from Wmfnl;

       If xxGrpCount-1 <> xxSelCount or inSkipRepl = 'S';
          ReadC sflToGrp;
          Dow Not %Eof;
             If scSelToGrp = 'Y';
                inToGroup = scToGrp;
                nxChgToGrp = *On;
                update sflToGrp;
                Select;
                // ********** For Logo Image Copy ******************
                //Replace when duplicates found in To Group
                When inSkipRepl = 'R' and inData = 'Copy Logo Image';
                   //Check if Logo exist in To Group
                   chain (inType:scToGrp:dtCYMD(inEfdt)) psp12l;
                   If %Found(stsp2l);
                      //Delete the existing Logos and write new Logo in To Group
                      Delete psp12l;
                      Exsr WriteSTsp2p;
                   Else;
                      Exsr WriteSTsp2p;
                   Endif;
                //Skip duplicate logos When duplicate found in To Group
                When inSkipRepl = 'S' and inData = 'Copy Logo Image';
                   //Check if Logo exist in To Group
                   chain(n) (inType:scToGrp:dtCYMD(inEfdt)) psp12l;
                   If not %Found(stsp2l);
                      //Write to To Group if Logo does not exist in To Group
                      Exsr WriteSTsp2p;
                   Endif;
                //No dupllicate Logos found in To Group, Copy all the Logos
                When inSkipRepl = 'A'and inData = 'Copy Logo Image';
                   Exsr WriteSTsp2p;

                // ********** For Signature Image Copy ******************

                //Replace when duplicates found in To Group
                When inSkipRepl = 'R' and inData = 'Copy Signature Image';
                   //Check if Signature exist in To Group
                   chain (inType:scToGrp:dtCYMD(inEfdt)) psp13l;
                   If %Found(stsp3l);
                      //Delete the existing Signature and write new in To Group
                      Delete psp13l;
                      Exsr WriteSTsp3p;
                   Else;
                      Exsr WriteSTsp3p;
                   Endif;
                //Skip duplicate Signature When duplicate found in To Group
                When inSkipRepl = 'S' and inData = 'Copy Signature Image';
                   //Check if Signature exist in To Group
                   chain(n) (inType:scToGrp:dtCYMD(inEfdt)) psp13l;
                   If not %Found(stsp3l);
                      //Write to To Group if Signature does not exist in To Group
                      Exsr WriteSTsp3p;
                   Endif;
                //No dupllicate Signature found in To Group, Copy all the Signatures
                When inSkipRepl = 'A'and inData = 'Copy Signature Image';
                   Exsr WriteSTsp3p;

                // ********** For Letter Template Copy ******************

                //Replace when duplicates found in To Group
                When inSkipRepl = 'R' and inData = 'Copy Letter Template';
                   //Check if Letter exist in To Group
                   chain (inType:scToGrp:dtCYMD(inEfdt)) psp14l;
                   If %Found(stsp4l);
                      //Delete the existing Letter and write new in To Group
                      Delete psp14l;
                      Exsr WriteSTsp4p;
                   Else;
                      Exsr WriteSTsp4p;
                   Endif;
                //Skip duplicate Letter When duplicate found in To Group
                When inSkipRepl = 'S' and inData = 'Copy Letter Template';
                   //Check if Letter exist in To Group
                   chain(n) (inType:scToGrp:dtCYMD(inEfdt)) psp14l;
                   If not %Found(stsp4l);
                      //Write to To Group if Letter does not exist in To Group
                      Exsr WriteSTsp4p;
                   Endif;
                //No dupllicate Letter found in To Group, Copy all the Letter
                When inSkipRepl = 'A'and inData = 'Copy Letter Template';
                   Exsr WriteSTsp4p;

                // ********** For Cancel/Reinstate Copy ******************

                //Replace when duplicates found in To Group
                When inSkipRepl = 'R' and inData = 'Copy Cancellation/Rein'+
                                                     'statement';
                   //Check if Cancel/Reinstate form exist in To Group
                   chain (inCo#:scToGrp:inState:dtCYMD(inEfdt)) pcr00;
                   If %Found(wscrl);
                      //Delete the existing form and write new in To Group
                      Delete pcr00;
                      Exsr WriteWScrp;
                   Else;
                      Exsr WriteWScrp;
                   Endif;
                //Skip duplicate  When duplicate found in To Group
                When inSkipRepl = 'S' and inData = 'Copy Cancellation/Rein'+
                                                     'statement';
                   //Check if Cancel/Reinstate exist in To Group
                   chain(n) (inCo#:scToGrp:inState:dtCYMD(inEfdt)) pcr00;
                   If not %Found(wscrl);
                      //Write to To Group if form does not exist in To Group
                      Exsr WriteWScrp;
                   Endif;
                //No dupllicate Form found in To Group, Copy all the Letter
                When inSkipRepl = 'A'and inData = 'Copy Cancellation/Rein'+
                                                    'statement';
                   Exsr WriteWScrp;

                // ********** For Certificate Copy ******************

                //Replace when duplicates found in To Group
                When inSkipRepl = 'R' and inData = 'Copy Certificate of '+
                                                     'Insurance Forms';
                   //Check if Certificate form exist in To Group
                   chain (inCo#:scToGrp:inState:dtCYMD(inEfdt)) pci00;
                   If %Found(wscil);
                      //Delete the existing form and write new in To Group
                      Delete pci00;
                      Exsr WriteWScip;
                   Else;
                      Exsr WriteWScip;
                   Endif;
                //Skip duplicate  When duplicate found in To Group
                When inSkipRepl = 'S' and inData = 'Copy Certificate of '+
                                                     'Insurance Forms';
                   //Check if Cancel/Reinstate exist in To Group
                   chain(n) (inCo#:scToGrp:inState:dtCYMD(inEfdt)) pci00;
                   If not %Found(wscil);
                      //Write to To Group if form does not exist in To Group
                      Exsr WriteWScip;
                   Endif;
                //No dupllicate Form found in To Group, Copy all the Letter
                When inSkipRepl = 'A'and inData = 'Copy Certificate of '+
                                                    'Insurance Forms';
                   Exsr WriteWScip;

                // ********** For FROI Copy ******************

                //Replace when duplicates found in To Group
                When inSkipRepl = 'R' and inData = 'Copy FROI Forms';
                   //Check if Certificate form exist in To Group
                   chain (scToGrp:inState:dtCYMD(inEfdt)) pff00;
                   If %Found(wsffl);
                      //Delete the existing form and write new in To Group
                      Delete pff00;
                      Exsr WriteWSffp;
                   Else;
                      Exsr WriteWSffp;
                   Endif;
                //Skip duplicate  When duplicate found in To Group
                When inSkipRepl = 'S' and inData = 'Copy FROI Forms';
                   //Check if Cancel/Reinstate exist in To Group
                   chain(n) (scToGrp:inState:dtCYMD(inEfdt)) pff00;
                   If not %Found(wsffl);
                      //Write to To Group if form does not exist in To Group
                      Exsr WriteWSffp;
                   Endif;
                //No dupllicate Form found in To Group, Copy all the Letter
                When inSkipRepl = 'A'and inData = 'Copy FROI Forms';
                   Exsr WriteWSffp;

                // ********** For Policy Page Copy ******************

                //Replace when duplicates found in To Group
                When inSkipRepl = 'R' and (inData = 'Copy Policy Page Forms' or
                                inData = 'Copy Policy Form Templates');
                   //Check if Certificate form exist in To Group
                   Chain(n) inRrn ppp0p;
                   chain (PPtype:PPSType:scToGrp:inState:dtCYMD(inEfdt)) ppp00;
                   If %Found(wsppl);
                      //Delete the existing form and write new in To Group
                      Delete ppp00;
                      Exsr WriteWSppp;
                   Else;
                      Exsr WriteWSppp;
                   Endif;
                //Skip duplicate  When duplicate found in To Group
                When inSkipRepl = 'S' and (inData = 'Copy Policy Page Forms' or
                                inData = 'Copy Policy Form Templates');
                   //Check if Cancel/Reinstate exist in To Group
                   Chain(n) inRrn ppp0p;
                   chain(n) (PPtype:PPstype:scToGrp:inState:dtCYMD(inEfdt))
                                        ppp00;

                   If not %Found(wsppl);
                      //Write to To Group if form does not exist in To Group
                      Exsr WriteWSppp;
                   Endif;

                //Replace when duplicates found in To Group
                When inSkipRepl = 'R' and inData ='Copy Endorsement Signatures';
                   //Check if Certificate form exist in To Group
                   Chain(n) inRrn ppp0p;
                   chain (PPtype:scToGrp:PPendfrm:inState:dtCYMD(inEfdt)) ppp01;
                   If %Found(wsppl1);
                      //Delete the existing form and write new in To Group
                      Delete ppp01;
                      Exsr WriteWSppp;
                   Else;
                      Exsr WriteWSppp;
                   Endif;
                //Skip duplicate  When duplicate found in To Group
                When inSkipRepl = 'S' and inData ='Copy Endorsement Signatures';
                   //Check if Cancel/Reinstate exist in To Group
                   Chain(n) inRrn ppp0p;
                   chain(n) (PPtype:scToGrp:PPendfrm:inState:dtCYMD(inEfdt))
                                        ppp01;
                   If not %Found(wsppl1);
                      //Write to To Group if form does not exist in To Group
                      Exsr WriteWSppp;
                   Endif;

                //No dupllicate Form found in To Group, Copy all the Letter
                When inSkipRepl = 'A' and (inData = 'Copy Policy Page Forms' or
                                       inData = 'Copy Policy Form Templates' or
                                       inData = 'Copy Endorsement Signatures');
                   Exsr WriteWSppp;
                Endsl;
             Endif;
             ReadC sflToGrp;
          Enddo;
       Else;
          xxefdt = dtCYMD(inEfdt);

          Select;
          When (inSkipRepl = 'R' or inSkipRepl = 'A')
           and inData = 'Copy Logo Image';
             CopyToAllGroup('STsp2p':inRrn:inType:xxefdt:*omit:*omit);

          When (inSkipRepl = 'R' or inSkipRepl = 'A')
           and inData = 'Copy Signature Image';
             CopyToAllGroup('STsp3p':inRrn:inType:xxefdt:*omit:*omit);

          When (inSkipRepl = 'R' or inSkipRepl = 'A')
           and inData = 'Copy Letter Template';
             CopyToAllGroup('STsp4p':inRrn:inType:xxefdt:*omit:*omit);

          When (inSkipRepl = 'R' or inSkipRepl = 'A')
           and inData = 'Copy Cancellation/Reinstatement';
             CopyToAllGroup('WScrp':inRrn:inType:xxefdt:inState:*omit);

          When (inSkipRepl = 'R' or inSkipRepl = 'A')
           and inData = 'Copy Certificate of Insurance Forms';
             CopyToAllGroup('WScip':inRrn:inType:xxefdt:inState:*omit);

          When (inSkipRepl = 'R' or inSkipRepl = 'A')
           and inData = 'Copy FROI Forms';
             CopyToAllGroup('WSffp':inRrn:inType:xxefdt:inState:*omit);

          When (inSkipRepl = 'R' or inSkipRepl = 'A')
               and (inData = 'Copy Policy Page Forms' or
                inData = 'Copy Policy Form Templates' or
                inData = 'Copy Endorsement Signatures');
             Chain(n) inRrn ppp0p;
             CopyToAllGroup('WSppp':inRrn:inType:xxefdt:inState:PPtype);

          Endsl;
       Endif;

       //****************************************************************************
       // WriteSTsp2p - Writing to STsp2p
       //****************************************************************************
       Begsr WriteSTsp2p;
         //Write to To Group if Logo does not exist in To Group
         Chain(n) inRrn psp12p;
         If %Found(stsp2p);
            S2efdt = dtCYMD(inEfdt);
            S2fnd = inToGroup;
            S2pgm  = Q1pgm;
            S2user = Q1user;
            S2time = Timeto6(SystemTime());
            S2dt   = Dateto7(SystemDate());
            S2iusr = Q1user;
            S2idt  = Dateto7(SystemDate());
            Write psp12p;
         Endif;
       Endsr;

       //****************************************************************************
       // WriteSTsp3p - Writing to STsp3p
       //****************************************************************************
       Begsr WriteSTsp3p;
         //Write to To Group if Signature does not exist in To Group
         Chain(n) inRrn psp13p;
         If %Found(stsp3p);
            S3efdt = dtCYMD(inEfdt);
            S3fnd = inToGroup;
            S3pgm  = Q1pgm;
            S3user = Q1user;
            S3time = Timeto6(SystemTime());
            S3dt   = Dateto7(SystemDate());
            S3iusr = Q1user;
            S3idt  = Dateto7(SystemDate());
            Write psp13p;
         Endif;
       Endsr;

       //****************************************************************************
       // WriteSTsp4p - Writing to STsp4p
       //****************************************************************************
       Begsr WriteSTsp4p;
         //Write to To Group if Letter does not exist in To Group
         Chain(n) inRrn psp14p;
         If %Found(stsp4p);
            S4efdt = dtCYMD(inEfdt);
            S4fnd = inToGroup;
            S4pgm  = Q1pgm;
            S4user = Q1user;
            S4time = Timeto6(SystemTime());
            S4dt   = Dateto7(SystemDate());
            S4iusr = Q1user;
            S4idt  = Dateto7(SystemDate());
            Write psp14p;
         Endif;
       Endsr;

       //****************************************************************************
       // WriteWScrp - Writing to WScrp
       //****************************************************************************
       Begsr WriteWScrp;
         //Write to To Group if form does not exist in To Group
         Chain(n) inRrn pcr0p;
         If %Found(wscrp);
            CRefdt = dtCYMD(inEfdt);
            CRfnd = inToGroup;
            CRpgm  = Q1pgm;
            CRuser = Q1user;
            CRtime = Timeto6(SystemTime());
            CRdt   = Dateto7(SystemDate());
            CRiusr = Q1user;
            CRidt  = Dateto7(SystemDate());
            Write pcr0p;
         Endif;
       Endsr;

       //****************************************************************************
       // WriteWScip - Writing to WScip
       //****************************************************************************
       Begsr WriteWScip;
         //Write to To Group if form does not exist in To Group
         Chain(n) inRrn pci0p;
         If %Found(wscip);
            CIefdt = dtCYMD(inEfdt);
            CIfnd = inToGroup;
            CIpgm  = Q1pgm;
            CIuser = Q1user;
            CItime = Timeto6(SystemTime());
            CIdt   = Dateto7(SystemDate());
            CIiusr = Q1user;
            CIidt  = Dateto7(SystemDate());
            Write pci0p;
         Endif;
       Endsr;

       //****************************************************************************
       // WriteWSffp - Writing to WSffp
       //****************************************************************************
       Begsr WriteWSffp;
         //Write to To Group if form does not exist in To Group
         Chain(n) inRrn pff0p;
         If %Found(wsffp);
            FFefdt = dtCYMD(inEfdt);
            FFfnd = inToGroup;
            FFpgm  = Q1pgm;
            FFuser = Q1user;
            FFtime = Timeto6(SystemTime());
            FFdt   = Dateto7(SystemDate());
            FFiusr = Q1user;
            FFidt  = Dateto7(SystemDate());
            Write pff0p;
         Endif;
       Endsr;

       //****************************************************************************
       // WriteWSppp - Writing to WSppp
       //****************************************************************************
       Begsr WriteWSppp;
         //Write to To Group if form does not exist in To Group
         Chain(n) inRrn ppp0p;
         If %Found(wsppp);
            PPefdt = dtCYMD(inEfdt);
            PPfnd = inToGroup;
            PPpgm  = Q1pgm;
            PPuser = Q1user;
            PPtime = Timeto6(SystemTime());
            PPdt   = Dateto7(SystemDate());
            PPiusr = Q1user;
            PPidt  = Dateto7(SystemDate());
            Write ppp0p;
         Endif;
       Endsr;

      /End-Free
     p CopyToGroup     e
      *

       //===================================================================
       // CopyToAllGroup - Copying for all Groups (i.e FND = 0)
       //===================================================================
     p CopyToAllGroup  b
     d CopyToAllGroup  pi
     d fileName                      10a   const
     d inRRN                          9  0 const
     d inType                       100a   const
     d xxEfdt                         7  0 const
     d inState                        2a   const options(*OMIT)
     d inFlag                         1a   const options(*OMIT)
     d strSql          s           4000a   inz
     d tmpFile         s             10a   inz

        tmpFile =  %trimr(fileName) + 'Tmp';

        CreateFile(fileName:tmpFile);

        strSql = 'Insert into ' + %trim(tmpFile) +
                  ' select * from ' + %trim(fileName) + ' as r ' +
                  '  where rrn(r) = '+ %editc(inRrn:'X');
        exec sql execute immediate:strSql;

        Select;
        When fileName = 'STsp2p';
         strSql = 'Delete from '+ %trim(fileName) +
                   ' where S2type = ' + '''' + %trim(inType) + '''' +
                   '  and S2efdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName = 'STsp3p';
         strSql = 'Delete from '+ %trim(fileName) +
                   ' where S3type = ' + '''' + %trim(inType) + '''' +
                   '  and S3efdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName = 'STsp4p';
         strSql = 'Delete from '+ %trim(fileName) +
                   ' where S4type = ' + '''' + %trim(inType) + '''' +
                   '  and S4efdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName = 'WScrp';
         strSql = 'Delete from '+ %trim(fileName) +
                   ' where CRst = ' + '''' + %trim(inState) + '''' +
                   '  and CRefdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName = 'WScip';
         strSql = 'Delete from '+ %trim(fileName) +
                   ' where CIst = ' + '''' + %trim(inState) + '''' +
                   '  and CIefdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName = 'WSffp';
         strSql = 'Delete from '+ %trim(fileName) +
                   ' where FFst = ' + '''' + %trim(inState) + '''' +
                   '  and FFefdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName = 'WSppp';
         strSql = 'Delete from '+ %trim(fileName) +
                   ' where PPst = ' + '''' + %trim(inState) + '''' +
                   '  and PPefdt = ' + %editc(xxEfdt:'X') +
                   '  and PPtype = ' + '''' + %trim(inFlag)+ '''';
         exec sql execute immediate:strSql;

        Endsl;

        strSql = 'Insert into '+ %trim(fileName) +
                  ' select * from '+ %trim(tmpFile);
        exec sql execute immediate:strSql;

        Select;
        When fileName ='STsp2p';
         strSql = 'Update '+ %trim(fileName) + ' set S2fnd = 0 ' +
                   ' where S2type = ' + '''' + %trim(inType) + '''' +
                   '  and S2efdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName ='STsp3p';
         strSql = 'Update '+ %trim(fileName) + ' set S3fnd = 0 ' +
                   ' where S3type = ' + '''' + %trim(inType) + '''' +
                   '  and S3efdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName ='STsp4p';
         strSql = 'Update '+ %trim(fileName) + ' set S4fnd = 0 ' +
                   ' where S4type = ' + '''' + %trim(inType) + '''' +
                   '  and S4efdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName ='WScrp';
         strSql = 'Update '+ %trim(fileName) + ' set CRfnd = 0 ' +
                   ' where CRst = ' + '''' + %trim(inState) + '''' +
                   '  and CRefdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName ='WScip';
         strSql = 'Update '+ %trim(fileName) + ' set CIfnd = 0 ' +
                   ' where CIst = ' + '''' + %trim(inState) + '''' +
                   '  and CIefdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName ='WSffp';
         strSql = 'Update '+ %trim(fileName) + ' set FFfnd = 0 ' +
                   ' where FFst = ' + '''' + %trim(inState) + '''' +
                   '  and FFefdt = ' + %editc(xxEfdt:'X');
         exec sql execute immediate:strSql;

        When fileName ='WSppp';
         strSql = 'Update '+ %trim(fileName) + ' set PPfnd = 0 ' +
                   ' where PPst = ' + '''' + %trim(inState) + '''' +
                   '  and PPefdt = ' + %editc(xxEfdt:'X') +
                   '  and PPtype = ' + '''' + %trim(inFlag)+ '''';
         exec sql execute immediate:strSql;
        Endsl;

        DeleteFile(tmpFile);

     p CopyToAllGroup  e
      *

       //===================================================================
       // CreateFile - Creating a temporary file in QTEMP
       //===================================================================
     p CreateFile      b
     d CreateFile      pi
     d fileName                      10a   const
     d tmpFile                       10a   const
     d strRunCmd       s            500a

        strRunCmd = 'CRTDUPOBJ OBJ(' + %trim(fileName) + ')';
        strRunCmd = %Trim(strRunCmd) + ' FROMLIB(*LIBL)';
        strRunCmd = %Trim(strRunCmd) + ' OBJTYPE(*FILE)';
        strRunCmd = %Trim(strRunCmd) + ' TOLIB(QTEMP)';
        strRunCmd = %Trim(strRunCmd) + ' NEWOBJ('+ %trim(tmpFile) +')';
        strRunCmd = %Trim(strRunCmd) + ' CST(*NO) TRG(*NO)';

        Monitor;
           RunCmd(strRunCmd:150);
        On-Error;
        Endmon;

     p CreateFile      e
      *

       //===================================================================
       // DeleteFile - Deleting temporary file from QTEMP
       //===================================================================
     p DeleteFile      b
     d DeleteFile      pi
     d tmpFile                       10a   const
     d strRunCmd       s            500a

        strRunCmd = 'DLTF FILE(QTEMP/';
        strRunCmd = %Trim(strRunCmd) + %trim(tmpFile) + ')';

        Monitor;
           RunCmd(strRunCmd:90);
        On-Error;
        Endmon;

     p DeleteFile      e

       //===================================================================
       // FileOpen - Opening files
       //===================================================================
     p FileOpen        b
     d FileOpen        pi

       If not %Open(stappcfg);
          Open stappcfg;
       Endif;
       If not %Open(stsp1p);
          Open stsp1p;
       Endif;
       If not %Open(stsp2l);
          Open stsp2l;
       Endif;
       If not %Open(stsp2p);
          Open stsp2p;
       Endif;
       If not %Open(stsp3l);
          Open stsp3l;
       Endif;
       If not %Open(stsp3p);
          Open stsp3p;
       Endif;
       If not %Open(stsp4l);
          Open stsp4l;
       Endif;
       If not %Open(stsp4p);
          Open stsp4p;
       Endif;
       If not %Open(wmfnp);
          Open wmfnp;
       Endif;
       If not %Open(wdf2l);
          Open wdf2l;
       Endif;
       If not %Open(wscrl);
          Open wscrl;
       Endif;
       If not %Open(wscrp);
          Open wscrp;
       Endif;
       If not %Open(wscil);
          Open wscil;
       Endif;
       If not %Open(wscip);
          Open wscip;
       Endif;
       If not %Open(wsffl);
          Open wsffl;
       Endif;
       If not %Open(wsffp);
          Open wsffp;
       Endif;
       If not %Open(wsppl);
          Open wsppl;
       Endif;
       If not %Open(wsppl1);
          Open wsppl1;
       Endif;
       If not %Open(wsppp);
          Open wsppp;
       Endif;

     p FileOpen        e

       //===================================================================
       // FileClose - Opening files
       //===================================================================
     p FileClose       b
     d FileClose       pi

       If %Open(stappcfg);
          Close stappcfg;
       Endif;
       If %Open(stsp1p);
          Close stsp1p;
       Endif;
       If %Open(stsp2l);
          Close stsp2l;
       Endif;
       If %Open(stsp2p);
          Close stsp2p;
       Endif;
       If %Open(stsp3l);
          Close stsp3l;
       Endif;
       If %Open(stsp3p);
          Close stsp3p;
       Endif;
       If %Open(stsp4l);
          Close stsp4l;
       Endif;
       If %Open(stsp4p);
          Close stsp4p;
       Endif;
       If %Open(wmfnp);
          Close wmfnp;
       Endif;
       If %Open(wdf2l);
          Close wdf2l;
       Endif;
       If %Open(wscrl);
          Close wscrl;
       Endif;
       If %Open(wscrp);
          Close wscrp;
       Endif;
       If %Open(wscil);
          Close wscil;
       Endif;
       If %Open(wscip);
          Close wscip;
       Endif;
       If %Open(wsffl);
          Close wsffl;
       Endif;
       If %Open(wsffp);
          Close wsffp;
       Endif;
       If %Open(wsppl);
          Close wsppl;
       Endif;
       If %Open(wsppl1);
          Close wsppl1;
       Endif;
       If %Open(wsppp);
          Close wsppp;
       Endif;

     p FileClose       e
       //*** End Add ***** 08/21/23 **************************************
