     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) BNDDIR('STBNDDIR')
     H/define CONVERT
     H/TITLE S8050R
      //*************************************************************************
      //  Description:  SYSTEM VALUE MAINTENANCE PROGRAM
      //
      //  Created:            Programmer         Project:
      //*************************************************************************
      //  Revisions:
      //
      //
      // 02/23/23 C:340 Project: WC2430
      //                Add New/Legacy Payments Flag on Scr05
      //
      // 08/18/22 C:333 Project: MC443
      //                Modernized EOB Print
      //                 DSPF changes : Adding new field scMeobPrt
      //
      // 07/07/22 C:79  Project: 152WBV
      //                Add Immediate Lock of Remark field
      //
      // 05/23/22 C:33  Project: WC2663  #102357
      //                Add branding for Invoice Cloud
      //
      // 03/11/21 C:33  Project: WC2663
      //                Add branding for S/R Online
      //
      // 06/01/21 C:33  Project: SY407
      //                Add Third Party/Online Payment
      //
      // 01/11/21 C:33  Project: WC2663
      //                Add Invoice Cloud
      //
      // 07/20/20 C:79  Project: WC2749
      //                Add E-Pay installed
      //
      // 03/10/20 C:33  Project: SY420
      //                Add Masking variables
      //
      // 06/21/19 C:180 Project: WC2604
      //                Add IDC module check box/flag.
      //
      // 06/20/19 C:62  Project: 152MAV
      //                Add negative fee check flag
      //
      // 05/22/19 C:79  Support: 152/79622
      //               -Load SYFBIF
      //
      // 01/24/19 C:33  Project: SY406
      //               -Add Portal flag
      //
      // 03/08/18 C:079 Project: 114MJC
      //               -Add Use Incoming Image Extension
      //
      // 02/09/18 C:180 Project: 114MJC
      //               -Add 835 File processing.
      //
      // 10/17/17 C:180 Project: 114MJB
      //               -Add 837 File processing.
      //
      // 10/16/17 C:33  Save "Visit Tracking"
      //                Save "Claim Reserve Worksheet"
      //
      // 08/18/17 C:309 Project: 25MUJ
      //                Remove edit for visibility. Should always display   ation installed"
      //
      // 07/20/17 C:33  Project: WC2405
      //               -Add 'Rules SQL Action'
      //
      // 03/31/17 C:33  Project: WC2405
      //               -Separate Rules from Auto-Diary Triggers
      //               -Add SMS Enabled
      //
      // 03/01/17 C:33  Project: SY388
      //                Dashboard Configuration                             ation installed"
      //
      // 12/08/16 C:079 Project: MC325
      //                Added new check boxes for import/export processes   ation installed"
      //
      // 11/22/16 C:309 Project: 25MUJ
      //                Added a new check box "Bill Import Detail Line Validation installed"
      //
      // 06/10/16 C:33  Project: WC2405
      //                Add Rules Engine
      //
      // 02/08/16 C:307 PHW38717
      //                System control managed are/Fee Schedule flag changes
      //
      // 11/04/15 C:33  SY371
      //                Update SYFL11 flags
      //
      // 09/10/15 C:33  Add "Visit Tracking"
      //                Add "Claim Reserve Worksheet"
      //                Add Password notification
      //
      // 05/27/15 C:062 Proj: 114MRH
      //                Added "Include Zero dollar EOB's in EFT PDF's"
      //
      // 05/22/15 C:279 Project: 114WKA
      //                Added "Auto Diary Triggers" flag.
      //
      // 02/09/15 C:33  Rewrite to use tabs.
      //
      // 10/21/14 C:274 Proj: SY359
      //                Added "Provider Bills lite" flag.
      //
      // 08/22/12 C:280 Project: 114mer
      //                -remove scprom, scmcrv, scfreq, sccrac
      //
      // 12/13/11 C:191 Project: LA025
      //                - Add LA parish flag
      //                                                              *
      // 08/26/11 C: 53 Project: WC2029                               *
      //                -Add Cumulative TIN install flag (no code change)
      //                                                              *
      // 06/28/11 C: 53 Project: SY255                                *
      //                -Correct CMS flag
      //
      // 08/12/10 C:251 Proj: 114MWS
      //                Added "Medicare based PPO processing" flag
      //                for client 114
      //
      // 05/18/10 C:198 Proj: WC1879                                  **
      //                Updated WCFL46 to indiacte                    **
      //                CMS ICD-9 upload installed flag.              **
      //                Added standard ICD-9 upload installed         **
      //                flag after split units.                       **
      //                                                              **
      //  03/24/10 C:232 PROJ: 139WNN
      //                 COMPILED FOR THE PROJECT 139WNN.
      //                 (Renamed WCFL43, WCFL44 and WCFL45 in display file.)
      //
      //  02/16/09 C:198 PROJ: MC330
      //                 ADDED A NEW SCREEN FIELD SCNCDC
      //
      //*************************************************************************
     FS8050F    CF   E             WORKSTN HANDLER('PROFOUNDUI(HANDLER)')
     FSSYLF     UF A E           K DISK

     D                 DS
     D  XXA101                 1     10
     D  XFA101                 1      1
     D  XFA102                 2      2
     D  XFA103                 3      3
     D  XFA104                 4      4
     D  XFA105                 5      5
     D  XFA106                 6      6
     D  XFA107                 7      7
     D  XFA108                 8      8
     D  XFA109                 9      9
     D  XFA110                10     10
     D                 DS
     D  XXA301                 1     10
     D  XFA301                 1      1
     D  XFA302                 2      2
     D  XFA303                 3      3
     D  XFA304                 4      4
     D  XFA305                 5      5
     D  XFA306                 6      6
     D  XFA307                 7      7
     D  XFA308                 8      8
     D  XFA309                 9      9
     D  XFA310                10     10
     D                 DS
     D  XXA401                 1     10
     D  XFA401                 1      1
     D  XFA402                 2      2
     D  XFA403                 3      3
     D  XFA404                 4      4
     D  XFA405                 5      5
     D  XFA406                 6      6
     D  XFA407                 7      7
     D  XFA408                 8      8
     D  XFA409                 9      9
     D  XFA410                10     10
     D                 DS
     D  XXA501                 1     10
     D  XFA501                 1      1
     D  XFA502                 2      2
     D  XFA503                 3      3
     D  XFA504                 4      4
     D  XFA505                 5      5
     D  XFA506                 6      6
     D  XFA507                 7      7
     D  XFA508                 8      8
     D  XFA509                 9      9
     D  XFA510                10     10
     D                 DS
     D  dsDRal03               1     10
     D  SCDEIF                 1      1
     D  DXA302                 2      2
     D  DXA303                 3      3
     D  DXA304                 4      4
     D  DXA305                 5      5
     D  DXA306                 6      6
     D  DXA307                 7      7
     D  DXA308                 8      8
     D  DXA309                 9      9
     D  DXA310                10     10
     D                 DS
     D  dsSYfl11               1     10
     D  scQuest                1      1
     D  scDCF                  2      2
     D  sciDARTS               3      3
     D  XXGUI                  4      4
     D  scDocEnv               5      5
     D  scNotify               6      6
     D  scScriptur             7      7
     D  scScriptO              8      8
     D  sc3rdXml               9      9
     D                 DS
     D  dsSYfl12               1     10
     D  scGrpCopy              1      1
     D  XXCMS                  2      2
     d  scAutoTrig             3      3
     d  scRulesEng             5      5
     d  scSMS                  7      7
     d  scRulesSql             8      8
     D                 DS
     D  dsSYfl15               1     10
     D  scDashCfg              1      1
     D                 DS
     D  dsSYfl16               1     10
     D  scWebSvcs              1      1
     D  sc3rdParty             2      2
       //*** Beg Add *** 02/23/23 *******************************//
     D  scNewPay               3      3
       //*** End Add *** 02/23/23 *******************************//
     D                 DS
     D  dsWCfl54               1     10
     D  scInvCloud             1      1
     D aryMaskSC       ds                  likeds(dsMaskingGlobal) dim(#ofMasks)
     D dsMaskingSC     ds           350
     D   scMsk01                      1    overlay(dsMaskingSC:    1)
     D   scMskUsr01                  10    overlay(dsMaskingSC:    6)
     D   scMsk02                      1    overlay(dsMaskingSC:   36)
     D   scMskUsr02                  10    overlay(dsMaskingSC:   41)
     D   scMsk03                      1    overlay(dsMaskingSC:   71)
     D   scMskUsr03                  10    overlay(dsMaskingSC:   76)
     D   scMsk04                      1    overlay(dsMaskingSC:  106)
     D   scMskUsr04                  10    overlay(dsMaskingSC:  111)
     D   scMsk05                      1    overlay(dsMaskingSC:  141)
     D   scMskUsr05                  10    overlay(dsMaskingSC:  146)
     D   scMsk06                      1    overlay(dsMaskingSC:  176)
     D   scMskUsr06                  10    overlay(dsMaskingSC:  181)
     D   scMsk07                      1    overlay(dsMaskingSC:  211)
     D   scMskUsr07                  10    overlay(dsMaskingSC:  216)
     D   scMsk08                      1    overlay(dsMaskingSC:  246)
     D   scMskUsr08                  10    overlay(dsMaskingSC:  251)
     D   scMsk09                      1    overlay(dsMaskingSC:  281)
     D   scMskUsr09                  10    overlay(dsMaskingSC:  256)
     D   scMsk10                      1    overlay(dsMaskingSC:  316)
     D   scMskUsr10                  10    overlay(dsMaskingSC:  321)
     D   aryMaskingSC                35    overlay(dsMaskingSC:    1)
     D                                     dim(#ofMasks)
     D dsMaskingSY     ds           350
     D   SYmk01                      35    overlay(dsMaskingSY:    1)
     D   SYmk02                      35    overlay(dsMaskingSY:   36)
     D   SYmk03                      35    overlay(dsMaskingSY:   71)
     D   SYmk04                      35    overlay(dsMaskingSY:  106)
     D   SYmk05                      35    overlay(dsMaskingSY:  141)
     D   SYmk06                      35    overlay(dsMaskingSY:  176)
     D   SYmk07                      35    overlay(dsMaskingSY:  211)
     D   SYmk08                      35    overlay(dsMaskingSY:  246)
     D   SYmk09                      35    overlay(dsMaskingSY:  281)
     D   SYmk10                      35    overlay(dsMaskingSY:  316)
     D  dsMskUsrER     ds                  inz
     D   erMskUsr01                   1
     D   erMskUsr02                   1
     D   erMskUsr03                   1
     D   erMskUsr04                   1
     D   erMskUsr05                   1
     D   erMskUsr06                   1
     D   erMskUsr07                   1
     D   erMskUsr08                   1
     D   erMskUsr09                   1
     D   erMskUsr10                   1
     D   aryMskUsrER                  1    overlay(dsMskUsrER) dim(#ofMasks)
     D                 DS
     D  dsWCsc01               1     10
     D  scClmRsvWk             1      1
     D                 DS
     D  dsWCsc02               1     10
     D  scVstTrk               1      1
     D                 DS
     D  XXCLT                  1      3
     D  SYFL08                 1      1
     D  SYFL09                 2      2
     D  SYFL10                 3      3

     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D XXPWD           C                   CONST('12345PDS            ')
     D strMessage      s            256
     D sysDate7        s              7  0
     D sysTime6        s              6  0

     D/copy sPrDate
     D/copy sPrModule
     D/copy sPrSecure
     D/copy sPrGenPop
     D/copy sPrString
     D/copy sPrMask

     dChkObject        pr                  extpgm('SRCHKOBJ')
     d                               10    const
     d                                7    const
     d                                1

      /Free
       Exfmt SCR01;
       If *inkc;
          Exsr EndPgm;
       Endif;

       //Define PASSWORD
      /If not Defined(PASSWORD)
       If not Valid_KeyCode(scPwd);
          strMessage = 'Invalid password entered. Security key required. +
           Program will close.';
          Exsr InvalidPassword;
          Exsr EndPgm;
       Endif;
      /Endif
      /If Defined(PASSWORD)
       If scPwd <> xxPwd;
          strMessage = 'Invalid password entered. Program will close.';
          Exsr InvalidPassword;
          Exsr EndPgm;
       Endif;
      /Endif
       EXSR LoadScreenValues;
       scAllTab = *zero;
       Dou *inkc;
         Exfmt SCRall;
         If Validation() = *on;
            Exsr WriteToFile;
         Endif;
       Enddo;

       Exsr EndPgm;

       // This is old process left here for later review
       //** PROCESS SCREEN 2
       Exfmt SCR02;
       If *inkc;
          Exsr EndPgm;
       Endif;

       //** PROCESS SCREEN 3
       Exfmt SCR03;
       If *inkc;
          Exsr EndPgm;
       Endif;

       //** PROCESS SCREEN 4
       Exfmt SCR04;
       If *inkc;
          Exsr EndPgm;
       Endif;

       //** PROCESS SCREEN 5
       Exfmt SCR05;
       If *inkc;
          Exsr EndPgm;
       Endif;

       Exsr WriteToFile;

       *inlr = *on;
       Return;

       //=====================================================================
       Begsr EndPgm;
        *inlr = *on;
        Return;
       Endsr;

       //=====================================================================
       Begsr InvalidPassword;
        PopOneBtn(strMessage:'*** Invalid Password ***');
       Endsr;

       //=====================================================================
       Begsr LoadScreenValues;

        vsMasking = *off;
        If IsMaskingActive('*ANY');
           vsMasking = *on;
        Endif;

        //  Access System Control file 'SY' record
        Clear ssy01;
        Chain(n) 'SY' SSY01;
        dsSYfl11 = SYfl11;
        dsSYfl12 = SYfl12;
        dsSYfl15 = SYfl15;
        dsSYfl16 = SYfl16;
        scWebSvNm = GetModuleProperty('Portal Module':'Name');
        dsMaskingSC = dsMaskingSY;
      /End-free
     C                   MOVEL     SYWCIF        SCWCIF
     C                   MOVEL     SYRCIF        SCRCIF
     C                   MOVEL     SYFL02        SCMCIF
     C                   MOVEL     SYFL05        SCNPWC
     C                   MOVEL     SYFL06        SCMGRI
     C                   MOVEL     SYBCIF        SCBCIF
     C                   MOVEL     SYOFIF        SCOFIF
     C                   MOVEL     SYCSIF        SCCSIF
     C                   MOVEL     SYFIIF        SCFIIF
     C                   MOVEL     SYFBIF        SCFBIF
     C                   MOVEL     SYSCIF        SCSCIF
     C                   MOVEL     SYPCIF        SCPCIF
     C                   MOVEL     SYIMIF        SCIMIF
     C                   MOVEL     SYFXIF        SCFXIF
     C                   MOVEL     SYFL01        SCFL01
     C                   MOVE      SYAL04        SCCLT#
     C                   MOVE      XXCLT         SCCLT
     C                   MOVE      XXCMS         SCCMS
     C                   MOVEL     SYAL05        SCCRED
     C                   Z-ADD     SYMXDC        SCMXDC
      /Free
       //*** Beg Add *** 02/23/23 *******************************//
        If scNewPay = ' ';
           scNPS = 'Y';
        Else;
           scNPS = scNewPay;
        EndIf;
       //*** End Add *** 02/23/23 *******************************//
        scMxDc   = SYmxDc;
        scSyFl01 = SYfl01;
        scSyFl03 = SYfl03;
        scSyFl04 = SYfl04;
        scSyFl06 = SYfl06;
        scSyFl07 = SYfl07;
        scBcID   = DtMDYY(SYbcID);
        scCsID   = DtMDYY(SYcsID);
        scScID   = DtMDYY(SYscID);
        scOfID   = DtMDYY(SYofID);
        scFxID   = DtMDYY(SYfxID);
        scFbID   = DtMDYY(SYfbID);
        scImID   = DtMDYY(SYimID);
        scPcID   = DtMDYY(SYpcID);
        scRcID   = DtMDYY(SYrcID);
        scWcID   = DtMDYY(SYwcID);
        scFiID   = DtMDYY(SYfiID);
        scSyDt01 = DtMDYY(SYdt01);
        scSyDt02 = DtMDYY(SYdt02);
        scSyDt03 = DtMDYY(SYdt03);
        scImEx  = %Subst(SYFL12:9:1);

        //  Access Darts Control file 'SC' record
        Clear sdr01;
        Chain(n) 'SC' SDR01;
        dsDRal03 = DRal03;
        //*** Beg Add *** 07/07/22 ********************************************
        scImdLock = DRfl16;
        //*** End Add *** 07/07/22 ********************************************

        //  Access System Control file 'FB' record
        Clear sfb01;
        Chain(n) 'FB' SFB01;
      /End-free
     C                   MOVEL     FBFL01        SCWCIN
     C                   MOVE      FBFL06        SCNTWK
     C                   MOVE      FBFL07        SCUR
     C                   MOVE      FBFL08        SCFL08
     C                   MOVE      FBFL09        SCFL09
     C                   MOVE      FBAL03        XXA301
     C                   MOVE      XFA302        SCCPRV
     C                   SUBST     FBAL04:1      SCAL41
     C                   SUBST     FBAL04:2      SCAL42
     C                   SUBST     FBAL05:4      SCAINV
     C                   SUBST     FBAL05:5      SCWHPG
     C                   SUBST     FBAL05:6      SCPRIV
     C                   SUBST     FBAL05:7      SCTDUP
     C                   SUBST     FBAL05:8      SCNWSL
     C                   SUBST     FBAL04:5      SCAR
     C                   SUBST     FBAL04:6      SCEDI
     C                   SUBST     FBAL04:7      SCRHAB
     C                   SUBST     FBAL04:8      SCAL48
     C                   SUBST     FBAL04:9      SCFSMS
     C                   SUBST     FBAL04:10     SCPRVB
     C                   SUBST     FBAL05:1      SCAL51
     C                   SUBST     FBAL05:2      SCAL52
     C                   SUBST     FBAL05:3      SCOS
     C                   SUBST     FBAL01:1      SCAL01
     C                   SUBST     FBAL01:2      SCAL02
     C                   SUBST     FBAL01:3      SCAL03
     C                   SUBST     FBAL01:4      SCWCDK
     C                   SUBST     FBAL01:9      SCMED
     C                   SUBST     FBAL01:10     SCAL10
     C                   Z-ADD     FBDT02        SCDT02
     C                   Z-ADD     FBDT03        SCDT03
     C                   MOVE      FBINDT        SCINDT
     C                   MOVE      FBRLVL        SCRLVL
     C                   MOVE      'Y'           SCPNDF
     C                   MOVEL     FBST01        SCCRVW
     C                   MOVEL     FBST02        SCPRBL
     C                   MOVE      FBST03        SCPREO
     C                   MOVEL     FBST04        SCPARN
     C                   MOVE      FBST04        SCKCIF
     C                   MOVEL     FBST05        SCCURS
     C                   MOVE      FBST05        SCPCIB
     C                   MOVEL     FBST06        SCUCPF
     C                   MOVEL     FBST07        SCNCDC
     C                   MOVE      FBST07        SCSPLT
     C                   MOVEL     FBST08        SCICD9
     C                   MOVE      FBST08        SCMPPO
     C                   MOVEL     FBST09        SCLAST
     C                   MOVEL     FBst10        scIncEobE

      /Free
            scBIDLInst = %Subst(FBst10:2:1);
            scCexp = %Subst(FBal02:1:1);
            scEexp = %Subst(FBal02:2:1);
            scPexp = %Subst(FBal02:3:1);
            scPDexp = %Subst(FBal02:4:1);
            scBupl = %Subst(FBal02:5:1);
            sc837p = %Subst(FBal02:6:1);
            sc837i = %Subst(FBal02:7:1);
            sc835pa = %Subst(FBal02:8:1);
       //*** Beg Add *** 08/18/22 *******************************//
            scMeobPrt =  %Subst(FBal02:9:1);
       //*** End Add *** 08/18/22 *******************************//
      /End-free

     C                   SUBST     FBAL05:10     SCCOCO

      /Free
        //  Access WCMS Control file 'WC' record
        Clear swc01;
        Chain(n) 'WC' SWC01;

        dsWCsc01 = WCsc01;
        dsWCsc02 = WCsc02;
      /End-free
     C                   MOVEL     WCFL22        SCARLS
     C                   MOVEL     WCFL01        SCWF01
     C                   MOVEL     WCFL02        SCWF02
     C                   MOVEL     WCFL03        SCWF03
     C                   MOVEL     WCFL04        SCWF04
     C                   MOVEL     WCFL05        SCWF05
     C                   MOVEL     WCFL06        SCWF06
     C                   MOVEL     WCFL07        SCWF07
     C                   MOVEL     WCFL08        SCWF08
     C                   MOVEL     WCFL09        SCWF09
     C                   MOVEL     WCFL10        SCWF10
     C                   MOVEL     WCFL11        SCWF11
     C                   MOVEL     WCFL12        SCWF12
     C                   MOVEL     WCFL13        SCWF13
     C                   MOVEL     WCFL14        SCWF14
     C                   MOVEL     WCFL15        SCWF15
     C                   MOVEL     WCFL16        SCWF16
     C                   MOVEL     WCFL17        SCWF17
     C                   MOVEL     WCFL18        SCWF18
     C                   MOVEL     WCFL19        SCWF19
     C                   MOVEL     WCFL20        SCWF20
     C                   MOVEL     WCFL21        SCWF21
     C                   MOVEL     WCFL23        SCWF23
     C                   MOVEL     WCFL24        SCWF24
     C                   MOVEL     WCFL25        SCWF25
     C                   MOVEL     WCFL26        SCWF26
     C                   MOVEL     WCSC06        scNegFees
     C                   MOVEL     WCSC07        scIdc
      /Free
       scEpay = %Subst(WCSC07:2:1);
      /End-free
     C                   MOVEL     WCFL27        SCWF27
     C                   MOVEL     WCFL28        SCWF28
     C                   MOVEL     WCFL29        SCWF29
     C                   MOVEL     WCFL30        SCWF30
     C                   MOVEL     WCFL31        SCWF31
     C                   MOVEL     WCFL32        SCWF32
     C                   MOVEL     WCFL33        SCWF33
     C                   MOVEL     WCFL34        SCWF34
     C                   MOVEL     WCFL35        SCWF35
     C                   MOVEL     WCFL36        SCWF36
     C                   MOVEL     WCFL37        SCWF37
     C                   MOVEL     WCFL38        SCWF38
      /Free
       scOnlineSR = GetModuleProperty('S/R Online':'Name');
      /End-free
     C                   MOVEL     WCFL39        SCWF39
     C                   MOVEL     WCFL40        SCWF40
     C                   MOVEL     WCFL41        SCWF41
     C                   MOVEL     WCFL42        SCWF42
     C                   MOVEL     WCFL43        SCWF43
     C                   MOVEL     WCFL44        SCWF44
     C                   MOVEL     WCFL45        SCWF45
     C                   MOVEL     WCFL46        SCWF46
     C                   MOVEL     WCFL47        SCWF47
     C                   MOVEL     WCFL48        SCWF48
     C                   MOVEL     WCFL49        SCWF49
     C                   MOVEL     WCFL50        SCWF50
     C                   MOVEL     WCFL51        SCWF51
     C                   MOVEL     WCFL52        SCWF52
     C                   MOVEL     WCFL53        SCWF53

      /Free
        dsWCfl54 = WCfl54;
       //***  Begin Add  ***  05/23/22  ******************************
       scIVCNm = GetModuleProperty('Invoice Cloud':'Name');
       //***  End   Add  ***  05/23/22  ******************************

        //  Access Rehab Control file 'RC' record
        Clear src01;
        Chain(n) 'RC' SRC01;

        //  Access Barcode Control file 'BC' record
        Clear sbc01;
        Chain(n) 'BC' SBC01;

        //  Access PCMS Control file 'PC' & 'P2' records
        Clear spc01;
        Chain(n) 'PC' SPC01;
        Clear sp201;
        Chain(n) 'P2' SP201;

        //  Access Claim Scan Control file 'CS' record
        Clear scs01;
        Chain(n) 'CS' SCS01;

        //  Access GL/AP Control file 'GL' record
        Clear sgl01;
        Chain(n) 'GL' SGL01;

       Endsr;

       //=====================================================================
       Begsr WriteToFile;

        sysDate7 = DateTo7(SystemDate);
        sysTime6 = TimeTo6(SystemTime);

        //  Access System Control file 'SY' record
        Chain 'SY' SSY01;
        SYdt   = sysDate7;
        SYtime = sysTime6;
      /End-free
     C                   MOVEL(P)  Q1USER        SYUSER
     C                   MOVEL(P)  Q1PGM         SYPGM
     C                   MOVEL     SCWCIF        SYWCIF
     C                   MOVEL     SCRCIF        SYRCIF
     C                   MOVEL     SCMCIF        SYFL02
     C                   MOVEL     SCNPWC        SYFL05
     C                   MOVEL     SCMGRI        SYFL06
     C                   MOVEL     SCBCIF        SYBCIF
     C                   MOVEL     SCOFIF        SYOFIF
     C                   MOVEL     SCCSIF        SYCSIF
     C                   MOVEL     SCFIIF        SYFIIF
     C                   MOVEL     SCFBIF        SYFBIF
     C                   MOVEL     SCSCIF        SYSCIF
     C                   MOVEL     SCPCIF        SYPCIF
     C                   MOVEL     SCIMIF        SYIMIF
     C                   MOVEL     SCFXIF        SYFXIF
     C                   MOVEL     SCFL01        SYFL01
     C                   MOVE      SCCLT#        SYAL04
     C                   MOVEL     SCCRED        SYAL05
     C                   MOVE      SCCLT         XXCLT
     C                   MOVE      SCCMS         XXCMS
      /Free
        SYmxDc = scMxDc;
        SYfl01 = scSyFl01;
        SYfl03 = scSyFl03;
        SYfl04 = scSyFl04;
        SYfl06 = scSyFl06;
        SYfl07 = scSyFl07;
        SYbcID = DtCYMD(scBcID);
        SYcsID = DtCYMD(scCsID);
        SYscID = DtCYMD(scScID);
        SYofID = DtCYMD(scOfID);
        SYfxID = DtCYMD(scFxID);
        SYfbID = DtCYMD(scFbID);
        SYimID = DtCYMD(scImID);
        SYpcID = DtCYMD(scPcID);
        SYrcID = DtCYMD(scRcID);
        SYwcID = DtCYMD(scWcID);
        SYfiID = DtCYMD(scFiID);
        SYdt01 = DtCYMD(scSyDt01);
        SYdt02 = DtCYMD(scSyDt02);
        SYdt03 = DtCYMD(scSyDt03);
        SYfl11 = dsSYfl11;
        SYfl12 = dsSYfl12;
        SYfl15 = dsSYfl15;
        SYfl16 = dsSYfl16;
       //*** Beg Add *** 02/23/23 *******************************//
        If scNPS = 'Y';
           scNewPay = ' ';
        Else;
           scNewPay = 'N';
        EndIf;
       //*** End Add *** 02/23/23 *******************************//
        SetModuleProperty('Portal Module':'Name':scWebSvNm);
        dsMaskingSY = dsMaskingSC;
        %Subst(SYFL12:9:1) = scImEx;
        If %found;
           Update SSY01;
        Else;
           SYkey = 'SY';
           SYiDt  = SYdt;
           SYiUsr = SYuser;
           Write SSY01;
        Endif;

        //  Access Darts Control file 'SC' record
        Chain 'SC' SDR01;
        DRdt   = sysDate7;
        DRtime = sysTime6;
        DRuser = q1User;
        DRpgm  = q1Pgm;
        DRal03 = dsDRal03;
        //*** Beg Add *** 07/07/22 ********************************************
        DRfl16 = scImdLock;
        //*** End Add *** 07/07/22 ********************************************
        If %found;
           Update SDR01;
        Else;
           DRkey  = 'SC';
           DRiDt  = DRdt;
           DRiUsr = DRuser;
           Write SDR01;
        Endif;

        //  Access System Control file 'FB' record
        Chain 'FB' SFB01;
        FBdt   = sysDate7;
        FBtime = sysTime6;
      /End-free
     C                   MOVEL(P)  Q1USER        FBUSER
     C                   MOVEL(P)  Q1PGM         FBPGM
     C                   MOVEL     SCWCIN        FBFL01
     C                   MOVE      SCNTWK        FBFL06
     C                   MOVE      SCUR          FBFL07
     C                   MOVE      SCFL08        FBFL08
     C                   MOVE      SCFL09        FBFL09
     C                   MOVE      SCAL41        XFA401
     C                   MOVE      SCAL42        XFA402
     C                   MOVE      SCAINV        XFA504
     C                   MOVE      SCWHPG        XFA505
     C                   MOVE      SCPRIV        XFA506
     C                   MOVE      SCTDUP        XFA507
     C                   MOVE      SCNWSL        XFA508
     C                   MOVE      SCAL48        XFA408
     C                   MOVE      SCFSMS        XFA409
     C                   MOVE      SCPRVB        XFA410
     C                   MOVE      SCAR          XFA405
     C                   MOVE      SCEDI         XFA406
     C                   MOVE      SCRHAB        XFA407
     C                   MOVE      SCAL51        XFA501
     C                   MOVE      SCCOCO        XFA510
     C                   MOVE      SCAL52        XFA502
     C                   MOVE      SCOS          XFA503
     C                   MOVE      SCAL01        XFA101
     C                   MOVE      SCAL02        XFA102
     C                   MOVE      SCAL03        XFA103
     C                   MOVE      SCWCDK        XFA104
     C                   MOVE      'Y'           XFA108
     C                   MOVE      SCMED         XFA109
     C                   MOVE      SCAL10        XFA110
     C                   MOVE      XXA101        FBAL01
     C                   MOVE      SCCPRV        XFA302
     C                   MOVE      XXA301        FBAL03
     C                   MOVE      XXA401        FBAL04
     C                   MOVE      XXA501        FBAL05
     C                   Z-ADD     SCDT02        FBDT02
     C                   Z-ADD     SCDT03        FBDT03
     C                   MOVE      SCINDT        FBINDT
     C                   MOVE      SCRLVL        FBRLVL
     C                   MOVE      'Y'           FBPNDF
     C                   MOVEL     SCCRVW        FBST01
     C                   MOVEL     SCPRBL        FBST02
     C                   MOVE      SCPREO        FBST03
     C                   MOVEL     SCPARN        FBST04
     C                   MOVE      SCKCIF        FBST04
     C                   MOVEL     SCCURS        FBST05
     C                   MOVE      SCPCIB        FBST05
     C                   MOVEL     SCUCPF        FBST06
     C                   MOVEL     SCNCDC        FBST07
     C                   MOVEL     SCICD9        FBST08
     C                   MOVE      SCMPPO        FBST08
     C                   MOVE      SCSPLT        FBST07
     C                   MOVEL     SCLAST        FBST09
     C                   MoveL     scIncEobE     FBST10
      /Free
            %Subst(FBst10:2:1)   = scBIDLInst;
            %Subst(FBal02:1:1) = scCexp;
            %Subst(FBal02:2:1) = scEexp;
            %Subst(FBal02:3:1) = scPexp;
            %Subst(FBal02:4:1) = scPDexp;
            %Subst(FBal02:5:1) = scBupl;
            %Subst(FBal02:6:1) = sc837p;
            %Subst(FBal02:7:1) = sc837i;
            %Subst(FBal02:8:1) = sc835pa;
       //*** Beg Add *** 08/18/22 *******************************//
            %Subst(FBal02:9:1) = scMeobPrt;
       //*** End Add *** 08/18/22 *******************************//
        If %found;
           Update SFB01;
        Else;
           FBkey  = 'FB';
           FBiDt  = FBdt;
           FBiUsr = FBuser;
           Write SFB01;
        Endif;

        //  Access WCMS Control file 'WC' record
        Chain 'WC' SWC01;
        WCdt   = sysDate7;
        WCtime = sysTime6;
        WCuser = q1User;
        WCpgm  = q1Pgm;
      /End-free
     C                   MOVEL     SCWF01        WCFL01
     C                   MOVEL     SCWF02        WCFL02
     C                   MOVEL     SCWF03        WCFL03
     C                   MOVEL     SCWF04        WCFL04
     C                   MOVEL     SCWF05        WCFL05
     C                   MOVEL     SCWF06        WCFL06
     C                   MOVEL     SCWF07        WCFL07
     C                   MOVEL     SCWF08        WCFL08
     C                   MOVEL     SCWF09        WCFL09
     C                   MOVEL     SCWF10        WCFL10
     C                   MOVEL     SCWF11        WCFL11
     C                   MOVEL     SCWF12        WCFL12
     C                   MOVEL     SCWF13        WCFL13
     C                   MOVEL     SCWF14        WCFL14
     C                   MOVEL     SCWF15        WCFL15
     C                   MOVEL     SCWF16        WCFL16
     C                   MOVEL     SCWF17        WCFL17
     C                   MOVEL     SCWF18        WCFL18
     C                   MOVEL     SCWF19        WCFL19
     C                   MOVEL     SCWF20        WCFL20
     C                   MOVEL     SCWF21        WCFL21
     C                   MOVEL     SCWF23        WCFL23
     C                   MOVEL     SCWF24        WCFL24
     C                   MOVEL     SCWF25        WCFL25
     C                   MOVEL     SCWF26        WCFL26
     C                   MOVEL     scNegFees     WCSC06
     C                   MOVEL     scIdc         WCSC07
      /Free
       %Subst(WCsc07:2:1) = scEpay;
      /End-free
     C                   MOVEL     SCWF27        WCFL27
     C                   MOVEL     SCWF28        WCFL28
     C                   MOVEL     SCWF29        WCFL29
     C                   MOVEL     SCWF30        WCFL30
     C                   MOVEL     SCWF31        WCFL31
     C                   MOVEL     SCWF32        WCFL32
     C                   MOVEL     SCWF33        WCFL33
     C                   MOVEL     SCWF34        WCFL34
     C                   MOVEL     SCWF35        WCFL35
     C                   MOVEL     SCWF36        WCFL36
     C                   MOVEL     SCWF37        WCFL37
     C                   MOVEL     SCWF38        WCFL38
      /Free
       SetModuleProperty('S/R Online':'Name':scOnlineSR);
      /End-free
     C                   MOVEL     SCWF39        WCFL39
     C                   MOVEL     SCWF40        WCFL40
     C                   MOVEL     SCWF41        WCFL41
     C                   MOVEL     SCWF42        WCFL42
     C                   MOVEL     SCWF43        WCFL43
     C                   MOVEL     SCWF44        WCFL44
     C                   MOVEL     SCWF45        WCFL45
     C                   MOVEL     SCWF46        WCFL46
     C                   MOVEL     SCWF47        WCFL47
     C                   MOVEL     SCWF48        WCFL48
     C                   MOVEL     SCWF49        WCFL49
     C                   MOVEL     SCWF50        WCFL50
     C                   MOVEL     SCWF51        WCFL51
     C                   MOVEL     SCWF52        WCFL52
     C                   MOVEL     SCWF53        WCFL53
     C                   MOVEL     SCARLS        WCFL22
      /Free
        WCfl54 = dsWCfl54;
       //***  Begin Add  ***  05/23/222 ******************************
       SetModuleProperty('Invoice Cloud':'Name':scIVCNm);
       //***  End   Add  ***  05/23/22  ******************************
        WCsc01 = dsWCsc01;
        WCsc02 = dsWCsc02;
        If %found;
           Update SWC01;
        Else;
           WCiDt  = WCdt;
           WCiUsr = WCuser;
           Write SWC01;
        Endif;

       Endsr;
      /End-free

      //=============================================================
      // Check for object existence in LIBL
      //=============================================================
     PObjExists        b
     dObjExists        pi             1n
     d inObj                         10    const
     d inType                         7    const

     disError          s              1n

      /Free
       ChkObject(inObj:inType:isError);
       Return not isError;
      /End-free
     PObjExists        e
      //=============================================================
      // Validate screens
      //=============================================================
     PValidation       b
     dValidation       pi             1n

     di1               s             10i 0
     disValid          s              1n   inz('1')

      /Free
       Select;
       When scTab = 0;
          Exsr SSYPF;
       Endsl;
       Return isValid;
       //----------------
       Begsr SSYPF;
        Reset aryMskUsrER;
        aryMaskSC = aryMaskingSC;
        For i1 = 1 to #ofMasks;
           If aryMaskSC(i1).user1 <> *blanks;
              aryMaskSC(i1).user1 = Upper(aryMaskSC(i1).user1);
              If not ObjExists(aryMaskSC(i1).user1:'*USRPRF');
                 aryMskUsrER(i1) = *on;
                 isValid = *off;
              Endif;
           Endif;
        Endfor;
        aryMaskingSC = aryMaskSC;
       Endsr;
      /End-free
     PValidation       e
