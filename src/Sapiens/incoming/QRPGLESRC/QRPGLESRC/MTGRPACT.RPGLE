     hcopyright('(c) 2024 Sapiens, Inc. All rights reserved.')
     h/title MTGRPACT- Group Configuration(Profound)
     h dftname(MTGRPACT)
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h ALWNULL(*UsrCtl)
     h/define   profoundUI
     h bnddir('STBNDDIR')
     h bnddir('STAPPCFG')
      //*********************************************************************
      //==============================================================*
      //  Created   : 06/10/24                                         *
      //  Programmer: C:342                                            *
      //  Project   : MC447 - Redesign of Work with Groups             *
      //===============================================================*
      //  Revisions:                                                   *
      //                                                               *
      //                                                               *
      // ------------------------------------------------------------- *
      //  NOTE: Please add comments to the top of Revisions.           *
      // ------------------------------------------------------------- *
      //===============================================================*

     fMTGRPACT  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     Sfile(sflTMRate:scSflRRN1)
     f                                     Sfile(sflModInfo:scSflRRN2)
     f                                     Sfile(sflNetInfo:scSflRRN3)
     f                                     Sfile(sflOSCode:scSflRRN4)
     fstactpnl  cf   e             workstn
     f                                     handler('PROFOUNDUI(HANDLER)')
     f                                     include(rcdGrpPnl)
      * Company file
     fsmcol     if   e           k disk    rename(pco01:pco00)
      * Fund Master file
     ffmfnp     if   e             disk    rename(pfn01:pfn0p)
     ffmfnl     uf   e           k disk    rename(pfn01:pfn00)
     f                                     infds(dbfds)
     ffmfnl1    if   e           k disk    rename(pfn01:pfn01)
      * Fund Auxiliary File
     ffmf1p     if   e             disk    rename(pf101:pf10p)
     ffmf1l     uf a e           k disk    rename(pf101:pf100)
     f                                     infds(dbfds5)
      * Network (All systems) Billing Rate/Discount Master
     fnmbdl     if   e           k disk    rename(pbd01:pbd00)
      * User Parameter File
     fsmupl     if   e           k disk    rename(pup01:pup00)
      * Application Security File
     fsdtbl1    if   e           k disk
      * Group T/M rate file.
     ffmfrl     if   e           k disk    rename(pfr01:pfr00)
     f                                     infds(dbfds1)
     ffmfrp     uf a e             disk    rename(pfr01:pfr0p)
     f                                     infds(dbfds2)
      * Group Outcome/Savings Code File
     fftgop     uf a e             disk    rename(pgo01:pgo0p)
     f                                     infds(dbfds3)
     fftgol     if   e           k disk    rename(pgo01:pgo00)
     f                                     infds(dbfds4)
      * Outcome/Savings Table File
     fftotl     if   e           k disk    rename(pot01:pot00)
      * Generic table file
     fftgtl     if   e           k disk    rename(pgt01:pgt00)
      * WCMC Provider Master - PF
     ffmpdl     if   e           k disk    rename(ppd01:ppd00)
      * PPO Network Master
     fnmnwl     if   e           k disk    rename(pnw01:pnw00)

     d psds          esds                  extname(szq1p)
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
     d dbfds5          ds
     d  dbrrn5               397    400b 0
      * variable for Tab Names
     d arrTabNames     s            100    varying inz dim(20)
     d n1              s             10i 0 inz(1)
     d strTabNames     s           2000    varying inz
      * work variables
     d i               s              3  0
     d j               s              3  0
     d clientID        s              3    inz('000')
     d intCo#          s              3  0
     d inCo#           s              3  0
     d inSub           s              5  0
     d inFnd           s              3  0
     d xxRrn           s              9  0 inz
     d indexTab        s              5  0
     d chgMod          s               n   inz(*off)
     d dspMode         s               n   inz(*off)
     d isSaved         s               n   inz(*off)
     d isFirst         s               n   inz(*on)
     d error           s               n   inz(*off)
     d error1          s               n   inz(*off)
     d error2          s               n   inz(*off)
     d process         s               n   inz(*off)
     d UsrDftFnd       s                   like(UPfnd) inz
     d UsrDftSub       s                   like(UPsub#) inz
     d UsrDftFyr       s                   like(UPfyr) inz
     d xxDate1         s              8  0
     d isAR            s               n   inz
     d isNtwk          s               n   inz
     d isMedMgnt       s               n   inz
     d svSflRRN1       s              9  0 inz
     d svSflRRN4       s              9  0 inz

     d saveDT1         s                   like(FNdt)
     d saveTM1         s                   like(FNtime)
     d saveDT2         s                   like(FNdt)
     d saveTM2         s                   like(FNtime)
     d saveDT3         s                   like(FNdt)
     d saveTM3         s                   like(FNtime)
     d flgRcdUpdated   s              1    inz
     d xxDelResp       s               n   inz
     d
         dcl-s  inDspFileName char(10);
         dcl-s  inPgmName     char(50);
         dcl-s  inTabName     char(50);

     d                 ds
     d dsIsError               1     10    dim(10)
     d  isError01              1      1
     d  isError02              2      2
     d  isError03              3      3
     d  isError04              4      4
     d  isError05              5      5
     d  isError06              6      6
     d  isError07              7      7
     d  isError08              8      8
     d  isError09              9      9
     d  isError10             10     10

     d* Screen System Code
     d                 ds
     d dsSysCd                 1     10    dim(10)
     d  SysCd01                1      1
     d  SysCd02                2      2
     d  SysCd03                3      3
     d  SysCd04                4      4
     d  SysCd05                5      5
     d  SysCd06                6      6
     d  SysCd07                7      7
     d  SysCd08                8      8
     d  SysCd09                9      9
     d  SysCd10               10     10

     d* AR Rate Code
     d                 ds
     d dsArCd                  1     30    dim(10)
     d  ArCd01                 1      3
     d  ArCd02                 4      6
     d  ArCd03                 7      9
     d  ArCd04                10     12
     d  ArCd05                13     15
     d  ArCd06                16     18
     d  ArCd07                19     21
     d  ArCd08                22     24
     d  ArCd09                25     27
     d  ArCd10                28     30

     d* Group Network Information
     d* Network code
     d                 ds
     d dsNwCd                  1     50    dim(10)
     d  NwCd01                 1      5
     d  NwCd02                 6     10
     d  NwCd03                11     15
     d  NwCd04                16     20
     d  NwCd05                21     25
     d  NwCd06                26     30
     d  NwCd07                31     35
     d  NwCd08                36     40
     d  NwCd09                41     45
     d  NwCd10                46     50

     d* Network Type screen fields
     d                 ds
     d dsNwTyp                 1     10    dim(10)
     d  NwTyp01                1      1
     d  NwTyp02                2      2
     d  NwTyp03                3      3
     d  NwTyp04                4      4
     d  NwTyp05                5      5
     d  NwTyp06                6      6
     d  NwTyp07                7      7
     d  NwTyp08                8      8
     d  NwTyp09                9      9
     d  NwTyp10               10     10

     d* Network Fee Type screen fields
     d                 ds
     d dsNwFTyp                1     10    dim(10)
     d  NwFTyp01               1      1
     d  NwFTyp02               2      2
     d  NwFTyp03               3      3
     d  NwFTyp04               4      4
     d  NwFTyp05               5      5
     d  NwFTyp06               6      6
     d  NwFTyp07               7      7
     d  NwFTyp08               8      8
     d  NwFTyp09               9      9
     d  NwFTyp10              10     10

     d* Network Fee Rate screen fields
     d                 ds
     d dsNtFRt                 1     90  2 dim(10)
     d  NtFRt01                1      9  2
     d  NtFRt02               10     18  2
     d  NtFRt03               19     27  2
     d  NtFRt04               28     36  2
     d  NtFRt05               37     45  2
     d  NtFRt06               46     54  2
     d  NtFRt07               55     63  2
     d  NtFRt08               64     72  2
     d  NtFRt09               73     81  2
     d  NtFRt10               82     90  2

     d* FEIN/Mod screen fields
     d                 ds
     d dsNtFEIN                1    120    dim(10)
     d  NtFEIN01               1     12
     d  NtFEIN02              13     24
     d  NtFEIN03              25     36
     d  NtFEIN04              37     48
     d  NtFEIN05              49     60
     d  NtFEIN06              61     72
     d  NtFEIN07              73     84
     d  NtFEIN08              85     96
     d  NtFEIN09              97    108
     d  NtFEIN10             109    120

     d* WCMC Group Module Information
     d* Data base field for System Code
     d                 ds
     d dsFnSys                 1     10    dim(10)
     d  FNsy01                 1      1
     d  FNsy02                 2      2
     d  FNsy03                 3      3
     d  FNsy04                 4      4
     d  FNsy05                 5      5
     d  FNsy06                 6      6
     d  FNsy07                 7      7
     d  FNsy08                 8      8
     d  FNsy09                 9      9
     d  FNsy10                10     10

     d* Data base field for AR Rate Type
     d                 ds
     d dsFnAR                  1     30    dim(10)
     d  FNbr01                 1      3
     d  FNbr02                 4      6
     d  FNbr03                 7      9
     d  FNbr04                10     12
     d  FNbr05                13     15
     d  FNbr06                16     18
     d  FNbr07                19     21
     d  FNbr08                22     24
     d  FNbr09                25     27
     d  FNbr10                28     30

     d* WCMC Group Network Information
     d* Data base field for Network Code
     d                 ds
     d dsF1nwcd                1     50    dim(10)
     d  F1nw01                 1      5
     d  F1nw02                 6     10
     d  F1nw03                11     15
     d  F1nw04                16     20
     d  F1nw05                21     25
     d  F1nw06                26     30
     d  F1nw07                31     35
     d  F1nw08                36     40
     d  F1nw09                41     45
     d  F1nw10                46     50

     d* Data base field for Network Type
     d                 ds
     d dsF1NwTyp               1     10    dim(10)
     d  F1nf01                 1      1
     d  F1nf02                 2      2
     d  F1nf03                 3      3
     d  F1nf04                 4      4
     d  F1nf05                 5      5
     d  F1nf06                 6      6
     d  F1nf07                 7      7
     d  F1nf08                 8      8
     d  F1nf09                 9      9
     d  F1nf10                10     10

     d* Data base field for Network Fee Type
     d                 ds
     d dsF1NwFTyp              1     10    dim(10)
     d  F1nt01                 1      1
     d  F1nt02                 2      2
     d  F1nt03                 3      3
     d  F1nt04                 4      4
     d  F1nt05                 5      5
     d  F1nt06                 6      6
     d  F1nt07                 7      7
     d  F1nt08                 8      8
     d  F1nt09                 9      9
     d  F1nt10                10     10

     d* Data base field for Network Fee Rate
     d                 ds
     d dsF1NtFRt               1     90  2 dim(10)
     d  F1n$01                 1      9  2
     d  F1n$02                10     18  2
     d  F1n$03                19     27  2
     d  F1n$04                28     36  2
     d  F1n$05                37     45  2
     d  F1n$06                46     54  2
     d  F1n$07                55     63  2
     d  F1n$08                64     72  2
     d  F1n$09                73     81  2
     d  F1n$10                82     90  2

     d* Data base field for FEIN
     d                 ds
     d dsF1NtFEIN              1     90  0 dim(10)
     d  F1ni01                 1      9  0
     d  F1ni02                10     18  0
     d  F1ni03                19     27  0
     d  F1ni04                28     36  0
     d  F1ni05                37     45  0
     d  F1ni06                46     54  0
     d  F1ni07                55     63  0
     d  F1ni08                64     72  0
     d  F1ni09                73     81  0
     d  F1ni10                82     90  0

     d* Data base field for Mod
     d                 ds
     d dsF1NtMOD               1     30  0 dim(10)
     d  F1nm01                 1      3  0
     d  F1nm02                 4      6  0
     d  F1nm03                 7      9  0
     d  F1nm04                10     12  0
     d  F1nm05                13     15  0
     d  F1nm06                16     18  0
     d  F1nm07                19     21  0
     d  F1nm08                22     24  0
     d  F1nm09                25     27  0
     d  F1nm10                28     30  0

       // Variable for GroupType
     d  outRtCD        S              1
     d  outCo          S              3
     d  outSub#        S              5
     d  outFnd         S              3
     d  outSts         S              1

      * Clear both subfile error indicator
     d ClrSflError     pr

      * Get client ID
     d GetClientID     pr                  extpgm('RTVCLTID')
     d   outCltID                     3

     d GroupType       pr                  extpgm('FA701R')
     d                                1
     d                                3
     d                                5
     d                                3
     d                                1

      * Initialize the Screen fields for Add Group
     d InzGrpInfScVal  pr

      * Load System Code/ AR Rate grid
     d LoadModInfo     pr

      * Load Network Info grid
     d LoadNetInfo     pr

      * Load OS Code grid
     d LoadOSCode      pr

      * Load T/M Rates grid
     d LoadTMRate      pr

      * Main Processing
     d Main            pr

      * Process Add option Outcome/Savings Code
     d ProcessAddOSCd  pr

      * Process Add option T/M Rates
     d ProcessAddTMRt  pr

      * Process other option Outcome/Savings Code
     d ProcOtherOSCd   pr

      * Process other option T/M Rates
     d ProcOtherTMRt   pr

      * Process save option for Module Info
     d ProcSavModInfo  pr

      * Process save option for Network Info
     d ProcSavNetInfo  pr

      * Program Start
     d ProgramStart    pr
     d processMe                       n

      * Validate AR Info
     dValidateModInfo  pr              n

      * Validate Network Info
     dValidateNetInfo  pr              n

      * Validate Outcome/Savings Code
     d ValidateOSCd    pr              n

      * Validate T/M Rates
     d ValidateTMRt    pr              n
     d strmode                        1

     d/copy sprUsrPrms
     d/copy sPrDate
     d/copy sprCompany
     d/copy sPrAtrium
     d/copy sPrSecure
     d/copy sPrGenPop
     d/copy sPrModule

      // Entry of Program.

     c     *entry        plist
     c                   parm                    @@rrn             9            rrn
     c                   parm                    @@mode            1            mode
     c                   parm                    @@next           10            next pgm
     c                   parm                    @@TabInd          5            tab index

     c     keyFmfnl      klist
     c                   kfld                    intco#
     c                   kfld                    UsrDftSub

      /free

       If %parms >= 1;
          xxRrn = %int(@@rrn);
       Endif;

       Chain xxRrn pfn0p;
       inCo# = FNco#;
       inSub = FNsub#;
       inFnd = FNfnd;

       GetClientID(clientID);

       ProgramStart(process);

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
      /end-free

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
       // Clear Subfile error indicator;
       //===================================================================
     p ClrSflError     b
     d ClrSflError     pi

      /Free

       clear erSysCd;
       clear msSysCd;
       clear erArCd;
       clear msArCd;
       clear erNwCd;
       clear msNwCd;
       clear erNwTyp;
       clear msNwTyp;
       clear erNwFTyp;
       clear msNwFTyp;
       clear erNtFRt;
       clear msNtFRt;
       clear erNtFEIN;
       clear msNtFEIN;

      /End-free
     p ClrSflError     e

       //===================================================================
       // InzGrpInfScVal - Initialize the Screen fields and indicators
       //===================================================================
     p InzGrpInfScVal  b
     d InzGrpInfScVal  pi
     d
      /Free

      //Set visibility
       // Non-Workers Comp group only for client 114 CSI
       // Set visibility for AR Rate code, #days, #bills for auto release
       scRmvAR = 'Y';
       isAR     = *off;
       isAR     = IsModEnabled('Managed Care AR');
       If isAR = *on;
          visAR = *on;
          scRmvAR = *blanks;
       Endif;

       // Set visibility for ALLOW MC Case Closure

      //WCMS Group Module Information
      // System Type Selection Criteria
       selSys  = 'GTco# = ' + %EditC(intCo#:'X') + ' and ' +
          'GTstat<>' + '''I''' + 'and GTtbid=' + '''SYS''';

      //WCMC Group Network Information
      // Network Code Selection Criteria
       SelNwCd  = 'NWco# = ' + %EditC(intCo#:'X') + ' and ' +
          'NWstat<>' + '''I''';

      // Network Type Selection Criteria
       SelNwTyp = 'GTco# = ' + %EditC(intCo#:'X') + ' and ' +
          'GTstat<>' + '''I''' + 'and GTtbid=' + '''NWNT''';

      // Network Fee Type Selection Criteria
       SelNwFTyp = 'GTco# = ' + %EditC(intCo#:'X') + ' and ' +
          'GTstat<>' + '''I''' + 'and GTtbid=' + '''NWFT''';

       SelNwFEIN ='PDFEIN || '' '' || PDMOD || ''-'' || ' +
        'CASE WHEN PDDBA1=''Y'' THEN TRIM(PDADR1) || ''-'' || ' +
        'TRIM(PDADR2) ELSE ' +
        'TRIM(PDNAME) || ''-'' || TRIM(PDADR1) || '' '' || ' +
        'TRIM(PDADR2) END ' +
        ' || ''- '' || TRIM(PDCTY) || '', '' || ' +
        'PDST || '' '' || SUBSTR(DIGITS(PDZIP),1,5)';

      /End-Free
     p InzGrpInfScVal  e

       //===================================================================
       // Load System code / AR Rate Code Desciption
       //===================================================================
     p LoadModDesc     b
     d LoadModDesc     pi
     d  kktbid         s                   like(GTtbid)
     d  kkkey          s                   like(GTkey)

      /free

       scSysDesc = *blanks;
       scARDesc = *blanks;
       kktbid = 'SYS';
       kkkey  = scSysCd;
       // Load Sys code description
       Chain (intCo#:kktbid:kkkey) ftgtl;
       If %found(ftgtl) and GTstat <> 'I';
          scSysDesc = GTdesc;
       Endif;

       // Load AR code description
       Setgt (scARCd:*hival) nmbdl;
       Readpe (scARCd) nmbdl;
       If not %eof(nmbdl) and BDstat <> 'I';
          scARDesc = BDcom1;
       Endif;

       /end-free
     p LoadModDesc     e

       //===================================================================
       // Load System code / AR Rate Code information
       //===================================================================
     p LoadModInfo     b
     d LoadModInfo     pi

      /free

       // Clear the subfile
       InzGrpInfScVal();
       scSflRRN2 = 0;
       scSflDsp2 = *off;
       scSflClr2 = *on;
       Write rcdGrpMod;
       scSflClr2 = *off;
       scSflDsp2 = *on;
       ClrSflError();
       // Populate the subfile
       Chain(n) (inCo#:inSub:inFnd) fmfnl;
       If %found(fmfnl);
          scsflrrn2 += 1;
          scSysCd = FNsy01;
          scARCd  = FNbr01;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy02;
          scARCd  = FNbr02;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy03;
          scARCd  = FNbr03;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy04;
          scARCd  = FNbr04;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy05;
          scARCd  = FNbr05;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy06;
          scARCd  = FNbr06;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy07;
          scARCd  = FNbr07;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy08;
          scARCd  = FNbr08;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy09;
          scARCd  = FNbr09;
          LoadModDesc();
          Write sflModInfo;

          scsflrrn2 += 1;
          scSysCd = FNsy10;
          scARCd  = FNbr10;
          LoadModDesc();
          Write sflModInfo;

          saveDT1 = FNdt;
          saveTM1 = FNtime;
       Endif;
       scSflRRN2 = 1;
       scDays# = FN#day;
       scBills#= FN#bll;

       /end-free
     p LoadModInfo     e

       //===================================================================
       // Load Network Information Codes Desciption
       //===================================================================
     p LoadNetDesc     b
     d LoadNetDesc     pi
     d  kktbid         s                   like(GTtbid)
     d  kkkey          s                   like(GTkey)
     d  kkfein         s                   like(PDfein)
     d  kkmod          s                   like(PDmod)

      /free

       scNwDesc = *blanks;
       scNwTypDes = *blanks;
       scNwFTypDe = *blanks;
       scFeinAdd = *blanks;
       // Load net code description
       Chain (intCo#:scNwcd) nmnwl;
       If %found(nmnwl) and NWstat <> 'I';
          scNwDesc = NWname;
       Endif;

       kktbid = 'NWNT';
       kkkey  = scNwTyp;
       // Load Net Type description
       Chain (intCo#:kktbid:kkkey) ftgtl;
       If %found(ftgtl) and GTstat <> 'I';
          scNwTypDes = GTdesc;
       Endif;

       kktbid = 'NWFT';
       kkkey  = scNwFTyp;
       // Load Net Fee Type description
       Chain (intCo#:kktbid:kkkey) ftgtl;
       If %found(ftgtl) and GTstat <> 'I';
          scNwFTypDe = GTdesc;
       Endif;

       Monitor;
        If %dec(scNtFein:12:0) <> *zeros;
           kkfein = %dec(%subst(scNtFein:1:9):9:0);
           kkmod  = %dec(%subst(scNtFein:10:3):3:0);
           // Load Address of FEIN/Mod
           Chain (kkfein:kkmod) fmpdl;
           If %found(fmpdl) and PDstat <> 'I';
              If PDdba1='Y';
                 scFeinAdd = %trim(PDadr1) + ' - ' + %trim(PDadr2);
              Else;
                 scFeinAdd = %trim(PDname) + ' - ' + %trim(PDadr1)
                   + ' ' + %trim(PDadr2);
              Endif;
              scFeinAdd = %trim(scFeinAdd) + ',' + %trim(PDcty)
                   + ',' + %trim(PDst) + ' ' + %subst(%editc(PDzip:'X'):1:5);
           Endif;
        Endif;
       On-Error;
          scFeinAdd = *blanks;
       EndMon;

       /end-free
     p LoadNetDesc     e

       //===================================================================
       // Load Group Network Information
       //===================================================================
     p LoadNetInfo     b
     d LoadNetInfo     pi

      /free

       // Clear the subfile
       InzGrpInfScVal();
       scSflRRN3 = 0;
       scSflDsp3 = *off;
       scSflClr3 = *on;
       Write rcdGrpNet;
       scSflDsp3 = *on;
       scSflClr3 = *off;
       ClrSflError();

       Chain(n) (inCo#:inSub:inFnd) Fmf1l;
       If %found(fmf1l);
          scNwCd    =   F1nw01;
          scNwTyp   =   F1nf01;
          scNwFTyp  =   F1nt01;
          scNtFRt   =   F1n$01;
          scNtFEIN  =   %EditC(F1ni01:'X') + %EditC(F1nm01:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw02;
          scNwTyp   =   F1nf02;
          scNwFTyp  =   F1nt02;
          scNtFRt   =   F1n$02;
          scNtFEIN  =   %EditC(F1ni02:'X') + %EditC(F1nm02:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw03;
          scNwTyp   =   F1nf03;
          scNwFTyp  =   F1nt03;
          scNtFRt   =   F1n$03;
          scNtFEIN  =   %EditC(F1ni03:'X') + %EditC(F1nm03:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw04;
          scNwTyp   =   F1nf04;
          scNwFTyp  =   F1nt04;
          scNtFRt   =   F1n$04;
          scNtFEIN  =   %EditC(F1ni04:'X') + %EditC(F1nm04:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw05;
          scNwTyp   =   F1nf05;
          scNwFTyp  =   F1nt05;
          scNtFRt   =   F1n$05;
          scNtFEIN  =   %EditC(F1ni05:'X') + %EditC(F1nm05:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw06;
          scNwTyp   =   F1nf06;
          scNwFTyp  =   F1nt06;
          scNtFRt   =   F1n$06;
          scNtFEIN  =   %EditC(F1ni06:'X') + %EditC(F1nm06:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw07;
          scNwTyp   =   F1nf07;
          scNwFTyp  =   F1nt07;
          scNtFRt   =   F1n$07;
          scNtFEIN  =   %EditC(F1ni07:'X') + %EditC(F1nm07:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw08;
          scNwTyp   =   F1nf08;
          scNwFTyp  =   F1nt08;
          scNtFRt   =   F1n$08;
          scNtFEIN  =   %EditC(F1ni08:'X') + %EditC(F1nm08:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw09;
          scNwTyp   =   F1nf09;
          scNwFTyp  =   F1nt09;
          scNtFRt   =   F1n$09;
          scNtFEIN  =   %EditC(F1ni09:'X') + %EditC(F1nm09:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          scNwCd    =   F1nw10;
          scNwTyp   =   F1nf10;
          scNwFTyp  =   F1nt10;
          scNtFRt   =   F1n$10;
          scNtFEIN  =   %EditC(F1ni10:'X') + %EditC(F1nm10:'X');
          LoadNetDesc();
          scsflrrn3 += 1;
          Write sflNetInfo;

          saveDT2 = F1dt;
          saveTM2 = F1time;
       Endif;
       scSflRRN3 = 1;

       /end-free
     p LoadNetInfo     e

       //===================================================================
       // Load Outcome/Savings Code Grid
       //===================================================================
     p LoadOSCode      b
     d LoadOSCode      pi

      /free
       // Clear the subfile
       scSflRRN4 = 0;
       scSflDsp4 = *off;
       scSflClr4 = *on;
       Write rcdOSCode;
       scSflClr4 = *off;
       scSflDsp4 = *on;
       scRowSel4 = 1;

       // Populate the subfile
       Setll (inCo#:inSub:inFnd) Ftgol;
       Reade (inCo#:inSub:inFnd) Ftgol;
       Dow not %eof(Ftgol);

          scSflRRN4 += 1;
          scSflSel4 = *off;
          If svsflrrn4 = 0;
             If scSflRRN4 = 1;
                scSflSel4 = *on;
             Endif;
          Else;
             If scSflRRN4 = svsflrrn4;
                scSflSel4 = *on;
             Endif;
          Endif;
         scOSCode =  GOcode;                   // Code
          If GOstat = 'I';                      // Status
            scOSSts  =  'Inactive';
          Else;
            scOSSts  =  'Active';
          Endif;
          Chain (inCo#:scOSCode) Ftotl;
         scOSDesc =  *blanks;
          If %found(ftotl);
            scOSDesc =  %trim(OTdesc);         // Description
          Endif;
          scRRN4 = dbrrn4;
          Write sflOSCode;
          Reade (inCo#:inSub:inFnd) Ftgol;
       Enddo;

       scSflRRN4 = 1;
       If svSflRRN4 <> 0;
          scSflRRN4 = svSflRRN4;
          scRowSel4 = svSflRRN4;
          svSflRRN4 = 0;
       Endif;

      /end-free
     p LoadOSCode      e

       //===================================================================
       // Load T/M Rates
       //===================================================================
     p LoadTMRate      b
     d LoadTMRate      pi

      /free
       // Clear the subfile
       scSflRRN1 = 0;
       scSflDsp1 = *off;
       scSflClr1 = *on;
       Write rcdTMRate;
       scSflClr1 = *off;
       scSflDsp1 = *on;
       scRowSel1 = 1;

       // Populate the subfile
       Setll (inCo#:inSub:inFnd) fmfrl;
       Reade (inCo#:inSub:inFnd) fmfrl;
       Dow not %eof(fmfrl);

          scSflRRN1 += 1;
          scSflSel1 = *off;
          If svsflrrn1 = 0;
             If scSflRRN1 = 1;
                scSflSel1 = *on;
             Endif;
          Else;
             If scSflRRN1 = svsflrrn1;
                scSflSel1 = *on;
             Endif;
          Endif;
          scTMEfDt = CvtToDate(FRefdt);           // Effective date
          scPRt    =  FRprt1;                     // Prof Rate
          scNPRt   =  FRnrt1;                     // Non-Prof Rate
          scMileRt =  FRmrt1;                     // Mileage Rate
          If FRstat = 'I';                        // Status
             scTMSts  =  'Inactive';
          Else;
             scTMSts  =  'Active';
          Endif;
          scRRN1 = dbrrn1;
          Write sflTMRate;
          Reade (inCo#:inSub:inFnd) fmfrl;
       Enddo;

       scSflRRN1 = 1;
       If svSflRRN1 <> 0;
          scSflRRN1 = svSflRRN1;
          scRowSel1 = svSflRRN1;
          svSflRRN1 = 0;
       Endif;

      /end-free
     p LoadTMRate      e

       //===================================================================
       // Main - Main processing
       //===================================================================
     p Main            b
     d Main            pi
      /free

       // Process until exit button is pressed.
       DoU BtnCancel = *On;

          // display screen
          Exsr ExfmtScreens;

          // Perform actions on screen.
          Select;

          // Refresh
          When btnRefresh = *On;
             //btnRefresh =*Off;

          // Add T/M Rates
          When BtnTMRtAdd = *On;
             BtnTMRtAdd = *Off;
             ProcessAddTMRt();

          // Add Outcome/Savings Codes
          When BtnOSCdAdd = *On;
             BtnOSCdAdd = *Off;
             ProcessAddOSCd();

          // Save button
          When btnSave = *on;
             btnSave = *off;
             isSaved = *off;
             error = *off;
             If @@mode <> 'D';
                // Check if FMFNP updated since screen displayed (Group Master)
                flgRcdUpdated = RcdUpdatedErr('FMFNP':dbrrn:'FNdt':'FNtime':
                 'FNuser':saveDT1:saveTM1);

                If flgRcdUpdated = '0' and saveDT2 <> 0; // No error
                   // Check if FMF1P updated since screen displayed
                   flgRcdUpdated = RcdUpdatedErr('FMF1P':dbrrn5:'F1dt':
                    'F1time':'F1user':saveDT2:saveTM2);
                EndIf;

                If flgRcdUpdated = '0';
                   If %Lookup('A/R Information':arrTabNames) <> 0;
                      error1 = ValidateModInfo();
                   EndIf;
                   If isNtwk = *on and
                      %Lookup('Network Information':arrTabNames) <> 0;
                      error2 = ValidateNetInfo();
                   EndIf;

                   Select;
                   When error1=*on;
                      tabIndex= %Lookup('A/R Information':arrTabNames)-1;
                      error=*on;
                   When error2=*on;
                      tabIndex= %Lookup('Network Information':arrTabNames)-1;
                      error=*on;
                   Endsl;

                   If error1 = *off and error2 = *off;
                      ProcSavModInfo();
                      If isNtwk = *on;
                         ProcSavNetInfo();
                      Endif;
                      PopDataSaved();
                      isSaved = *on;
                   Endif;

                Endif;

             Endif;

          // Add User Access Window Logic
          When btnUsrAcc = *on;
             inDspFileName = 'MTGRPACT';
             inPgmName     = 'Accounting Information';
             inTabName     =  arrTabNames(TabIndex + 1);
             inTabName     =  %Trim(inTabName) + ' - ' + 'User Access';
             UserAccessWdw(inDspFileName:inPgmName:inTabName);
             btnUsrAcc = *off;

          // Menu Choices
          When scSelOpt <> *blanks;
             @@next = scSelOpt;
             Leave;

          Other;
             ProcOtherTMRt();
             ProcOtherOSCd();

          Endsl;
       Enddo;

       //===================================================================
       // Write Screen Formats;
       //===================================================================
       BegSr ExfmtScreens;

       indexTab = TabIndex + 1;
       If dspMode;
          inquiry = *on;
       Endif;
       If isFirst = *on or btnRefresh = *on or isSaved = *on;
          isFirst = *off;
          btnRefresh = *off;
          isSaved = *off;
          LoadModInfo();
          LoadNetInfo();
       Endif;

       Select;
       When arrTabNames(TabIndex+1) = 'A/R Information';
          pnlHeight = 300;
          APnlHeight = pnlHeight + 60;
          //LoadARInfo();
          scSflRRN2 = 1;
          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdGrpMod;
          Read rcdGrpPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'Network Information';
          pnlHeight = 280;
          APnlHeight = pnlHeight + 60;
          //LoadNetInfo();
          scSflRRN3 = 1;
          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdGrpNet;
          Read rcdGrpPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'T/M Rates';
          pnlHeight = 280;
          APnlHeight = pnlHeight + 60;
          //LoadTMRate();
          LoadTMRate();
          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdTMRate;
          Read rcdGrpPnl;
          Read rcdPanel;

       When arrTabNames(TabIndex+1) = 'Outcome/Savings Codes';
          pnlHeight = 280;
          APnlHeight = pnlHeight + 60;
          //LoadOSCode();
          LoadOSCode();
          Write rcdGrpPnl;
          Write rcdPanel;
          Exfmt rcdOSCode;
          Read rcdGrpPnl;
          Read rcdPanel;

       Endsl;
       Endsr;

      /end-free
     p Main            e

       //===================================================================
       // ProcessAddOSCd - Process addition of Outcome/Savings Codes
       //===================================================================
     P ProcessAddOSCd  B
     D ProcessAddOSCd  PI
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdOSCdAdd;
       isexit = *off;
       Dow isexit = *off;
          scOSCdHdg = 'Add Outcome/Savings Codes Information';
          Exfmt rcdOSCdAdd;
          Select;
          When BtnSavOSCd = *on;
             BtnSavOSCd = *off;
             If ValidateOSCd();
                Exsr MoveScDb;
                write pgo0p;
                isexit = *on;
             Endif;
          When BtnCanOSCd = *on;
             BtnCanOSCd = *off;
             isexit = *on;
          Endsl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        clear pgo0p;
        GOco#   = inCo#;
        GOsub#  = inSub;
        GOfnd   = inFnd;
        GOcode  = scOSCodeE;
        GOstat  = 'A';
        GOuser  = q1user;                // User
        GOpgm   = q1pgm;                 // Program
        GOtime  = Timeto6(SystemTime()); // Time
        GOdt    = Dateto7(SystemDate()); // Date Mintained
        GOiusr  = q1user;                // Initial User
        GOipgm  = q1pgm;                 // Initial Program
        GOitim  = Timeto6(SystemTime()); // Initial Time
        GOidt   = Dateto7(SystemDate()); // Initial Date
       EndSr;

      /end-free
     P ProcessAddOSCd  E

       //===================================================================
       // ProcessAddTMRt - process addition of T/M Rates
       //===================================================================
     P ProcessAddTMRt  B
     D ProcessAddTMRt  PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdTMRtAdd;
       mode = 'A';
       isexit = *off;
       scTMStsE  = 'A';
       Dow isexit = *off;
          scTMRtHdg = 'Add T/M Rate Information';
          Exfmt rcdTMRtAdd;
          Select;
          When BtnSavTMRt = *on;
             BtnSavTMRt = *off;
             If ValidateTMRt(mode);
                Exsr MoveScDb;
                write pfr0p;
                isexit = *on;
             Endif;
          When BtnCanTMRt = *on;
             BtnCanTMRt = *off;
             isexit = *on;
          Endsl;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MoveScDb;
        clear pfr0p;
        FRco#   = inCo#;
        FRsub#  = inSub;
        FRfnd   = inFnd;
        FRefdt  = dtCYMD(scTMEfDtE);
        FRprt1  = scPRtE;
        FRnrt1  = scNPRtE;
        FRmrt1  = scMileRtE;
        FRstat  = scTMStsE;
        FRuser  = q1user;                // User
        FRpgm   = q1pgm;                 // Program
        FRtime  = Timeto6(SystemTime()); // Time
        FRdt    = Dateto7(SystemDate()); // Date Mintained
        FRiusr  = q1user;                // Initial User
        FRipgm  = q1pgm;                 // Initial Program
        FRitim  = Timeto6(SystemTime()); // Initial Time
        FRidt   = Dateto7(SystemDate()); // Initial Date
       EndSr;

      /end-free
     P ProcessAddTMRt  E

       //===================================================================
       // ProcOtherOSCd  - Process other option Outcome/Savings Codes
       //===================================================================
     P ProcOtherOSCd   B
     D ProcOtherOSCd   PI
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdOSCdAdd;
       isexit = *off;
       ReadC sflOSCode;
       Dow not %eof;
          Select;
          When scOSCdDlt = *On;
             scOSCdDlt = *Off;
             Chain(e) scRRN4 pgo0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRrn4 pgo0p;
             scOSCodeE  =  GOcode;
             scOSCdHdg = 'Delete Outcome/Savings Code Information';
             Dow isexit = *off;
                Write rcdOSCdAdd;
                xxDelresp = PopTwoBtn('Do you want to delete this +
                Outcome/Savings Code?':'Confirm Delete':'Yes':'No');
                Select;
                When xxDelResp = *On;
                   Delete pgo0p;
                   PopOneBtn('Outcome/Savings Code +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svsflrrn4 = scSflrrn4;  //save rrn value
                   isexit = *on;
                Endsl;
             Enddo;
          Endsl;
          Unlock ftgop;
          ReadC sflOSCode;
       Enddo;

      /end-free
     P ProcOtherOSCd   E

       //===================================================================
       // ProcOtherTMRt  - Process other option for T/M Rates
       //===================================================================
     P ProcOtherTMRt   B
     D ProcOtherTMRt   PI
     d  mode           s              1    Inz
     d  isexit         s               n   Inz(*off)

      /free

       Clear rcdTMRtAdd;
       mode = 'C';
       isexit = *off;
       ReadC sflTMRate;
       Dow not %eof;

          Select;
          When scTMEdt  = *On; // Processing for Edit
             scTMEdt  = *Off;
             Chain(e) scRRN1 pfr0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;
             svsflrrn1 = scSflrrn1;  //save rrn value

             Chain(n) scRRN1 pfr0p;
             Exsr Movdbsc;
             scTMRtHdg = 'Edit T/M Rates Information';
             dsbTMRtDt = *on;
             Dow isexit = *off;
                Exfmt rcdTMRtAdd;

                Select;
                When BtnSavTMRt = *on;
                   BtnSavTMRt = *off;
                   // Check if fmfrp updated since screen displayed
                   flgRcdUpdated=RcdUpdatedErr('FMFRP':dbrrn2:'FRdt':'FRtime':
                    'FRuser':saveDT3:saveTM3:'1');
                   If flgRcdUpdated = '2'; // Error with Refresh Button
                      Chain(n) scRRN1 pfr0p;
                      Exsr Movdbsc;
                   EndIf;
                   If flgRcdUpdated = '0'; // No error
                      // Validate the T/M Rates Information
                      If ValidateTMRt(mode);
                         Chain scRRN1 pfr0p;
                         Exsr Movscdb;
                         update pfr0p;
                         isExit = *on;
                      Endif;
                   Endif;

                When BtnCanTMRt = *on;
                   BtnCanTMRt = *off;
                   isExit = *on;
                EndSl;
             Enddo;

          When scTMDlt = *On;
             scTMDlt = *Off;
             Chain(e) scRRN1 pfr0p;
             If %status = 1218;
                PopOneBtn('Record locked by another user. Please try again-
                 later.':'Record Lock');
                Return;
             Endif;

             Chain scRrn1 pfr0p;
             Exsr MovDbSc;
             scTMRtHdg = 'Delete T/M Rates Information';
             Dow isexit = *off;
                Write rcdTMRtAdd;
                xxDelresp = PopTwoBtn('Do you want to delete this T/M +
                Rate Information?':'Confirm Delete':'Yes':'No');
                Select;
                When xxDelResp = *On;
                   Delete pfr0p;
                   PopOneBtn('T/M Rate Information +
                   Deleted!':'Delete':'Ok');
                   isexit = *on;
                When xxDelResp = *Off;
                   svsflrrn1 = scSflrrn1;  //save rrn value
                   isexit = *on;
                Endsl;
             Enddo;

          Endsl;
          Unlock fmfrp;
          ReadC sflTMRate;
       Enddo;

       // Subrotine to move Screen fields into database fields.
       BegSr MovScDb;
        FRprt1  = scPRtE;
        FRnrt1  = scNPRtE;
        FRmrt1  = scMileRtE;
        FRstat  = scTMStsE;
        FRuser  = q1user;                // User
        FRpgm   = q1pgm;                 // Program
        FRtime  = Timeto6(SystemTime()); // Time
        FRdt    = Dateto7(SystemDate()); // Date Mintained
       EndSr;

       // Subrotine to move files into screen fields.
       BegSr Movdbsc;
        scTMEfDtE  =  dtMDYY(FRefdt);
        scPRtE     =  FRprt1;
        scNPRtE    =  FRnrt1;
        scMileRtE  =  FRmrt1;
        scTMStsE   =  FRstat;
        saveDT3    =  FRdt;
        saveTM3    =  FRtime;
       EndSr;

      /end-free
     P ProcOtherTMRt   E

       //===================================================================
       // ProcSavModInfo - save System - AR Information
       //===================================================================
     P ProcSavModInfo  B
     D ProcSavModInfo  PI

      /free
        chain (inCo#:inSub:inFnd) pfn00;
        For i = 1 to 10;
           dsFNSys(i) = dsSysCd(i);
        Endfor;
        For i = 1 to 10;
           dsFNAR(i) = dsArCd(i);
        Endfor;
        FN#day = scDays#;
        FN#bll = scBills#;

        FNuser  = q1user;                // User
        FNpgm   = q1pgm;                 // Program
        FNtime  = Timeto6(SystemTime()); // Time
        FNdt    = Dateto7(SystemDate()); // Date Mintained
        update pfn00;

      /end-free
     P ProcSavModInfo  E

       //===================================================================
       // ProcSavNetInfo- Save group Network Information
       //===================================================================
     P ProcSavNetInfo  B
     D ProcSavNetInfo  PI

      /free
        //Write WCMC Group Network Information  //Fmf1p file
        Chain (inCo#:inSub:inFnd) Fmf1l;
        If %found(fmf1l);
           exsr movescdb2;
           update pf100;
        Else;
           clear pf100;
           exsr movescdb2;
           F1co#   = inCo#;
           F1sub#  = inSub;
           F1fnd   = inFnd;
           F1iusr  = q1user;                // Initial User
           F1ipgm  = q1pgm;                 // Initial Program
           F1itim  = Timeto6(SystemTime()); // Initial Time
           F1idt   = Dateto7(SystemDate()); // Initial Date
           write pf100;
        Endif;

        Begsr movescdb2;
         For i = 1 to 10;
            dsF1Nwcd(i)   =  dsNwcd(i);
            dsF1NwTyp(i)  =  dsNwTyp(i);
            dsF1NwFTyp(i) =  dsNwFTyp(i);
            dsF1NtFRt(i)  =  dsNtFRt(i);

            Monitor;
               dsF1NtFEIN(i) = %Dec(%Subst(dsNtFein(i):1:9):9:0);
            On-Error 105;
               dsF1NtFEIN(i) = 0;
            EndMon;

            Monitor;
               dsF1NtMOD(i) = %Dec(%Subst(dsNtFein(i):10:3):3:0);
            On-Error 105;
               dsF1NtMOD(i) = 0;
            EndMon;
         Endfor;

         F1user  = q1user;                // User
         F1pgm   = q1pgm;                 // Program
         F1time  = Timeto6(SystemTime()); // Time
         F1dt    = Dateto7(SystemDate()); // Date Mintained
        Endsr;

      /end-free
     P ProcSavNetInfo  E

       //===================================================================
       // ProgramStart - Initial Program tasks
       //===================================================================
     p ProgramStart    b
     d ProgramStart    pi
     d processMe                       n
      /free

       // Program Mode.
       If q1prms >= 2;
          Select;
          When @@mode = 'D';
             dspMode = *on;
          When @@mode = 'C';
             chgMod  = *on;
          Endsl;
       Endif;

       isMedMgnt = *off;
       isMedMgnt = IsModEnabled('Med Case Management');

       isNtwk   = *off;
       isNtwk   = IsModEnabled('Managed Care Network');

       apShowMenu = *on;
       // Set Menu options
       scMenuChoi = 'Primary Group Information';
       scMenuValu = 'MTGRPPRI';

       scMenuChoi = %trim(scMenuChoi) + ',Employer Information';
       scMenuValu = %trim(scMenuValu) + ',MTGRPEMP';

       scMenuChoi = %trim(scMenuChoi) + ',Accounting Information';
       scMenuValu = %trim(scMenuValu) + ',MTGRPACT';

       // Set Screen Heading
       scHeading = 'Accounting Information';

       aMenHeight = 80;

       apFootHght = aMenHeight + 125;

       // Tab Index
       tabIndex = 0;
       If q1prms >= 4;
          If @@TabInd <> ' ';
             tabIndex = %int(@@TabInd);
             If tabIndex < 0 or tabIndex > 2;
                TabIndex = 0;
             Endif;
          Endif;
          @@TabInd = '00000';
       Endif;

       intCo# = GetUserInfo(q1user);
       scCo# = intCo#;
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

      // Get User Default Group
       UsrDftFnd = 0;
       Chain q1user pup00;
       If %found(smupl);
          UsrDftFnd = UPfnd;
          UsrDftSub = UPsub#;
          UsrDftFyr = UPfyr;
       EndIf;

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

       // A/R Information tab
       Chain (intCo#:'MTGRPACT':'Accounting Information':
        'A/R Information - User Access':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('A/R Information':arrTabNames:n1);
       Endif;

       // Network Information tab
       If isNtwk = *on;
          Chain (intCo#:'MTGRPACT':'Accounting Information':
           'Network Information - User Access':Q1user) pTB01;
          If %Found(SDTBL1) or isUsrAcc = *on;
             strTabNames += AddToList('Network Information':arrTabNames:n1);
          Endif;
       Endif;

       // T/M Rates tab
       If isMedMgnt = *on;
          Chain (intCo#:'MTGRPACT':'Accounting Information':
           'T/M Rates - User Access':Q1user) pTB01;
          If %Found(SDTBL1) or isUsrAcc = *on;
             strTabNames += AddToList('T/M Rates':arrTabNames:n1);
          Endif;
       Endif;

       // Outcome/Savings Codes tab
       Chain (intCo#:'MTGRPACT':'Accounting Information':
        'Outcome/Savings Codes - User Access':Q1user) pTB01;
       If %Found(SDTBL1) or isUsrAcc = *on;
          strTabNames += AddToList('Outcome/Savings Codes':arrTabNames:n1);
       Endif;
       TbNmRcdPnl = strTabNames;

       outRTCD = *blanks;
       outSTS  = *blanks;
       outCO   = %editc(intco#:'X');
       outSUB# = %editc(UsrDftSub:'X');
       outFND  = %editc(FNfnd:'X');
       GroupType(outRTCD:outCO:outSUB#:outFND:outSTS);

       processMe = *on;
       Return;

      /end-free
     p ProgramStart    e

      //===================================================================
      // Validate the Group addition information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     pValidateModInfo  b
     dValidateModInfo  pi              n
     d isError         s               n   inz
      /Free

       isError = *off;
      // Initialize all the messageId and error indicators.

       // Error indicator and message ID for Group Module Information
       Readc sflModInfo;
       Dow not %eof;
          ClrSflError();
          update sflModInfo;
          Readc sflModInfo;
       Enddo;
       scsflRRN2 =1;

       Clear dsSysCd;
       Clear dsArCd;
       i = 1;
       Readc sflModInfo;
       Dow not %eof and i <=10;
          dsSysCd(i) = scSysCd;
          dsArCd(i) = scARCd;
          update sflModInfo;
          Readc sflModInfo;
          i = i + 1;
       Enddo;

      // Validate error for System Code and AR Code entered on Group Module Info
       // Validate System Code fields
       // First System Code Can't be Blank
       If SysCd01 = *Blanks;
          // Since we are re-arranging System Code before Save
          // so this error is required when all 10 System Code are Blank
          If SysCd02 = *Blanks and SysCd03 = *Blanks and SysCd04 = *Blanks and
           SysCd05 = *Blanks and SysCd06 = *Blanks and SysCd07 = *Blanks and
           SysCd08 = *Blanks and SysCd09 = *Blanks and SysCd10 = *Blanks;
             scsflRRN2 = 1;
             chain scSflRRN2 sflModInfo;
             erSysCd = *on;
             msSysCd = 'MCR0503';
             update sflModInfo;
             isError = *on;
          Endif;
       Endif;

       // First AR Rate code can't be Blank
       If ArCd01 = *Blanks and visAR = *on;
          // Since we are re-arranging AR Rate Code before Save
          // so this error is required when all 10 AR Rate Code are Blank
          If ArCd02 = *Blanks and ArCd03 = *Blanks and ArCd04 = *Blanks and
           ArCd05 = *Blanks and ArCd06 = *Blanks and ArCd07 = *Blanks and
           ArCd08 = *Blanks and ArCd09 = *Blanks and ArCd10 = *Blanks;
             scsflRRN2 = 1;
             chain scSflRRN2 sflModInfo;
             erArCd = *on;
             msArCd = 'MCR0503';
             update sflModInfo;
             isError = *on;
          Endif;
       Endif;

       // Check for Duplicate System Codes
       For i = 1 to 9;
          For j = 2 to 10;
             If i<>j and dsSysCd(i) <> *blanks and dsSysCd(i) = dsSysCd(j);
                isError = *on;
                scsflRRN2 = i;
                chain scSflRRN2 sflModInfo;
                erSysCd = *on;
                msSysCd = 'M400077';
                update sflModInfo;
                scsflRRN2 = j;
                chain scSflRRN2 sflModInfo;
                erSysCd = *on;
                msSysCd = 'M400077';
                update sflModInfo;
             Endif;
          Endfor;
       Endfor;

       // If System Code entered then A/R rate code must be entered.
       If visAR = *on;
          For i = 2 to 10;
             If dsSysCd(i) <> *blanks and dsArCd(i) = *blanks;
                scsflRRN2 = i;
                chain scSflRRN2 sflModInfo;
                erArCd = *on;
                msArCd  = 'MCR0503';         //This field can not be blank.
                update sflModInfo;
                isError = *on;
             Endif;
          Endfor;

          // If System Code blank then A/R rate code must be blank
          For i = 2 to 10;
             If dsSysCd(i) = *blanks and dsArCd(i) <> *blanks;
                scsflRRN2 = i;
                chain scSflRRN2 sflModInfo;
                erArCd = *on;
                msArCd  = 'MC40403';         //This field must be blank.
                update sflModInfo;
                isError = *on;
             Endif;
          Endfor;

          // If A/R rate code entered then A/R rate code must match system code
          For i = 1 to 10;
             If dsSysCd(i) <> *blanks and dsArCd(i) <> *blanks;
                chain dsArCd(i) pbd00;
                If %found(Nmbdl);
                   If %subst(BDtype:1:1) <> dsSysCd(i);
                      scsflRRN2 = i;
                      chain scSflRRN2 sflModInfo;
                      erArCd = *on;
                      msArCd  = 'MCR0619';   //Invalid A/R code entered for associated System code
                      update sflModInfo;
                      isError = *on;
                   Endif;
                Endif;
             Endif;
          Endfor;
       Endif;

       // If No Error, for saving in the database file
       // Re-arrange System Codes to Eliminate Blank codes
       If isError = *off;
          For i = 1 to 9;
             For j = i+1 to 10;
                If dsSysCd(i) = *blanks and dsSysCd(j) <> *blanks;
                   dsSysCd(i) = dsSysCd(j);
                   dsSysCd(j) = *blanks;
                Endif;
             Endfor;
          Endfor;
       Endif;

       // If No Error, for saving in the database file
       // Re-arrange AR Rate Codes to Eliminate Blank codes
       If isError = *off and visAR = *on;
          For i = 1 to 9;
             For j = i+1 to 10;
                If dsArCd(i) = *blanks and dsArCd(j) <> *blanks;
                   dsArCd(i) = dsArCd(j);
                   dsArCd(j) = *blanks;
                Endif;
             Endfor;
          Endfor;
       Endif;

      // Return *on indicator when No error exists.
       If isError = *on;
          return *on;
       Else;
          return *off;
       Endif;
      /End-Free
     pValidateModInfo  e

      //===================================================================
      // Validate the Group Network information.
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     pValidateNetInfo  b
     dValidateNetInfo  pi              n
     d isError         s               n   inz
      /Free

       isError = *off;
      // Initialize all the messageId and error indicators.
       clear dsIsError;
       Readc sflNetInfo;
       Dow not %eof;
          ClrSflError();
          update sflNetInfo;
          Readc sflNetInfo;
       Enddo;
       scsflRRN3 =1;

       Clear dsNwCd;
       Clear dsNwTyp;
       Clear dsNwFTyp;
       Clear dsNtFRt;
       Clear dsNtFein;
       i = 1;
       Readc sflNetInfo;
       Dow not %eof and i <=10;
          dsNwCd(i)   =  scNwCd;
          dsNwTyp(i)  =  scNwTyp;
          dsNwFTyp(i) =  scNwFTyp;
          dsNtFRt(i)  =  scNtFRt;
          Monitor;
             If %dec(scNtFein:12:0) <> *zeros;
                dsNtFein(i) =  scNtFein;
             Endif;
          On-Error;
            dsNtFein(i) = *blanks;
          EndMon;
          update sflNetInfo;
          Readc sflNetInfo;
          i = i + 1;
       Enddo;

       //Network code must be entered
       For i = 1 to 10;
          If dsNwCd(i) = *blanks and (dsNwTyp(i) <> *blanks or
          dsNwFTyp(i) <> *blanks or dsNtFRt(i) <> *zeros
          or dsNtFein(i) <> *blanks);
             scsflRRN3 = i;
             chain scsflRRN3 sflNetInfo;
             erNwCd   = *on;
             msNwCd   = 'MCR0544'; //The indicated information is mutually inclusive.
             erNwTyp  = *on;
             msNwTyp  = 'MCR0544';
             erNwFTyp = *on;
             msNwFTyp = 'MCR0544';
             erNtFRt  = *on;
             msNtFRt  = 'MCR0544';
             erNtFein = *on;
             msNtFein = 'MCR0544';
             update sflNetInfo;
             isError = *on;
             dsIsError(i) = *on;
          Endif;
       Endfor;

       //Fee Rate must be zero if Network flag not equal to 'N' OR 'Y'
       For i = 1 to 10;
          If ( dsNwTyp(i) <> 'N' and dsNwTyp(i) <> 'Y') and
           dsNtFRt(i) <> *zeros and dsIsError(i) = ' ';
             scsflRRN3 = i;
             chain scsflRRN3 sflNetInfo;
             erNtFRt  = *on;
             msNtFRt  = 'MC40011'; //Value must equal zero.
             update sflNetInfo;
             dsIsError(i) = *on;
             isError = *on;
          Endif;
       Endfor;

       //Network Fee Type is required if Network type = 'N' OR 'Y'
       For i = 1 to 10;
          If ( dsNwTyp(i) = 'N' or dsNwTyp(i) = 'Y') and
           dsNwFTyp(i) = *blanks and dsIsError(i) = ' ';
             scsflRRN3 = i;
             chain scsflRRN3 sflNetInfo;
             erNwFTyp  = *on;
             msNwFTyp  = 'MCR0503';   //This field cannot be blank.
             update sflNetInfo;
             dsIsError(i) = *on;
             isError = *on;
          Endif;
       Endfor;

       //Network Fein is required if Network type = 'N' OR 'Y'
       For i = 1 to 10;
          If (dsNwTyp(i) = 'N' or dsNwTyp(i) = 'Y') and
            dsNwFTyp(i) <> 'N' and dsNtFEIN(i) = *blanks and dsIsError(i) = ' ';
             scsflRRN3 = i;
             chain scsflRRN3 sflNetInfo;
             erNtFEIN  = *on;
             msNtFEIN  = 'MCR0556'; //Value cannot be zero.
             update sflNetInfo;
             dsIsError(i) = *on;
             isError = *on;
          Endif;
       Endfor;

       //four errors
       //If Network Fee Type = 'B' or 'S' then flag = 'N' fee > *ZEROS
       For i = 1 to 10;
          If (dsNwFTyp(i) = 'B' or dsNwFTyp(i) = 'S') and
           (dsNwTyp(i) <>'N' or dsNtFRt(i) <= *zeros) and dsIsError(i) = ' ';
             scsflRRN3 = i;
             chain scsflRRN3 sflNetInfo;
             If dsNwTyp(i) <> 'N';
                erNwTyp  = *on;
                msNwTyp  = 'MC40008'; // Invalid type.
             Endif;
             If dsNtFRt(i) <= *zeros;
                erNtFRt  = *on;
                msNtFRt  = 'MC40008'; // Invalid type.
             Endif;
             update sflNetInfo;
             dsIsError(i) = *on;      //error text desc
             isError = *on;           //Network type must = 'N' and Fee rate must be > *zeros.
          Endif;
       Endfor;

       //If Network Fee Type = 'N' then Net Type = 'N' fee = *ZEROS
       For i = 1 to 10;
          If dsNwFTyp(i) = 'N' and (dsNwTyp(i)<>'N' or
           dsNtFRt(i) <> *zeros) and dsIsError(i) = ' ';
             scsflRRN3 = i;
             chain scsflRRN3 sflNetInfo;
             If dsNwTyp(i) <> 'N';
                erNwTyp  = *on;
                msNwTyp  = 'MC40009';       //Invalid type.
             Endif;
             If dsNtFRt(i) <> *zeros;
                erNtFRt  = *on;
                msNtFRt  = 'MC40009';       //Invalid type.
             Endif;
             update sflNetInfo;
             dsIsError(i) = *on;
             isError = *on;             //Network type must = 'N' and Fee rate must = *zeros.
          Endif;
       Endfor;

       //If Network Fee Type = 'R' then Net Type = 'Y' fee = *zeros
       For i = 1 to 10;
          If dsNwFTyp(i) = 'R' and (dsNwTyp(i)<>'Y' or
           dsNtFRt(i) <> *zeros) and dsIsError(i) = ' ';
             scsflRRN3 = i;
             chain scsflRRN3 sflNetInfo;
             If dsNwTyp(i) <> 'Y';
                erNwTyp  = *on;
                msNwTyp  = 'MC40010';     //Invalid type
             Endif;
             If dsNtFRt(i) <> *zeros;
                erNtFRt  = *on;
                msNtFRt  = 'MC40010';     //Invalid type
             Endif;
             update sflNetInfo;
             dsIsError(i) = *on;
             isError = *on;             //Network type must = 'Y' and Fee rate must = *zeros.
          Endif;
       Endfor;

       // If No Error, for saving in the database file
       // Re-arrange Network Info to Eliminate Blank codes
       If isError = *off;
          For i = 1 to 9;
             For j = i+1 to 10;
                If dsNwCd(i) = *blanks and dsNwCd(j) <> *blanks;
                   dsNwCd(i) = dsNwCd(j);
                   dsNwCd(j) = *blanks;
                   dsNwTyp(i) = dsNwTyp(j);
                   dsNwTyp(j) = *blanks;
                   dsNwFTyp(i) = dsNwFTyp(j);
                   dsNwFTyp(j) = *blanks;
                   dsNtFRt(i) = dsNtFRt(j);
                   dsNtFRt(j) = *zeros;
                   dsNtFein(i) = dsNtFein(j);
                   dsNtFein(j) = *blanks;
                Endif;
             Endfor;
          Endfor;
       Endif;

      // Return *on indicator when No error exists.
        If isError = *on;
           return *on;
        Else;
           return *off;
        Endif;

      /End-Free
     pValidateNetInfo  e

       //===================================================================
       // Validate T/M Rates
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateTMRt    b
     d ValidateTMRt    pi              n
     d strmode                        1
     d isError         s               n   inz

      /Free

       // Initialize all the messageId and error indicators.
       isError = *off;
       erTMEfDtE  = *off;
       msTMEfDtE  = *blanks;
       erPRtE     = *off;
       msPRtE     = *blanks;
       erNPRtE    = *off;
       msNPRtE    = *blanks;
       erMileRtE  = *off;
       msMileRtE  = *blanks;

       If strmode = 'A';
          // Validation Check for Effective Date
          xxDate1 = dtCheck(scTMEfDtE);
          If xxDate1 = 0;
             isError = *on;
             erTMEfDtE = *on;
             msTMEfDtE ='MCR0500';
          Endif;
       Endif;

       // Check if Prof Rate zip is zero
       If scPRtE  = *zeros;
          erPRtE  = *on;
          msPRtE  = 'MCR0556';
          isError = *on;
       Endif;

       // Check if Non-Prof Rate zip is zero
       If scNPRtE  = *zeros;
          erNPRtE  = *on;
          msNPRtE  = 'MCR0556';
          isError  = *on;
       Endif;

       // Check if Mileage Rate zip is zero
       If scMileRtE = *zeros;
          erMileRtE = *on;
          msMileRtE = 'MCR0556';
          isError   = *on;
       Endif;

       // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateTMRt    e

       //===================================================================
       // Validate Outcome/Savings Codes
       // Returns *on  ----> No errors
       // Returns *off ----> Any errors
       //===================================================================
     p ValidateOSCd    b
     d ValidateOSCd    pi              n
     d isError         s               n   inz

      /Free

       // Initialize all the messageId and error indicators.
       isError = *off;
       erOSCodeE  = *off;
       msOSCodeE  = *blanks;

       // Check if OS code is blank
       If scOSCodeE = *blanks;
          erOSCodeE = *on;
          msOSCodeE = 'MCR0503';
          isError = *on;
       Endif;

       // Return *on indicator when No error exists.
       If isError = *on;
          return *off;
       Else;
          return *on;
       Endif;

      /End-Free
     p ValidateOSCd    e

